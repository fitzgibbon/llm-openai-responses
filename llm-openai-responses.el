;;; llm-openai-responses.el --- Responses API provider for llm-openai -*- lexical-binding: t; package-lint-main-file: "llm-openai-responses.el" -*-

;; Author: llm-openai-responses contributors
;; Maintainer: llm-openai-responses contributors
;; Package-Version: 0.1.0
;; Package-Revision: nil
;; Package-Requires: ((emacs "29.1") (llm "0") (oauth2 "0.18.4"))
;; Keywords: ai, llm
;; URL: https://github.com/fitzgibbon/llm-openai-responses
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This package adds an llm.el provider that talks to OpenAI's Responses API
;; endpoint (/v1/responses) while reusing llm-openai authentication, model, and
;; embedding configuration.
;;
;; It also supports the Codex / ChatGPT OAuth-backed Responses endpoint used by
;; the official Codex tooling.  In that mode, the provider authenticates via
;; Emacs oauth2.el and sends requests to
;; `https://chatgpt.com/backend-api/codex/responses'.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'browse-url)
(require 'llm)
(require 'llm-openai)
(require 'llm-provider-utils)
(require 'oauth2)

(defconst llm-openai-responses-default-api-url "https://api.openai.com/v1/"
  "Default OpenAI Responses API base URL.")

(defconst llm-openai-responses-default-codex-url "https://chatgpt.com/backend-api/codex/"
  "Default Codex OAuth Responses API base URL.")

(defconst llm-openai-responses-default-codex-client-id "app_EMoamEEZ73f0CkXaXp7hrann"
  "Default OAuth client id used by Codex ChatGPT auth.")

(defconst llm-openai-responses-default-codex-issuer "https://auth.openai.com"
  "Default OAuth issuer used by Codex ChatGPT auth.")

(defconst llm-openai-responses-default-codex-auth-url "https://auth.openai.com/oauth/authorize"
  "Default OAuth authorization URL used by Codex ChatGPT auth.")

(defconst llm-openai-responses-default-codex-callback-port 1455
  "Default localhost callback port used by the official Codex login flow.")

(defconst llm-openai-responses-fallback-codex-callback-port 1457
  "Fallback localhost callback port used when the default Codex port is unavailable.")

(defconst llm-openai-responses-default-codex-scope "openid profile email offline_access"
  "Default OAuth scope used by Codex ChatGPT auth.")

(defconst llm-openai-responses-default-codex-login-scope
  "openid profile email offline_access api.connectors.read api.connectors.invoke"
  "Default OAuth scope used by the official Codex browser login flow.")

(defconst llm-openai-responses-default-codex-originator "codex_cli_rs"
  "Default originator used by Codex ChatGPT auth flows.")

(defconst llm-openai-responses-default-codex-oauth-host-name "chatgpt.com"
  "Default oauth2 host name for cached Codex ChatGPT auth.")

(defconst llm-openai-responses-codex-refresh-expiry-margin-seconds (* 5 60)
  "Refresh Codex OAuth tokens this many seconds before access-token expiry.")

(defconst llm-openai-responses-codex-refresh-interval-seconds (* 55 60)
  "Refresh Codex OAuth tokens this often even if JWT expiry is unavailable.")

(defconst llm-openai-responses-codex-login-timeout-seconds (* 5 60)
  "Wait this long for the Codex OAuth browser callback.")

(defconst llm-openai-responses-codex-login-url-buffer-name "*Codex Login URL*"
  "Buffer name used to display manual Codex login URLs.")

(defvar llm-openai-responses-codex-login-thread nil
  "Background thread for the active Codex OAuth login flow.")

(defvar llm-openai-responses--codex-oauth-token-cache (make-hash-table :test #'equal)
  "OAuth tokens indexed by their oauth2 plstore id.")

(cl-defstruct (llm-openai-responses
               (:include llm-openai-compatible
                         (url llm-openai-responses-default-api-url)))
  "OpenAI provider that uses the /v1/responses API for chat calls.

REASONING-SUMMARY controls whether the Responses API should return
reasoning summaries.  nil leaves provider defaults unchanged.  Non-nil
requests a summary string mode (for example \"auto\").

DEFAULT-REASONING-EFFORT is a reasoning effort string (for example
\"none\", \"low\", \"medium\", \"high\") sent when a prompt does not
request an effort itself.  It is passed through verbatim; which values
are valid depends on the backend and model.  nil sends no default.

When CODEX-OAUTH is non-nil, the provider talks to the Codex ChatGPT backend
instead of the standard OpenAI API.  Tokens are acquired and refreshed via
oauth2.el, and CODEX-ACCOUNT-ID can be used as an explicit override when the
token response does not already contain one."
  reasoning-summary
  default-reasoning-effort
  codex-oauth
  codex-account-id
  codex-client-id
  codex-issuer
  codex-auth-url
  codex-token-url
  codex-scope
  codex-oauth-user-name
  codex-oauth-host-name
  codex-originator
  codex-store
  codex-instructions)

(defun llm-openai-responses--as-list (v)
  "Return V as a list when V is a vector or list."
  (cond
   ((vectorp v) (append v nil))
   ((listp v) v)
   (t nil)))

(defun llm-openai-responses--coerce-string (v)
  "Return V as a string when possible, otherwise nil."
  (cond
   ((null v) nil)
   ((stringp v) v)
   ((symbolp v) (symbol-name v))
   (t nil)))

(defun llm-openai-responses--normalize-base-url (url)
  "Return URL with exactly one trailing slash."
  (let ((trimmed (replace-regexp-in-string "/+\\'" "" (or url ""))))
    (concat trimmed "/")))

(defun llm-openai-responses--call-key (key)
  "Resolve KEY into a string when KEY is a function or string."
  (let ((value (if (functionp key) (funcall key) key)))
    (when (stringp value)
      (let ((trimmed (string-trim value)))
        (unless (string-empty-p trimmed)
          trimmed)))))

(defun llm-openai-responses--base64url-decode-string (s)
  "Decode base64url string S to UTF-8 text."
  (let* ((standard (replace-regexp-in-string "_" "/"
                                             (replace-regexp-in-string "-" "+" s)))
         (pad-len (% (- 4 (% (length standard) 4)) 4))
         (padded (concat standard (make-string pad-len ?=))))
    (decode-coding-string (base64-decode-string padded) 'utf-8)))

(defun llm-openai-responses--jwt-claims (token)
  "Return JWT TOKEN payload as an alist when possible."
  (when (and (stringp token) (string-match-p "\\." token))
    (let ((parts (split-string token "\\." t)))
      (when (= (length parts) 3)
        (condition-case nil
            (let ((payload (llm-openai-responses--base64url-decode-string (nth 1 parts))))
              (when (stringp payload)
                (json-parse-string payload :object-type 'alist :array-type 'list
                                   :null-object nil :false-object :false)))
          (error nil))))))

(defun llm-openai-responses--jwt-account-id (token)
  "Extract ChatGPT account id from JWT TOKEN when present."
  (when-let* ((claims (llm-openai-responses--jwt-claims token))
              (auth (assoc-default 'https://api.openai.com/auth claims)))
    (or (assoc-default 'chatgpt_account_id auth)
        (assoc-default "chatgpt_account_id" auth))))

(defun llm-openai-responses--codex-token-endpoint (provider)
  "Return the OAuth token endpoint for PROVIDER."
  (or (llm-openai-responses-codex-token-url provider)
      (concat (replace-regexp-in-string "/+\\'" ""
                                        (or (llm-openai-responses-codex-issuer provider)
                                            llm-openai-responses-default-codex-issuer))
              "/oauth/token")))

(defun llm-openai-responses--codex-auth-url (provider)
  "Return the OAuth authorization endpoint for PROVIDER."
  (or (llm-openai-responses-codex-auth-url provider)
      llm-openai-responses-default-codex-auth-url))

(defun llm-openai-responses--codex-scope (provider)
  "Return the OAuth scope string for PROVIDER."
  (or (llm-openai-responses-codex-scope provider)
      llm-openai-responses-default-codex-scope))

(defun llm-openai-responses--codex-login-scope (provider)
  "Return the browser-login OAuth scope string for PROVIDER."
  (or (llm-openai-responses-codex-scope provider)
      llm-openai-responses-default-codex-login-scope))

(defun llm-openai-responses--codex-originator (provider)
  "Return the Codex originator string for PROVIDER."
  (llm-openai-responses--coerce-string
   (or (llm-openai-responses-codex-originator provider)
       llm-openai-responses-default-codex-originator)))

(defun llm-openai-responses--build-codex-auth-url (provider redirect-uri state code-verifier)
  "Build the Codex browser-login authorization URL for PROVIDER.

REDIRECT-URI, STATE, and CODE-VERIFIER come from the local PKCE flow."
  (let ((params (list
                 "client_id" (or (llm-openai-responses-codex-client-id provider)
                                 llm-openai-responses-default-codex-client-id)
                 "response_type" "code"
                 "redirect_uri" redirect-uri
                 "scope" (llm-openai-responses--codex-login-scope provider)
                 "state" state
                 "code_challenge" (oauth2--get-challenge-from-verifier code-verifier)
                 "code_challenge_method" "S256"
                 "id_token_add_organizations" "true"
                 "codex_cli_simplified_flow" "true")))
    (when-let ((originator (llm-openai-responses--codex-originator provider)))
      (setq params (append params (list "originator" originator))))
    (apply #'oauth2--build-url
           (llm-openai-responses--codex-auth-url provider)
           params)))

(defun llm-openai-responses--codex-oauth-user-name (provider)
  "Return the explicit oauth2 cache user name for PROVIDER.

Codex OAuth providers must set `:codex-oauth-user-name' explicitly so each
provider's persisted session identity is unambiguous." 
  (let ((user-name (llm-openai-responses-codex-oauth-user-name provider)))
    (unless (and (stringp user-name) (not (string-empty-p user-name)))
      (user-error (concat "Codex OAuth providers must set :codex-oauth-user-name "
                          "explicitly")))
    user-name))

(defun llm-openai-responses--codex-oauth-host-name (provider)
  "Return the oauth2 cache host name for PROVIDER."
  (or (llm-openai-responses-codex-oauth-host-name provider)
      llm-openai-responses-default-codex-oauth-host-name))

(defun llm-openai-responses--codex-oauth-plstore-id (provider)
  "Return the oauth2 plstore id for PROVIDER."
  (oauth2-compute-id
   (llm-openai-responses--codex-auth-url provider)
   (llm-openai-responses--codex-token-endpoint provider)
   (llm-openai-responses--codex-scope provider)
   (or (llm-openai-responses-codex-client-id provider)
       llm-openai-responses-default-codex-client-id)
   (llm-openai-responses--codex-oauth-user-name provider)))

(defun llm-openai-responses--oauth2-load-cached-codex-token (provider)
  "Load and return the oauth2 token for PROVIDER from plstore."
  (oauth2--with-plstore
   (let* ((plstore-id (llm-openai-responses--codex-oauth-plstore-id provider))
          (plist (cdr (plstore-get plstore plstore-id))))
     (when-let* ((access-response (plist-get plist :access-response))
                 (refresh-token (cdr (assoc 'refresh_token access-response))))
       (make-oauth2-token
        :plstore-id plstore-id
        :client-id (or (llm-openai-responses-codex-client-id provider)
                       llm-openai-responses-default-codex-client-id)
        :client-secret ""
        :access-token (or (oauth2--get-from-request-cache
                           (plist-get plist :request-cache)
                           (llm-openai-responses--codex-oauth-host-name provider)
                           :access-token)
                          "")
        :refresh-token refresh-token
        :request-cache (plist-get plist :request-cache)
        :code-verifier (plist-get plist :code-verifier)
        :auth-url (llm-openai-responses--codex-auth-url provider)
        :token-url (llm-openai-responses--codex-token-endpoint provider)
        :access-response access-response)))))

(defun llm-openai-responses--cached-codex-token (provider)
  "Return PROVIDER's cached oauth2 token, loading it from plstore if needed."
  (let ((plstore-id (llm-openai-responses--codex-oauth-plstore-id provider)))
    (or (gethash plstore-id llm-openai-responses--codex-oauth-token-cache)
        (when-let ((token (llm-openai-responses--oauth2-load-cached-codex-token provider)))
          (puthash plstore-id token llm-openai-responses--codex-oauth-token-cache)
          token))))

(defun llm-openai-responses--oauth2-token-account-id (token provider)
  "Return account id for oauth2 TOKEN and PROVIDER."
  (let* ((access-response (oauth2-token-access-response token))
         (id-token (cdr (assoc 'id_token access-response)))
         (stored-account-id (cdr (assoc 'account_id access-response))))
    (or (llm-openai-responses-codex-account-id provider)
        stored-account-id
        (llm-openai-responses--jwt-account-id id-token)
        (llm-openai-responses--jwt-account-id (oauth2-token-access-token token)))))

(defun llm-openai-responses--oauth2-codex-auth (provider)
  "Return Codex auth plist from cached oauth2 state for PROVIDER."
  (let ((plstore-id (llm-openai-responses--codex-oauth-plstore-id provider)))
    (if-let* ((token (llm-openai-responses--cached-codex-token provider))
              (token (oauth2-refresh-access
                      token (llm-openai-responses--codex-oauth-host-name provider)))
              (account-id (llm-openai-responses--oauth2-token-account-id token provider)))
        (let ((access-token (oauth2-token-access-token token)))
          (unless (string-empty-p access-token)
            (list :access-token access-token :account-id account-id)))
      (remhash plstore-id llm-openai-responses--codex-oauth-token-cache)
      nil)))

(defun llm-openai-responses--random-state ()
  "Return a random OAuth state string."
  (secure-hash 'sha256
               (format "%s:%s:%s"
                       (emacs-pid)
                       (float-time (current-time))
                       (random most-positive-fixnum))))

(defun llm-openai-responses--callback-response (status title body)
  "Return an HTML callback response using STATUS, TITLE, and BODY."
  (format (concat "HTTP/1.1 %s\r\n"
                  "Content-Type: text/html; charset=utf-8\r\n"
                  "Connection: close\r\n\r\n"
                  "<!doctype html><html><head><meta charset=\"utf-8\">"
                  "<title>%s</title></head><body><h1>%s</h1><p>%s</p></body></html>")
          status title title body))

(defun llm-openai-responses--callback-param (params name)
  "Return first query param NAME from PARAMS."
  (car (alist-get name params nil nil #'string=)))

(defun llm-openai-responses--reply-to-callback-client (proc response)
  "Send HTTP RESPONSE to callback client PROC and close cleanly."
  (process-send-string proc response)
  (process-send-eof proc))

(defun llm-openai-responses--make-callback-log (result expected-state)
  "Return a server log function for callback RESULT and EXPECTED-STATE."
  (let ((filter (llm-openai-responses--make-callback-filter result expected-state)))
    (lambda (_server client _message)
      (set-process-filter client filter)
      (set-process-query-on-exit-flag client nil)
      (process-put client 'payload nil))))

(defun llm-openai-responses--make-callback-filter (result expected-state)
  "Return a process filter that stores callback data into RESULT.

EXPECTED-STATE is compared against the callback state parameter."
  (lambda (proc chunk)
    (let ((buffer (concat (or (process-get proc 'payload) "") chunk)))
      (process-put proc 'payload buffer)
      (when (string-match-p "\r?\n\r?\n" buffer)
        (let* ((request-line (car (split-string buffer "\r?\n" t)))
               (path+query (when (string-match "\\`GET \\([^ ]+\\) HTTP/" request-line)
                             (match-string 1 request-line)))
               (query (cadr (split-string (or path+query "") "?" t)))
               (params (url-parse-query-string (or query ""))))
           (cond
            ((null path+query)
            (llm-openai-responses--reply-to-callback-client
             proc
             (llm-openai-responses--callback-response
              "400 Bad Request"
              "Sign-in Failed"
              "Missing callback path.")))
           ((not (string-prefix-p "/auth/callback" path+query))
            (llm-openai-responses--reply-to-callback-client
             proc
             (llm-openai-responses--callback-response
              "404 Not Found"
              "Not Found"
              "This endpoint only handles /auth/callback.")))
           ((not (equal (llm-openai-responses--callback-param params "state") expected-state))
            (setcar result (cons :error "OAuth callback state mismatch"))
            (llm-openai-responses--reply-to-callback-client
             proc
             (llm-openai-responses--callback-response
              "400 Bad Request"
              "Sign-in Failed"
              "OAuth state mismatch.")))
           ((llm-openai-responses--callback-param params "error")
            (setcar result (cons :error
                                 (or (llm-openai-responses--callback-param params "error_description")
                                     (llm-openai-responses--callback-param params "error"))))
            (llm-openai-responses--reply-to-callback-client
             proc
             (llm-openai-responses--callback-response
              "400 Bad Request"
              "Sign-in Failed"
              "The OAuth provider returned an error.")))
           ((llm-openai-responses--callback-param params "code")
            (setcar result (cons :code (llm-openai-responses--callback-param params "code")))
            (llm-openai-responses--reply-to-callback-client
             proc
             (llm-openai-responses--callback-response
              "200 OK"
              "Sign-in Complete"
              "You can return to Emacs now.")))
           (t
            (setcar result (cons :error "Missing authorization code"))
            (llm-openai-responses--reply-to-callback-client
             proc
             (llm-openai-responses--callback-response
              "400 Bad Request"
              "Sign-in Failed"
              "Missing authorization code.")))))))))

(defun llm-openai-responses--callback-server-port (server)
  "Return the local port for callback SERVER."
  (let ((local (process-contact server :local)))
    (unless (vectorp local)
      (error "Expected vector network address from process-contact :local, got: %S" local))
    (or (and (> (length local) 0)
             (aref local (1- (length local))))
        (process-contact server :service))))

(defun llm-openai-responses--make-codex-callback-server (result state)
  "Start a localhost callback server for RESULT and STATE.

Prefer the official Codex callback port, then fall back to the registered
secondary port when the preferred port is already in use."
  (let ((ports (list llm-openai-responses-default-codex-callback-port
                     llm-openai-responses-fallback-codex-callback-port))
        server)
    (while (and ports (null server))
      (condition-case err
          (setq server
                (make-network-process
                 :name "llm-openai-responses-codex-login"
                 :server t
                 :host "127.0.0.1"
                 :service (car ports)
                 :family 'ipv4
                 :log (llm-openai-responses--make-callback-log result state)
                 :noquery t))
        (file-error
         (setq ports (cdr ports)))))
    (or server
        (user-error "Unable to start Codex OAuth callback server on localhost:%s or localhost:%s"
                    llm-openai-responses-default-codex-callback-port
                    llm-openai-responses-fallback-codex-callback-port))))

(defun llm-openai-responses--await-callback (server result &optional timeout)
  "Wait for callback SERVER to populate RESULT up to TIMEOUT seconds."
  (let ((deadline (+ (float-time (current-time))
                     (or timeout llm-openai-responses-codex-login-timeout-seconds))))
    (while (and (null (car result))
                (< (float-time (current-time)) deadline))
      (accept-process-output nil 1))
    (or (car result)
        (cons :error "Timed out waiting for OAuth callback"))))

(defun llm-openai-responses--persist-oauth2-token (provider token)
  "Persist oauth2 TOKEN for PROVIDER and return TOKEN."
  (let ((plstore-id (llm-openai-responses--codex-oauth-plstore-id provider)))
    (setf (oauth2-token-plstore-id token) plstore-id)
    (oauth2--with-plstore
     (oauth2--update-plstore plstore token))
    (puthash plstore-id token llm-openai-responses--codex-oauth-token-cache)
    token))

(defun llm-openai-responses--run-on-main-thread (fn &rest args)
  "Run FN with ARGS back on the main Emacs thread."
  (when fn
    (apply #'run-at-time 0 nil fn args)))

(cl-defstruct llm-openai-responses-codex-login-session
  provider
  result
  server
  redirect-uri
  auth-url
  code-verifier)

(defun llm-openai-responses--start-codex-browser-login (provider)
  "Create a Codex browser-login session for PROVIDER.

The returned session includes the callback server, redirect URI, auth URL, and
PKCE verifier.  Callers are responsible for opening the auth URL and finishing
or cancelling the session."
  (unless (and provider (llm-openai-responses-codex-oauth provider))
    (user-error "Codex browser login requires an explicit Codex OAuth provider"))
  (llm-openai-responses--codex-oauth-user-name provider)
  (let* ((state (llm-openai-responses--random-state))
         (code-verifier (oauth2--generate-code-verifier))
         (result (list nil))
         (server (llm-openai-responses--make-codex-callback-server result state))
         (port (llm-openai-responses--callback-server-port server))
         (redirect-uri (format "http://localhost:%s/auth/callback" port))
         (auth-url (llm-openai-responses--build-codex-auth-url
                     provider redirect-uri state code-verifier)))
    (make-llm-openai-responses-codex-login-session
     :provider provider
     :result result
     :server server
     :redirect-uri redirect-uri
     :auth-url auth-url
     :code-verifier code-verifier)))

(defun llm-openai-responses--cancel-codex-browser-login (session)
  "Tear down callback resources for Codex login SESSION."
  (when-let ((server (llm-openai-responses-codex-login-session-server session)))
    (when (process-live-p server)
      (delete-process server))))

(defun llm-openai-responses--finish-codex-browser-login (session)
  "Wait for callback completion and exchange tokens for SESSION."
  (let* ((provider (llm-openai-responses-codex-login-session-provider session))
         (result (llm-openai-responses-codex-login-session-result session))
         (server (llm-openai-responses-codex-login-session-server session))
         (redirect-uri (llm-openai-responses-codex-login-session-redirect-uri session))
         (code-verifier (llm-openai-responses-codex-login-session-code-verifier session)))
    (unwind-protect
        (pcase-let ((`(,kind . ,value)
                     (llm-openai-responses--await-callback server result)))
          (unless (eq kind :code)
            (user-error "%s" value))
          (oauth2-request-access
           (llm-openai-responses--codex-auth-url provider)
           (llm-openai-responses--codex-token-endpoint provider)
           (or (llm-openai-responses-codex-client-id provider)
               llm-openai-responses-default-codex-client-id)
           ""
           value
           redirect-uri
           (llm-openai-responses--codex-oauth-host-name provider)
           code-verifier))
      (llm-openai-responses--cancel-codex-browser-login session))))

(defun llm-openai-responses--codex-refresh-payload (provider refresh-token)
  "Return the refresh request payload for PROVIDER and REFRESH-TOKEN."
  (json-serialize
   `((grant_type . "refresh_token")
     (refresh_token . ,refresh-token)
     (client_id . ,(or (llm-openai-responses-codex-client-id provider)
                       llm-openai-responses-default-codex-client-id))
     (scope . "openid profile email offline_access"))))

(defun llm-openai-responses--url-response-json (url request-data)
  "POST REQUEST-DATA as JSON to URL and return parsed JSON on success."
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url-request-data (encode-coding-string request-data 'utf-8)))
    (when-let ((buffer (url-retrieve-synchronously url t t 15)))
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (when (and (boundp 'url-http-response-status)
                       (>= url-http-response-status 200)
                       (< url-http-response-status 300)
                       (re-search-forward "\r?\n\r?\n" nil t))
              (json-parse-buffer :object-type 'alist :array-type 'list
                                 :null-object nil :false-object :false)))
        (kill-buffer buffer)))))

(defun llm-openai-responses--write-json-file (path data)
  "Write DATA as JSON to PATH with user-only permissions."
  (make-directory (file-name-directory path) t)
  (with-temp-file path
    (insert (json-serialize data :null-object nil :false-object :false)))
  (set-file-modes path #o600))

(defun llm-openai-responses--refresh-codex-auth (provider auth-file auth-data)
  "Refresh AUTH-DATA from AUTH-FILE for PROVIDER when needed.

Return the updated auth-data alist on refresh success, otherwise AUTH-DATA."
  (let* ((tokens (assoc-default 'tokens auth-data))
         (refresh-token (assoc-default 'refresh_token tokens))
         (request-data (and (stringp refresh-token)
                            (not (string-empty-p refresh-token))
                            (llm-openai-responses--codex-refresh-payload provider refresh-token))))
    (if (not request-data)
        auth-data
      (let* ((response
              (llm-openai-responses--url-response-json
               (llm-openai-responses--codex-token-endpoint provider)
               request-data))
             (access-token (and response (assoc-default 'access_token response))))
        (if (not access-token)
            auth-data
          (let* ((id-token (or (assoc-default 'id_token response)
                               (assoc-default 'id_token tokens)))
                 (next-refresh-token (or (assoc-default 'refresh_token response)
                                         refresh-token))
                 (stored-account-id (or (assoc-default 'account_id tokens)
                                        (llm-openai-responses-codex-account-id provider)
                                        (llm-openai-responses--jwt-account-id id-token)
                                        (llm-openai-responses--jwt-account-id access-token)))
                 (updated (list
                           (cons 'auth_mode (or (assoc-default 'auth_mode auth-data) "chatgpt"))
                           (cons 'tokens
                                 (list (cons 'access_token access-token)
                                       (cons 'refresh_token next-refresh-token)
                                       (cons 'id_token id-token)
                                       (cons 'account_id stored-account-id)))
                           (cons 'last_refresh
                                 (format-time-string "%FT%TZ" (current-time) t)))))
            (llm-openai-responses--write-json-file auth-file updated)
            updated))))))

(defun llm-openai-responses--codex-auth (provider)
  "Return a plist with Codex OAuth auth details for PROVIDER."
  (or (llm-openai-responses--oauth2-codex-auth provider)
      (signal 'llm-provider-unconfigured
              '("No Codex OAuth access token available. Run `M-x llm-openai-responses-codex-login-start`."))))

(defun llm-openai-responses--complete-codex-login-session (session)
  "Wait for SESSION callback, exchange tokens, and persist the login."
  (let* ((provider (llm-openai-responses-codex-login-session-provider session))
         (token (llm-openai-responses--finish-codex-browser-login session))
         (account-id (llm-openai-responses--oauth2-token-account-id token provider)))
    (unless account-id
      (user-error "Codex OAuth login succeeded, but no account id was returned"))
    (setf (oauth2-token-access-response token)
          (cons (cons 'account_id account-id)
                (oauth2-token-access-response token)))
    (llm-openai-responses--persist-oauth2-token provider token)
    token))

;;;###autoload
(defun llm-openai-responses-codex-login-async
    (provider &optional open-url-fn success-fn error-fn)
  "Authenticate PROVIDER asynchronously and return the worker thread.

OPEN-URL-FN is called on the main thread with the authorization URL.  The
callback server and wait loop stay on the worker thread because Emacs network
processes are locked to the thread that created them.  SUCCESS-FN is called on
the main thread with the resulting token when login succeeds.  ERROR-FN is
called on the main thread with the signaled error object when login fails."
  (make-thread
   (lambda ()
     (condition-case err
         (let* ((session (llm-openai-responses--start-codex-browser-login provider))
                (_ (llm-openai-responses--run-on-main-thread
                    (or open-url-fn #'browse-url)
                    (llm-openai-responses-codex-login-session-auth-url session)))
                 (token (llm-openai-responses--complete-codex-login-session session)))
            (llm-openai-responses--run-on-main-thread success-fn token)
            token)
        (error
          (llm-openai-responses--run-on-main-thread error-fn err)
          nil)))
     "llm-openai-responses-codex-login"))

(defun llm-openai-responses-codex-login-show-url (display-name url)
  "Copy and display Codex login URL for DISPLAY-NAME."
  (kill-new url)
  (when (display-graphic-p)
    (gui-set-selection 'CLIPBOARD url))
  (with-current-buffer (get-buffer-create llm-openai-responses-codex-login-url-buffer-name)
    (erase-buffer)
    (insert url "\n")
    (goto-char (point-min))
    (display-buffer (current-buffer)))
  (message "Codex login URL copied for %s" display-name))

(defun llm-openai-responses-codex-login-success (display-name _token)
  "Handle successful Codex login completion for DISPLAY-NAME."
  (setq llm-openai-responses-codex-login-thread nil)
  (message "Codex OAuth login ready for %s" display-name))

(defun llm-openai-responses-codex-login-error (display-name err)
  "Handle failed Codex login completion for DISPLAY-NAME and ERR."
  (setq llm-openai-responses-codex-login-thread nil)
  (message "Codex OAuth login failed for %s: %s"
           display-name
           (error-message-string err)))

;;;###autoload
(defun llm-openai-responses-codex-login-start (provider &optional display-name manual-url)
  "Start Codex OAuth login for PROVIDER and return the worker thread.

DISPLAY-NAME is used in status messages.  When MANUAL-URL is non-nil, copy and
display the auth URL instead of opening a browser automatically."
  (let ((display-name (or display-name "Codex OAuth")))
    (when (and (threadp llm-openai-responses-codex-login-thread)
               (thread-live-p llm-openai-responses-codex-login-thread))
      (user-error "A Codex OAuth login is already in progress"))
    (let ((open-url-fn (if manual-url
                           (apply-partially #'llm-openai-responses-codex-login-show-url display-name)
                         #'browse-url))
          (success-fn (apply-partially #'llm-openai-responses-codex-login-success display-name))
          (error-fn (apply-partially #'llm-openai-responses-codex-login-error display-name)))
      (setq llm-openai-responses-codex-login-thread
            (llm-openai-responses-codex-login-async
             provider open-url-fn success-fn error-fn))
      (message "Codex OAuth login started for %s" display-name)
      llm-openai-responses-codex-login-thread)))

(defun llm-openai-responses--provider-base-url (provider)
  "Return the effective base URL for PROVIDER."
  (let ((url (llm-openai-compatible-url provider)))
    (llm-openai-responses--normalize-base-url
     (if (and (llm-openai-responses-codex-oauth provider)
              (or (null url)
                  (string-empty-p url)
                  (string= (replace-regexp-in-string "/+\\'" "" url)
                           (replace-regexp-in-string "/+\\'" "" llm-openai-responses-default-api-url))))
         llm-openai-responses-default-codex-url
       url))))

(defun llm-openai-responses--build-reasoning-request (provider prompt)
  "Build :reasoning request payload from PROVIDER and PROMPT.
When the prompt does not request a reasoning effort, fall back to the
provider's DEFAULT-REASONING-EFFORT."
  (let* ((reasoning (llm-chat-prompt-reasoning prompt))
         (effort (or (pcase reasoning
                       ('light "low")
                       ('medium "medium")
                       ('maximum "high")
                       (_ nil))
                     (llm-openai-responses-default-reasoning-effort provider)))
         (summary-mode (llm-openai-responses--coerce-string
                        (llm-openai-responses-reasoning-summary provider)))
         (payload nil))
    (when effort
      (setq payload (plist-put payload :effort effort)))
    (when summary-mode
      (setq payload (plist-put payload :summary summary-mode)))
    payload))

(defun llm-openai-responses--extract-reasoning (response)
  "Extract reasoning summary text from Responses API RESPONSE."
  (let (parts)
    (dolist (item (llm-openai-responses--as-list (assoc-default 'output response)))
      (when (equal (assoc-default 'type item) "reasoning")
        (let ((summary (assoc-default 'summary item))
              (text (assoc-default 'text item)))
          (when (stringp text)
            (push text parts))
          (cond
           ((stringp summary)
            (push summary parts))
           (t
            (dolist (entry (llm-openai-responses--as-list summary))
              (cond
               ((stringp entry)
                (push entry parts))
               ((listp entry)
                (when-let ((entry-text (or (assoc-default 'text entry)
                                           (assoc-default 'summary_text entry))))
                  (push (format "%s" entry-text) parts))))))))))
    (let ((joined (string-trim (string-join (nreverse parts) "\n"))))
      (unless (string-empty-p joined)
        joined))))

(defun llm-openai-responses--parse-tool-args (args)
  "Parse function-call ARGS JSON into an alist.

Responses API occasionally emits non-object JSON (for example `true').
Return an empty alist in that case so llm.el can raise a normal
missing-argument error instead of a low-level type error."
  (let* ((raw (if (or (null args) (string-empty-p args)) "{}" args))
         (parsed (json-parse-string raw :object-type 'alist :false-object :false)))
    (if (listp parsed)
        parsed
      nil)))

(defun llm-openai-responses--multibyte-string (value)
  "Return VALUE as a multibyte string."
  (let ((text (format "%s" value)))
    (if (multibyte-string-p text)
        text
      (decode-coding-string text 'utf-8 t))))

(defun llm-openai-responses--normalize-request-strings (value)
  "Return VALUE with all unibyte strings decoded as UTF-8 text."
  (cond
   ((stringp value) (llm-openai-responses--multibyte-string value))
   ((consp value)
    (cons (llm-openai-responses--normalize-request-strings (car value))
          (llm-openai-responses--normalize-request-strings (cdr value))))
   ((vectorp value)
    (vconcat (mapcar #'llm-openai-responses--normalize-request-strings value)))
   (t value)))

(defun llm-openai-responses--json-serialize-string (value)
  "Serialize VALUE to a multibyte JSON string.

Responses API function_call `arguments' is itself a JSON string nested inside
the outer JSON request.  `json-serialize' can return a unibyte string containing
UTF-8 bytes for non-ASCII text, which the outer request serialization rejects.
Decode the nested JSON bytes to text before embedding it in the request."
  (llm-openai-responses--multibyte-string (json-serialize value)))

(defun llm-openai-responses--tool-spec (tool)
  "Convert llm TOOL spec to Responses API function-tool shape."
  (let ((spec (llm-provider-utils-openai-tool-spec tool)))
    (if-let ((fun (plist-get spec :function)))
        (append (list :type "function") fun)
      spec)))

(defun llm-openai-responses--interaction->input-items (interaction)
  "Convert one llm INTERACTION to a list of Responses API input items."
  (if (llm-chat-prompt-interaction-tool-results interaction)
      (mapcar
       (lambda (tool-result)
         (list :type "function_call_output"
               :call_id (or (llm-chat-prompt-tool-result-call-id tool-result)
                            (format "call_%s"
                                    (md5
                                     (format "%s"
                                             (llm-chat-prompt-tool-result-result tool-result)))))
               :output (llm-openai-responses--multibyte-string
                         (llm-chat-prompt-tool-result-result tool-result))))
       (llm-chat-prompt-interaction-tool-results interaction))
    (let ((role (symbol-name (llm-chat-prompt-interaction-role interaction)))
          (content (llm-chat-prompt-interaction-content interaction)))
      (cond
       ((and (consp content) (llm-provider-utils-tool-use-p (car content)))
        (mapcar
         (lambda (tool-use)
           (list :type "function_call"
                 :call_id (or (llm-provider-utils-tool-use-id tool-use)
                              (format "call_%s"
                                      (md5 (llm-provider-utils-tool-use-name tool-use))))
                 :name (llm-provider-utils-tool-use-name tool-use)
                 :arguments (llm-openai-responses--json-serialize-string
                             (llm-provider-utils-tool-use-args tool-use))))
         content))
       ((llm-multipart-p content)
        (list
         (list :role role
               :content
               (vconcat
                (mapcar
                 (lambda (part)
                   (if (llm-media-p part)
                       (list :type "input_image"
                             :image_url
                             (concat "data:"
                                     (llm-media-mime-type part)
                                     ";base64,"
                                     (base64-encode-string (llm-media-data part) t)))
                     (list :type "input_text" :text (format "%s" part))))
                 (llm-multipart-parts content))))))
       (t
        (list (list :role role :content (format "%s" content))))))))

(defun llm-openai-responses--build-input (prompt)
  "Build Responses API :input items from llm chat PROMPT."
  (apply #'append
         (mapcar #'llm-openai-responses--interaction->input-items
                 (llm-chat-prompt-interactions prompt))))

(defun llm-openai-responses--extract-output-text (response)
  "Extract assistant text from Responses API RESPONSE alist."
  (or (assoc-default 'output_text response)
      (string-join
       (delq nil
             (mapcan
              (lambda (item)
                (when (equal (assoc-default 'type item) "message")
                  (mapcar
                   (lambda (part)
                     (when (or (equal (assoc-default 'type part) "output_text")
                               (equal (assoc-default 'type part) "text"))
                       (or (assoc-default 'text part)
                           (assoc-default 'output_text part))))
                   (append (assoc-default 'content item) nil))))
              (append (assoc-default 'output response) nil)))
       "\n")))

(defun llm-openai-responses--stream-usage-plist (payload)
  "Extract token accounting plist from streaming PAYLOAD when present."
  (when-let* ((response (or (assoc-default 'response payload) payload))
              (usage (assoc-default 'usage response)))
    (let* ((input-tokens (assoc-default 'input_tokens usage))
           (output-tokens (assoc-default 'output_tokens usage))
           (input-details (or (assoc-default 'input_tokens_details usage)
                              (assoc-default 'prompt_tokens_details usage)))
           (cached-input-tokens (assoc-default 'cached_tokens input-details)))
      (append
       (when input-tokens (list :input-tokens input-tokens))
       (when output-tokens (list :output-tokens output-tokens))
       (when cached-input-tokens
         (list :cached-input-tokens cached-input-tokens))))))

(defun llm-openai-responses--stream-tool-fragment (output-index item &optional replace-arguments)
  "Build one streaming tool fragment for OUTPUT-INDEX from ITEM.

When REPLACE-ARGUMENTS is non-nil, ITEM arguments replace earlier streamed
partials instead of being appended again."
  (when (equal (assoc-default 'type item) "function_call")
    (list 'index output-index
          'id (or (assoc-default 'call_id item)
                  (assoc-default 'id item))
          'call-id (assoc-default 'call_id item)
          'name (assoc-default 'name item)
          'arguments (or (assoc-default 'arguments item) "")
          'replace replace-arguments)))

(defun llm-openai-responses--handle-stream-event (payload receiver err-receiver)
  "Dispatch one streaming event PAYLOAD to RECEIVER or ERR-RECEIVER."
  (pcase (assoc-default 'type payload)
    ("response.output_text.delta"
     (when-let ((delta (assoc-default 'delta payload)))
       (funcall receiver `(:text ,delta))))
    ((or "response.reasoning_summary_text.delta"
         "response.reasoning_text.delta")
     (when-let ((delta (assoc-default 'delta payload)))
       (funcall receiver `(:reasoning ,delta))))
    ("response.output_item.added"
     (when-let ((fragment (llm-openai-responses--stream-tool-fragment
                           (assoc-default 'output_index payload)
                           (assoc-default 'item payload))))
       (funcall receiver `(:tool-uses-raw ,(vector fragment)))))
    ("response.function_call_arguments.delta"
     (funcall receiver
              `(:tool-uses-raw
                ,(vector
                  (list 'index (assoc-default 'output_index payload)
                        'id (assoc-default 'item_id payload)
                        'arguments (or (assoc-default 'delta payload) ""))))))
    ("response.function_call_arguments.done"
     (when-let ((fragment (llm-openai-responses--stream-tool-fragment
                           (assoc-default 'output_index payload)
                           (assoc-default 'item payload)
                           t)))
       (funcall receiver `(:tool-uses-raw ,(vector fragment)))))
    ("response.output_item.done"
     (when-let ((fragment (llm-openai-responses--stream-tool-fragment
                           (assoc-default 'output_index payload)
                           (assoc-default 'item payload)
                           t)))
       (funcall receiver `(:tool-uses-raw ,(vector fragment))))
     (when-let ((usage (llm-openai-responses--stream-usage-plist payload)))
       (funcall receiver usage)))
    ("response.completed"
     (when-let ((usage (llm-openai-responses--stream-usage-plist payload)))
       (funcall receiver usage)))
    ((or "response.failed" "error")
     (funcall err-receiver
              (or (assoc-default 'message (assoc-default 'error payload))
                  (assoc-default 'message payload)
                  "Responses streaming request failed")))
    (_ nil)))

(cl-defmethod llm-provider-chat-url ((provider llm-openai-responses))
  "Return Responses API URL for PROVIDER chat calls."
  (concat (llm-openai-responses--provider-base-url provider) "responses"))

(cl-defmethod llm-provider-headers ((provider llm-openai-responses))
  "Return request headers for PROVIDER."
  (if (llm-openai-responses-codex-oauth provider)
      (let* ((auth (llm-openai-responses--codex-auth provider))
             (token (encode-coding-string (plist-get auth :access-token) 'utf-8))
             (headers `(("Authorization" . ,(format "Bearer %s" token))
                        ("chatgpt-account-id" . ,(plist-get auth :account-id))
                        ("OpenAI-Beta" . "responses=experimental"))))
        (if-let ((originator (llm-openai-responses--codex-originator provider)))
            (append headers `(("originator" . ,originator)))
          headers))
    (cl-call-next-method)))

(cl-defmethod llm-capabilities ((provider llm-openai-responses))
  "Return capabilities for PROVIDER."
  (if (llm-openai-responses-codex-oauth provider)
      '(streaming tool-use streaming-tool-use json-response model-list image-input)
    '(streaming embeddings embeddings-batch tool-use streaming-tool-use
                json-response model-list image-input)))

(cl-defmethod llm-provider-chat-request ((provider llm-openai-responses) prompt streaming)
  "Build Responses API request plist from PROMPT for PROVIDER.

Use `:false' for non-streaming requests so `json-serialize' on Emacs 30
produces a valid JSON `false` literal.  `:json-false' is returned by
`json-parse-*', but it is not accepted as input by `json-serialize'."
  (llm-provider-utils-combine-to-system-prompt prompt llm-openai-example-prelude)
  (let ((request (list :model (llm-openai-chat-model provider)
                       :input (vconcat (llm-openai-responses--build-input prompt))
                       :stream (if streaming t :false))))
    (when (llm-openai-responses-codex-oauth provider)
      (setq request
            (plist-put request :instructions
                       (or (llm-openai-responses-codex-instructions provider) "")))
      (setq request
            (plist-put request :store
                       (if (llm-openai-responses-codex-store provider) t :false))))
    (when-let ((reasoning-opts (llm-openai-responses--build-reasoning-request provider prompt)))
      (setq request (plist-put request :reasoning reasoning-opts)))
    (when (and (not (llm-openai-responses-codex-oauth provider))
               (llm-chat-prompt-max-tokens prompt))
      (setq request
            (plist-put request :max_output_tokens (llm-chat-prompt-max-tokens prompt))))
    (when (llm-chat-prompt-temperature prompt)
      (setq request
            (plist-put request :temperature (* 2.0 (llm-chat-prompt-temperature prompt)))))
    (when-let ((format (llm-chat-prompt-response-format prompt)))
      (setq request
            (plist-put
             request
             :text
             (if (eq format 'json)
                 '(:format (:type "json_object"))
               (list :format
                     (list :type "json_schema"
                           :name "response"
                           :schema (append
                                    (llm-provider-utils-convert-to-serializable format)
                                    '(:additionalProperties :false))))))))
    (when-let ((tools (llm-chat-prompt-tools prompt)))
      (setq request
            (plist-put request :tools
                       (vconcat (mapcar #'llm-openai-responses--tool-spec tools)))))
    (when-let ((options (llm-chat-prompt-tool-options prompt)))
      (setq request
            (plist-put
             request :tool_choice
             (pcase (llm-tool-options-tool-choice options)
               ('auto "auto")
               ('none "none")
               ('any "required")
                ((pred stringp) (list :type "function"
                                      :name (llm-tool-options-tool-choice options)))
                (_ "auto")))))
    (llm-openai-responses--normalize-request-strings
     (llm-provider-merge-non-standard-params
      (llm-chat-prompt-non-standard-params prompt)
      request))))

(cl-defmethod llm-provider-chat-extract-result ((_ llm-openai-responses) response)
  "Extract final assistant text from Responses API RESPONSE."
  (llm-openai-responses--extract-output-text response))

(cl-defmethod llm-provider-extract-tool-uses ((_ llm-openai-responses) response)
  "Extract tool-use calls from Responses API RESPONSE."
  (delq nil
        (mapcar
         (lambda (item)
           (when (equal (assoc-default 'type item) "function_call")
             (make-llm-provider-utils-tool-use
              :id (or (assoc-default 'call_id item)
                      (assoc-default 'id item))
              :name (assoc-default 'name item)
              :args (llm-openai-responses--parse-tool-args
                     (assoc-default 'arguments item)))))
         (append (assoc-default 'output response) nil))))

(cl-defmethod llm-provider-extract-reasoning ((_ llm-openai-responses) response)
  "Extract reasoning summary text from Responses API RESPONSE."
  (llm-openai-responses--extract-reasoning response))

(cl-defmethod llm-provider-populate-tool-uses ((_ llm-openai-responses) prompt tool-uses)
  "Populate PROMPT with TOOL-USES as assistant tool calls."
  (llm-provider-utils-append-to-prompt prompt tool-uses nil 'assistant))

(cl-defmethod llm-provider-streaming-media-handler ((_ llm-openai-responses)
                                                    receiver err-receiver)
  "Handle Responses API streaming events."
  (cons 'text/event-stream
        (plz-event-source:text/event-stream
         :events `((response.output_text.delta
                    . ,(lambda (event)
                         (llm-openai-responses--handle-stream-event
                          (json-parse-string (plz-event-source-event-data event)
                                             :object-type 'alist :false-object :false)
                          receiver err-receiver)))
                   (response.reasoning_summary_text.delta
                    . ,(lambda (event)
                         (llm-openai-responses--handle-stream-event
                          (json-parse-string (plz-event-source-event-data event)
                                             :object-type 'alist :false-object :false)
                          receiver err-receiver)))
                   (response.reasoning_text.delta
                    . ,(lambda (event)
                         (llm-openai-responses--handle-stream-event
                          (json-parse-string (plz-event-source-event-data event)
                                             :object-type 'alist :false-object :false)
                          receiver err-receiver)))
                   (response.output_item.added
                    . ,(lambda (event)
                         (llm-openai-responses--handle-stream-event
                          (json-parse-string (plz-event-source-event-data event)
                                             :object-type 'alist :false-object :false)
                          receiver err-receiver)))
                   (response.output_item.done
                    . ,(lambda (event)
                         (llm-openai-responses--handle-stream-event
                          (json-parse-string (plz-event-source-event-data event)
                                             :object-type 'alist :false-object :false)
                          receiver err-receiver)))
                   (response.function_call_arguments.delta
                    . ,(lambda (event)
                         (llm-openai-responses--handle-stream-event
                          (json-parse-string (plz-event-source-event-data event)
                                             :object-type 'alist :false-object :false)
                          receiver err-receiver)))
                   (response.function_call_arguments.done
                    . ,(lambda (event)
                         (llm-openai-responses--handle-stream-event
                          (json-parse-string (plz-event-source-event-data event)
                                             :object-type 'alist :false-object :false)
                          receiver err-receiver)))
                   (response.completed
                    . ,(lambda (event)
                         (llm-openai-responses--handle-stream-event
                          (json-parse-string (plz-event-source-event-data event)
                                             :object-type 'alist :false-object :false)
                          receiver err-receiver)))
                   (response.failed
                    . ,(lambda (event)
                         (llm-openai-responses--handle-stream-event
                          (json-parse-string (plz-event-source-event-data event)
                                             :object-type 'alist :false-object :false)
                          receiver err-receiver)))
                   (error
                    . ,(lambda (event)
                         (llm-openai-responses--handle-stream-event
                          (json-parse-string (plz-event-source-event-data event)
                                             :object-type 'alist :false-object :false)
                          receiver err-receiver)))))))

(cl-defmethod llm-provider-collect-streaming-tool-uses ((_ llm-openai-responses) data)
  "Transform Responses streaming DATA fragments into tool-use structs."
  (let ((tools (make-hash-table :test 'equal))
        result)
    (cl-loop for fragment across data do
             (let* ((index (plist-get fragment 'index))
                    (tool (or (gethash index tools)
                              (let ((new-tool (make-llm-provider-utils-tool-use :args "")))
                                (puthash index new-tool tools)
                                new-tool)))
                    (id (or (plist-get fragment 'call-id)
                            (plist-get fragment 'id)))
                    (name (plist-get fragment 'name))
                    (arguments (plist-get fragment 'arguments)))
               (when id
                 (setf (llm-provider-utils-tool-use-id tool) id))
               (when name
                 (setf (llm-provider-utils-tool-use-name tool) name))
               (when arguments
                 (setf (llm-provider-utils-tool-use-args tool)
                       (if (plist-get fragment 'replace)
                           arguments
                         (concat (llm-provider-utils-tool-use-args tool)
                                 arguments))))))
    (maphash
     (lambda (_ tool)
       (setf (llm-provider-utils-tool-use-args tool)
             (llm-openai-responses--parse-tool-args
              (llm-provider-utils-tool-use-args tool)))
       (push tool result))
     tools)
    (nreverse result)))

(provide 'llm-openai-responses)

;;; llm-openai-responses.el ends here
