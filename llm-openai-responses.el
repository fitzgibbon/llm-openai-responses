;;; llm-openai-responses.el --- Responses API provider for llm-openai -*- lexical-binding: t; package-lint-main-file: "llm-openai-responses.el" -*-

;; Author: llm-openai-responses contributors
;; Maintainer: llm-openai-responses contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (llm "0"))
;; Keywords: ai, llm
;; URL: https://github.com/fitzgibbon/llm-openai-responses
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This package adds an llm.el provider that talks to OpenAI's Responses API
;; endpoint (/v1/responses) while reusing llm-openai authentication, model, and
;; embedding configuration.
;;
;; It also supports the Codex / ChatGPT OAuth-backed Responses endpoint used by
;; the official Codex tooling.  In that mode, the provider loads and refreshes
;; tokens from `auth.json' and sends requests to
;; `https://chatgpt.com/backend-api/codex/responses'.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'llm)
(require 'llm-openai)
(require 'llm-provider-utils)

(defconst llm-openai-responses-default-api-url "https://api.openai.com/v1/"
  "Default OpenAI Responses API base URL.")

(defconst llm-openai-responses-default-codex-url "https://chatgpt.com/backend-api/codex/"
  "Default Codex OAuth Responses API base URL.")

(defconst llm-openai-responses-default-codex-client-id "app_EMoamEEZ73f0CkXaXp7hrann"
  "Default OAuth client id used by Codex ChatGPT auth.")

(defconst llm-openai-responses-default-codex-issuer "https://auth.openai.com"
  "Default OAuth issuer used by Codex ChatGPT auth.")

(defconst llm-openai-responses-codex-refresh-expiry-margin-seconds (* 5 60)
  "Refresh Codex OAuth tokens this many seconds before access-token expiry.")

(defconst llm-openai-responses-codex-refresh-interval-seconds (* 55 60)
  "Refresh Codex OAuth tokens this often even if JWT expiry is unavailable.")

(cl-defstruct (llm-openai-responses
               (:include llm-openai-compatible
                         (url llm-openai-responses-default-api-url)))
  "OpenAI provider that uses the /v1/responses API for chat calls.

REASONING-SUMMARY controls whether the Responses API should return
reasoning summaries.  nil leaves provider defaults unchanged.  Non-nil
requests a summary string mode (for example \"auto\").

When CODEX-OAUTH is non-nil, the provider talks to the Codex ChatGPT backend
instead of the standard OpenAI API.  Tokens are loaded from CODEX-AUTH-FILE or
the standard Codex auth.json search paths and refreshed using the stored
refresh token when needed.  CODEX-ACCOUNT-ID can be used as an explicit
override when the auth file does not already contain one."
  reasoning-summary
  codex-oauth
  codex-auth-file
  codex-account-id
  codex-client-id
  codex-issuer
  codex-token-url
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

(defun llm-openai-responses--parse-iso-time (value)
  "Return VALUE parsed as Emacs time or nil when invalid."
  (when (and (stringp value) (not (string-empty-p value)))
    (condition-case nil
        (date-to-time value)
      (error nil))))

(defun llm-openai-responses--codex-auth-file-candidates (&optional explicit-file)
  "Return candidate auth.json paths for Codex OAuth.

EXPLICIT-FILE takes precedence when non-nil."
  (delete-dups
   (delq nil
         (list explicit-file
               (let ((path (string-trim (or (getenv "CODEX_AUTH_JSON_PATH") ""))))
                 (unless (string-empty-p path) path))
               (let ((home (string-trim (or (getenv "CHATGPT_LOCAL_HOME") ""))))
                 (unless (string-empty-p home)
                   (expand-file-name "auth.json" home)))
               (let ((home (string-trim (or (getenv "CODEX_HOME") ""))))
                 (unless (string-empty-p home)
                   (expand-file-name "auth.json" home)))
               (expand-file-name "~/.chatgpt-local/auth.json")
               (expand-file-name "~/.codex/auth.json")))))

(defun llm-openai-responses--read-json-file (path)
  "Parse PATH as JSON and return an alist."
  (when (and path (file-readable-p path))
    (with-temp-buffer
      (insert-file-contents path)
      (json-parse-buffer :object-type 'alist :array-type 'list
                         :null-object nil :false-object :false))))

(defun llm-openai-responses--find-codex-auth-file (&optional explicit-file)
  "Return the first readable Codex auth file path and parsed contents."
  (catch 'found
    (dolist (candidate (llm-openai-responses--codex-auth-file-candidates explicit-file))
      (when-let ((data (llm-openai-responses--read-json-file candidate)))
        (throw 'found (cons candidate data))))))

(defun llm-openai-responses--codex-token-expiring-p (access-token last-refresh)
  "Return non-nil when ACCESS-TOKEN should be refreshed.

LAST-REFRESH is the stored auth-file timestamp string."
  (let ((now (float-time (current-time))))
    (or (not (and (stringp access-token) (not (string-empty-p access-token))))
        (when-let* ((claims (llm-openai-responses--jwt-claims access-token))
                    (exp (assoc-default 'exp claims)))
          (<= (* (float exp) 1000.0)
              (* (+ now llm-openai-responses-codex-refresh-expiry-margin-seconds) 1000.0)))
        (when-let ((refreshed-at (llm-openai-responses--parse-iso-time last-refresh)))
          (<= (float-time refreshed-at)
              (- now llm-openai-responses-codex-refresh-interval-seconds))))))

(defun llm-openai-responses--codex-token-endpoint (provider)
  "Return the OAuth token endpoint for PROVIDER."
  (or (llm-openai-responses-codex-token-url provider)
      (concat (replace-regexp-in-string "/+\\'" ""
                                        (or (llm-openai-responses-codex-issuer provider)
                                            llm-openai-responses-default-codex-issuer))
              "/oauth/token")))

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
  (let* ((key-token (llm-openai-responses--call-key (llm-openai-key provider)))
         (explicit-account-id (llm-openai-responses-codex-account-id provider))
         (auth-entry (llm-openai-responses--find-codex-auth-file
                      (llm-openai-responses-codex-auth-file provider)))
         (auth-file (car-safe auth-entry))
         (auth-data (cdr-safe auth-entry)))
    (when (and auth-data auth-file)
      (let* ((tokens (assoc-default 'tokens auth-data))
             (access-token (assoc-default 'access_token tokens))
             (last-refresh (assoc-default 'last_refresh auth-data)))
        (when (llm-openai-responses--codex-token-expiring-p access-token last-refresh)
          (setq auth-data (llm-openai-responses--refresh-codex-auth provider auth-file auth-data)))))
    (let* ((tokens (assoc-default 'tokens auth-data))
           (access-token (or key-token
                             (assoc-default 'access_token tokens)))
           (id-token (assoc-default 'id_token tokens))
           (account-id (or explicit-account-id
                           (assoc-default 'account_id tokens)
                           (llm-openai-responses--jwt-account-id id-token)
                           (llm-openai-responses--jwt-account-id access-token))))
      (unless (and (stringp access-token) (not (string-empty-p access-token)))
        (signal 'llm-provider-unconfigured
                '("No Codex OAuth access token available. Run `codex login` to create auth.json.")))
      (unless (and (stringp account-id) (not (string-empty-p account-id)))
        (signal 'llm-provider-unconfigured
                '("No Codex OAuth account id available. Re-authenticate with `codex login`.")))
      (list :access-token access-token :account-id account-id))))

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
  "Build :reasoning request payload from PROVIDER and PROMPT."
  (let* ((reasoning (llm-chat-prompt-reasoning prompt))
         (effort (pcase reasoning
                   ('light "low")
                   ('medium "medium")
                   ('maximum "high")
                   (_ nil)))
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
               :output (format "%s" (llm-chat-prompt-tool-result-result tool-result))))
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
                 :arguments (json-serialize (llm-provider-utils-tool-use-args tool-use))))
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
    (let ((input-tokens (assoc-default 'input_tokens usage))
          (output-tokens (assoc-default 'output_tokens usage)))
      (append
       (when input-tokens (list :input-tokens input-tokens))
       (when output-tokens (list :output-tokens output-tokens))))))

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
        (if-let ((originator (llm-openai-responses--coerce-string
                              (or (llm-openai-responses-codex-originator provider)
                                  "pi"))))
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
    (llm-provider-merge-non-standard-params
     (llm-chat-prompt-non-standard-params prompt)
     request))

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
