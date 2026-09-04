(require 'ert)
(require 'llm-openai-responses)

(defun llm-openai-responses-tests--http-get (port path)
  (with-temp-buffer
    (let ((proc (open-network-stream
                 "llm-openai-responses-test-client"
                 (current-buffer)
                 "127.0.0.1"
                 port)))
      (unwind-protect
          (progn
            (process-send-string
             proc
             (format "GET %s HTTP/1.1\r\nHost: localhost:%s\r\nConnection: close\r\n\r\n"
                     path port))
            (process-send-eof proc)
            (while (accept-process-output proc 0.1))
            (buffer-string))
        (when (process-live-p proc)
          (delete-process proc))))))

(ert-deftest llm-openai-responses-chat-request-enables-streaming ()
  (let* ((provider (make-llm-openai-responses
                    :key (lambda () "test")
                    :chat-model "gpt-5.4"
                    :embedding-model "text-embedding-3-small"))
         (prompt (llm-make-simple-chat-prompt "hello"))
         (request (llm-provider-chat-request provider prompt t)))
    (should (eq (plist-get request :stream) t))))

(ert-deftest llm-openai-responses-compatible-url-is-used-for-chat ()
  (let ((provider (make-llm-openai-responses
                   :url "http://127.0.0.1:11481/v1/"
                   :key (lambda () "test")
                   :chat-model "gemma"
                   :embedding-model "gemma")))
    (should (equal (llm-provider-chat-url provider)
                   "http://127.0.0.1:11481/v1/responses"))))

(ert-deftest llm-openai-responses-chat-request-serializes-non-streaming-false ()
  (let* ((provider (make-llm-openai-responses
                    :key (lambda () "test")
                    :chat-model "gpt-5.4"
                    :embedding-model "text-embedding-3-small"))
         (prompt (llm-make-simple-chat-prompt "hello"))
         (request (llm-provider-chat-request provider prompt nil))
         (json (json-serialize request)))
    (should (equal (plist-get request :stream) :false))
    (should (string-match-p "\"stream\":false" json))))

(ert-deftest llm-openai-responses-codex-request-forces-streaming-shape ()
  (let* ((provider (make-llm-openai-responses
                    :codex-oauth t
                    :codex-oauth-user-name "test-user"
                    :chat-model "gpt-5.4"))
         (prompt (llm-make-simple-chat-prompt "hello"))
         (request (llm-provider-chat-request provider prompt t))
         (json (json-serialize request)))
    (should (eq (plist-get request :stream) t))
    (should (equal (plist-get request :instructions) ""))
    (should (eq (plist-get request :store) :false))
    (should (string-match-p "\"stream\":true" json))
    (should (string-match-p "\"store\":false" json))))

(ert-deftest llm-openai-responses-codex-auth-caches-plstore-token ()
  (let ((llm-openai-responses--codex-oauth-token-cache
         (make-hash-table :test #'equal))
        (loads 0)
        (token (make-oauth2-token :access-token "access")))
    (cl-letf (((symbol-function 'llm-openai-responses--oauth2-load-cached-codex-token)
               (lambda (_provider)
                 (setq loads (1+ loads))
                 token))
              ((symbol-function 'llm-openai-responses--live-access-token)
               (lambda (cached-token _host-name) (oauth2-token-access-token cached-token)))
              ((symbol-function 'llm-openai-responses--oauth2-token-account-id)
               (lambda (_token _provider) "acct_123")))
      (let ((provider (make-llm-openai-responses
                       :codex-oauth t
                       :codex-oauth-user-name "test-user")))
        (should (equal (llm-openai-responses--oauth2-codex-auth provider)
                       '(:access-token "access" :account-id "acct_123")))
        (should (equal (llm-openai-responses--oauth2-codex-auth provider)
                       '(:access-token "access" :account-id "acct_123")))
        (should (= loads 1))))))

(ert-deftest llm-openai-responses-live-access-token-honours-expiry ()
  (let* ((now (oauth2--current-timestamp))
         (token (make-oauth2-token
                 :access-response '((expires_in . 3600))
                 :request-cache (oauth2--update-request-cache
                                 "chatgpt.com" "access" (- now 60)))))
    (should (equal (llm-openai-responses--live-access-token token "chatgpt.com")
                   "access"))
    (setf (oauth2-token-request-cache token)
          (oauth2--update-request-cache "chatgpt.com" "access" (- now 7200)))
    (should-not (llm-openai-responses--live-access-token token "chatgpt.com"))))

(ert-deftest llm-openai-responses-refresh-persists-rotated-refresh-token ()
  (let ((token (make-oauth2-token
                :access-token "old-access"
                :refresh-token "old-refresh"
                :access-response '((refresh_token . "old-refresh")
                                   (account_id . "acct_123")
                                   (expires_in . 3600))))
        (provider (make-llm-openai-responses
                   :codex-oauth t
                   :codex-oauth-user-name "test-user"))
        persisted)
    (cl-letf (((symbol-function 'llm-openai-responses--url-response-json)
               (lambda (_url _data)
                 '((access_token . "new-access")
                   (refresh_token . "new-refresh")
                   (expires_in . 3600))))
              ((symbol-function 'llm-openai-responses--persist-oauth2-token)
               (lambda (_provider tok) (setq persisted tok) tok)))
      (should (equal (llm-openai-responses--refresh-oauth2-token provider token)
                     "new-access"))
      (should (eq persisted token))
      (should (equal (oauth2-token-refresh-token token) "new-refresh"))
      (let ((response (oauth2-token-access-response token)))
        ;; The rotated refresh token replaces the old one, and entries a
        ;; refresh does not return survive.
        (should (equal (cdr (assq 'refresh_token response)) "new-refresh"))
        (should (equal (cdr (assq 'account_id response)) "acct_123")))
      (should (equal (llm-openai-responses--live-access-token token "chatgpt.com")
                     "new-access")))))

(ert-deftest llm-openai-responses-failed-refresh-keeps-stored-token ()
  (let ((token (make-oauth2-token
                :access-token "old-access"
                :refresh-token "old-refresh"
                :access-response '((refresh_token . "old-refresh")))))
    (cl-letf (((symbol-function 'llm-openai-responses--url-response-json)
               (lambda (_url _data) nil))
              ((symbol-function 'llm-openai-responses--persist-oauth2-token)
               (lambda (_provider _token) (error "Must not persist a failed refresh"))))
      (should-not (llm-openai-responses--refresh-oauth2-token
                   (make-llm-openai-responses :codex-oauth t
                                              :codex-oauth-user-name "test-user")
                   token))
      (should (equal (oauth2-token-refresh-token token) "old-refresh")))))

(ert-deftest llm-openai-responses-inhibits-url-credential-prompt ()
  ;; `url-registered-auth-schemes' is empty until url.el is set up.
  (url-do-setup)
  (cl-letf (((symbol-function 'url-basic-auth)
             (lambda (&rest _) (error "url.el must not prompt for credentials"))))
    ;; Without the guard url.el reaches its prompting auth handler.
    (should-error (url-get-authentication "https://auth.openai.com/oauth/token"
                                          nil "basic" t))
    (should-not (llm-openai-responses--without-url-auth-prompt
                  (url-get-authentication "https://auth.openai.com/oauth/token"
                                          nil "basic" t)))))

(ert-deftest llm-openai-responses-handle-stream-text-and-usage ()
  (let (events errors)
    (llm-openai-responses--handle-stream-event
     '((type . "response.output_text.delta") (delta . "Hi"))
     (lambda (payload) (push payload events))
     (lambda (msg) (push msg errors)))
    (llm-openai-responses--handle-stream-event
     '((type . "response.completed")
       (response . ((usage . ((input_tokens . 12)
                              (output_tokens . 7)
                              (input_tokens_details . ((cached_tokens . 9))))))))
     (lambda (payload) (push payload events))
     (lambda (msg) (push msg errors)))
    (should (equal errors nil))
    (should (equal (nreverse events)
                   '((:text "Hi")
                     (:input-tokens 12
                      :output-tokens 7
                      :cached-input-tokens 9))))))

(ert-deftest llm-openai-responses-collects-streaming-tool-uses ()
  (let* ((provider (make-llm-openai-responses
                    :key (lambda () "test")
                    :chat-model "gpt-5.4"
                    :embedding-model "text-embedding-3-small"))
         (raw (vconcat
               (list
                '(index 0 id "fc_1" call-id "call_1" name "lookup" arguments "")
                '(index 0 id "fc_1" arguments "{\"city\":" )
                '(index 0 id "fc_1" arguments " \"Paris\"}")
                '(index 0 id "fc_1" call-id "call_1" name "lookup"
                         arguments "{\"city\": \"Paris\"}" replace t))))
         (tools (llm-provider-collect-streaming-tool-uses provider raw))
         (tool (car tools)))
    (should (= (length tools) 1))
    (should (equal (llm-provider-utils-tool-use-id tool) "call_1"))
    (should (equal (llm-provider-utils-tool-use-name tool) "lookup"))
    (should (equal (assoc-default 'city (llm-provider-utils-tool-use-args tool)) "Paris"))))

(ert-deftest llm-openai-responses-chat-request-serializes-unicode-tool-arguments ()
  (let* ((provider (make-llm-openai-responses
                    :key (lambda () "test")
                    :chat-model "gpt-5.4"
                    :embedding-model "text-embedding-3-small"))
         (tool-use (make-llm-provider-utils-tool-use
                    :id "call_1"
                    :name "eval"
                    :args '((source . "(list \"☀️\" \"ok\")"))))
         (prompt (llm-make-chat-prompt "ACTIVE USER TURN"))
         (request nil))
    (llm-provider-populate-tool-uses provider prompt (list tool-use))
    (setq request (llm-provider-chat-request provider prompt t))
    (should (stringp (json-serialize request)))))

(ert-deftest llm-openai-responses-chat-request-serializes-unibyte-unicode-tool-output ()
  (let* ((provider (make-llm-openai-responses
                    :key (lambda () "test")
                    :chat-model "gpt-5.4"
                    :embedding-model "text-embedding-3-small"))
         (prompt (llm-make-chat-prompt "ACTIVE USER TURN"))
         (output (encode-coding-string "(:ok t :value \"☀️\")" 'utf-8 t))
         (request nil))
    (setf (llm-chat-prompt-interactions prompt)
          (append (llm-chat-prompt-interactions prompt)
                  (list (make-llm-chat-prompt-interaction
                         :role 'tool-results
                         :tool-results
                         (list (make-llm-chat-prompt-tool-result
                                :call-id "call_1"
                                :tool-name "eval"
                                :result output))))))
    (setq request (llm-provider-chat-request provider prompt t))
    (should (stringp (json-serialize request)))))

(ert-deftest llm-openai-responses-chat-request-serializes-unibyte-unicode-content ()
  (let* ((provider (make-llm-openai-responses
                    :key (lambda () "test")
                    :chat-model "gpt-5.4"
                    :embedding-model "text-embedding-3-small"))
         (prompt (llm-make-chat-prompt
                  (encode-coding-string "Hello ☀️" 'utf-8 t)))
         (request (llm-provider-chat-request provider prompt t)))
    (should (stringp (json-serialize request)))))

(ert-deftest llm-openai-responses-callback-server-port-supports-vector-contact ()
  (let ((server (make-network-process :name "llm-openai-responses-test-server"
                                      :server t
                                      :host "127.0.0.1"
                                      :service 0
                                      :family 'ipv4
                                      :noquery t)))
    (unwind-protect
        (should (integerp (llm-openai-responses--callback-server-port server)))
      (when (process-live-p server)
        (delete-process server)))))

(ert-deftest llm-openai-responses-codex-auth-url-matches-cli-shape ()
  (let* ((provider (make-llm-openai-responses
                    :codex-oauth t
                    :codex-oauth-user-name "test-user"
                    :chat-model "gpt-5.5"))
         (url (url-generic-parse-url
               (llm-openai-responses--build-codex-auth-url
                provider
                "http://localhost:1455/auth/callback"
                "state-123"
                "verifier-123")))
         (params (url-parse-query-string (url-filename url))))
    (should (equal (car (alist-get "scope" params nil nil #'string=))
                   "openid profile email offline_access api.connectors.read api.connectors.invoke"))
    (should (equal (car (alist-get "id_token_add_organizations" params nil nil #'string=))
                   "true"))
    (should (equal (car (alist-get "codex_cli_simplified_flow" params nil nil #'string=))
                   "true"))
    (should (equal (car (alist-get "originator" params nil nil #'string=))
                   "codex_cli_rs"))))

(ert-deftest llm-openai-responses-codex-callback-server-uses-registered-port ()
  (let ((server (llm-openai-responses--make-codex-callback-server (list nil) "state-123")))
    (unwind-protect
        (should (memq (llm-openai-responses--callback-server-port server)
                      '(1455 1457)))
      (when (process-live-p server)
        (delete-process server)))))

(ert-deftest llm-openai-responses-codex-callback-server-falls-back-to-secondary-port ()
  (let ((primary (make-network-process :name "llm-openai-responses-primary-port-guard"
                                       :server t
                                       :host "127.0.0.1"
                                       :service 1455
                                       :family 'ipv4
                                       :noquery t))
        server)
    (unwind-protect
        (progn
          (setq server (llm-openai-responses--make-codex-callback-server (list nil) "state-123"))
          (should (= (llm-openai-responses--callback-server-port server) 1457)))
      (when (process-live-p server)
        (delete-process server))
      (when (process-live-p primary)
        (delete-process primary)))))

(ert-deftest llm-openai-responses-callback-server-responds-to-non-callback-path ()
  (let* ((result (list nil))
         (server (llm-openai-responses--make-codex-callback-server result "state-123"))
         (port (llm-openai-responses--callback-server-port server)))
    (unwind-protect
        (let ((response (llm-openai-responses-tests--http-get port "/")))
          (should (string-match-p "404 Not Found" response))
          (should (string-match-p "This endpoint only handles /auth/callback" response))
          (should (null (car result))))
      (when (process-live-p server)
        (delete-process server)))))

(ert-deftest llm-openai-responses-codex-login-async-calls-success-callback ()
  (let (success-token error-value)
    (cl-letf (((symbol-function 'llm-openai-responses--start-codex-browser-login)
               (lambda (_provider)
                 (make-llm-openai-responses-codex-login-session
                  :provider (make-llm-openai-responses :codex-oauth t :codex-oauth-user-name "test" :chat-model "gpt-5.5")
                  :auth-url "https://example.test/login")))
              ((symbol-function 'llm-openai-responses--finish-codex-browser-login)
               (lambda (_session) (make-oauth2-token :access-response nil)))
              ((symbol-function 'llm-openai-responses--oauth2-token-account-id)
               (lambda (_token _provider) "acct_123"))
              ((symbol-function 'llm-openai-responses--persist-oauth2-token)
               (lambda (_provider token) token)))
      (let ((thread (llm-openai-responses-codex-login-async
                     'provider nil
                     (lambda (token) (setq success-token token))
                     (lambda (err) (setq error-value err)))))
        (thread-join thread)
        (while (and (null success-token) (null error-value))
          (sleep-for 0.01))
        (should (oauth2-token-p success-token))
        (should (null error-value))))))

(ert-deftest llm-openai-responses-codex-login-async-calls-error-callback ()
  (let (success-token error-value)
    (cl-letf (((symbol-function 'llm-openai-responses--start-codex-browser-login)
               (lambda (_provider)
                 (make-llm-openai-responses-codex-login-session
                  :provider (make-llm-openai-responses :codex-oauth t :codex-oauth-user-name "test" :chat-model "gpt-5.5")
                  :auth-url "https://example.test/login")))
              ((symbol-function 'llm-openai-responses--finish-codex-browser-login)
               (lambda (_session) (signal 'user-error '("boom")))))
      (let ((thread (llm-openai-responses-codex-login-async
                     'provider nil
                     (lambda (token) (setq success-token token))
                     (lambda (err) (setq error-value err)))))
        (thread-join thread)
        (while (and (null success-token) (null error-value))
          (sleep-for 0.01))
        (should (null success-token))
        (should (equal (car error-value) 'user-error))))))

(ert-deftest llm-openai-responses-codex-login-start-tracks-thread ()
  (let ((llm-openai-responses-codex-login-thread nil))
    (cl-letf (((symbol-function 'llm-openai-responses-codex-login-async)
               (lambda (_provider _open-url-fn _success-fn _error-fn)
                 (make-thread (lambda () nil) "llm-openai-responses-test-login"))))
      (let ((thread (llm-openai-responses-codex-login-start 'provider "Test Provider" t)))
        (should (threadp thread))
        (should (eq thread llm-openai-responses-codex-login-thread))))))

(ert-deftest llm-openai-responses-default-reasoning-effort-applies ()
  (let* ((provider (make-llm-openai-responses
                    :key (lambda () "test")
                    :chat-model "test-model"
                    :default-reasoning-effort "none"))
         (prompt (llm-make-simple-chat-prompt "hello"))
         (request (llm-provider-chat-request provider prompt nil)))
    (should (equal (plist-get (plist-get request :reasoning) :effort) "none"))))

(ert-deftest llm-openai-responses-prompt-reasoning-overrides-default-effort ()
  (let* ((provider (make-llm-openai-responses
                    :key (lambda () "test")
                    :chat-model "test-model"
                    :default-reasoning-effort "none"))
         (prompt (llm-make-chat-prompt "hello" :reasoning 'maximum))
         (request (llm-provider-chat-request provider prompt nil)))
    (should (equal (plist-get (plist-get request :reasoning) :effort) "high"))))

(ert-deftest llm-openai-responses-prompt-can-disable-reasoning ()
  (let* ((provider (make-llm-openai-responses
                    :key (lambda () "test")
                    :chat-model "test-model"
                    :default-reasoning-effort "medium"))
         (prompt (llm-make-chat-prompt "hello" :reasoning 'none))
         (request (llm-provider-chat-request provider prompt nil)))
    (should (equal (plist-get (plist-get request :reasoning) :effort) "none"))))

(ert-deftest llm-openai-responses-no-default-effort-sends-no-effort ()
  (let* ((provider (make-llm-openai-responses
                    :key (lambda () "test")
                    :chat-model "test-model"))
         (prompt (llm-make-simple-chat-prompt "hello"))
         (request (llm-provider-chat-request provider prompt nil)))
    (should-not (plist-get (plist-get request :reasoning) :effort))))
