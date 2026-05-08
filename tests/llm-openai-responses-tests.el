(require 'ert)
(require 'llm-openai-responses)

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

(ert-deftest llm-openai-responses-handle-stream-text-and-usage ()
  (let (events errors)
    (llm-openai-responses--handle-stream-event
     '((type . "response.output_text.delta") (delta . "Hi"))
     (lambda (payload) (push payload events))
     (lambda (msg) (push msg errors)))
    (llm-openai-responses--handle-stream-event
     '((type . "response.completed")
       (response . ((usage . ((input_tokens . 12) (output_tokens . 7))))) )
     (lambda (payload) (push payload events))
     (lambda (msg) (push msg errors)))
    (should (equal errors nil))
    (should (equal (nreverse events)
                   '((:text "Hi") (:input-tokens 12 :output-tokens 7))))))

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
