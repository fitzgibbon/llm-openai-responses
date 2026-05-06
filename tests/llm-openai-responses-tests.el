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
