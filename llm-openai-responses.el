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

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'llm)
(require 'llm-openai)
(require 'llm-provider-utils)

(cl-defstruct (llm-openai-responses (:include llm-openai))
  "OpenAI provider that uses the /v1/responses API for chat calls.

REASONING-SUMMARY controls whether the Responses API should return
reasoning summaries.  nil leaves provider defaults unchanged.  Non-nil
requests a summary string mode (for example \"auto\")."
  reasoning-summary)

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
         (parsed (json-parse-string raw :object-type 'alist)))
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
  (llm-openai--url provider "responses"))

(cl-defmethod llm-capabilities ((_ llm-openai-responses))
  "Return capabilities for this provider." 
  '(streaming embeddings embeddings-batch tool-use streaming-tool-use
              json-response model-list image-input))

(cl-defmethod llm-provider-chat-request ((provider llm-openai-responses) prompt streaming)
  "Build Responses API request plist from PROMPT for PROVIDER." 
  (llm-provider-utils-combine-to-system-prompt prompt llm-openai-example-prelude)
  (let ((request (list :model (llm-openai-chat-model provider)
                       :input (vconcat (llm-openai-responses--build-input prompt))
                       :stream (if streaming t :json-false))))
    (when-let ((reasoning-opts (llm-openai-responses--build-reasoning-request provider prompt)))
      (setq request (plist-put request :reasoning reasoning-opts)))
    (when (llm-chat-prompt-max-tokens prompt)
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
      request)))

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
                                             :object-type 'alist)
                          receiver err-receiver)))
                   (response.reasoning_summary_text.delta
                    . ,(lambda (event)
                         (llm-openai-responses--handle-stream-event
                          (json-parse-string (plz-event-source-event-data event)
                                             :object-type 'alist)
                          receiver err-receiver)))
                   (response.reasoning_text.delta
                    . ,(lambda (event)
                         (llm-openai-responses--handle-stream-event
                          (json-parse-string (plz-event-source-event-data event)
                                             :object-type 'alist)
                          receiver err-receiver)))
                   (response.output_item.added
                    . ,(lambda (event)
                         (llm-openai-responses--handle-stream-event
                          (json-parse-string (plz-event-source-event-data event)
                                             :object-type 'alist)
                          receiver err-receiver)))
                   (response.output_item.done
                    . ,(lambda (event)
                         (llm-openai-responses--handle-stream-event
                          (json-parse-string (plz-event-source-event-data event)
                                             :object-type 'alist)
                          receiver err-receiver)))
                   (response.function_call_arguments.delta
                    . ,(lambda (event)
                         (llm-openai-responses--handle-stream-event
                          (json-parse-string (plz-event-source-event-data event)
                                             :object-type 'alist)
                          receiver err-receiver)))
                   (response.function_call_arguments.done
                    . ,(lambda (event)
                         (llm-openai-responses--handle-stream-event
                          (json-parse-string (plz-event-source-event-data event)
                                             :object-type 'alist)
                          receiver err-receiver)))
                   (response.completed
                    . ,(lambda (event)
                         (llm-openai-responses--handle-stream-event
                          (json-parse-string (plz-event-source-event-data event)
                                             :object-type 'alist)
                          receiver err-receiver)))
                   (response.failed
                    . ,(lambda (event)
                         (llm-openai-responses--handle-stream-event
                          (json-parse-string (plz-event-source-event-data event)
                                             :object-type 'alist)
                          receiver err-receiver)))
                   (error
                    . ,(lambda (event)
                         (llm-openai-responses--handle-stream-event
                          (json-parse-string (plz-event-source-event-data event)
                                             :object-type 'alist)
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
