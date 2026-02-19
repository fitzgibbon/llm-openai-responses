;;; llm-openai-responses.el --- Responses API provider for llm-openai -*- lexical-binding: t; package-lint-main-file: "llm-openai-responses.el" -*-

;; Author: llm-openai-responses contributors
;; Maintainer: llm-openai-responses contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (llm "0") (llm-openai "0"))
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
  "OpenAI provider that uses the /v1/responses API for chat calls.")

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

(cl-defmethod llm-provider-chat-url ((provider llm-openai-responses))
  "Return Responses API URL for PROVIDER chat calls." 
  (llm-openai--url provider "responses"))

(cl-defmethod llm-capabilities ((_ llm-openai-responses))
  "Return capabilities for this provider.

Streaming is omitted because llm.el's stock OpenAI SSE decoder expects chat
completions framing, not Responses events." 
  '(embeddings embeddings-batch tool-use json-response model-list image-input))

(cl-defmethod llm-provider-chat-request ((provider llm-openai-responses) prompt _streaming)
  "Build Responses API request plist from PROMPT for PROVIDER." 
  (llm-provider-utils-combine-to-system-prompt prompt llm-openai-example-prelude)
  (let ((request (list :model (llm-openai-chat-model provider)
                       :input (vconcat (llm-openai-responses--build-input prompt)))))
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
              :args (json-parse-string
                     (let ((args (assoc-default 'arguments item)))
                       (if (or (null args) (string-empty-p args)) "{}" args))
                     :object-type 'alist))))
         (append (assoc-default 'output response) nil))))

(cl-defmethod llm-provider-populate-tool-uses ((_ llm-openai-responses) prompt tool-uses)
  "Populate PROMPT with TOOL-USES as assistant tool calls." 
  (llm-provider-utils-append-to-prompt prompt tool-uses nil 'assistant))

(provide 'llm-openai-responses)

;;; llm-openai-responses.el ends here
