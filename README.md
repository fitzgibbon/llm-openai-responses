# llm-openai-responses

`llm-openai-responses` adds an `llm.el` provider that sends chat requests to
OpenAI's Responses API endpoint (`/v1/responses`) instead of Chat Completions.

## Example

```elisp
(require 'llm-openai)
(require 'llm-openai-responses)

(setq madrigal-llm-provider
      (make-llm-openai-responses
        :key #'my/get-openai-api-key-fs
        :chat-model "gpt-5.2-codex"
        :reasoning-summary "auto"
        :embedding-model "text-embedding-3-small"))
```

`reasoning-summary` can be set to a summary mode string supported by the
Responses API (for example `"auto"`) to request reasoning summaries in
provider output.
