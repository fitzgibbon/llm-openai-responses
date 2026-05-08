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

## Codex OAuth

`llm-openai-responses` can also talk to the Codex ChatGPT backend used by the
official Codex tooling.

```elisp
(setq my-codex-provider
      (make-llm-openai-responses
        :codex-oauth t
        :chat-model "gpt-5.5"
        :reasoning-summary "auto"))
```

When `:codex-oauth` is non-nil, the provider:

- reads `auth.json` from `CODEX_AUTH_JSON_PATH`, `CHATGPT_LOCAL_HOME`,
  `CODEX_HOME`, `~/.chatgpt-local/auth.json`, or `~/.codex/auth.json`
- refreshes the OAuth access token using the stored refresh token when needed
- sends requests to `https://chatgpt.com/backend-api/codex/responses` by default

Optional constructor keywords for Codex mode:

- `:codex-auth-file`
- `:codex-account-id`
- `:codex-client-id`
- `:codex-issuer`
- `:codex-token-url`
- `:codex-originator`
- `:codex-store`
- `:codex-instructions`
