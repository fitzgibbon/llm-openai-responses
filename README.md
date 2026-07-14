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

`default-reasoning-effort` sets a reasoning effort (for example `"none"`,
`"low"`, `"medium"`, `"high"`) sent whenever a prompt does not request an
effort itself. The value is passed through verbatim; which values are valid
depends on the backend and model — OpenAI-compatible local servers such as
mlx-openai-server treat `"none"` as disabling thinking entirely. A prompt
that sets `:reasoning` still takes precedence.

## Codex OAuth

`llm-openai-responses` can also talk to the Codex ChatGPT backend used by the
official Codex tooling.

```elisp
(setq my-codex-provider
      (make-llm-openai-responses
        :codex-oauth t
        :codex-oauth-user-name "openai-fs-codex"
        :chat-model "gpt-5.5"
        :reasoning-summary "auto"))
```

When `:codex-oauth` is non-nil, the provider:

- uses native oauth2.el cached login state
- refreshes the OAuth access token using the stored refresh token when needed
- sends requests to `https://chatgpt.com/backend-api/codex/responses` by default

To create a native Emacs oauth2 login, pass an explicit provider object:

```elisp
(llm-openai-responses-codex-login my-codex-provider)
```

That function:

- opens the OpenAI browser login flow
- listens for the localhost callback in Emacs
- exchanges the returned authorization code with PKCE enabled
- stores the token in `oauth2.plstore`

`llm-openai-responses` does not maintain its own named provider registry, so
interactive provider selection belongs in the calling application, not in the
package itself.

Optional constructor keywords for Codex mode:

- `:codex-account-id`
- `:codex-client-id`
- `:codex-issuer`
- `:codex-auth-url`
- `:codex-token-url`
- `:codex-scope`
- `:codex-oauth-user-name`
- `:codex-oauth-host-name`
- `:codex-originator`
- `:codex-store`
- `:codex-instructions`
