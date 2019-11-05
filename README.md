# github-agent

Copy `config.json.example` to `config.json` for usage.

Usage:

- `github-agent sync-in` -- sync issues from the repo and put under
  `<number>.md` in some local git repo path, then commit the changes
- `github-agent sync-out` -- for every modified issue, check if it
  wasn't modified on server, and edit it in the repo, commit changes
