# GitHub (and GitLab) Agent

Many of us enjoy working on a project where issues are detailed,
well-written and kept up to date. There's just one problem with that:
editing issues is not fun.

With Git(Hub/Lab) Agent, you can sync your issues as text files to some
local folder, in fact a git repo, and use your favorite editor for
editing, and run a periodic "sync" command doing the least surprising
job at syncing the issues.

## Installation

**Binary installs** (Linux and macOS at the moment) are available via npm:

```
npm install -g github-agent
```

**Source install**

Install [Stack](https://haskellstack.org), then

```
stack install
```

This will put `github-agent` into `~/.local/bin`.

## Usage

Copy `config.dhall.example` to `~/.github-agent.dhall` for usage.

Usage:

- `github-agent sync-in` -- sync issues from the repo and put under
  `<number>.md` in some local git repo path, then commit the changes
- `github-agent sync-out` -- for every modified issue, check if it
  wasn't modified on server, and edit it in the repo, commit changes

You can use `~` in your paths to refer to your `$HOME` directory.

## Misc

TODO:

- add parameter to select a repo to sync (otherwise takes too long to sync many repos)
- gitlab: don't list all the issues in the `getIssueInfo` every time (add appropriate API in gitlab-haskell)
