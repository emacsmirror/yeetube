# AGENTS.md

**WARNING** as you work on this project try to keep this file up-to-date for future sessions

## Project Overview

YeeTube is a YouTube front-end for GNU Emacs. It scrapes YouTube search results, plays videos via mpv, and downloads with yt-dlp. Licensed under GPL-3.0+.

## Build & Development

A Makefile drives compile/test/lint. When `nix` is available, targets
re-enter themselves through `nix develop` (a dev shell from `flake.nix`
providing Emacs with `compat` and `keymap-popup`); set
`YEETUBE_ENV_WRAPPED=1` to skip the wrapping:

- `make compile` — byte-compile all modules in `SRCS`
- `make test` — run the ERT suites in `TESTS` (under `test/`)
- `make lint` — checkdoc over `SRCS`
- `make dev` — compile + lint + test
- `make load` — reload all modules into a running Emacs via emacsclient
- `nix flake check` — run the test suite as a Nix check

Track `flake.lock` together with `flake.nix`; it is generated JSON, never
hand-edit it (refresh with `nix flake update nixpkgs`). New source or test
files must be registered in the Makefile `SRCS`/`TESTS` lists. When
compiling manually, add the local keymap-popup checkout to the load path
(e.g. `-L ../keymap-popup`).

**Dependencies:** `emacs >= 29.1`, `compat >= 29.1.4.2`, `keymap-popup >= 0.2.0`
**External tools:** `mpv`, `yt-dlp`, optionally `torsocks`

## Architecture

Eight modules with a clear layering:

- **yeetube.el** — Backend-neutral orchestration. HTTP transport
  (`yeetube--fetch`, optional Tor/SOCKS routing), search/pagination/channel
  commands dispatching through the backend generics, bookmarks (persisted
  as s-expressions), popup keymap definitions (`yeetube-mode-map` and the
  `yeetube-settings-map` submenu, via `keymap-popup-define`), download
  orchestration.
- **yeetube-backend.el** — Generic backend interface: `cl-defgeneric`s
  dispatching on the `yeetube-backend` symbol (default `youtube`).
  Backends return request-spec plists (`:url`, optional `:method`
  `:headers` `:data`); the core owns all transport. Adding a backend means
  implementing these generics in a new module.
- **yeetube-youtube.el** — YouTube backend. Search/channel/InnerTube
  continuation request construction, sort filter codes, invidious browse
  URLs, RSS feed parsing. Page parsing delegates to yeetube-scraper.el.
- **yeetube-scraper.el** — Pure `ytInitialData` JSON parsing. Side-effect
  free; input alists, output item plists.
- **yeetube-ui.el** — Display layer. Tabulated-list rows, faces, sort
  functions, async thumbnails via `url-queue-retrieve`.
- **yeetube-mpv.el** — MPV integration. Process management, remote control
  via keypresses, video quality selection, modeline display.
- **yeetube-download.el** — yt-dlp download helpers.
- **yeetube-ol.el** — Org-mode link types (`yt-video:` and `yt-playlist:`)
  for store/follow/export.

**Data flow:** `yeetube-search` → `yeetube-backend-search-request` →
`yeetube--fetch` (optional Tor/SOCKS) → `yeetube-backend-parse-page`
(YouTube: `yeetube-scraper-parse`) → item plists → `yeetube-ui-render` +
async thumbnail fetch → display in `yeetube-mode` (derived from
`tabulated-list-mode`) → user action → `yeetube-mpv-play` or
`yeetube-download-video`.

The player function is pluggable via `yeetube-play-function` (defaults to
`yeetube-mpv-play`). YouTube-specific user options were renamed to
`yeetube-youtube-*` in 2.4.0 (`yeetube-youtube-filter`,
`yeetube-youtube-video-url`, `yeetube-youtube-playlist-url`,
`yeetube-youtube-invidious-instances`); the old names survive as obsolete
variable aliases.
