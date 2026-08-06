# yeetube

YouTube front-end for GNU Emacs: scrape search/channel results, play with mpv,
download with yt-dlp. GPL-3.0+. Emacs >= 29.1; `compat`, `keymap-popup`.

## Public safety

- Public document. No local paths, users, hosts, private topology, secrets,
  tokens, private issues, or personal config.
- Repository-relative paths and generic examples only.
- Keep durable architecture and contributor rules; drop session choreography.

## Architecture

Dependencies flow up. Core owns HTTP; backends never transport. Scraper pure.

- Core (`yeetube.el`): `yeetube--fetch` (optional Tor/SOCKS), search/pagination/
  channel commands, bookmarks, download orchestration, popup keymaps.
- Backend (`yeetube-backend.el`): `cl-defgeneric`s on `yeetube-backend` (default
  `youtube`). Request plists (`:url`, optional `:method` `:headers` `:data`);
  core fetches. New backend = new module implementing generics only.
- YouTube (`yeetube-youtube.el` + `yeetube-scraper.el`): requests/filters/
  Invidious/RSS; `ytInitialData` → item plists (scraper: no network/process).
- UI/player (`yeetube-ui.el`, `yeetube-mpv.el`, `yeetube-download.el`,
  `yeetube-ol.el`): tabulated-list + async thumbs; mpv; yt-dlp; Org
  `yt-video:` / `yt-playlist:`.

Flow: command → backend request-spec → fetch → parse → plists →
`yeetube-ui-render` → `yeetube-mode` → `yeetube-play-function` (default
`yeetube-mpv-play`) or download.

## Invariants

- Transport only in core. Backends build specs and parse bodies.
- Scraper has no network or process I/O; buffer entry is pure
  `save-excursion` over `ytInitialData` JSON.
- Register every new source/test file in Makefile `SRCS` / `TESTS`.
- Track `flake.lock` with `flake.nix`; never hand-edit lock
  (`nix flake update nixpkgs`).
- External tools: `mpv`, `yt-dlp`, optional `torsocks`.

## Elisp

- Lexical binding. Public `yeetube-`; internals `yeetube--`.
- Thin commands; pure helpers; effects at clear boundaries.
- Small focused functions. Comments explain why.

## Verification

Makefile re-enters `nix develop` when `nix` exists unless
`YEETUBE_ENV_WRAPPED=1`. Manual compile needs keymap-popup on load-path.

```sh
make compile          # byte-compile SRCS
make test             # ERT under test/
make lint             # checkdoc on SRCS
make dev              # compile + lint + test
make load             # reload via emacsclient
nix flake check       # Nix test suite
git diff --check
```

Focused ERT: one file from `TESTS` with project batch load-path. Green tests
count only when assertions prove claimed behavior.

## Contributions

Patches: patches@thanosapollo.org — `[PATCH yeetube] Short description`.

Bugs/features: bugs@thanosapollo.org — `[BUG yeetube] Short description`.

Stage scoped verified files only. Short `area: Change` subjects. No
generated-by or agent co-author metadata. No commit/amend/rebase/push/
destructive Git without explicit authorization.
