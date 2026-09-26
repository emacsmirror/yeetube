.POSIX:

NIX := $(shell command -v nix 2>/dev/null)

ENV_MAKE = $(MAKE) --no-print-directory
ifeq ($(YEETUBE_ENV_WRAPPED)$(IN_NIX_SHELL),)
ifneq ($(NIX),)
ENV_MAKE = nix develop "git+file://$(CURDIR)" --no-write-lock-file --command env YEETUBE_ENV_WRAPPED=1 $(MAKE) --no-print-directory
endif
endif

EMACS_CMD ?= emacs
EMACSCLIENT ?= emacsclient

# Dependencies first, shared by compilation, lint and live reload.
SRCS = yeetube-backend.el yeetube-scraper.el yeetube-youtube.el yeetube-ui.el yeetube-mpv.el yeetube-download.el yeetube.el yeetube-ol.el

TESTS = test/yeetube-tests.el test/yeetube-youtube-tests.el test/yeetube-scraper-tests.el test/yeetube-ui-tests.el test/yeetube-mpv-tests.el test/yeetube-ol-tests.el test/yeetube-tor-tests.el test/yeetube-lifecycle-tests.el test/yeetube-playback-tests.el test/yeetube-download-tests.el

FIXTURES = test/fixtures/search-videorenderer.json test/fixtures/search-lockupviewmodel.json

MATRIX_FILES = admin/test-matrix admin/matrix-ert.el admin/test-matrix-runner.py admin/test-matrix-negative.py

BATCH = $(EMACS_CMD) -Q --batch -L . -L test

.PHONY: all compile do-compile test do-test lint do-lint clean dev load

all: compile

.PHONY: test-matrix do-matrix-test test-matrix-negative
test-matrix:
	@python3 admin/test-matrix

test-matrix-negative:
	@python3 admin/test-matrix-negative.py

do-matrix-test:
	@$(BATCH) $(foreach f,$(TESTS),--eval "(require '$(basename $(notdir $(f))))") -l admin/matrix-ert.el

compile:
	@$(ENV_MAKE) do-compile

do-compile:
	@for f in $(SRCS); do \
	  echo "Compiling $$f..."; \
	  $(BATCH) --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile $$f || exit 1; \
	done

test:
	@$(ENV_MAKE) do-test

do-test:
	@for f in $(TESTS); do \
	  echo "Testing $$f..."; \
	  $(BATCH) -l ert -l $$f -f ert-run-tests-batch-and-exit || exit 1; \
	done

lint:
	@$(ENV_MAKE) do-lint

do-lint:
	@echo "Running checkdoc..."
	@for f in $(SRCS); do \
	  $(BATCH) -l checkdoc --eval "(progn (advice-add 'checkdoc-error :before (lambda (_point message) (error \"%s: %s\" \"$$f\" message))) (checkdoc-file \"$$f\"))" || exit 1; \
	done

dev: compile lint test

load: clean
	@$(EMACSCLIENT) --eval "(progn \
	  (add-to-list 'load-path \"$(CURDIR)\") \
	  (dolist (sym '(yeetube-mode-map yeetube-settings-map)) \
	    (when (boundp sym) (makunbound sym))))" > /dev/null
	@for f in $(SRCS); do \
	  $(EMACSCLIENT) --eval "(load-file \"$(CURDIR)/$$f\")" > /dev/null || \
	    { printf "\033[31mFAIL\033[0m $$f\n" >&2; exit 1; }; \
	done
	@$(EMACSCLIENT) --eval "(dolist (buf (buffer-list)) \
	  (with-current-buffer buf \
	    (when (derived-mode-p 'yeetube-mode) \
	      (use-local-map yeetube-mode-map))))" > /dev/null
	@printf "\033[32mLoaded all modules into Emacs\033[0m\n"

clean:
	rm -f *.elc
