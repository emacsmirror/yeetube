.POSIX:

NIX := $(shell command -v nix 2>/dev/null)

ENV_MAKE = $(MAKE) --no-print-directory
ifeq ($(YEETUBE_ENV_WRAPPED),)
ifneq ($(NIX),)
ENV_MAKE = nix develop path:$(CURDIR) --command env YEETUBE_ENV_WRAPPED=1 $(MAKE) --no-print-directory
endif
endif

EMACS_CMD ?= emacs

SRCS = yeetube.el yeetube-backend.el yeetube-youtube.el yeetube-scraper.el yeetube-ui.el yeetube-mpv.el yeetube-download.el yeetube-ol.el

TESTS = test/yeetube-tests.el test/yeetube-youtube-tests.el test/yeetube-scraper-tests.el test/yeetube-ui-tests.el

BATCH = $(EMACS_CMD) -Q --batch -L .

.PHONY: all compile do-compile test do-test lint do-lint clean dev load

all: compile

compile:
	@$(ENV_MAKE) do-compile

do-compile:
	@for f in $(SRCS); do \
	  echo "Compiling $$f..."; \
	  $(BATCH) -l $$f -f batch-byte-compile $$f || exit 1; \
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
	  $(BATCH) --eval "(checkdoc-file \"$$f\")" || exit 1; \
	done

dev: compile lint test

load: clean
	@emacsclient --eval "(progn \
	  (add-to-list 'load-path \"$(CURDIR)\") \
	  (dolist (sym '(yeetube-mode-map yeetube-settings-map)) \
	    (when (boundp sym) (makunbound sym))))" > /dev/null
	@for f in $(SRCS); do \
	  emacsclient --eval "(load-file \"$(CURDIR)/$$f\")" > /dev/null || \
	    printf "\033[31mFAIL\033[0m $$f\n"; \
	done
	@emacsclient --eval "(dolist (buf (buffer-list)) \
	  (with-current-buffer buf \
	    (when (derived-mode-p 'yeetube-mode) \
	      (use-local-map yeetube-mode-map))))" > /dev/null
	@printf "\033[32mLoaded all modules into Emacs\033[0m\n"

clean:
	rm -f *.elc
