.POSIX:

ifndef EMACS_CMD
GUIX := $(shell command -v guix 2>/dev/null)
ifdef GUIX
GUIX_SHELL := guix shell --pure -D -f guix.scm emacs-next --
EMACS_CMD := $(GUIX_SHELL) emacs
else
GUIX_SHELL :=
EMACS_CMD := emacs
endif
endif

GUIX_WRAP = $(if $(GUIX_SHELL),$(GUIX_SHELL) $(MAKE) --no-print-directory EMACS_CMD=emacs,$(MAKE) --no-print-directory)

SRCS = yeetube.el yeetube-scraper.el yeetube-ui.el yeetube-mpv.el yeetube-download.el yeetube-ol.el

TESTS = test/yeetube-tests.el test/yeetube-scraper-tests.el test/yeetube-ui-tests.el

BATCH = $(EMACS_CMD) -Q --batch -L .

.PHONY: all compile do-compile test do-test lint do-lint clean dev load

all: compile

compile:
	@$(GUIX_WRAP) do-compile

do-compile:
	@for f in $(SRCS); do \
	  echo "Compiling $$f..."; \
	  $(BATCH) -l $$f -f batch-byte-compile $$f || exit 1; \
	done

test:
	@$(GUIX_WRAP) do-test

do-test:
	@for f in $(TESTS); do \
	  echo "Testing $$f..."; \
	  $(BATCH) -l ert -l $$f -f ert-run-tests-batch-and-exit || exit 1; \
	done

lint:
	@$(GUIX_WRAP) do-lint

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
