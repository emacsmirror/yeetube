.POSIX:

EMACS ?= emacs

# Comment out or override to skip Guix:
#   make GUIX_SHELL= test
GUIX_SHELL ?= guix shell -m manifest.scm --

BATCH = $(GUIX_SHELL) $(EMACS) -Q --batch -L .

SRCS = keymap-popup.el yeetube-scraper.el yeetube-ui.el \
       yeetube-download.el yeetube-mpv.el yeetube.el yeetube-ol.el

TESTS = test/yeetube-scraper-tests.el test/yeetube-ui-tests.el \
        test/yeetube-tests.el

.PHONY: all compile test lint clean dev load

all: compile

compile:
	@for f in $(SRCS); do \
	  echo "Compiling $$f..."; \
	  $(BATCH) -l $$f -f batch-byte-compile $$f || exit 1; \
	done

test:
	@for f in $(TESTS); do \
	  echo "Testing $$f..."; \
	  $(BATCH) -l ert -l $$f -f ert-run-tests-batch-and-exit || exit 1; \
	done

lint:
	@echo "Running checkdoc..."
	@for f in $(SRCS); do \
	  $(BATCH) --eval "(checkdoc-file \"$$f\")" || exit 1; \
	done

dev: compile lint test

load: clean
	@emacsclient --eval "(progn \
	  (add-to-list 'load-path \"$(CURDIR)\") \
	  (dolist (sym '(yeetube-mode-map)) \
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
