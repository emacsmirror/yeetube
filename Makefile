EMACS ?= emacs

# Comment out or override to skip Guix:
#   make GUIX_SHELL= test
GUIX_SHELL ?= guix shell -m manifest.scm --

BATCH = $(GUIX_SHELL) $(EMACS) -Q --batch

SRCS = yeetube.el yeetube-mpv.el yeetube-ol.el

.PHONY: test compile clean

test:
	$(BATCH) -L . -l test/yeetube-tests.el \
	  -f ert-run-tests-batch-and-exit

compile:
	$(BATCH) -L . -f batch-byte-compile $(SRCS)

clean:
	rm -f *.elc
