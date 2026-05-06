;;; guix.scm --- Build emacs-yeetube from the current working tree.
;;
;; Usage:
;;
;;   One-shot install into the user profile:
;;       guix package -f guix.scm
;;
;;   Development shell with all dependencies:
;;       guix shell -D -f guix.scm

(use-modules (gnu packages)
             (gnu packages emacs)
             (gnu packages emacs-build)
             (gnu packages emacs-xyz)
             (guix build-system emacs)
             (guix download)
             (guix gexp)
             (guix git-download)
             ((guix licenses) #:prefix license:)
             (guix packages)
             (guix utils)
             (ice-9 popen)
             (ice-9 rdelim))

(define %source-dir (dirname (current-filename)))

(define (git-output . args)
  "Run `git -C %source-dir ARGS...' and return its trimmed stdout, or
#f if the command fails or produces no output."
  (let* ((port (apply open-pipe* OPEN_READ "git" "-C" %source-dir args))
         (line (read-line port)))
    (close-pipe port)
    (if (eof-object? line) #f line)))

(define %version
  (or (git-output "describe" "--tags" "--always" "--dirty")
      (and=> (git-output "rev-parse" "--short" "HEAD")
             (lambda (hash) (string-append "2.2.0-" hash)))
      "2.2.0-git"))

(define (yeetube-file? file stat)
  "Include every file in the checkout except VCS metadata and build
artifacts."
  (let ((name (basename file)))
    (not (or (string-prefix? "." name)
             (string-suffix? ".elc" file)
             (string-suffix? "~" file)
             (string-suffix? "-autoloads.el" file)
             (string-suffix? "-pkg.el" file)))))

(define-public emacs-yeetube-git
  (package
   (name "emacs-yeetube-git")
   (version %version)
   (source (local-file %source-dir
                       "yeetube-checkout"
                       #:recursive? #t
                       #:select? yeetube-file?))
   (build-system emacs-build-system)
   (arguments (list #:tests? #f))
   (propagated-inputs (list emacs-compat emacs-keymap-popup))
   (home-page "https://thanosapollo.org/projects/yeetube/")
   (synopsis "YouTube front-end for GNU Emacs")
   (description
    "Scrape YouTube search results, play videos via mpv, and download
with yt-dlp.  This package definition builds from the current git
checkout, so the installed version always matches the working tree.")
   (license license:gpl3+)))

emacs-yeetube-git
