;;; yeetube-ol-tests.el --- Tests for yeetube-ol  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Thanos Apollo

;; Run: emacs -Q --batch -L .. -l test/yeetube-ol-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(unless (boundp 'find-function-mode)
  (defvar find-function-mode nil))

(require 'ert)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name ".." dir)))
(require 'yeetube-ol)

(ert-deftest yeetube-ol-test-follow-video-uses-active-backend ()
  "Video follow builds the URL with `yeetube-backend'."
  (let* ((yeetube-backend 'test-backend)
         (played (list nil))
         (yeetube-play-function
          (lambda (url) (setcar played url))))
    (cl-letf (((symbol-function 'yeetube-backend-item-url)
               (lambda (backend id type)
                 (should (eq backend 'test-backend))
                 (should (equal id "vid1"))
                 (should (eq type 'video))
                 "https://example.test/v/vid1")))
      (yeetube-ol-follow-video "vid1" nil)
      (should (equal (car played) "https://example.test/v/vid1")))))

(ert-deftest yeetube-ol-test-export-video-uses-active-backend ()
  "Video export builds the URL with `yeetube-backend'."
  (let ((yeetube-backend 'test-backend))
    (cl-letf (((symbol-function 'yeetube-backend-item-url)
               (lambda (backend id type)
                 (should (eq backend 'test-backend))
                 (should (equal id "vid1"))
                 (should (eq type 'video))
                 "https://example.test/v/vid1")))
      (should (equal "<a href=\"https://example.test/v/vid1\">label</a>"
                     (yeetube-ol-export-video "vid1" "label" 'html nil))))))

(ert-deftest yeetube-ol-test-follow-playlist-uses-active-backend ()
  "Playlist follow builds the URL with `yeetube-backend'."
  (let ((yeetube-backend 'test-backend)
        (fetched (list nil)))
    (cl-letf (((symbol-function 'yeetube-backend-item-url)
               (lambda (backend id type)
                 (should (eq backend 'test-backend))
                 (should (equal id "PLxyz"))
                 (should (eq type 'playlist))
                 "https://example.test/p/PLxyz"))
              ((symbol-function 'yeetube--display-loading) #'ignore)
              ((symbol-function 'yeetube-display-content-from-url)
               (lambda (url) (setcar fetched url))))
      (yeetube-ol-follow-playlist "PLxyz" nil)
      (should (equal (car fetched) "https://example.test/p/PLxyz")))))

(ert-deftest yeetube-ol-test-export-playlist-uses-active-backend ()
  "Playlist export builds the URL with `yeetube-backend'."
  (let ((yeetube-backend 'test-backend))
    (cl-letf (((symbol-function 'yeetube-backend-item-url)
               (lambda (backend id type)
                 (should (eq backend 'test-backend))
                 (should (equal id "PLxyz"))
                 (should (eq type 'playlist))
                 "https://example.test/p/PLxyz")))
      (should (equal "[label](https://example.test/p/PLxyz)"
                     (yeetube-ol-export-playlist "PLxyz" "label" 'md nil))))))

(require 'ox-html)
(require 'ox-md)
(require 'ox-latex)

(ert-deftest yeetube-ol-test-full-html-attribute-injection ()
  "Export hostile video and playlist paths as data, not attributes."
  (dolist (type '(video playlist))
    (dolist (description '(nil "label" "*bold* & <tag>"))
      (let* ((path "abc\"onclick=\"alert(1)")
             (url (yeetube-backend-item-url yeetube-backend path type))
             (safe-url (replace-regexp-in-string "\"" "&quot;" url t t))
             (label (pcase description
                      ('nil safe-url)
                      ("label" "label")
                      (_ "<b>bold</b> &amp; &lt;tag&gt;")))
             (text (if description
                       (format "[[yt-%s:%s][%s]]" type path description)
                     (format "[[yt-%s:%s]]" type path))))
        (should (string-match-p
                 (regexp-quote (format "<a href=\"%s\">%s</a>" safe-url label))
                 (org-export-string-as text 'html t)))))))

(ert-deftest yeetube-ol-test-full-custom-backend-export ()
  "Escape URL syntax and raw fallback text, but preserve exported labels."
  (let ((yeetube-backend 'test-backend)
        (url "https://example.test/a)_[x]<tag>\"\\{}?a=1&b=2#frag%20"))
    (dolist (type '(video playlist))
      (dolist (description '(nil "*bold* & text"))
        (dolist (backend '(html md latex))
          (cl-letf (((symbol-function 'yeetube-backend-item-url)
                     (lambda (active id kind)
                       (should (eq active 'test-backend))
                       (should (equal id "opaque:id/with?punctuation"))
                       (should (eq kind type))
                       url)))
            (let* ((text (if description
                             (format "[[yt-%s:opaque:id/with?punctuation][%s]]"
                                     type description)
                           (format "[[yt-%s:opaque:id/with?punctuation]]" type)))
                   (output (org-export-string-as text backend t))
                   (destination
                    (pcase backend
                      ('html "https://example.test/a)_[x]&lt;tag&gt;&quot;\\{}?a=1&amp;b=2#frag%20")
                      ('md "https://example.test/a%29_%5Bx%5D%3Ctag%3E%22%5C%7B%7D?a=1&amp;b=2#frag%20")
                      ('latex "https://example.test/a)\\_[x]<tag>\"\\%5C\\%7B\\%7D?a=1\\&b=2\\#frag\\%20")))
                   (label
                    (if description
                        (pcase backend
                          ('html "<b>bold</b> &amp; text")
                          ('md "**bold** & text")
                          ('latex "\\textbf{bold} \\& text"))
                      (pcase backend
                        ('html destination)
                        ('md "https://example.test/a)\\_\\[x\\]&lt;tag&gt;\"\\\\{}?a=1&amp;b=2#frag%20")
                        ('latex "https://example.test/a)\\_[x]<tag>\"$\\backslash$\\{\\}?a=1\\&b=2\\#frag\\%20"))))
                   (link (pcase backend
                           ('html (format "<a href=\"%s\">%s</a>" destination label))
                           ('md (format "[%s](%s)" label destination))
                           ('latex (format "\\href{%s}{%s}" destination label)))))
              (should (string-match-p (regexp-quote link) output)))))))))

(ert-deftest yeetube-ol-test-full-url-whitespace-and-entities ()
  "Keep whitespace and entity-looking query values inside destinations."
  (dolist (type '(video playlist))
    (cl-letf (((symbol-function 'yeetube-backend-item-url)
               (lambda (&rest _)
                 "https://example.test/a b\n\t ?x=&copy;&y=%20")))
      (dolist (case '((md . "[label](https://example.test/a%20b%0A%09%C2%A0?x=&amp;copy;&amp;y=%20)")
                      (latex . "\\href{https://example.test/a\\%20b\\%0A\\%09\\%C2\\%A0?x=\\&copy;\\&y=\\%20}{label}")))
        (should (string-match-p
                 (regexp-quote (cdr case))
                 (org-export-string-as
                  (format "[[yt-%s:opaque][label]]" type) (car case) t)))))))

(ert-deftest yeetube-ol-test-full-normal-links ()
  "Preserve normal destinations and already-exported emphasis."
  (dolist (type '(video playlist))
    (dolist (description '(nil "*bold*"))
      (dolist (backend '(html md latex))
        (let* ((url (yeetube-backend-item-url yeetube-backend "abc123" type))
               (label (if description
                          (pcase backend
                            ('html "<b>bold</b>")
                            ('md "**bold**")
                            ('latex "\\textbf{bold}"))
                        url))
               (expected (pcase backend
                           ('html (format "<a href=\"%s\">%s</a>" url label))
                           ('md (format "[%s](%s)" label url))
                           ('latex (format "\\href{%s}{%s}" url label)))))
          (should (string-match-p
                   (regexp-quote expected)
                   (org-export-string-as
                    (if description
                        (format "[[yt-%s:abc123][%s]]" type description)
                      (format "[[yt-%s:abc123]]" type))
                    backend t))))))))

(provide 'yeetube-ol-tests)
;;; yeetube-ol-tests.el ends here
