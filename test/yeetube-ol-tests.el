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

(provide 'yeetube-ol-tests)
;;; yeetube-ol-tests.el ends here
