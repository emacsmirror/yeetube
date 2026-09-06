;;; yeetube-lifecycle-tests.el --- Request ownership tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'yeetube)

;; A backend with deliberately non-plist continuation tokens.  Transport is
;; deferred, but commands, dispatch, decoding, callbacks and rendering are real.
(cl-defmethod yeetube-backend-search-request ((_backend (eql 'lifecycle)) query)
  (list :url query))
(cl-defmethod yeetube-backend-channel-request
  ((_backend (eql 'lifecycle)) channel _what &optional _query)
  (list :url channel))
(cl-defmethod yeetube-backend-feed-url ((_backend (eql 'lifecycle)) channel)
  channel)
(cl-defmethod yeetube-backend-continuation-request
  ((_backend (eql 'lifecycle)) continuation)
  (list :url continuation))
(cl-defmethod yeetube-backend-parse-page ((_backend (eql 'lifecycle)))
  (goto-char (point-min))
  (read (current-buffer)))
(cl-defmethod yeetube-backend-parse-continuation ((_backend (eql 'lifecycle)))
  (goto-char (point-min))
  (read (current-buffer)))
(cl-defmethod yeetube-backend-parse-feed ((_backend (eql 'lifecycle)))
  (goto-char (point-min))
  (read (current-buffer)))

(defvar yeetube-lifecycle--requests nil)

(defmacro yeetube-lifecycle--with-ui (&rest body)
  "Run BODY with an isolated results buffer and deferred transport."
  (declare (indent 0) (debug t))
  `(let ((yeetube--buffer-name " *yeetube-lifecycle*")
         (yeetube-backend 'lifecycle)
         (yeetube-results-limit 1)
         (yeetube-display-thumbnails-p nil)
         (yeetube-lifecycle--requests nil))
     (save-window-excursion
       (unwind-protect
           (cl-letf (((symbol-function 'yeetube--fetch)
                      (lambda (request callback &optional args)
                        (setq yeetube-lifecycle--requests
                              (append yeetube-lifecycle--requests
                                      (list (list request callback args)))))))
             (set-buffer (get-buffer-create yeetube--buffer-name))
             ,@body)
         (when-let* ((buffer (get-buffer yeetube--buffer-name)))
           (kill-buffer buffer))))))

(defun yeetube-lifecycle--deliver (request result &optional status)
  "Deliver RESULT and STATUS to a captured REQUEST; assert response cleanup."
  (let ((response (generate-new-buffer " *yeetube-response*")))
    (unwind-protect
        (progn
          (with-current-buffer response
            (insert "HTTP/1.1 200 OK\nContent-Type: text/plain\n\n" (prin1-to-string result))
            (apply (nth 1 request) status (nth 2 request)))
          (should-not (buffer-live-p response)))
      (when (buffer-live-p response) (kill-buffer response)))))

(defun yeetube-lifecycle--item (id &optional channel)
  "Return an item with ID and optional CHANNEL."
  (list :id id :title id :views "1" :duration "1:00" :date "1 day ago"
        :channel (or channel "") :type 'video :thumbnail-url "https://invalid/thumb"))

(defun yeetube-lifecycle--page (id &optional token channel)
  "Return a page with ID, opaque TOKEN and CHANNEL identity."
  (list :items (list (yeetube-lifecycle--item id))
        :continuation token :channel-identity (and channel (list :channel channel))))

(ert-deftest yeetube-lifecycle-latest-search-wins ()
  (yeetube-lifecycle--with-ui
    (yeetube-search "old")
    (yeetube-search "new")
    ;; Changing the default does not change the parser or pagination backend.
    (setq yeetube-backend 'youtube)
    (yeetube-lifecycle--deliver (nth 1 yeetube-lifecycle--requests)
                               (yeetube-lifecycle--page "new" "next"))
    (with-current-buffer yeetube--buffer-name
      (let ((text (buffer-string)))
        (yeetube-lifecycle--deliver (nth 0 yeetube-lifecycle--requests)
                                   (yeetube-lifecycle--page "old"))
        (should (equal text (buffer-string)))
        (should (equal "new" (plist-get (car yeetube-items) :id))))
      (yeetube-next-page)
      (should (equal '(:url "next") (car (nth 2 yeetube-lifecycle--requests)))))))

(ert-deftest yeetube-lifecycle-old-page-cannot-fill-new-channel ()
  (yeetube-lifecycle--with-ui
    (yeetube-channel-videos "old")
    (yeetube-lifecycle--deliver (nth 0 yeetube-lifecycle--requests)
                               (yeetube-lifecycle--page "old" "old-next" "Old"))
    (yeetube-next-page)
    (yeetube-channel-videos "new")
    (yeetube-lifecycle--deliver (nth 2 yeetube-lifecycle--requests)
                               (yeetube-lifecycle--page "new" "new-next" "New"))
    (yeetube-lifecycle--deliver (nth 1 yeetube-lifecycle--requests)
                               (yeetube-lifecycle--page "stale" "bad"))
    (should (equal '("new") (mapcar (lambda (item) (plist-get item :id)) yeetube-items)))
    (should (equal "new-next" yeetube--continuation))
    (yeetube-next-page)
    (yeetube-lifecycle--deliver (nth 3 yeetube-lifecycle--requests)
                               (yeetube-lifecycle--page "added"))
    (should (equal "New" (plist-get (cadr yeetube-items) :channel)))
    (should (= 2 (length yeetube-content)))))

(ert-deftest yeetube-lifecycle-next-page-single-flight-and-retry ()
  (yeetube-lifecycle--with-ui
    (yeetube-search "first")
    (yeetube-lifecycle--deliver (car yeetube-lifecycle--requests)
                               (yeetube-lifecycle--page "first" "next"))
    (yeetube-next-page)
    (yeetube-next-page)
    (should (= 2 (length yeetube-lifecycle--requests)))
    (yeetube-lifecycle--deliver (nth 1 yeetube-lifecycle--requests) nil
                               '(:error (error "Failure")))
    (should-not yeetube--page-pending)
    (yeetube-next-page)
    (should (= 3 (length yeetube-lifecycle--requests)))
    (yeetube-lifecycle--deliver (nth 2 yeetube-lifecycle--requests)
                               (yeetube-lifecycle--page "second"))
    ;; A transport accidentally delivering the same response twice cannot append.
    (yeetube-lifecycle--deliver (nth 2 yeetube-lifecycle--requests)
                               (yeetube-lifecycle--page "second"))
    (should (= 2 (length yeetube-items)))))

(ert-deftest yeetube-lifecycle-target-replacement ()
  (dolist (exit '(kill mode reenter))
    (yeetube-lifecycle--with-ui
      (yeetube-search "old")
      (let ((target (get-buffer yeetube--buffer-name)))
        (pcase exit
          ('kill (kill-buffer target))
          (_ (with-current-buffer target
               (fundamental-mode)
               (when (eq exit 'reenter) (yeetube-mode)))))
        (with-current-buffer (get-buffer-create yeetube--buffer-name)
          (let ((inhibit-read-only t)) (erase-buffer) (insert "Untouched")))
        (yeetube-lifecycle--deliver (car yeetube-lifecycle--requests)
                                   (yeetube-lifecycle--page "old"))
        (with-current-buffer yeetube--buffer-name
          (should (equal "Untouched" (buffer-string))))))))

(ert-deftest yeetube-lifecycle-stale-feed-does-not-fallback ()
  (yeetube-lifecycle--with-ui
    (yeetube-display-feed "old" "fallback")
    (yeetube-search "new")
    (yeetube-lifecycle--deliver (car yeetube-lifecycle--requests) nil
                               '(:error (error "Failure")))
    (should (= 2 (length yeetube-lifecycle--requests)))
    (should (string-match-p "Loading" (buffer-string)))))

(ert-deftest yeetube-lifecycle-feed-fallback-retains-backend ()
  (yeetube-lifecycle--with-ui
    (yeetube-display-feed "feed" "fallback")
    (setq yeetube-backend 'youtube)
    (yeetube-lifecycle--deliver (car yeetube-lifecycle--requests) nil)
    (should (equal '(:url "fallback") (car (nth 1 yeetube-lifecycle--requests))))
    (yeetube-lifecycle--deliver (nth 1 yeetube-lifecycle--requests)
                               (yeetube-lifecycle--page "fallback"))
    (should (equal "fallback" (plist-get (car yeetube-items) :id)))))

(ert-deftest yeetube-lifecycle-pagination-preserves-settings-point ()
  (yeetube-lifecycle--with-ui
    (yeetube-search "first")
    (yeetube-lifecycle--deliver (car yeetube-lifecycle--requests)
                               (yeetube-lifecycle--page "first" "next"))
    (setq-local yeetube-mpv-video-quality "360")
    (goto-char (+ (point-min) 4))
    (let ((before (point)))
      (yeetube-next-page)
      (yeetube-lifecycle--deliver (nth 1 yeetube-lifecycle--requests)
                                 (yeetube-lifecycle--page "second"))
      (should (= before (point)))
      (should (equal "360" yeetube-mpv-video-quality)))))

(ert-deftest yeetube-lifecycle-items-are-buffer-owned ()
  (yeetube-lifecycle--with-ui
    (yeetube-search "first")
    (yeetube-lifecycle--deliver (car yeetube-lifecycle--requests)
                               (yeetube-lifecycle--page "first"))
    (with-temp-buffer
      (yeetube-mode)
      (should-not yeetube-items)
      (should-not yeetube-content))))

(ert-deftest yeetube-lifecycle-identity-preserves-extension-fields ()
  (let* ((item '(:id "x" :extra (opaque value) :channel ""))
         (filled (car (yeetube-scraper-fill-channel-identity
                       (list item) '(:channel "Owner")))))
    (should (equal '(opaque value) (plist-get filled :extra)))
    (should (equal "Owner" (plist-get filled :channel)))
    (should (equal "" (plist-get item :channel)))))

(ert-deftest yeetube-lifecycle-deferred-thumbnails ()
  (dolist (change '(none render layout mode kill request))
    (yeetube-lifecycle--with-ui
      (let ((yeetube-display-thumbnails-p t) queued extracted)
        (cl-letf (((symbol-function 'yeetube--queue-retrieve)
                   (lambda (_url callback args) (setq queued (list nil callback args))))
                  ((symbol-function 'yeetube-ui--extract-image)
                   (lambda (_status) (setq extracted t) '(image :type xpm :data "test"))))
          (yeetube-search "one")
          (yeetube-lifecycle--deliver (car yeetube-lifecycle--requests)
                                     (yeetube-lifecycle--page "one"))
          (let* ((old-row (car yeetube-content))
                 (target (current-buffer)))
            (pcase change
              ('render (yeetube-ui-render (list (yeetube-lifecycle--item "one"))))
              ('layout (setq tabulated-list-format (copy-sequence tabulated-list-format)))
              ('mode (fundamental-mode))
              ('kill (kill-buffer target)
                     (set-buffer (get-buffer-create yeetube--buffer-name)))
              ('request (yeetube-search "two")))
            (let ((text (buffer-string)) (before (point)))
              (yeetube-lifecycle--deliver queued nil)
              (should (= before (point)))
              (if (eq change 'none)
                  (progn
                    (should extracted)
                    (should (get-text-property 0 'display (aref (cadr old-row) 0)))
                    (save-excursion
                      (goto-char (point-min))
                      (search-forward "[[one.jpg]]")
                      (should (equal '(image :type xpm :data "test")
                                     (get-text-property (match-beginning 0) 'display)))))
                (should-not extracted)
                (should (equal text (buffer-string)))
                (should-not (get-text-property 0 'display (aref (cadr old-row) 0)))))))))))

(ert-deftest yeetube-lifecycle-thumbnail-error-releases-resources ()
  (yeetube-lifecycle--with-ui
    (let ((yeetube-display-thumbnails-p t) queued destroyed response)
      (cl-letf (((symbol-function 'yeetube--queue-retrieve)
                 (lambda (_url callback args) (setq queued (list nil callback args))))
                ((symbol-function 'mm-destroy-parts) (lambda (handle) (setq destroyed handle))))
        (yeetube-search "one")
        (yeetube-lifecycle--deliver (car yeetube-lifecycle--requests)
                                   (yeetube-lifecycle--page "one"))
        (setq response (generate-new-buffer " *bad-image*"))
        (cl-letf (((symbol-function 'mm-dissect-buffer) (lambda (&rest _) 'handle))
                  ((symbol-function 'mm-get-image) (lambda (_) (error "Bad image"))))
          (with-current-buffer response
            (should-error (apply (nth 1 queued) nil (nth 2 queued)))))
        (should-not (buffer-live-p response))
        (should (eq 'handle destroyed))))))

(ert-deftest yeetube-lifecycle-dispatch-error-is-retryable ()
  (yeetube-lifecycle--with-ui
    (yeetube-search "first")
    (yeetube-lifecycle--deliver (car yeetube-lifecycle--requests)
                               (yeetube-lifecycle--page "first" "next"))
    (cl-letf (((symbol-function 'yeetube--fetch)
               (lambda (&rest _) (error "Dispatch failed"))))
      (should-error (yeetube-next-page)))
    (should-not yeetube--page-pending)
    (should (equal "next" yeetube--continuation))
    (yeetube-next-page)
    (should (= 2 (length yeetube-lifecycle--requests)))))

(ert-deftest yeetube-lifecycle-auto-pagination-and-empty-settlement ()
  (yeetube-lifecycle--with-ui
    (let ((yeetube-results-limit 3))
      (yeetube-search "first")
      (yeetube-lifecycle--deliver (car yeetube-lifecycle--requests)
                                 (yeetube-lifecycle--page "first" "next"))
      (should (= 2 (length yeetube-lifecycle--requests)))
      (yeetube-lifecycle--deliver (nth 1 yeetube-lifecycle--requests)
                                 (yeetube-lifecycle--page "second" "last"))
      (should (= 3 (length yeetube-lifecycle--requests)))
      (let ((text (buffer-string)))
        (yeetube-lifecycle--deliver (nth 2 yeetube-lifecycle--requests)
                                   '(:items nil :continuation "unused"))
        (should (equal text (buffer-string))))
      (should-not yeetube--continuation)
      (should-not yeetube--page-pending))))

(ert-deftest yeetube-lifecycle-parser-reentry-invalidates-old-result ()
  (yeetube-lifecycle--with-ui
    (yeetube-search "old")
    (cl-letf (((symbol-function 'yeetube-backend-parse-page)
               (lambda (_backend)
                 (yeetube-search "new")
                 (yeetube-lifecycle--page "old" "stale"))))
      (yeetube-lifecycle--deliver (car yeetube-lifecycle--requests) nil))
    (with-current-buffer yeetube--buffer-name
      (should (string-match-p "Loading" (buffer-string)))
      (should-not yeetube--continuation))
    (yeetube-lifecycle--deliver (nth 1 yeetube-lifecycle--requests)
                               (yeetube-lifecycle--page "new"))
    (with-current-buffer yeetube--buffer-name
      (should (equal "new" (plist-get (car yeetube-items) :id))))))

(ert-deftest yeetube-lifecycle-thumbnail-extraction-reentry ()
  (yeetube-lifecycle--with-ui
    (let ((yeetube-display-thumbnails-p t) queued)
      (cl-letf (((symbol-function 'yeetube--queue-retrieve)
                 (lambda (_url callback args) (setq queued (list nil callback args)))))
        (yeetube-search "one")
        (yeetube-lifecycle--deliver (car yeetube-lifecycle--requests)
                                   (yeetube-lifecycle--page "one")))
      (let ((target (current-buffer)))
        (cl-letf (((symbol-function 'yeetube-ui--extract-image)
                   (lambda (_status)
                     (with-current-buffer target
                       (yeetube-ui-render (list (yeetube-lifecycle--item "one"))))
                     '(image :type xpm :data "obsolete"))))
          (yeetube-lifecycle--deliver queued nil))
        (should-not (get-text-property 0 'display (aref (cadar yeetube-content) 0)))
        (save-excursion
          (goto-char (point-min))
          (search-forward "[[one.jpg]]")
          (should-not (get-text-property (match-beginning 0) 'display)))))))

(provide 'yeetube-lifecycle-tests)
;;; yeetube-lifecycle-tests.el ends here
