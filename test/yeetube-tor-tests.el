;;; yeetube-tor-tests.el --- Deferred transport tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Exercise the real URL queue without timers or network access.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'url-queue)
(require 'url-http)
(require 'socks)
(require 'yeetube)

;; These tests retain URL dispatch, HTTP(S), proxy selection, the pool,
;; redirects and retries.  Only connection leaves and timer scheduling are
;; replaced.  Pipe processes simulate sockets without DNS or network I/O.
(defmacro yeetube-tor-test--with-transport (&rest body)
  "Run BODY with real URL networking above harmless connection leaves."
  (declare (indent 0) (debug t))
  `(let ((url-queue nil)
         (url-queue-progress-timer nil)
         (url-setup-done t)
         (url-retrieve-number-of-calls 1)
         (url-history-track nil)
         (url-proxy-services nil)
         (url-http-open-connections (make-hash-table :test 'equal))
         (url-gateway-method 'native)
         (socks-override-functions nil)
         (yeetube-enable-tor nil)
         (process-environment
          (cl-remove-if (lambda (entry)
                          (string-match-p "_proxy=" (downcase entry)))
                        process-environment))
         (original-buffers (buffer-list))
         connections writes entries processes observers)
     (unwind-protect
         (cl-letf (((symbol-function 'run-with-idle-timer) #'ignore)
                   ((symbol-function 'open-network-stream)
                    (lambda (name buffer host service &rest options)
                      (push (list host service options) connections)
                      (let ((process (make-pipe-process :name name :buffer buffer
                                                       :noquery t)))
                        (push process processes)
                        process)))
                   ((symbol-function 'process-send-string)
                    (lambda (process string) (push (cons process string) writes)))
                   ((symbol-function 'socks-open-network-stream)
                    (lambda (&rest _) (ert-fail "Unexpected SOCKS connection")))
                   ((symbol-function 'make-network-process)
                    (lambda (&rest _) (ert-fail "Unexpected network connection")))
                   ((symbol-function 'network-lookup-address-info)
                    (lambda (&rest _) (ert-fail "Unexpected DNS lookup"))))
           ;; Observe entry, do not replace any URL implementation.
           (dolist (function '(url-retrieve url-queue-retrieve
                               url-find-proxy-for-url url-https url-http
                               url-http-find-free-connection url-open-stream))
             (let ((observer (lambda (&rest _) (push function entries))))
               (push (cons function observer) observers)
               (advice-add function :before observer)))
           ,@body)
       (dolist (observer observers)
         (advice-remove (car observer) (cdr observer)))
       (dolist (process processes)
         ;; No retry during teardown.
         (set-process-sentinel process #'ignore)
         (delete-process process))
       (dolist (buffer (buffer-list))
         (unless (memq buffer original-buffers) (kill-buffer buffer))))))

(ert-deftest yeetube-tor-rejects-before-url-proxy-pool-or-queue ()
  "Reject every Tor submission, including HTTP redirect origins and retries."
  (yeetube-tor-test--with-transport
    (dolist (proxies '(nil (("http" . "proxy.invalid:8080")
                           ("https" . "proxy.invalid:8080"))))
      (let ((url-proxy-services proxies))
        (dolist (url '("https://i.ytimg.com/vi/x/default.jpg"
                       "http://redirect.invalid/" "http://localhost/"
                       "ftp://other.invalid/"))
          (dotimes (_ 2)                 ; Explicit caller retry cannot escape.
            (let ((yeetube-enable-tor t))
              (dolist (submit (list (lambda () (yeetube--fetch
                                                (list :url url) #'ignore))
                                   (lambda () (yeetube--queue-retrieve
                                                url #'ignore nil))))
                (let ((failure (should-error (funcall submit) :type 'user-error)))
                  (should (string-match-p "Tor HTTP(S) retrieval is unsupported"
                                          (error-message-string failure)))
                  (should (string-match-p "yeetube-enable-tor to nil"
                                          (error-message-string failure)))))))
          ;; The submission binding has unwound: no latent job can run,
          ;; time out, redirect or retry under a now-native policy.
          (url-queue-run-queue)
          (url-queue-prune-old-entries)
          (should-not url-queue)
          (should-not entries)
          (should-not connections)
          (should-not writes))))))

(ert-deftest yeetube-tor-existing-native-pool-cannot-escape ()
  "Leave an actual reusable native connection untouched by Tor requests."
  (yeetube-tor-test--with-transport
    (let ((process (make-pipe-process :name "native-pool" :noquery t)))
      (push process processes)
      (url-http-mark-connection-as-free "pool.invalid" 443 process)
      (let ((yeetube-enable-tor t))
        (should-error (yeetube--fetch '(:url "https://pool.invalid/") #'ignore)
                      :type 'user-error)
        (should-error (yeetube--queue-retrieve "https://pool.invalid/" #'ignore nil)
                      :type 'user-error))
      (url-queue-run-queue)
      (should-not entries)
      (should-not writes)
      (should (equal (hash-table-values url-http-open-connections)
                     (list (list process))))
      ;; Positive control: the real URL pool accepts this connection for
      ;; an unrelated native request, even while YeeTube requires Tor.
      (let ((yeetube-enable-tor t))
        (url-retrieve "https://pool.invalid/" #'ignore nil t t))
      (should (memq 'url-http-find-free-connection entries))
      (should-not (memq 'url-open-stream entries))
      (should-not connections)
      (should (eq (caar writes) process)))))

(ert-deftest yeetube-tor-native-deferred-https-keeps-tls ()
  "Start accepted native jobs after bindings unwind without altering TLS."
  (yeetube-tor-test--with-transport
    (let ((yeetube-enable-tor nil))
      (yeetube--queue-retrieve "https://native.invalid/" #'ignore nil))
    (should-not connections)
    (let ((yeetube-enable-tor t))
      (url-queue-run-queue)
      (url-retrieve "https://unrelated.invalid/" #'ignore nil t t))
    (should (equal (mapcar #'car connections)
                   '("unrelated.invalid" "native.invalid")))
    (dolist (connection connections)
      (should (= (nth 1 connection) 443))
      (should (eq (plist-get (nth 2 connection) :type) 'tls)))
    (should (memq 'url-https entries))
    (should (memq 'url-open-stream entries))
    (should (string-prefix-p "GET / HTTP/1.1" (cdr (cadr writes))))
    (should (eq url-gateway-method 'native))))

(ert-deftest yeetube-tor-native-redirect-and-retry-retain-tls ()
  "Exercise the real redirect and retry routes that Tor must never enter."
  (yeetube-tor-test--with-transport
    ;; HTTP must be refused under Tor: its real redirect handler can
    ;; select HTTPS/TLS regardless of a dynamic SOCKS gateway binding.
    (yeetube--fetch '(:url "http://redirect.invalid/") #'ignore)
    (let ((yeetube-enable-tor t))
      (url-http-generic-filter
       (car processes)
       (concat "HTTP/1.1 302 Found\r\n"
               "Location: https://destination.invalid/\r\n"
               "Content-Length: 0\r\n\r\n")))
    (should (equal (caar connections) "destination.invalid"))
    (should (eq (plist-get (nth 2 (car connections)) :type) 'tls))
    (should (memq 'url-https entries))
    ;; A closed connection causes the actual native sentinel to retry.
    ;; This job was accepted as native; later Tor toggles cannot reroute it.
    (let ((process (car processes))
          (yeetube-enable-tor t))
      (set-process-sentinel process #'ignore)
      (delete-process process)
      (url-http-end-of-document-sentinel process "closed\n"))
    (should (= (length connections) 3))
    (should (equal (caar connections) "destination.invalid"))
    (should (eq (plist-get (nth 2 (car connections)) :type) 'tls))))

(ert-deftest yeetube-tor-native-fetch-isolates-request-settings ()
  "Keep regular native request data local and retain HTTPS transport."
  (yeetube-tor-test--with-transport
    (let ((yeetube-request-headers '(("X-Default" . "one"))))
      (yeetube--fetch '(:url "https://fetch.invalid/" :method "POST"
                            :headers (("X-Request" . "two")) :data "body")
                      #'ignore))
    (url-retrieve "https://unrelated.invalid/" #'ignore nil t t)
    (should (string-prefix-p "POST / HTTP/1.1" (cdr (cadr writes))))
    (should (string-match-p "X-Default: one" (cdr (cadr writes))))
    (should (string-match-p "X-Request: two" (cdr (cadr writes))))
    (should (string-suffix-p "body" (cdr (cadr writes))))
    (should (string-prefix-p "GET / HTTP/1.1" (cdar writes)))
    (should-not (string-match-p (rx (or "X-Default" "X-Request" "body"))
                                (cdar writes)))
    (should (eq (plist-get (nth 2 (cadr connections)) :type) 'tls))))

(ert-deftest yeetube-tor-native-queue-settles-callback-and-timeout ()
  "Preserve native queue completion, callback arguments and timeout cleanup."
  (yeetube-tor-test--with-transport
    (let ((url-queue-parallel-processes 1) completed)
      (yeetube--queue-retrieve
       "https://complete.invalid/"
       (lambda (status arg) (push (list status arg) completed)) '(first))
      (yeetube--queue-retrieve
       "https://timeout.invalid/"
       (lambda (status arg) (push (list status arg) completed)) '(second))
      (url-queue-run-queue)
      (let ((first (car processes)))
        (url-http-generic-filter first "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n"))
      (should (equal completed '((nil first))))
      (should (= (length url-queue) 1))
      (should (= (length connections) 2))
      (setf (url-queue-start-time (car url-queue)) 0)
      (url-queue-prune-old-entries)
      (should-not url-queue)
      (should (eq (cadar completed) 'second))
      (should (eq (caar (car completed)) :error)))))

(ert-deftest yeetube-tor-legacy-macro-fails-closed ()
  "Never run a legacy macro body under an unsupported Tor policy."
  (let ((yeetube-enable-tor t) called)
    (should-error (yeetube-with-tor-socks (setq called t)) :type 'user-error)
    (should-not called)
    (let ((yeetube-enable-tor nil))
      (should (eq (yeetube-with-tor-socks 'native) 'native)))))

(provide 'yeetube-tor-tests)
;;; yeetube-tor-tests.el ends here
