;;; matrix-ert.el --- Batch completion evidence -*- lexical-binding: t; -*-

;; Run after the selected test libraries have been loaded by Make.
(require 'ert)
(require 'json)

(let* ((stats (ert-run-tests-batch t))
       (total (ert-stats-total stats))
       (completed (ert-stats-completed stats))
       (expected (ert-stats-completed-expected stats))
       (unexpected (ert-stats-completed-unexpected stats))
       (skipped (ert-stats-skipped stats)))
  (with-temp-file (getenv "YEETUBE_MATRIX_RECEIPT")
    (insert (json-encode `((total . ,total) (completed . ,completed)
                          (expected . ,expected) (unexpected . ,unexpected)
                          (skipped . ,skipped))))))
;;; matrix-ert.el ends here
