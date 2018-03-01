;;;; -*- lexical-binding:t -*-
;;
;; Editing buffer that related to configurations.
;;














;; fix weird os x kill error
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (safe-fn-when ns-get-selection-internal
    (condition-case nil
        (ns-get-selection-internal 'CLIPBOARD)
      (quit nil))))



















