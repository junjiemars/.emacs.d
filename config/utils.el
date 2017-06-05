;;;;
;; Utils
;;;;


(defun take (n seq)
  "Returns a sequence of the first n itmes in seq, or all items if
   there are fewer than n."
  (let ((lexical-binding t))
    (let ((acc nil) (n1 n) (s1 seq))
      (while (and (> n1 0) s1)
        (setq acc (cons (car s1) acc))
        (setq n1 (1- n1) s1 (cdr s1)))
      (nreverse acc))))


(safe-do-when number-sequence (fset 'range 'number-sequence))


;; use `pp' `pp-eval-expression' or `pp-eval-last-sexp'
(safe-do-when cl-prettyexpand (fset 'pprint 'cl-prettyprint))


(defun int-to-binary-string (i)
  "Display an integer in binary string representation."
  (let ((lexical-binding t))
    (let ((s ""))
      (while (not (= i 0))
        (setq s (concat (if (= 1 (logand i 1)) "1" "0") s))
        (setq i (lsh i -1)))
      (concat "#b" (if (string= s "") (setq s "0") s)))))



