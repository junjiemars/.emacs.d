;;;; -*- lexical-binding:t -*-
;;;;
;; Utils
;;;;


(defun take (n seq)
  "Returns a sequence of the first n itmes in seq, or all items if
   there are fewer than n."
  (let ((acc nil) (n1 n) (s1 seq))
    (while (and (> n1 0) s1)
      (setq acc (cons (car s1) acc))
      (setq n1 (1- n1) s1 (cdr s1)))
    (nreverse acc)))


(defun drop-while (pred seq)
  (let ((s seq)
        (w nil))
    (while (and (not w) (car s))
      (if (funcall pred (car s))
          (setq w t)
        (setq s (cdr s))))
    (cdr s)))


(defun take-while (pred seq)
  (let ((s seq)
        (w nil)
        (s1 nil))
    (while (and (not w) (car s))
      (if (funcall pred (car s))
          (setq w t)
        (setq s1 (cons (car s) s1))
        (setq s (cdr s))))
    (nreverse s1)))


(defmacro string-trim< (s)
  "Remove leading whitespace from STRING."
  `(if (string-match "\\`[ \t\n\r]+" ,s)
       (replace-match "" t t ,s)
     ,s))


(defmacro string-trim>< (s)
  "Remove leading and trailing whitespace from STRING."
  `(let ((s1 (string-trim> ,s)))
     (string-trim< s1)))


(version-supported-when
    > 24.4
  (defmacro split-string>< (string &optional separators omit-nulls trim)
    "Split STRING into substrings bounded by matches for SEPARATORS, 
like `split-string' Emacs 24.4+"
    `(if ,trim
         (mapcar (lambda (s) (string-trim>< s))
                 (split-string ,string ,separators ,omit-nulls))
       (split-string ,string ,separators ,omit-nulls))))

(version-supported-when
    <= 24.4
  (defmacro split-string>< (&rest args)
    "An alias of `split-string' Emacs 24.4+"
    (declare (indent 0))
    `(split-string ,@args)))


(safe-fn-when number-sequence (fset 'range 'number-sequence))


;; use `pp' `pp-eval-expression' or `pp-eval-last-sexp'
(safe-fn-when cl-prettyexpand (fset 'pprint 'cl-prettyprint))


(defun int-to-binary-string (i)
  "Display an integer in binary string representation."
  (let ((s ""))
    (while (not (= i 0))
      (setq s (concat (if (= 1 (logand i 1)) "1" "0") s))
      (setq i (lsh i -1)))
    (concat "#b" (if (string= s "") (setq s "0") s))))



