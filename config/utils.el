;;;; -*- lexical-binding:t -*-
;;;;
;; Utils
;;;;



(defun take (n seq)
  "Returns a sequence of the first n itmes in seq, or all items if
   there are fewer than n."
  (let ((acc nil) (n1 n) (s1 seq))
    (while (and (> n1 0) s1)
      (setq acc (cons (car s1) acc)
            n1 (1- n1) s1 (cdr s1)))
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
        (setq s1 (cons (car s) s1)
              s (cdr s))))
    (nreverse s1)))


(defmacro string-trim< (s &optional lr)
  "Remove leading whitespace or the matching of LR from S.

\(fn STRING &optional LEFT-REGEXP)"
  `(let ((r (if ,lr (concat "\\`" ,lr) "\\`[ \t\n\r]+")))
     (if (string-match r ,s)
         (replace-match "" t t ,s)
       ,s)))


(defmacro string-trim>< (s &optional rr lr)
  "Remove leading and trailing whitespace or the matching of LR/RR 
from STRING.

\(fn STRING &optional LEFT-REGEXP RIGHT-REGEXP)"
  `(let ((s1 (string-trim> ,s ,rr)))
     (string-trim< s1 ,lr)))


(defmacro save-sexpr-to-file (sexpr file)
  "Save SEXPR to the FILE."
  `(save-excursion
     (let ((sexpr-buffer (find-file-noselect ,file)))
       (set-buffer sexpr-buffer)
       (erase-buffer)
       (print ,sexpr sexpr-buffer)
       (save-buffer)
       (kill-buffer sexpr-buffer))))




;; compatiable functions

(version-supported-if
    <= 24.4
    (defalias 'split-string>< 'split-string)
  (defmacro split-string>< (string &optional separators omit-nulls trim)
    "Split STRING into substrings bounded by matches for SEPARATORS, 
like `split-string' Emacs 24.4+"
    `(if ,trim
         (delete ""
                 (mapcar (lambda (s)
                           (if (and (stringp ,trim) (> (length ,trim) 0))
                               (string-trim>< s ,trim ,trim)
                             (string-trim>< s)))
                         (split-string ,string ,separators ,omit-nulls)))
       (split-string ,string ,separators ,omit-nulls))))


(defmacro alist-get-> (key alist &optional default)
  "Return the value associated with KEY in ALIST, using `assq'.
If KEY is not found in ALIST, return DEFAULT. There're no `alist-get' 
function definition in Emacs25-."
  `(let ((x (assq ,key ,alist)))
     (if x (cdr x) ,default)))


(safe-fn-when number-sequence (fset 'range 'number-sequence))


;; use `pp' `pp-eval-expression' or `pp-eval-last-sexp'
(safe-fn-when cl-prettyexpand (fset 'pprint 'cl-prettyprint))





;; platform related functions

(platform-supported-when windows-nt
  (defmacro windows-nt-posix-path (p)
    "Return the posix path that windows-nt can recoganized."
    `(replace-regexp-in-string "\\\\" "/" ,p)))


(platform-supported-when windows-nt
  (defmacro windows-nt-unix-path (p)
    "Return the unix path that shell can regcoganized on windows-nt."
    `(replace-regexp-in-string
      ";" ":"
      (replace-regexp-in-string "\\([a-zA-Z]\\):/" "/\\1/"
                                (windows-nt-posix-path ,p)))))
