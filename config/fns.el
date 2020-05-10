;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; fns.el
;;;;


(defalias 'range #'number-sequence)


(defun flatten (seq)
  "Flatten SEQ."
  (cond ((atom seq) (list seq))
        ((null (cdr seq)) (flatten (car seq)))
        (t (append (flatten (car seq)) (flatten (cdr seq))))))


(defun take (n seq)
  "Returns a sequence of the first N items in SEQ.

Or all items if SEQ has fewer items than N."
  (let ((acc nil) (n1 n) (s1 seq))
    (while (and (> n1 0) s1)
      (setq acc (cons (car s1) acc)
            n1 (1- n1) s1 (cdr s1)))
    (nreverse acc)))


(defun drop-while (pred seq)
  "Returns a sequence of successive items from SEQ after the item 
for which (PRED item) returns t."
  (let ((s seq) (w nil))
    (while (and (not w) (car s))
      (if (funcall pred (car s))
          (setq w t)
        (setq s (cdr s))))
    (cdr s)))


(defun take-while (pred seq)
  "Returns a sequence of successive items from SEQ before the
item for which (PRED item) returns t."
  (let ((s seq) (w nil) (s1 nil))
    (while (and (not w) (car s))
      (if (funcall pred (car s))
          (setq w t)
        (setq s1 (cons (car s) s1)
              s (cdr s))))
    (nreverse s1)))


;; Load cl-lib/cl at runtime
(if-version% <= 24
             (when-version% > 26
               (require 'cl-lib))
  (with-no-warnings
    (require 'cl)))


(defmacro assoc** (key list &optional testfn)
  "Return non-nil if KEY is equal to the `car' of an element of LIST.

The value is actually the first element of LIST whose car equals KEY.
Equality is defined by TESTFN if non-nil or by `equal' if nil."
  (if-version%
      <= 26.1
      `(assoc ,key ,list ,testfn)
    (if-fn% 'cl-assoc 'cl-lib
            (progn%
             (declare-function cl-assoc "cl-seq.elc"
                               (item seq &rest keys)
                               t)
             `(cl-assoc ,key ,list :test (or ,testfn #'equal)))
      (when-fn% 'assoc* 'cl
        `(with-no-warnings
           (assoc* ,key ,list :test (or ,testfn #'equal)))))))


;; Unify `cl-mapcar' and `mapcar*'
(defmacro mapcar** (fn seq &rest seqs)
  "Apply FUNCTION to each element of SEQ, and make a list of the results.
If there are several SEQs, FUNCTION is called with that many arguments,
and mapping stops as soon as the shortest list runs out.  With just one
SEQ, this is like `mapcar'.  With several, it is like the Common Lisp
`mapcar' function extended to arbitrary sequence types.
\n(fn FUNCTION SEQ...)"
  (if-fn% 'cl-mapcar 'cl-lib
          (if-version% <= 25
                       `(cl-mapcar ,fn ,seq ,@seqs)
            (declare-function cl-mapcar "cl-lib.elc"
                              (fn x &rest rest)
                              t)
            `(cl-mapcar ,fn ,seq ,@seqs))
    `(when-fn% 'mapcar* 'cl
       (with-no-warnings
         (mapcar* ,fn ,seq ,@seqs)))))


;; Unify `cl-remove' and `remove*'
(defmacro remove** (item seq &rest keys)
  "Remove all occurrences of ITEM in SEQ.
This is a non-destructive function; it makes a copy of SEQ if necessary
to avoid corrupting the original SEQ.
\nKeywords supported:  :test :test-not :key :count :start :end :from-end
\n(fn ITEM SEQ [KEYWORD VALUE]...)"
  (if-fn% 'cl-remove 'cl-lib
          (if-version% <= 25
                       `(cl-remove ,item ,seq ,@keys)
            (declare-function cl-remove "cl-seq.elc"
                              (item seq &rest keys)
                              t)
            `(cl-remove ,item ,seq ,@keys))
    `(with-no-warnings
       (remove* ,item ,seq ,@keys))))


;; Unify `cl-member' and `member*'
(defmacro member** (item list &rest keys)
  "Find the first occurrence of ITEM in LIST.
Return the sublist of LIST whose car is ITEM.
\nKeywords supported:  :test :test-not :key
\n(fn ITEM LIST [KEYWORD VALUE]...)"
  (if-fn% 'cl-member 'cl-lib
          (if-version% <= 25
                       `(cl-member ,item ,list ,@keys)
            (declare-function cl-member (item seq &rest keys)
                              t)
            `(cl-member ,item ,list ,@keys))
    `(with-no-warnings
       (member* ,item ,list ,@keys))))


(defmacro every* (pred &rest seq)
  "Return t if PRED is true of every element of SEQ or SEQs."
  (declare (indent 1))
  (if-fn% 'cl-every 'cl-lib
          (if-version% <= 25
                       `(cl-every ,pred ,@seq)
            (declare-function cl-every "cl-extra.elc"
                              (pred seq &rest rest)
                              t)
            `(cl-every ,pred ,@seq))
    (when-fn% 'every 'cl
      `(with-no-warnings
         (every ,pred ,@seq)))))


(defmacro some* (pred &rest seq)
  "Return t if PRED is true of any element of SEQ or SEQs."
  (declare (indent 1))
  (if-fn% 'cl-some 'cl-lib
          (if-version% <= 25
                       `(cl-some ,pred ,@seq)
            (declare-function cl-some "cl-extra.elc"
                              (pred seq &rest rest)
                              t)
            `(cl-some ,pred ,@seq))
    (when-fn% 'some 'cl
      `(with-no-warnings
         (some ,pred ,@seq)))))




;; end of file
