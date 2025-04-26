;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; cls.el
;;;;
;; Commentary: common lisp fn.
;;;;



;; Load cl-lib/cl at runtime
(eval-when-compile
  (if-version%
      <= 24.1
      (require 'cl-lib)
    (require 'cl)))

(fset 'assoc**
      (if-fn% cl-assoc cl-lib
              (prog1 #'cl-assoc
                (declare-function cl-assoc 'cl-lib))
        #'assoc*))

(fset 'mapcar**
      (if-fn% cl-mapcar cl-lib
              (prog1 #'cl-mapcar
                (declare-function cl-mapcar 'cl-lib))
        #'mapcar*))

(fset 'remove-if*
      (if-fn% cl-remove-if cl-lib
              (prog1 #'cl-remove-if
                (declare-function cl-remove-if 'cl-lib))
        #'remove-if))

(fset 'member-if*
      (if-fn% cl-member-if cl-lib
              (prog1 #'cl-member-if
                (declare-function cl-member-if 'cl-lib))
        #'member-if))

(fset 'every*
      (if-fn% cl-every cl-lib
              (prog1 #'cl-every
                (declare-function cl-every 'cl-lib))
        #'every))

(fset 'some*
      (if-fn% cl-some cl-lib
              (prog1 #'cl-some
                (declare-function cl-some 'cl-lib))
        #'some))

(fset 'loop*
      (if-fn% cl-loop cl-lib
              (prog1 #'cl-loop
                (declare-function cl-loop 'cl-lib))
        #'loop))

;; end of common-lisp

;;;
;; seq
;;;

(fset 'drop #'nthcdr)
(fset 'range 'number-sequence)

(defun drop-while (pred seq)
  "Return a sequence of items from SEQ drop while PRED is t."
  (let ((s nil))
    (while seq
      (unless (funcall pred (car seq))
        (setq s (cons (car seq) s)))
      (setq seq (cdr seq)))
    (nreverse s)))

(defun insert-at (newelt seq idx)
  (cond ((null (integerp idx)) seq)
        ((null seq) (list newelt))
        (t (let ((acc nil))
             (while (> idx 0)
               (setq  acc (cons (car seq) acc)
                      seq (cdr seq)
                      idx (1- idx)))
             (nconc (nreverse acc) (cons newelt seq))))))

(defmacro insert! (newelt seq idx)
  "Insert NEWELT into the SEQ."
  `(setq ,seq (insert-at ,newelt ,seq ,idx)))

(defun flatten (seq)
  "Flatten SEQ."
  (cond ((atom seq) (list seq))
        ((null (cdr seq)) (flatten (car seq)))
        (t (append (flatten (car seq)) (flatten (cdr seq))))))

(unless-fn% take nil
  (defun take (n seq)
    "Return a sequence of the first N items in SEQ."
    (let ((s nil))
      (while (and (> n 0) seq)
        (setq s (cons (car seq) s)
              n (1- n)
              seq (cdr seq)))
      (nreverse s))))

(defun take-while (pred seq)
  "Return a sequence of items from SEQ just take while PRED is t."
  (let ((s nil))
    (while seq
      (when (funcall pred (car seq))
        (setq s (cons (car seq) s)))
      (setq seq (cdr seq)))
    (nreverse s)))

;; end of seq

(provide 'cls)

;; end of cls.el
