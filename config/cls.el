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

(defmacro drop (n seq)
  "Return rest sequence after drop the first N items in SEQ."
  `(nthcdr ,n ,seq))

(defun drop-while (pred seq)
  "Return a sequence of items from SEQ drop while PRED is t."
  (let ((s nil))
    (while seq
      (unless (funcall pred (car seq))
        (setq s (cons (car seq) s)))
      (setq seq (cdr seq)))
    (nreverse s)))

(defmacro insert! (newelt seq idx)
  "Insert NEWELT into the SEQ."
  (let ((n1 (gensym*)) (i1 (gensym*)))
    `(let ((,n1 ,newelt) (,i1 ,idx))
       (let ((l (length ,seq)))
         (when (and (integerp ,i1) (>= ,i1 0) (< ,i1 l))
           (let ((c1 (copy-sequence ,seq))
                 (j (1+ ,i1)))
             (while (< j l)
               (setf (nth j c1) (nth (1- j) ,seq) j (1+ j)))
             (setf (nth ,i1 c1) ,n1)
             (setq ,seq (append c1 `(,(nth (1- l) ,seq))))))))))

(defun flatten (seq)
  "Flatten SEQ."
  (cond ((atom seq) (list seq))
        ((null (cdr seq)) (flatten (car seq)))
        (t (append (flatten (car seq)) (flatten (cdr seq))))))

(defmacro range (from &optional to inc)
  `(number-sequence ,from ,to ,inc))

(unless-fn% take nil
  (defun take (n seq)
    "Return a sequence of the first N items in SEQ."
    (let ((s nil))
      (while (and (> n 0) seq)
        (setq s (cons (car seq) s)
              n (1- n)
              seq (cdr seq)))
      (nreverse s))))

(defmacro take* (n seq)
  `(take ,n ,seq))

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
