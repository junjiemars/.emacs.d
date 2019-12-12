;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; fns.el
;;;;


(defalias 'range #'number-sequence)


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


;; end of file
