;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; marks.el
;;;;


;;; require

;; end of require

(defun mark-thing (begin end)
  "Mark thing at point."
  (goto-char begin)
  (set-mark (point))
  (goto-char end))

(defun symbol@ (&optional thing)
  "Return the (cons \\='region|nil THING) at point."
  (if-region-active
      (let ((ss (buffer-substring-no-properties
                 (region-beginning) (region-end))))
        (setq mark-active nil)
        (cons 'region ss))
    (let ((ss (thing-at-point (or thing 'symbol))))
      (and ss (cons nil (substring-no-properties ss))))))



(defmacro _mark_symbol@_ ()
  `(bounds-of-thing-at-point 'symbol))

;; end of `_mark_symbol@_'


(defmacro _mark_string@_ ()
  `(bounds-of-thing-at-point 'string))

;; end of `_mark_string@_'

(defmacro _mark_filename@_ ()
  `(bounds-of-thing-at-point 'filename))


;; end of mark-* marco

;;; `_forward_symmetry_' `_backward_symmetry_'

(defmacro _forward_symmetry_ (chr pos rx ls rs)
  `(let ((cur ,pos) (ss (cons ,chr nil)))
     (catch 'br
       (while (< cur ,rx)
         (let ((l (and (= ,ls (char-after cur)) ,chr))
               (r (and (= ,rs (char-after cur)) ,chr)))
           (cond ((and r ss (= r (car ss)))
                  (setq ss (cdr ss)))
                 (l (setq ss (cons l ss)))))
         (when (null ss) (throw 'br cur))
         (setq cur (1+ cur))))))

(defmacro _backward_symmetry_ (chr pos lx ls rs)
  `(let ((cur ,pos) (ss (cons ,chr nil)))
     (catch 'br
       (while (> cur ,lx)
         (let ((l (and (= ,ls (char-before cur)) ,chr))
               (r (and (= ,rs (char-before cur)) ,chr)))
           (cond ((and l ss (= l (car ss)))
                  (setq ss (cdr ss)))
                 (r (setq ss (cons r ss)))))
         (when (null ss) (throw 'br cur))
         (setq cur (1- cur))))))

;; `_forward_symmetry_' `_backward_symmetry_'

;;; `_mark_quoted_symmetry@_'

(defun _mark_quoted_symmetry@_ ()
  (let* ((p (point)) (l1 p) (r1 p)
         (lx (point-min)) (rx (point-max))
         (ls "\"'`([{<")
         (rs "\"'`)]}>"))
    (while (and (> l1 lx)
                (null (strchr ls (char-before l1))))
      (setq l1 (1- l1)))
    (while (and (< r1 rx)
                (null (strchr rs (char-after r1))))
      (setq r1 (1+ r1)))
    (when (and l1 r1)
      (let* ((l2 (and r1 (let* ((c (char-after r1))
                                (i (strchr rs c)))
                           (_backward_symmetry_ c r1 lx (aref ls i) c))))
             (r2 (and l1 (let* ((c (char-before l1))
                                (i (strchr ls c)))
                           (_forward_symmetry_ c l1 rx c (aref rs i)))))
             (l3 (and r2 (let* ((c (char-after r2))
                                (i (strchr rs c)))
                           (_backward_symmetry_ c r2 lx (aref ls i) c))))
             (r3 (and l2 (let* ((c (char-before l2))
                                (i (strchr ls c)))
                           (_forward_symmetry_ c l2 rx c (aref rs i))))))
        (cond ((and l2 r2 l3 r3
                    (and (= l1 l2) (= l2 l3))
                    (and (= r1 r2) (= r2 r3)))
               (cons (1- l1) (1+ r1)))
              ((and l2 l3 r3 (= l1 l3) (= r1 r3) (> r2 r3))
               (cons (1- l1) (1+ r2)))
              ((and l2 l3 r3 (= l1 l3) (= r1 r3))
               (cons (1- l2) (1+ r3)))
              ((and r2 l3 (= l1 l3))
               (cons (1- l1) (1+ r2)))
              ((and l2 r3 (= r1 r3))
               (cons (1- l2) (1+ r1)))
              (t (cons (1- (if l2 (min l1 l2) l1))
                       (1+ (if r2 (max r1 r2) r1)))))))))

;; end of `_mark_quoted_symmetry@_'

;;; `_forward_asymmetry_' `_backward_asymmetry_'

(defmacro _forward_asymmetry_ (chr pos rx)
  `(let ((cur ,pos))
     (catch 'br
       (while (< cur ,rx)
         (when (char-equal ,chr (char-after cur))
           (throw 'br cur))
         (setq cur (1+ cur))))))

(defmacro _backward_asymmetry_ (chr pos lx)
  `(let ((cur ,pos))
     (catch 'br
       (while (> cur ,lx)
         (when (char-equal ,chr (char-before cur))
           (throw 'br cur))
         (setq cur (1- cur))))))


;; end of `_forward_asymmetry_'


(defun _mark_quoted_asymmetry@_ ()
  (let* ((p (point)) (l1 p) (r1 p)
         (lx (point-min)) (rx (point-max))
         (ls "\"'`([{<")
         (rs "\"'`)]}>"))
    (while (and (> l1 lx)
                (null (strchr ls (char-before l1))))
      (setq l1 (1- l1)))
    (while (and (< r1 rx)
                (null (strchr rs (char-after r1))))
      (setq r1 (1+ r1)))
    (when (and l1 r1)
      (let* ((l2 (and r1 (_backward_asymmetry_
                          (aref
                           ls (strchr rs (char-after r1)))
                          r1 lx)))
             (r2 (and l1 (_forward_asymmetry_
                          (aref
                           rs (strchr ls (char-before l1)))
                          l1 rx))))
        (cond ((and l2 r2 (and (= l1 l2) (= r1 r2)))
               (cons (1- l1) (1+ r1)))
              ((and l2 r2 (= r1 r2))
               (cons (1- l1) (1+ r2)))
              (l2 (cons (1- l2) (1+ r1)))
              (r2 (cons (1- l1) (1+ r2))))))))

;; end of `_mark_quoted_asymmetry@_'

;;; _mark_sexp@_

(defmacro _mark_sexp@_ (&optional n)
  `(let* ((n1 (or ,n 1))
          (p (point))
          (fp (condition-case _
                  (save-excursion
                    (forward-sexp n1)
                    (point))
                (scan-error p)))
          (bp (condition-case _
                  (save-excursion
                    (goto-char fp)
                    (forward-sexp (* -1 n1))
                    (point))
                (scan-error p))))
     (if (>= n1 0)
         (cons bp fp)
       (cons fp bp))))

;; end of `_mark_sexp@_'

;;; `_mark_whole_sexp@_

(defmacro _mark_whole_sexp@_ ()
  `(let* ((p (point)) (p1 p)
          (fp (condition-case _
                  (save-excursion
                    (while p1
                      (forward-sexp 1)
                      (setq p1 (point)))
                    p1)
                (scan-error p1)))
          (bp (condition-case _
                  (save-excursion
                    (goto-char fp)
                    (while p1
                      (forward-sexp -1)
                      (setq p1 (point))))
                (scan-error p1))))
     (cons bp fp)))

;; end of `_mark_whole_sexp@_

;;; `_mark_word@_'

(defmacro _mark_word@_ (&optional n)
  `(let* ((n1 (or ,n 1))
          (fp (save-excursion (forward-word n1) (point)))
          (bp (save-excursion (goto-char fp) (forward-word (* -1 n1))
                              (point))))
     (if (>= n1 0)
         (cons bp fp)
       (cons fp bp))))

;; end of `_mark_word@_'

;;; `_mark_line@_'

(defmacro _mark_line@_ (&optional n)
  `(let* ((n1 (or ,n 0))
          (fp (save-excursion (forward-line n1)
                              (if (> n1 0) (end-of-line) (beginning-of-line))
                              (point)))
          (bp (save-excursion (if (> n1 0) (beginning-of-line)
                                (end-of-line))
                              (point))))
     (cons fp bp)))

;; end of `_mark_line@_'

;;; `_mark_defun@_'

(defmacro _mark_defun@_ (&optional n)
  `(let ((n1 (or ,n 1)))
     (cons
      (save-excursion
        (cond ((bounds-of-thing-at-point 'defun)
               (if (> n1 0)
                   (beginning-of-defun)
                 (end-of-defun)))
              ((string-match "^[ \t\v]*$"
                             (string-trim>
                              (substring-no-properties
                               (thing-at-point 'line))))
               (if (> n1 0)
                   (beginning-of-defun -1)
                 (end-of-defun -1))))
        (point))
      (save-excursion
        (if (> n1 0)
            (end-of-defun n1)
          (beginning-of-defun (- n1)))
        (point)))))

;; end of `_mark_defun@_'


(provide 'marks)

;; end of marks.el
