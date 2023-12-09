;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; marks.el
;;;;


;;; `thingatpt' compatible definitions

(unless-fn% 'thing-at-point-bounds-of-string-at-point 'thingatpt
  ;; fix wrong behavior on anicent Emacs.
  (defun thing-at-point-bounds-of-string-at-point ()
    "Return the bounds of the double quoted string at point."
    (save-excursion
      (let ((beg (nth 8 (syntax-ppss))))
        (when beg
          (goto-char beg)
          (forward-sexp)
          (cons (1+  beg) (1- (point)))))))

  (put 'string 'bounds-of-thing-at-point
       'thing-at-point-bounds-of-string-at-point))


(unless-fn% 'thing-at-point-bounds-of-list-at-point 'thingatpt
  ;; fix wrong behavior on ancient Emacs.
  (defun thing-at-point-bounds-of-list-at-point ()
    "Return the bounds of the list at point."
    (save-excursion
      (let* ((st (parse-partial-sexp (point-min) (point)))
             (beg (or (and (eq 4 (car (syntax-after (point))))
                           (not (nth 8 st))
                           (point))
                      (nth 1 st))))
        (when beg
          (goto-char beg)
          (forward-sexp)
          (cons beg (point))))))

  (put 'list 'bounds-of-thing-at-point
       'thing-at-point-bounds-of-list-at-point))


(unless% (or (get 'defun 'beginning-of-defun)
             (get 'defun 'end-of-defun))
  ;; fix wrong behavior on ancient Emacs.
  (put 'defun 'beginning-op 'beginning-of-defun)
  (put 'defun 'end-op       'end-of-defun)
  (put 'defun 'forward-op   'end-of-defun))

;; end of `thingatpt' compatible definitions


;;; mark-* macro

(eval-when-compile
  (defmacro _mark_thing_ (begin end)
    "Mark thing at point."
    `(progn
       (goto-char ,begin)
       (set-mark (point))
       (goto-char ,end))))

;; end of `_mark_thing_'


(eval-when-compile
  (defmacro _mark_symbol@_ ()
    `(bounds-of-thing-at-point 'symbol)))

;; end of `_mark_symbol@_'


(eval-when-compile
  (defmacro _mark_string@_ ()
    `(bounds-of-thing-at-point 'string)))

;; end of `_mark_string@_'

(eval-when-compile
  (defmacro _mark_filename@_ ()
    `(bounds-of-thing-at-point 'filename)))


;; end of mark-* marco

;;; `_forward_symmetry_' `_backward_symmetry_'

(eval-when-compile
  (defmacro _forward_symmetry_ (chr pos rx ls rs)
    (let ((chr1 (gensym))
          (pos1 (gensym))
          (rx1 (gensym))
          (ls1 (gensym))
          (rs1 (gensym)))
      `(let* ((,chr1 ,chr) (,pos1 ,pos) (,rx1 ,rx)
              (,ls1 ,ls) (,rs1 ,rs)
              (cur ,pos1) (ss (cons ,chr1 nil)))
         (catch 'break
           (while (< cur ,rx1)
             (let ((l (and (= ,ls1 (char-after cur)) ,chr1))
                   (r (and (= ,rs1 (char-after cur)) ,chr1)))
               (cond ((and r ss (= r (car ss)))
                      (setq ss (cdr ss)))
                     (l (setq ss (cons l ss)))))
             (when (null ss) (throw 'break cur))
             (setq cur (1+ cur))))))))

(eval-when-compile
  (defmacro _backward_symmetry_ (chr pos lx ls rs)
    (let ((chr1 (gensym))
          (pos1 (gensym))
          (lx1 (gensym))
          (ls1 (gensym))
          (rs1 (gensym)))
      `(let* ((,chr1 ,chr) (,pos1 ,pos) (,lx1 ,lx)
              (,ls1 ,ls) (,rs1 ,rs)
              (cur ,pos1) (ss (cons ,chr1 nil)))
         (catch 'break
           (while (> cur ,lx1)
             (let ((l (and (= ,ls1 (char-before cur)) ,chr1))
                   (r (and (= ,rs1 (char-before cur)) ,chr1)))
               (cond ((and l ss (= l (car ss)))
                      (setq ss (cdr ss)))
                     (r (setq ss (cons r ss)))))
             (when (null ss) (throw 'break cur))
             (setq cur (1- cur))))))))

;; `_forward_symmetry_' `_backward_symmetry_'

;;; `_mark_quoted_symmetry@_'

(eval-when-compile
  (defmacro _mark_quoted_symmetry@_ ()
    `(let* ((p (point)) (l1 p) (r1 p)
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
  														(_backward_symmetry_
  														 c r1 lx (aref ls i) c))))
                (r2 (and l1 (let* ((c (char-before l1))
  																 (i (strchr ls c)))
  														(_forward_symmetry_
  														 c l1 rx c (aref rs i)))))
                (l3 (and r2 (let* ((c (char-after r2))
  																 (i (strchr rs c)))
  														(_backward_symmetry_
  														 c r2 lx (aref ls i) c))))
                (r3 (and l2 (let* ((c (char-before l2))
  																 (i (strchr ls c)))
  														(_forward_symmetry_
  														 c l2 rx c (aref rs i))))))
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
                          (1+ (if r2 (max r1 r2) r1))))))))))

;; end of `_mark_quoted_symmetry@_'

;;; `_forward_asymmetry_' `_backward_asymmetry_'

(eval-when-compile
  (defmacro _forward_asymmetry_ (chr pos rx)
    (let ((chr1 (gensym))
          (pos1 (gensym))
          (rx1 (gensym)))
      `(let* ((,chr1 ,chr) (,pos1 ,pos) (,rx1 ,rx)
              (cur ,pos1))
         (catch 'break
           (while (< cur ,rx1)
             (when (char= ,chr1 (char-after cur))
               (throw 'break cur))
             (setq cur (1+ cur))))))))

(eval-when-compile
  (defmacro _backward_asymmetry_ (chr pos lx)
    (let ((chr1 (gensym))
          (pos1 (gensym))
          (lx1 (gensym)))
      `(let* ((,chr1 ,chr) (,pos1 ,pos) (,lx1 ,lx)
              (cur ,pos1))
         (catch 'break
           (while (> cur ,lx1)
             (when (char= ,chr1 (char-before cur))
               (throw 'break cur))
             (setq cur (1- cur))))))))


;; end of `_forward_asymmetry_'


(eval-when-compile
  (defmacro _mark_quoted_asymmetry@_ ()
    `(let* ((p (point)) (l1 p) (r1 p)
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
                 (r2 (cons (1- l1) (1+ r2)))))))))

;; end of `_mark_quoted_asymmetry@_'

;;; _mark_sexp@_

(eval-when-compile
  (defmacro _mark_sexp@_ (&optional n)
    (let ((n1 (gensym)))
      `(let* ((,n1 (or ,n 1))
              (p (point))
              (fp (condition-case _
                      (save-excursion
                        (forward-sexp ,n1)
                        (point))
                    (scan-error p)))
              (bp (condition-case _
                      (save-excursion
                        (goto-char fp)
                        (forward-sexp (* -1 ,n1))
                        (point))
                    (scan-error p))))
         (if (>= ,n1 0)
             (cons bp fp)
           (cons fp bp))))))

;; end of `_mark_sexp@_'

;;; `_mark_whole_sexp@_

(eval-when-compile
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
       (cons bp fp))))

;; end of `_mark_whole_sexp@_

;;; `_mark_word@_'

(eval-when-compile
  (defmacro _mark_word@_ (&optional n)
    (let ((n1 (gensym)))
      `(let* ((,n1 (or ,n 1))
              (fp (save-excursion
                    (forward-word ,n1)
                    (point)))
              (bp (save-excursion
                    (goto-char fp)
                    (forward-word (* -1 ,n1))
                    (point))))
         (if (>= ,n1 0)
             (cons bp fp)
           (cons fp bp))))))

;; end of `_mark_word@_'


(eval-when-compile
  (defmacro _mark_defun@_ (&optional n)
    (let ((n1 (gensym)))
      `(let ((,n1 (or ,n 1)))
         (cons
          (save-excursion
            (cond ((bounds-of-thing-at-point 'defun)
                   (if (> ,n1 0)
                       (beginning-of-defun)
                     (end-of-defun)))
                  ((string-match "^[ \t\v]*$"
                                 (string-trim>
                                  (substring-no-properties
                                   (thing-at-point 'line))))
                   (if (> ,n1 0)
                       (beginning-of-defun -1)
                     (end-of-defun -1))))
            (point))
          (save-excursion
            (if (> ,n1 0)
                (end-of-defun ,n1)
              (beginning-of-defun (- ,n1)))
            (point)))))))

;; end of `_mark_defun@_'


(provide 'marks)

;; end of marks.el
