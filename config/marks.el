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
  (defmacro _mark_filename@_ ()
    `(bounds-of-thing-at-point 'filename)))


;; end of mark-* marco

(eval-when-compile
  (defmacro _mark_quoted@_ ()
    `(let* ((p (point)) (l1 p) (r1 p)
            (lx (point-min)) (rx (point-max))
            (ls "\"'([{‘")
            (rs "\"')]}’"))
       (while (and (> l1 lx)
                   (null (strchr ls (char-before l1))))
         (setq l1 (1- l1)))
       (while (and (< r1 rx)
                   (null (strchr rs (char-after r1))))
         (setq r1 (1+ r1)))
       (cond ((and l1 r1 (= (strchr ls (char-before l1))
                            (strchr rs (char-after r1))))
              (cons (1- l1) (1+ r1)))
             ((and l1 r1)
              (if (<= (- p l1) (- r1 p))
                  (let ((r2 (1+ r1)))
                    (while
                        (and (< r2 rx)
                             (null (strchr rs (char-after r2))))
                      (setq r2 (1+ r2)))
                    (cond ((= (strchr ls (char-before l1))
                              (strchr rs (char-after r2)))
                           (cons (1- l1) (1+ r2)))
                          (t (let ((l3 (1- l1)))
                               (while
                                   (and (> l3 lx)
                                        (null (strchr ls (char-before l3))))
                                 (setq l3 (1- l3)))
                               (when (= (strchr ls (char-before l3))
                                        (strchr ls (char-after r1)))
                                 (cons (1- l3) (1+ r1)))))))
                (let ((l2 (1- l1)))
                  (while (and (> l2 lx)
                              (null (strchr ls (char-before l2))))
                    (setq l2 (1- l2)))
                  (cond ((= (strchr ls (char-before l2))
                            (strchr rs (char-after r1)))
                         (cons (1- l2) (1+ r1)))
                        (t (let ((r3 (1+ r1)))
                             (while
                                 (and (< r3 rx)
                                      (null (strchr rs (char-after r3))))
                               (setq r3 (1+ r3)))
                             (when (= (strchr ls (char-before l1))
                                      (strchr rs (char-after r3)))
                               (cons (1- l1) (1+ r3)))))))))))))

;; end of `_mark_quoted@_'

;;; `_forward_sexp_'

(eval-when-compile
  (defmacro _forward_sexp_ (n)
    (let ((n1 (gensym*)))
      `(lexical-let*% ((,n1 ,n)
                       (i (abs ,n1))
                       (pre (point))
                       (_ nil))
         (condition-case _
             (save-excursion
               (while (> i 0)
                 (setq pre (point))
                 (forward-sexp (if (>= ,n1 0) 1 -1))
                 (setq i (1- i)))
               (point))
           (scan-error pre))))))

;; end of `_forward_sexp_'


(eval-when-compile
  (defmacro _mark_sexp@_ (&optional n)
    (let ((n1 (gensym*)))
      `(lexical-let*% ((,n1 (or ,n 1))
                       (p (point))
                       (bs (bounds-of-thing-at-point 'sexp))
                       (fs (save-excursion
                             (goto-char (if (>= ,n1 0) (1- p) (1+ p)))
                             (goto-char (_forward_sexp_ ,n1))
                             (bounds-of-thing-at-point 'sexp))))
         (cons (if (>= ,n1 0)
                   (or (car bs) p)
                 (or (car fs) (car bs) p))
               (if (>= ,n1 0)
                   (or (cdr fs) (cdr bs) p)
                 (or (cdr bs) p)))))))

;; end of `_mark_sexp@_'

;;; `_mark_whole_sexp@_'

(eval-when-compile
  (defmacro _mark_whole_sexp@_ (&optional boundary)
    (let ((b1 (gensym*)))
      `(lexical-let*% ((,b1 ,boundary)
                       (p (point))
                       (bs (bounds-of-thing-at-point 'list))
                       (ls (save-excursion
                             (goto-char (if (car bs)
                                            (1+ (car bs))
                                          (1+ p)))
                             (goto-char (_forward_sexp_ -1))
                             (bounds-of-thing-at-point 'sexp)))
                       (rs (save-excursion
                             (goto-char (if (cdr bs)
                                            (1- (cdr bs))
                                          (1- p)))
                             (goto-char (_forward_sexp_ 1))
                             (bounds-of-thing-at-point 'sexp))))
         (cons (or (and ,b1 (car bs)) (car ls) p)
               (or (and ,b1 (cdr bs)) (cdr rs) p))))))

;; end of `_mark_whole_sexp@_'

;;; `_mark_word@_'

(eval-when-compile
  (defmacro _mark_word@_ (&optional n)
    (let ((n1 (gensym*)))
      `(lexical-let*% ((,n1 (or ,n 1))
                       (p (point))
                       (bs (bounds-of-thing-at-point 'word))
                       (fs (save-excursion
                             (goto-char (if (>= ,n1 0) (1- p) (1+ p)))
                             (forward-word ,n1)
                             (bounds-of-thing-at-point 'word))))
         (cons (if (>= ,n1 0)
                   (or (car bs) p)
                 (or (car fs) (car bs) p))
               (if (>= ,n1 0)
                   (or (cdr fs) (cdr bs) p)
                 (or (cdr bs) p)))))))

;; end of `_mark_word@_'

;;; `_mark_whole_word@_'

(eval-when-compile
  (defmacro _mark_whole_word@_ ()
    `(lexical-let*% ((p (point))
                     (bs (bounds-of-thing-at-point 'symbol))
                     (ls (save-excursion
                           (goto-char (if (car bs)
                                          (1+ (car bs))
                                        (1+ p)))
                           (forward-word -1)
                           (bounds-of-thing-at-point 'word)))
                     (rs (save-excursion
                           (goto-char (if (cdr bs)
                                          (1- (cdr bs))
                                        (1- p)))
                           (forward-word 1)
                           (bounds-of-thing-at-point 'word))))
       (cons (or (car bs) (car ls) p)
             (or (cdr bs) (cdr rs) p)))))

;; end of `_mark_whole_word@_'


(eval-when-compile
  (defmacro _mark_defun@_ (&optional n)
    (let ((n1 (gensym*)))
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
