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
  (defmacro _forward_sexp_ (n)
    (let ((n1 (gensym*))
          (n2 (gensym*))
          (pre (gensym*))
          (err (gensym*)))
      `(let* ((,n1 ,n)
              (,n2 (abs ,n1))
              (,pre (point))
              (,err nil))
         (condition-case ,err
             (save-excursion
               (while (> ,n2 0)
                 (setq ,pre (point))
                 (forward-sexp (if (>= ,n1 0) 1 -1))
                 (setq ,n2 (1- ,n2)))
               (point))
           (scan-error ,pre))))))

;; end of `_forward_sexp_'


(eval-when-compile
  (defmacro _mark_sexp@_ (&optional n)
    (let ((n1 (gensym*))
          (bs (gensym*))
          (fs (gensym*))
          (p (gensym*)))
      `(let* ((,n1 (or ,n 1))
              (,p (point))
              (,bs (bounds-of-thing-at-point 'sexp))
              (,fs (save-excursion
                     (goto-char (_forward_sexp_ ,n1))
                     (bounds-of-thing-at-point 'sexp))))
         (cons (if (>= ,n1 0)
                   (or (car ,bs) ,p)
                 (or (car ,fs) (car ,bs) ,p))
               (if (>= ,n1 0)
                   (or (cdr ,fs) (cdr ,bs) ,p)
                 (or (cdr ,bs) ,p)))))))

;; end of `_mark_sexp@_'


(eval-when-compile
  (defmacro _mark_whole_sexp@_ (&optional boundary)
    (let ((b1 (gensym*))
          (bs (gensym*))
          (p (gensym*))
          (ls (gensym*))
          (rs (gensym*)))
      `(let* ((,b1 ,boundary)
              (,p (point))
              (,bs (bounds-of-thing-at-point 'list))
              (,ls (save-excursion
                     (goto-char (or (car ,bs) ,p))
                     (skip-syntax-forward "'([{")
                     (goto-char (_forward_sexp_ -1))
                     (bounds-of-thing-at-point 'sexp)))
              (,rs (save-excursion
                     (goto-char (or (cdr ,bs) ,p))
                     (skip-syntax-backward ")]}")
                     (goto-char (_forward_sexp_ 1))
                     (bounds-of-thing-at-point 'sexp))))
         (cons (or (and ,b1 (car ,bs)) (car ,ls) ,p)
               (or (and ,b1 (cdr ,bs)) (cdr ,rs) ,p))))))

;; end of `_mark_whole_sexp@_'


(eval-when-compile
  (defmacro _mark_word@_ (&optional n)
    (let ((n1 (gensym*))
          (bs (gensym*))
          (p (gensym*))
          (fs (gensym*)))
      `(let* ((,n1 (or ,n 1))
              (,p (point))
              (,bs (bounds-of-thing-at-point 'word))
              (,fs (save-excursion
                     (forward-word ,n1)
                     (bounds-of-thing-at-point 'word))))
         (cons (if (>= ,n1 0)
                   (or (car ,bs) ,p)
                 (or (car ,fs) (car ,bs) ,p))
               (if (>= ,n1 0)
                   (or (cdr ,fs) (cdr ,bs) ,p)
                 (or (cdr ,bs) ,p)))))))

;; end of `_mark_word@_'


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


(eval-when-compile
  (defmacro _mark_quoted@_ (&optional boundary quoted)
    (let ((bnd (gensym*))
          (qut (gensym*)))
      `(let* ((,bnd ,boundary)
              (,qut ,quoted))
         (catch 'block
           (let* ((lss '(?\" ?\' ?\( ?\[ ?\` ?\{ ?\‘))
                  (rss '(?\" ?\' ?\) ?\] ?\` ?\} ?\’))
                  (ls (if (characterp ,qut)
                          (list ,qut)
                        lss))
                  (rs (if (characterp ,qut)
                          (let ((l (memq ,qut lss)))
                            (cond (l (list
                                      (nth (- (length rss) (length l))
                                           rss)))
                                  (t (list ,qut))))
                        rss))
                  (cur (point))
                  (min (point-min))
                  (max (point-max))
                  (lproc
                   (lambda (i &optional l)
                     (catch 'left
                       (let ((s))
                         (while (> (- cur i) min)
                           (let ((c (char-before (- cur i)))
                                 (r (when l
                                      (nth (- (length rs)
                                              (length (memq l ls)))
                                           rs))))
                             (cond ((and l s (char= c l))
                                    (setq s (cdr s)))
                                   ((and r (char= c r))
                                    (setq s (cons (cons r i) s)))
                                   ((and r (not s) (char= c l))
                                    (throw 'left (cons l i)))
                                   ((and (not l) (memq c ls))
                                    (throw 'left (cons c i)))))
                           (setq i (1+ i)))
                         (cond ((caar s)
                                (cons l (cdar s)))
                               (t i))))))
                  (rproc
                   (lambda (i &optional r)
                     (catch 'right
                       (let ((s))
                         (while (< (+ cur i) max)
                           (let ((c (char-after (+ cur i)))
                                 (l (when r
                                      (nth (- (length ls)
                                              (length (memq r rs)))
                                           ls))))
                             (cond ((and r s (char= c r))
                                    (setq s (cdr s)))
                                   ((and l (char= c l))
                                    (setq s (cons (cons l i) s)))
                                   ((and l (not s) (char= c r))
                                    (throw 'right (cons r i)))
                                   ((and (not r) (memq c rs))
                                    (throw 'right (cons c i)))))
                           (setq i (1+ i)))
                         (cond ((caar s)
                                (cons r (cdar s)))
                               (t i)))))))
             (let ((li 0) (ri 0) (ss))
               (catch 'q
                 (while (> (- cur li) min)
                   (let* ((c (char-before (- cur li)))
                          (l (memq c ls))
                          (r (memq c rs))
                          (s (when l (nth (- (length rs)
                                             (length l))
                                          rs))))
                     (cond ((and ,qut (not ss))
                            (let ((m (funcall rproc ri (car rs)))
                                  (n (funcall lproc li (car ls))))
                              (cond ((and (consp m) (consp n))
                                     (setq ri (cdr m) li (cdr n))
                                     (throw 'q nil))
                                    ((consp m)
                                     (setq ri (cdr m))))))
                           ((and l (not r) (not ss))
                            (let ((m (funcall rproc ri s)))
                              (cond ((and (consp m) (char= s (car m)))
                                     (setq ri (cdr m))
                                     (throw 'q nil)))))
                           ((and (not l) r (not ss))
                            (let* ((s1 (nth (- (length ls) (length r))
                                            ls))
                                   (m (funcall rproc ri c))
                                   (n (funcall lproc li s1)))
                              (cond ((and (consp m) (consp n)
                                          (char= c (car m))
                                          (char= s1 (car n)))
                                     (setq ri (cdr m)
                                           li (cdr n))
                                     (throw 'q nil))
                                    (t (setq ss (cons (cons c li) ss))))))
                           ((and l (not r) ss
                                 (not (memq (caar ss) ls))
                                 (not (char= c (caar ss)))
                                 (not (char= s (caar ss))))
                            (let ((m (funcall rproc ri s)))
                              (cond ((and (consp m)
                                          (char= s (car m)))
                                     (setq ri (cdr m))
                                     (throw 'q nil))
                                    (t (throw 'block nil)))))
                           ((and l (not r) ss
                                 (not (char= c (caar ss)))
                                 (not (char= s (caar ss))))
                            (cond ((char= (caar ss)
                                          (char-after (+ cur ri)))
                                   (let ((m (funcall lproc 0 (caar ss))))
                                     (cond ((and (consp m)
                                                 (char= (caar ss)
                                                        (car m)))
                                            (setq li (cdr m))
                                            (throw 'q nil)))))
                                  (t (let ((m (funcall rproc ri s)))
                                       (cond ((and (consp m)
                                                   (char= s (car m)))
                                              (setq ri (cdr m))
                                              (throw 'q nil))
                                             ((not (consp m))
                                              (setq li (cdar ss))
                                              (throw 'block nil)))))))
                           ((and l r ss (not (char= c (caar ss))))
                            (let ((m (funcall rproc ri)))
                              (cond ((and (consp m) (char= c (car m)))
                                     (setq ri (cdr m)))
                                    ((and (char= (caar ss)
                                                 (char-after (+ cur ri))))
                                     (setq li (cdar ss))
                                     (throw 'q nil)))))
                           ((and l (not ss))
                            (let ((m (funcall rproc ri)))
                              (cond ((and (consp m) (char= s (car m)))
                                     (let ((n1 (funcall lproc (1+ li) c))
                                           (m1 (funcall rproc
                                                        (1+ (cdr m))
                                                        s)))
                                       (cond ((or (not (consp n1))
                                                  (not (consp m1)))
                                              (setq ri (cdr m))
                                              (throw 'q nil))
                                             (t (setq ss
                                                      (cons (cons c li)
                                                            ss))))))
                                    (t (setq ss (cons (cons c li) ss))))))
                           ((and l ss (char= s (caar ss)))
                            (setq ss (cdr ss)))
                           ((or l r)
                            (setq ss (cons (cons c li) ss))))
                     (setq li (1+ li)))))
               (cond ((= (- cur li) min)
                      (cond (ss (let ((n (funcall
                                          lproc
                                          0
                                          (char-after (+ cur ri)))))
                                  (cond ((consp n)
                                         (setq li (cdr n))))))
                            (t (throw 'block nil))))
                     (t (cons (- cur li ,bnd)
                              (+ cur ri ,bnd)))))))))))

;; end of `_mark_quoted@_'


(eval-when-compile
  (defmacro _mark_filename@_ ()
    `(bounds-of-thing-at-point 'filename)))


;; end of mark-* marco


(provide 'marks)

;; end of marks.el
