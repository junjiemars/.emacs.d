;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-mark-autoload.el
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

 ; end of `thingatpt' compatible definitions


(eval-when-compile
  (defmacro _mark_thing@_ (begin end)
    "Mark thing at point."
    `(progn
       ,begin
       (set-mark (point))
       ,end)))

(eval-when-compile
  (defmacro _mark_thing@_1 (begin end)
    "Mark thing at point."
    `(progn
       (goto-char ,begin)
       (set-mark (point))
       (goto-char ,end))))


;;; mark/kill-symbol

(eval-when-compile
  (defmacro _mark_symbol@_ (&optional n)
    (let ((n1 (gensym*)))
      `(let ((,n1 (or ,n 1))
             (ls (bounds-of-thing-at-point 'list))
             (bs (bounds-of-thing-at-point 'symbol))
             (pos (point)))
         (let ((cur (cond ((null bs) pos)
                          ((and (>= ,n1 0)
                                (> pos (car bs))
                                (< pos (cdr bs)))
                           (car bs))
                          ((and (< ,n1 0)
                                (> pos (car bs))
                                (< pos (cdr bs)))
                           (cdr bs))
                          (t pos))))
           (save-excursion
             (if (< ,n1 0)
                 (let ((lhs (save-excursion
                              (forward-symbol ,n1)
                              (skip-syntax-backward "'\"")
                              (point)))
                       (ss (save-excursion
                             (goto-char (car ls))
                             (skip-syntax-forward "'([{")
                             (point))))
                   (cons (max lhs ss) cur))
               (let ((rhs (save-excursion
                            (forward-symbol ,n1)
                            (skip-syntax-forward "\"")
                            (point)))
                     (ss (save-excursion
                           (goto-char (cdr ls))
                           (if (<= (skip-syntax-backward ")]}") -1)
                               (1- (cdr ls))
                             (cdr ls)))))
                 (cons cur (min rhs ss))))))))))


(defun mark-symbol@ (&optional n)
  "Mark symbol at point.

If prefix N is non-nil, then select the Nth symbol."
  (interactive "p")
  (let ((bs (_mark_symbol@_ n)))
    (_mark_thing@_1 (car bs) (cdr bs))))


(defun kill-whole-symbol (&optional n)
  "Kill current symbol.

With prefix N, do it N times forward if positive, or move
backwards N times if negative."
  (interactive "p")
  (let ((bs (_mark_symbol@_ n)))
    (kill-region (car bs) (cdr bs))))

 ; end of mark/kill-symbol


;;; mark/kill-sexp

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

(eval-when-compile
  (defmacro _mark_sexp@_ (&optional n)
    (let ((n1 (gensym*)))
      `(let ((,n1 (or ,n 1))
             (ls (bounds-of-thing-at-point 'list))
             (bs (bounds-of-thing-at-point 'symbol))
             (pos (point)))
         (let ((cur (cond ((null bs) pos)
                          ((and (>= ,n1 0)
                                (> pos (car bs))
                                (< pos (cdr bs)))
                           (car bs))
                          ((and (< ,n1 0) (> pos (car bs))
                                (< pos (cdr bs)))
                           (cdr bs))
                          (t pos))))
           (if (< ,n1 0)
               (let ((lhs (_forward_sexp_ ,n1))
                     (ss (save-excursion
                           (goto-char (car ls))
                           (skip-syntax-forward "'([{")
                           (point))))
                 (cons (max lhs ss) cur))
             (let ((rhs (_forward_sexp_ ,n1))
                   (ss (save-excursion
                         (goto-char (cdr ls))
                         (if (<= (skip-syntax-backward ")]}") -1)
                             (1- (cdr ls))
                           (cdr ls)))))
               (cons cur (min rhs ss)))))))))

(eval-when-compile
  (defmacro _mark_whole_sexp@_ (&optional boundary)
    (let ((bs (gensym*))
          (lhs (gensym*))
          (rhs (gensym*)))
      `(let ((,bs (bounds-of-thing-at-point 'list)))
         (cons
          (let ((,lhs (car ,bs)))
            (+ ,lhs (if ,boundary
                        0
                      (save-excursion
                        (goto-char ,lhs)
                        (skip-syntax-forward "'([{")))))
          (let ((,rhs (cdr ,bs)))
            (+ ,rhs
               (if ,boundary
                   0
                 (save-excursion
                   (goto-char ,rhs)
                   (save-excursion
                     (if (<= (skip-syntax-backward ")]}") -1)
                         -1 0)))))))))))


(defun mark-sexp@ (&optional n)
  "Mark sexp at point.

If prefix N is non nil, then forward or backward N sexps.
Otherwise, select the whole list."
  (interactive "p")
  (let ((bs (if (consp current-prefix-arg)
                (_mark_whole_sexp@_)
              (_mark_sexp@_ n))))
    (unless (and bs (car bs) (cdr bs))
      (user-error* "%s" "No sexp found"))
    (_mark_thing@_1 (car bs) (cdr bs))))


(defun kill-whole-sexp (&optional boundary)
  "Kill current sexp.

With prefix BOUNDARY, killing include BOUNDARY otherwise do not."
  (interactive "P")
  (let ((bs (_mark_whole_sexp@_ boundary)))
    (unless bs (user-error* "%s" "No whole sexp found"))
    (kill-region (car bs) (cdr bs))))

 ; end of mark/kill-sexp


;;; mark/kill-word

(eval-when-compile
  (defmacro _mark_word@_ (&optional n)
    (let ((n1 (gensym*)))
      `(let ((,n1 (or ,n 1)))
         (kill-region
          (goto-char
           (let ((b (bounds-of-thing-at-point 'word)))
             (unless b
               (save-excursion
                 (forward-word (if (>= n 0) 1 -1)))
               (setq b (bounds-of-thing-at-point 'word))
               (unless b (user-error* "%s" "No word found")))
             (if (>= n 0) (car b) (cdr b))))
          (progn
            (forward-word n)
            (point)))))))


(defun mark-word@ (&optional n)
  "Mark the word at point.

If prefix N is non nil, then forward or backward N words."
  (interactive "p")
  (let ((bounds
         (let ((n1 (if (not (consp current-prefix-arg))
                       (if (or (null n) (zerop n)) 1 n)
                     1)))
           (cons (save-excursion
                   (cond ((bounds-of-thing-at-point 'word)
                          (cond ((> n1 0)
                                 (forward-word)
                                 (backward-word))
                                (t (backward-word)
                                   (forward-word))))
                         ((bounds-of-thing-at-point 'whitespace)
                          (cond ((> n1 0)
                                 (forward-word)
                                 (backward-word))
                                (t (backward-word)
                                   (forward-word)))))
                   (point))
                 (save-excursion
                   (if (> n1 0)
                       (forward-word n1)
                     (backward-word (abs n1)))
                   (point))))))
    (when bounds
      (_mark_thing@_  (goto-char (car bounds))
                      (goto-char (cdr bounds))))))

(defun kill-whole-word (&optional n)
  "Kill current word.

With prefix N, do it N times forward if positive, or move
backwards N times if negative."
  (interactive "p")
  (kill-region (goto-char
                (let ((b (bounds-of-thing-at-point 'word)))
                  (unless b
                    (save-excursion
                      (forward-word (if (>= n 0) 1 -1)))
                    (setq b (bounds-of-thing-at-point 'word))
                    (unless b (user-error* "%s" "No word found")))
                  (if (>= n 0) (car b) (cdr b))))
               (progn
                 (forward-word n)
                 (point))))

 ; end of mark/kill-word


(defun mark-line@ (&optional indent)
  "Mark line at point.

If prefix INDENT is non-nil, then mark indent line."
  (interactive "P")
  (_mark_thing@_ (if indent
                     (back-to-indentation)
                   (beginning-of-line))
                 (end-of-line)))


;;; mark-defun

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

(defun mark-defun@ (&optional n)
  "Mark function at point.

If prefix N is non-nil, then forward or backward N functions."
  (interactive "p")
  (let ((bs (_mark_defun@_ n)))
    (unless (and bs (car bs) (cdr bs))
      (user-error* "%s" "No defun found"))
    (_mark_thing@_1 (cdr bs) (car bs))))

 ; end of mark-defun


;;; mark-filename

(defun mark-filename@ ()
  "Mark filename at point."
  (interactive)
  (let ((bs (bounds-of-thing-at-point 'filename)))
    (unless (and bs (car bs) (cdr bs))
      (user-error* "%s" "No filename found"))
    (_mark_thing@_1 (car bs) (cdr bs))))

 ; end of mark-filename


;;; mark-quoted


(defun mark-quoted@ (&optional enclose quoted)
  "Mark QUOTED thing at point.

If prefix ENCLOSE is non-nil, then mark the whole quoted thing.
If prefix QUOTED is non-nil, then mark nested quoted thing absolutely."
  (interactive
   (list (if (or (consp current-prefix-arg)
                 (and (numberp current-prefix-arg)
                      (> current-prefix-arg -1)))
             1 0)
         (cond ((or (consp current-prefix-arg)
                    (and (symbolp current-prefix-arg)
                         (eq '- current-prefix-arg))
                    (and (numberp current-prefix-arg)
                         (< current-prefix-arg -1)))
                nil)
               (current-prefix-arg
                (read-char (propertize "Input quoted character: "
                                       'face 'minibuffer-prompt))))))
  (let ((bs
         (catch 'block
           (let* ((lss '(?\" ?\' ?\( ?\[ ?\` ?\{ ?\‘))
                  (rss '(?\" ?\' ?\) ?\] ?\` ?\} ?\’))
                  (ls (if (characterp quoted)
                          (list quoted)
                        lss))
                  (rs (if (characterp quoted)
                          (let ((l (memq quoted lss)))
                            (cond (l (list (nth (- (length rss) (length l))
                                                rss)))
                                  (t (list quoted))))
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
                     (cond ((and quoted (not ss))
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
                            (let* ((s1 (nth (- (length ls) (length r)) ls))
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
                                                 (char= (caar ss) (car m)))
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
                     (t (cons (- cur li) (+ cur ri)))))))))
    (unless (and bs (car bs) (cdr bs))
      (user-error* "%s" "No quoted thing found"))
    (_mark_thing@_1 (- (car bs) enclose) (+ (cdr bs) enclose))))

 ; end of mark-quoted


;;; Keys

;; Kill
(define-key (current-global-map) (kbd "C-x M-d") #'kill-whole-word)
(define-key (current-global-map) (kbd "C-x M-s") #'kill-whole-symbol)
(define-key (current-global-map) (kbd "C-x M-e") #'kill-whole-sexp)
(define-key% (current-global-map) (kbd "C-x M-DEL") #'kill-whole-line)

;; Mark
(define-key (current-global-map) (kbd "C-c m s") #'mark-symbol@)
(define-key (current-global-map) (kbd "C-c m f") #'mark-filename@)
(define-key (current-global-map) (kbd "C-c m l") #'mark-line@)
(define-key (current-global-map) (kbd "C-c m q") #'mark-quoted@)
(define-key (current-global-map) (kbd "M-@") #'mark-word@)
(define-key (current-global-map) (kbd "C-M-SPC") #'mark-sexp@)
(define-key% (current-global-map) (kbd "C-M-@") #'mark-sexp)
(define-key (current-global-map) (kbd "C-M-h") #'mark-defun@)

 ; end of Keys


 ; end of on-mark-autoload.el
