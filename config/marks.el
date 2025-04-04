;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; marks.el
;;;;
;; commentary: mark or kill thing.
;;;;
;; naming:
;;; 1. `xxx@' is function.
;;; 2. `mark-xxx@' is command.
;;;;


;;; require

(eval-when-compile
  (require 'thingatpt nil t))

 ;; end of require

(defun mark-thing (begin end)
  "Mark thing at point."
  (goto-char begin)
  (set-mark (point))
  (goto-char end))



;;;
;; symbol
;;;

(defun symbol@ (&optional thing)
  "Return the (cons \\='region|nil THING) at point."
  (if-region-active
      (let ((ss (buffer-substring-no-properties
                 (region-beginning) (region-end))))
        (setq mark-active nil)
        (cons 'region ss))
    (let ((ss (thing-at-point (or thing 'symbol))))
      (and ss (cons nil (substring-no-properties ss))))))

(defun mark-symbol@ (&optional _)
  "Mark the symbol at point."
  (interactive)
  (let ((bs (bounds-of-thing-at-point 'symbol)))
    (unless (and bs (car bs) (cdr bs))
      (user-error "%s" "No symbol found"))
    (mark-thing (car bs) (cdr bs))))

;; end of symbol

;;;
;; string
;;;

(defun mark-string@ (&optional n)
  "Mark the partial string at point.\n
If prefix N is positive, mark from point to the end;
If prefix N is negative, mark from point to the begining;
Otherwise mark the whole string."
  (interactive "p")
  (let ((bs (bounds-of-thing-at-point 'string))
        (n1 (if (consp current-prefix-arg) 0 n)))
    (unless bs
      (user-error "%s" "No string found"))
    (mark-thing (if (> n1 0) (point) (1+ (car bs)))
                (if (< n1 0) (point) (1- (cdr bs))))))

(defun kill-string@ (&optional n)
  "Kill the partial string at point.\n
If prefix N is positive, kill from point to the end;
If prefix N is negative, kill from point to the begining;
Otherwise kill the whole string."
  (interactive "p")
  (let ((bs (bounds-of-thing-at-point 'string))
        (n1 (if (consp current-prefix-arg) 0 n)))
    (unless bs
      (user-error "%s" "No string found"))
    (kill-region (if (> n1 0) (point) (1+ (car bs)))
                 (if (< n1 0) (point) (1- (cdr bs))))))

;; end of string

;;;
;; sexp
;;;

(defun sexp@ (&optional n)
  (let* ((n1 (or n 1))
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

(defun whole-sexp@ ()
  (let* ((p (point)) (p1 p)
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

(defun mark-sexp@ (&optional n)
  "Mark the sexp at point.\n
If prefix N is a number, then forward or backward N sexps.
Otherwise, select the whole list."
  (interactive "p")
  (let ((bs (cond ((consp current-prefix-arg) (whole-sexp@))
                  (t (sexp@ n)))))
    (unless (and bs (car bs) (cdr bs)
                 (null (= (car bs) (cdr bs))))
      (user-error "%s" "No sexp found"))
    (mark-thing (car bs) (cdr bs))))

(defun kill-sexp@ (&optional n)
  "Kill the whole sexp at point.\n
If prefix N is a number, killing forward or backward N sexps."
  (interactive "p")
  (let ((bs (cond ((consp current-prefix-arg) (whole-sexp@))
                  (t (sexp@ n)))))
    (unless (and bs (car bs) (cdr bs)
                 (null (= (car bs) (cdr bs))))
      (user-error "%s" "No sexp found"))
    (kill-region (car bs) (cdr bs))))

;; end of sexp

;;;
;; word
;;;

(defun word@ (&optional n)
  (let* ((n1 (or n 1))
         (fp (save-excursion (forward-word n1) (point)))
         (bp (save-excursion (goto-char fp) (forward-word (* -1 n1))
                             (point))))
    (if (>= n1 0)
        (cons bp fp)
      (cons fp bp))))

(defun mark-word@ (&optional n)
  "Mark the word at point.\n
If prefix N is non nil, then forward or backward N words."
  (interactive "p")
  (let ((ws (word@ n)))
    (unless ws
      (user-error "%s" "No word found"))
    (mark-thing (car ws) (cdr ws))))

(defun kill-word@ (&optional n)
  "Kill the whole word at point.\n
If prefix N is non nil, then kill N whole word forward or backward."
  (interactive "p")
  (let ((ws (word@ n)))
    (unless ws
      (user-error "%s" "No whole word found"))
    (kill-region (car ws) (cdr ws))))

;; end of word

;;;
;; line
;;;

(defun line@ (&optional n)
  (let* ((n1 (or n 0))
         (fp (save-excursion (forward-line n1)
                             (if (> n1 0) (end-of-line) (beginning-of-line))
                             (point)))
         (bp (save-excursion (if (> n1 0) (beginning-of-line)
                               (end-of-line))
                             (point))))
    (cons fp bp)))

(defun mark-line@ (&optional n)
  "Mark line at point.\n
If prefix N is non nil, then forward or backward N lines."
  (interactive "P")
  (let ((ls (line@ n)))
    (unless ls
      (user-error "%s" "No line found"))
    (mark-thing (car ls) (cdr ls))))

;; end of

;;;
;; defun
;;;

(defun defun@ (&optional n)
  (let ((n1 (or n 1)))
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

(defun mark-defun@ (&optional n)
  "Mark function at point.\n
If prefix N is non-nil, then forward or backward N functions."
  (interactive "p")
  (let ((bs (defun@ n)))
    (unless (and bs (car bs) (cdr bs))
      (user-error "%s" "No defun found"))
    (mark-thing (cdr bs) (car bs))))

;; end of defun

;;;
;; filename
;;;

(defun mark-filename@ (&optional _)
  "Mark filename at point."
  (interactive)
  (let ((bs (bounds-of-thing-at-point 'filename)))
    (unless (and bs (car bs) (cdr bs))
      (user-error "%s" "No filename found"))
    (mark-thing (car bs) (cdr bs))))

;; end of filename

;;;
;; quoted symmetry
;;;

(defun forward-quoted-symmetry (chr pos rx ls rs)
  (let ((cur pos) (ss (cons chr nil)))
    (catch :br
      (while (< cur rx)
        (let ((l (and (= ls (char-after cur)) chr))
              (r (and (= rs (char-after cur)) chr)))
          (cond ((and r ss (= r (car ss)))
                 (setq ss (cdr ss)))
                (l (setq ss (cons l ss)))))
        (when (null ss) (throw :br cur))
        (setq cur (1+ cur))))))

(defun backward-quoted-symmetry (chr pos lx ls rs)
  (let ((cur pos) (ss (cons chr nil)))
    (catch :br
      (while (> cur lx)
        (let ((l (and (= ls (char-before cur)) chr))
              (r (and (= rs (char-before cur)) chr)))
          (cond ((and l ss (= l (car ss)))
                 (setq ss (cdr ss)))
                (r (setq ss (cons r ss)))))
        (when (null ss) (throw :br cur))
        (setq cur (1- cur))))))

(defun quoted-symmetry@ ()
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
                           (backward-quoted-symmetry c r1 lx (aref ls i) c))))
             (r2 (and l1 (let* ((c (char-before l1))
                                (i (strchr ls c)))
                           (forward-quoted-symmetry c l1 rx c (aref rs i)))))
             (l3 (and r2 (let* ((c (char-after r2))
                                (i (strchr rs c)))
                           (backward-quoted-symmetry c r2 lx (aref ls i) c))))
             (r3 (and l2 (let* ((c (char-before l2))
                                (i (strchr ls c)))
                           (forward-quoted-symmetry c l2 rx c (aref rs i))))))
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

(defun mark-quoted-symmetry@ (&optional boundary)
  "Mark symmetry quoted thing at point.\n
If prefix BOUNDARY is non-nil, then mark the whole quoted thing."
  (interactive "P")
  (let ((bs (quoted-symmetry@)))
    (unless (and bs (car bs) (cdr bs))
      (user-error "%s" "No quoted thing found"))
    (mark-thing (if boundary (car bs) (1+ (car bs)))
                (if boundary (cdr bs) (1- (cdr bs))))))

(defun kill-quoted-symmetry@ (&optional boundary)
  "Kill symmetry quoted thing at point.\n
If prefix BOUNDARY is non-nil, then mark the whole quoted thing."
  (interactive "P")
  (let ((bs (quoted-symmetry@)))
    (unless (and bs (car bs) (cdr bs))
      (user-error "%s" "No quoted thing found"))
    (kill-region (if boundary (car bs) (1+ (car bs)))
                 (if boundary (cdr bs) (1- (cdr bs))))))

;; end of quoted symmetry

;;;
;; quoted asymmetry
;;;

(defun forward-quoted-asymmetry (chr pos rx)
  (let ((cur pos))
    (catch :br
      (while (< cur rx)
        (when (char-equal chr (char-after cur))
          (throw :br cur))
        (setq cur (1+ cur))))))

(defun backward-quoted-asymmetry (chr pos lx)
  (let ((cur pos))
    (catch :br
      (while (> cur lx)
        (when (char-equal chr (char-before cur))
          (throw :br cur))
        (setq cur (1- cur))))))

(defun quoted-asymmetry@ ()
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
      (let* ((l2 (and r1 (backward-quoted-asymmetry
                          (aref
                           ls (strchr rs (char-after r1)))
                          r1 lx)))
             (r2 (and l1 (forward-quoted-asymmetry
                          (aref
                           rs (strchr ls (char-before l1)))
                          l1 rx))))
        (cond ((and l2 r2 (and (= l1 l2) (= r1 r2)))
               (cons (1- l1) (1+ r1)))
              ((and l2 r2 (= r1 r2))
               (cons (1- l1) (1+ r2)))
              (l2 (cons (1- l2) (1+ r1)))
              (r2 (cons (1- l1) (1+ r2))))))))

(defun mark-quoted-asymmetry@ (&optional boundary)
  "Mark asymmetry quoted thing at point.\n
If prefix BOUNDARY is non-nil, then mark the whole quoted thing."
  (interactive "P")
  (let ((bs (quoted-asymmetry@)))
    (unless (and bs (car bs) (cdr bs))
      (user-error "%s" "No quoted thing found"))
    (mark-thing (if boundary (car bs) (1+ (car bs)))
                (if boundary (cdr bs) (1- (cdr bs))))))

(defun kill-quoted-asymmetry@ (&optional boundary)
  "Kill asymmetry quoted thing at point.\n
If prefix BOUNDARY is non-nil, then mark the whole quoted thing."
  (interactive "P")
  (let ((bs (quoted-asymmetry@)))
    (unless (and bs (car bs) (cdr bs))
      (user-error "%s" "No quoted thing found"))
    (kill-region (if boundary (car bs) (1+ (car bs)))
                 (if boundary (cdr bs) (1- (cdr bs))))))

;; end of quoted asymmetry

(provide 'marks)

;; end of marks.el
