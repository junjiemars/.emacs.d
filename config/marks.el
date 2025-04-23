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
;;; 1. `xxx@*' the string of thing
;;; 2. `xxx@' the bounds of thing.
;;; 3. `mark-xxx@' mark the thing
;;;;


;;; require

(eval-when-compile
  (require 'thingatpt)
  (when-feature-treesit%
    (declare-function treesit-language-at "treesit")
    (declare-function treesit-beginning-of-defun "treesit")
    (declare-function treesit-end-of-defun "treesit")))

 ;; end of require

;;;
;; env
;;;

(defun mark-thing (beg end)
  "Mark thing at (BEG, END)."
  (goto-char beg)
  (set-mark (point))
  (goto-char end))

(defun symbol@* (&optional thing)
  "Return the string of THING at point."
  (if-region-active
      (let ((ss (buffer-substring-no-properties
                 (region-beginning) (region-end))))
        (prog1 ss
          (setq mark-active nil)))
    (let ((ss (thing-at-point (or thing 'symbol))))
      (and ss (substring-no-properties ss)))))

;; end of env

;;;
;; symbol
;;;

(defun mark-symbol@ (&optional _)
  "Mark the symbol at point."
  (interactive)
  (let ((bs (bounds-of-thing-at-point 'symbol)))
    (unless bs
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
                   (forward-sexp (- n1))
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
    (unless (and bs (car bs) (cdr bs) (null (= (car bs) (cdr bs))))
      (user-error "%s" "No sexp found"))
    (mark-thing (car bs) (cdr bs))))

(defun kill-sexp@ (&optional n)
  "Kill the whole sexp at point.\n
If prefix N is a number, killing forward or backward N sexps."
  (interactive "p")
  (let ((bs (cond ((consp current-prefix-arg) (whole-sexp@))
                  (t (sexp@ n)))))
    (unless (and bs (car bs) (cdr bs) (null (= (car bs) (cdr bs))))
      (user-error "%s" "No sexp found"))
    (kill-region (car bs) (cdr bs))))

;; end of sexp

;;;
;; word
;;;

(defun word@ (&optional n)
  (let* ((n1 (or n 1))
         (fp (save-excursion (forward-word n1) (point)))
         (bp (save-excursion (goto-char fp) (forward-word (- n1))
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

(when-feature-treesit%
  (defun defun-ts@ (&optional n)
    (let* ((n1 (or n 1))
           (fp (save-excursion
                 (if (> n1 0)
                     (treesit-end-of-defun n1)
                   (treesit-beginning-of-defun (- n1)))
                 (point)))
           (bp (save-excursion
                 (goto-char fp)
                 (if (> n1 0)
                     (treesit-beginning-of-defun n1)
                   (treesit-end-of-defun (- n1)))
                 (point))))
      (if (>= n1 0)
          (cons bp fp)
        (cons fp bp)))))


(defun mark-defun@ (&optional n)
  "Mark function at point.\n
If prefix N is non-nil, then forward or backward N functions."
  (interactive "p")
  (let ((bs (if-feature-treesit%
                (if (and (fboundp 'treesit-language-at)
                         (treesit-language-at (point)))
                    (defun-ts@ n)
                  (defun@ n))
              (defun@ n))))
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

(provide 'marks)

;; end of marks.el
