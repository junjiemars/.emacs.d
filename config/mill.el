;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; mill.el
;;;;
;; Commentary: mark and kill keys
;;;;

;;; require

(eval-when-compile
  (require 'marks (v-home%> "config/marks")))

;; end of require

;;; `mark-symbol@' `kill-symbol@'

(defun mark-symbol@ (&optional _)
  "Mark the symbol at point."
  (interactive)
  (let ((bs (_mark_symbol@_)))
    (unless (and bs (car bs) (cdr bs))
      (user-error "%s" "No symbol found"))
    (_mark_thing_ (car bs) (cdr bs))))


;; end of `mark-symbol@' `kill-symbol@'

;;; `mark-string@'

(defun mark-string@ (&optional n)
  "Mark the partial string at point.\n
If prefix N is positive, mark from point to the end;
If prefix N is negative, mark from point to the begining;
Otherwise mark the whole string."
  (interactive "p")
  (let ((bs (_mark_string@_))
        (n1 (if (consp current-prefix-arg) 0 n)))
    (unless bs
      (user-error "%s" "No string found"))
    (_mark_thing_ (if (> n1 0) (point) (1+ (car bs)))
                  (if (< n1 0) (point) (1- (cdr bs))))))

(defun kill-string@ (&optional n)
  "Kill the partial string at point.\n
If prefix N is positive, kill from point to the end;
If prefix N is negative, kill from point to the begining;
Otherwise kill the whole string."
  (interactive "p")
  (let ((bs (_mark_string@_))
        (n1 (if (consp current-prefix-arg) 0 n)))
    (unless bs
      (user-error "%s" "No string found"))
    (kill-region (if (> n1 0) (point) (1+ (car bs)))
                 (if (< n1 0) (point) (1- (cdr bs))))))

;; end of `mark-string@'


;;; `mark-sexp@' `kill-sexp@'

(defun mark-sexp@ (&optional n)
  "Mark the sexp at point.\n
If prefix N is a number, then forward or backward N sexps.
Otherwise, select the whole list."
  (interactive "p")
  (let ((bs (cond ((consp current-prefix-arg) (_mark_whole_sexp@_))
                  (t (_mark_sexp@_ n)))))
    (unless (and bs (car bs) (cdr bs)
                 (null (= (car bs) (cdr bs))))
      (user-error "%s" "No sexp found"))
    (_mark_thing_ (car bs) (cdr bs))))

(defun kill-sexp@ (&optional n)
  "Kill the whole sexp at point.\n
If prefix N is a number, killing forward or backward N sexps."
  (interactive "p")
  (let ((bs (cond ((consp current-prefix-arg) (_mark_whole_sexp@_))
                  (t (_mark_sexp@_ n)))))
    (unless (and bs (car bs) (cdr bs)
                 (null (= (car bs) (cdr bs))))
      (user-error "%s" "No sexp found"))
    (kill-region (car bs) (cdr bs))))

;; end of `mark-sexp@' `kill-sexp@'


;;; `mark-word' `kill-word@'

(defun mark-word@ (&optional n)
  "Mark the word at point.\n
If prefix N is non nil, then forward or backward N words."
  (interactive "p")
  (let ((ws (_mark_word@_ n)))
    (unless ws
      (user-error "%s" "No word found"))
    (_mark_thing_ (car ws) (cdr ws))))

(defun kill-word@ (&optional n)
  "Kill the whole word at point.\n
If prefix N is non nil, then kill N whole word forward or backward."
  (interactive "p")
  (let ((ws (_mark_word@_ n)))
    (unless ws
      (user-error "%s" "No whole word found"))
    (kill-region (car ws) (cdr ws))))


;; end of `mark-word' `kill-word@'


;;; `mark-line@'

(defun mark-line@ (&optional n)
  "Mark line at point.\n
If prefix N is non nil, then forward or backward N lines."
  (interactive "P")
  (let ((ls (_mark_line@_ n)))
    (unless ls
      (user-error "%s" "No line found"))
    (_mark_thing_ (car ls) (cdr ls))))

;; end of `mark-line@'


;;; `mark-defun@'

(defun mark-defun@ (&optional n)
  "Mark function at point.\n
If prefix N is non-nil, then forward or backward N functions."
  (interactive "p")
  (let ((bs (_mark_defun@_ n)))
    (unless (and bs (car bs) (cdr bs))
      (user-error "%s" "No defun found"))
    (_mark_thing_ (cdr bs) (car bs))))

;; end of `mark-defun@'


;;; `mark-filename@'

(defun mark-filename@ (&optional _)
  "Mark filename at point."
  (interactive)
  (let ((bs (_mark_filename@_)))
    (unless (and bs (car bs) (cdr bs))
      (user-error "%s" "No filename found"))
    (_mark_thing_ (car bs) (cdr bs))))

;; end of `mark-filename@'

;;; `mark-quoted-symmetry@' `kill-quoted-symmetry@'

(defun mark-quoted-symmetry@ (&optional boundary)
  "Mark symmetry quoted thing at point.\n
If prefix BOUNDARY is non-nil, then mark the whole quoted thing."
  (interactive "P")
  (let ((bs (_mark_quoted_symmetry@_)))
    (unless (and bs (car bs) (cdr bs))
      (user-error "%s" "No quoted thing found"))
    (_mark_thing_ (if boundary (car bs) (1+ (car bs)))
                  (if boundary (cdr bs) (1- (cdr bs))))))

(defun kill-quoted-symmetry@ (&optional boundary)
  "Kill symmetry quoted thing at point.\n
If prefix BOUNDARY is non-nil, then mark the whole quoted thing."
  (interactive "P")
  (let ((bs (_mark_quoted_symmetry@_)))
    (unless (and bs (car bs) (cdr bs))
      (user-error "%s" "No quoted thing found"))
    (kill-region (if boundary (car bs) (1+ (car bs)))
                 (if boundary (cdr bs) (1- (cdr bs))))))

;; end of `mark-quoted-symmetry@'


;;; `mark-quoted-asymmetry@' `kill-quoted-asymmetry@'

(defun mark-quoted-asymmetry@ (&optional boundary)
  "Mark asymmetry quoted thing at point.\n
If prefix BOUNDARY is non-nil, then mark the whole quoted thing."
  (interactive "P")
  (let ((bs (_mark_quoted_asymmetry@_)))
    (unless (and bs (car bs) (cdr bs))
      (user-error "%s" "No quoted thing found"))
    (_mark_thing_ (if boundary (car bs) (1+ (car bs)))
                  (if boundary (cdr bs) (1- (cdr bs))))))

(defun kill-quoted-asymmetry@ (&optional boundary)
  "Kill asymmetry quoted thing at point.\n
If prefix BOUNDARY is non-nil, then mark the whole quoted thing."
  (interactive "P")
  (let ((bs (_mark_quoted_asymmetry@_)))
    (unless (and bs (car bs) (cdr bs))
      (user-error "%s" "No quoted thing found"))
    (kill-region (if boundary (car bs) (1+ (car bs)))
                 (if boundary (cdr bs) (1- (cdr bs))))))

;; end of `mark-quoted-asymmetry@'

(provide 'mill)

;; end of mill.el
