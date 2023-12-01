;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-mark-autoload.el
;;;;

;; (eval-when-compile (require 'marks))


;;; `mark-symbol@' `kill-symbol@'

(defun mark-symbol@ ()
  "Mark the symbol at point."
  (interactive)
  (let ((bs (_mark_symbol@_)))
    (unless (and bs (car bs) (cdr bs))
      (user-error "No symbol found"))
    (_mark_thing_ (car bs) (cdr bs))))


;; end of `mark-symbol@' `kill-symbol@'

;;; `mark-sexp@' `kill-sexp@'

(defun mark-sexp@ (&optional n)
  "Mark the sexp at point.\n
If prefix N is a number, then forward or backward N sexps.
Otherwise, select the whole list."
  (interactive "p")
  (let ((bs (_mark_sexp@_ n)))
    (unless bs
      (user-error "No sexp found"))
    (_mark_thing_ (car bs) (cdr bs))))

(defun kill-sexp@ (&optional n)
  "Kill the whole sexp at point.\n
If prefix N is a number, killing forward or backward N sexps."
  (interactive "P")
  (let ((bs (_mark_sexp@_ n)))
    (unless bs
      (user-error "No whole sexp found"))
    (kill-region (car bs) (cdr bs))))

;; end of `mark-sexp@' `kill-sexp@'


;;; `mark-word' `kill-word@'

(defun mark-word@ (&optional n)
  "Mark the word at point.\n
If prefix N is non nil, then forward or backward N words."
  (interactive "p")
  (let ((ws (_mark_word@_ n)))
    (unless ws
      (user-error "No word found"))
    (_mark_thing_ (car ws) (cdr ws))))

(defun kill-word@ (&optional n)
  "Kill the whole word at point.\n
If prefix N is non nil, then kill N whole word forward or
backward."
  (interactive "p")
  (let ((ws (_mark_word@_ n)))
    (unless ws
      (user-error "No whole word found"))
    (kill-region (car ws) (cdr ws))))


;; end of `mark-word' `kill-word@'

;;; `mark-line@'

(defun mark-line@ (&optional indent)
  "Mark line at point.\n
If prefix INDENT is non-nil, then mark indent line."
  (interactive "P")
  (_mark_thing_ (if indent
                    (save-excursion
                      (back-to-indentation)
                      (point))
                  (save-excursion
                    (beginning-of-line)
                    (point)))
                (save-excursion
                  (end-of-line)
                  (point))))

;; end of `mark-line@'


;;; `mark-defun@'

(defun mark-defun@ (&optional n)
  "Mark function at point.\n
If prefix N is non-nil, then forward or backward N functions."
  (interactive "p")
  (let ((bs (_mark_defun@_ n)))
    (unless (and bs (car bs) (cdr bs))
      (user-error "No defun found"))
    (_mark_thing_ (cdr bs) (car bs))))

;; end of `mark-defun@'


;;; `mark-filename@'

(defun mark-filename@ ()
  "Mark filename at point."
  (interactive)
  (let ((bs (_mark_filename@_)))
    (unless (and bs (car bs) (cdr bs))
      (user-error "No filename found"))
    (_mark_thing_ (car bs) (cdr bs))))

;; end of `mark-filename@'

;;; `mark-quoted@'

(defun mark-quoted@ (&optional boundary)
  "Mark QUOTE thing at point.\n
If prefix BOUNDARY is non-nil, then mark the whole quoted thing."
  (interactive "P")
  (let ((bs (_mark_quoted@_)))
    (unless (and bs (car bs) (cdr bs))
      (user-error "No quoted thing found"))
    (_mark_thing_ (if boundary (car bs) (1+ (car bs)))
                  (if boundary (cdr bs) (1- (cdr bs))))))

;; end of `mark-quoted@'


;;; Keys

;; Kill
(define-key% (current-global-map) (kbd "C-x M-d") #'kill-word@)
(define-key% (current-global-map) (kbd "C-x M-e") #'kill-sexp@)
(define-key% (current-global-map) (kbd "C-x M-l") #'kill-whole-line)

;; Mark
(define-key% (current-global-map) (kbd "C-c m f") #'mark-filename@)
(define-key% (current-global-map) (kbd "C-c m l") #'mark-line@)
(define-key% (current-global-map) (kbd "C-c m q") #'mark-quoted@)
(define-key% (current-global-map) (kbd "M-@") #'mark-word@)
(define-key% (current-global-map) (kbd "M-SPC") #'mark-word)
(define-key% (current-global-map) (kbd "C-M-@") #'mark-sexp@)
(define-key% (current-global-map) (kbd "C-M-SPC") #'mark-sexp)
(define-key% (current-global-map) (kbd "C-M-h") #'mark-defun@)

;; end of Keys


;; end of on-mark-autoload.el
