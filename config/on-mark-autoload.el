;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-mark-autoload.el
;;;;

(eval-when-compile
  (require 'marks))


;;; mark/kill-symbol

(defun mark-symbol@ (&optional n)
  "Mark symbol at point.

If prefix N is non-nil, then select the Nth symbol."
  (interactive "p")
  (let ((bs (_mark_symbol@_ n)))
    (unless (and bs (car bs) (cdr bs))
      (user-error* "No symbol found"))
    (_mark_thing_ (car bs) (cdr bs))))


(defun kill-whole-symbol (&optional n)
  "Kill current symbol.

With prefix N, do it N times forward if positive, or move
backwards N times if negative."
  (interactive "p")
  (let ((bs (_mark_symbol@_ n)))
    (unless (and bs (car bs) (cdr bs))
      (user-error* "No whole symbol found"))
    (kill-region (car bs) (cdr bs))))

;; end of mark/kill-symbol


;;; mark/kill-sexp

(defun mark-sexp@ (&optional n)
  "Mark sexp at point.

If prefix N is non nil, then forward or backward N sexps.
Otherwise, select the whole list."
  (interactive "p")
  (let ((bs (if (consp current-prefix-arg)
                (_mark_whole_sexp@_)
              (_mark_sexp@_ n))))
    (unless (and bs (car bs) (cdr bs))
      (user-error* "No sexp found"))
    (_mark_thing_ (car bs) (cdr bs))))


(defun kill-whole-sexp (&optional boundary)
  "Kill current sexp.

With prefix BOUNDARY, killing include BOUNDARY otherwise do not."
  (interactive "P")
  (let ((bs (_mark_whole_sexp@_ boundary)))
    (unless (and bs (car bs) (cdr bs))
      (user-error* "No whole sexp found"))
    (kill-region (car bs) (cdr bs))))

;; end of mark/kill-sexp


;;; mark/kill-word

(defun mark-word@ (&optional n)
  "Mark the word at point.

If prefix N is non nil, then forward or backward N words."
  (interactive "p")
  (let ((bs (_mark_word@_ n)))
    (unless (and bs (car bs) (cdr bs))
      (user-error* "No word found"))
    (_mark_thing_ (car bs) (cdr bs))))


(defun kill-whole-word (&optional n)
  "Kill current word.

With prefix N, do it N times forward if positive, or move
backwards N times if negative."
  (interactive "p")
  (let ((bs (_mark_word@_ n)))
    (unless (and bs (car bs) (cdr bs))
      (user-error* "No whole word found"))
    (kill-region (car bs) (cdr bs))))

;; end of mark/kill-word


(defun mark-line@ (&optional indent)
  "Mark line at point.

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


;;; mark-defun

(defun mark-defun@ (&optional n)
  "Mark function at point.

If prefix N is non-nil, then forward or backward N functions."
  (interactive "p")
  (let ((bs (_mark_defun@_ n)))
    (unless (and bs (car bs) (cdr bs))
      (user-error* "No defun found"))
    (_mark_thing_ (cdr bs) (car bs))))

;; end of mark-defun


;;; mark-filename

(defun mark-filename@ ()
  "Mark filename at point."
  (interactive)
  (let ((bs (_mark_filename@_)))
    (unless (and bs (car bs) (cdr bs))
      (user-error* "No filename found"))
    (_mark_thing_ (car bs) (cdr bs))))

;; end of mark-filename


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
  (let ((bs (_mark_quoted@_ enclose quoted)))
    (unless (and bs (car bs) (cdr bs))
      (user-error* "No quoted found"))
    (_mark_thing_ (- (car bs) enclose) (+ (cdr bs) enclose))))

;; end of mark-quoted


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

;; end of Keys


;; end of on-mark-autoload.el
