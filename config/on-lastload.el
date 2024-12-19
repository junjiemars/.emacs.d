;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-afterload.el
;;;;



;;; `clipboard'

(unless-graphic%
  (declare-function on-clipboard-init! (v-home%> "config/clipboard"))
  (autoload 'on-clipboard-init! (v-home%> "config/clipboard"))
  (make-thread* #'on-clipboard-init!))

;; end of `clipboard'

;;; `dict'

(autoload 'lookup-dict (v-home%> "config/dict") "Lookup dict." t)
(define-key (current-global-map) (kbd% "M-s d") 'lookup-dict)

;; end of `dict'

;;; `mill'

;; Kill
(define-key (current-global-map) (kbd% "C-x M-d") #'kill-word@)
(define-key (current-global-map) (kbd% "C-x M-e") #'kill-sexp@)
(define-key (current-global-map) (kbd% "C-x M-q a") #'kill-quoted-asymmetry@)
(define-key (current-global-map) (kbd% "C-x M-q s") #'kill-quoted-symmetry@)
(define-key (current-global-map) (kbd% "C-x M-s") #'kill-string@)
(define-key% (current-global-map) (kbd "C-x M-l") #'kill-whole-line)
;; Mark
(define-key (current-global-map) (kbd% "C-c C-M-@") #'mark-sexp@)
(define-key (current-global-map) (kbd% "C-c M-@") #'mark-word@)
(define-key (current-global-map) (kbd% "C-c M-f") #'mark-filename@)
(define-key (current-global-map) (kbd% "C-c M-h") #'mark-defun@)
(define-key (current-global-map) (kbd% "C-c M-l") #'mark-line@)
(define-key (current-global-map) (kbd% "C-c M-q a") #'mark-quoted-asymmetry@)
(define-key (current-global-map) (kbd% "C-c M-q s") #'mark-quoted-symmetry@)
(define-key (current-global-map) (kbd% "C-c M-s") #'mark-string@)
(define-key% (current-global-map) (kbd "C-M-@") #'mark-sexp)
(define-key% (current-global-map) (kbd "C-M-SPC") #'mark-sexp)
(define-key% (current-global-map) (kbd "C-M-h") #'mark-defun)
(define-key% (current-global-map) (kbd "M-@") #'mark-word)

;; end of `mill'

;;; `progs'
(declare-function on-progs-init! (v-home%> "config/progs"))
(autoload 'on-progs-init! (v-home%> "config/progs"))
(make-thread* #'on-progs-init!)

;; end of `progs'

;;; `trans'

(autoload 'ascii-table (v-home%> "config/trans") nil t)
(autoload 'chinese->arabic (v-home%> "config/trans") nil t)
(autoload 'decode-base64 (v-home%> "config/trans") nil t)
(autoload 'decode-chinese-number (v-home%> "config/trans") nil t)
(autoload 'decode-ipv4 (v-home%> "config/trans") nil t)
(autoload 'decode-roman-number (v-home%> "config/trans") nil t)
(autoload 'decode-url (v-home%> "config/trans") nil t)
(autoload 'encode-base64 (v-home%> "config/trans") nil t)
(autoload 'encode-ipv4 (v-home%> "config/trans") nil t)
(autoload 'encode-url (v-home%> "config/trans") nil t)
(autoload 'greek-alphabet (v-home%> "config/trans") nil t)
(autoload 'int->ipv4 (v-home%> "config/trans") nil t)
(autoload 'ipv4->int (v-home%> "config/trans") nil t)
(autoload 'roman->arabic (v-home%> "config/trans") nil t)

;; end of `trans'

;; end of on-afterload.el
