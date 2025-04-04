;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-afterload.el
;;;;



;;; `chez'

;; (*org-babel-schemes* :put 'chez "scheme")
(autoload 'chez-mode (v-home%> "config/chez") "Toggle chez mode." t)
(autoload 'run-chez (v-home%> "config/chez") "Toggle chez process." t)

;; end of `chez'

;;; `clipboard'

(unless-graphic%
  (declare-function on-clipboard-init! (v-home%> "config/clipboard"))
  (autoload 'on-clipboard-init! (v-home%> "config/clipboard"))
  (make-thread* #'on-clipboard-init!))

;; end of `clipboard'

;;; `cscope'

(autoload 'cscope (v-home%> "config/cscope") "Run cscope process." t)
(autoload 'run-cscope (v-home%> "config/cscope") "Run cscope REPL process." t)

;; end of `cscope'

;;; `dict'

(autoload 'lookup-dict (v-home%> "config/dict") "Lookup dict." t)
(define-key (current-global-map) (kbd% "M-s d") 'lookup-dict)

;; end of `dict'

;;; `gud-cdb'
(when-platform% windows-nt
  (autoload 'gud-cdb (v-home%> "config/gud-cdb") "Run cdb." t))

;; end of `gud-cdb'

;;; `gambit'
;; (autoload 'gambit-mode (v-home%> "config/gambit") "Toggle gambit mode." t)
;; end of `gambit'

;;; `gud-lldb'

(autoload 'gud-lldb (v-home%> "config/gud-lldb") "Run lldb." t)

;; end of `gud-lldb'

;;; `isearchs'
(autoload 'isearch*-forward (v-home%> "config/isearchs"))
(autoload 'isearch*-backward (v-home%> "config/isearchs"))
(autoload 'isearch*-forward-symbol (v-home%> "config/isearchs"))
(define-global-key% "" #'isearch*-forward)
(define-global-key% "" #'isearch*-backward)
(define-global-key% (kbd "M-s .") #'isearch*-forward-symbol)
;; end of `isearchs'

;;; `jshell'

(autoload 'jshell-mode (v-home%> "config/jshell") "Toggle jshell mode." t)
(autoload 'run-jshell (v-home%> "config/jshell") "Toggle jshell process." t)
(push! `("\\.jsh\\'" . java-mode) auto-mode-alist)

;; end of `jshell'

;;; `marks'

;; Kill
(define-key (current-global-map) (kbd% "C-x M-d") #'kill-word@)
(define-key (current-global-map) (kbd% "C-x M-e") #'kill-sexp@)
(define-key (current-global-map) (kbd% "C-x M-s") #'kill-string@)
(define-global-key% (kbd "C-x M-l") #'kill-whole-line)

;; Mark
(define-key (current-global-map) (kbd% "C-c C-M-@") #'mark-sexp@)
(define-key (current-global-map) (kbd% "C-c M-@") #'mark-word@)
(define-key (current-global-map) (kbd% "C-c M-f") #'mark-filename@)
(define-key (current-global-map) (kbd% "C-c M-h") #'mark-defun@)
(define-key (current-global-map) (kbd% "C-c M-l") #'mark-line@)
(define-key (current-global-map) (kbd% "C-c M-s") #'mark-string@)
(define-global-key% (kbd "C-M-@") #'mark-sexp)
(define-global-key% (kbd "C-M-SPC") #'mark-sexp)
(define-global-key% (kbd "C-M-h") #'mark-defun)
(define-global-key% (kbd "M-@") #'mark-word)

;; end of `marks'

;;; `mixval'

(autoload 'mixvm (v-home%> "config/mixvm") "Run mixvm." t)

;; end of `mixval'

;;; `node'

(autoload 'node-mode (v-home%> "config/node") "Toggle node mode." t)
(autoload 'run-node (v-home%> "config/node") "Toggle node process." t)

;; end of `node'

;;; `progs'
(declare-function on-progs-init! (v-home%> "config/progs"))
(autoload 'on-progs-init! (v-home%> "config/progs"))
(make-thread* #'on-progs-init!)

;; end of `progs'

;;; `scratch'

(autoload 'scratch (v-home%> "config/scratch") "Scratch" t)

;; end of `scratch'

;;; `sudoku'

(autoload 'sudoku (v-home%> "config/sudoku") "Play sudoku." t)

;; end of `sudoku'

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
