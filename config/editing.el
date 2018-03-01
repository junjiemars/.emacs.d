;;
;; Editing buffer that related to configurations.
;;



;; linum mode
(defmacro linum-mode-supported-p (body)
  "When `emacs-version' supports linum mode then do BODY."
  `(version-supported-when <= 23.1 ,body))


;; Toggle linum mode 
(linum-mode-supported-p
 (defun toggle-linum-mode ()
   "Toggle linum-mode."
   (interactive)
   (defvar linum-mode)
   (if (or (not (boundp 'linum-mode))
           (null linum-mode))
       (linum-mode t)
     (linum-mode -1))))






;; Shows all options when running apropos. For more info,
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
;;apropos-do-all t
(safe-setq apropos-do-all t)

;; Makes killing/yanking interact with the clipboard

;; Save clipboard strings into kill ring before replacing them.
;; When one selects something in another program to paste it into Emacs,
;; but kills something in Emacs before actually pasting it,
;; this selection is gone unless this variable is non-nil
;; I'm actually not sure what this does but it's recommended?
;; http://emacswiki.org/emacs/CopyAndPaste
(version-supported-if
    <= 25
    (progn
      (safe-setq select-enable-clipboard t)
      (safe-setq select-enable-primary t))
  (safe-setq x-select-enable-clipboard t)
  (safe-setq x-select-enable-primary t))


;; Save before kill
(safe-setq save-interprogram-paste-before-kill t)

;; Mouse yank commands yank at point instead of at click.
(safe-setq mouse-yank-at-point t)


;; Key binding to use "hippie expand" for text autocompletion
;; http://www.emacswiki.org/emacs/HippieExpand
(global-set-key (kbd "M-/") 'hippie-expand)


;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))


;; comments
(defun toggle-comment ()
  "Comment or uncomment current line"
  (interactive)
  (let (begin end)
    (safe-fn-if region-active-p
        (if (region-active-p)
            (setq begin (region-beginning)
                  end (region-end))
          (setq begin (line-beginning-position)
                end (line-end-position)))
      (if mark-active
          (setq begin (region-beginning)
                end (region-end))
        (setq begin (line-beginning-position)
              end (line-end-position))))
    (comment-or-uncomment-region begin end)
    (safe-fn-if next-logical-line
        (next-logical-line)
      (next-line))))



;; fix weird os x kill error
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (safe-fn-when ns-get-selection-internal
    (condition-case nil
        (ns-get-selection-internal 'CLIPBOARD)
      (quit nil))))


;; No need for ~ files when editing
(safe-setq create-lockfiles nil)


;; Don't use hard tabs
(setq-default indent-tabs-mode nil)


;; Disable electric indent mode 
(safe-setq electric-indent-mode nil)


;; Enable column number mode
(safe-setq column-number-mode t)


;; Default tab-width
(setq-default tab-width 2)


;; shell scripts
(defun set-sh-mode! ()
  (let ((w tab-width))
    (defvar sh-basic-offset)
    (defvar sh-indentation)
    (setq sh-basic-offset w)
    (setq sh-indentation w)))

(add-hook 'sh-mode-hook #'set-sh-mode!)





;; Greek letters C-x 8 <RET> greek small letter lambda
;; (global-set-key (kbd "C-c l") "λ")


;; Rmail file
(setq rmail-file-name (v-home! ".mail/" "RMAIL"))


;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(progn
  (require 'saveplace)
  (setq-default save-place t)
  (setq-default save-place-file (v-home! ".places/" "places")))



;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(v-home! ".backup/"))))



;; Bookmarks
(setq-default eww-bookmarks-directory (v-home! ".bookmarks/"))
(setq-default bookmark-default-file (v-home! ".bookmarks/" "emacs.bmk"))



;; Enable save minibuffer history
(version-supported-if
    <= 24
    (savehist-mode)
  (savehist-mode t))


;; Enable upcase/downcase region
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

