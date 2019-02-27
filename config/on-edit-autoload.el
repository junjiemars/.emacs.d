;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-edit-autoload.el
;;;;


(terminal-supported-p
  ;;above version 23 transient-mark-mode is enabled by default
  (version-supported-when > 23 (transient-mark-mode t))
  (set-face-background 'region "white")
  (set-face-foreground 'region "black"))

;; No cursor blinking, it's distracting
(when-fn% 'blink-cursor-mode nil (blink-cursor-mode 0))

;; full path in title bar
(graphic-supported-p
  (setq% frame-title-format "%b (%f)"))

;; Ignore ring bell
(setq% ring-bell-function 'ignore)


;; Changes all yes/no questions to y/n type
;; (defalias 'yes-or-no-p 'y-or-n-p)

;; Highlights matching parenthesis
(show-paren-mode 1)


;; enable save minibuffer history
(version-supported-if
    <= 24
    (savehist-mode)
  (savehist-mode t))

;; enable save-place
(version-supported-if
    <= 25.1
    (save-place-mode t)
  (setq% save-place t 'saveplace))

;; Shows all options when running apropos. For more info,
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
;;enable apropos-do-all, but slower
(setq% apropos-do-all t 'apropos)


;; Makes killing/yanking interact with the clipboard

;; Save clipboard strings into kill ring before replacing them.
;; When one selects something in another program to paste it into Emacs,
;; but kills something in Emacs before actually pasting it,
;; this selection is gone unless this variable is non-nil
;; I'm actually not sure what this does but it's recommended?
;; http://emacswiki.org/emacs/CopyAndPaste
(version-supported-if
    <= 24.1
    (setq% select-enable-clipboard t)
  (setq% x-select-enable-clipboard t))
(version-supported-if
    <= 25.1
    (setq% select-enable-primary t 'select)
  (setq% x-select-enable-primary t 'select))

;; Save before kill
(setq% save-interprogram-paste-before-kill t 'simple)

;; Mouse yank commands yank at point instead of at click.
(setq% mouse-yank-at-point t 'mouse)


;; no need for ~ files when editing
(setq% create-lockfiles nil)


;; Greek letters C-x 8 <RET> greek small letter lambda
;; (global-set-key (kbd "C-c l") "λ")


;; enable upcase/downcase region
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)


;; enable column number mode
(setq% column-number-mode t 'simple)


;; comments
(defun toggle-comment ()
  "Toggle comment on current line or region."
  (interactive)
  (comment-or-uncomment-region
   (region-active-if (region-beginning) (line-beginning-position))
   (region-active-if (region-end) (line-end-position)))
  (if-fn% 'next-logical-line nil
          (next-logical-line)
    (next-line)))

;; toggle comment key strike1
(define-key (current-global-map) (kbd "C-c ;") #'toggle-comment)

;; auto org-mode
(version-supported-when >= 23
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode)))


;; :edit
(when (self-spec->*env-spec :edit :allowed)
  ;; default tab-width
  (setq-default tab-width (self-spec->*env-spec :edit :tab-width))
  ;; default auto-save-default
  (setq auto-save-default (self-spec->*env-spec :edit :auto-save-default)))


;; find-tag and pop-tag-mark
;; same as Emacs22+
(unless-fn% 'xref-find-definitions 'xref
  (when-fn% 'pop-tag-mark 'etags
    (with-eval-after-load 'etags
      ;; define keys for `pop-tag-mark' and `tags-loop-continue'
      (define-key% (current-global-map) (kbd "M-,") #'pop-tag-mark)
      (define-key% (current-global-map) (kbd "M-*") #'tags-loop-continue))))


(version-supported-when > 24.4
  ;; fix: no quit key to hide *Messages* buffer
  ;; [DEL] for `scroll-down'
  ;; [SPC] for `scroll-up'
  (with-current-buffer (get-buffer "*Messages*")
    (if-fn% 'read-only-mode nil
            (read-only-mode 1)
      (toggle-read-only t))
    (local-set-key (kbd "q") #'quit-window)
    (local-set-key (kbd "DEL") #'scroll-down)
    (local-set-key (kbd "SPC") #'scroll-up)))


(version-supported-when > 24
  ;; fix: `uniquify' may not be autoloaded on ancient Emacs.
  (when% (let ((x byte-compile-warnings))
           (setq byte-compile-warnings nil)
           (prog1 (require 'uniquify nil t)
             (setq byte-compile-warnings x)))
    (require 'uniquify)
    (setq uniquify-buffer-name-style 'post-forward-angle-brackets)))


(defmacro pprint (form)
  "Insert a pretty-printed rendition of a Lisp FORM in current buffer."
  `(cl-prettyprint ,form))
(autoload 'cl-prettyprint "cl-extra")


;; abbreviated `eshell' prompt
(version-supported-when > 23
  (when% (and (require 'em-prompt) (require 'em-dirs))
    (setq eshell-prompt-function
          #'(lambda ()
              (concat (abbreviate-file-name (eshell/pwd))
                      (if (= (user-uid) 0) " # " " $ "))))))


;; Mark thing at point

(defmacro mark-thing@ (begin end)
  "Mark THING at point."
  `(progn
     ,begin
     (set-mark (point))
     ,end
     (mark)))

(defun mark-symbol@ ()
  "Mark symbol at point."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (mark-thing@ (goto-char (car bounds))
                   (goto-char (cdr bounds))))))

(defun mark-filename@ ()
  "Mark filename at point."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'filename)))
    (when bounds
      (mark-thing@ (goto-char (car bounds))
                   (goto-char (cdr bounds))))))

(defun mark-line@ (&optional arg)
  "Mark line at point.
If ARG is non-nil should mark the whole line."
  (interactive "P")
  (mark-thing@ (if arg
                   (beginning-of-line)
                 (back-to-indentation))
               (end-of-line)))

(defun mark-list@ ()
  "Mark list at point.
More accurate than `mark-sexp'."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'list)))
    (when bounds
      (mark-thing@ (goto-char (car bounds))
                   (goto-char (cdr bounds))))))

(defun mark-defun@ ()
  "Mark function at point.
More accurate than `mark-defun'."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'defun)))
    (when bounds
      (mark-thing@ (goto-char (car bounds))
                   (goto-char (cdr bounds))))))

(unless-fn% 'thing-at-point-bounds-of-list-at-point 'thingatpt
  ;; fix the wrong behavior of (bounds-of-thing-at-point 'list) on
  ;; ancient Emacs.
  (defun thing-at-point-bounds-of-list-at-point ()
    "Return the bounds of the list at point.
 [Internal function used by `bounds-of-thing-at-point'.]"
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
  ;; fix wrong behavior (bounds-of-thing-at-point 'defun) on ancient
  ;; Emacs.
  (put 'defun 'beginning-op 'beginning-of-defun)
  (put 'defun 'end-op       'end-of-defun)
  (put 'defun 'forward-op   'end-of-defun))


(define-key (current-global-map) (kbd "C-c m s") #'mark-symbol@)
(define-key (current-global-map) (kbd "C-c m f") #'mark-filename@)
(define-key (current-global-map) (kbd "C-c m l") #'mark-line@)
(define-key (current-global-map) (kbd "C-c m a") #'mark-list@)
(define-key (current-global-map) (kbd "C-c m d") #'mark-defun@)


;; system clipboard
(terminal-supported-p
  (platform-supported-unless 'windows-nt
    
    (defmacro defun-x-select-text* (bin &rest args)
      "Define `x-select-text*'"
      `(defun x-select-text* (text &optional _unused_)
         "Copy TEXT to system clipboard."
         (with-temp-buffer
           (insert text)
           (call-process-region (point-min) (point-max)
                                ,bin
                                nil 0 nil
                                ,@args))))

    (defmacro defun-x-selection-value* (bin &rest args)
      "Define x-selection-value*'"
      `(defun x-selection-value* ()
         "Paste from system clipboard."
         (let ((out (shell-command* ,bin ,@args)))
           (when (zerop (car out))
             (cdr out)))))

    (defmacro enable-x-select-clipboard! ()
      "Enable `x-select-clipboard'"
      `(progn
         (setq interprogram-cut-function #'x-select-text*)
         (setq interprogram-paste-function #'x-selection-value*))))
  
  (platform-supported-when 'darwin
    (defun-x-select-text* "pbcopy")
    (defun-x-selection-value* "pbpaste")
    (enable-x-select-clipboard!))

  (when% (executable-find% "xsel")
    (platform-supported-when 'gnu/linux
      (enable-x-select-clipboard!))))


;; end of file
