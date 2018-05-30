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
(when-fn% blink-cursor-mode nil (blink-cursor-mode 0))

;; full path in title bar
(graphic-supported-p
	(setq% frame-title-format "%b (%f)"))

;; Ignore ring bell
(setq% ring-bell-function 'ignore)


;; Changes all yes/no questions to y/n type
(defalias 'yes-or-no-p 'y-or-n-p)

;; Highlights matching parenthesis
(show-paren-mode 1)

;; ido-mode allows you to more easily navigate choices. For example,
;; when you want to switch buffers, ido presents you with a list
;; of buffers in the the mini-buffer. As you start to type a buffer's
;; name, ido will narrow down the list of buffers to match the text
;; you've typed in
;; http://www.emacswiki.org/emacs/InteractivelyDoThings
(ido-mode t)

;; enable save minibuffer history
(version-supported-if
		<= 24
		(savehist-mode)
	(savehist-mode t))

;; enable save-place
(version-supported-if
		<= 25.1
		(save-place-mode t)
	(setq% save-place t saveplace))

;; Shows all options when running apropos. For more info,
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
;;enable apropos-do-all, but slower
(setq% apropos-do-all t apropos)


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
		(setq% select-enable-primary t select)
	(setq% x-select-enable-primary t select))

;; Save before kill
(setq% save-interprogram-paste-before-kill t simple)

;; Mouse yank commands yank at point instead of at click.
(setq% mouse-yank-at-point t mouse)


;; no need for ~ files when editing
(setq% create-lockfiles nil)


;; Greek letters C-x 8 <RET> greek small letter lambda
;; (global-set-key (kbd "C-c l") "λ")


;; enable upcase/downcase region
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)


;; enable column number mode
(setq% column-number-mode t simple)


;; :edit
(when (self-spec->*env-spec :edit :allowed)
  ;; don't use hard tabs
  ;; (setq indent-tabs-mode
  ;; 			(self-spec->*env-spec :indent :indent-tabs-mode))

  ;; ;; disable electric indent mode
  ;; (setq% electric-indent-mode
  ;; 			 (self-spec->*env-spec :indent :electric-indent-mode)
  ;; 			 electric)
  
  ;; default tab-width
  (setq-default tab-width (self-spec->*env-spec :edit :tab-with))

  (setq auto-save-default (self-spec->*env-spec :edit :auto-save-default))
  
  )

 ;; end of indent
