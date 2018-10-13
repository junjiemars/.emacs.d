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


;; comments
(defun toggle-comment ()
  "Toggle comment on current line or region."
  (interactive)
  (let (begin end)
    (if-fn% region-active-p nil
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
    (if-fn% next-logical-line nil
            (next-logical-line)
      (next-line))))

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
(unless-fn% xref-find-definitions xref
  (when-fn% pop-tag-mark etags
    (with-eval-after-load 'etags
      ;; define keys for `pop-tag-mark' and `tags-loop-continue'
      (define-key% (current-global-map) (kbd "M-,") #'pop-tag-mark)
      (define-key% (current-global-map) (kbd "M-*") #'tags-loop-continue))))


 ;; end of indent


(version-supported-when > 24.4
  ;; fix: no quit key to hide *Messages* buffer
  (with-current-buffer (get-buffer "*Messages*")
    (if-fn% read-only-mode nil
            (read-only-mode 1)
      (toggle-read-only t))
    (local-set-key (kbd "q") #'quit-window)))


;; define `goto-char' keybinding for Emacs23.4-
(define-key% (current-global-map) (kbd "M-g c") #'goto-char)


;; define `recenter-top-bottom' for Emacs23.2-
(unless-fn% recenter-top-bottom nil

  (defvar recenter-last-op nil
    "Indicates the last recenter operation performed.
Possible values: `top', `middle', `bottom', integer or float numbers.
It can also be nil, which means the first value in `recenter-positions'.")

  (defvar recenter-positions '(middle top bottom)
    "Cycling order for `recenter-top-bottom'.
A list of elements with possible values `top', `middle', `bottom',
integer or float numbers that define the cycling order for
the command `recenter-top-bottom'.

Top and bottom destinations are `scroll-margin' lines from the true
window top and bottom.  Middle redraws the frame and centers point
vertically within the window.  Integer number moves current line to
the specified absolute window-line.  Float number between 0.0 and 1.0
means the percentage of the screen space from the top.  The default
cycling order is middle -> top -> bottom.")


  (defun recenter-top-bottom (&optional arg)
    "Move current buffer line to the specified window line.
With no prefix argument, successive calls place point according
to the cycling order defined by `recenter-positions'.

A prefix argument is handled like `recenter':
 With numeric prefix ARG, move current line to window-line ARG.
 With plain `C-u', move current line to window center."
    (interactive "P")
    (cond
     (arg (recenter arg)); Always respect ARG.
     (t
      (setq recenter-last-op
            (if (eq this-command last-command)
                (car (or (cdr (member recenter-last-op recenter-positions))
                         recenter-positions))
              (car recenter-positions)))
      (let ((this-scroll-margin
             (min (max 0 scroll-margin)
                  (truncate (/ (window-body-height) 4.0)))))
        (cond ((eq recenter-last-op 'middle)
               (recenter))
              ((eq recenter-last-op 'top)
               (recenter this-scroll-margin))
              ((eq recenter-last-op 'bottom)
               (recenter (- -1 this-scroll-margin)))
              ((integerp recenter-last-op)
               (recenter recenter-last-op))
              ((floatp recenter-last-op)
               (recenter (round (* recenter-last-op (window-height))))))))))

  (define-key (current-global-map) (kbd "C-l") #'recenter-top-bottom))

 ;; end of recenter-top-bottom

