;;;;
;; UI related configurations.
;;;;


(comment
 (defmacro self-cjk-font! (name size)
   "Set CJK font in Graphic mode.

\(FN NAME SIZE\)"
   `(when (font-exists-p ,name)
      (safe-do-when set-fontset-font
        (dolist (c '(han kana cjk-misc))
          (set-fontset-font (frame-parameter nil 'font)
                            c (font-spec :family ,name
                                         :size ,size))))))


 (self-safe-call
  "cjk-font"
  (when (consp _val_)
    (self-cjk-font! (car _val_) (cdr _val_)))))


;; Terminal style
(terminal-supported-p
  ;; line number format on Terminal
  (safe-setq linum-format "%2d ")
  ;;above version 23 transient-mark-mode is enabled by default
  (version-supported-when > 23 (transient-mark-mode t))
  (set-face-background 'region "white")
  (set-face-foreground 'region "black"))


;; These settings relate to how emacs interacts with your platform


;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; Highlights matching parenthesis
(show-paren-mode 1)

;; No cursor blinking, it's distracting
(safe-call blink-cursor-mode 0)

;; full path in title bar
(graphic-supported-p
  (safe-setq frame-title-format "%b (%f)"))

;; Ignore ring bell
(safe-setq ring-bell-function 'ignore)
