;;;;
;; UI related configurations.
;;;;



;; Disable menu bar
(safe-call menu-bar-mode -1)

;; Disable tool bar
(graphic-supported-p (safe-call tool-bar-mode -1))

;; Disable scroll bar
(safe-call scroll-bar-mode -1)

;; Go straight to scratch buffer on startup
(version-supported-when
    <= 24
  (setq inhibit-splash-screen t))



;; Theme and Font


(defmacro self-load-theme! (dir name)
  "Load THEME of current platform from THEME-DIR by THEME-NAME, if THEME-DIR
or THEME-NAME non-existing then load default `theme/tomorrow-night-eighties'

\(fn THEME-DIR THEME-NAME)"
  `(progn
     (add-to-list 'custom-theme-load-path ,dir)
     (add-to-list 'load-path ,dir)
     (version-supported-if >= 24.1
                           (load-theme ,name)
       (load-theme ,name t))))


(self-safe-call*
 "theme"
 (when (consp _val_)
   (self-load-theme! (car _val_) (cdr _val_))))


(defmacro font-exists-p (font)
  "Return t if font exists

\(FN FONT\)"
  `(when (find-font (font-spec :name ,font))
     t))


(defmacro self-default-font! (font)
  "Set default font in graphic mode.

\(FN FONT\)"
  `(when (font-exists-p ,font)
     (add-to-list 'default-frame-alist (cons 'font  ,font))
     (set-face-attribute 'default t :font ,font)
     (set-face-attribute 'default nil :font ,font)
     (version-supported-if <= 24.0
                           (set-frame-font ,font nil t)
       (set-frame-font ,font))))


(self-safe-call*
 "font"
 (self-default-font! _val_))



(defmacro self-cjk-font! (name size)
  "Set CJK font in Graphic mode.

\(FN NAME SIZE\)"
  `(when (font-exists-p ,name)
     (safe-do-when set-fontset-font
       (dolist (c '(han kana cjk-misc))
         (set-fontset-font (frame-parameter nil 'font)
                           c (font-spec :family ,name
                                        :size ,size))))))



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
