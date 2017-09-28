;;;; -*- lexical-binding:t -*-
;;;;
;; Boot
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

(font-supported-p
    
    (defmacro font-exists-p (font)
      "Return t if font exists

\(FN FONT\)"
      `(when (find-font (font-spec :name ,font))
         t))


  (defmacro self-default-font! (font)
    "Set default font in graphic mode.

\(FN FONT\)"
    `(when (font-exists-p ,font)
       (add-to-list 'default-frame-alist (cons 'font ,font))
       (set-face-attribute 'default t :font ,font)
       (set-face-attribute 'default nil :font ,font)
       (version-supported-if <= 24.0
                             (set-frame-font ,font nil t)
         (set-frame-font ,font))))

  
  ;; Load default font
  (self-safe-call*
   "env-spec"
   (when (self-spec->* :font :allowed)
     (self-default-font! (self-spec->* :font :name))))

  (defmacro self-cjk-font! (name size)
    "Set CJK font in Graphic mode.

\(FN NAME SIZE\)"
    `(when (font-exists-p ,name)
       (safe-fn-when set-fontset-font
         (dolist (c '(han kana cjk-misc))
           (set-fontset-font (frame-parameter nil 'font)
                             c (font-spec :family ,name
                                          :size ,size))))))

  ;; Load cjk font
  (self-safe-call*
   "env-spec"
   (when (self-spec->* :cjk-font :allowed)
     (self-cjk-font! (self-spec->* :cjk-font :name)
                     (self-spec->* :cjk-font :size)))))

;; End of font-supported-p


(theme-supported-p
    
    (defmacro self-load-theme! (name &optional dir)
      "Load theme from THEME-DIR by THEME-NAME, if THEME-DIR is nil then
load the named built-in theme, if THEME-NAME non-existing then 
load default `theme/tomorrow-night-eighties'

\(fn THEME-NAME &optional THEME-DIR)"
      `(progn%
        (when (and ,dir (file-exists-p ,dir))
          (add-to-list 'custom-theme-load-path ,dir)
          (add-to-list 'load-path ,dir))
        (version-supported-if >= 24.1
                              (load-theme ,name)
          (load-theme ,name t))))


  ;; Load theme
  (self-safe-call*
   "env-spec"
   (when (self-spec->* :theme :allowed)
     (self-load-theme! (self-spec->* :theme :name)
                       (self-spec->* :theme :path)))))

;; End of theme-supported-p




;; Terminal style
(terminal-supported-p
  ;; line number format on Terminal
  (safe-setq linum-format "%2d ")
  ;;above version 23 transient-mark-mode is enabled by default
  (version-supported-when > 23 (transient-mark-mode t))
  (set-face-background 'region "white")
  (set-face-foreground 'region "black"))



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




