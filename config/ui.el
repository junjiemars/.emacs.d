;;;;
;; UI related configurations.
;;;;


;; Disable menu bar
(safe-call menu-bar-mode -1)

;; Disable tool bar
(graphic-supported-p (safe-call tool-bar-mode -1))

;; Disable scroll bar
(safe-call scroll-bar-mode -1)


;; Set font based on platform

(defmacro font-exists-p (font)
  "Return t if font exists"
  `(when (find-font (font-spec :name ,font))
     t))

(defmacro set-default-font! (font)
  `(when (font-exists-p ,font)
     (add-to-list 'default-frame-alist (cons 'font  ,font))
     (set-face-attribute 'default t :font ,font)
     (set-face-attribute 'default nil :font ,font)
     (version-supported-if <= 24.0
                           (set-frame-font ,font nil t)
                           (set-frame-font ,font))))

(defmacro set-cjk-font! (font)
  `(let ((name (car ,font))
         (size (cdr ,font)))
     (when (font-exists-p name)
       (safe-do-when set-fontset-font
         (dolist (c '(han kana cjk-misc))
           (set-fontset-font (frame-parameter nil 'font)
                             c (font-spec :family name
                                          :size size)))))))

;; Graphic / Terminal
(graphic-supported-if
    ;; load themes on graphic mode
    (let ((self-theme (self-symbol "theme")))
      (when (and (boundp self-theme)
                 (consp (symbol-value self-theme)))
        (let ((theme-dir (car (symbol-value self-theme))) 
              (theme-name (cdr (symbol-value self-theme))))
          (add-to-list 'custom-theme-load-path theme-dir)
          (add-to-list 'load-path theme-dir)
          (version-supported-if >= 24.1
                                (load-theme theme-name)
            (load-theme theme-name t)))))
  
  ;; line number format on Terminal
  (safe-setq linum-format "%2d ")
  ;;above version 23 transient-mark-mode is enabled by default
  (version-supported-when > 23 (transient-mark-mode t))
  (set-face-background 'region "white")
  (set-face-foreground 'region "black"))



;; Fonts
(version-supported-when
    <= 24.0 
  (safe-do-when!* (self-symbol "font")
    (set-default-font! (symbol-value (self-symbol "font"))))
  (safe-do-when!* (self-symbol "cjk-font")
    (set-cjk-font! (symbol-value (self-symbol "cjk-font")))))


;; Go straight to scratch buffer on startup
(version-supported-when
    <= 24
  (setq inhibit-splash-screen t))




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

