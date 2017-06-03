;;;;
;; Splash 
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
(defmacro self-load-theme! (&optional theme-dir theme-name)
  "Load THEME of current platform from THEME-DIR by THEME-NAME, if THEME-DIR
or THEME-NAME non-existing then load default `theme/tomorrow-night-eighties'

\(fn THEME-DIR THEME-NAME)"
  `(graphic-supported-p
     (let ((dir (if (and ,theme-dir (file-exists-p ,theme-dir))
                    ,theme-dir
                  (concat emacs-home "theme")))
           (name (if ,theme-name ,theme-name 'tomorrow-night-eighties)))
       (add-to-list 'custom-theme-load-path dir)
       (add-to-list 'load-path dir)
       (version-supported-if >= 24.1
                             (load-theme name)
         (load-theme name t)))))


(defmacro font-exists-p (font)
  "Return t if font exists

\(FN FONT\)"
  `(when (find-font (font-spec :name ,font))
     t))


(defmacro self-default-font! (font)
  "Set default font in graphic mode.

\(FN FONT\)"
  `(version-supported-when <= 24
     (graphic-supported-p
       (when (font-exists-p ,font)
         (add-to-list 'default-frame-alist (cons 'font  ,font))
         (set-face-attribute 'default t :font ,font)
         (set-face-attribute 'default nil :font ,font)
         (version-supported-if <= 24.0
                               (set-frame-font ,font nil t)
           (set-frame-font ,font))))))
