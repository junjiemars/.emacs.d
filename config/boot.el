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


;; Load theme
(graphic-supported-p
  (version-supported-when
      < 23
    (self-safe-call*
     "env-spec"
     (let ((theme (plist-get _val_ :theme)))
       (if (and theme (plist-get theme :allowed))
           (self-load-theme!
            (plist-get theme :path)
            (plist-get theme :name))
         (self-load-theme!
          (emacs-home* "theme/")
          'tomorrow-night-eighties))))))



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


;; Load default font
(graphic-supported-p 
  (self-safe-call*
   "env-spec"
   (let ((font (plist-get _val_ :font)))
     (when (and font (plist-get font :allowed))
       (self-default-font! (plist-get font :name))))))



(defmacro self-cjk-font! (name size)
  "Set CJK font in Graphic mode.

\(FN NAME SIZE\)"
  `(when (font-exists-p ,name)
     (safe-fn-when set-fontset-font
       (dolist (c '(han kana cjk-misc))
         (set-fontset-font (frame-parameter nil 'font)
                           c (font-spec :family ,name
                                        :size ,size))))))


;; Load CJK font
(graphic-supported-p
  (self-safe-call*
   "env-spec"
   (let ((cjk (plist-get _val_ :cjk-font)))
     (when (and cjk (plist-get cjk :allowed))
       (self-cjk-font!
        (plist-get cjk :name)
        (plist-get cjk :size))))))


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




(defmacro start-socks! (&optional port server version)
  "Switch on url-gateway to socks"
  `(version-supported-when < 22
     (eval-when-compile (require 'url))
     (setq-default url-gateway-method 'socks)
     (setq-default socks-server
                   (list "Default server"
                         (if ,server ,server "127.0.0.1")
                         (if ,port ,port 32000)
                         (if ,version ,version 5)))))


(self-safe-call*
 "env-spec"
 (let ((socks (plist-get _val_ :socks)))
   (when (and socks (plist-get socks :allowed))
     (start-socks! (plist-get socks :port)
                   (plist-get socks :server)
                   (plist-get socks :version)))))


(defmacro stop-socks! (&optional method)
  "Switch off url-gateway to native."
  `(version-supported-when < 22
     (eval-when-compile (require 'url))
     (setq-default url-gateway-method
                   (if ,method  ,method 'native))))


(defmacro clean-compiled-files ()
  "Clean all compiled files, need restart Emacs."
  `(dolist (d (list ,(vdir* "config/")
                    ,(vdir* "private/")))
     (dolist (f (directory-files d nil "\\.elc$"))
       (message "#Clean compiled file: %s" f)
       (delete-file (concat d f)))))


(defmacro clean-saved-user-files ()
  "Clean saved desktop, need restart Emacs."
  `(let ((dirs (list ,(vdir*  ".auto-save/")
                     ,(vdir*  ".desktop/")
                     ,(vdir*  ".bookmarks/")
                     ,(vdir*  ".ido/")
                     ,(vdir*  ".minibuffer/")
                     ,(vdir*  ".recentf/")
                     ,(vdir*  ".tags/")
                     ,(vdir*  ".places/")
                     ,(vdir*  ".smex/")
                     ,(vdir*  ".url/"))))
     (dolist (d dirs)
       (when (file-exists-p d)
         (dolist (f (directory-files d nil "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))
           (message "#Clean saved user file: %s" (concat d f))
           (delete-file (concat d f)))))))


(defmacro reset-emacs ()
  "Clean all compiled file and desktop, then restart Emacs."
  `(progn
     (clean-compiled-files)
     (clean-saved-user-files)
     (kill-emacs 0)))
