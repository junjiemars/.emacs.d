;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; boot.el
;;;;


;; basic UI

;; Disable menu bar
(when-fn%  'menu-bar-mode nil (menu-bar-mode -1))

;; Disable tool bar
(graphic-supported-p (when-fn% 'tool-bar-mode nil (tool-bar-mode -1)))

;; Disable scroll bar
(when-fn% 'scroll-bar-mode nil (scroll-bar-mode -1))

;; Go straight to scratch buffer on startup when graphic supported
(graphic-supported-if
    (setq% inhibit-splash-screen t)
  (setq% inhibit-splash-screen nil))


 ;; end of basic UI


;; Theme and Font

(defmacro theme-supported-p (&rest body)
  (declare (indent 0))
  `(graphic-supported-p
     (version-supported-when < 23
       ,@body)))


(defmacro font-supported-p (&rest body)
  (declare (indent 0))
  `(graphic-supported-p
     ,@body))


(font-supported-p

    (defmacro when-font% (font &rest body)
      "If FONT exists then do BODY."
      (declare (indent 1))
      `(when% (find-font (font-spec :name ,font))
         ,@body)))

(font-supported-p
    
    (defmacro self-default-font! (font)
      "Set default FONT in graphic mode."
      `(when-font% ,font
         (add-to-list 'default-frame-alist (cons 'font ,font))
         (set-face-attribute 'default nil :font ,font))))

(font-supported-p
  
  ;; Load default font
  (when (self-spec->*env-spec :font :allowed)
    (self-default-font! (self-spec->*env-spec :font :name))))

(font-supported-p

    (defmacro self-cjk-font! (name size)
      "Set CJK font's NAME and SIZE in graphic mode."
      `(when-font% ,name
         (when-fn% 'set-fontset-font nil
           (mapc (lambda (c)
                   (set-fontset-font (frame-parameter nil 'font)
                                     c (font-spec :family ,name
                                                  :size ,size)))
                 '(han kana cjk-misc))))))

(font-supported-p
  
  ;; Load cjk font
  (when (self-spec->*env-spec :cjk-font :allowed)
    (self-cjk-font! (self-spec->*env-spec :cjk-font :name)
                    (self-spec->*env-spec :cjk-font :size))))

 ;; end of font-supported-p

(theme-supported-p

  (defmacro self-load-theme! (name &optional dir)
    "`load-theme' by NAME.
If DIR is nil then load the built-in `customize-themes' by NAME."
    `(progn
       (when (and ,dir (file-exists-p ,dir))
         (setq custom-theme-directory ,dir))
       (version-supported-if >= 24.1
                             (load-theme ,name)
         (load-theme ,name t)))))


;; Load theme
(theme-supported-p

  (when (self-spec->*env-spec :theme :allowed)
    (cond
     ((and (self-spec->*env-spec :theme :name)
           (self-spec->*env-spec :theme :custom-theme-directory))
      ;; load theme from :custom-theme-directory
      (if (self-spec->*env-spec :theme :compile)
          (when
              (compile!
                (compile-unit*
                 (concat
                  (self-spec->*env-spec :theme :custom-theme-directory)
                  (symbol-name (self-spec->*env-spec :theme :name))
                  "-theme.el")
                 t t))
            (self-load-theme!
             (self-spec->*env-spec :theme :name)
             (concat (self-spec->*env-spec :theme :custom-theme-directory)
                     (v-path* "/"))))
        (self-load-theme!
         (self-spec->*env-spec :theme :name)
         (self-spec->*env-spec :theme :custom-theme-directory))))

     ;; load builtin theme
     ((self-spec->*env-spec :theme :name)
      (self-load-theme! (self-spec->*env-spec :theme :name))))))


 ;; end of theme-supported-p


;; end of file

