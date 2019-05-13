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
(when-graphic% (when-fn% 'tool-bar-mode nil (tool-bar-mode -1)))

;; Disable scroll bar
(when-fn% 'scroll-bar-mode nil (scroll-bar-mode -1))

;; Go straight to scratch buffer on startup when graphic supported
(if-graphic%
    (setq% inhibit-splash-screen t)
  (setq% inhibit-splash-screen nil))


 ;; end of basic UI


;; Theme and Font

(defmacro if-theme% (&rest body)
  (declare (indent 0))
  `(when-graphic%
     (when-version% < 23
       ,@body)))


(defmacro if-font% (&rest body)
  (declare (indent 0))
  `(when-graphic%
     ,@body))

;; font supported

(if-font%

  (defmacro when-font-exist% (font &rest body)
    "If FONT exists then do BODY."
    (declare (indent 1))
    `(when% (and ,font (find-font (font-spec :name ,font)))
       ,@body)))

(if-font%
  
  (defmacro self-default-font! (font)
    "Set default FONT in graphic mode."
    `(when-font-exist% ,font
       (add-to-list 'default-frame-alist (cons 'font ,font))
       (set-face-attribute 'default nil :font ,font))))

(if-font%
  
  ;; Load default font
  (when (self-spec->*env-spec :font :allowed)
    (self-default-font! (self-spec->*env-spec :font :name))))

(if-font%

  (defmacro self-cjk-font! (name size)
    "Set CJK font's NAME and SIZE in graphic mode."
    `(when-font-exist% ,name
       (when-fn% 'set-fontset-font nil
         (mapc (lambda (c)
                 (set-fontset-font (frame-parameter nil 'font)
                                   c (font-spec :family ,name
                                                :size ,size)))
               '(han kana cjk-misc))))))

(if-font%

  ;; Load cjk font
  (when (self-spec->*env-spec :cjk-font :allowed)
    (self-cjk-font! (self-spec->*env-spec :cjk-font :name)
                    (self-spec->*env-spec :cjk-font :size))))

 ;; end of if-font%

;; theme supported

(if-theme%

  (defmacro self-load-theme! (name &optional dir)
    "`load-theme' by NAME.
If DIR is nil then load the built-in `customize-themes' by NAME."
    `(when ,name
       (when (and ,dir (file-exists-p ,dir))
         (setq custom-theme-directory ,dir))
       (if-version% >= 24.1
                    (load-theme ,name)
         (load-theme ,name t)))))


;; Load theme
(if-theme%

  (when (and (self-spec->*env-spec :theme :allowed)
             (self-spec->*env-spec :theme :name))
    (cond
     ((self-spec->*env-spec :theme :custom-theme-directory)
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
     (t (self-load-theme! (self-spec->*env-spec :theme :name))))))


 ;; end of if-theme%


;; end of file

