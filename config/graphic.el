;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; graphic.el: menu, toolbar, font, theme, etc.
;;;;


;; basic UI

;; Disable menu bar
(when-fn%  'menu-bar-mode nil (menu-bar-mode -1))

;; Disable tool bar
(when-graphic% (when-fn% 'tool-bar-mode nil (tool-bar-mode -1)))

;; Disable scroll bar
(when-fn% 'scroll-bar-mode nil (scroll-bar-mode -1))

;; Go straight to scratch buffer on startup
(setq% inhibit-splash-screen t)


 ;; end of basic UI


;; Theme and Font

(defmacro when-theme% (&rest body)
  (declare (indent 0))
  `(when-graphic%
     (when-version% < 23
       ,@body)))


(defmacro when-font% (&rest body)
  (declare (indent 0))
  `(when-graphic%
     ,@body))

;; font supported

;; (when-font%

;;   (defmacro when-font-exist% (font &rest body)
;;     "If FONT exists then do BODY."
;;     (declare (indent 1))
;;     `(when% (and ,font (find-font (font-spec :name ,font)))
;;        ,@body)))

(when-font%
  
  (defmacro self-default-font! (name size)
    "Set default font by NAME and SIZE in graphic mode."
    ;; `(when-font-exist% ,name)
    `(when (and (numberp ,size) (> ,size 0))
       (let ((font (format "%s-%s" ,name ,size)))
         (add-to-list 'default-frame-alist
                      (cons 'font font))
         (set-face-attribute 'default nil :font font)))))

(when-font%
  
  ;; Load default font
  (when (self-spec->*env-spec :font :allowed)
    (self-default-font! (self-spec->*env-spec :font :name)
                        (self-spec->*env-spec :font :size))))


 ;; end of when-font%

;; theme supported

(when-theme%

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
(when-theme%

  (when (and (self-spec->*env-spec :theme :allowed)
             (self-spec->*env-spec :theme :name))
    (cond
     ((self-spec->*env-spec :theme :custom-theme-directory)
      ;; load theme from :custom-theme-directory
      (if (self-spec->*env-spec :theme :compile)
          (progn
            (compile! (compile-unit*
                       (concat
                        (self-spec->*env-spec :theme
                                              :custom-theme-directory)
                        (symbol-name (self-spec->*env-spec :theme
                                                           :name))
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


 ;; end of when-theme%


;; end of file

