;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; graphic.el: menu, toolbar, font, theme, etc.
;;;;


;;; basic UI

;; Disable menu bar
(when-fn%  'menu-bar-mode nil (menu-bar-mode -1))

;; Disable tool bar
(when-graphic% (when-fn% 'tool-bar-mode nil (tool-bar-mode -1)))

;; Disable scroll bar
(when-fn% 'scroll-bar-mode nil (scroll-bar-mode -1))

;; Go straight to scratch buffer on startup
(setq% inhibit-splash-screen t)


 ;; end of basic UI


;;; basic macro

(defmacro when-theme% (&rest body)
  (declare (indent 0))
  `(when-graphic%
     (when-version% < 23
       ,@body)))


(defmacro when-font% (&rest body)
  (declare (indent 0))
  `(when-graphic%
     ,@body))

 ;; end of when-* macro


;; Frame

(when-graphic%
  (when (self-spec->*env-spec :frame :allowed)
    (mapc (lambda (x)
            (add-to-list 'initial-frame-alist x))
          (self-spec->*env-spec :frame :initial))
    (mapc (lambda (x)
            (add-to-list 'default-frame-alist x)
            (when (eq 'font (car x))
              (set-face-attribute 'default nil :font (cdr x))))
          (self-spec->*env-spec :frame :default))
    (setq frame-resize-pixelwise
          (self-spec->*env-spec :frame :frame-resize-pixelwise))))

 ;; end of Frame


;; Theme

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

