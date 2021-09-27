;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
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


;;; Frame
(when-graphic%

  (defmacro self-frame-load! ()
    "Load frame specs from `*self-env-spec*'."

    `(when (*self-env-spec* :get :frame :allowed)
       ;; `frame-resize-pixelwise'
       (when-var% frame-resize-pixelwise nil
         (setq frame-resize-pixelwise
               (*self-env-spec* :get :frame
                                :frame-resize-pixelwise)))

       ;; `initial-frame-alist'
       (setq initial-frame-alist
             (*self-env-spec* :get :frame :initial))

       ;; `default-frame-alist'
       (let ((font (*self-env-spec* :get :frame :font))
             (frame (*self-env-spec* :get :frame :default)))
         (setq default-frame-alist
               (if font
                   (push! (cons 'font font) frame)
                 frame))))))

 ;; end of Frame


;;; Theme

(when-theme%

  (defmacro load-theme! (name &optional dir)
    "`load-theme' by NAME.
If DIR is nil then load the built-in `customize-themes' by NAME."
    (let ((n (gensym*))
          (d (gensym*)))
      `(let ((,n ,name)
             (,d ,dir))
         (when ,d (setq custom-theme-directory ,d))
         (if-version% >= 24.1
                      (load-theme ,n)
           (load-theme ,n t))))))


;;; disable customized themes
(when-theme%

  (defun restore-default-theme! ()
    "Restore default theme."
    (interactive)
    (mapc #'disable-theme custom-enabled-themes)))


;;; Load theme
(when-theme%

  (defmacro self-theme-load! ()
    "Load theme specs from `*self-env-spec*'."
    `(when (*self-env-spec* :get :theme :allowed)
       (let ((name (*self-env-spec* :get :theme :name)))
         (when name
           (let ((dir (*self-env-spec* :get :theme
                                       :custom-theme-directory)))
             (cond (dir
                    ;; load theme from :custom-theme-directory
                    (if (*self-env-spec* :get :theme :compile)
                        (progn
                          (compile!
                            (compile-unit*
                             (concat dir (symbol-name name) "-theme.el")
                             t t))
                          (load-theme! name (concat dir (v-path* "/"))))
                      (load-theme! name dir)))
                   (t
                    ;; load builtin theme
                    (load-theme! name)))))))))


 ;; end of when-theme%


(when-graphic%
  (self-frame-load!)
  (self-theme-load!))


;; end of file
