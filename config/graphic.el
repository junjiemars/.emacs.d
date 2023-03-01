;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; graphic.el: menu, toolbar, font, theme, etc.
;;;;


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

  (defun self-frame-init-load! ()
    "Load frame initial specs from `*self-env-spec*'."
    (when (*self-env-spec* :get :frame :allowed)

      ;; `initial-frame-alist'
      (let ((init (*self-env-spec* :get :frame :initial)))
        (setq initial-frame-alist init))

      ;; disable menu bar
      (when-fn%  'menu-bar-mode nil (menu-bar-mode -1))

      ;; disable tool bar
      (when-fn% 'tool-bar-mode nil (tool-bar-mode -1))

      ;; disable scroll bar
      (when-fn% 'scroll-bar-mode nil (scroll-bar-mode -1))

      ;; disable tab bar
      (when-fn% 'tab-bar-mode nil (tab-bar-mode -1))

      ;; `inhibit-splash-screen'
      (when (*self-env-spec* :get :frame :allowed)
        (setq inhibit-splash-screen
              (*self-env-spec* :get :frame :inhibit-splash-screen))))))


(when-graphic%

  (defun self-frame-default-load! ()
    "Load frame default specs from `*self-env-spec*'."
    (when (*self-env-spec* :get :frame :allowed)
      ;; `frame-resize-pixelwise'
      (when-var% frame-resize-pixelwise nil
        (let ((pixelwise (*self-env-spec* :get :frame
                                          :frame-resize-pixelwise)))
          (setq frame-resize-pixelwise pixelwise)))

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
                    (if (and (*self-env-spec* :get :theme :compile)
                             (if-native-comp% nil t))
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
  (self-frame-init-load!)
  (when-theme%
    (make-thread* (lambda () (self-theme-load!)))))


(provide 'graphic)


;; end of file
