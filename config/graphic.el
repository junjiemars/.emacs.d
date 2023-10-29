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

(defconst +essential-frame-set+
  (eval-when-compile
    (append
     `((menu-bar-lines . 0)
       (vertical-scroll-bars))
     (when-fn% 'tab-bar-mode nil `((tab-bar-lines . 0)))
     (when-graphic% `((tool-bar-lines . 0)))))
  "The essential frame set.")


(defun self-frame-init-load! ()
  "Load frame initial specs from \\=`*self-env-spec*\\='."
  (let* ((f1 (*self-env-spec* :get :frame))
         (a1 (self-spec-> f1 :allowed)))
    ;; `initial-frame-alist'
    (setq initial-frame-alist (append
                               +essential-frame-set+
                               (when-graphic%
                                 (when a1
                                   (self-spec-> f1 :initial)))))
    ;; `inhibit-splash-screen'
    (when a1 (setq inhibit-splash-screen
                   (self-spec-> f1 :inhibit-splash-screen)))))


(when-graphic%

  (defun self-frame-default-load! (&optional frame force)
    "Load frame default specs from \\=`*self-env-spec*\\='."
    (let ((f1 (*self-env-spec* :get :frame)))
      (when (self-spec-> f1 :allowed)
        ;; `frame-resize-pixelwise'
        (when-var% frame-resize-pixelwise nil
          (setq frame-resize-pixelwise
                (self-spec-> f1 :frame-resize-pixelwise)))
        ;; `default-frame-alist'
        (let ((a (setq default-frame-alist
                       (or (self-spec-> f1 :default)
                           initial-frame-alist))))
          (when force
            (let ((fs (frame-parameter frame 'fullscreen))
                  (fr (frame-parameter frame 'fullscreen-restore)))
              (modify-frame-parameters
               frame
               (list (cons 'fullscreen (and (not fs) fr))
                     (cons 'fullscreen-restore fs)))
              (unless (and (not fs) fr)
                (set-frame-width frame (cdr (assoc** 'width a)))
                (set-frame-height frame (cdr (assoc** 'height a)))))))))))


(when-graphic%

  (defun toggle-frame-initialized (&optional frame)
    "Toggle initialiation state of FRAME."
    (interactive)
    (self-frame-default-load! frame t)))


 ;; end of Frame


;;; Theme

(when-theme%

  (defmacro load-theme! (name &optional dir)
    "\\=`load-theme\\=' by NAME.\n
If DIR is nil then load the built-in \\=`customize-themes\\=' by NAME."
    (let ((n (gensym*))
          (d (gensym*)))
      `(let ((,n ,name)
             (,d ,dir))
         (when ,d (setq custom-theme-directory ,d))
         (if-version% >= 24.1
                      (load-theme ,n)
           (load-theme ,n t))))))


(when-theme%

  (defun self-theme-load! (&optional reset)
    "Load theme specs from \\=`*self-env-spec*\\='.\n
If RESET is true then reset before load."
    (when reset (mapc #'disable-theme custom-enabled-themes))
    (let ((t1 (*self-env-spec* :get :theme)))
      (when (self-spec-> t1 :allowed)
        (let ((name (self-spec-> t1 :name)))
          (when name
            (let ((dir (self-spec-> t1 :custom-theme-directory)))
              (cond (dir
                     ;; load theme from :custom-theme-directory
                     (if (and (self-spec-> t1 :compile)
                              (if-native-comp% nil t))
                         (let ((f (concat dir (symbol-name name) "-theme.el")))
                           (compile! (compile-unit* f t))
                           (load-theme! name (concat dir (v-path* "/"))))
                       (load-theme! name dir)))
                    (t
                     ;; load builtin theme
                     (load-theme! name))))))))))


 ;; end of when-theme%


(self-frame-init-load!)
(when-theme% (make-thread* #'self-theme-load!))


(provide 'graphic)


;; end of file
