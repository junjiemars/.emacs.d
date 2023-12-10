;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; graphic.el
;;;;
;; Commentary: initialize menu, toolbar, font, theme, etc.
;;;;


;;; macro

(defmacro when-theme% (&rest body)
  (declare (indent 0))
  `(when-graphic%
     (when-version% < 23
       ,@body)))


(defmacro when-font% (&rest body)
  (declare (indent 0))
  `(when-graphic%
     ,@body))

;; end of macro

;;;
;; frame
;;;

(defconst +essential-frame-set+
  (eval-when-compile
    (append
     `((menu-bar-lines . 0)
       (vertical-scroll-bars))
     (when-fn% 'tab-bar-mode nil `((tab-bar-lines . 0)))
     (when-graphic% `((tool-bar-lines . 0)))))
  "The essential frame set.")


(defun self-frame-init! ()
  "Initialize frame specs from \\=`*self-env-spec*\\='."
  (let* ((f1 (*self-env-spec* :get :frame))
         (a1 (self-spec-> f1 :allowed)))
    (setq
     ;; `initial-frame-alist'
     initial-frame-alist (append
                          +essential-frame-set+
                          (when-graphic%
                            (when a1
                              (self-spec-> f1 :initial))))
     ;; `inhibit-splash-screen'
     inhibit-splash-screen (if a1
                               (self-spec-> f1 :inhibit-splash-screen)
                             inhibit-splash-screen))))


(when-graphic%

  (defun toggle-frame-initialized (&optional frame)
    "Toggle initialiation state of FRAME."
    (interactive)
    (let ((f1 (*self-env-spec* :get :frame)))
      (when (self-spec-> f1 :allowed)
        ;; `frame-resize-pixelwise'
        (setq% frame-resize-pixelwise
               (self-spec-> f1 :frame-resize-pixelwise))
        ;; `default-frame-alist'
        (let ((a (setq default-frame-alist
                       (or (self-spec-> f1 :default)
                           initial-frame-alist))))
          (modify-frame-parameters
           frame
           (list (cons 'fullscreen nil)
                 (cons 'fullscreen-restore nil)))
          (set-frame-width frame (cdr (assoc** 'width a)))
          (set-frame-height frame (cdr (assoc** 'height a))))))))


;; end of Frame

;;;
;; theme
;;;

(when-theme%

  (defmacro load-theme! (name &optional dir)
    "\\=`load-theme\\=' by NAME.\n
If DIR is nil then load the built-in \\=`customize-themes\\=' by NAME."
    (let ((n (gensym)) (d (gensym)))
      `(let ((,n ,name) (,d ,dir))
         (when ,d (setq custom-theme-directory ,d))
         (if-version% >= 24.1
                      (load-theme ,n)
           (load-theme ,n t))))))


(when-theme%

  (defun self-theme-init! (&optional reset)
    "Initialize theme specs from \\=`*self-env-spec*\\='.\n
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
                         (let ((f (concat dir
                                          (symbol-name name)
                                          "-theme.el")))
                           (compile! (compile-unit* f t))
                           (load-theme!
                            name (concat dir "/" (v-name) "/")))
                       (load-theme! name dir)))
                    (t
                     ;; load builtin theme
                     (load-theme! name))))))))))


;; end of when-theme%


(self-frame-init!)
(when-theme% (make-thread* #'self-theme-init!))


(provide 'graphic)


;; end of graphic.el
