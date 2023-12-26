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

(defmacro inhibit-blinking (&rest body)
  (declare (indent 0))
  `(lexical-let% ((inhibit-redisplay t)
                  (frame-inhibit-implied-resize t))
     (when-version% >= 25
       (ignore* frame-inhibit-implied-resize))
     (progn% ,@body)))

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
         (a1 (self-spec-> f1 :allowed))
         (s1 (cond (a1 (self-spec-> f1 :inhibit-splash-screen))
                   (t inhibit-splash-screen)))
         (i1 (append
              +essential-frame-set+
              (when-graphic%
                (when a1 (self-spec-> f1 :initial)))))
         (d1 (when-graphic%
               (when a1 (self-spec-> f1 :default)))))
    ;; `frame-resize-pixelwise'
    (setq% frame-resize-pixelwise
           (self-spec-> f1 :frame-resize-pixelwise))
    (setq
     ;; `inhibit-splash-screen'
     inhibit-splash-screen s1
     ;; `initial-frame-alist'
     initial-frame-alist i1
     ;; `default-frame-alist'
     default-frame-alist (or d1 i1))))


(when-graphic%

  (defun toggle-frame-initialized (&optional frame)
    "Toggle initialiation state of FRAME."
    (interactive)
    (let ((f1 (*self-env-spec* :get :frame)))
      (when (self-spec-> f1 :allowed)
        (modify-frame-parameters
         frame
         (list (cons 'fullscreen nil)
               (cons 'fullscreen-restore nil)))
        (let ((w (cdr (assoc** 'width default-frame-alist)))
              (h (cdr (assoc** 'height default-frame-alist))))
          (when w (set-frame-width nil w))
          (when h (set-frame-height nil h)))))))


;; end of Frame

;;;
;; theme
;;;

(when-theme%

  (defmacro load-theme! (name &optional dir)
    "\\=`load-theme\\=' by NAME.\n
If DIR is nil then load the built-in \\=`customize-themes\\=' by NAME."
    (let ((n (gensym*)) (d (gensym*)))
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

(inhibit-blinking
  (self-frame-init!)
  (when-theme%
    (if (*self-env-spec* :get :desktop :allowed)
        (make-thread* #'self-theme-init!)
      (self-theme-init!))))


(provide 'graphic)


;; end of graphic.el
