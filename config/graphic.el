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
     ,@body))

(defmacro frame-spec->* (&rest keys)
  "Extract :frame from env-spec via KEYS."
  (declare (indent 0))
  `(*self-env-spec* :get :frame ,@keys))

(defmacro theme-spec->* (&rest keys)
  "Extract from :theme env-spec via KEYS."
  (declare (indent 0))
  `(*self-env-spec* :get :theme ,@keys))

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
  (setq% frame-resize-pixelwise
         (frame-spec->* :frame-resize-pixelwise))
  (setq inhibit-splash-screen
        (or (and (frame-spec->* :allowed)
                 (frame-spec->* :inhibit-splash-screen))
            inhibit-splash-screen)
        initial-frame-alist
        (append
         +essential-frame-set+
         (when-graphic%
           (and (frame-spec->* :allowed)
                (frame-spec->* :initial))))
        default-frame-alist
        (or (when-graphic%
              (and (frame-spec->* :allowed)
                   (frame-spec->* :default)))
            initial-frame-alist)))


(when-graphic%

  (defun toggle-frame-initialized (&optional frame)
    "Toggle initialiation state of FRAME."
    (interactive)
    (when (frame-spec->* :allowed)
      (modify-frame-parameters
       frame
       (list (cons 'fullscreen nil)
             (cons 'fullscreen-restore nil)))
      (let ((w (cdr (assoc** 'width default-frame-alist)))
            (h (cdr (assoc** 'height default-frame-alist))))
        (when w (set-frame-width nil w))
        (when h (set-frame-height nil h))))))


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
    (when (and (theme-spec->* :allowed)
               (theme-spec->* :name))
      (cond ((theme-spec->* :custom-theme-directory)
             ;; load theme from :custom-theme-directory
             (let ((dir (theme-spec->* :custom-theme-directory))
                   (name (theme-spec->* :name)))
               (if (and (theme-spec->* :compile)
                        (if-native-comp% nil t))
                   (let ((f (concat dir
                                    (symbol-name name)
                                    "-theme.el")))
                     (compile! (compile-unit* f t))
                     (load-theme!
                      name (concat dir "/" (v-name) "/")))
                 (load-theme! name dir))))
            (t
             ;; load builtin theme
             (load-theme! (theme-spec->* :name)))))))


;; end of when-theme%

(inhibit-blinking
  (self-frame-init!)
  (when-theme%
    (if (*self-env-spec* :get :desktop :allowed)
        (make-thread* #'self-theme-init!)
      (self-theme-init!))))


(provide 'graphic)


;; end of graphic.el
