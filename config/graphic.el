;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; graphic.el
;;;;
;; Commentary: initialize menu, toolbar, font, theme, etc.
;;;;


(defun frame-spec->* (&optional spec)
  "Extract :frame from env-spec via SPEC."
  (cond (spec (*self-env-spec* :get :frame spec))
        (t (*self-env-spec* :get :frame))))

(defun theme-spec->* (&optional spec)
  "Extract from :theme env-spec via SPEC."
  (cond (spec (*self-env-spec* :get :theme spec))
        (t (*self-env-spec* :get :theme))))



;;; macro

(defmacro when-theme% (&rest body)
  (declare (indent 0))
  `(when-graphic%
     (when-version% < 23
       ,@body)))

(defmacro inhibit-blinking (&rest body)
  (declare (indent 0))
  `(let ((inhibit-redisplay t)
         (frame-inhibit-implied-resize t))
     (when-version% >= 25
       (ignore* frame-inhibit-implied-resize))
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
     (when-fn% tab-bar-mode nil `((tab-bar-lines . 0)))
     (when-graphic% `((tool-bar-lines . 0)))))
  "The essential frame set.")

;;; compatible for Emacs-23-
(unless-var% initial-buffer-choice nil
  (defvar initial-buffer-choice t))

(defun self-frame-init! ()
  "Initialize frame specs from \\=`*self-env-spec*\\='."
  (setq% frame-resize-pixelwise
         (frame-spec->* :frame-resize-pixelwise))
  (setq initial-buffer-choice (null (cadr command-line-args))
        inhibit-splash-screen
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
       `((fullscreen . nil) (fullscreen-restore . nil)))
      (let ((w (cdr (assq 'width default-frame-alist)))
            (h (cdr (assq 'height default-frame-alist))))
        (when w (set-frame-width nil w))
        (when h (set-frame-height nil h))))))

(unless-graphic%
  (defconst +tui-background-color+ "yellow")
  (defconst +tui-foreground-color+ "black"))

;; end of Frame

;;;
;; theme
;;;

(when-theme%

  (defun load-theme! (name &optional dir)
    "\\=`load-theme\\=' by NAME.\n
If DIR is nil then load the built-in \\=`customize-themes\\=' by NAME."
    (when dir (setq custom-theme-directory dir))
    (if-version%
        < 24.1
        (load-theme name t)
      (load-theme name))))


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
                     (load-theme! name (concat dir "/" (v-name) "/")))
                 (load-theme! name dir))))
            (t
             ;; load builtin theme
             (load-theme! (theme-spec->* :name)))))))


;; end of when-theme%

(defun self-graphic-init! ()
  (inhibit-blinking (self-frame-init!))
  (when-theme%
    (if (*self-env-spec* :get :desktop :allowed)
        (make-thread* #'self-theme-init!)
      (self-theme-init!))))




(provide 'graphic)


;; end of graphic.el
