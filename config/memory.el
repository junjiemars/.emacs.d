;;;; -*- lexical-binding:t -*-
;;;;
;; Session 
;;;;




;; Read/Save desktop


(defvar self-previous-env-spec nil)


(defmacro env-spec ()
  "Return the `cons' of virtualized `path-env-spec' source 
and compiled file name."
  (let ((*v* (v-home* "config/")))
    `(cons ,(concat *v* ".env-spec.el")
           ,(concat *v* ".env-spec.elc"))))


(defun save-env-spec ()
  (self-safe-call*
   "env-spec"
   (let ((f (car (env-spec))))
     (save-sexpr-to-file
      (list 'setq 'self-previous-env-spec (list 'quote *val*)) f)
     (byte-compile-file f))))

(add-hook 'kill-emacs-hook #'save-env-spec)


(defun load-env-spec ()
  (let ((f (cdr (env-spec))))
    (when (file-exists-p f)
      (load f))))

(load-env-spec)


(theme-supported-p
    (defun switch-theme! (previous current)
      (safe-fn-when self-load-theme!
        (cond
         ((and (not (plist-get current :allowed))
               (plist-get previous :allowed))
          (self-load-theme! 
           (plist-get previous :path)
           (plist-get previous :name))
          (disable-theme (plist-get previous :name)))
         ((and (plist-get current :allowed)
               (not (plist-get previous :allowed)))
          (enable-theme (plist-get current :name)))))))


(defun self-desktop-read! ()
  (terminal-supported-p
    (version-supported-when <= 24.4
      (version-supported-when > 25
        (setq-default desktop-restore-forces-onscreen nil))))

  (self-safe-call*
   "env-spec"
   (let ((desktop (plist-get *val* :desktop)))
     (when (and desktop
                (plist-get desktop :allowed))
       (desktop-read (v-home* ".desktop/"))))))

(add-hook 'after-init-hook #'self-desktop-read!)



(defun self-desktop-save! ()
  (self-safe-call*
   "env-spec"
   
   (let ((desktop (plist-get *val* :desktop)))
     (when (and desktop
                (plist-get desktop :allowed))
       
       (let ((f (plist-get desktop :files-not-to-save)))
         (when f
           (setq-default desktop-files-not-to-save f)))
       (let ((b (plist-get desktop :buffers-not-to-save)))
         (when b
           (setq-default desktop-buffers-not-to-save b)))
       (let ((m (plist-get desktop :modes-not-to-save)))
         (setq-default desktop-modes-not-to-save
                       (append '(tags-table-mode) m)))

       (theme-supported-p
        (switch-theme!
         (plist-get self-previous-env-spec :theme)
         (plist-get *val* :theme)))

       (version-supported-if
           >= 23
           (desktop-save (v-home! ".desktop/"))
         (desktop-save (v-home! ".desktop/") t))))))

(add-hook 'kill-emacs-hook #'self-desktop-save!)
