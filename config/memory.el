;;;; -*- lexical-binding:t -*-
;;;;
;; Session 
;;;;




;; Read/Save desktop



(defmacro env-spec ()
  "Return the `cons' of virtualized `path-env-spec' source 
and compiled file name."
  (let ((*v* (v-home* "config/")))
    `(cons ,(concat *v* ".env-spec.el")
           ,(concat *v* ".env-spec.elc"))))


(defun save-env-spec ()
  (let ((env (env-spec)))
    (self-safe-call*
     "env-spec"
     (message "##:%s"(listp *val*))
     (save-sexpr-to-file
      (list 'defvar 'self-previous-env-spec (list 'quote *val*))
      (car env))
     (byte-compile-file (car env)))))


(defmacro load-env-spec ()
  `(let ((env (env-spec)))
     (if (file-exists-p (cdr env))
         (load (cdr env)))))


(defun self-desktop-read! ()
  
  (terminal-supported-p
    (version-supported-when = 24.4
      (setq-default desktop-restore-forces-onscreen nil)))

  (self-safe-call*
   "env-spec"
   (let ((desktop (plist-get *val* :desktop)))
     (when (and desktop
                (plist-get desktop :allowed))
       (desktop-read (v-home* ".desktop/"))))))


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

       (safe-fn-when self-load-theme!
         (let ((current (plist-get *val* :theme)))
           (cond
            ((or (not current)
                 (not (plist-get current :allowed)))
             (progn
               (load-env-spec)
               (when (and (symbolp 'self-previous-env-spec)
                          self-previous-env-spec)
                 (let ((previous
                        (plist-get self-previous-env-spec :theme)))
                   (self-load-theme! (expand-file-name
                                      (plist-get previous :path))
                                     (plist-get previous :name))
                   (list (plist-get previous :name))
                   (disable-theme (plist-get previous :name))))))
            ((and current (plist-get current :allowed))
             (progn
               (load-env-spec)
               (when (and (symbolp 'self-previous-env-spec)
                          self-previous-env-spec)
                 (let ((previous
                        (plist-get self-previous-env-spec :theme)))
                   (when (not (plist-get previous :allowed))
                     (disable-theme (plist-get previous :name))
                     (enable-theme (plist-get previous :name))))))))))

       (save-env-spec)
       
       (version-supported-if
           >= 23
           (desktop-save (v-home! ".desktop/"))
         (desktop-save (v-home! ".desktop/") t))))))


(add-hook 'after-init-hook #'self-desktop-read!)

(add-hook 'kill-emacs-hook #'save-env-spec)

(add-hook 'kill-emacs-hook #'self-desktop-save!)
