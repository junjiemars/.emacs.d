;;;; -*- lexical-binding:t -*-
;;;;
;; Session 
;;;;




;; Read/Save desktop


(defvar self-previous-env-spec nil)


(defmacro env-spec->% (&rest keys)
  "Extract a value from the `list' of virtualized `env-spec' via KESY 
at compile time."
  (let ((spec `(list :source ,(concat (v-home* "config/") ".env-spec.el")
                     :compiled ,(concat (v-home* "config/") ".env-spec.elc"))))
    `(self-spec->% ,spec ,@keys)))


(defun save-env-spec ()
  (self-safe-call*
   "env-spec"
   (let ((f (env-spec->% :source)))
     (save-sexpr-to-file
      (list 'setq 'self-previous-env-spec (list 'quote *val*)) f)
     (byte-compile-file f))))

(add-hook 'kill-emacs-hook #'save-env-spec)


(defmacro load-env-spec ()
  `(let ((f (env-spec->% :compiled)))
     (when (file-exists-p f)
       (load f))))

(load-env-spec)


(theme-supported-p
    
    (defmacro theme-changed-p (previous current)
      "Return (previous . current)"
      `(cond
        ((not (eq (self-spec-> ,previous :allowed)
                  (self-spec-> ,current :allowed)))
         (cons (self-spec-> ,previous :allowed)
               (self-spec-> ,current :allowed)))
        ((or (not (eq (self-spec-> ,previous :name)
                      (self-spec-> ,current :name)))
             (not (string= (self-spec-> ,previous :path)
                           (self-spec-> ,current :path))))
         (cons nil t)))))

(theme-supported-p
    
    (defmacro switch-theme! (previous current)
      `(safe-fn-when self-load-theme!
         (let ((p->c (theme-changed-p ,previous ,current)))
           (when (consp p->c)
             (if (and (car p->c) (not (cdr p->c)))
                 (progn
                   (self-load-theme! (self-spec-> ,previous :name)
                                     (self-spec-> ,previous :path))
                   (disable-theme (self-spec-> ,previous :name)))
               (enable-theme (self-spec-> ,current :name)))
             (setq-default desktop-restore-frames t))))))


;; Read desktop
(defun self-desktop-read! ()
  (terminal-supported-p
    (version-supported-when <= 24.4
      (version-supported-when > 25
        (setq-default desktop-restore-forces-onscreen nil))))

  (self-safe-call*
   "env-spec"
   (when (self-spec->* :desktop :allowed)
     (theme-supported-p
         (when (consp (theme-changed-p
                       (self-spec-> self-previous-env-spec :theme)
                       (self-spec->* :theme)))
           (setq-default desktop-restore-frames nil)))
     
     (desktop-read (v-home* ".desktop/")))))

(add-hook 'after-init-hook #'self-desktop-read!)


 ;; end of Read desktop



;; Save desktop
(defun self-desktop-save! ()
  (self-safe-call*
   "env-spec"
   
   (when (self-spec->* :desktop :allowed)
     (let ((f (self-spec->* :desktop :files-not-to-save)))
       (when f
         (setq-default desktop-files-not-to-save f)))
     (let ((b (self-spec->* :desktop :buffers-not-to-save)))
       (when b
         (setq-default desktop-buffers-not-to-save b)))
     (let ((m (self-spec->* :desktop :modes-not-to-save)))
       (setq-default desktop-modes-not-to-save
                     (append '(tags-table-mode) m)))

     (theme-supported-p
         (switch-theme! (self-spec-> self-previous-env-spec :theme)
                        (self-spec->* :theme)))

     (version-supported-if
         >= 23
         (desktop-save (v-home! ".desktop/"))
       (desktop-save (v-home! ".desktop/") t)))))

(add-hook 'kill-emacs-hook #'self-desktop-save!)
