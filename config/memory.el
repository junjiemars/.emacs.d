;;;; -*- lexical-binding:t -*-
;;;;
;; memory
;;;;




;; Read/Save desktop


(defvar self-previous-env-spec nil)


(defmacro env-spec->% (&rest keys)
  "Extract a value from the `list' of virtualized `env-spec' via KESY 
at compile time."
  (let ((spec `(list :source ,(concat (v-home* "config/") ".env-spec.el")
                     :compiled ,(concat (v-home* "config/") ".env-spec.elc"))))
    `(self-spec->% ,spec ,@keys)))


(defun save-env-spec! ()
  (self-safe-call
   "env-spec"
   (let ((f (env-spec->% :source)))
     (when (save-sexp-to-file
            `(setq self-previous-env-spec ',*val*) f)
       (byte-compile-file f)))))

(add-hook 'kill-emacs-hook #'save-env-spec! t)


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
      `(let ((p->c (theme-changed-p ,previous ,current)))
	 (when (consp p->c)
	   (if (and (car p->c) (not (cdr p->c)))
	       (progn
		 (self-load-theme! (self-spec-> ,previous :name)
				   (self-spec-> ,previous :path))
		 (disable-theme (self-spec-> ,previous :name)))
	     (enable-theme (self-spec-> ,current :name)))
	   (setq% desktop-restore-frames t desktop)))))


;; Read desktop
(defun self-desktop-read! ()
  (terminal-supported-p
    (version-supported-when <= 24.4
      (version-supported-when > 25
        (setq% desktop-restore-forces-onscreen nil desktop))))

  (self-safe-call
   "env-spec"
   (when (and (self-spec->* :desktop :allowed)
              (file-exists-p (v-home* ".desktop/")))
     (theme-supported-p
         (when (consp (theme-changed-p
                       (self-spec-> self-previous-env-spec :theme)
                       (self-spec->* :theme)))
           (setq% desktop-restore-frames nil desktop)))
     (setq% desktop-restore-eager
	    (self-spec->* :desktop :restore-eager) desktop)
     (desktop-read (v-home* ".desktop/")))))


(add-hook 'after-init-hook #'self-desktop-read! t)


 ;; end of Read desktop



;; Save desktop
(defun self-desktop-save! ()
  (self-safe-call
   "env-spec"
   
   (when (self-spec->* :desktop :allowed)
     (let ((f (self-spec->* :desktop :files-not-to-save)))
       (when f
         (setq% desktop-files-not-to-save f desktop)))
     (let ((b (self-spec->* :desktop :buffers-not-to-save)))
       (when b
         (setq% desktop-buffers-not-to-save b desktop)))
     (let ((m (self-spec->* :desktop :modes-not-to-save)))
       (setq% desktop-modes-not-to-save
	      (append '(tags-table-mode) m) desktop))

     (theme-supported-p
         (switch-theme! (self-spec-> self-previous-env-spec :theme)
                        (self-spec->* :theme)))

     (version-supported-if
         >= 23
         (desktop-save (v-home! ".desktop/"))
       (desktop-save (v-home! ".desktop/") t)))))


(add-hook 'kill-emacs-hook #'self-desktop-save! t)
