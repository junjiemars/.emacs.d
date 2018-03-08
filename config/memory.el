;;;; -*- lexical-binding:t -*-
;;;;
;; memory
;;;;




;; Read/Save desktop


(defvar *self-previous-env-spec* nil)


(defmacro memory-spec->% (&rest keys)
  "Extract value from a list of spec via KEYS at compile time."
  `(self-spec->%
       (list :source ,(concat (v-home* "config/") ".env-spec.el")
	     :compiled ,(concat (v-home* "config/") ".env-spec.elc"))
     ,@keys))


(defun save-env-spec! ()
  (when (save-sexp-to-file
	 `(setq *self-previous-env-spec*
		',(self-spec->*env-spec))
	 (memory-spec->% :source))
    (byte-compile-file (memory-spec->% :source))))

(add-hook 'kill-emacs-hook #'save-env-spec! t)


(defmacro load-env-spec ()
  `(when (file-exists-p (memory-spec->% :compiled))
     (load (memory-spec->% :compiled))))

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

  (when (and (self-spec->*env-spec :desktop :allowed)
	     (file-exists-p (v-home* ".desktop/")))
    (theme-supported-p
	(when (consp (theme-changed-p
		      (self-spec-> *self-previous-env-spec* :theme)
		      (self-spec->*env-spec :theme)))
	  (setq% desktop-restore-frames nil desktop)))
    (setq% desktop-restore-eager
	   (self-spec->*env-spec :desktop :restore-eager) desktop)
    (desktop-read (v-home* ".desktop/"))))


(add-hook 'after-init-hook #'self-desktop-read! t)


 ;; end of Read desktop



;; Save desktop
(defun self-desktop-save! ()
  (when (self-spec->*env-spec :desktop :allowed)
    (let ((f (self-spec->*env-spec :desktop :files-not-to-save)))
      (when f (setq% desktop-files-not-to-save f desktop)))
    
    (let ((b (self-spec->*env-spec :desktop :buffers-not-to-save)))
      (when b (setq% desktop-buffers-not-to-save b desktop)))
    
    (let ((m (self-spec->*env-spec :desktop :modes-not-to-save)))
      (setq% desktop-modes-not-to-save
	     (append '(tags-table-mode) m) desktop))

    (theme-supported-p
	(switch-theme! (self-spec-> *self-previous-env-spec* :theme)
		       (self-spec->*env-spec :theme)))

    (version-supported-if
	>= 23
	(desktop-save (v-home! ".desktop/"))
      (desktop-save (v-home! ".desktop/") t))))


(add-hook 'kill-emacs-hook #'self-desktop-save! t)
