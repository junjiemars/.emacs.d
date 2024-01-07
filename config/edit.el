;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; edit.el
;;;;
;; Commentary: essential editing environment.
;;;;

;;;
;; self-edit
;;;

(defmacro edit-spec->* (&rest keys)
  "Extract :edit from env-spec via KEYS."
  `(*self-env-spec* :get :edit ,@keys))

(defun self-edit-env->disable-indent-tabs-mode ()
  "Disable \\=`indent-tabs-mode\\=' in major mode."
  (set (make-local-variable 'indent-tabs-mode) nil))

(defun self-edit-env->delete-trailing-whitespace ()
  "\\=`delete-trailing-whitespace\\=' before save."
  (when (apply #'derived-mode-p
               (edit-spec->* :delete-trailing-whitespace))
    (delete-trailing-whitespace)))

(defun self-edit-init! ()
  "Initialize edit spec from \\=`*self-edit-spec*\\='"
  (when (edit-spec->* :allowed)
    ;; indent
    (dolist* (x (edit-spec->* :indent))
      (set (car x) (or (cdr x) (edit-spec->* :tab-width))))

    ;; disable `indent-tabs-mode'
    (let ((modes (edit-spec->* :disable-indent-tabs-mode)))
      (dolist* (m modes)
        (let ((h (intern-soft (format "%s-hook" m))))
          (when h
            (add-hook
             h #'self-edit-env->disable-indent-tabs-mode)))))

    ;; default `tab-width' and `standard-indent'
    (setq-default tab-width (edit-spec->* :tab-width)
                  standard-indent tab-width)

    ;; default `auto-save-default'
    (setq auto-save-default (edit-spec->* :auto-save-default))

    ;; enable `narrow-to-region'
    (put 'narrow-to-region 'disabled
         (null (edit-spec->* :narrow-to-region)))
    ;; enable `narrow-to-page'
    (when% (get 'narrow-to-page 'disabled)
      (put 'narrow-to-page 'disabled nil))

    ;; `delete-trailing-whitespace' before save
    (append! #'self-edit-env->delete-trailing-whitespace
             before-save-hook)))

;; end of self-edit

;;;
;; Clean Emacs' user files
;;;

(defun clean-versioned-dirs (dirs &optional scope)
  "Clean versioned SCOPEd DIRS."
  (dolist* (d dirs)
    (when (and d (file-exists-p d))
      (dolist* (f (directory-files d nil "^[gt]_.*$"))
        (when (cond ((eq :8 scope) t)
                    ((eq :< scope)
                     (< (string-to-number
                         (string-match* "^[gt]_\\(.*\\)$" f 1))
                        +emacs-version+))
                    (t (null (string-match
                              (format
                               "^[gt]_%s\\'"
                               (regexp-quote
                                (number-to-string +emacs-version+)))
                              f))))
          (if-platform% 'windows-nt
              (shell-command (concat "rmdir /Q /S " (concat d f)))
            (shell-command (concat "rm -r " (concat d f)))))))))

(defun reset-emacs ()
  "Clean all compiled files and dot files, then kill Emacs."
  (interactive)
  (when (yes-or-no-p "Reset emacs?")
  	(clean-versioned-dirs
  	 (delq nil
  				 (mapcar
  					(lambda (d)
  						(unless (member d '(".git" ".gitignore" ".github"))
  							(concat (emacs-home* d) "/")))
  					(directory-files (emacs-home*) nil "^\\.[a-z]+")))
  	 :8)
  	(clean-compiled-files)
  	(setq kill-emacs-hook nil)
  	(kill-emacs 0)))

;; end of Clean Emacs' user files


(self-edit-init!)

;; end of edit.el
