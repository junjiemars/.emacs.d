;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; module.el
;;;;


;; define package user dir
(setq% package-user-dir (v-home% "elpa/") package)


(defvar *repository-initialized* nil
  "Indicate `initialize-package-repository!' whether has been called.")


(defsubst initialize-package-repository! ()
  (setq%
   package-archives
   (append (list '("gnu" . "https://elpa.gnu.org/packages/")
                 '("melpa-stable" . "https://stable.melpa.org/packages/"))
           (version-supported-when
               <= 25.1
             (list '("melpa" . "https://melpa.org/packages/"))))
   package)

  (version-supported-when
      <= 25.1
    (setq% package-archive-priorities
	   (list '("melpa-stable" . 10)
		 '("melpa" . 5)
		 '("gnu" . 0))
	   package))

  (package-refresh-contents))


(defsubst check-package-name (package)
  (cond ((symbolp package) (cons package nil))
	((and (stringp package) (file-exists-p package))
	 (cons (intern (match-string* "\\(.*\\)-[.0-9]+\\'"
				      (file-name-base* package) 1))
	       package))
	(t nil)))


(defsubst delete-package! (description &optional package)
  (version-supported-if
      <= 25.0
      (progn%
       (ignore* package)
       (package-delete (car description) t t))
    (version-supported-if
	<= 24.4
	(progn%
	 (ignore* package)
	 (package-delete (car description)))
      (package-delete
       (symbol-name package)
       (mapconcat #'identity
		  (mapcar (lambda (x)
			    (number-to-string x))
			  (aref description 0))
		  ".")))))


(defsubst install-package! (package &optional tar)
  (if tar
      (package-install-file package)
    (version-supported-if
	<= 25.0
	(package-install package t)
      (package-install package))))




;; Package Initialize
;; (require 'package)
(declare-function package-installed-p "package")
(setq package-enable-at-startup nil)

(version-supported-when
    <= 25.1
  (setq custom-file (v-home% "config/" ".selected-packages.el")))

(package-initialize)

(defsubst parse-package-spec! (dir spec &optional remove-unused)
  "Parse SPEC, install, remove and setup packages in DIR."
  (dolist (s spec)
    (when (consp s)
      (dolist (p (self-spec-> s :packages))
	(let ((ns (check-package-name p)))
	  (when (consp ns)
	    (let ((n (car ns)) (tar (cdr ns)))
	      (if (package-installed-p n)
		  (when (and remove-unused (not (self-spec-> s :cond)))
		    (let ((d (alist-get n package-alist)))
		      (when d (delete-package! d n))))
		(when (self-spec-> s :cond)
		  (if tar
		      (install-package! tar t)
		    (unless *repository-initialized*
		      (initialize-package-repository!)
		      (setq *repository-initialized* t))
		    (install-package! n))))))))
      (when (self-spec-> s :cond)
	(apply #'compile!
	       dir
	       (delete nil (self-spec-> s :compile)))))))


(defvar basic-package-spec
  (list (list
         :cond t
         :packages `(aggressive-indent
                     bing-dict
                     paredit
                     rainbow-delimiters
                     ,(version-supported-when <= 24.1 'yaml-mode))
        :compile `(,(compile-unit (emacs-home* "config/on-module.el"))))))


(package-spec-:allowed-p
  ;; Load basic package spec
  (parse-package-spec! v-dir basic-package-spec
		       (self-spec->*env-spec :package :remove-unused))
  ;; Load self packages spec
  (parse-package-spec! v-dir (self-spec->*package-spec)
		       (self-spec->*env-spec :package :remove-unused)))

