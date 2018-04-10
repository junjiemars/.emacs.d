;;;; -*- lexical-binding:t -*-
;;;;
;; Moduel Management 
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
             (list '("melpa" . "https://melpa.org/packages/")))) package)

  (version-supported-when
      <= 25.1
    (setq% package-archive-priorities
	   (list '("melpa-stable" . 10)
		 '("melpa" . 5)
		 '("gnu" . 0)) package))

  (package-refresh-contents))



;; Package Initialize
;; (require 'package)
(declare-function package-installed-p "package")
(setq package-enable-at-startup nil)

(version-supported-when
    <= 25.1
  (setq custom-file (v-home% "config/" ".selected-packages.el")))

(package-initialize)

(defmacro parse-package-spec! (dir spec &optional remove-unused)
  "Parse SPEC, install, remove and setup packages in DIR."
  `(dolist (s ,spec)
		 (when (consp s)
			 (dolist (p (self-spec-> s :packages))
				 (if (package-installed-p p)
						 (when (and ,remove-unused (not (self-spec-> s :cond)))
							 (let ((d (car (alist-get p package-alist))))
								 (when d
									 (version-supported-if
											 <= 25.0
											 (package-delete d t t)
										 (package-delete d)))))
					 (when (self-spec-> s :cond)
						 (unless *repository-initialized*
							 (initialize-package-repository!)
							 (setq *repository-initialized* t))
						 (version-supported-if
								 <= 25.0
								 (package-install p t)
							 (package-install p)))))
			 (when (self-spec-> s :cond)
				 (apply #'compile!
								,dir
								(delete nil (self-spec-> s :compile)))))))


(defvar basic-package-spec
  (list (list
         :cond t
         :packages `(aggressive-indent
                     bing-dict
                     markdown-mode
                     paredit
                     rainbow-delimiters
                     ,(version-supported-when <= 24.1 'yaml-mode))
         :compile `(,(compile-unit (emacs-home* "config/lisps.el"))))))



(package-spec-:allowed-p
	;; Load basic package spec
	(parse-package-spec! v-dir basic-package-spec
											 (self-spec->*env-spec :package :remove-unused))
	;; Load self packages spec
	(parse-package-spec! v-dir (self-spec->*package-spec)
											 (self-spec->*env-spec :package :remove-unused)))

