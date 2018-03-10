;;;; -*- lexical-binding:t -*-
;;;;
;; Moduel Management 
;;;;




;; define package user dir
(setq% package-user-dir (v-home% "elpa/") package)


(defvar *repostory-initialized* nil
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



(defmacro install-package! (packages &optional dry)
  "Install missing packages, return alist of installed packages."
  `(let ((not-installed-packages
          (delete t (mapcar #'(lambda (p)
                                (if (package-installed-p p) t p))
                            ,packages))))
     (when not-installed-packages
       (unless ,dry
         (unless *repostory-initialized*
           (initialize-package-repository!)
           (setq *repostory-initialized* t)))
       (mapc (lambda (i)
               (unless ,dry
                 (version-supported-if
                     <= 25.0
                     (package-install i t)
                   (package-install i))))
             not-installed-packages))))


(defmacro parse-package-spec (dir spec)
  "Parse SPEC, install and setup packages in DIR."
  `(dolist (s ,spec)
     (when (consp s)
       (let ((m (self-spec-> s :packages)))
         (when (and (consp m) (self-spec-> s :cond))
           (install-package! (delete nil m))
           (apply #'compile!
                  ,dir
                  (delete nil (self-spec-> s :compile))))))))


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



;; Load basic package spec
(parse-package-spec v-dir basic-package-spec)


;; Load self packages spec
(parse-package-spec v-dir (self-spec->*package-spec))


