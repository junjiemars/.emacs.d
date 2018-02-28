;;;; -*- lexical-binding:t -*-
;;;;
;; Moduel Management 
;;;;




;; define package user dir
(setq-default package-user-dir (v-home* "elpa/"))


(defvar *repostory-initialized* nil
  "Indicate `initialize-package-repository!' whether has been called.")


(defsubst initialize-package-repository! ()
  (setq-default
   package-archives
   (append (list '("gnu" . "https://elpa.gnu.org/packages/")
                 '("melpa-stable" . "https://stable.melpa.org/packages/"))
           (version-supported-when
               <= 25.1
             (list '("melpa" . "https://melpa.org/packages/")))))

  (version-supported-when
      <= 25.1
    (setq-default package-archive-priorities
                  (list '("melpa-stable" . 10)
                        '("melpa" . 5)
                        '("gnu" . 0))))

  (package-refresh-contents))



;; Package Initialize
;; (require 'package)
(declare-function package-installed-p "package")
(setq package-enable-at-startup nil)

(version-supported-when
    <= 25.1
  (setq custom-file (v-home* "config/" ".selected-packages.el")))

(package-initialize)



(defmacro install-package! (packages &optional dry)
  "Install missing packages, returns alist of installed packages"
  `(let ((not-installed-packages
          (delete t (mapcar #'(lambda (p)
                                (if (package-installed-p p) t p))
                            ,packages))))
     (when not-installed-packages
       (unless ,dry
         (when (not *repostory-initialized*)
           (initialize-package-repository!)
           (setq *repostory-initialized* t)))
       (message "#Installing the missing %d ,packages: %s"
                (length not-installed-packages)
                not-installed-packages)
       
       (mapc (lambda (i)
               (unless ,dry
                 (version-supported-if
                     <= 25.0
                     (package-install i t)
                   (package-install i))))
             not-installed-packages)
       not-installed-packages)))


(defmacro parse-package-spec (dir spec)
  "Parse SPEC, install packages and setup."
  `(dolist (s ,spec)
     (when (consp s)
       (let ((p (self-spec-> s :cond))
             (m (self-spec-> s :packages)))
         (when (and p (consp m))
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
         :compile `(,(emacs-home* "config/lisps.el")
                    ))))



;; Load basic package spec
(parse-package-spec v-dir basic-package-spec)


;; Load self packages spec
(self-safe-call*
 "package-spec"
 (when (self-spec->*)
   (parse-package-spec v-dir (self-spec->*))))


