;;;; -*- lexical-binding:t -*-
;;;;
;; Moduel Management 
;;;;




;; define package user dir

(setq-default package-user-dir (v-home* "elpa/"))



(defvar module-initialized nil
  "Just initialize package repository once.")


(defun initialize-package-repository! ()
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


(defun install-package! (packages &optional dry)
  "Install missing packages, returns alist of installed packages"
  (let ((not-installed-packages
         (delete t (mapcar #'(lambda (p)
                               (if (package-installed-p p) t p))
                           packages))))
    (when not-installed-packages
      (unless dry
        (when (not module-initialized)
          (initialize-package-repository!)
          (setq module-initialized t)))
      (message "#Installing the missing %d packages: %s"
               (length not-installed-packages)
               not-installed-packages)
      
      (mapc (lambda (i)
              (unless dry
                (package-install i)))
            not-installed-packages)
      not-installed-packages)))


(defun parse-package-spec (spec dir)
  "Parse SPEC, install packages and setup."
  (dolist (s spec)
    (let ((pred (plist-get s :cond)))
      (when (and pred
                 (cond
                  ((booleanp pred) pred)
                  ((functionp pred) (funcall pred))))
        (install-package! (plist-get s :packages))
        (let ((setup (plist-get s :setup)))
          (when setup
            (cond
             ((listp setup) (compile-and-load-elisp-files dir setup))
             ((functionp setup) (funcall setup)))))))))



;; Package specs


(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)


(defvar basic-package-spec
  (list (list
         :cond t
         :packages '(aggressive-indent
                     bing-dict
                     ido-ubiquitous
                     markdown-mode
                     paredit
                     rainbow-delimiters
                     smex
                     tagedit)
         :setup `(,(emacs-home* "config/setup-lisp.el")
                  ,(emacs-home* "config/setup-navigation.el")
                  ,(emacs-home* "config/setup-python.el")))))



(version-supported-when
    <= 25.1
  (safe-fn-when package--save-selected-packages
    (defun package--save-selected-packages (&optional _)
      "Fake `package-selected-packages' to nil."
      nil)))



;; Load basic package spec
(parse-package-spec basic-package-spec v-dir)


;; Load self packages spec
(self-safe-call*
 "package-spec"
 (parse-package-spec *val* v-dir))


