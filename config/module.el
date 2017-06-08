;;;;
;; Package Management
;;;;




;; define package user dir

(setq-default package-user-dir (vdir* "elpa/"))



;; define package repositories

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



(defun install-package! (packages &optional dry)
  "Install missing packages, returns alist of installed packages"
  (let ((not-installed-packages
         (delete t (mapcar #'(lambda (p)
                               (if (package-installed-p p) t p))
                           packages))))
    (when not-installed-packages
      (unless dry
        (package-refresh-contents))
      (message "#Installing the missing %d packages: %s"
               (length not-installed-packages)
               not-installed-packages)
      
      (mapc (lambda (i)
              (unless dry
                (package-install i)))
            not-installed-packages)
      not-installed-packages)))


(defun parse-package-spec (spec)
  "Returns a list of `:packages' and `:setup' from the SPEC."
  (let ((packages nil)
        (files nil))
    (dolist (s spec)
      (when (or (plist-get s :cond)
                 (funcall (plist-get s :cond)))
        (setq packages (append packages (plist-get s :packages)))
        (when (plist-get s :setup)
          (setq files (append files (plist-get s :setup))))))
    (list :packages packages :setup files)))



;; Install basic packages


(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)


(defvar basic-package-spec (list (list
                                  :cond t
                                  :packages '(aggressive-indent
                                              bing-dict
                                              ido-ubiquitous
                                              markdown-mode
                                              paredit
                                              rainbow-delimiters
                                              smex
                                              tagedit)
                                  :setup '("setup-lisp.el"
                                           "setup-navigation.el"
                                           "setup-python.el"))))



(version-supported-when
    <= 25.1
  (safe-setq% 'package-selected-packages nil))


(let ((spec (parse-package-spec basic-package-spec)))
  (version-supported-when
      <= 25.1
    (safe-setq% 'package-selected-packages
                (plist-get spec :packages)))
  
  (install-package! (plist-get spec :packages))
  (compile-and-load-elisp-files (plist-get spec :setup) "config/"))



;; Install self packages


(self-safe-call*
 "package-spec"
 (let ((spec (parse-package-spec _val_)))
   (version-supported-when
       <= 25.1
     (safe-setq% 'package-selected-packages
                 (append package-selected-packages
                         (plist-get spec :packages))))
   (install-package! (plist-get spec :packages))
   (compile-and-load-elisp-files (plist-get spec :setup) "config/")))


