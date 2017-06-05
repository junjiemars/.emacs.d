;;;;
;; Package Management
;;;;


(package-supported-p

  (require 'package)
  (package-initialize)


  ;; define package user dir
  (setq-default package-user-dir (make-vdir "elpa/"))
  
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


  (defun install-packages (packages &optional dry)
    "Install missing packages, returns alist of installed packages"
    (package-supported-p
      (let ((not-installed-packages
             (delete t (mapcar #'(lambda (p)
                                   (if (package-installed-p p) t p))
                               packages))))
        (when not-installed-packages
          (unless dry (package-refresh-contents))
          (message "#Installing the missing %d packages: %s"
                   (length not-installed-packages)
                   not-installed-packages)
          (mapc (lambda (i) (unless dry (package-install i)))
                not-installed-packages)
          not-installed-packages))))


  (defmacro parse-package-spec (spec)
    "Returns a list of `:packages' and `:setup' from the SPEC."
    `(let ((packages nil)
           (files nil))
       (dolist (s ,spec)
         (when (and (plist-get s :cond)
                    (funcall (plist-get s :cond)))
           (setq packages (append packages (plist-get s :packages)))
           (when (plist-get s :setup)
             (setq files (append files (plist-get s :setup))))))
       (list :packages packages :setup files)))


  ;; Install basic packages
  (defvar basic-packages '(aggressive-indent
                           bing-dict
                           ido-ubiquitous
                           markdown-mode
                           paredit
                           rainbow-delimiters
                           smex
                           tagedit))

  
  (defvar basic-setup-files '("setup-lisp.el"
                              "setup-navigation.el"
                              "setup-python.el"))


  (install-packages basic-packages)
  (compile-and-load-elisp-files basic-setup-files "config/")


  )

