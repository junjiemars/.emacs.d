;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; modules.el
;;;;


;;; require

(declare-function package-installed-p "package")

;; end of require


(defun module-spec->* (&optional key)
  "Extract :module from env-spec via KEY."
  (cond (key (*self-env-spec* :get :module key))
        (t (*self-env-spec* :get :module))))

(defmacro module-unit-spec->* (module key)
  "Extract unit spec from MODULE via KEY."
  `(plist-get ,module ,key))



(defalias '*package-init-repo*
  (let ((b))
    (lambda (&optional n)
      (if n (setq b n) b)))
  "Indicate \\=`package*-init-repo!\\=' whether has been called.")

(defun package*-init-repo! ()
  (setq% package-check-signature
         (module-spec->* :package-check-signature) package)
  (let ((archives (module-spec->* :package-archives)))
    (when archives
      (setq% package-archives archives package)))
  (when-version%
      <= 25.1
    (setq% package-archive-priorities
           `(list ("gnu" . 7)
                  ("melpa-stable" . 5)
                  ("melpa" . 3)
                  ("nongnu" . 1))
           package))
  (package-refresh-contents))

(defun package*-check-name (package)
  "Check PACKAGE is a symbol or a tar file."
  (cond ((null package) nil)
        ((symbolp package) (cons package nil))
        ((and (stringp package)
              (inhibit-file-name-handler (file-exists-p package)))
         (cons (intern (string-match* "\\(.*\\)-[.0-9]+\\'"
                                      (file-name-base* package) 1))
               package))
        (t nil)))

(defun package*-delete! (package)
  "Delete PACKAGE."
  (when (and package (symbolp package))
    (if-version%
        <= 25.0
        (package-delete (cadr (assq package package-alist)) t nil)
      (if-version%
          <= 24.4
          (package-delete (cadr (assq package package-alist)))
        (package-delete
         (symbol-name package)
         (mapconcat
          #'identity
          (let ((xs nil))
            (dolist (x (aref (cdr (assq package package-alist)) 0)
                       (nreverse xs))
              (setq xs (cons (number-to-string x) xs))))
          "."))))))

(defun package*-install! (package &optional tar)
  "Install PACKAGE optional via TAR."
  (if tar
      (package-install-file package)
    (unless (*package-init-repo*)
      (package*-init-repo!)
      (*package-init-repo* t))
    (if-version%
        <= 25.0
        (package-install package t)
      (package-install package))))

(defun package*-parse-spec! (spec &optional remove-unused)
  "Parse SPEC, install, REMOVE-UNUSED packages."
  (dolist (s spec)
    (let ((ss (cdr s)))
      (when (consp ss)
        (cond ((module-unit-spec->* ss :cond)
               (dolist (p (module-unit-spec->* ss :packages))
                 (let ((ns (package*-check-name p))
                       (cs (module-unit-spec->* ss :compile)))
                   (when (consp ns)
                     (let ((n (car ns)) (tar (cdr ns)))
                       (cond ((package-installed-p n) nil)
                             (t (package*-install! (if tar tar n) tar))))
                     (and (consp cs) (apply #'compile! cs))))))
              (remove-unused
               (dolist (p (module-unit-spec->* ss :packages))
                 (let ((ns (package*-check-name p)))
                   (when (consp ns)
                     (let ((n (car ns)))
                       (and (package-installed-p n)
                            (package*-delete! n))))))))))))

(defun self-module-init! ()
  "Initialize :package spec from \\=`*self-env-spec*\\='."
  (when-version%
      <= 25.1
    (setq custom-file (v-home! ".transient/packages.el")))
  ;; define package user dir
  (setq% package-gnupghome-dir (v-home! ".elpa/gnupg/") package)
  (setq% package-user-dir package*-user-dir package)
  ;; load self :packages-spec
  (compile! (compile-unit* (*self-paths* :get :mod-spec)))
  (package-initialize)
  (package*-parse-spec! (*self-mod-spec*) (module-spec->* :remove-unused))
  t)

;; end of package*

(provide 'modules)

;; end of modules.el
