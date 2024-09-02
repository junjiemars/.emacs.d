;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; modules.el
;;;;

;; (require 'package)

(declare-function package-installed-p "package")

;; end of require

(defalias '*package-init-repo*
  (lexical-let% ((b))
    (lambda (&optional n)
      (if n (setq b n) b)))
  "Indicate \\=`package*-init-repo!\\=' whether has been called.")

(defun package*-init-repo! ()
  (setq% package-check-signature
         (*self-env-spec* :get :module :package-check-signature)
         'package)
  (let ((archives (*self-env-spec* :get :module :package-archives)))
    (when archives
      (setq% package-archives archives 'package)))
  (when-version%
      <= 25.1
    (setq% package-archive-priorities
           `(list ("gnu" . 7)
                  ("melpa-stable" . 5)
                  ("melpa" . 3)
                  ("nongnu" . 1))
           'package))
  (package-refresh-contents))

(defmacro package*-check-name (package)
  "Check PACKAGE is a symbol or a tar file."
  (let ((p (gensym*)))
    `(let ((,p ,package))
       (cond ((and (symbolp ,p) ,p) (cons ,p nil))
             ((and (stringp ,p) (inhibit-file-name-handler
                                  (file-exists-p ,p)))
              (cons (intern (string-match* "\\(.*\\)-[.0-9]+\\'"
                                           (file-name-base* ,p) 1))
                    ,p))
             (t nil)))))

(defmacro package*-delete! (package)
  "Delete PACKAGE."
  (let ((p (gensym*)))
    `(let ((,p ,package))
       (when (and (symbolp ,p) ,p)
         (if-version%
             <= 25.0
             (package-delete (cadr (assq ,p package-alist)) t nil)
           (if-version%
               <= 24.4
               (package-delete (cadr (assq ,p package-alist)))
             (package-delete
              (symbol-name ,p)
              (mapconcat #'identity
                         (mapcar (lambda (x)
                                   (number-to-string x))
                                 (aref (cdr (assq ,p package-alist)) 0))
                         "."))))))))

(defmacro package*-install! (package &optional tar)
  "Install PACKAGE optional via TAR."
  (let ((p (gensym*))
        (f (gensym*)))
    `(let ((,p ,package)
           (,f ,tar))
       (if ,f
           (package-install-file ,p)
         (unless (*package-init-repo*)
           (package*-init-repo!)
           (*package-init-repo* t))
         (if-version%
             <= 25.0
             (package-install ,p t)
           (package-install ,p))))))

(defun package*-parse-spec! (spec &optional remove-unused)
  "Parse SPEC, install, REMOVE-UNUSED packages."
  (dolist* (s spec)
    (let ((ss (cdr s)))
      (when (and (consp ss) (self-spec-> ss :cond))
        (dolist* (p (self-spec-> ss :packages))
          (let ((ns (package*-check-name p)))
            (when (consp ns)
              (let ((n (car ns)) (tar (cdr ns)))
                (if (package-installed-p n)
                    (when (and remove-unused
                               (null (self-spec-> ss :cond)))
                      (package*-delete! n))
                  (package*-install! (if tar tar n) tar))))))
        (apply #'compile! (self-spec-> ss :compile))))))

(defun self-module-init! ()
  "Initialize :package spec from \\=`*self-env-spec*\\='."
  (inhibit-gc
    (when-version%
        <= 25.1
      (setq custom-file (v-home! ".transient/packages.el")))
    ;; define package user dir
    (setq% package-gnupghome-dir (v-home! ".elpa/gnupg/") 'package)
    (setq% package-user-dir package*-user-dir 'package)
    ;; load self :packages-spec
    (inhibit-gc
      (compile! (compile-unit* (*self-paths* :get :mod-spec)))
      (package-initialize)
      (package*-parse-spec!
       (*self-mod-spec*)
       (*self-env-spec* :get :module :remove-unused)))))

;; end of package*

(provide 'modules)

;; end of modules.el
