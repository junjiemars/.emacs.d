;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; modules.el
;;;;


;; (require 'package)
(declare-function package-installed-p "package")


(defalias '*package-init-repo*
  (lexical-let% ((b))
    (lambda (&optional n)
      "N"
      (if n (setq b n) b)))
  "Indicate \\=`package*-init-repo!\\=' whether has been called.")

(defun package*-init-repo! ()
  (setq% package-check-signature
         (*self-env-spec* :get :package :package-check-signature)
         'package)
  (setq%
   package-archives
   (append (list '("gnu" . "https://elpa.gnu.org/packages/")
                 '("melpa-stable" . "https://stable.melpa.org/packages/"))
           (when-version%
               <= 25.1
             (list '("melpa" . "https://melpa.org/packages/"))))
   'package)

  (when-version%
      <= 25.1
    (setq% package-archive-priorities
           (list '("melpa-stable" . 10)
                 '("melpa" . 5)
                 '("gnu" . 0))
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
        (*package-compile-units* (self-spec-> ss :compile))))))


(defalias '*package-compile-units*
  (lexical-let% ((us '()))
    (lambda (&optional n)
      "N"
      (cond ((consp n) (dolist* (s n us)
                         (setq us (cons s us))))
            (t us))))
  "Autloaded \\=`compile-unit\\='.")


(defun self-package-init! ()
  "Initialize :package spec from \\=`*self-env-spec*\\='."
  (when (*self-env-spec* :get :package :allowed)
    ;; compile self :package-spec
    (compile! (compile-unit* (*self-paths* :get :package-spec)))
    (when-version%
        <= 25.1
      (setq custom-file (v-home! ".transient/packages.el")))
    ;; define package user dir
    (setq% package-gnupghome-dir (v-home! ".elpa/gnupg/") 'package)
    (setq% package-user-dir package*-user-dir 'package)
    (package-initialize)
    ;; load self :packages-spec
    (package*-parse-spec! (*self-packages*)
                          (*self-env-spec* :get :package
                                           :remove-unused))
    (apply #'compile! (*package-compile-units*))))



(provide 'modules)

;; end of modules.el
