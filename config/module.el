;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; module.el
;;;;


;; define package user dir
(setq% package-user-dir (v-home% "elpa/") 'package)


(defalias '*repository-initialized*
  (lexical-let% ((b))
    (lambda (&optional n)
      (if n (setq b n) b)))
  "Indicate `initialize-package-repository!' whether has been called.")

;; (defvar *repository-initialized* nil
;;   "Indicate `initialize-package-repository!' whether has been called.")


(defun initialize-package-repository! ()
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


(defmacro check-package-name (package)
  "Check PACKAGE is a symbol or a tar file."
  (let ((p (gensym*)))
    `(let ((,p ,package))
       (cond ((and (symbolp ,p)
                   (not (null ,p)))
              (cons ,p nil))
             ((and (stringp ,p)
                   (file-exists-p ,p))
              (cons (intern (match-string* "\\(.*\\)-[.0-9]+\\'"
                                           (file-name-base* ,p) 1))
                    ,p))
             (t nil)))))


(defmacro delete-package! (package)
  "Delete PACKAGE."
  (let ((p (gensym*)))
    `(let ((,p ,package))
       (when (and (not (null ,p))
                  (symbolp ,p))
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


(defmacro install-package! (package &optional tar)
  "Install PACKAGE."
  (let ((p (gensym*))
        (f (gensym*)))
    `(let ((,p ,package)
           (,f ,tar))
       (if ,f
           (package-install-file ,p)
         (unless (*repository-initialized*)
           (initialize-package-repository!)
           (*repository-initialized* t))
         (if-version%
             <= 25.0
             (package-install ,p t)
           (package-install ,p))))))


;; Package Initialize
;; (require 'package)
(declare-function package-installed-p "package")
(setq package-enable-at-startup nil)


(when-version%
    <= 25.1
  (setq custom-file (v-home% "config/.selected-packages.el")))

(package-initialize)


(defun parse-package-spec! (spec &optional remove-unused)
  "Parse SPEC, install, remove and setup packages."
  (dolist* (s spec)
    (let ((ss (cdr s)))
      (when (and (consp ss) (self-spec-> ss :cond))
        (dolist* (p (self-spec-> ss :packages))
          (let ((ns (check-package-name p)))
            (when (consp ns)
              (let ((n (car ns)) (tar (cdr ns)))
                (if (package-installed-p n)
                    (when (and remove-unused (not (self-spec-> ss :cond)))
                      (delete-package! n))
                  (install-package! (if tar tar n) tar))))))
        (*package-compile-units* (self-spec-> ss :compile))))))


(defmacro defun-on-module-autoload^ (module &rest body)
  "Define FN threading macro."
  (declare (indent 1))
  (let ((name (intern (format "on-%s-autoload^" module))))
    `(defun ,name ()
       ,@body)))


(*self-packages*
 :put :|basic|
 `(:cond t :packages (paredit
                      rainbow-delimiters)))

(package-spec-:allowed-p
  ;; Load self packages spec
  (parse-package-spec! (*self-packages*)
                       (*self-env-spec* :get :package :remove-unused)))


;;; eof
