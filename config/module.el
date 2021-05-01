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


(defsubst initialize-package-repository! ()
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


(defsubst check-package-name (package)
  "Check PACKAGE is a symbol or a tar file."
  (cond ((and (symbolp package)
              (not (null package)))
         (cons package nil))
        ((and (stringp package)
              (file-exists-p package))
         (cons (intern (match-string* "\\(.*\\)-[.0-9]+\\'"
                                      (file-name-base* package) 1))
               package))
        (t nil)))


(defsubst delete-package! (package)
  "Delete PACKAGE."
  (when (and (not (null package))
             (symbolp package))
    (if-version%
        <= 25.0
        (package-delete (cadr (assq package package-alist)) t nil)
      (if-version%
          <= 24.4
          (package-delete (cadr (assq package package-alist)))
        (package-delete
         (symbol-name package)
         (mapconcat #'identity
                    (mapcar (lambda (x)
                              (number-to-string x))
                            (aref (cdr (assq package package-alist)) 0))
                    "."))))))


(defsubst install-package! (package &optional tar)
  "Install PACKAGE."
  (if tar
      (package-install-file package)
    (unless (*repository-initialized*)
      (initialize-package-repository!)
      (*repository-initialized* t))
    (if-version%
        <= 25.0
        (package-install package t)
      (package-install package))))


;; Package Initialize
;; (require 'package)
(declare-function package-installed-p "package")
(setq package-enable-at-startup nil)


(when-version%
    <= 25.1
  (setq custom-file (v-home% "config/.selected-packages.el")))

(package-initialize)


(defsubst parse-package-spec! (spec &optional remove-unused)
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
                      rainbow-delimiters
                      ,(when-version% <= 24.1 'yaml-mode))))

(package-spec-:allowed-p
  ;; Load self packages spec
  (parse-package-spec! (*self-packages*)
                       (*self-env-spec* :get :package :remove-unused)))


;;; eof
