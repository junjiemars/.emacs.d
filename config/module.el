;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; module.el
;;;;


;; define package user dir
(setq% package-user-dir (v-home% "elpa/") 'package)


(defvar *repository-initialized* nil
  "Indicate `initialize-package-repository!' whether has been called.")


(defsubst initialize-package-repository! ()
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
    (unless *repository-initialized*
      (initialize-package-repository!)
      (setq *repository-initialized* t))
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

(defvar *autoload-compile-units* nil
  "Autloaded `compile-unit'.")

(defsubst parse-package-spec! (spec &optional remove-unused)
  "Parse SPEC, install, remove and setup packages."
  (dolist* (s spec)
    (when (consp s)
      (dolist* (p (self-spec-> s :packages))
        (let ((ns (check-package-name p)))
          (when (consp ns)
            (let ((n (car ns)) (tar (cdr ns)))
              (if (package-installed-p n)
                  (when (and remove-unused (not (self-spec-> s :cond)))
                    (delete-package! n))
                (when (self-spec-> s :cond)
                  (install-package! (if tar tar n) tar)))))))
      (when (self-spec-> s :cond)
        (setq *autoload-compile-units*
              (append *autoload-compile-units*
                      (delete nil (self-spec-> s :compile))))))))


(defvar basic-package-spec
  (list (list
         :cond t
         :packages `(aggressive-indent
                     paredit
                     rainbow-delimiters
                     ,(when-version% <= 24.1 'yaml-mode)))))


(defmacro defun-on-module-autoload^ (module &rest body)
  "Define FN threading macro."
  (declare (indent 1))
  (let ((name (intern (format "on-%s-autoload^" module))))
    `(defun ,name ()
       ,@body)))


(package-spec-:allowed-p
  ;; Load basic package spec
  (parse-package-spec! basic-package-spec))


(package-spec-:allowed-p
  ;; Load self packages spec
  (parse-package-spec! (self-spec->*package-spec)
                       (self-spec->*env-spec :package :remove-unused)))
