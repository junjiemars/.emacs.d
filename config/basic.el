;;;; -*- lexical-binding:t -*-
;;;;
;; Basic
;;;;



(version-supported-when >= 24.0
  (defalias 'eldoc-mode 'turn-on-eldoc-mode
    "After Emacs 24.0 `turn-on-eldoc-mode' is obsoleted, use `eldoc-mode' indeed.

Unify this name `eldoc-mode' in Emacs 24.0-, see `http://www.emacswiki.org/emacs/ElDoc'"))


;; default web browser: eww
(defmacro default-browser-eww ()
  "If `browser-url-default-browser' has not been touched, 
then set `eww' to default browser."
  (when (eq browse-url-browser-function
            'browse-url-default-browser)
    `(safe-fn-when eww-browse-url
       (setq-default url-configuration-directory (v-home! ".url/"))
       (setq browse-url-browser-function 'eww-browse-url))))

(default-browser-eww)

;; open file or url at point
(safe-fn-when find-file-at-point
  (global-set-key (kbd "C-c b") 'find-file-at-point))


;; linum mode
(defmacro linum-mode-supported-p (body)
  "When `emacs-version' supports linum mode then do BODY."
  `(version-supported-when <= 23.1 ,body))


;; Toggle linum mode 
(linum-mode-supported-p
 (defun toggle-linum-mode ()
   "Toggle linum-mode."
   (interactive)
   (defvar linum-mode)
   (if (or (not (boundp 'linum-mode))
           (null linum-mode))
       (linum-mode t)
     (linum-mode -1))))

(linum-mode-supported-p
 (global-set-key (kbd "C-c l") 'toggle-linum-mode))


;; emacs lisp mode
(defun set-emacs-lisp-mode! ()
  "Elisp basic settings."
  (eldoc-mode)
  (version-supported-if
      <= 24.4
      (local-set-key (kbd "TAB") #'completion-at-point)
    (local-set-key (kbd "TAB") #'lisp-complete-symbol))
  (cond
   ((string= "*scratch*" (buffer-name))
    (local-set-key (kbd "RET")
                   (lambda ()
                     (interactive)
                     (eval-print-last-sexp)
                     (newline)))
    (version-supported-when > 24
      (local-set-key (kbd "C-j")
                     (lambda ()
                       (interactive)
                       (newline-and-indent)))))))

;; emacs lisp basic 
(add-hook 'emacs-lisp-mode-hook #'set-emacs-lisp-mode!)


(defun set-ielm-mode! ()
  "IELM basic settings."
  (eldoc-mode))


;; ielm basic
(add-hook 'ielm-mode-hook #'set-ielm-mode!)


(defmacro safe-setq-inferior-lisp-program (lisp &optional force)
  "Safe set inferior-lisp-program var, it must be set before slime start."
  `(if (boundp 'inferior-lisp-program)
       (if ,force
           (setq inferior-lisp-program ,lisp)
         (when (or (not (string= ,lisp inferior-lisp-program))
                   (string= "lisp" inferior-lisp-program))
           (setq inferior-lisp-program ,lisp)))
     (setq-default inferior-lisp-program ,lisp)))


;; Shows a list of buffers
(global-set-key (kbd "C-x C-b") #'ibuffer)




;; on Drawin: ls does not support --dired;
;; see `dired-use-ls-dired' for more defails
(platform-supported-when
    darwin
  (setq-default ls-lisp-use-insert-directory-program nil)
  (require 'ls-lisp))


;; Auto-save
(setq auto-save-default nil)
(setq auto-save-list-file-prefix (v-home! ".auto-save/" "saves-"))


;; Eshell
(setq-default eshell-directory-name (v-home! ".eshell/"))

(defun set-eshell-mode! ()
  (eval-when-compile (require 'em-term))
  (self-safe-call*
   "env-spec"
   (dolist (x (self-spec->* :eshell :visual-commands))
     (add-to-list 'eshell-visual-commands x t #'string=))
   (safe-setq eshell-destroy-buffer-when-process-dies
              (self-spec->* :eshell :destroy-buffer-when-process-dies))
   (safe-setq eshell-visual-subcommands
              (self-spec->* :eshell :visual-subcommands))
   (safe-setq eshell-visual-options
              (self-spec->* :eshell :visual-options))))

(self-safe-call*
 "env-spec"
 (when (self-spec->* :eshell :allowed)
   (defadvice eshell (after eshell-after compile)
     (set-eshell-mode!))))


(safe-fn-unless directory-name-p
  (defun directory-name-p (name)
    "Return t if NAME ends with a directory separator character."
    (let ((len (length name))
          (lastc ?.))
      (if (> len 0)
          (setq lastc (aref name (1- len))))
      (or (= lastc ?/)
          (and (memq system-type '(windows-nt ms-dos))
               (= lastc ?\\))))))


(defun dir-iterate (dir ff df fn)
  "Iterating DIR, if FF file-fitler return t then call FN, 
and if DF dir-filter return t then iterate into deeper DIR.

   (defun FILE-FILTER (file-name absolute-name))
   (defun DIR-FILTER (dir-name absolute-name))"
  (dolist (f (file-name-all-completions "" dir))
    (unless (member f '("./" "../"))
      (let ((a (expand-file-name f dir)))
        (if (directory-name-p f)
            (when (and df (funcall df f a)
                       (dir-iterate a ff df fn)))
          (when (and ff (funcall ff f a)
                     (funcall fn a))))))))


;; Socks

(defun start-socks! (&optional port server version)
  "Switch on `url-gateway-method' to socks, you can start a ssh proxy before 
call it: ssh -vnNTD32000 <user>@<host>"
  (version-supported-when < 22
    (eval-when-compile (require 'url))
    (setq-default url-gateway-method 'socks)
    (setq-default socks-server
                  (list "Default server"
                        (if server server "127.0.0.1")
                        (if port port 32000)
                        (if version version 5)))))


;; Load socks settings
(self-safe-call*
 "env-spec"
   (when (self-spec->* :socks :allowed)
     (start-socks! (self-spec->* :socks :port)
                   (self-spec->* :socks :server)
                   (self-spec->* :socks :version))))


(defun stop-socks! (&optional method)
  "Switch off url-gateway to native."
  (version-supported-when < 22
    (eval-when-compile (require 'url))
    (setq-default url-gateway-method
                  (if method method 'native))))

