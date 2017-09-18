;;;; -*- lexical-binding:t -*-
;;;;
;; Basic
;;;;




(defmacro enable-eldoc-mode ()
  "After Emacs 24.0 `turn-on-eldoc-mode' is obsoleted, use `eldoc-mode' indeed.
  `eldoc-mode' shows documentation in the minibuffer when writing code.
  http://www.emacswiki.org/emacs/ElDoc"
  `(version-supported-if < 24.0
                         (eldoc-mode)
     (turn-on-eldoc-mode)))


(defmacro linum-mode-supported-p ()
  "Check Emacs version supports linum mode. "
  `(version-supported-when <= 23.1 t))


;; Default web browser: eww
;; open file or url at point
(when (eq browse-url-browser-function
          'browse-url-default-browser)
  (safe-fn-when eww-browse-url
    (setq-default url-configuration-directory (v-home! ".url/"))
    (setq browse-url-browser-function 'eww-browse-url)))
(safe-fn-when find-file-at-point
  (global-set-key (kbd "C-c b") 'find-file-at-point))


;; Toggle linum mode 
(when (linum-mode-supported-p)
  (defun toggle-linum-mode ()
    "Toggle linum-mode."
    (interactive)
    (if (or (not (boundp 'linum-mode))
            (null linum-mode))
        (linum-mode t)
      (linum-mode -1)))
  (global-set-key (kbd "C-c l") 'toggle-linum-mode))




(defun set-emacs-lisp-mode! ()
  "Elisp basic settings."
  (enable-eldoc-mode)
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
  (enable-eldoc-mode))


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


(safe-fn-unless directory-name-p
  (defun directory-name-p (name)
    "Return non-nil if NAME ends with a directory separator character."
    (let ((len (length name))
          (lastc ?.))
      (if (> len 0)
          (setq lastc (aref name (1- len))))
      (or (= lastc ?/)
          (and (memq system-type '(windows-nt ms-dos))
               (= lastc ?\\))))))


(defun dir-iterate (dir ff df fn)
  "Iterating DIR, if FILE-FILTER return T then call FN, 
and if DIR-FILTER return T then iterate into deeper DIR.

   (defun FILE-FILTER (file-name absolute-name))
   (defun DIR-FILTER (dir-name absolute-name))

\(FN DIR FILE-FILTER DIR-FILTER FN\)"
  (dolist (f (file-name-all-completions "" dir))
    (unless (member f '("./" "../"))
      (let ((a (expand-file-name f dir)))
        (if (directory-name-p f)
            (when (and df (funcall df f a)
                       (dir-iterate a ff df fn)))
          (when (and ff (funcall ff f a)
                     (funcall fn a))))))))


(defun save-string-to-file (string file)
  "Save STRING to FILE.

\(FN STRING FILE\)"
  (with-temp-file file
    (insert string)))


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
 (let ((socks (plist-get *val* :socks)))
   (when (and socks (plist-get socks :allowed))
     (start-socks! (plist-get socks :port)
                   (plist-get socks :server)
                   (plist-get socks :version)))))


(defun stop-socks! (&optional method)
  "Switch off url-gateway to native."
  (version-supported-when < 22
    (eval-when-compile (require 'url))
    (setq-default url-gateway-method
                  (if method method 'native))))

