;;;; -*- lexical-binding:t -*-
;;;;
;; basic
;;;;



;; versioned dirs: .*
(setq-default recentf-save-file (v-home! ".recentf/" "recentf"))
(setq-default savehist-file (v-home! ".minibuffer/" "history"))
(setq auto-save-list-file-prefix (v-home! ".auto-save/" "saves-"))
(setq-default eshell-directory-name (v-home! ".eshell/"))
(setq-default server-auth-dir (v-home! ".server/"))
(setq-default image-dired-dir (v-home! ".image-dired/"))
(version-supported-when <= 23
  (setq-default tramp-persistency-file-name (v-home! ".tramp/" "tramp")))


;; versioned dirs: load-path
(add-to-list 'load-path (v-home* "config/") t #'string=)
(add-to-list 'load-path (v-home* "private/") t #'string=)



(version-supported-when > 24.4
  (defmacro with-eval-after-load (file &rest body)
    "Execute BODY after FILE is loaded.

FILE is normally a feature name, but it can also be a file name,
in case that file does not provide any feature.  See ‘eval-after-load’
for more details about the different forms of FILE and their semantics."
    `(eval-after-load ,file
       '(progn% ,@body))))





;; Strings

(defmacro string-trim< (s &optional lr)
  "Remove leading whitespace or the matching of LR from S."
  `(let ((r (if ,lr (concat "\\`" ,lr) "\\`[ \t\n\r]+")))
     (if (string-match r ,s)
         (replace-match "" t t ,s)
       ,s)))


(defmacro string-trim>< (s &optional rr lr)
  "Remove leading and trailing whitespace or the matching of LR/RR from S."
  `(let ((s1 (string-trim> ,s ,rr)))
     (string-trim< s1 ,lr)))





;; Compatiable functions


(version-supported-if
    > 25.0
    (defmacro alist-get (key alist &optional default remove)
      "Return the value associated with KEY in ALIST, using `assq'.
If KEY is not found in ALIST, return DEFAULT.

This is a generalized variable suitable for use with `setf'.
When using it to set a value, optional argument REMOVE non-nil
means to remove KEY from ALIST if the new value is `eql' to DEFAULT.

If KEY is not found in ALIST, returns DEFAULT. There're no `alist-get' 
function definition in Emacs25-."
      (ignore remove) ;;Silence byte-compiler.
      `(let ((x (assq ,key ,alist)))
         (if x (cdr x) ,default))))


(version-supported-if
    <= 24.4
    (defalias 'split-string>< 'split-string)
  (defmacro split-string>< (string &optional separators omit-nulls trim)
    "Split STRING into substrings bounded by matches for SEPARATORS, 
like `split-string' Emacs 24.4+"
    `(if ,trim
         (delete ""
                 (mapcar (lambda (s)
                           (if (and (stringp ,trim) (> (length ,trim) 0))
                               (string-trim>< s ,trim ,trim)
                             (string-trim>< s)))
                         (split-string ,string ,separators ,omit-nulls)))
       (split-string ,string ,separators ,omit-nulls))))





;; File functions

(defmacro save-sexp-to-file (sexp file)
  "Save SEXP to the FILE. 

Returns FILE when successed otherwise nil."
  `(progn
     (when (and (save-excursion
                  (let ((sexp-buffer (find-file-noselect ,file)))
                    (set-buffer sexp-buffer)
                    (erase-buffer)
                    (print ,sexp sexp-buffer)
                    (save-buffer)
                    (kill-buffer sexp-buffer)))
                (file-exists-p ,file))
       ,file)))


(defmacro save-str-to-file (str file)
  "Save STR to FILE. 

Returns FILE when successed otherwise nil."
  `(progn
     (with-temp-file ,file (insert ,str))
     (when (file-exists-p ,file)
       ,file)))





;; Clean Emacs' user files

(defun clean-saved-user-files (&optional all)
  "Clean saved user files except current `emacs-version'.

Clean all when ALL is t,
otherwise default to keep the directories of current `emacs-version'."
  (let ((dirs (list `,(emacs-home* ".auto-save/")
                    `,(emacs-home* ".backup/")
                    `,(emacs-home* ".bookmarks/")
                    `,(emacs-home* ".desktop/")
                    `,(emacs-home* ".eshell/")
                    `,(emacs-home* ".ido/")
                    `,(emacs-home* ".image-dired/")
                    `,(emacs-home* ".minibuffer/")
                    `,(emacs-home* ".places/")
                    `,(emacs-home* ".recentf/")
                    `,(emacs-home* ".semanticdb/")
                    `,(emacs-home* ".server/")
                    `,(emacs-home* ".smex/")
                    `,(emacs-home* ".tags/")
                    `,(emacs-home* ".tramp/")
                    `,(emacs-home* ".url/"))))
    (dolist (d dirs)
      (when (file-exists-p d)
        (dolist (f (directory-files d nil "^[gt]_.*$"))
          (when (or all
                    (not (string-match
                          (concat "^[gt]_" emacs-version) f)))
            (message "#Clean saved user file: %s" (concat d f))
            (platform-supported-if windows-nt
                (shell-command (concat "rmdir /Q /S " (concat d f)))
              (shell-command (concat "rm -r " (concat d f))))))))))


(defun reset-emacs ()
  "Clean all compiled file and desktop, then restart Emacs."
  (progn
    (clean-saved-user-files t)
    (clean-compiled-files)
    (setq kill-emacs-hook nil)
    (kill-emacs 0)))






(version-supported-when >= 24.0
  (defalias 'eldoc-mode 'turn-on-eldoc-mode
    "After Emacs 24.0 `turn-on-eldoc-mode' is obsoleted, use `eldoc-mode' indeed.

Unify this name `eldoc-mode' in Emacs 24.0-, 
see `http://www.emacswiki.org/emacs/ElDoc'"))


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



;; on Drawin: ls does not support --dired;
;; see `dired-use-ls-dired' for more defails
(platform-supported-when
    darwin
  (setq-default ls-lisp-use-insert-directory-program nil)
  (require 'ls-lisp))


;; auto-save
(setq auto-save-default nil)


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




;; Platform related functions

(platform-supported-when windows-nt
  (defmacro windows-nt-posix-path (p)
    "Returns the posix path from P which can be recognized on`system-type'."
    `(replace-regexp-in-string "\\\\" "/" ,p)))


(platform-supported-when windows-nt
  (defmacro windows-nt-unix-path (p)
    "Returns the unix path from P which can be recognized by shell on `system-type'"
    `(replace-regexp-in-string
      ";" ":"
      (replace-regexp-in-string "\\([a-zA-Z]\\):/" "/\\1/"
                                (windows-nt-posix-path ,p)))))




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





;; Computations


(safe-fn-when number-sequence (fset 'range 'number-sequence))


;; use `pp' `pp-eval-expression' or `pp-eval-last-sexp'
(safe-fn-when cl-prettyexpand (fset 'pprint 'cl-prettyprint))


(defun take (n seq)
  "Returns a sequence of the first N items in SEQ.

Or all items if SEQ has fewer items than N."
  (let ((acc nil) (n1 n) (s1 seq))
    (while (and (> n1 0) s1)
      (setq acc (cons (car s1) acc)
            n1 (1- n1) s1 (cdr s1)))
    (nreverse acc)))


(defun drop-while (pred seq)
  "Returns a sequence of successive items from SEQ after the item 
for which (PRED item) returns t."
  (let ((s seq) (w nil))
    (while (and (not w) (car s))
      (if (funcall pred (car s))
          (setq w t)
        (setq s (cdr s))))
    (cdr s)))


(defun take-while (pred seq)
  "Returns a sequence of successive items from SEQ before the item 
for which (PRED item) returns t."
  (let ((s seq) (w nil) (s1 nil))
    (while (and (not w) (car s))
      (if (funcall pred (car s))
          (setq w t)
        (setq s1 (cons (car s) s1)
              s (cdr s))))
    (nreverse s1)))


