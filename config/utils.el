;;;; -*- lexical-binding:t -*-
;;;;
;; Utils
;;;;



(defun take (n seq)
  "Returns a sequence of the first n itmes in seq, or all items if
   there are fewer than n."
  (let ((acc nil) (n1 n) (s1 seq))
    (while (and (> n1 0) s1)
      (setq acc (cons (car s1) acc)
            n1 (1- n1) s1 (cdr s1)))
    (nreverse acc)))


(defun drop-while (pred seq)
  (let ((s seq)
        (w nil))
    (while (and (not w) (car s))
      (if (funcall pred (car s))
          (setq w t)
        (setq s (cdr s))))
    (cdr s)))


(defun take-while (pred seq)
  (let ((s seq)
        (w nil)
        (s1 nil))
    (while (and (not w) (car s))
      (if (funcall pred (car s))
          (setq w t)
        (setq s1 (cons (car s) s1)
              s (cdr s))))
    (nreverse s1)))


(defmacro string-trim< (s &optional lr)
  "Remove leading whitespace or the matching of LR from S.

\(fn STRING &optional LEFT-REGEXP)"
  `(let ((r (if ,lr (concat "\\`" ,lr) "\\`[ \t\n\r]+")))
     (if (string-match r ,s)
         (replace-match "" t t ,s)
       ,s)))


(defmacro string-trim>< (s &optional rr lr)
  "Remove leading and trailing whitespace or the matching of LR/RR 
from STRING.

\(fn STRING &optional LEFT-REGEXP RIGHT-REGEXP)"
  `(let ((s1 (string-trim> ,s ,rr)))
     (string-trim< s1 ,lr)))


(defmacro save-sexp-to-file (sexp file)
  "Save SEXP to the FILE. Returns FILE when successed otherwise nil."
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




;; compatiable functions

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


(defmacro alist-get-> (key alist &optional default)
  "Return the value associated with KEY in ALIST, using `assq'.
If KEY is not found in ALIST, return DEFAULT. There're no `alist-get' 
function definition in Emacs25-."
  `(let ((x (assq ,key ,alist)))
     (if x (cdr x) ,default)))


(safe-fn-when number-sequence (fset 'range 'number-sequence))


;; use `pp' `pp-eval-expression' or `pp-eval-last-sexp'
(safe-fn-when cl-prettyexpand (fset 'pprint 'cl-prettyprint))





;; platform related functions

(platform-supported-when windows-nt
  (defmacro windows-nt-posix-path (p)
    "Return the posix path that windows-nt can recoganized."
    `(replace-regexp-in-string "\\\\" "/" ,p)))


(platform-supported-when windows-nt
  (defmacro windows-nt-unix-path (p)
    "Return the unix path that shell can regcoganized on windows-nt."
    `(replace-regexp-in-string
      ";" ":"
      (replace-regexp-in-string "\\([a-zA-Z]\\):/" "/\\1/"
                                (windows-nt-posix-path ,p)))))


;; Clean Emacs' user files

(defun clean-saved-user-files (&optional all)
  "Clean saved user files but current version, except ALL is t,
otherwise default to keep the directories of current version."
  (let ((dirs (list `,(emacs-home* ".auto-save/")
                    `,(emacs-home* ".backup/")
                    `,(emacs-home* ".bookmarks/")
                    `,(emacs-home* ".desktop/")
                    `,(emacs-home* ".eshell/")
                    `,(emacs-home* ".ido/")
                    `,(emacs-home* ".minibuffer/")
                    `,(emacs-home* ".recentf/")
                    `,(emacs-home* ".tags/")
                    `,(emacs-home* ".places/")
                    `,(emacs-home* ".semanticdb/")
                    `,(emacs-home* ".smex/")
                    `,(emacs-home* ".url/"))))
    (dolist (d dirs)
      (when (file-exists-p d)
        (dolist (f (directory-files d nil "^[gt]_.*$"))
          (when (or all
                    (not (string-match-p
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
