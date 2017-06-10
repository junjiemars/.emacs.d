;;;; -*- lexical-binding:t -*-
;;;;
;; Basic
;;;;




(defmacro enable-eldoc-mode ()
  "After Emacs 24.4 `turn-on-eldoc-mode is obsoleted, use `eldoc-mode indeed.
  `eldoc-mode shows documentation in the minibuffer when writing code.
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


;; ido-mode allows you to more easily navigate choices. For example,
;; when you want to switch buffers, ido presents you with a list
;; of buffers in the the mini-buffer. As you start to type a buffer's
;; name, ido will narrow down the list of buffers to match the text
;; you've typed in
;; http://www.emacswiki.org/emacs/InteractivelyDoThings
(ido-mode t)


;; Where to save ido.last
(setq-default ido-save-directory-list-file (v-home! ".ido/" "ido.last"))

;; This allows partial matches, e.g. "tl" will match "Tyrion Lannister"
(safe-setq ido-enable-flex-matching t)

;; Turn this behavior off because it's annoying
(safe-setq ido-use-filename-at-point nil)

;; Don't try to match file across all "work" directories; only match files
;; in the current directory displayed in the minibuffer
(safe-setq ido-auto-merge-work-directories-length -1)

;; Includes buffer names of recently open files, even if they're not
;; open now
(safe-setq ido-use-virtual-buffers t)

;; on Drawin: ls does not support --dired;
;; see `dired-use-ls-dired' for more defails
(platform-supported-when
    darwin
  (setq-default ls-lisp-use-insert-directory-program nil)
  (require 'ls-lisp))


;; Auto-save
(setq auto-save-default nil)
(setq auto-save-list-file-prefix (v-home! ".auto-save/" "saves-"))


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
and if DIR-FILTER return T then iterate into deep DIR.

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
