;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; direds-ls.el
;;;;

(defun dired*-use-ls-dired ()
  ;; prefer GNU's ls (--dired option) on Windows or Darwin. on
  ;; Windows: `dired-mode' does not display executable flag in file
  ;; mode，see `dired-use-ls-dired' and `ido-dired' for more defails
  ;; on Drawin: the builtin `ls' does not support --dired option
  (when-var% dired-use-ls-dired dired
    (if% (executable-find* "ls")
        (if% (= 0 (car (shell-command* "ls" "--dired")))
            (set-default 'dired-use-ls-dired t)
          (set-default 'dired-use-ls-dired nil)
          (set-default 'ls-lisp-use-insert-directory-program t))
      (set-default 'dired-use-ls-dired nil)
      (set-default 'ls-lisp-use-insert-directory-program nil))))

(defun on-direds-ls-init! ()
  (if-version%
      <= 28.1
      (dired*-use-ls-dired)
    (add-hook 'dired-load-hook #'dired*-use-ls-dired)))

;;

(provide 'direds-ls)

;; end of direds-ls.el
