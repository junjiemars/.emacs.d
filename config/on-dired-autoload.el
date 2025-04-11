;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-dired-autoload.el
;;;;

(autoload 'on-dired-init! (v-home%> "config/direds"))
(autoload 'on-dired-aux-init! (v-home%> "config/direds"))
(autoload 'on-arc-mode-init! (v-home%> "config/direds"))

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

(if-version%
    <= 28.1
    (dired*-use-ls-dired)
  (add-hook 'dired-load-hook #'dired*-use-ls-dired))


;;; `dired' after load
(with-eval-after-load 'dired
  (make-thread* #'on-dired-init!))

;;; `dired-aux' after load
(with-eval-after-load 'dired-aux
  (make-thread* #'on-dired-aux-init!))

;;; `arc-mode' after load
(with-eval-after-load 'arc-mode
  (make-thread* #'on-arc-mode-init!))

;; end of on-dired-autoload.el
