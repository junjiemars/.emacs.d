;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-dired-autoload.el
;;;;

(declare-function on-dired-init! (v-home%> "config/direds"))
(declare-function on-dired-aux-init! (v-home%> "config/direds"))
(declare-function on-arc-mode-init! (v-home%> "config/direds"))
(declare-function browse-file (v-home%> "config/direds"))
(autoload 'on-dired-init! (v-home%> "config/direds"))
(autoload 'on-dired-aux-init! (v-home%> "config/direds"))
(autoload 'on-arc-mode-init! (v-home%> "config/direds"))

;;; `dired' after load
(with-eval-after-load 'dired
  (make-thread* #'on-dired-init!))

;;; `dired-aux' after load
(with-eval-after-load 'dired-aux
  (make-thread* #'on-dired-aux-init!))

;;; `arc-mode' after load
(with-eval-after-load 'arc-mode
  (make-thread* #'on-arc-mode-init!))

;; prefer GNU's ls (--dired option) on Windows or Darwin. on
;; Windows: `dired-mode' does not display executable flag in file
;; mode，see `dired-use-ls-dired' and `ido-dired' for more defails
(when% (executable-find%
        "ls"
        (lambda (bin)
          (let ((home (shell-command* bin (emacs-home*))))
            (zerop (car home)))))
  ;; on Drawin: the builtin ls does not support --dired option
  (setq% dired-use-ls-dired
         (executable-find%
          "ls"
          (lambda (bin)
            (let ((dired (shell-command* bin "--dired")))
              (zerop (car dired)))))
         'dired)
  ;; using `insert-directory-program'
  (setq% ls-lisp-use-insert-directory-program t 'ls-lisp))

;;; autoload
(autoload 'dired-get-file-for-visit "dired")
(autoload 'dired-current-directory "dired")
(autoload 'browse-file (v-home%> "config/direds"))

(define-key% (current-global-map) (kbd "C-x x B") 'browse-file)

;; end of on-dired-autoload.el
