;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-ido-autoload.el
;;;;


;; default view file
(define-key% (current-global-map) (kbd "C-x 5 r") #'view-file-other-frame)
(define-key% (current-global-map) (kbd "C-x 4 r") #'view-file-other-window)
(define-key% (current-global-map) (kbd "C-x C-r") #'view-file)


(with-eval-after-load 'ido

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


  ;; `ido' override default view file keybindings
  (define-key% (current-global-map)
    (kbd "C-x 5 r") #'ido-find-file-read-only-other-frame)
  (define-key% (current-global-map)
    (kbd "C-x 4 r") #'ido-find-file-read-only-other-window)
  (define-key% (current-global-map)
    (kbd "C-x C-r") #'ido-find-file-read-only)

)


 ;; end of file
