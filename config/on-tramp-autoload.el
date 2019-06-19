;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-tramp-autoload.el
;;;;


(defun tramp-parse-docker-containers (&optional ignored)
  "Return a list of running docker container."
  (ignore* ignored)
  (let ((cmd (shell-command* "docker" "ps" "--format {{.Names}}")))
    (when (zerop (car cmd))
      (append '((nil nil))
              (mapcar (lambda (x)
                        (list nil x))
                      (split-string* (cdr cmd) "\n" t "\n"))))))


(when% (and (require 'tramp)
            (not (find "docker" tramp-methods :key #'car :test #'string=)))
  (with-eval-after-load 'tramp
    (add-to-list 'tramp-methods
                 '("docker"
                   (tramp-login-program "docker")
                   (tramp-login-args
                    (nil
                     ("exec" "-it")
                     ("-u" "%u")
                     ("%h")
                     ("sh")))
                   (tramp-remote-shell "/bin/sh")
                   (tramp-remote-shell-args ("-i" "-c")))
                 nil
                 (lambda (a b)
                   (string= (car a) (car b))))
    (tramp-set-completion-function
     "docker"
     '((tramp-parse-docker-containers "")))))


;; end of on-tramp-autoload.el

