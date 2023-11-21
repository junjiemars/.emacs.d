;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
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


(with-eval-after-load 'tramp

  (when% (executable-find% "ssh")
    ;; ssh faster than scp on ancient Emacs?
    (setq% tramp-default-method "ssh"))

  ;; docker
  ;; C-x C-f /docker:user@container:/path/to/file
  ;; C-x d /docker:user@container:/path/
  (when% (executable-find% "docker")
    (when-var% tramp-methods 'tramp
      (unless (cdr (assoc** "docker" tramp-methods :test #'string=))
        (push! '("docker"
                 (tramp-login-program "docker")
                 (tramp-login-args
                  (nil
                   ("exec" "-it")
                   ("-u" "%u")
                   ("%h")
                   ("sh")))
                 (tramp-remote-shell "/bin/sh")
                 (tramp-remote-shell-args ("-i" "-c")))
               tramp-methods)
        (tramp-set-completion-function
         "docker"
         '((tramp-parse-docker-containers "")))))))


;; end of on-tramp-autoload.el
