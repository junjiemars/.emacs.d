;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-tramp-autoload.el
;;;;


(defalias 'docker-program
  (lexical-let% ((b (or (executable-find% "podman")
												(executable-find% "docker")
												"docker")))
    (lambda (&optional n)
      (if (null n) b (setq b n))))
  "Program of docker/podman.")


(defun tramp*-docker-ls-containers (&optional ignored)
  "List the running docker/podman containers."
  (ignore* ignored)
  (let ((cmd (shell-command* (docker-program)
							 "ps" "--format {{.Names}}")))
    (when (zerop (car cmd))
      (append '((nil nil))
              (mapcar (lambda (x)
                        (list nil x))
                      (split-string* (cdr cmd) "\n" t "\n"))))))


(defun on-tramp-init! ()
	"On \\=`tramp\\=' initialization."
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
                 (tramp-login-program (docker-program))
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
         '((tramp*-docker-ls-containers "")))))))


;; `tramp' after load
(eval-after-load 'tramp #'on-tramp-init!)


;; end of on-tramp-autoload.el
