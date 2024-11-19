;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; tramps.el
;;;;


(defalias 'docker-program
  (lexical-let% ((b (or (executable-find% "podman")
                        (executable-find% "docker")
                        "docker")))
    (lambda (&optional n)
      (if (null n) b (setq b n))))
  "Program of docker/podman.")

(when-fn% 'tramp-set-completion-function 'tramp
  (defun tramp*-parse-docker-containers (program)
    "Return a list name of running docker/podman containers."
    (let ((cmd (shell-command* program "ps" "--format {{.Names}}")))
      (when (zerop (car cmd))
        (let ((xs nil) (ss (split-string* (cdr cmd) "\n" t "\n")))
          (dolist* (x ss (nreverse xs))
            (setq xs (cons (list nil x) xs))))))))


(defun on-tramp-init! ()
  "On \\=`tramp\\=' initialization."
  (when% (executable-find% "ssh")
    ;; ssh faster than scp on ancient Emacs?
    (setq% tramp-default-method "ssh"))
  ;;; docker
  ;; `C-x C-f' /docker:[<user>@]<container>:/path/to/file
  ;; `C-x d' /docker:[<user>@]<container>:/path/
  (when-var% tramp-methods 'tramp
    (unless% (catch 'br
               (dolist* (x tramp-methods)
                 (let ((x1 (car x)))
                   (and (or (string= "docker" x1) (string= "podman" x1))
                        (throw 'br t)))))
      (setq tramp-methods
            ;; podman is compatible with docker
            (let ((ts (let ((xs nil))
                        (dolist* (x tramp-methods (nreverse xs))
                          (let ((x1 (car x)))
                            (unless (string= "docker" x1)
                              (setq xs (cons x xs))))))))
              (push! `("docker"
                       (tramp-login-program ,(docker-program))
                       (tramp-login-args
                        (nil
                         ("exec" "-it")
                         ("-u" "%u")
                         ("%h")
                         ("sh")))
                       (tramp-remote-shell "/bin/sh")
                       (tramp-remote-shell-login ("-l"))
                       (tramp-remote-shell-args ("-i" "-c")))
                     ts t)))
      ;; completion for docker container
      (tramp-set-completion-function
       "docker"
       `((tramp*-parse-docker-containers ,(docker-program)))))))



(provide 'tramps)

;; end of tramps.el
