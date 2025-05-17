;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; shells.el
;;;;
;; features:
;;; 1. copy environment variable to `process-environment'.
;;; 2. spin specified variable to `process-environment'.
;;; 3. duplicate PATH to `exec-path'.
;;;;


;;;
;; spec
;;;

(defun shell-spec->* (&optional spec)
  "Extract :shell from env-spec via SPEC."
  (cond ((and spec (eq spec :file)) (v-home% ".exec/shell-env.el"))
        ((and spec (eq spec :SHELL)) "SHELL")
        ((and spec (eq spec :PATH)) "PATH")
        (spec (*self-env-spec* :get :shell spec))
        (t (*self-env-spec* :get :shell))))

(defalias '*default-shell-env*
  (let ((dx nil))
    (lambda (&optional op k n)
      (cond ((and op (eq :get op)) (plist-get dx k))
            ((and op (eq :put! op)) (setq dx (plist-put dx k n)))
            ((and op (eq :set! op)) (setq dx k))
            (t dx))))
  "Default shell\\='s environment.")

;; end of spec

;;;
;; vars/paths
;;;

(defun echo-var (var)
  "Return the value of $VAR via echo."
  (when (stringp var)
    (let ((rc (if-platform% windows-nt
                  (if (string-match "cmdproxy\\.exe$" shell-file-name)
                      (shell-command* (format "echo %%%s%%" var))
                    ;; non cmdproxy
                    (shell-command* (format "echo $%s 2>/dev/null" var)))
                ;; other platforms
                (shell-command* (format "echo $%s 2>/dev/null" var)))))
      (and (= 0 (car rc))
           (string-trim> (cdr rc))))))

(defun var->paths (var)
  "Refine VAR like $PATH to list by \\=`path-separator\\='.\n
See also: \\=`parse-colon-path\\='."
  (and (stringp var)
       (split-string* var path-separator t "[ ]+\n")))

;; end of vars/paths

;;;
;; env
;;;

(defun setenv* (name value)
  "Change or add an environment variable: NAME=VALUE.\n
See \\=`setenv\\='."
  (let* ((env process-environment)
         (name= (concat name "="))
         (new (concat name= value)))
    (catch :br
      (while (car env)
        (when (string-equal
               name=
               (substring-no-properties (car env) 0 (length name=)))
          (setcar env new)
          (throw :br process-environment))
        (setq env (cdr env)))
      (push! new process-environment))))

(defun copy-env-vars! (env vars)
  (dolist (v vars)
    (and (> (length v) 0)
         (let ((v1 (cdr (assoc-string v env))))
           (and (stringp v1) (setenv* v v1))))))

(defun spin-env-vars! (vars)
  (dolist (v vars)
    (and (stringp (car v)) (stringp (cdr v))
         (setenv* (car v) (cdr v)))))

 ;; end of env

;;;
;; windows
;;;

(when-platform% windows-nt
  (defun paths->var (path &optional predicate)
    "Convert a list of PATH to $PATH like var that separated by
\\=`path-separator\\='."
    (string-trim>
     (let ((vs))
       (dolist (s path vs)
         (setq vs (concat vs (if (null predicate)
                                 (concat s path-separator)
                               (when (and (functionp predicate)
                                          (funcall predicate s))
                                 (concat s path-separator)))))))
     path-separator)))

;; end of windows

;;;
;; save/read env
;;;

(defun self-shell-save! ()
  "Save \\=`*default-shell-env*\\=' to file."
  (let ((vars nil) (paths nil)
        (default-directory (emacs-home%))
        (PATH (shell-spec->* :PATH))
        (exec-path? (shell-spec->* :exec-path)))
    (dolist (v (shell-spec->* :copy-vars))
      (let ((s (echo-var v)))
        (when (and s (> (length s) 0))
          (and (string-equal v PATH)
               (setq s (if-platform% windows-nt (posix-path s) s)))
          (push! (cons v s) vars))))
    (when exec-path?
      (dolist (p (var->paths (echo-var PATH)))
        (let ((p1 (if-platform% windows-nt (posix-path p) p)))
          (when (and (stringp p1) (> (length p1) 0))
            (push! p1 paths delete)))))
    (setq vars (nreverse vars)
          paths (nreverse paths))
    (*default-shell-env* :set! nil)
    (*default-shell-env* :put! :copy-vars vars)
    (push! (v-home% ".exec/") paths delete)
    (append! exec-directory paths delete)
    (*default-shell-env* :put! :exec-path paths))
  (write-file* (*default-shell-env*) (shell-spec->* :file)))


(defun self-shell-read! ()
  "Read \\=`*default-shell-env*\\=' from file."
  (cond ((null (shell-spec->* :allowed))
         (push! (v-home% ".exec/") exec-path))
        (t
         ;; read from file
         (*default-shell-env*
          :set!
          (read-file* (shell-spec->* :file) t))
         ;; `shell-file-name'
         (let ((bin (shell-spec->* :shell-file-name)))
           (when bin
             (setq% explicit-shell-file-name bin shell)
             (setq shell-file-name bin)
             (setenv* (shell-spec->* :SHELL) bin)))
         ;; :copy-vars
         (copy-env-vars! (*default-shell-env* :get :copy-vars)
                         (shell-spec->* :copy-vars))
         ;; :spin-vars
         (spin-env-vars! (shell-spec->* :spin-vars))
         ;; :exec-path
         (when (shell-spec->* :exec-path)
           (let ((path (*default-shell-env* :get :exec-path)))
             (when path
               (setq exec-path path))))))
  (append! #'self-shell-save! kill-emacs-hook))


;; save/read env


(provide 'shells)


;; end of shells.el
