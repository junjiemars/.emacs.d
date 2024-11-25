;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; shells.el
;;;;
;; Commentary: shell's environment
;;;;

;;;
;; spec
;;;

(defun shells-spec->* (&optional key)
  "Extract :shell from env-spec via KEY."
  (cond (key (*self-env-spec* :get :shell key))
        (t (*self-env-spec* :get :shell))))

(defmacro shells-spec->% (key)
  (nore-spec-> :shell key))

(defalias '*default-shell-env*
  (lexical-let% ((dx nil))
    (lambda (&optional op k n)
      (cond ((eq :get op) (plist-get dx k))
            ((eq :put! op) (setq dx (plist-put dx k n)))
            ((eq :set! op) (setq dx k))
            (t dx))))
  "Default shell\\='s environment.")

;; end of spec

;;;
;; vars/paths
;;;

(defun echo-var (var &optional options)
  "Return the value of $VAR via echo."
  (and
   (stringp var)
   (let* ((c1 (shell-command* shell-file-name
                (let ((os nil))
                  (dolist* (s options os)
                    (setq os (concat os s " "))))
                (format "-c 'echo $%s'" var)))
          (cmd (if-platform% 'windows-nt
                   (if (string-match "cmdproxy\\.exe$" shell-file-name)
                       (shell-command* (format "echo %%%s%%" var))
                     c1)
                 c1)))
     (and (zerop (car cmd))
          (string-trim> (cdr cmd))))))


(defun paths->var (path &optional predicate)
  "Convert a list of PATH to $PATH like var that separated by
\\=`path-separator\\='."
  (string-trim>
   (let ((vs))
     (dolist* (s path vs)
       (setq vs (concat vs (if (null predicate)
                               (concat s path-separator)
                             (when (and (functionp predicate)
                                        (funcall predicate s))
                               (concat s path-separator)))))))
   path-separator))


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
         (newval (concat name= value)))
    (if (catch 'br
          (while (car env)
            (and (string= name= (car env))
                 (setcar env newval)
                 (throw 'br t))
            (setq env (cdr env))))
        process-environment
      (setq process-environment (cons newval process-environment)))))


(defun copy-env-vars! (env vars)
  (dolist* (v vars)
    (and (> (length v) 0)
         (let ((v1 (cdr (assoc-string v env))))
           (and (stringp v1) (setenv* v v1))))))


(defun spin-env-vars! (vars)
  (dolist* (v vars)
    (and (stringp (car v)) (stringp (cdr v))
         (setenv* (car v) (cdr v)))))


(defmacro copy-exec-path! (path)
  (let ((p (gensym*)))
    `(let ((,p ,path))
       (and ,p (setq exec-path ,p)))))

;; end of env

;;;
;; windows
;;;

(when-platform% 'windows-nt

  (defun windows-nt-env-path+ (dir &optional append)
    "APPEND or push DIR to %PATH%."
    (let ((env (var->paths (getenv (shells-spec->% :PATH)))))
      (when (or (and (null append) (not (string= dir (car env))))
                (and append (not (string= dir (last env)))))
        (let ((path (let ((xs nil))
                      ;; remove dir
                      (dolist* (x env (nreverse xs))
                        (unless (string= x dir)
                          (setq xs (cons x xs)))))))
          (setenv* (shells-spec->% :PATH)
                   (paths->var (if append
                                   (append path dir)
                                 (cons dir path)))))))))

;; end of windows

;;;
;; save/read env
;;;

(defun self-shell-save! ()
  "Save \\=`*default-shell-env*\\=' to file."
  (let ((vars nil)
        (default-directory (emacs-home%)))
    (dolist* (v (shells-spec->* :copy-vars) vars)
      (when (stringp v)
        (let ((val (echo-var v (shells-spec->* :options))))
          (when (and val (> (length val) 0))
            (when (string= v (shells-spec->% :PATH))
              (let ((paths nil))
                (dolist* (p (var->paths val))
                  (let ((p1 (if-platform% 'windows-nt
                                (posix-path p)
                              p)))
                    (when (and (stringp p1) (file-exists-p p1))
                      (when (shells-spec->* :exec-path)
                        (push! p1 exec-path))
                      (append! p1 paths t))))
                (setq val (paths->var paths))))
            (push! (cons v val) vars)))))
    (*default-shell-env* :set! nil)
    (*default-shell-env* :put! :copy-vars vars)
    (when (shells-spec->* :exec-path)
      (let ((paths '()))
        (dolist* (p exec-path)
          (and (and (stringp p) (file-exists-p p))
               (append! p paths t)))
        (append! (v-home% ".exec/") paths t)
        (*default-shell-env* :put! :exec-path paths))))
  (save-sexp-to-file (*default-shell-env*) (shells-spec->% :file)))


(defun self-shell-read! ()
  "Read \\=`*default-shell-env*\\=' from file."
  (cond ((null (shells-spec->* :allowed))
         (append! (v-home% ".exec/") exec-path))
        (t
         ;; read from file
         (*default-shell-env*
          :set!
          (read-sexp-from-file (shells-spec->% :file)))
         ;; `shell-file-name'
         (let ((shell (shells-spec->* :shell-file-name)))
           (when shell
             (setq% explicit-shell-file-name shell 'shell)
             (setq shell-file-name shell)
             (setenv* (shells-spec->% :SHELL) shell)))
         ;; :copy-vars
         (copy-env-vars! (*default-shell-env* :get :copy-vars)
                         (shells-spec->* :copy-vars))
         ;; :exec-path
         (when (shells-spec->* :exec-path)
           (copy-exec-path!
            (*default-shell-env* :get :exec-path)))
         ;; :spin-vars
         (spin-env-vars! (shells-spec->* :spin-vars))))
  (append! #'self-shell-save! kill-emacs-hook))


;; save/read env


(provide 'shells)


;; end of shells.el
