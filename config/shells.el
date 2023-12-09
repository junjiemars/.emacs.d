;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; shells.el
;;;;
;; Commentary: shell's environment
;;;;


(defmacro shells-spec->% (&rest keys)
  "Extract value from the list of spec via KEYS at compile time."
  (declare (indent 0))
  `(self-spec->%
    (list :file ,(v-home% ".exec/shell-env.el")
          :SHELL "SHELL"
          :PATH "PATH")
    ,@keys))


(defmacro shells-spec->* (&rest keys)
  "Extract value from the list of :shell spec via KEYS at runtime."
  (declare (indent 0))
  `(*self-env-spec* :get :shell ,@keys))


(defalias '*default-shell-env*
  (lexical-let% ((dx))
    (lambda (&optional op k n)
      (cond ((eq :get op) (plist-get dx k))
            ((eq :put! op) (setq dx (plist-put dx k n)))
            ((eq :set! op) (setq dx k))
            (t dx))))
  "Default shell\\='s environment.")

;; end of spec


(defmacro echo-var (var &optional options)
  "Return the value of $VAR via echo."
  (let ((v (gensym))
        (o (gensym)))
    `(let ((,v ,var)
           (,o ,options))
       (when (stringp ,v)
         (let* ((c1 (shell-command* shell-file-name
                      (mapconcat #'identity ,o " ")
                      (format "-c 'echo $%s'" ,v)))
                (cmd (if-platform% 'windows-nt
                         (if (string-match "cmdproxy\\.exe$" shell-file-name)
                             (shell-command* (format "echo %%%s%%" ,v))
                           c1)
                       c1)))
           (when (zerop (car cmd))
             (string-trim> (cdr cmd))))))))


(defmacro paths->var (path &optional predicate)
  "Convert a list of PATH to $PATH like var that separated by
\\=`path-separator\\='."
  (let ((p (gensym))
        (c (gensym)))
    `(let ((,p ,path)
           (,c ,predicate))
       (string-trim>
        (apply #'concat
               (mapcar #'(lambda (s)
                           (if (null ,c)
                               (concat s path-separator)
                             (when (and (functionp ,c)
                                        (funcall ,c s))
                               (concat s path-separator))))
                       ,p))
        path-separator))))


(defmacro var->paths (var)
  "Refine VAR like $PATH to list by \\=`path-separator\\='.\n
See also: \\=`parse-colon-path\\='."
  (let ((v (gensym)))
    `(let ((,v ,var))
       (when (stringp ,v)
         (split-string* ,v path-separator t "[ ]+\n")))))

;; end of vars/paths


(defun setenv* (name value)
  "Change or add an environment variable: NAME=VALUE.\n
See \\=`setenv\\='."
  (lexical-let*%
      ((env process-environment)
       (name= (concat name "="))
       (len (length name=))
       (newval (concat name= value)))
    (if (catch 'break
          (while (car env)
            (when (eq t (compare-strings name= 0 len (car env) 0 len))
              (setcar env newval)
              (throw 'break t))
            (setq env (cdr env))))
        process-environment
      (setq process-environment (cons newval process-environment)))))


(defmacro copy-env-vars! (env vars)
  (let ((e (gensym)) (vs (gensym)))
    `(let ((,e ,env) (,vs ,vars))
       (dolist* (v ,vs)
         (when (> (length v) 0)
           (let ((v1 (cdr (assoc-string v ,e))))
             (when (stringp v1)
               (setenv* v v1))))))))


(defmacro spin-env-vars! (vars)
  (let ((vs (gensym)))
    `(let ((,vs ,vars))
       (dolist* (v ,vs)
         (when (and (stringp (car v))
                    (stringp (cdr v)))
           (setenv* (car v) (cdr v)))))))


(defmacro copy-exec-path! (path)
  (let ((p (gensym)))
    `(let ((,p ,path))
       (when ,p (setq exec-path ,p)))))

;; end of env

;;;
;; windows shell
;;;

(when-platform% 'windows-nt

  (defun windows-nt-env-path+ (dir &optional append)
    "APPEND or push DIR to %PATH%."
    (let ((env (var->paths (getenv (shells-spec->% :PATH)))))
      (when (or (and (null append) (not (string= dir (car env))))
                (and append (not (string= dir (last env)))))
        (let ((path (remove-if* (lambda (x) (string= x dir))
                                env)))
          (setenv* (shells-spec->% :PATH)
                   (paths->var (if append
                                   (append path dir)
                                 (cons dir path)))))))))

;; end of windows shell


(defun save-shell-env! ()
  "Save \\=`*default-shell-env*\\=' to file."
  (let ((vars nil))
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
          (when (and (stringp p) (file-exists-p p))
            (append! p paths t)))
        (append! (v-home% ".exec/") paths t)
        (*default-shell-env* :put! :exec-path paths))))
  (save-sexp-to-file (*default-shell-env*) (shells-spec->% :file)))


(defun read-shell-env! ()
  "Read \\=`*default-shell-env*\\=' from file."
  (let ((spec (shells-spec->*)))
    (if (null (self-spec-> spec :allowed))
        ;; allowed/disallowed `shells-spec->*'
        (append! (v-home% ".exec/") exec-path)

      ;; read from file
  		(let ((env (read-sexp-from-file (shells-spec->% :file))))
  			(when env
  				(*default-shell-env* :set!)))

      (let ((shell (self-spec-> spec :shell-file-name)))
        (when shell
          (setq% explicit-shell-file-name shell 'shell)
          (setq shell-file-name shell)
          (setenv* (shells-spec->% :SHELL) shell)))

      (let ((copying (self-spec-> spec :copy-vars)))
        (when (consp copying)
          (copy-env-vars!
  				 (*default-shell-env* :get :copy-vars) copying)))

      (when (self-spec-> spec :exec-path)
        (copy-exec-path!
  			 (*default-shell-env* :get :exec-path)))

      (let ((spinning (self-spec-> spec :spin-vars)))
        (when (consp spinning)
          (spin-env-vars! spinning)))))

  (append! #'save-shell-env! kill-emacs-hook))


(read-shell-env!)


(provide 'shells)


;; end of shells.el
