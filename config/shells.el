;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; shells.el
;;;;


(defmacro shells-spec->% (&rest keys)
  "Extract value from the list of spec via KEYS at compile time."
  (declare (indent 0))
  `(self-spec->%
       (list :file ,(v-home! ".exec/shell-env.el")
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

  "Default shell's environment.")


(defmacro echo-var (var &optional options)
  "Return the value of $VAR via echo."
  `(when (stringp ,var)
     (let* ((cmdproxy (when-platform% 'windows-nt
                        (string-match "cmdproxy\\.exe$"
                                      shell-file-name)))
            (cmd (if cmdproxy
                     (shell-command* (format "echo %%%s%%" ,var))
                   (shell-command* shell-file-name
                     (mapconcat #'identity ,options " ")
                     (format "-c 'echo $%s'" ,var)))))
       (when (zerop (car cmd))
         (string-trim> (cdr cmd))))))


(defmacro paths->var (path &optional predicate)
  "Convert a list of PATH to $PATH like var that separated by
`path-separator'."
  `(string-trim> (apply #'concat
                        (mapcar #'(lambda (s)
                                    (if (null ,predicate)
                                        (concat s path-separator)
                                      (when (and (functionp ,predicate)
                                                 (funcall ,predicate s))
                                        (concat s path-separator))))
                                ,path))
                 path-separator))


(defmacro var->paths (var)
  "Refine VAR like $PATH to list by `path-separator'.
See also: `parse-colon-path'."
  `(when (stringp ,var)
     (split-string* ,var path-separator t "[ ]+\n")))





(defmacro copy-env-vars! (env vars)
  `(dolist* (v ,vars)
     (when (and (stringp v) (not (string= v "")))
       (let ((v1 (cdr (assoc** v ,env #'string=))))
         (when (stringp v1)
           (setenv v v1))))))

(defmacro spin-env-vars! (vars)
  `(dolist* (v ,vars)
     (when (and (stringp (car v))
                (stringp (cdr v)))
       (setenv (car v) (cdr v)))))


(defmacro copy-exec-path! (path)
  `(when ,path (setq exec-path ,path)))




;; Windows ansi-term/shell

(when-platform% 'windows-nt

  (defadvice ansi-term (before ansi-term-before disable)
    (set-window-buffer (selected-window)
                       (make-comint-in-buffer "ansi-term" nil "cmd"))))


(when-platform% 'windows-nt
  (with-eval-after-load 'term
		(ad-enable-advice #'ansi-term 'before "ansi-term-before")
		(ad-activate #'ansi-term t)))


(when-platform% 'windows-nt

  (defun windows-nt-env-path+ (dir &optional append)
    "APPEND or push DIR to %PATH%."
    (let ((env (var->paths (getenv (shells-spec->% :PATH)))))
      (when (or (and (null append) (not (string= dir (car env))))
                (and append (not (string= dir (last env)))))
        (let ((path (remove-if* (lambda (x) (string= x dir))
                                env)))
          (setenv (shells-spec->% :PATH)
                  (paths->var (if append
                                  (append path dir)
                                (cons dir path)))))))))

 ;; end of Windows ansi-term/shell


(defun save-shell-env! ()
  "Save `*default-shell-env*' to file."
  (let ((vars nil))
    (dolist* (v (shells-spec->* :copy-vars) vars)
      (when (stringp v)
        (let ((val (echo-var v (shells-spec->* :options))))
          (when (and val (> (length val) 0))
            (when (string= v (shells-spec->% :PATH))
              (let ((paths))
                (dolist* (p (var->paths val))
                  (when (and (stringp p) (file-exists-p p))
                    (push! p exec-path t t)
                    (push! p paths t t)))
                (push! (v-home% ".exec/") exec-path t t)
                (*default-shell-env* :put! :exec-path exec-path)
                (setq val (paths->var paths))))
            (push! (cons v val) vars)))))
    (*default-shell-env* :put! :copy-vars vars))
  (save-sexp-to-file (*default-shell-env*) (shells-spec->% :file)))


(defun read-shell-env! ()
  "Read `*default-shell-env*' from file."
  (if (not (shells-spec->* :allowed))
      ;; allowed/disallowed `shells-spec->*'
      (push! (v-home% ".exec/") exec-path t)

    ;; read from file
    (when (file-exists-p (shells-spec->% :file))
      (*default-shell-env*
       :set!
       (car (read-from-string (read-str-from-file
                               (shells-spec->% :file))))))

    (let ((shell (shells-spec->* :shell-file-name)))
      (when shell
        (setq% explicit-shell-file-name shell 'shell)
        (setq shell-file-name shell)
        (setenv (shells-spec->% :SHELL) shell)))

    (when (shells-spec->* :exec-path)
      (copy-exec-path! (*default-shell-env* :get :exec-path)))

    (let ((copying (shells-spec->* :copy-vars)))
      (when (consp copying)
        (copy-env-vars! (*default-shell-env* :get :copy-vars)
                        copying)))

    (let ((spinning (shells-spec->* :spin-vars)))
      (when (consp spinning)
        (spin-env-vars! spinning))))

  (add-hook 'kill-emacs-hook #'save-shell-env! t))


(read-shell-env!)


 ;; end of shells.el
