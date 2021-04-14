;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; shells.el
;;;;


(defmacro shells-spec->% (&rest keys)
  "Extract value from the list of spec via KEYS at compile time."
  (declare (indent 0))
  `(self-spec->%
       (list :source-file ,(v-home! ".exec/.shell-env.el")
             :compiled-file ,(v-home! ".exec/.shell-env.elc")
             :SHELL "SHELL"
             :PATH "PATH")
     ,@keys))


(defmacro shells-spec->* (&rest keys)
  "Extract value from the list of :shell spec via KEYS at runtime."
  (declare (indent 0))
   `(*self-env-spec* :get :shell ,@keys))


(defvar *default-shell-env*
  (list :exec-path nil
        :copy-vars nil)
  "Default shell environments,
get via `(shell-env-> k)' and put via `(shell-env<- k v)'")


(defmacro shell-env-> (&optional k)
  "Extract the value from `*default-shell-env*' via K."
  `(if ,k
       (plist-get *default-shell-env* ,k)
     *default-shell-env*))

(defmacro shell-env<- (k v)
  "Put K and V into `*default-shell-env*'."
  `(plist-put *default-shell-env* ,k ,v))


(defmacro echo-var (var &optional options)
  "Return the value of $VAR via echo."
  `(when (stringp ,var)
     (let ((cmd (shell-command*
                    (shell-quote-argument (getenv "SHELL"))
                  (mapconcat #'identity ,options " ")
                  (format "-c 'echo $%s'" ,var))))
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



(defun save-shell-env! ()
  (shell-env<-
   :copy-vars
   (let ((vars nil))
     (dolist* (v (shells-spec->* :copy-vars) vars)
       (when (stringp v)
         (let ((val (echo-var v (shells-spec->* :options))))
           (when (and val (> (length val) 0))
             (when (string= v (shells-spec->% :PATH))
               (let ((paths))
                 (dolist* (p (var->paths val))
                   (when (and (stringp p)
                              (file-exists-p p))
                     (add-to-list 'exec-path p t #'string=)
                     (setq paths (cons p paths))))
                 (add-to-list 'exec-path (v-home% ".exec/") t)
                 (shell-env<- :exec-path exec-path)
                 (setq val (paths->var (reverse paths)))))
             (push (cons v val) vars)))))))
  (when (save-sexp-to-file
         (list 'setq '*default-shell-env*
               (list 'list
                     ':exec-path (list 'quote (shell-env-> :exec-path))
                     ':copy-vars (list 'quote (shell-env-> :copy-vars))))
         (shells-spec->% :source-file))
    (byte-compile-file (shells-spec->% :source-file))))


(defmacro read-shell-env! ()
  `(progn
     (when (file-exists-p (shells-spec->% :compiled-file))
       (load (shells-spec->% :compiled-file)))
     (add-hook 'kill-emacs-hook #'save-shell-env! t)))


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


;; allowed/disallowed `shells-spec->*'

(if (not (shells-spec->* :allowed))
    ;; disallowed: append .exec/ to `exec-path'
    (add-to-list 'exec-path (v-home% ".exec/") t #'string=)
  (read-shell-env!)
  (let ((shell (shells-spec->* :shell-file-name)))
    (when shell
      (setq% explicit-shell-file-name shell 'shell)
      (setq shell-file-name shell)
      (setenv (shells-spec->% :SHELL) shell)))
  (when (shells-spec->* :exec-path)
    (copy-exec-path! (shell-env-> :exec-path)))
  (let ((copying (shells-spec->* :copy-vars)))
    (when (consp copying)
      (copy-env-vars! (shell-env-> :copy-vars)
                      (shells-spec->* :copy-vars))))
  (let ((spinning (shells-spec->* :spin-vars)))
    (when (consp spinning)
      (spin-env-vars! spinning))))


 ;; end of allowed/disallowed `shells-spec->*'





 ;; end of shells.el
