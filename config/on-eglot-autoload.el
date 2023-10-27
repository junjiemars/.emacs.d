;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-eglot-autoload.el
;;;;


(defmacro-if-feature% eglot)
(defmacro-if-feature% project)


(defmacro when-feature-eglot% (&rest body)
  "When \\=`eglot\\=', do BODY."
  (if-feature-eglot%
      `(progn% ,@body)
    `(comment ,@body)))


(defmacro when-feature-project% (&rest body)
  "When \\=`project\\=', do BODY."
  (if-feature-project%
      `(progn% ,@body)
    `(comment ,@body)))


;;; `elgot'

(when-feature-eglot%

 (defun eglot*-lsp-server ()
   "Return the name of lsp-server program."
   (with-current-buffer (current-buffer)
     (when (eglot-managed-p)
       (with-current-buffer
           (jsonrpc-events-buffer (eglot-current-server))
         (goto-char (point-min))
         (let* ((r (concat "\"Running language server: "
                           "\\([.-/a-zA-Z0-9_]+\\) *?.*?\""))
                (i (search-forward-regexp r nil t)))
           (when i (buffer-substring-no-properties
                    (match-beginning 1)
                    (match-end 1)))))))))


(when-feature-eglot%

 (defun eglot*-set-style (&optional style indent)
   "Set the current \\=`eglot-managed-p\\=' buffer to use the STYLE and INDENT."
   (with-current-buffer (current-buffer)
     (when (eglot-managed-p)
       (let ((p (caddr (eglot--current-project)))
             (s (eglot*-lsp-server)))
         (when (and p s)
           (cond ((eq major-mode 'c-mode)
                  (cond ((string= "clangd" (file-name-base* s))
                         (save-str-to-file
                          (concat
                           "BasedOnStyle: "
                           (downcase
                            (or style
                                (buffer-local-value
                                 'c-indentation-style (current-buffer))))
                           "\n"
                           "IndentWidth: "
                           (number-to-string
                            (or indent
                                (buffer-local-value
                                 'c-basic-offset (current-buffer)))))
                          (concat p ".clang-format"))))))))))))


(when-feature-eglot%

 (defalias 'eglot*-server-programs
   (lexical-let% ((b (v-home% ".exec/eglot-server.el"))
                  (m '((c-mode . ("clangd" "--header-insertion=never"))
                       (swift-mode . ("sourcekit-lsp")))))
     (lambda (&optional op sexp)
       (cond ((eq op :push)
              (dolist* (x sexp sexp)
                (push! x eglot-server-programs)))
             ((eq op :read)
              (read-sexp-from-file b))
             ((eq op :save)
              (when sexp (save-sexp-to-file sexp b)))
             (t m))))
   "The \\=`eglot-server-programs\\=' cache."))


(when-feature-eglot%

 (defun eglot*-eldoc-no-builtins ()
   "Remove the builtin \\=`eldoc\\=' fns from \\=`eglot--managed-mode\\=' mode."
   (with-current-buffer (current-buffer)
     (when (eglot-managed-p)
       (let ((p (caddr (eglot--current-project)))
             (s (eglot*-lsp-server)))
         (when (and p s)
           (cond ((eq major-mode 'python-mode)
                  (setq eldoc-documentation-functions
                        (delq 'python-eldoc-function
                              eldoc-documentation-functions))
                  (setq completion-at-point-functions
                        (delq 'python-completion-at-point
                              completion-at-point-functions)))
                 (t nil))))))))


(when-feature-eglot%
 (with-eval-after-load 'eglot

   ;; load recipe
   (eglot*-server-programs :push (or (eglot*-server-programs :read)
                                     (eglot*-server-programs)))
   ;; most reduced
   (when-var% eldoc-echo-area-use-multiline-p 'eldoc
     (setq eldoc-echo-area-use-multiline-p nil))))

;; end of `eglot'


;;; `project'

(when-feature-project%

 (defalias 'project*-root
   (lexical-let% ((b (emacs-home* "private/project-root.el"))
                  (c '()))
     (lambda (&optional op sexp)
       (cond ((eq op :cache)
              (if sexp
                  (catch 'rc
                    (dolist* (s1 c)
                      (when (string= s1 sexp)
                        (throw 'rc s1))))
                c))
             ((eq op :read)
              (setq c (read-sexp-from-file b)))
             ((eq op :save)
              (when sexp (save-sexp-to-file sexp b)))
             (t c))))
   "The \\=`project-root\\=' cache."))


(when-feature-project%

 (defun project*-try-abs (dir)
   (let ((d (project*-root :cache dir)))
     (when d (list 'vc 'Git d)))))


(when-feature-project%

 (with-eval-after-load 'project

   (project*-root :read)
   (push! #'project*-try-abs project-find-functions)))

;; end of `project'


;; end of on-eglot-autoload.el
