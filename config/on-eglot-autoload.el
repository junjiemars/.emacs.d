;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-eglot-autoload.el
;;;;


(defmacro-if-feature% eglot)
(defmacro-if-feature% project)


;;; `elgot'

(if-feature-eglot%

    (defun eglot*-lsp-server ()
      "Return the name of lsp-server program."
      (with-current-buffer (current-buffer)
        (when (eglot-managed-p)
          (with-current-buffer (jsonrpc-events-buffer (eglot-current-server))
            (goto-char (point-min))
            (let* ((r (concat "\"Running language server: "
                              "\\([.-/a-zA-Z0-9_]+\\) *?.*?\""))
                   (i (search-forward-regexp r nil t)))
              (when i (buffer-substring-no-properties
                       (match-beginning 1)
                       (match-end 1)))))))))


(if-feature-eglot%

    (defun eglot*-set-style (&optional style indent)
      "Set the current `eglot-managed-p' buffer to use the STYLE and INDENT."
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


(if-feature-eglot%

    (defalias 'eglot*-server-file
      (lexical-let% ((b (v-home% ".exec/eglot-server.el"))
                     (m '((c-mode . ("clangd" "--header-insertion=never"))
                          (swift-mode . ("sourcekit-lsp")))))
        (lambda (&optional op sexp)
          (cond ((eq op :push)
                 (let ((s (or sexp (read-sexp-from-file b) m)))
                   (dolist* (x s)
                     (push! x eglot-server-programs))))
                ((eq op :read)
                 (read-sexp-from-file b))
                ((eq op :save)
                 (when sexp (save-sexp-to-file sexp b)))
                ((eq op :mode)
                 (save-sexp-to-file m b))
                (t b))))
      "The `eglot-server-programs' file."))


(if-feature-eglot%

    (defun eglot*-eldoc-no-builtins ()
      "Remove the builtin `eldoc' fns from `eglot--managed-mode's mode."
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


(if-feature-eglot%

  (with-eval-after-load 'eglot
    (eglot*-server-file :push)))




;;; `project'

(if-feature-project%

    (defalias 'project*-root-file
      (lexical-let% ((b (v-home% ".exec/project-root.el"))
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
                 (when (setq c sexp) (save-sexp-to-file c b)))
                (t b))))
      "The `project-root' file."))


(if-feature-project%

    (defun project*-try-abs (dir)
      (let ((d (project*-root-file :cache dir)))
        (when d (list 'vc 'Git d)))))


(if-feature-project%

    (with-eval-after-load 'project
      (project*-root-file :read)
      (push! #'project*-try-abs project-find-functions)))





;;; end of on-eglot-autoload.el
