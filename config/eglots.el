;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; eglots.el
;;;;

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
                   (match-end 1))))))))

(defun eglot*-set-style (&optional style indent)
  "Set the current \\=`eglot-managed-p\\=' buffer to use the STYLE and INDENT."
  (with-current-buffer (current-buffer)
    (when (eglot-managed-p)
      (let ((p (let ((c (eglot--current-project)))
                 (cond ((eq 'transient (car c)) (cdr c))
                       ((eq 'vc (car c)) (caddr c))
                       (t (last c)))))
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
                         (concat p ".clang-format")))))))))))

(defalias 'eglot*-server-programs
  (lexical-let%
      ((b (v-home% ".exec/eglot-server.el"))
       (m `((c-mode . ("clangd" "--header-insertion=never"))
            ,(let ((cxx (executable-find% "c++")))
               `(c++-mode
                 .
                 ("clangd"
                  ,(format "--query-driver=%s" cxx)))))))
    (lambda (&optional op sexp)
      (cond ((eq op :push)
             (dolist* (x sexp sexp)
               (push! x eglot-server-programs)))
            ((eq op :read)
             (read-sexp-from-file b))
            ((eq op :save)
             (when sexp (save-sexp-to-file sexp b)))
            (t m))))
  "The \\=`eglot-server-programs\\=' cache.")

(defun eglot*-eldoc-no-builtins ()
  "Remove the builtin \\=`eldoc\\=' fns from
\\=`eglot--managed-mode\\=' mode."
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
                (t nil)))))))

(defun eglot*-shutdown-all ()
  (condition-case _
      (prog1 t
        (let ((debug-on-signal nil))
          (eglot-shutdown-all)))
    (error t)))

(defun on-eglot-init! ()
  "On \\=`eglot\\=' initialization."
  ;; load recipe
  (eglot*-server-programs :push (or (eglot*-server-programs :read)
                                    (eglot*-server-programs)))
  ;; most reduced
  (setq% eldoc-echo-area-use-multiline-p nil 'eldoc)

  ;; shutdown when `kill-emacs'
  (when-var% kill-emacs-query-functions nil
    (push! 'eglot*-shutdown-all kill-emacs-query-functions)))



(provide 'eglots)

;; end of eglots.el
