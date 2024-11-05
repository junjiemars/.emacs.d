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
  "Set STYLE and INDENT for \\=`eglot-managed-p\\=' buffer."
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
                          (upcase
                           (or style
                               (buffer-local-value
                                'c-indentation-style (current-buffer))))
                          "\n---\n"
                          "Language: Cpp\n"
                          "IndentWidth: "
                          (number-to-string
                           (or indent
                               (buffer-local-value
                                'c-basic-offset (current-buffer)))))
                         ;; /* clang-format on */
                         ;; /* clang-format off */
                         (concat p ".clang-format")))))))))))

(defalias 'eglot*-server-programs
  (lexical-let%
      ((f (v-home% ".exec/eglot-server.el"))
       (b `((c-mode . ("clangd" "--header-insertion=never"))
            ,(let ((cxx (executable-find% "c++")))
               `(c++-mode
                 .
                 ("clangd"
                  ,(format "--query-driver=%s" cxx)))))))
    (lambda (&optional op sexp)
      (cond ((eq op :read) (let ((s1 (read-sexp-from-file f)))
                             (dolist* (x s1 s1)
                               (push! x b t)
                               (push! x eglot-server-programs t))))
            ((eq op :save) (save-sexp-to-file (or sexp b) f))
            ((eq op :file) f)
            (t b))))
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
  (eglot*-server-programs :read)
  ;; most reduced `eldoc'
  (setq% eldoc-echo-area-use-multiline-p nil 'eldoc)

  ;; shutdown when `kill-emacs'
  (when-var% kill-emacs-query-functions nil
    (push! 'eglot*-shutdown-all kill-emacs-query-functions))
  ;; keys
  (define-key eglot-mode-map (kbd% "C-c M-c f") #'eglot-format-buffer)
  (define-key eglot-mode-map (kbd% "C-c M-c r") #'eglot-rename))



(provide 'eglots)

;; end of eglots.el
