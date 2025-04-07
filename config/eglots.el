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
               (i (re-search-forward r nil t)))
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
                 (cond ((string-equal "clangd" (file-name-base* s))
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
  (let ((f (v-home% ".exec/eglot-server.el"))
        (b `((c-mode . ("clangd" "--header-insertion=never"))
             (c++-mode . ("clangd" ,(format "--query-driver=%s"
                                            (executable-find* "c++")))))))
    (lambda (&optional op sexp)
      (cond ((and op (eq op :read)) (let ((s1 (read-sexp-from-file f)))
                                      (dolist (x s1 s1)
                                        (push! x b t)
                                        (push! x eglot-server-programs t))))
            ((and op (eq op :save)) (save-sexp-to-file (or sexp b) f))
            ((and op (eq op :file)) f)
            (t b))))
  "The \\=`eglot-server-programs\\=' cache.")

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
                (t nil)))))))

(defun on-eglot-init! ()
  "On \\=`eglot\\=' initialization."
  ;; load recipe
  (or (eglot*-server-programs :read) (eglot*-server-programs))
  ;; most reduced `eldoc'
  (setq% eldoc-echo-area-use-multiline-p nil eldoc)
  ;; keys
  (define-key eglot-mode-map (kbd% "C-c M-c f") #'eglot-format-buffer)
  (define-key eglot-mode-map (kbd% "C-c M-c r") #'eglot-rename))



(provide 'eglots)

;; end of eglots.el
