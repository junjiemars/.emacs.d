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
        (let* ((r "Running language server: \\([^\"\n]+\\)")
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
                        (write-file*
                         (format (read-file*
                                  (emacs-home% "config/eglots_clangd"))
                                 (upcase
                                  (or style
                                      (buffer-local-value
                                       'c-indentation-style
                                       (current-buffer))))
                                 (number-to-string
                                  (or indent
                                      (buffer-local-value
                                       'c-basic-offset
                                       (current-buffer)))))
                         (path+ p ".clang-format")))))))))))

(defalias 'eglot*-recipe
  (let ((f (v-home% ".exec/eglot-recipe.el"))
        (b `((c-mode . ("clangd" "--header-insertion=never"))
             (c++-mode . ("clangd" ,(format "--query-driver=%s"
                                            (executable-find* "c++"))))))
        (env nil))
    (lambda (&optional op n)
      (cond ((and op (eq op :save)) (write-file* (or env b) f))
            ((and op (eq op :file)) f)
            ((and n op (eq op :load)) (setq env n))
            (t env))))
  "The \\=`eglot-server-programs\\=' cache.")

(defun eglot*--recipe-init! ()
  (let ((recipe (eglot*-recipe :file)))
    (unless (file-exists-p recipe)
      (eglot*-recipe :save))
    (eglot*-recipe :load (read-file* recipe t))))

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
  ;; init recipe
  (eglot*--recipe-init!)
  ;; most reduced `eldoc'
  (setq% eldoc-echo-area-use-multiline-p nil eldoc)
  ;; keys
  (define-key eglot-mode-map (kbd% "C-c M-c f") #'eglot-format-buffer)
  (define-key eglot-mode-map (kbd% "C-c M-c r") #'eglot-rename)
  t)



(provide 'eglots)

;; end of eglots.el
