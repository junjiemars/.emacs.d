;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-eglot-autoload.el
;;;;


;; (defmacro-if-feature% eglot)

(require 'eglot)
(require 'cc-mode)


;;; `elgot'

(defun eglot*-lsp-server ()
  "Return the name of lsp-server program."
  (with-current-buffer (current-buffer)
    (when (eglot-managed-p)
      (with-current-buffer (jsonrpc-events-buffer (eglot-current-server))
        (goto-char (point-min))
        (let* ((r "\"Running language server: \\([-/a-zA-Z]+\\)\"")
               (i (search-forward-regexp r nil t)))
          (when i (buffer-substring-no-properties
                   (match-beginning 1)
                   (match-end 1))))))))


(defun eglot*-set-style (&optional style indent)
  "Set the current `eglot-managed-p' buffer to use the STYLE and INDENT."
  (with-current-buffer (current-buffer)
    (when (eglot-managed-p)
      (let ((p (caddr (eglot--current-project)))
            (s (eglot*-lsp-server)))
        (when (and p s)
          (cond ((eq major-mode 'c-mode)
                 (cond ((string= "clangd" (file-name-base* s))
                        (let ((f (concat p ".clang-format"))
                              (ss (concat
                                   "BasedOnStyle: "
                                   (downcase (or style
                                                 c-indentation-style))
                                   "\n"
                                   "IndentWidth: "
                                   (number-to-string
                                    (or indent c-basic-offset)))))
                          (save-str-to-file ss f)))))))))))


(defalias 'eglot*-server-file
  (lexical-let% ((b (v-home% ".exec/eglot-server.el"))
                 (c '(c-mode . ("clangd" "--header-insertion=never"))))
    (lambda (&optional op sexp)
      (cond ((eq op :push)
             (let ((c (or sexp (read-sexp-from-file b))))
               (when c (car (push! c eglot-server-programs)))))
            ((eq op :read)
             (read-sexp-from-file b))
            ((eq op :save)
             (when sexp (save-sexp-to-file sexp b)))
            ((eq op 'c-mode)
             (save-sexp-to-file c b))
            (t b))))
  "The `eglot-server-programs' file.")


(with-eval-after-load 'eglot

  (eglot*-server-file :push))




;;; `project'

(defun project*-try-abs (dir)
  (let ((d (locate-dominating-file dir ".project.el")))
    (when d (list 'vc 'Git d))))


(defun project*-anchor-root (&optional dir)
  "Anchor the root DIR of `project-root'."
  (let ((d (or dir default-directory)))
    (when (directory-name-p d)
      (save-str-to-file
       "; this is a metafile helping `project' to locate directory."
       (concat dir ".project.el")))))


(with-eval-after-load 'project

  (push! #'project*-try-abs project-find-functions))




;;; end of on-eglot-autoload.el
