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
        (let* ((r "\"Running language server: \\([-/a-zA-Z0-9]+\\)\"")
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
                 (c '((c-mode . ("clangd" "--header-insertion=never")))))
    (lambda (&optional op sexp)
      (cond ((eq op :push)
             (let ((c (or sexp (read-sexp-from-file b))))
               (dolist* (c1 c)
                 (push! c1 eglot-server-programs))))
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
  "The `project-root' file.")


(defun project*-try-abs (dir)
  (let ((d (project*-root-file :cache dir)))
    (when d (list 'vc 'Git d))))


(with-eval-after-load 'project

  (project*-root-file :read)
  (push! #'project*-try-abs project-find-functions))




;;; end of on-eglot-autoload.el
