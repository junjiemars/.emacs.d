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
                                   (or indent
                                       (number-to-string c-basic-offset)))))
                          (save-str-to-file ss f)))))))))))




;;; `project'

(defun project*-try-abs (dir)
  (let ((d (locate-dominating-file dir ".project.el")))
    (when d (list 'vc 'Git d))))


(defun project*-set-root (dir)
  "Set the root directory of `project-root'."
  (when (and (file-exists-p dir) (directory-name-p dir))
    (save-str-to-file "" (concat dir ".project.el"))))


(with-eval-after-load 'project

  (push! #'project*-try-abs project-find-functions nil t))




;;; end of on-eglot-autoload.el
