;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-isearch-autoload.el
;;;;


(defun isearch-forward* (&optional arg)
  "Do incremental region or symbol search forward."
  (interactive "P")
  (isearch-forward arg 1)
  (let ((ss (symbol@)))
    (when (eq 'region (car ss))
      (isearch-yank-string (cdr ss)))))

(defun isearch-backward* (&optional arg)
  "Do incremental region or symbol search backward."
  (interactive "P")
  (isearch-backward arg 1)
  (let ((ss (symbol@)))
    (when (eq 'region (car ss))
      (isearch-yank-string (cdr ss)))))


(with-eval-after-load 'isearch
  (define-key% (current-global-map) (kbd "C-s") #'isearch-forward*)
  (define-key% (current-global-map) (kbd "C-r") #'isearch-backward*))


 ;; end of file
