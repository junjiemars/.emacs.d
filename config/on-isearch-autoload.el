;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-isearch-autoload.el
;;;;


(defun isearch-forward* (&optional regexp-p)
  "Do incremental search forward."
  (interactive "P")
  (isearch-forward regexp-p 1)
  (let ((ss (symbol@)))
    (when (eq 'region (car ss))
      (isearch-yank-string (cdr ss)))))

(defun isearch-backward* (&optional regexp-p)
  "Do incremental search backward."
  (interactive "P")
  (isearch-backward regexp-p 1)
  (let ((ss (symbol@)))
    (when (eq 'region (car ss))
      (isearch-yank-string (cdr ss)))))


(defun isearch-forward-symbol* ()
  "Do incremental symbol forward."
  (interactive)
  (if (mark-symbol@)
      (isearch-forward*)
    (message "%s: [No symbol at point]"
             (propertize "Symbol I-search"
                         'face 'minibuffer-prompt))))


(defun isearch-forward-word* ()
  "Do incremental word forward."
  (interactive)
  (if (mark-word@)
      (isearch-forward*)
    (message "%s: [No word at point]"
             (propertize "Word I-search"
                         'face 'minibuffer-prompt))))

(defun isearch-forward-string* ()
  "Do incremental string forward."
  (interactive)
  (if (mark-string@)
      (isearch-forward*)
    (message "%s: [No string at point]"
             (propertize "String I-search"
                         'face 'minibuffer-prompt))))

(defun isearch-forward-filename* ()
  "Do incremental filename backward."
  (interactive)
  (if (mark-filename@)
      (isearch-forward*)
    (message "%s: [No filename at point]"
             (propertize "Filename I-search"
                         'face 'minibuffer-prompt))))


(with-eval-after-load 'isearch
  (define-key% (current-global-map) (kbd "C-s") #'isearch-forward*)
  (define-key% (current-global-map) (kbd "C-r") #'isearch-backward*)
  (define-key% (current-global-map)
    (kbd "M-s s") #'isearch-forward-symbol*)
  (define-key% (current-global-map)
    (kbd "M-s w") #'isearch-forward-word*)
  (define-key% (current-global-map)
    (kbd "M-s q") #'isearch-forward-string*)
  (define-key% (current-global-map)
    (kbd "M-s f") #'isearch-forward-filename*))


 ;; end of file
