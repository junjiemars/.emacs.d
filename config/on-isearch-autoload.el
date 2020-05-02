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


(defun isearch-forward-symbol* (&optional arg)
  "Do incremental symbol forward."
  (interactive "P")
  (if (mark-symbol@)
      (if arg
          (isearch-backward*)
        (isearch-forward*))
    (comment arg)
    (message "%s: [No symbol at point]"
             (propertize "Symbol I-search"
                         'face 'minibuffer-prompt))))


(defun isearch-forward-word* (&optional arg)
  "Do incremental word forward."
  (interactive "P")
  (if (mark-word@)
      (if arg
          (isearch-backward*)
        (isearch-forward*))
    (comment arg)
    (message "%s: [No word at point]"
             (propertize "Word I-search"
                         'face 'minibuffer-prompt))))

(defun isearch-forward-string* (&optional arg)
  "Do incremental string forward."
  (interactive "P")
  (if (mark-string@)
      (if arg
          (isearch-backward*)
        (isearch-forward*))
    (comment arg)
    (message "%s: [No string at point]"
             (propertize "String I-search"
                         'face 'minibuffer-prompt))))

(defun isearch-forward-filename* (&optional arg)
  "Do incremental filename backward."
  (interactive "P")
  (if (mark-filename@)
      (if arg
          (isearch-backward*)
        (isearch-forward*))
    (comment arg)
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
