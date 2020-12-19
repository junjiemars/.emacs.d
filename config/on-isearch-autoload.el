;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-isearch-autoload.el
;;;;


(defun isearch-forward* (&optional style backward)
  "Do incremental search forward."
  (interactive
   (list (when current-prefix-arg
           (read-key
            (format "%s: %s "
                    (propertize "I-search" 'face 'minibuffer-prompt)
                    "(r)egexp (s)ymbol (w)ord (f)ile (q)uoted")))))
  (let ((regexp-p (and style (or (char= ?\r style)
                                 (char= ?r style)))))
    (if backward
        (isearch-backward regexp-p 1)
      (isearch-forward regexp-p 1)))
  (let ((ms (cond ((and style (char= ?s style))
                   (cons "symbol" (mark-symbol@)))
                  ((and style (char= ?w style))
                   (cons "word" (mark-word@)))
                  ((and style (char= ?f style))
                   (cons "file" (mark-filename@)))
                  ((and style (char= ?q style))
                   (cons "quoted" (mark-string@))))))
    (let ((ss (symbol@)))
      (if (eq 'region (car ss))
          (isearch-yank-string (cdr ss))
        (when ms
          (message "%s: [No %s at point]"
                   (propertize "I-search"
                               'face 'minibuffer-prompt)
                   (propertize (car ms)
                               'face 'font-lock-warning-face)))))))


(defun isearch-backward* (&optional style)
  "Do incremental search backward."
  (interactive
   (list (when current-prefix-arg
           (read-key
            (format "%s: %s "
                    (propertize "I-search" 'face 'minibuffer-prompt)
                    "(r)egexp (s)ymbol (w)ord (f)ile (q)uoted")))))
  (isearch-forward* style t))


(defun isearch-forward-symbol* (&optional backward)
  "Do incremental search forward symbol."
  (interactive "P")
  (isearch-forward* ?s backward))


(defun isearch-forward-word* (&optional backward)
  "Do incremental search forward word."
  (interactive "P")
  (isearch-forward* ?w backward))


(defun isearch-forward-file* (&optional backward)
  "Do incremental search forward file."
  (interactive "P")
  (isearch-forward* ?f backward))


(defun isearch-forward-quoted* (&optional backward)
  "Do incremental search forward quoted."
  (interactive "P")
  (isearch-forward* ?q backward))


;;;;
;; Keys
;;;;

(define-key% (current-global-map) (kbd "C-s") #'isearch-forward*)
(define-key% (current-global-map) (kbd "C-r") #'isearch-backward*)
(define-key% (current-global-map) (kbd "M-s .") #'isearch-forward-symbol*)
(define-key% (current-global-map) (kbd "M-s @") #'isearch-forward-word*)
(define-key% (current-global-map) (kbd "M-s f") #'isearch-forward-file*)
(define-key% (current-global-map) (kbd "M-s _") #'isearch-forward-quoted*)


;; EOF
