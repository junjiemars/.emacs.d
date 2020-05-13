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
           (read-key (format
                      "Choose I-search style: %s "
                      "(r)egexp (s)ymbol (w)ord (f)ile (q)uoted")))))
  (let ((regexp-p (and style (char-equal ?r style))))
    (if backward
        (isearch-backward regexp-p 1)
      (isearch-forward (and style (char-equal ?r style)) 1)))
  (unless (null style)
    (let ((ms (cond
               ((char-equal ?s style) (cons "symbol" (mark-symbol@)))
               ((char-equal ?w style) (cons "word" (mark-word@)))
               ((char-equal ?f style) (cons "file" (mark-filename@)))
               ((char-equal ?q style) (cons "quoted" (mark-string@)))
               (t nil))))
      (when ms
        (let ((ss (symbol@)))
          (if (eq 'region (car ss))
              (isearch-yank-string (cdr ss))
            (message "%s: [No %s at point]"
                     (propertize "I-search"
                                 'face 'minibuffer-prompt)
                     (propertize (car ms)
                                 'face 'font-lock-warning-face))))))))


(defun isearch-backward* (&optional style)
  "Do incremental search backward."
  (interactive
   (list (when current-prefix-arg
           (read-key (format
                      "Choose I-search style: %s "
                      "(r)egexp (s)ymbol (w)ord (f)ile (q)uoted")))))
  (isearch-forward* style t))


(with-eval-after-load 'isearch
  (define-key% (current-global-map) (kbd "C-s") #'isearch-forward*)
  (define-key% (current-global-map) (kbd "C-r") #'isearch-backward*))


 ;; end of file
