;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; isearchs.el
;;;;

;;; require

(require% 'marks (v-home%> "config/marks"))

;; end of require

(defun isearch*-forward (&optional style backward)
  "Search incrementally forward or BACKWARD in STYLE."
  (interactive
   (list (when current-prefix-arg
           (read-key
            (format "%s: %s"
                    (propertize "I-search" 'face 'minibuffer-prompt)
                    (format "(%s)egexp (%s)ymbol (%s)ord (%s)ile (%s)uoted"
                            (propertize "r" 'face 'minibuffer-prompt)
                            (propertize "s" 'face 'minibuffer-prompt)
                            (propertize "w" 'face 'minibuffer-prompt)
                            (propertize "f" 'face 'minibuffer-prompt)
                            (propertize "q" 'face 'minibuffer-prompt)))))))
  (let ((regexp-p (and style (or (char= ?\r style) (char= ?r style)))))
    (cond (backward (isearch-backward regexp-p 1))
          (t (isearch-forward regexp-p 1)))
    (let ((ms (cond ((null style) nil)
                    ((char= ?s style)
                     (cons "symbol"
                           (unless-region-active
                             (let ((bs (_mark_symbol@_)))
                               (unless bs
                                 (user-error "%s" "No symbol at point"))
                               (mark-thing (car bs) (cdr bs))))))
                    ((char= ?w style)
                     (cons "word"
                           (unless-region-active
                             (let ((bs (_mark_word@_)))
                               (unless bs
                                 (user-error "%s" "No word at point"))
                               (mark-thing (car bs) (cdr bs))))))
                    ((char= ?f style)
                     (cons "file"
                           (unless-region-active
                             (let ((bs (_mark_filename@_)))
                               (unless bs
                                 (user-error "%s" "No file at point"))
                               (mark-thing (car bs) (cdr bs))))))
                    ((char= ?q style)
                     (cons "quoted"
                           (unless-region-active
                             (let ((bs (_mark_quoted_symmetry@_)))
                               (unless bs
                                 (user-error "%s" "No quoted thing at point"))
                               (mark-thing (car bs) (cdr bs)))))))))
      (let ((ss (symbol@)))
        (if (eq 'region (car ss))
            (isearch-yank-string (cdr ss))
          (when ms
            (message "%s: [No %s at point]"
                     (propertize "I-search"
                                 'face 'minibuffer-prompt)
                     (propertize (car ms)
                                 'face 'font-lock-warning-face))))))))


(defun isearch*-backward (&optional style)
  "Search incrementally backward in STYLE."
  (interactive
   (list (when current-prefix-arg
           (read-key
            (format "%s: %s"
                    (propertize "I-search backward" 'face 'minibuffer-prompt)
                    (format "(%s)egexp (%s)ymbol (%s)ord (%s)ile (%s)uoted"
                            (propertize "r" 'face 'minibuffer-prompt)
                            (propertize "s" 'face 'minibuffer-prompt)
                            (propertize "w" 'face 'minibuffer-prompt)
                            (propertize "f" 'face 'minibuffer-prompt)
                            (propertize "q" 'face 'minibuffer-prompt)))))))
  (isearch*-forward style t))


(defun isearch*-forward-symbol (&optional backward)
  "Search symbol incrementally forward or BACKWARD."
  (interactive "P")
  (isearch*-forward ?s backward))



(provide 'isearchs)

;; end of isearchs.el
