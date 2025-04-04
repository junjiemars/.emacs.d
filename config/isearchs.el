;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; isearchs.el
;;;;
;; Commentary: `isearch' for Regexp, Symbol, Word, File, and Quoted string.
;;;;


;;; require

(eval-when-compile
  (require 'marks (v-home%> "config/marks")))

;; end of require

;;;
;; env
;;;

(defun isearch*--prompt (prompt)
  (list
   (when current-prefix-arg
     (read-key
      (format "%s: %s"
              (propertize prompt 'face 'minibuffer-prompt)
              (format "(%s)egexp (%s)ymbol (%s)ord (%s)ile (%s)uoted"
                      (propertize "r" 'face 'minibuffer-prompt)
                      (propertize "s" 'face 'minibuffer-prompt)
                      (propertize "w" 'face 'minibuffer-prompt)
                      (propertize "f" 'face 'minibuffer-prompt)
                      (propertize "q" 'face 'minibuffer-prompt)))))))
;; end of env

(defun isearch*-forward (&optional style backward)
  "Search incrementally forward or BACKWARD in STYLE."
  (interactive (isearch*--prompt "I-search"))
  (let ((regexp-p (and style (or (char-equal ?\r style)
                                 (char-equal ?r style)))))
    (cond (backward (isearch-backward regexp-p 1))
          (t (isearch-forward regexp-p 1)))
    (let ((ms (cond ((null style) nil)
                    ((char-equal ?s style)
                     (cons "symbol" (unless-region-active (mark-symbol@))))
                    ((char-equal ?w style)
                     (cons "word" (unless-region-active (mark-word@))))
                    ((char-equal ?f style)
                     (cons "file" (unless-region-active (mark-filename@))))
                    ((char-equal ?q style)
                     (cons "quoted" (unless-region-active
                                      (mark-quoted-symmetry@)))))))
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
  (interactive (isearch*--prompt "I-search backward"))
  (isearch*-forward style t))

(defun isearch*-forward-symbol (&optional backward)
  "Search symbol incrementally forward or BACKWARD."
  (interactive "P")
  (isearch*-forward ?s backward))



(provide 'isearchs)

;; end of isearchs.el
