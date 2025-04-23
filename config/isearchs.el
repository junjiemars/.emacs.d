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

(defun isearch*--yank-string (string style)
  (if string
      (isearch-yank-string string)
    (message "%s: No %s at point"
             (propertize "I-search" 'face 'minibuffer-prompt)
             (propertize style 'face 'font-lock-warning-face))))

;; end of env

(defun isearch*-forward (&optional style backward)
  "Search incrementally forward or BACKWARD in STYLE."
  (interactive (isearch*--prompt "I-search"))
  (let ((regexp-p (and style (or (char-equal ?r style)
                                 ;; suppose (r)egexp
                                 (char-equal ?\r style)))))
    (cond (backward (isearch-backward regexp-p 1))
          (t (isearch-forward regexp-p 1)))
    (if-region-active
        (isearch-yank-x-selection)
      (cond ((null style) nil)
            ((char-equal ?s style)
             (isearch*--yank-string (symbol@*) "symbol"))
            ((char-equal ?w style)
             (isearch*--yank-string (word@*) "word"))
            ((char-equal ?f style)
             (isearch*--yank-string (filename@*) "file"))
            ((char-equal ?q style)
             (isearch*--yank-string (string@*) "quoted"))))))


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
