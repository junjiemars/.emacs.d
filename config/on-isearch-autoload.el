;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-isearch-autoload.el
;;;;


(terminal-supported-p
  (when% (facep 'isearch-fail)
    (set-face-background 'isearch-fail "white")
    (set-face-foreground 'isearch-fail "black")))

(eval-when-compile

  (defmacro defun-isearch-forward-or-backward (direction)
    "Define `isearch-forward*' or `isearch-backward*'"
    (let ((fn (intern (format "isearch-%s*" (symbol-name direction))))
          (dn (intern (format "isearch-%s" (symbol-name direction)))))
      `(defun ,fn ()
         ,(format "Do incremental regexp or symbol search %s."
                  `,(symbol-name direction))
         (interactive)
         (let* ((ss (thing-at-point 'symbol))
                (s (and (stringp ss) (substring-no-properties ss))))
           (if (and s (< 1 (length s)))
               (progn
                 (,dn nil 1)
                 (isearch-yank-string s))
             (,dn t 1)))))))


(defun-isearch-forward-or-backward forward)
(defun-isearch-forward-or-backward backward)

(define-key (current-global-map) (kbd "C-s") #'isearch-forward*)
(define-key (current-global-map) (kbd "C-r") #'isearch-backward*)


;; end of file
