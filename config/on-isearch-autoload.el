;;; on-isearch-autoload.el --- isearch -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;;; Commentary:
;;;
;;


(eval-when-compile (require 'marks))


(defun isearch-forward* (&optional style backward)
  "Search incrementally forward or BACKWARD in STYLE."
  (interactive
   (list (when current-prefix-arg
           (read-key
            (let ((r (propertize "r" 'face 'minibuffer-prompt))
                  (s (propertize "s" 'face 'minibuffer-prompt))
                  (w (propertize "w" 'face 'minibuffer-prompt))
                  (f (propertize "f" 'face 'minibuffer-prompt))
                  (q (propertize "q" 'face 'minibuffer-prompt)))
              (format "%s: (%s)egexp (%s)ymbol (%s)ord (%s)ile (%s)uoted "
                      (propertize "I-search" 'face 'minibuffer-prompt)
                      r s w f q))))))
  (let ((regexp-p (and style (or (char= ?\r style) (char= ?r style)))))
    (if backward (isearch-backward regexp-p 1)
      (isearch-forward regexp-p 1))
    (let ((ms (cond ((null style) nil)
                    ((char= ?s style)
                     (cons "symbol"
                           (region-active-unless
                             (let ((bs (_mark_symbol@_)))
                               (_mark_thing_ (car bs) (cdr bs))))))
                    ((char= ?w style)
                     (cons "word"
                           (region-active-unless
                             (let ((bs (_mark_word@_)))
                               (_mark_thing_ (car bs) (cdr bs))))))
                    ((char= ?f style)
                     (cons "file"
                           (region-active-unless
                             (let ((bs (_mark_filename@_)))
                               (_mark_thing_ (car bs) (cdr bs))))))
                    ((char= ?q style)
                     (cons "quoted"
                           (region-active-unless
                             (let ((bs (_mark_quoted@_)))
                               (_mark_thing_ (car bs) (cdr bs)))))))))
      (let ((ss (symbol@)))
        (if (eq 'region (car ss))
            (isearch-yank-string (cdr ss))
          (when ms
            (message "%s: [No %s at point]"
                     (propertize "I-search"
                                 'face 'minibuffer-prompt)
                     (propertize (car ms)
                                 'face 'font-lock-warning-face))))))))


(defun isearch-backward* (&optional style)
  "Search incrementally backward in STYLE."
  (interactive
   (list (when current-prefix-arg
           (read-key
            (format "%s: %s "
                    (propertize "I-search" 'face 'minibuffer-prompt)
                    "(r)egexp (s)ymbol (w)ord (f)ile (q)uoted")))))
  (isearch-forward* style t))


(defun isearch-forward-symbol* (&optional backward)
  "Search symbol incrementally forward or BACKWARD."
  (interactive "P")
  (isearch-forward* ?s backward))


(defun isearch-forward-word* (&optional backward)
  "Search word incrementally forward or BACKWARD."
  (interactive "P")
  (isearch-forward* ?w backward))


(defun isearch-forward-file* (&optional backward)
  "Search filename incrementally forward or BACKWARD."
  (interactive "P")
  (isearch-forward* ?f backward))


(defun isearch-forward-quoted* (&optional backward)
  "Search quoted string incrementally search forward or BACKWARD."
  (interactive "P")
  (isearch-forward* ?q backward))


;;; Keys

(when-fn% 'isearch-forward-symbol nil
  (define-key% (current-global-map)
    (kbd "M-s >") #'isearch-forward-symbol))

(when-fn% 'isearch-forward-word nil
  (define-key% (current-global-map)
    (kbd "M-s @") #'isearch-forward-word))

(define-key% (current-global-map) (kbd "C-s") #'isearch-forward*)
(define-key% (current-global-map) (kbd "C-r") #'isearch-backward*)
(define-key% (current-global-map) (kbd "M-s .") #'isearch-forward-symbol*)
(define-key% (current-global-map) (kbd "M-s 2") #'isearch-forward-word*)
(define-key% (current-global-map) (kbd "M-s f") #'isearch-forward-file*)
(define-key% (current-global-map) (kbd "M-s _") #'isearch-forward-quoted*)

;; end of Keys



;; end of on-isearch-autoload.el
