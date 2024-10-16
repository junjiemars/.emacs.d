;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; isearchs.el
;;;;

;;; macro

;; (if-version%
;;     < 25
;;     (eval-when-compile
;;       (require 'marks (v-home%> "config/marks")))
;;   (require 'marks (v-home%> "config/marks")))

(declare-function _mark_filename@_ (v-home%> "config/marks"))
(declare-function _mark_quoted_symmetry@_ (v-home%> "config/marks"))
(declare-function _mark_symbol@_ (v-home%> "config/marks"))
(declare-function _mark_thing_ (v-home%> "config/marks"))
(declare-function _mark_word@_ (v-home%> "config/marks"))
(autoload '_mark_filename@_ (v-home%> "config/marks") nil nil 'macro)
(autoload '_mark_quoted_symmetry@_ (v-home%> "config/marks") nil nil 'macro)
(autoload '_mark_symbol@_ (v-home%> "config/marks") nil nil 'macro)
(autoload '_mark_thing_ (v-home%> "config/marks") nil nil 'macro)
(autoload '_mark_word@_ (v-home%> "config/marks") nil nil 'macro)

;; end of macro

(defun isearch*-forward (&optional style backward)
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
                           (unless-region-active
                             (let ((bs (_mark_symbol@_)))
                               (unless bs
                                 (user-error "%s" "No symbol at point"))
                               (_mark_thing_ (car bs) (cdr bs))))))
                    ((char= ?w style)
                     (cons "word"
                           (unless-region-active
                             (let ((bs (_mark_word@_)))
                               (unless bs
                                 (user-error "%s" "No word at point"))
                               (_mark_thing_ (car bs) (cdr bs))))))
                    ((char= ?f style)
                     (cons "file"
                           (unless-region-active
                             (let ((bs (_mark_filename@_)))
                               (unless bs
                                 (user-error "%s" "No file at point"))
                               (_mark_thing_ (car bs) (cdr bs))))))
                    ((char= ?q style)
                     (cons "quoted"
                           (unless-region-active
                             (let ((bs (_mark_quoted_symmetry@_)))
                               (unless bs
                                 (user-error "%s" "No quoted thing at point"))
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


(defun isearch*-backward (&optional style)
  "Search incrementally backward in STYLE."
  (interactive
   (list (when current-prefix-arg
           (read-key
            (format "%s: %s "
                    (propertize "I-search" 'face 'minibuffer-prompt)
                    "(r)egexp (s)ymbol (w)ord (f)ile (q)uoted")))))
  (isearch*-forward style t))


(defun isearch*-forward-symbol (&optional backward)
  "Search symbol incrementally forward or BACKWARD."
  (interactive "P")
  (isearch*-forward ?s backward))


(defun isearch*-forward-word (&optional backward)
  "Search word incrementally forward or BACKWARD."
  (interactive "P")
  (isearch*-forward ?w backward))


(defun isearch*-forward-file (&optional backward)
  "Search filename incrementally forward or BACKWARD."
  (interactive "P")
  (isearch*-forward ?f backward))


(defun isearch*-forward-quoted (&optional backward)
  "Search quoted string incrementally search forward or BACKWARD."
  (interactive "P")
  (isearch*-forward ?q backward))



(provide 'isearchs)

;; end of isearchs.el
