;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; scratch.el
;;;;


(defvar *scratch-kinds*
  '("scratch" "org")
  "Kinds of scratch.")


(defvar *scratch-history* nil
  "Scrach choosing history list.")


(defun scratch (&optional kind)
  "New a *scratch* buffer by KIND or switch to the existing one."
  (interactive
   (list (if current-prefix-arg
             (read-string (format "Choose (%s) "
                                  (mapconcat #'identity
                                             *scratch-kinds*
                                             "|"))
                          (or (car *scratch-history*)
                              (car *scratch-kinds*))
                          '*scratch-history*)
           "scratch")))
  (switch-to-buffer
   (let ((n (format "*%s*" (cond ((string= "org" kind) "scratch-org")
                                 (t kind)))))
     (or (get-buffer n)
         (with-current-buffer (get-buffer-create n)
           (set-buffer-major-mode (current-buffer))
           (when (zerop (buffer-size))
             (cond ((string= "org" kind)
                    (insert-file-contents (emacs-home* "config/scratch.org"))
                    (org-mode))
                 (t (insert (substitute-command-keys initial-scratch-message))
                    (set-buffer-modified-p nil))))
           (current-buffer))))))



(provide 'scratch)

;;; eof
