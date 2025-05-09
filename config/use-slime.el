;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-slime.el
;;;;

(defun slime*-lisp-implementations (&optional implementation)
  "Parameterized \\=`slime-lisp-implementations\\='."
  (setq% slime-lisp-implementations
         (let ((ns nil))
           (dolist (x (append implementation '("sbcl" "ecl" "acl")) ns)
             (let ((bin (executable-find* x)))
               (and bin (push! (list x (list bin)) ns)))))
         slime))

(defalias 'slime*-source-locations
  (let ((b nil))
    (lambda (&optional n)
      (cond (n (setq b (cons n b)))
            (t b))))
  "Parameterized source locations for \\=`slime\\='.")

(when-fn% slime-show-source-location slime
  (defun slime-show-source-location* (&rest args)
    (let ((r (apply '_slime-show-source-location_ args)))
      (prog1 r
        (with-current-buffer (current-buffer)
          (let ((b (current-buffer)))
            (catch :br
              (dolist (ss (slime*-source-locations))
                (when (and (stringp ss) (string-match ss (buffer-file-name b)))
                  (view-mode 1)
                  (throw :br t))))))))))

(defun use-slime-init! ()
  "On \\=`slime\\=' initialization."
  (slime*-lisp-implementations)
  (when-fn% slime-setup slime
    (slime-setup '(slime-fancy slime-asdf)))
  (when-fn% slime-selector slime
    (define-global-key% "\C-css" #'slime-selector))
  (when-fn% slime-show-source-location slime
    (defadvice* '_slime-show-source-location_
      'slime-show-source-location #'slime-show-source-location*)))



(provide 'use-slime)

;; end of use-slime.el
