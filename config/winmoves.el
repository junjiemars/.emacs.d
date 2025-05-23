;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; winmoves.el
;;;;

;;;
;; `recenter-top-bottom' for Emacs23.2-
;;;

(eval-when-compile
  (defmacro unless-fn-recenter-top-bottom% (&rest body)
    `(unless-fn% recenter-top-bottom nil
       (progn% ,@body))))

(unless-fn-recenter-top-bottom%
 (defvar recenter-last-op nil
   "Indicates the last recenter operation performed."))

(unless-fn-recenter-top-bottom%
 (defvar recenter-positions '(middle top bottom)
   "Cycling order for \\=`recenter-top-bottom\\='."))

(unless-fn-recenter-top-bottom%
 (defun recenter-top-bottom (&optional arg)
   "Move current buffer line to the specified window line."
   (interactive "P")
   (cond (arg (recenter arg))
         (t (setq recenter-last-op
                  (if (eq this-command last-command)
                      (car (or (cdr (memq recenter-last-op
                                          recenter-positions))
                               recenter-positions))
                    (car recenter-positions)))
            (let ((this-scroll-margin
                   (min (max 0 scroll-margin)
                        (truncate (/ (window-body-height) 4.0)))))
              (cond ((eq recenter-last-op 'middle)
                     (recenter))
                    ((eq recenter-last-op 'top)
                     (recenter this-scroll-margin))
                    ((eq recenter-last-op 'bottom)
                     (recenter (- -1 this-scroll-margin)))
                    ((integerp recenter-last-op)
                     (recenter recenter-last-op))
                    ((floatp recenter-last-op)
                     (recenter (round (* recenter-last-op
                                         (window-height)))))))))))

;; end of `recenter-top-bottom'

(provide 'winmoves)

;; end of winmoves.el
