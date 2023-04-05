;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-window-autoload.el
;;;;


;; Define `recenter-top-bottom' for Emacs23.2-
(unless-fn% 'recenter-top-bottom nil

  (defvar recenter-last-op nil
    "Indicates the last recenter operation performed.
Possible values: `top', `middle', `bottom', integer or float numbers.
It can also be nil, which means the first value in `recenter-positions'.")

  (defvar recenter-positions '(middle top bottom)
    "Cycling order for `recenter-top-bottom'.
A list of elements with possible values `top', `middle', `bottom',
integer or float numbers that define the cycling order for
the command `recenter-top-bottom'.

Top and bottom destinations are `scroll-margin' lines from the true
window top and bottom.  Middle redraws the frame and centers point
vertically within the window.  Integer number moves current line to
the specified absolute window-line.  Float number between 0.0 and 1.0
means the percentage of the screen space from the top.  The default
cycling order is middle -> top -> bottom.")


  (defun recenter-top-bottom (&optional arg)
    "Move current buffer line to the specified window line.
With no prefix argument, successive calls place point according
to the cycling order defined by `recenter-positions'.

A prefix argument is handled like `recenter':
 With numeric prefix ARG, move current line to window-line ARG.
 With plain `C-u', move current line to window center."
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
                                          (window-height))))))))))

  (define-key (current-global-map) (kbd "C-l") #'recenter-top-bottom))

 ;; end of recenter-top-bottom


;;; Window move key bindings `windmove-default-keybindings'
(define-key% (current-global-map) (kbd "C-c w l") #'windmove-left)
(define-key% (current-global-map) (kbd "C-c w r") #'windmove-right)
(define-key% (current-global-map) (kbd "C-c w u") #'windmove-up)
(define-key% (current-global-map) (kbd "C-c w d") #'windmove-down)


;;; Buffer's key bindings
(define-key% (current-global-map) (kbd "C-x x c") #'clone-buffer)
(define-key% (current-global-map) (kbd "C-x x n") #'echo-buffer-name)
(define-key% (current-global-map) (kbd "C-x x t") #'toggle-truncate-lines)
(define-key% (current-global-map) (kbd "C-x RET =") #'get-buffer-coding-system)
(define-key% (current-global-map) (kbd "C-x x g")
  (if-fn% 'revert-buffer-quick nil
          #'revert-buffer-quick
    #'revert-buffer))
(when-fn% 'linum-mode 'linum
  (define-key% (current-global-map) (kbd "C-x x l") #'linum-mode))
(define-key% (current-global-map) (kbd "C-x x r") #'rename-buffer)
(when-fn% 'toggle-word-wrap 'simple
  (define-key% (current-global-map) (kbd "C-x x w") #'toggle-word-wrap))
(when-fn% 'whitespace-mode 'whitespace
  (define-key% (current-global-map) (kbd "C-x x SPC") #'whitespace-mode))
(define-key% (current-global-map) (kbd "C-x x u") #'rename-uniquely)




(with-eval-after-load 'view

  ;; keep `view-mode' when quit
  (when-var% view-mode-map 'view
    (define-key% view-mode-map (kbd "q") #'quit-window)))
