;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; clip.el
;;;;
;; Commentary: killing/yanking with the clipboard
;; See also: http://emacswiki.org/emacs/CopyAndPaste
;;;;

(eval-when-compile
  (defmacro _defun_x_kill_ (bin &rest args)
    (declare (indent 1))
    `(defun x-kill* (text &optional _)
       "Copy TEXT to system clipboard."
       (with-temp-buffer
         (insert text)
         (call-process-region (point-min) (point-max)
                              ,bin
                              nil 0 nil
                              ,@args)))))

(eval-when-compile
  (defmacro _defun_x_yank_ (bin &rest args)
    (declare (indent 1))
    `(defun x-yank* ()
       "Paste from system clipboard."
       (let ((out (shell-command* ,bin ,@args)))
         (when (zerop (car out))
           (cdr out))))))

;;; `darwin'
(when-platform% 'darwin (_defun_x_kill_ "pbcopy"))
(when-platform% 'darwin (_defun_x_yank_ "pbpaste"))

;;; `gnu/linux'
(when-platform% 'gnu/linux
  (_defun_x_kill_ "xsel" "--clipboard" "--input"))
(when-platform% 'gnu/linux
  (_defun_x_yank_ "xsel" "--clipboard" "--output"))



(provide 'clip)


;; end of clip.el
