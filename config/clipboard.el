;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; clipboard.el
;;;;
;; Commentary: killing/yanking with the clipboard
;; See also: http://emacswiki.org/emacs/CopyAndPaste
;;;;

(defun clipboard-x-kill (text)
  "Kill TEXT to system clipboard."
  (with-temp-buffer
    (insert text)
    (let ((kill (if-platform% 'darwin
                    `("pbcopy" . nil)
                  (if-platform% 'gnu/linux
                      `("xsel" . `("--clipboard" "--input"))))))
      (unless kill
        (error "%s" "No kill command found"))
      (apply #'call-process-region (point-min) (point-max)
             (car kill) nil 0 nil (cdr kill)))))

(defun clipboard-x-yank ()
  "Yank from system clipboard."
  (let ((yank (if-platform% 'darwin
                  `("pbpaste" . nil)
                (if-platform% 'gnu/linux
                    `("xsel" . `("--clipboard" "--output"))))))
    (unless yank
      (error "%s" "No yank command found"))
    (let ((out (apply #'shell-command* (car yank) (cdr yank))))
      (when (zerop (car out))
        (cdr out)))))



(provide 'clipboard)


;; end of clipboard.el
