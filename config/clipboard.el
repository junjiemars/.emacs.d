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

(defun x-kill (text)
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

(defun x-yank ()
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



(defun on-clipboard-init! ()
  "On \\=`clipboard\\=' initialization."
  ;; enable `select'
  (if-version%
      <= 24.1
      (setq% select-enable-clipboard t)
    (setq% x-select-enable-clipboard t))
  (if-version%
      <= 25.1
      (setq% select-enable-primary t 'select)
    (setq% x-select-enable-primary t 'select))

  ;; kill/yank
  (unless-platform% 'windows-nt
    (setq interprogram-cut-function #'x-kill
          interprogram-paste-function #'x-yank)))



(provide 'clipboard)


;; end of clipboard.el
