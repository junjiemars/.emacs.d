;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-clip-autoload.el
;;;;

;;; (unless-graphic% t)
(eval-when-compile
  (defmacro _when_x_select_ (&rest body)
    "When x-select do BODY."
    (declare (indent 0))
    `(unless-platform% 'windows-nt
       (if-platform% 'darwin
           (when% (and (executable-find% "pbcopy")
                       (executable-find% "pbpaste"))
             ,@body)
         (if-platform% 'gnu/linux
             (when% (executable-find% "xsel")
               ,@body))))))

(_when_x_select_
  ;; declaration
  (declare-function x-kill* (v-home%> "config/clip"))
  (declare-function x-yank* (v-home%> "config/clip"))
  (autoload 'x-kill* (v-home%> "config/clip"))
  (autoload 'x-yank* (v-home%> "config/clip"))
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
  (setq interprogram-cut-function #'x-kill*
        interprogram-paste-function #'x-yank*))

 ;; end of on-clip-autoload.el
