;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-clipboard-autoload.el
;;;;

;;; (unless-graphic% t)
(defmacro when-clipboard% (&rest body)
  "When clipboard do BODY."
  (declare (indent 0))
  `(unless-platform% 'windows-nt
     (if-platform% 'darwin
         (when% (and (executable-find% "pbcopy")
                     (executable-find% "pbpaste"))
           ,@body)
       (if-platform% 'gnu/linux
           (when% (executable-find% "xsel")
             ,@body)))))

(when-clipboard%
  ;; declaration
  (declare-function x-kill* (v-home%> "config/clipboard"))
  (declare-function x-yank* (v-home%> "config/clipboard"))
  (autoload 'x-kill* (v-home%> "config/clipboard"))
  (autoload 'x-yank* (v-home%> "config/clipboard"))
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

 ;; end of on-clipboard-autoload.el
