;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-compile-autoload.el
;;;;


(platform-supported-when 'windows-nt
  ;; There are no builtin `grep' in Windows, GNU's `grep' may use
  ;; the UNIX path in Windows which cannot be recognized by Emacs.
  ;; When such case occurred, we try to translate UNIX path to POSIX path.
  (defadvice compilation-find-file (before compilation-find-file-before compile)
    (when (string-match "^/\\([a-zA-Z]\\)/" (ad-get-arg 1))
      (ad-set-arg 1 ;; filename argument
                  (replace-match (concat (match-string 1 (ad-get-arg 1)) ":/")
                                 t t (ad-get-arg 1))))))


(with-eval-after-load 'compile
  
  (platform-supported-when 'windows-nt
    ;; compile and activate `compilation-find-file' advice on Windows
    (ad-activate #'compilation-find-file t))
  
  (add-hook 'compilation-filter-hook #'colorize-compilation-buffer!)
  (when-var% compilation-mode-map 'compile
    ;; define `recompile' and `quit-window' key bindings
    (define-key% compilation-mode-map (kbd "g") #'recompile)
    (define-key% compilation-mode-map (kbd "q") #'quit-window))
  (setq% compilation-scroll-output t compile))


(with-eval-after-load 'grep
  (when-var% grep-mode-map 'grep
    ;; define `recompile' and `quit-window' key binding for `grep'
    (define-key% grep-mode-map (kbd "g") #'recompile)
    (define-key% grep-mode-map (kbd "q") #'quit-window)))
