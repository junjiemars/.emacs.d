;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-tramp-autoload.el
;;;;


(with-eval-after-load 'tramp

  (setq% tramp-default-method (platform-supported-if 'windows-nt
                                  "sshx" "ssh") 'tramp))


 ;; end of file
