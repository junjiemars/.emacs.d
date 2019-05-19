;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-org-autoload.el (Lisp Flavoured Erlang)
;;;;


(defmacro-if-feature% ox-reveal)

;; fix: Warning (bytecomp): `org-bookmark-jump-unhide' fn might not be
;; defined at runtime.
(when-fn% 'org-bookmark-jump-unhide 'org
  (autoload 'org-bookmark-jump-unhide "org"))

(with-eval-after-load 'org

  ;; load `ox-reveal' if it had been installed.
  (if-feature-ox-reveal%
    (require 'ox-reveal)
    (setq org-reveal-root
          (let ((f (expand-file-name "~/.reveal.js/")))
            (if (file-exists-p f)
                f
              "https://cdn.jsdelivr.net/reveal.js/3.0.0/")))))



;; EOF
