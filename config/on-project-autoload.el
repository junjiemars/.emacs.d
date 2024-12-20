;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-project-autoload.el
;;;;

(autoload 'on-project-init! (v-home%> "config/projects"))
(autoload 'project*-root-dirs (v-home%> "config/projects"))

;;; `project' after load
(with-eval-after-load 'project
  (make-thread* #'on-project-init!))

(when-fn% project-find-file project
  (define-global-key% "pf" #'project-find-file))

(when-fn% project-find-regexp project
  (define-global-key% "pg" #'project-find-regexp))

;; end of on-project-autoload.el
