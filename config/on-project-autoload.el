;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-project-autoload.el
;;;;

(declare-function on-project-init! (v-home%> "config/projects.el"))


;;; `project' after load
(with-eval-after-load 'project
  (on-project-init!))

(when-fn% 'project-find-file 'project
  (define-key% (current-global-map) (kbd "C-x p f")
               #'project-find-file))


;; end of on-project-autoload.el
