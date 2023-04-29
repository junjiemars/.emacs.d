;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-eglot-autoload.el
;;;;


;; (defmacro-if-feature% eglot)


(when-var% project-find-functions 'project

	(defun project*-try-abs (dir)
		(let ((d (locate-dominating-file dir ".project.el")))
			(when d (list 'vc 'Git d)))))



(with-eval-after-load 'project

  (when-var% project-find-functions 'project
    (push! #'project*-try-abs project-find-functions nil t)))



;;; end of on-eglot-autoload.el
