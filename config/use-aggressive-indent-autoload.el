;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-aggressive-indent-autoload.el
;;;;



(feature-allowed-p aggressive-indent
  ;; enable automatically adjust the identation of code

	(with-eval-after-load 'aggressive-indent
		
		(when-fn% global-aggressive-indent-mode aggressive-indent
			(global-aggressive-indent-mode t))
		
		(when-var% aggressive-indent-excluded-modes aggressive-indent
							 (add-to-list 'aggressive-indent-excluded-modes 'org-mode)))
	
	)
