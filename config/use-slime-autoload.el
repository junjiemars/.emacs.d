;;;; -*- lexical-binding:t -*-
;;;;
;; use-slime-autoload
;;;;



(with-eval-after-load 'slime
	(when-fn% set-slime-lisp-implementations! use-slime
		(require 'use-slime)
		(set-slime-lisp-implementations!))
	(when-fn% slime-setup slime
		(slime-setup '(slime-fancy slime-asdf)))
	(when-fn% slime-selector slime
    (global-set-key (kbd "C-c s s") #'slime-selector)))

