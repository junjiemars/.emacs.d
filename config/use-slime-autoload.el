;;;; -*- lexical-binding:t -*-
;;;;
;; use-slime-autoload
;;;;



(with-eval-after-load 'slime
	(set-slime-lisp-implementations!)
  (slime-setup '(slime-fancy slime-asdf))
	(when-fn% slime-selector slime
    (global-set-key (kbd "C-c s s") #'slime-selector)))

