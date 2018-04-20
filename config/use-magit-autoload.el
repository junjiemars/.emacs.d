;;;; -*- lexical-binding:t -*-
;;
;; use-magit-autoload
;;


(defun use-magit! ()
	(when-fn% magit-pull magit
		(global-set-key (kbd "C-c g p") 'magit-pull))
	(when-fn% magit-push magit
		(global-set-key (kbd "C-c g P") 'magit-push))
	(when-fn% magit-log magit
		(global-set-key (kbd "C-c g l") 'magit-log))
	(when-fn% magit-log-buffer-file magit
		(global-set-key (kbd "C-c g b") 'magit-log-buffer-file))
	(when-fn% magit-checkout magit
		(global-set-key (kbd "C-c g c") 'magit-checkout))
	(when-fn% magit-merge magit
		(global-set-key (kbd "C-c g m") 'magit-merge))
	(when-fn% magit-fetch magit
		(global-set-key (kbd "C-c g f") 'magit-fetch)))


(with-eval-after-load 'magit
	(use-magit!))


(when-fn% magit-status magit
  (global-set-key (kbd "C-c g s") #'magit-status))
