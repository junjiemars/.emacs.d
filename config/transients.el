;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; transients.el
;;;;

(defun on-transient-init! ()
  (v-home! ".transient/history.el")
  (setq transient-history-file (v-home% ".transient/history.el")
        transient-levels-file (v-home% ".transient/levels.el")
        transient-values-file (v-home%".transient/values.el")
        transient-save-history t))

;; end of transients.el
