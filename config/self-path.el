;;;
;;  self-path.el: locate at `~/.emacs.d/private/self-path.el' 
;;;



;; Run in the order: env-spec -> prelogue -> package-spec -> epilogue
;; You can point to your Gited Emacs' configuration files.
;; In `~/.emacs.d/private/' has samples: `self-*.el'

(setq self-def-paths
      (list
       :env-spec (emacs-home* "private/self-env-spec.el")
       :prelogue nil
       :package-spec nil
       :epilogue nil))
