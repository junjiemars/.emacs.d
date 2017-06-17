;;;;
;; self-sample-tiny.el: a tiny self-configuration elisp file
;;   in `private/self-path' let `def-self-path' to it.
;;   
;;;;




;; define env-spec
(def-self-env-spec
  :theme (list :name 'atom-one-dark
               :path (emacs-home* "theme/")
               :allowed t)
  :desktop (list :files-not-to-save "\.el\.gz\\|~$"
                 :buffers-not-to-save "^TAGS\\|\\.log"
                 :modes-not-to-save '(dired-mode)
                 :allowed t))


(def-self-prelogue
  (message "#self prelogue ..."))


(def-self-epilogue
  (message "#self epilogue ..."))
