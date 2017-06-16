;;;;
;; self-sample-tiny.el: a tiny self-configuration elisp file
;;   copy it to <user-emacs-directory>private/self.el
;;;;




;; define env-spec
(def-self-env-spec
  :theme (list :name 'atom-one-dark
               :path (emacs-home* "theme/")
               :allowed t))

