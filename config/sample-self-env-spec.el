;;;;
;; sample-self-env-spec.el: specify the private environment specs of yourself
;; 
;;;;



;; Basic Emacs' GUI environment configurations:
;; you can customize your `:theme', `:font', `:desktop' or `:socks',
;; all those can be Gited in yourself Git repository.


(def-self-env-spec
  :theme (list :name 'atom-one-dark
               :path (emacs-home* "theme/")
               :allowed t)
  :font (list :name "Monaco-13"
              :allowed t)
  :cjk-font (list :name "Microsoft Yahei"
                  :size 13
                  :allowed nil)
  :env-vars (comment '("JAVA_HOME" "PYTHONPATH"))
  :desktop (list :files-not-to-save
                 ".*\.t?gz\\|\.desktop\\|~$\\|^su:.*\\|^sudo:.*\\|^ssh:.*"
                 :buffers-not-to-save "^TAGS\\|\\.log"
                 :modes-not-to-save '(dired-mode fundamental-mode rmail-mode)
                 :allowed t)
  :socks (list :port 32000
               :server "127.0.0.1"
               :version 5
               :allowed nil))
