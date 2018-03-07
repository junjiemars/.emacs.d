;;;; -*- lexical-binding:t -*-
;;;;
;; sample-self-env-spec.el: specify the private environment specs
;; 
;;;;



;; Basic Emacs' environment configurations


(def-self-env-spec
  :theme (list :name 'atom-one-dark
               :path (emacs-home* "theme/")
               :allowed t)

  :font (list :name (platform-supported-if darwin
                        "Monaco-13"
                      (platform-supported-if windows-nt
                          "Consolas-13"
                        "DejaVu Sans Mono-12")) 
              :allowed nil)

  :cjk-font (list :name "Microsoft Yahei"
                  :size 13
                  :allowed nil)
  
  :shell (list :env-vars `("JAVA_HOME"
                           "PYTHONPATH"
                           ,(platform-supported-unless windows-nt
                              (platform-supported-if darwin
                                  "DYLD_LIBRARY_PATH"
                                "LD_LIBRARY_PATH")))
               :interactive-shell (platform-supported-unless darwin t nil)
               :exec-path t
               :bin-path (platform-supported-if windows-nt
			     `,(bin-path "bash")
			   "bin/bash")
               :allowed nil)
  
  :desktop (list :files-not-to-save
                 ".*\.t?gz\\|\.desktop\\|~$\\|\\/ssh[: ]"
                 :buffers-not-to-save "^TAGS\\|\\.log"
                 :modes-not-to-save
                 '(dired-mode fundamental-mode rmail-mode)
                 :restore-eager 8
                 :allowed nil)
  
  :eshell (list :visual-commands '("mtr")
                :destroy-buffer-when-process-dies t
                :visual-subcommands '(("git" "log"))
                :visual-options nil
                :allowed nil)
  
  :socks (list :port 32000
               :server "127.0.0.1"
               :version 5
               :allowed nil))
