;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; sample-self-env-spec.el: specify the private environment specs
;; 
;;;;



;; Basic Emacs' environment configurations


(def-self-env-spec
  :theme (list :name 'dracula
               :custom-theme-directory (emacs-home* "theme/")
               :compile nil ;; expert option
               :allowed t)

  :font (list :name (if-platform% 'darwin
                        "Monaco-13"
                      (if-platform% 'windows-nt
                          "Consolas-13"
                        "DejaVu Sans Mono-12")) 
              :allowed nil)

  :cjk-font (list :name (if-platform% 'darwin
                            "Hei"
                          (if-platform% 'windows-nt
                              "Microsoft Yahei"
                            "DejaVu Sans Mono-12"))
                  :size 12
                  :allowed nil)
  
  :shell (list :env-vars `("JAVA_HOME"
                           "PYTHONPATH"
                           "RACKET_HOME"
                           "PATH"
                           ,(unless-platform% 'windows-nt
                              (if-platform% 'darwin
                                  "DYLD_LIBRARY_PATH"
                                "LD_LIBRARY_PATH")))
               :options '("-i" "2>/dev/null") ;; '("--login")
               :exec-path t
               :shell-file-name (eval-when-compile (executable-find "bash"))
               :allowed nil)
  
  :desktop (list :files-not-to-save
                 ".*\.t?gz\\|\.desktop\\|~$\\|^/ssh[x]?:\\|\.elc$"
                 :buffers-not-to-save "^TAGS\\|\\.log"
                 :modes-not-to-save
                 '(dired-mode fundamental-mode eww-mode rmail-mode)
                 :restore-eager 8
                 :restore-via-threading* (if-platform% 'darwin
                                             (unless-graphic% t)
                                           t)
                 :allowed nil)
  
  :eshell (list :visual-commands '("mtr")
                :destroy-buffer-when-process-dies t
                :visual-subcommands nil ;; '(("git" "log"))
                :visual-options nil
                :allowed nil)
  
  :socks (list :port 32000
               :server "127.0.0.1"
               :version 5
               :allowed nil)
  
  :package (list :remove-unused nil
                 :allowed nil)

  :edit (list :tab-width 2
              :auto-save-default nil
              :allowed t)
  
  )
