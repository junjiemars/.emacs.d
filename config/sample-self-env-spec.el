;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; sample-self-env-spec.el: specify the private environment specs
;;
;;;;


;; Run order: :env-spec -> :package-spec -> :epilogue
;; You can point to your Gited Emacs' configuration repo.
;; Default samples `sample-self-*.el' in `(emacs-home* "config/")' directory.
;; :epilogue run in `after-init-hook'


;;; theme
(*self-env-spec*
 :put :theme
 (list :name nil ;; 'dracula
       :custom-theme-directory (emacs-home* "theme/")
       :compile nil ;; expert option
       :allowed nil))

;;; frame
(*self-env-spec*
 :put :frame
 (list :initial `((width . 80)
                  (height . 32)
                  (font . ,(if-platform% 'darwin
                               "Monaco-17"
                             (if-platform% 'windows-nt
                                 "Consolas-13"
                               "Monaco-13"))))
       :default nil ;; `((fullscreen . fullheight))
       :frame-resize-pixelwise nil
       :inhibit-splash-screen nil
       :allowed t))


;;; glyph
(*self-env-spec*
 :put :glyph
 `((:name ,(if-platform% 'darwin
               "Hack"
             (if-platform% 'windows-nt
                 "Courier New"
               "DejaVu Sans Mono"))
          :size 17
          :scale nil
          :scripts (greek)
          :allowed t)
   (:name ,(if-platform% 'darwin
               "PingFang"
             (if-platform% 'windows-nt
                 "Microsoft YaHei"
               "Noto Sans"))
          :size 12
          :scale nil
          :scripts (han)
          :allowed nil)))


;;; key
(*self-env-spec*
 :put :key
 (list :modifier `(,(if-platform% 'darwin
                        '(mac-option-modifier . meta)
                      '(ns-option-modifier . meta))
                   ; (ns-right-option-modifier . control)
                   ; (ns-right-command-modifier . meta)
                   )
       :allowed (when-version% <= 23 (when-graphic% t))))


;;; shell
(*self-env-spec*
 :put :shell
 (list :copy-vars `("PATH")
       :spin-vars nil     ;; `(("ZZZ" . "123"))
       :options nil       ;; '("-i" "2>/dev/null") ;; '("--login")
       :exec-path t
       :shell-file-name (unless-platform% 'windows-nt
                          (or (executable-find% "zsh")
                              (executable-find% "bash")))
       :prompt (unless-platform% 'windows-nt
                 (list :bash "\\u@\\h \\W \\$ "
                       :zsh "%n@%m %1~ %# "))
       :allowed t))

;;; desktop
(*self-env-spec*
 :put :desktop
 (list :files-not-to-save
       "\.t?gz$\\|\.zip$\\|\.desktop\\|~$\\|^/sudo:\\|^/ssh[x]?:\\|\.elc$"
       :buffers-not-to-save "^TAGS\\|\\.log"
       :modes-not-to-save
       '(dired-mode fundamental-mode eww-mode rmail-mode tags-table-mode)
       :restore-eager 8
       :allowed nil))

;;; eshell
(*self-env-spec*
 :put :eshell
 (list :visual-commands '("mtr")
       :destroy-buffer-when-process-dies nil
       :visual-subcommands nil ;; '(("git" "log"))
       :visual-options nil
       :allowed nil))

;;; socks
(*self-env-spec*
 :put :socks
 (list :port 32000
       :server "127.0.0.1"
       :version 5
       :allowed nil))

(*self-env-spec*
 :put :package
 (list :remove-unused nil
       :package-check-signature 'allow-unsigned
       :allowed nil))

;;; edit
(*self-env-spec*
 :put :edit
 (list :tab-width 2
       :narrow-to-region t
       :auto-save-default nil
       :indent '((sh-basic-offset . 2)
                 (python-indent-offset . 4))
       :disable-indent-tabs-mode '(c-mode-common-hook
                                   sh-mode-hook
                                   emacs-lisp-mode-hook
                                   scheme-mode-hook)
       :delete-trailing-whitespace '(prog-mode)
       :allowed nil))

;;; eof
