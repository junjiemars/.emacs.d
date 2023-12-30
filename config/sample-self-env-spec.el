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
 (list :name nil ; 'tango-dark
       :custom-theme-directory nil ; (emacs-home* "theme/")
       :compile nil ; expert option
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
       :default nil ; `((fullscreen . fullheight))
       :frame-resize-pixelwise nil
       :inhibit-splash-screen t
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
          :allowed nil)
   (:name ,(if-platform% 'darwin
               "PingFang"
             (if-platform% 'windows-nt
                 "SimHei"
               "Noto Sans CJK"))
          :size 12
          :scale nil
          :scripts (han)
          :allowed nil)))


;;; key
(*self-env-spec*
 :put :key
 (list :modifier (when-window% 'mac
                   '((mac-option-modifier . meta)
                     (mac-right-option-modifier . control)
                     (mac-right-command-modifier . meta)))
       :allowed nil))


;;; shell
(*self-env-spec*
 :put :shell
 (list :copy-vars `("PATH")
       :spin-vars nil     ; `(("ZZZ" . "123"))
       :options nil       ; '("-i" "2>/dev/null") ; '("--login")
       :exec-path t
       :shell-file-name (or (executable-find% "zsh")
                            (executable-find% "bash"))
       :prompt (list :bash "\\u@\\h \\W \\$ "
                     :zsh "%n@%m %1~ %# ")
       :allowed nil))

;;; desktop
(*self-env-spec*
 :put :desktop
 (list :files-not-to-save
       "\\.desktop\\|~$\\|^/sudo:\\|^/ssh[x]?:\\|\\.el[cn]$"
       :buffers-not-to-save "\\.?[Tt][Aa][Gg][Ss]\\|\\.[Ll][Oo][Gg]"
       :modes-not-to-save
       '(dired-mode fundamental-mode)
       :allowed nil))

;;; eshell
(*self-env-spec*
 :put :eshell
 (list :visual-commands '("mtr")
       :destroy-buffer-when-process-dies nil
       :visual-subcommands nil ; '(("git" "log"))
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
       :safe-local-variable nil
       :disable-indent-tabs-mode '(c-mode
                                   sh-mode
                                   emacs-lisp-mode
                                   scheme-mode)
       :delete-trailing-whitespace '(prog-mode)
       :allowed nil))

;;; eof
