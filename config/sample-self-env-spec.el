;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; sample-self-env-spec.el: specify the environment specs
;;
;;;;


;; Run order: :env-spec -> :mod-spec -> :epilogue
;; You can point to your Gited Emacs' configuration repo.
;; Default samples `sample-self-*.el' in `(emacs-home% "config/")' directory.
;; :epilogue run in `after-init-hook'


;;; theme
(*self-env-spec*
 :put :theme
 `( :name nil                           ; tango-dark
    :custom-theme-directory nil         ; ,(emacs-home% "theme/")
    :compile nil                        ; expert option
    :allowed nil))

;;; frame
(*self-env-spec*
 :put :frame
 `( :initial ((width . 80)
              (height . 32)
              (font . ,(if-platform% darwin
                           "Monaco-17"
                         (if-platform% windows-nt
                             "Consolas-13"
                           "Monospace-13"))))
    :default nil                        ; ((fullscreen . fullheight))
    :frame-resize-pixelwise nil
    :inhibit-splash-screen t
    :allowed t))


;;; glyph
(*self-env-spec*
 :put :glyph
 `(( :name ,(if-platform% darwin
                "Hack"
              (if-platform% windows-nt
                  "Courier New"
                "DejaVu Sans Mono"))
     :size 17
     :scale nil
     :scripts (greek)
     :allowed (when-graphic% t))
   ( :name ,(if-platform% darwin
                "PingFang"
              (if-platform% windows-nt
                  "SimHei"
                "Noto Sans CJK"))
     :size 12
     :scale nil
     :scripts (han)
     :allowed nil)))


;;; key
(*self-env-spec*
 :put :key
 `( :modifier ,(if-window% mac
                   '( ;; (mac-right-option-modifier . control)
                      ;; (mac-right-command-modifier . meta)
                     (mac-option-modifier . meta))
                 (when-window% ns
                   '( ;; (ns-right-option-modifier . control)
                      ;; (ns-right-command-modifier . meta)
                     (ns-option-modifier . meta))))
    :allowed ,(when-graphic% t)))


;;; shell
(*self-env-spec*
 :put :shell
 `( :copy-vars ("PATH")
    :spin-vars nil              ; (("ZZZ" . "123"))
    :options nil                ; ("-i" "2>/dev/null") ; ("--login")
    :exec-path t
    :shell-file-name ,(or (executable-find% "zsh")
                          (executable-find% "bash"))
    :prompt ( :bash "\\u@\\h \\W \\$ "
              :zsh "%n@%m %1~ %# ")
    :allowed nil))

;;; eshell
(*self-env-spec*
 :put :eshell
 `( :visual-commands ("mtr")
    :destroy-buffer-when-process-dies nil
    :visual-subcommands nil             ; (("git" "log"))
    :visual-options nil
    :allowed nil))

;;; desktop
(*self-env-spec*
 :put :desktop
 `( :files-not-to-save nil              ; "\\(\\`/[^/:]*:\\|(ftp)\\'\\)"
    :buffers-not-to-save nil            ; "\\` "
    :modes-not-to-save nil              ; (tags-table-mode)
    :allowed nil))

;;; edit
(*self-env-spec*
 :put :edit
 `( :tab-width 2
    :narrow-to-region t
    :auto-save-default nil
    :indent ((sh-basic-offset . 2)
             (python-indent-offset . 4))
    :disable-indent-tabs-mode (awk-mode
                               c-mode
                               emacs-lisp-mode
                               mixal-mode
                               scheme-mode
                               sh-mode
                               sql-mode)
    :delete-trailing-whitespace (prog-mode)
    :allowed nil))

;;; socks
(*self-env-spec*
 :put :socks
 `( :port 32000
    :server "127.0.0.1"
    :version 5
    :allowed nil))

(*self-env-spec*
 :put :module
 `( :remove-unused nil
    :package-check-signature allow-unsigned
    :package-archives nil
    (comment
     (("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
      ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
      ("melpa-stable" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/stable-melpa/")
      ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
    :allowed nil))

;; end of sample-self-env-spec.el
