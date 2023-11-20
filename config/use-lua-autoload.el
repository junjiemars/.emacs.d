;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-lua-autoload.el
;;;;


(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(push! '("\\.lua$" . lua-mode) auto-mode-alist)
(push! '("lua" . lua-mode) interpreter-mode-alist)


;; end of use-lua-autoload.el
