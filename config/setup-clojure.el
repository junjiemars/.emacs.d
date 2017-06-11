;;;;
;; Clojure
;;;;




(defun set-clojure-mode! ()
  (enable-eldoc-mode)
  (enable-paredit-mode)
  (subword-mode)
  (rainbow-delimiters-mode)
  (aggressive-indent-mode)
  (inf-clojure-minor-mode))
;; clojure mode hooks
(add-hook 'clojure-mode-hook #'set-clojure-mode!)


;; use clojure mode for other extensions
;; (add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'magic-mode-alist '(".* boot" . clojure-mode))



;;;;
;; inferior
;;;;




(defun set-inf-clojure-mode! ()
  (enable-eldoc-mode)
  (enable-paredit-mode)
  (subword-mode)
  (rainbow-delimiters-mode)
  (aggressive-indent-mode))


(defadvice inf-clojure (before inf-clojure-before compile)
  (platform-supported-when
      windows-nt
    ;; Fix returning nothing in Windows
    (let ((jlinerc "~/.jline.rc"))
      (when (not (file-exists-p jlinerc))
        (write-region "jline.terminal=unsupported" "" jlinerc))))
  ;; minor modes for inf-clojure
  (add-hook 'inf-clojure-mode-hook #'set-inf-clojure-mode!))



;;;;
;; Cider
;;;;




;; Go right to the REPL buffer when it's finished connecting
(safe-setq cider-repl-pop-to-buffer-on-connect t)


;; When there's a cider error, show its buffer and switch to it
(safe-setq cider-show-error-buffer t)
(safe-setq cider-auto-select-error-buffer t)


;; Where to store the cider history.
(safe-setq cider-repl-history-file (v-home! ".cider-history/"))


;; Wrap when navigating history.
(safe-setq cider-repl-wrap-history t)


(defun set-cider-repl-mode! ()
  (enable-eldoc-mode)
  (enable-paredit-mode)
  (rainbow-delimiters-mode)
  (aggressive-indent-mode))


(defadvice cider-jack-in (before cider-jack-in-before compile)
  ;; minor modes for cider
  (add-hook 'cider-repl-mode-hook #'set-cider-repl-mode!))



;; key bindings
;; these help me out with the way I usually develop web apps
(defun cider-start-http-server ()
  (interactive)
  (safe-call cider-load-current-buffer)
  (safe-fn-when cider-current-ns
    (let ((ns (cider-current-ns)))
      (safe-call cider-repl-set-ns ns)
      (safe-fn-when cider-interactive-eval
        (cider-interactive-eval
         (format "(println '(def server (%s/start))) (println 'server)"
                 ns))
        (cider-interactive-eval
         (format "(def server (%s/start)) (println server)"
                 ns))))))


(defun cider-refresh ()
  (interactive)
  (safe-call cider-interactive-eval (format "(user/reset)")))

(defun cider-user-ns ()
  (interactive)
  (safe-call cider-repl-set-ns "user"))



;;;;
;; Figwheel `https://github.com/bhauman/lein-figwheel'
;;;;




(defmacro figwheel-after-load-cider ()
  "Enable Figwheel: cider-jack-in-clojurescript"
  `(safe-setq* cider-cljs-lein-repl
               "(do (require 'figwheel-sidecar.repl-api)
                 (figwheel-sidecar.repl-api/start-figwheel!)
                 (figwheel-sidecar.repl-api/cljs-repl))"))


(eval-after-load 'cider
  '(progn
     (figwheel-after-load-cider)))
