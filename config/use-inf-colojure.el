;;;; -*- lexical-binding:t -*-
;;;;
;; use-inf-clojure
;;;;


;; key bindings
;; these help me out with the way I usually develop web apps
(defun cider-start-http-server ()
  (interactive)
  (safe-call cider-load-current-buffer)
  (when-fn% cider-current-ns cider
    (let ((ns (cider-current-ns)))
      (safe-call cider-repl-set-ns ns)
      (when-fn% cider-interactive-eval cider
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
  `(setq% cider-cljs-lein-repl
	  "(do (require 'figwheel-sidecar.repl-api)
                 (figwheel-sidecar.repl-api/start-figwheel!)
                 (figwheel-sidecar.repl-api/cljs-repl))" cider))


(eval-after-load 'cider
  '(progn
     (figwheel-after-load-cider)))


(defun figwheel-repl ()
  (interactive)
  (inf-clojure "lein figwheel"))


(add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)
