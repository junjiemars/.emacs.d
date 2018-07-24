;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-inf-clojure.el
;;;;


;; key bindings
;; these help me out with the way I usually develop web apps
(defun cider-start-http-server ()
  (interactive)
  (when-fn% cider-load-current-buffer cider (cider-load-current-buffer))
  (when-fn% cider-current-ns cider
    (let ((ns (cider-current-ns)))
      (when-fn% cider-repl-set-ns cider (cider-repl-set-ns ns))
      (when-fn% cider-interactive-eval cider
	(cider-interactive-eval
	 (format "(println '(def server (%s/start))) (println 'server)"
		 ns))
	(cider-interactive-eval
	 (format "(def server (%s/start)) (println server)"
		 ns))))))


(defun cider-refresh ()
  (interactive)
  (when-fn% cider-interactive-eval cider
    (cider-interactive-eval (format "(user/reset)"))))


(defun cider-user-ns ()
  (interactive)
  (when-fn% cider-repl-set-ns cider (cider-repl-set-ns "user")))


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
