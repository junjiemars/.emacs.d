;;;;
;; self-sample.el: specified yourself private configuration elisp file
;;                 and named it with self.el
;;;;


(package-supported-p 
  (defmacro def-self-package-spec ()
    (let ((--spec-- (self-symbol 'package-spec)))
      `(defvar ,--spec--
         (list
          (list
           :cond (lambda ()
                   (bin-exists-p "latex"))
           :packages '(auctex))
          (list
           :cond (lambda ()
                   (and (version-supported-p '<= 24.4)
                        (bin-exists-p "java")))
           :packages '(cider
                       clojure-mode
                       clojure-mode-extra-font-locking
                       inf-clojure)
           :setup '("setup-clojure.el"))
          (list
           :cond (lambda ()
                   (and (version-supported-p '<= 24.4)
                        (bin-exists-p "docker")))
           :packages '(dockerfile-mode
                       docker-tramp))
          (list
           :cond (lambda ()
                   (bin-exists-p "erlang"))
           :packages '(erlang))
          (list
           :cond (lambda ()
                   (and (bin-exists-p "erlang")
                        (bin-exists-p "lfe")))
           :packages '(lfe-mode)
           :setup '("setup-lfe.el"))
          (list
           :cond (lambda ()
                   (version-supported-p '<= 25.1))
           :packages '(ereader))
          (list
           :cond (lambda ()
                   (and (version-supported-p '<= 24.4)
                        (bin-exists-p "git")))
           :packages '(magit)
           :setup '("setup-magit.el"))
          (list
           :cond (lambda ()
                   (and (version-supported-p '<= 23.2)
                        (bin-exists-p "racket")))
           :packages '(geiser))
          (list
           :cond (lambda ()
                   (or (bin-exists-p "sbcl")))
           :packages '(slime)
           :setup '("setup-slime.el"))
          (list
           :cond (lambda () t)
           :packages '(sx)))))))


(comment
 (platform-supported-when
     gnu/linux
   (defvar self-gnu/linux-font "White Rabbit-12"
     "default font-size for gnu/linux")
   (defvar self-gnu/linux-cjk-font (cons "Microsoft Yahei" 12)
     "default cjk font for gnu/linux")
   (defvar self-gnu/linux-theme 'tomorrow-night-eighties
     "default theme for linux")
   
   (defvar self-gnu/linux-prelogue
     (lambda () (message "#self prelogue ...")))

   (defvar self-gnu/linux-epilogue
     (lambda ()
       ;;(start-socks)
       (message "#self epilogue ...")))))

(comment
 (platform-supported-when
     darwin
   (defvar self-darwin-font "Monaco-13"
     "default font-size for darwin")
   (defvar self-darwin-theme 'tomorrow-night-eighties
     "default theme for darwin")
   (defvar self-darwin-packages '(geiser clojure lfe)
     "default packages for darwin")
   
   (defvar self-darwin-prelogue
     (lambda () (message "#self prelogue ...")))

   (defvar self-darwin-epilogue
     (lambda ()
       ;;(start-socks)
       (message "#self epilogue ...")))))

(comment
 (platform-supported-when
     windows-nt
   (defvar self-windows-nt-font "Consolas-13"
     "default font-size for windows nt")
   (defvar self-windows-nt-cjk-font (cons "Microsoft Yahei" 12))
   
   (defvar self-windows-nt-prelogue
     (lambda () (message "#self prelogue ...")))

   (defvar self-windows-nt-epilogue
     (lambda ()
       ;;(start-socks)
       (message "#self epilogue ...")))))
