;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; boot.el
;;;;
;; Commentary: boot.
;;;;


;;;
;; self-spec*: self specifications
;;;

(defalias '*self-paths*
  (let ((ps `(;; paths
              :prologue ,(emacs-home% "private/self-prologue.el")
              :env-spec ,(emacs-home% "private/self-env-spec.el")
              :mod-spec ,(emacs-home% "private/self-mod-spec.el")
              :epilogue ,(emacs-home% "private/self-epilogue.el")))
        (ss `(;; specs, exclude :prologue
              (:env-spec
               . ,(emacs-home% "config/sample-self-env-spec.el"))
              (:mod-spec
               . ,(emacs-home% "config/sample-self-mod-spec.el"))
              (:epilogue
               . ,(emacs-home% "config/sample-self-epilogue.el")))))
    (lambda (&optional op k v)
      (cond ((and op (eq :get op)) (plist-get ps k))
            ((and op (eq :put op)) (setq ps (plist-put ps k v)))
            ((and op (eq :dup op))
             (inhibit-file-name-handler
               (dolist (fs ss)
                 (let ((dst (plist-get ps (car fs)))
                       (src (cdr fs)))
                   (unless (file-exists-p dst)
                     (copy-file src dst t))))))
            (t ps))))
  "Define the PATH references.\n
No matter the declaration order, the executing order is:
\\=`:env-spec -> :mod-spec -> :epilogue\\='")

(defalias '*self-env-spec*
  (let ((env `( :desktop nil
                :edit nil
                :eshell nil
                :frame nil
                :glyph nil
                :key nil
                :module nil
                :shell nil
                :socks nil
                :theme nil)))
    (lambda (&optional op &rest keys)
      (cond ((and op (eq :get op)) (let ((rs env) (ks keys))
                                     (while ks
                                       (setq rs (plist-get rs (car ks))
                                             ks (cdr ks)))
                                     rs))
            ((and op (eq :put op))
             (setq env (plist-put env (car keys) (cadr keys))))
            (t env)))))

(defalias '*self-mod-spec*
  (let ((ps nil))
    (lambda (&optional op k v)
      (cond ((and op (eq :get op)) (list (assq k ps)))
            ((and op (eq :put op)) (setq ps (cons (cons k v) ps)))
            (t ps)))))

;; end of self-spec* macro

;;;
;; boot
;;;

;; reset user emacs dir
(setq% user-emacs-directory (emacs-home%))
;; let `lexical-binding' var safe under Emacs24.1-
(if-lexical%
    (setq lexical-binding t)
  (safe-local-variable 'lexical-binding))
;; default `:safe'
(setq% enable-local-variables :safe files)
;; make `v-home' .exec/
(v-home! ".exec/")
;; make `v-home' private/
(v-home! "private/")
;; duplicate spec files
(*self-paths* :dup)


;;; <1> prologue
(compile! (compile-unit% (emacs-home* "config/vdir.el"))
          (compile-unit* (*self-paths* :get :prologue)))

;;; <2> env
(compile!
 (compile-unit* (*self-paths* :get :env-spec))
 (compile-unit% (emacs-home* "config/graphic.el"))
 (prog1
     (compile-unit% (emacs-home* "config/shells.el") t)
   (autoload 'self-shell-read! (v-home%> "config/shells"))
   (declare-function self-shell-read! (v-home%> "config/shells"))))

;;; <3> epilogue
(compile!
 (when (*self-env-spec* :get :edit :allowed)
   (prog1
       (compile-unit% (emacs-home* "config/edit.el") t)
     (autoload 'self-edit-init! (v-home%> "config/edit"))
     (declare-function self-edit-init! (v-home%> "config/edit"))))
 (when (*self-env-spec* :get :key :allowed)
   (prog1
       (compile-unit% (emacs-home* "config/key.el") t)
     (autoload 'self-key-init! (v-home%> "config/key"))
     (declare-function self-key-init! (v-home%> "config/key"))))
 (progn
    ;;; --batch mode: disable `desktop'
   (setq% desktop-save-mode nil desktop)
   (when-interactive%
     (when (*self-env-spec* :get :desktop :allowed)
       (prog1
           (compile-unit% (emacs-home* "config/memo.el") t)
         (autoload 'self-desktop-read! (v-home%> "config/memo"))
         (declare-function self-desktop-read! (v-home%> "config/memo"))))))
 (when (*self-env-spec* :get :socks :allowed)
   (prog1
       (compile-unit% (emacs-home* "config/sockets.el") t)
     (autoload 'self-socks-init! (v-home%> "config/sockets"))
     (declare-function self-socks-init! (v-home%> "config/sockets"))))
 (when-package%
   (when (*self-env-spec* :get :module :allowed)
     (prog1
         (compile-unit% (emacs-home* "config/modules.el") t)
       (autoload 'self-module-init! (v-home%> "config/modules"))
       (declare-function self-module-init! (v-home%> "config/modules")))))
 (compile-unit% (emacs-home* "config/autoloads.el")))

;; end of boot


(provide 'boot)

;; end of boot.el
