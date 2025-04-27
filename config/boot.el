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

(defun self-path--dup (paths keys)
  (inhibit-file-name-handler
    (while (car keys)
      (let ((dst (plist-get paths (caar keys)))
            (src (cdar keys)))
        (unless (file-exists-p dst)
          (copy-file src dst))
        (setq keys (cdr keys))))))

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
            ((and op (eq :dup op)) (self-path--dup ps ss))
            (t ps))))
  "Define the PATH references.\n
No matter the declaration order, the executing order is:
\\=`:env-spec -> :mod-spec -> :epilogue\\='")

(defun self-env--get (env keys)
  (let ((rs env) (ks keys))
    (while ks
      (setq rs (plist-get rs (car ks))
            ks (cdr ks)))
    rs))

(defun self-env--put (env keys)
  (plist-put env (car keys) (cadr keys)))

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
      (cond ((and op (eq :get op)) (self-env--get env keys))
            ((and op (eq :put op)) (setq env (self-env--put env keys)))
            (t env)))))

(defun self-mod--put (ps k v)
  (let ((a1 (assq k ps)))
    (if a1
        (progn (setcdr a1 v) ps)
      (cons (cons k v) ps))))

(defalias '*self-mod-spec*
  (let ((ps nil))
    (lambda (&optional op k v)
      (cond ((and op (eq :get op)) (assq k ps))
            ((and op (eq :put op)) (setq ps (self-mod--put ps k v)))
            (t ps)))))

;; end of self-spec* macro

;;;
;; boot
;;;

(defun boot! ()

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

;;; <3> elements
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
   (when-graphic%
     (when (*self-env-spec* :get :glyph :allowed)
       (prog1
           (compile-unit% (emacs-home* "config/glyph.el") t)
         (autoload 'self-glyph-init! (v-home%> "config/glyph"))
         (declare-function self-glyph-init! (v-home%> "config/glyph")))))
   (progn
     ;; --batch mode: disable `desktop'
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
         (declare-function self-module-init! (v-home%> "config/modules"))))))

;;; <4> epilogue
  (compile! (compile-unit% (emacs-home* "config/autoloads.el")))
  (if-interactive%
      (set 'after-init-hook `(on-autoloads!))
    (declare-function on-autoloads! (v-home%> "config/autoload"))
    (on-autoloads!)))

;; end of boot


(provide 'boot)

;; end of boot.el
