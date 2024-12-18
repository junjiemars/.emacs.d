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

(defmacro self-spec-> (seq &rest keys)
  "Read spec from SEQ via KEYS."
  (declare (indent 1))
  (let ((r seq) (ks keys))
    (while ks
      (setq r (list 'plist-get r (car ks))
            ks (cdr ks)))
    r))

(defmacro self-spec<- (k v seq &rest keys)
  "Save the spec of K V to SEQ via KEYS."
  (declare (indent 3))
  `(plist-put (self-spec-> ,seq ,@keys) ,k ,v))

(defalias '*self-paths*
  (lexical-let%
      ((ps `(;; paths
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
      (cond ((eq :get op) (plist-get ps k))
            ((eq :put op) (setq ps (plist-put ps k v)))
            ((eq :dup op)
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
  (lexical-let% ((env (list :desktop nil
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
      (cond ((eq :get op) (let ((rs env) (ks keys))
                            (while ks
                              (setq rs (plist-get rs (car ks))
                                    ks (cdr ks)))
                            rs))
            ((eq :put op) (setq env (plist-put env (car keys) (cadr keys))))
            (t env)))))

(defmacro env-spec->* (&rest keys)
  "Extract spec from \\=`*self-env-spec*\\=' via KEYS."
  `(self-spec-> (*self-env-spec*) ,@keys))

(defalias '*self-mod-spec*
  (lexical-let% ((ps nil))
    (lambda (&optional op k v)
      (cond ((eq :get op) (list (assq k ps)))
            ((eq :put op) (setq ps (cons (cons k v) ps)))
            (t ps)))))

;; end of self-spec* macro

;;;
;; boot
;;;

;; disable `package' at startup
(when-package% (setq package-enable-at-startup nil))
;; make `v-home' .exec/
(v-home! ".exec/")
;; make `v-home' private/
(v-home! "private/")
;; duplicate spec files
(*self-paths* :dup)
;; reset user emacs dir
(setq% user-emacs-directory (emacs-home%))
;; default `:safe'
(setq% enable-local-variables :safe files)
;; let `lexical-binding' var safe under Emacs24.1-
(unless-lexical% (safe-local-variable 'lexical-binding))
;; string hash test: see `%fn:save/read-sexp-to/from-file' in test.el
(define-hash-table-test 'nore-emacs-string-hash= #'string= #'sxhash)

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
  (when (env-spec->* :edit :allowed)
    (prog1
        (compile-unit% (emacs-home* "config/edit.el") t)
      (autoload 'self-edit-init! (v-home%> "config/edit"))
      (declare-function self-edit-init! (v-home%> "config/edit"))))
  (when (env-spec->* :key :allowed)
    (prog1
        (compile-unit% (emacs-home* "config/key.el") t)
      (autoload 'self-key-init! (v-home%> "config/key"))
      (declare-function self-key-init! (v-home%> "config/key"))))
  (progn
    ;;; --batch mode: disable `desktop'
    (setq% desktop-save-mode nil desktop)
    (when-interactive%
      (when (env-spec->* :desktop :allowed)
        (prog1
            (compile-unit% (emacs-home* "config/memo.el") t)
          (autoload 'self-desktop-read! (v-home%> "config/memo"))
          (declare-function self-desktop-read! (v-home%> "config/memo"))))))
  (when (env-spec->* :socks :allowed)
    (prog1
        (compile-unit% (emacs-home* "config/sockets.el") t)
      (autoload 'self-socks-init! (v-home%> "config/sockets"))
      (declare-function self-socks-init! (v-home%> "config/sockets"))))
  (when-package%
    (when (env-spec->* :module :allowed)
      (prog1
          (compile-unit% (emacs-home* "config/modules.el") t)
        (autoload 'self-module-init! (v-home%> "config/modules"))
        (declare-function self-module-init! (v-home%> "config/modules")))))
  (compile-unit% (emacs-home* "config/autoloads.el")))

;; end of boot


(provide 'boot)

;; end of boot.el
