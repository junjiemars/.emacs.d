;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; boot.el
;;;;
;; Commentary: postulates.
;;;;



;;;
;; compile-*: compiling instrument
;;;

(defun compile-unit* (file &optional only-compile)
  "Make an compile unit for \\=`compile!\\='."
  (declare (pure t))
  (and (stringp file)
       (inhibit-file-name-handler (file-exists-p file))
       (let ((u1 (v-comp-file! file)))
         `[,(car u1) ,(cdr u1) ,only-compile nil])))

(defmacro compile-unit% (file &optional only-compile)
  "Make an compile unit at compile time for \\=`compile!\\='"
  (declare (pure t))
  (let* ((-u1- (v-comp-file! (funcall `(lambda () ,file))))
         (-src1- (car -u1-))
         (-dst1- (cdr -u1-)))
    (and -u1- `[,-src1- ,-dst1- ,only-compile nil])))

(defmacro compile-unit->src (unit)
  "Return the :src part of UNIT."
  `(aref ,unit 0))

(defmacro compile-unit->dst (unit)
  "Return the :dst part of UNIT."
  `(aref ,unit 1))

(defmacro compile-unit->only-compile (unit)
  "Return the :only-compile indicator of UNIT."
  `(aref ,unit 2))

(defun compile! (&rest units)
  "Compile and load UNITS."
  (declare (indent 0))
  (dolist* (u units)
    (and u (compile-and-load-file*
            (compile-unit->src u)
            (compile-unit->dst u)
            (compile-unit->only-compile u)))))

;; end of compile-* macro

;;;
;; self-spec*: self specifications
;;;

(defmacro self-spec-> (seq &rest keys)
  "Read spec from SEQ via KEYS."
  (declare (indent 1) (pure t))
  (let ((r seq) (ks keys))
    (while ks
      (setq r (list 'plist-get r (car ks))
            ks (cdr ks)))
    r))

(defmacro self-spec<- (k v seq &rest keys)
  "Save the spec of K V to SEQ via KEYS."
  (declare (indent 3) (pure t))
  `(plist-put (self-spec-> ,seq ,@keys) ,k ,v))

(defmacro self-spec->% (seq &rest keys)
  "Read spec from SEQ via KEYS at compile time."
  (declare (indent 1) (pure t))
  (funcall `(lambda () (self-spec-> ,seq ,@keys))))

(defalias '*self-paths*
  (lexical-let%
      ((ps `(;; paths
             :prologue ,(emacs-home* "private/self-prologue.el")
             :env-spec ,(emacs-home* "private/self-env-spec.el")
             :mod-spec ,(emacs-home* "private/self-mod-spec.el")
             :epilogue ,(emacs-home* "private/self-epilogue.el")))
       (ss `(;; specs, exclude :prologue
             (:env-spec
              . ,(emacs-home* "config/sample-self-env-spec.el"))
             (:mod-spec
              . ,(emacs-home* "config/sample-self-mod-spec.el"))
             (:epilogue
              . ,(emacs-home* "config/sample-self-epilogue.el")))))
    (lambda (&optional op k v)
      (cond ((eq :get op) (plist-get ps k))
            ((eq :put op) (setq ps (plist-put ps k v)))
            ((eq :dup op)
             (inhibit-file-name-handler
               (dolist* (fs ss)
                 (let ((dst (plist-get ps (car fs)))
                       (src (cdr fs)))
                   (unless (file-exists-p dst)
                     (copy-file src dst t))))))
            (t ps))))
  "Define the PATH references.\n
No matter the declaration order, the executing order is:
\\=`:env-spec -> :mod-spec -> :epilogue\\='")

(defalias '*self-env-spec*
  (lexical-let% ((env (list :theme nil
                            :frame nil
                            :glyph nil
                            :key nil
                            :shell nil
                            :desktop nil
                            :eshell nil
                            :socks nil
                            :module nil
                            :edit nil)))
    (lambda (&optional op &rest keys)
      (cond ((eq :get op) (let ((rs env) (ks keys))
                            (while ks
                              (setq rs (plist-get rs (car ks))
                                    ks (cdr ks)))
                            rs))
            ((eq :put op) (setq env (plist-put env (car keys) (cadr keys))))
            (t env)))))

(defalias '*self-mod-spec*
  (lexical-let% ((ps nil))
    (lambda (&optional op k v)
      (cond ((eq :get op) (list (assq k ps)))
            ((eq :put op) (setq ps (cons (cons k v) ps)))
            (t ps)))))

(defmacro shells-spec->% (&rest keys)
  "Extract constant from env-spec via KEYS."
  (declare (indent 0) (pure t))
  `(self-spec->%
    (list :file ,(v-home% ".exec/shell-env.el")
          :SHELL "SHELL"
          :PATH "PATH")
    ,@keys))

(defmacro tags-spec->% (&rest key)
  "Extract value from the list of spec via KEYS at compile time."
  (declare (indent 0) (pure t))
  `(self-spec->% (list
                  :root ,(emacs-home* ".tags/")
                  :nore ,(v-home% ".tags/nore.emacs.TAGS")
                  :emacs ,(v-home% ".tags/emacs.TAGS"))
                 ,@key))

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
(setq% user-emacs-directory (emacs-home*))
;; default `:safe'
(setq% enable-local-variables :safe 'files)
;; let `lexical-binding' var safe under Emacs24.1-
(unless-lexical% (safe-local-variable 'lexical-binding))
;; string hash test: see `%fn:save/read-sexp-to/from-file' in test.el
(define-hash-table-test 'nore-emacs-string-hash= #'string= #'sxhash)

;;; <1> prologue
(compile! (compile-unit% (emacs-home* "config/fn.el"))
          (compile-unit% (emacs-home* "config/vdir.el"))
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
    (setq% desktop-save-mode nil 'desktop)
    (unless-noninteractive%
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
