;;;; -*- lexical-binding:t -*-
;;;;
;; strap
;;;;



;; Let `lexical-binding' var safe under Emacs24.1
(version-supported-when > 24.1
  (put 'lexical-binding 'safe-local-variable (lambda (x) t)))


(defun compile! (vdir &rest files)
  "Compile and load the elisp FILES in VDIR."
  (declare (indent 1))
  (dolist (file files)
    (let ((f (if (atom file) file (car file)))
          (c (if (atom file) nil (cdr file))))
      (when f (compile-and-load-file* vdir f c)))))


(defmacro graphic-supported-if (then &rest else)
  "If in graphic mode, do THEN, else do ELSE...

Returns the value of THEN or the value of the last of the ELSE’s.
THEN must be one expression, but ELSE... can be zero or more expressions.
If in terminal mode, and there are no ELSE’s, the value is nil. "
  (declare (indent 1))
  (if (display-graphic-p)
      `,then
    `(progn% ,@else)))


(defmacro graphic-supported-p (&rest body)
  "Run BODY code if in graphic mode, else returns nil."
  (declare (indent 0))
  `(graphic-supported-if (progn% ,@body)))


(defmacro terminal-supported-p (&rest body)
  "Run BODY code if in terminal mode, else returns nil."
  (declare (indent 0))
  `(graphic-supported-if nil ,@body))





(defsubst self-def-files! ()
  "Returns the path of `(emacs-home* \"private/\" \"self-path.el\")' and make self-*.el files."
  (let ((fs `(,(cons (emacs-home* "private/self-path.el")
                     (emacs-home* "config/sample-self-path.el"))
              ,(cons (emacs-home* "private/self-env-spec.el")
                     (emacs-home* "config/sample-self-env-spec.el"))
              ,(cons (emacs-home* "private/self-package-spec.el")
                     (emacs-home* "config/sample-self-package-spec.el"))
              ,(cons (emacs-home* "private/self-prologue.el")
                     (emacs-home* "config/sample-self-prologue.el"))
              ,(cons (emacs-home* "private/self-epilogue.el")
                     (emacs-home* "config/sample-self-epilogue.el")))))
    (unless (file-exists-p (caar fs))
      (make-directory `,(emacs-home* "private/") t)
      (dolist (f fs)
        (let ((dst (car f)) (src (cdr f)))
          (unless (file-exists-p dst)
            (copy-file src dst t)))))
    (caar fs)))





(defvar self-def-where (self-def-files!)
  "Where's the path of self-path.el")

;; Load self where
(compile!
 v-dir
 self-def-where)


(defmacro self-def-path-ref-> (k)
  `(let ((*s* (self-symbol 'path)))
     (when (boundp *s*)
       (plist-get (symbol-value *s*) ,k))))


(defmacro self-spec-> (seq &rest keys)
  (declare (indent 1))
  (let ((x seq))
    (dolist (k keys x)
      (setq x (list 'plist-get x k)))))


(defmacro self-spec->% (seq &rest keys)
  (declare (indent 1))
  `(eval-when-compile (self-spec-> ,seq ,@keys)))


(defmacro self-spec->* (&rest keys)
  (declare (indent 0))
  `(self-spec-> *val* ,@keys))


(compile!
    v-dir
  (self-def-path-ref-> :env-spec))


 ;; end of Load self env


;; Load ui, shell, basic env:
(compile!
    v-dir
  `,(emacs-home* "config/boot.el")
  `,(emacs-home* "config/basic.el")
  `,(emacs-home* "config/shells.el"))


;; Self do prologue ...
(compile!
 v-dir
 (self-def-path-ref-> :prologue))


(package-supported-p
  ;; (package-initialize)

  ;; Load basic and self modules
  (compile!
      v-dir
    (self-def-path-ref-> :package-spec)
    `,(emacs-home* "config/module.el")))


;; Load package independent modules
(compile!
    v-dir
  ;; `,(cons (emacs-home* "config/debugger.el") t)
  `,(emacs-home* "config/eshells.el")
  `,(cons (emacs-home* "config/financial.el") t)
  `,(cons (emacs-home* "config/pythons.el") t)
  `,(cons (emacs-home* "config/tags.el") t)
  `,(cons (emacs-home* "config/cc.el") t)
  `,(emacs-home* "config/autoloads.el")
  ;; --batch mode: disable desktop read/save
  `,(unless noninteractive (emacs-home* "config/memory.el")))


;; Self do epilogue ...
(compile!
 v-dir
 (self-def-path-ref-> :epilogue))
