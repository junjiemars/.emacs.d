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





(defmacro theme-supported-p (&rest body)
  (declare (indent 1))
  `(graphic-supported-p
     (version-supported-when
         < 23
       ,@body)))


(defmacro font-supported-p (&rest body)
  (declare (indent 1))
  `(graphic-supported-p
     ,@body))


(defsubst self-def-files! ()
  "Returns the path of `(emacs-home* \"private/\" \"self-path.el\")' and make self-*.el files."
  (let ((d (emacs-home* "private/"))
        (p (emacs-home* "private/self-path.el"))
        (fs `(,(cons (emacs-home* "private/self-path.el")
                     (emacs-home* "config/sample-self-path.el"))
              ,(cons (emacs-home* "private/self-env-spec.el")
                     (emacs-home* "config/sample-self-env-spec.el"))
              ,(cons (emacs-home* "private/self-package-spec.el")
                     (emacs-home* "config/sample-self-package-spec.el"))
              ,(cons (emacs-home* "private/self-prologue.el")
                     (emacs-home* "config/sample-self-prologue.el"))
              ,(cons (emacs-home* "private/self-epilogue.el")
                     (emacs-home* "config/sample-self-epilogue.el")))))
    (when (not (file-exists-p p))
      (when (not (file-exists-p d)) (make-directory d t))
      (dolist (f fs)
        (let ((dst (car f)) (src (cdr f)))
          (when (not (file-exists-p dst))
            (copy-file src dst t)))))
    p))





(defvar self-def-where (self-def-files!)
  "Where's the path of self-path.el")

;; Load self where
(compile!
 v-dir
 self-def-where)


(defmacro self-def-path-ref-> (k)
  `(plist-get (symbol-value (self-symbol 'path)) ,k))


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
  (emacs-home* "config/shim.el"))


(compile!
 v-dir
 (self-def-path-ref-> :env-spec))


 ;; end of Load self env


;; Load ui, shell, basic env:
(compile!
 v-dir
 (emacs-home* "config/boot.el")
 (emacs-home* "config/shells.el")
 (emacs-home* "config/basic.el"))


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
   (emacs-home* "config/module.el")))


;; Load package independent modules
(compile!
    v-dir
  (cons (emacs-home* "config/debugger.el") t)
  (emacs-home* "config/editing.el")
  (cons (emacs-home* "config/financial.el") t)
  (emacs-home* "config/pythons.el")
  (cons (emacs-home* "config/tags.el") t)
  (cons (emacs-home* "config/cc.el") t)
  ;; --batch mode: disable desktop read/save
  (unless noninteractive (emacs-home* "config/memory.el")))


;; Self do epilogue ...
(compile!
 v-dir
 (self-def-path-ref-> :epilogue))
