;;;; -*- lexical-binding:t -*-
;;;;
;; strap
;;;;



;; Let `lexical-binding' var safe under Emacs24.1-
(version-supported-when > 24.1
  (put 'lexical-binding 'safe-local-variable (lambda (x) t)))

;; preferred coding system
(prefer-coding-system 'utf-8)




;; platform and graphic checking macro

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


(defmacro platform-supported-if (os then &rest else)
  "If (eq system-type OS) yields non-nil, do THEN, else do ELSE...

Returns the value of THEN or the value of the last of the ELSE’s.
THEN must be one expression, but ELSE... can be zero or more expressions.
If (eq `system-type' OS) yields nil, and there are no ELSE’s, the value is nil. "
  (declare (indent 2))
  (if (eq system-type os)
      `,then
    `(progn% ,@else)))


(defmacro platform-supported-when (os &rest body)
  "Run BODY code if on specified OS platform, else return nil."
  (declare (indent 1))
  `(platform-supported-if ,os (progn% ,@body)))


(defmacro platform-supported-unless (os &rest body)
  "Run BODY code unless on specified OS platform, else return nil."
  (declare (indent 1))
  `(platform-supported-if ,os nil ,@body))


(defmacro setq% (x val &optional feature)
  "Set X when variable X is bound.
If X requires the FEATURE load it on compile-time."
  (declare (debug t))
  (when feature (require feature))
  (when (boundp x)
    `(setq ,x ,val)))


(defmacro if-fn% (fn feature then &rest else)
  "If FN is bounded yields non-nil, do THEN, else do ELSE...
If FN requires the FEATURE load it on compile-time."
  (declare (indent 3) (debug t))
  (when feature (require feature))
  (if (fboundp fn)
      `,then
    `(progn% ,@else)))


(defmacro when-fn% (fn feature &rest body)
  "Do BODY when FN is bound.
If FN requires FEATURE load it on compile-time."
  (declare (indent 2) (debug t))
  `(if-fn% ,fn ,feature (progn% ,@body)))


(defmacro unless-fn% (fn feature &rest body)
  "Do BODY unless FN is bound.
If FN requires FEATURE load it on compile-time."
  (declare (indent 2) (debug t))
  `(if-fn% ,fn ,feature nil ,@body))


(defvar *gensym-counter* 0)

(unless-fn% gensym nil
	;; feature Emacs version will add `gensym' into the core
	;; but now using cl-gensym indeed
	(defun gensym (&optional prefix)
	  "Generate a new uninterned symbol.
The name is made by appending a number to PREFIX, default \"G\"."
	  (let ((pfix (if (stringp prefix) prefix "G"))
		(num (if (integerp prefix) prefix
		       (prog1 *gensym-counter*
			 (setq *gensym-counter* (1+ *gensym-counter*))))))
	    (make-symbol (format "%s%d" pfix num)))))





(defun compile! (vdir &rest files)
  "Compile and load the elisp FILES in VDIR."
  (declare (indent 1))
  (dolist (file files)
    (let ((f (if (atom file) file (car file)))
          (c (if (atom file) nil (cdr file))))
      (when f (compile-and-load-file* vdir f c)))))


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


(defmacro self-symbol (name)
  `(intern (format "self-%s-%s" system-type ,name)))


(defmacro def-self-path-ref (&rest path)
  "Define the PATH references for all specs in `self-path.el'."
  (declare (indent 0))
  `(defvar ,(self-symbol 'path) (list ,@path)))


(defmacro def-self-env-spec (&rest spec)
  "Define default Emacs env SPEC of current platform on current `emacs-version'."
  (declare (indent 0))
  `(defvar ,(self-symbol 'env-spec) (list ,@spec)))


(defmacro def-self-package-spec (&rest spec)
  "Define self package SPEC list."
  (declare (indent 0))
  (package-supported-p 
    `(defvar ,(self-symbol 'package-spec) (list ,@spec))))


(defmacro self-safe-call (var &rest body)
  (when (boundp (self-symbol var))
    `(let ((*val* (symbol-value (self-symbol ,var))))
       (when *val* ,@body))))





(defvar self-def-where (self-def-files!)
  "Where's the path of self-path.el")

;; Load self where
(compile!
 v-dir
 self-def-where)


(defun self-def-path-ref-> (k)
  (when (boundp (eval-when-compile (self-symbol 'path)))
    (plist-get (symbol-value (eval-when-compile (self-symbol 'path))) k)))


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
