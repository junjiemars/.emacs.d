;;;; -*- lexical-binding:t -*-
;;;;
;; strap
;;;;



;; Let `lexical-binding' var safe under Emacs24.1-
(lexical-supported-unless
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
  ;; (declare (debug t))
  (when feature (require feature nil t))
	(when (boundp x)
		`(setq ,x ,val)))


(defmacro if-fn% (fn feature then &rest else)
  "If FN is bounded yields non-nil, do THEN, else do ELSE...
If FN requires the FEATURE load it on compile-time."
  (declare (indent 3))
  (when feature (require feature nil t))
  (if (fboundp fn)
      `,then
    `(progn% ,@else)))


(defmacro when-fn% (fn feature &rest body)
  "Do BODY when FN is bound.
If FN requires FEATURE load it on compile-time."
  (declare (indent 2))
  `(if-fn% ,fn ,feature (progn% ,@body)))


(defmacro unless-fn% (fn feature &rest body)
  "Do BODY unless FN is bound.
If FN requires FEATURE load it on compile-time."
  (declare (indent 2))
  `(if-fn% ,fn ,feature nil ,@body))


(unless-fn% gensym nil
	(defvar *gensym-counter* 0))

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


(defmacro ignore* (&rest vars)
	"Return nil, list VARS at compile time if in lexical context."
	(declare (indent 0))
	(lexical-supported-when
		(when lexical-binding
			`(progn% ,@vars nil))))


(defmacro feature-if% (feature filename then &rest else)
	"If FEATURE supports do THEN, otherwise do ELSE... at compile-time."
	(if (require feature filename t)
			`,then
		`(progn% ,@else)))

(defmacro def-feature-supported-p (feature &optional filename docstring)
	"Define FEATURE supported-p macro."
	(let ((name (intern (format "feature-%s-supported-p" feature)))
				(ds1 (format "If has `%s' feauture then do BODY." feature)))
		`(feature-if% ,feature ,filename
									(defmacro ,name (&rest body)
										,(or docstring ds1)
										(declare (indent 0))
										`(progn% ,@body))
									(defmacro ,name (&rest body)
										,(or docstring ds1)
										(declare (indent 0))
										`(comment ,@body)))))

(defmacro def-function-supported-p (fn &optional feature docstring)
	"Define FN supported-p macro."
	(let ((name (intern (format "function-%s-supported-p" fn)))
				(ds1 (format "If has `%s' fn then do BODY." fn)))
		`(if-fn% ,fn ,feature
						 (defmacro ,name (&rest body)
							 ,(or docstring ds1)
							 (declare (indent 0))
							 `(progn% ,@body))
			 (defmacro ,name (&rest body)
				 ,(or docstring ds1)
				 (declare (indent 0))
				 `(comment ,body)))))





(defmacro compile-unit (file &optional only-compile)
	"Make an unit of compilation."
	`(cons ,file ,only-compile))

(defmacro compile-unit->file (unit)
	"Return the FILE part of `compile-unit'."
	`(car ,unit))

(defmacro compile-unit->only-compile (unit)
	"Return the ONLY-COMPILE indicator of `compile-unit'."
	`(cdr ,unit))


(defun compile! (vdir &rest units)
  "Compile and load the elisp UNITS in VDIR."
  (declare (indent 1))
  (dolist (unit units)
		(when unit
			(compile-and-load-file*
			 vdir
			 (compile-unit->file unit)
			 (compile-unit->only-compile unit)))))


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





(defvar self-def-where (self-def-files!)
  "Where's the path of self-path.el")

;; Load self where
(compile!
		v-dir
	(compile-unit self-def-where))


(defsubst self-def-path-ref-> (&optional key)
  (when (boundp (eval-when-compile (self-symbol 'path)))
    (let ((ref (symbol-value (eval-when-compile (self-symbol 'path)))))
      (if key (plist-get ref key) ref))))


(defmacro self-spec-> (seq &rest keys)
  (declare (indent 1))
  (let ((x seq))
    (dolist (k keys x)
      (setq x (list 'plist-get x k)))))


(defmacro self-spec<- (k v seq &rest keys)
  (declare (indent 3))
  `(plist-put (self-spec-> ,seq ,@keys) ,k ,v))


(defmacro self-spec->% (seq &rest keys)
  (declare (indent 1))
  `(eval-when-compile (self-spec-> ,seq ,@keys)))


(defmacro self-spec->*env-spec (&rest keys)
  (declare (indent 0))
  (when (boundp (self-symbol 'env-spec))
    `(self-spec-> ,(self-symbol 'env-spec) ,@keys)))


(defmacro self-spec->*package-spec (&rest keys)
  (declare (indent 0))
  (when (boundp (self-symbol 'package-spec))
    `(self-spec-> ,(self-symbol 'package-spec) ,@keys)))


(defmacro package-spec-:allowed-p (&rest body)
	(declare (indent 0))
	`(package-supported-p
		 (when (self-spec->*env-spec :package :allowed)
			 ,@body)))


(compile!
    v-dir
  (compile-unit (self-def-path-ref-> :env-spec)))


 ;; end of Load self env


;; Load ui, shell, basic env:
(compile!
    v-dir
  (compile-unit (emacs-home* "config/boot.el"))
  (compile-unit (emacs-home* "config/basic.el"))
  (compile-unit (emacs-home* "config/shells.el")))


;; Self do prologue ...
(compile!
		v-dir
	(compile-unit (self-def-path-ref-> :prologue)))


(package-supported-p
  ;; (package-initialize)

  ;; Load basic and self modules
  (compile!
      v-dir
    (compile-unit (self-def-path-ref-> :package-spec))
    (compile-unit (emacs-home* "config/module.el"))))


;; Load package independent modules
(compile!
    v-dir
  ;; (compile-unit (emacs-home* "config/lldb.el") t)
  (compile-unit (emacs-home* "config/eshells.el"))
  (compile-unit (emacs-home* "config/financial.el") t)
  (compile-unit (emacs-home* "config/pythons.el") t)
  (compile-unit (emacs-home* "config/tags.el") t)
  (compile-unit (emacs-home* "config/cc.el") t)
  (compile-unit (emacs-home* "config/autoloads.el"))
  ;; --batch mode: disable desktop read/save
  `,(unless noninteractive 
      (compile-unit (emacs-home* "config/memory.el"))))


;; Self do epilogue ...
(compile!
    v-dir
  (compile-unit (self-def-path-ref-> :epilogue)))

