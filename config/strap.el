;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; strap.el
;;;;



;; Let `lexical-binding' var safe under Emacs24.1-
(lexical-supported-unless
  (put 'lexical-binding 'safe-local-variable (lambda (x) t)))


;; preferred coding system
(prefer-coding-system 'utf-8)




;; graphic-supported macro

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

 ;; end of graphic-supported macro


;; platform-supported macro

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

 ;; end of platform-supported macro


(defmacro setq% (x val &optional feature)
  "Set X to the value of its VAL when variable X is bound.
If X requires the FEATURE load it on compile-time."
  ;; (declare (debug t))
  (when feature (require feature nil t))
	(when (boundp x)
		`(setq ,x ,val)))

;; fn compile-time checking macro

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

 ;; end of compile-time checking macro


;; var compile-time checking macro


(defmacro if-var% (var feature then &rest else)
  "If VAR is bounded yields non-nil, do THEN, else do ELSE...
If VAR requires the FEATURE load it on compile-time."
  (declare (indent 3))
  (when feature (require feature nil t))
  (if (boundp var)
      `,then
    `(progn% ,@else)))

(defmacro when-var% (var feature &rest body)
	"Do BODY when VAR is bound.
If VAR requires FEATURE load it on compile-time."
	'	(declare (indent 2))
		`(if-var% ,var ,feature (progn% ,@body)))

 ;; end of var compile-time checking macro

;; byte-compiler macro

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


(defmacro def-function-threading (fn &optional join)
	"Define FN threading macro."
	(if-fn% make-thread nil
					(let ((name (symbol-name fn))
								(name1 (intern (format "function-threading-%s"
																			 (symbol-name fn))))
								(ds1 (format "Threading `%s'." fn)))
						`(defun ,name1 ()
							 ,ds1
							 (let ((thread (make-thread (function ,fn) ,name)))
								 (if ,join
										 (thread-join thread)
									 thread))))
		(ignore* join)
		`(function ,fn)))


 ;; end of byte-compiler macro


;; compile macro

(defmacro compile-unit (file &optional only-compile)
  "Make an unit of compilation."
  `(cons ,file ,only-compile))

(defmacro compile-unit->file (unit)
  "Return the FILE part of `compile-unit'."
  `(car ,unit))

(defmacro compile-unit->only-compile (unit)
  "Return the ONLY-COMPILE indicator of `compile-unit'."
  `(cdr ,unit))

(defmacro compile-lock-owner ()
	"Return the pid of the Emacs process that owns the `+compile-lock-name+' file.

Return nil if no desktop file found or no Emacs process is using it.
DIRNAME omitted or nil means use `desktop-dirname'"
	`(when (file-exists-p +compile-lock-name+)
		 (let ((owner nil))
			 (ignore-errors
				 (with-temp-buffer
					 (insert-file-contents-literally +compile-lock-name+)
					 (goto-char (point-min))
					 (setq owner (read (current-buffer)))))
			 (and (integerp owner) owner))))

(defmacro compile-lock-p (&optional owned)
	"Return t if the `+compile-lock-name+' existing and be OWNED by current Emacs process."
	`(cond (,owned (let ((owner (compile-lock-owner)))
									 (and owner (eql (emacs-pid) owner))))
				 (t (file-exists-p +compile-lock-name+))))

(defun compile-release-lock ()
	"Remove the `+compile-lock-name+' file."
	(when (file-exists-p +compile-lock-name+)
		(delete-file +compile-lock-name+)))


(defun compile! (vdir &rest units)
  "Compile and load the elisp UNITS in VDIR."
  (declare (indent 1))
  (dolist (unit units)
    (when unit
      (compile-and-load-file*
       vdir
       (compile-unit->file unit)
       (compile-unit->only-compile unit)))))

 ;; end of compile macro


;; utils macro

(defmacro make-directory-unless (dir &rest body)
	"Create the DIR directory If DIR does not exists and then do BODY.

See `make-directory'. "
	(declare (indent 1))
	`(let ((d ,dir))
		 (unless (file-exists-p d)
			 (make-directory d t)
			 ,@body)
		 d))


 ;; end of utils macro


;; self-def macro

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
  "Define the PATH references for all specs in `self-path.el'.

If there are no self-path.el under (emacs-home* \"private/\") directory, 
More Reasonable Emacs should create a default one:

(def-self-path-ref
  :env-spec (emacs-home* \"private/self-env-spec.el\")
  :prologue (emacs-home* \"private/self-prologue.el\")
  :package-spec (emacs-home* \"private/self-package-spec.el\")
  :epilogue (emacs-home* \"private/self-epilogue.el\"))

No matter the declaration order, the executing order is:
:env-spec -> :prologue -> :package-spec -> :epilogue
"
  (declare (indent 0))
  `(defvar ,(self-symbol 'path) (list ,@path)
		 "Define the path references which point to 
:env-spec, :prologue, :package-spec and :epilogue in order."))


(defmacro def-self-env-spec (&rest spec)
  "Define default Emacs env SPEC.

If there are no self-path.el under (emacs-home* \"private/\") directory, 
More Reasonable Emacs should create a default (emacs-home* \"private/self-env-spec.el\").

Involves: :theme, :font, :cjk-font, :shell, :eshell, :socks, :package, :edit

Take effect after restart Emacs.
"
  (declare (indent 0))
  `(defvar ,(self-symbol 'env-spec) (list ,@spec)
		 "Define the environment specs."))


(defmacro def-self-package-spec (&rest spec)
  "Define self package SPEC list."
  (declare (indent 0))
  (package-supported-p 
    `(defvar ,(self-symbol 'package-spec) (list ,@spec))))


(defvar self-def-where (self-def-files!)
  "Where's the path of self-path.el")

 ;; end of self-def macro


;; self-spec macro

(compile!
    +v-dir+
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
    +v-dir+
  (compile-unit (self-def-path-ref-> :env-spec)))

 ;; end of self-spec macro


;; Load ui, shell, basic env:

(compile!
    +v-dir+
  (compile-unit (emacs-home* "config/boot.el"))
  (compile-unit (emacs-home* "config/basic.el"))
  (compile-unit (emacs-home* "config/shells.el")))


;; Self do prologue ...
(compile!
    +v-dir+
  (compile-unit (self-def-path-ref-> :prologue)))


(package-supported-p
  ;; (package-initialize)

  ;; Load basic and self modules
  (compile!
      +v-dir+
    (compile-unit (self-def-path-ref-> :package-spec))
    (compile-unit (emacs-home* "config/module.el"))))


;; Load package independent modules
(compile!
    +v-dir+
  (compile-unit (emacs-home* "config/on-module.el"))
  (compile-unit (emacs-home* "config/eshells.el"))
  (compile-unit (emacs-home* "config/autoload.el"))
  ;; --batch mode: disable desktop read/save
  `,(unless noninteractive 
      (compile-unit (emacs-home* "config/memory.el"))))


;; release compile-lock when Emacs exiting
(add-to-list 'kill-emacs-hook #'compile-release-lock)
