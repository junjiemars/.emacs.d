;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; boot.el: compile and load elisp files
;;;;


(defmacro more-reasonable-emacs ()
  "More Reasonable Emacs git repo"
  "https://github.com/junjiemars/.emacs.d")


;;; Load cl-lib/cl at compile-time
(eval-when-compile
  (if-version% <= 24
               (require 'cl-lib)
    (require 'cl)))

;;;;
;; *-lexical macro
;;;;

(defmacro if-lexical% (then &rest else)
  "If current Emacs supports lexical binding do THEN,
otherwise do ELSE..."
  (declare (indent 1))
  `(if-version%
       <= 24.1
       ,then
     (progn% ,@else)))

(defmacro when-lexical% (&rest body)
  "When current Emacs supports lexical binding do BODY."
  (declare (indent 0))
  `(if-lexical% (progn% ,@body)))

(defmacro unless-lexical% (&rest body)
  "Unless current Emacs supports lexical binding do BODY."
  (declare (indent 0))
  `(if-lexical% nil ,@body))

;;; Let `lexical-binding' var safe under Emacs24.1-
(unless-lexical%
  (put 'lexical-binding 'safe-local-variable (lambda (_) t)))


 ;; end of *-lexical% macro

;;;;
;; *-graphic% macro
;;;;

(defmacro if-graphic% (then &rest else)
  "If in graphic mode, do THEN, else do ELSE...

Returns the value of THEN or the value of the last of the ELSE’s.
THEN must be one expression, but ELSE... can be zero or more expressions.
If in terminal mode, and there are no ELSE’s, the value is nil. "
  (declare (indent 1))
  (if (display-graphic-p)
      `,then
    `(progn% ,@else)))


(defmacro when-graphic% (&rest body)
  "Run BODY code if in graphic mode, else returns nil."
  (declare (indent 0))
  `(if-graphic% (progn% ,@body)))


(defmacro unless-graphic% (&rest body)
  "Run BODY code if in terminal mode, else returns nil."
  (declare (indent 0))
  `(if-graphic% nil ,@body))

 ;; end of *-graphic% macro


;;;;
;; *-platform% macro
;;;;

(defmacro if-platform% (os then &rest else)
  "If \(eq system-type OS\) yields non-nil, do THEN, else do ELSE...

Returns the value of THEN or the value of the last of the ELSE’s.
THEN must be one expression, but ELSE... can be zero or more expressions.
If (eq `system-type' OS) yields nil, and there are no ELSE’s, the value is nil. "
  (declare (indent 2))
  `(if% (eq system-type ,os)
       ,then
     (progn% ,@else)))

(defmacro when-platform% (os &rest body)
  "Run BODY code if on specified OS platform, else return nil."
  (declare (indent 1))
  `(if-platform% ,os (progn% ,@body)))

(defmacro unless-platform% (os &rest body)
  "Run BODY code unless on specified OS platform, else return nil."
  (declare (indent 1))
  `(if-platform% ,os nil ,@body))

 ;; end of if-platform% macro


;;;;
;; fn compile-time checking macro
;;;;

(defmacro if-fn% (fn feature then &rest else)
  "If FN is bounded yields non-nil, do THEN, else do ELSE...
If FN requires the FEATURE, load it on compile-time."
  (declare (indent 3))
  `(if% (or (and ,feature (require ,feature nil t) (fboundp ,fn))
            (fboundp ,fn))
       ,then
     (progn% ,@else)))


(defmacro when-fn% (fn feature &rest body)
  "Do BODY when FN is bound.
If FN requires the FEATURE, load it on compile-time."
  (declare (indent 2))
  `(if-fn% ,fn ,feature (progn% ,@body)))


(defmacro unless-fn% (fn feature &rest body)
  "Do BODY unless FN is bound.
If FN requires the FEATURE, load it on compile-time."
  (declare (indent 2))
  `(if-fn% ,fn ,feature nil ,@body))


 ;; end of fn compile-time checking macro


;;;;
;; var compile-time checking macro
;;;;

(defmacro setq% (x val &optional feature)
  "Set X to the value of its VAL when variable X is bound.
If X requires the FEATURE, load it on compile-time."
  ;; (declare (debug t))
  `(when% (or (and ,feature (require ,feature nil t) (boundp ',x))
              (boundp ',x))
     (setq ,x ,val)))

(defmacro if-var% (var feature then &rest else)
  "If VAR is bounded yields non-nil, do THEN, else do ELSE...
If VAR requires the FEATURE, load it on compile-time."
  (declare (indent 3))
  `(if% (or (and ,feature (require ,feature nil t) (boundp ',var))
            (boundp ',var))
       ,then
     (progn% ,@else)))

(defmacro when-var% (var feature &rest body)
  "Do BODY when VAR is bound.
If VAR requires the FEATURE, load it on compile-time."
  (declare (indent 2))
  `(if-var% ,var ,feature (progn% ,@body)))

(defmacro unless-var% (var feature &rest body)
  "Do BODY unless VAR is bound.
If VAR requires the FEATURE, load it on compile-time."
  (declare (indent 2))
  `(if-var% ,var ,feature nil (progn% ,@body)))


 ;; end of var compile-time checking macro


;; Preferred coding system
(prefer-coding-system 'utf-8)




;;;;
;; compile macro
;;;;

(defmacro compile-unit* (file &optional only-compile delete-booster)
  "Make an unit of compilation."
  `(list :source ,file
         :dir (when ,file (v-path* (file-name-directory ,file)))
         :only-compile ,only-compile
         :delete-booster ,delete-booster))

(defmacro compile-unit% (file &optional only-compile delete-booster)
  "Make an unit of compilation at compile time."
  (let* ((-source1- (funcall `(lambda () ,file)))
         (-dir1- (when -source1-
                   (funcall
                    `(lambda ()
                       (v-path* (file-name-directory ,-source1-)))))))
    `(list :source ,-source1-
           :dir ,-dir1-
           :only-compile ,only-compile
           :delete-booster ,delete-booster)))

(defmacro compile-unit->file (unit)
  "Return the :source part of `compile-unit'."
  `(plist-get ,unit :source))

(defmacro compile-unit->dir (unit)
  "Return the :dir part of `compile-unit'."
  `(plist-get ,unit :dir))

(defmacro compile-unit->only-compile (unit)
  "Return the :only-compile indicator of `compile-unit'."
  `(plist-get ,unit :only-compile))

(defmacro compile-unit->delete-booster (unit)
  "Return the :delete-booster indicator of `compile-unit'."
  `(plist-get ,unit :delete-booster))


(defun compile! (&rest units)
  "Compile and load the elisp UNITS."
  (declare (indent 0))
  (mapc (lambda (unit)
          (when unit
            (compile-and-load-file*
             (compile-unit->file unit)
             (compile-unit->only-compile unit)
             (compile-unit->delete-booster unit)
             (compile-unit->dir unit))))
        units))


 ;; end of compile macro


;;;;
;; self-def macro
;;;;


(defconst +self-def-where+
  `( ,(cons (emacs-home* "private/self-path.el")
            (emacs-home* "config/sample-self-path.el"))
     ,(cons (emacs-home* "private/self-env-spec.el")
            (emacs-home* "config/sample-self-env-spec.el"))
     ,(cons (emacs-home* "private/self-package-spec.el")
            (emacs-home* "config/sample-self-package-spec.el"))
     ,(cons (emacs-home* "private/self-prologue.el")
            (emacs-home* "config/sample-self-prologue.el"))
     ,(cons (emacs-home* "private/self-epilogue.el")
            (emacs-home* "config/sample-self-epilogue.el")))
  "Default a list of self-* files.")


(defsubst self-def-files! ()
  "Make default self-*.el files."
  (unless (file-exists-p (caar +self-def-where+))
    (path! (file-name-directory (caar +self-def-where+)))
    (mapc (lambda (f)
            (let ((dst (car f)) (src (cdr f)))
              (unless (file-exists-p dst)
                (copy-file src dst t))))
          +self-def-where+)))


(defmacro self-symbol (name)
  `(intern (format "self-%s-%s" system-type ,name)))


(defmacro def-self-path-ref (&rest path)
  "Define the PATH references for all specs in `self-path.el'.

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
  "Define default Emacs env SPEC."
  (declare (indent 0))
  `(defvar ,(self-symbol 'env-spec) (list ,@spec)
     "Define the environment specs."))


(defmacro def-self-package-spec (&rest spec)
  "Define default package SPEC."
  (declare (indent 0))
  (when-package%
    `(defvar ,(self-symbol 'package-spec) (list ,@spec))))


 ;; end of self-def macro


;;;;
;; self-spec macro
;;;;


(self-def-files!)

(compile! (compile-unit* (caar +self-def-where+)))


(defsubst self-def-path-ref-> (&optional key)
  (when (boundp (eval-when-compile (self-symbol 'path)))
    (let ((ref (symbol-value (eval-when-compile (self-symbol 'path)))))
      (if key (plist-get ref key) ref))))


(defmacro self-spec-> (seq &rest keys)
  (declare (indent 1))
  (let ((r seq))
    (mapc (lambda (k)
            (setq r (list 'plist-get r k)))
          keys)
    r))


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
  `(when-package%
     (when (self-spec->*env-spec :package :allowed)
       ,@body)))


 ;; end of self-spec macro

;;; <1>
(compile! (compile-unit% (emacs-home* "config/fns.el"))
          (compile-unit* (self-def-path-ref-> :env-spec)))

;;; <2>
(compile! (compile-unit% (emacs-home* "config/graphic.el"))
          (compile-unit% (emacs-home* "config/basic.el"))
          (compile-unit% (emacs-home* "config/shells.el")))

;;; <3>
(compile! (compile-unit* (self-def-path-ref-> :prologue)))

;;; <4>
(when-package%
  ;; (package-initialize)

  (defvar *autoload-compile-units* nil
    "Autloaded `compile-unit'.")

  ;; Load basic and self modules
  (compile! (compile-unit* (self-def-path-ref-> :package-spec))))

;;; <5>
(compile!
  ;; --batch mode: disable desktop read/save
  `,(unless noninteractive 
      (compile-unit% (emacs-home* "config/memory.el")))
  (compile-unit% (emacs-home* "config/autoloads.el")))


 ;; end of file
