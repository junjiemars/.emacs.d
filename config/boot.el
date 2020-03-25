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


;; *-lexical macro

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

(defmacro lexical-let% (varlist &rest body)
  "Like `let', but lexically scoped."
  (declare (indent 1) (debug let))
  `(when-fn% 'lexical-let 'cl
     (lexical-let ,varlist ,@body)))

(defmacro lexical-let*% (varlist &rest body)
  "Like `let*', but lexically scoped."
  (declare (indent 1) (debug let))
  `(when-fn% 'lexical-let* 'cl
     (lexical-let* ,varlist ,@body)))


;; Let `lexical-binding' var safe under Emacs24.1-
(unless-lexical%
  (put 'lexical-binding 'safe-local-variable (lambda (_) t)))


 ;; end of *-lexical% macro


;; *-graphic% macro

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


;; *-platform% macro

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


(defmacro setq% (x val &optional feature)
  "Set X to the value of its VAL when variable X is bound.
If X requires the FEATURE, load it on compile-time."
  ;; (declare (debug t))
  `(when% (or (and ,feature (require ,feature nil t) (boundp ',x))
              (boundp ',x))
     (setq ,x ,val)))


;; fn compile-time checking macro

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


;; var compile-time checking macro

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


(defmacro with-var (var &rest body)
  "Execute BODY and restore VAR before return.

Returns the value of BODY if no error happens."
  (declare (indent 1))
  (let ((old (gensym*)))
    `(let ((,old ,var))
       (prog1
           (unwind-protect
               (progn% ,@body)
             (setq ,var ,old))
         (setq ,var ,old)))))


 ;; end of var compile-time checking macro


;; byte-compiler macro


(defmacro ignore* (&rest vars)
  "Return nil, list VARS at compile time if in lexical context."
  (declare (indent 0))
  (when-lexical%
    `(when% lexical-binding
       (progn% ,@vars nil))))


(defmacro defmacro-if-feature% (feature &optional docstring)
  "Define if-FEATURE% compile-time macro."
  (let ((name (intern (format "if-feature-%s%%" feature)))
        (ds1 (format "If has `%s' feauture do THEN, otherwise do BODY."
                     feature)))
    `(defmacro ,name (then &rest body)
       ,(or docstring ds1)
       (declare (indent 1))
       (if% (require ',feature nil t)
           `(progn% (comment ,@body)
                    ,then)
         `(progn% (comment ,then)
                  ,@body)))))


(defmacro make-thread* (sexp &optional join name)
  "Make SEXP as threading call."
  `(if-fn% 'make-thread nil
           (let ((thread (make-thread (lambda () ,sexp) ,name)))
             (if% ,join
                 (thread-join thread)
               thread))
     (ignore* ,join ,name)
     (funcall (lambda () ,sexp))))


(defmacro dolist* (spec &rest body)
  "Loop over a list.
Evaluate BODY with VAR bound to each car from LIST, in turn.
Then evaluate RESULT to get return value, default nil.

 (dolist* (VAR LIST [RESULT]) BODY...)"
  (declare (indent 1) (debug ((symbolp form &optional form) body)))
  (unless (consp spec)
    (signal 'wrong-type-argument (list 'consp spec)))
  (unless (and (<= 2 (length spec)) (<= (length spec) 3))
    (signal 'wrong-number-of-arguments (list '(2 . 3) (length spec))))
  (let ((lst (gensym*)))
    `(lexical-let% ((,lst ,(nth 1 spec)))
       (while ,lst
         (let ((,(car spec) (car ,lst)))
           ,@body
           (setq ,lst (cdr ,lst))))
       ,@(cdr (cdr spec)))))


 ;; end of byte-compiler macro


;; Preferred coding system
(prefer-coding-system 'utf-8)

;; ;; Preferred coding system for terminal mode
;; (unless-graphic%
;;   (when-platform% 'windows-nt
;;     ;; also need to adjust the font and code page.
;;     (set-terminal-coding-system 'utf-8)))
;; (set-default-coding-systems 'utf-8)





;; compile macro

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
  (mapc (lambda (unit) (when unit
                         (compile-and-load-file*
                          (compile-unit->file unit)
                          (compile-unit->only-compile unit)
                          (compile-unit->delete-booster unit)
                          (compile-unit->dir unit))))
        units))


 ;; end of compile macro


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
      (mapc (lambda (f)
              (let ((dst (car f)) (src (cdr f)))
                (unless (file-exists-p dst)
                  (copy-file src dst t))))
            fs))
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

If there are no self-path.el under (emacs-home* \"private/\")
directory, More Reasonable Emacs should create a
default (emacs-home* \"private/self-env-spec.el\").

Involves: :theme, :font, :cjk-font, :shell, :eshell, :socks,
:package, :edit

Take effect after restart Emacs.
"
  (declare (indent 0))
  `(defvar ,(self-symbol 'env-spec) (list ,@spec)
     "Define the environment specs."))


(defmacro def-self-package-spec (&rest spec)
  "Define self package SPEC list."
  (declare (indent 0))
  (when-package%
    `(defvar ,(self-symbol 'package-spec) (list ,@spec))))


(defvar self-def-where (self-def-files!)
  "Where's the path of self-path.el")

 ;; end of self-def macro


;; self-spec macro

(compile! (compile-unit* self-def-where))


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


(compile! (compile-unit* (self-def-path-ref-> :env-spec)))

 ;; end of self-spec macro


;; Load cl-lib/cl at compile-time
(eval-when-compile
  (if-version% <= 24
               (require 'cl-lib)
    (require 'cl)))

;; Load cl at runtime
(if-version% <= 24
             (require 'cl-lib)
  (with-var byte-compile-warnings
    (setq byte-compile-warnings nil)
    (require 'cl)))


;; Load ui, shell, basic env:

(compile! (compile-unit% (emacs-home* "config/graphic.el"))
          (compile-unit% (emacs-home* "config/basic.el"))
          (compile-unit% (emacs-home* "config/sockets.el"))
          (compile-unit% (emacs-home* "config/shells.el")))


;; Self do prologue ...
(compile! (compile-unit* (self-def-path-ref-> :prologue)))


(when-package%
  ;; (package-initialize)

  ;; Load basic and self modules
  (compile! (compile-unit* (self-def-path-ref-> :package-spec)))
  (compile! (compile-unit% (emacs-home* "config/module.el"))))


;; Load package independent modules
(compile! (compile-unit% (emacs-home* "config/on-module.el"))
          ;; --batch mode: disable desktop read/save
          `,(unless noninteractive 
              (compile-unit% (emacs-home* "config/memory.el")))
          (compile-unit% (emacs-home* "config/autoloads.el")))


 ;; end of file
