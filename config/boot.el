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

(defmacro lexical-let% (varlist &rest body)
  "Like `let', but lexically scoped."
  (declare (indent 1) (debug let))
  `(if-lexical%
       (if-version% < 27
                    (let ,varlist ,@body)
         (let ((lexical-binding t))
           (let ,varlist ,@body)))
     (when-fn% 'lexical-let 'cl
       (lexical-let ,varlist ,@body))))

(defmacro lexical-let*% (varlist &rest body)
  "Like `let*', but lexically scoped."
  (declare (indent 1) (debug let))
  `(if-lexical%
       `(let ((lexical-binding t))
          (let ,varlist ,@body))
     (when-fn% 'lexical-let* 'cl
       (lexical-let* ,varlist ,@body))))


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
  (let ((us units))
    (while (not (null us))
      (let ((u (car us)))
        (when u
          (compile-and-load-file*
           (compile-unit->file u)
           (compile-unit->only-compile u)
           (compile-unit->delete-booster u)
           (compile-unit->dir u))))
      (setq us (cdr us)))))


 ;; end of compile macro


;;;;
;; self-def macro
;;;;


(defmacro self-symbol (name)
  `(intern (format "self-%s-%s" system-type ,name)))


(defalias '*self-paths*
  (lexical-let%
      ((ps (list
            :env-spec (emacs-home* "private/self-env-spec.el")
            :package-spec (emacs-home* "private/self-package-spec.el")
            :epilogue (emacs-home* "private/self-epilogue.el")))
       (ss (list
            (cons :env-spec (emacs-home* "config/sample-self-env-spec.el"))
            (cons :package-spec
                  (emacs-home* "config/sample-self-package-spec.el"))
            (cons :epilogue
                  (emacs-home* "config/sample-self-epilogue.el")))))
    (lambda (&optional op k v)
      (cond ((eq :get op) (plist-get ps k))
            ((eq :put op) (setq ps (plist-put ps k v)))
            ((eq :dup op) (mapc
                           (lambda (fs)
                             (let ((dst (plist-get ps (car fs)))
                                   (src (cdr fs)))
                               (unless (file-exists-p dst)
                                 (path! (file-name-directory dst))
                                 (copy-file src dst t))))
                           ss))
            (t ps))))
  "Define the PATH references.

No matter the declaration order, the executing order is:
:env-spec -> :package-spec -> :epilogue")


(defalias '*self-env-spec*
  (lexical-let% ((env (list :theme nil
                            :frame nil
                            :glyph nil
                            :shell nil
                            :desktop nil
                            :eshell nil
                            :socks nil
                            :package nil
                            :edit nil)))
    (lambda (&optional op &rest keys)
      (cond ((eq :get op) (let ((rs env) (ks keys))
                            (while (not (null ks))
                              (setq rs (plist-get rs (car ks))
                                    ks (cdr ks)))
                            rs))
            ((eq :put op) (setq env (plist-put env (car keys) (cadr keys))))
            (t env)))))


(defmacro def-self-package-spec (&rest spec)
  "Define default package SPEC."
  (declare (indent 0))
  (when-package%
    `(defvar ,(self-symbol 'package-spec) (list ,@spec))))

 ;; end of self-def macro


;;;;
;; self-spec macro
;;;;

(*self-paths* :dup)


(defmacro self-spec-> (seq &rest keys)
  (declare (indent 1))
  (let ((r seq) (ks keys))
    (while (not (null ks))
      (setq r (list 'plist-get r (car ks))
            ks (cdr ks)))
    r))


(defmacro self-spec<- (k v seq &rest keys)
  (declare (indent 3))
  `(plist-put (self-spec-> ,seq ,@keys) ,k ,v))


(defmacro self-spec->% (seq &rest keys)
  (declare (indent 1))
  `(eval-when-compile (self-spec-> ,seq ,@keys)))


(defmacro package-spec-:allowed-p (&rest body)
  (declare (indent 0))
  `(when-package%
     (when (*self-env-spec* :get :package :allowed)
       ,@body)))


 ;; end of self-spec macro

;;; <1>
(compile! (compile-unit% (emacs-home* "config/fns.el"))
          (when (*self-paths* :get :env-spec)
            (compile-unit* (*self-paths* :get :env-spec)))
          (compile-unit% (emacs-home* "config/graphic.el"))
          (compile-unit% (emacs-home* "config/basic.el"))
          (compile-unit% (emacs-home* "config/shells.el")))

;;; <2>
(when (*self-paths* :get :prologue)
  (compile! (compile-unit* (*self-paths* :get :prologue))))

;;; <3>
(when-package%
  ;; (package-initialize)

  (defalias '*package-compile-units*
    (lexical-let% ((us '()))
      (lambda (&optional n)
        (cond ((consp n) (let ((s n))
                           (while (not (null s))
                             (setq us (cons (car s) us)
                                   s (cdr s)))
                           us))
              (t us))))
    "Autloaded `compile-unit'.")

  (defalias '*self-packages*
    (lexical-let% ((ps))
      (lambda (&optional op k v)
        (cond ((eq :get op) (list (assq k ps)))
              ((eq :put op) (setq ps (cons (cons k v) ps)))
              (t ps)))))

  ;; Load basic and self modules
  (when (*self-paths* :get :package-spec)
    (compile! (compile-unit* (*self-paths* :get :package-spec)))))

;;; <4>
(compile!
  ;; --batch mode: disable desktop read/save
  `,(unless noninteractive 
      (compile-unit% (emacs-home* "config/memory.el")))
  (compile-unit% (emacs-home* "config/autoloads.el")))


 ;; end of file
