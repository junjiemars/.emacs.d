;;; boot.el --- boot -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;;; Commentary:
;;; boot compiled elisp files and modules in order.
;;;;

(defmacro nore-emacs ()
  "Nore Emacs git repo."
  "https://github.com/junjiemars/.emacs.d")


(defmacro emacs-arch ()
  "Return emacs architecture, 64bits or 32bits."
  (if (= most-positive-fixnum (1- (expt 2 61))) 64
    (if (= most-positive-fixnum (1- (expt 2 29))) 32 0)))


;;;
;; fn compile-time checking macro
;;;

(defmacro if-fn% (fn feature then &rest else)
  "If FN is bounded yield non-nil, do THEN, else do ELSE...\n
Argument FEATURE that FN dependent on, be loaded at compile time."
  (declare (indent 3))
  `(if% (or (and ,feature (require ,feature nil t) (fboundp ,fn))
            (fboundp ,fn))
       ,then
     (progn% ,@else)))


(defmacro when-fn% (fn feature &rest body)
  "When FN is bounded yield non-nil, do BODY.\n
Argument FEATURE that FN dependent on, be loaded at compile time."
  (declare (indent 2))
  `(if-fn% ,fn ,feature (progn% ,@body)))


(defmacro unless-fn% (fn feature &rest body)
  "Unless FN is bounded yield non-nil, do BODY.\n
Argument FEATURE that FN dependent on, be loaded at compile time."
  (declare (indent 2))
  `(if-fn% ,fn ,feature nil ,@body))

;; end of fn compile-time checking macro


;;;
;; *-lexical macro
;;;

(defmacro if-lexical% (then &rest else)
  "If Emacs supports lexical binding do THEN, otherwise do ELSE..."
  (declare (indent 1))
  `(if-version%
       <= 24.1
       ,then
     (progn% ,@else)))

(defmacro when-lexical% (&rest body)
  "When Emacs supports lexical binding do BODY."
  (declare (indent 0))
  `(if-lexical% (progn% ,@body)))

(defmacro unless-lexical% (&rest body)
  "Unless Emacs supports lexical binding do BODY."
  (declare (indent 0))
  `(if-lexical% nil ,@body))

;;; Let `lexical-binding' var safe under Emacs24.1-
(unless-lexical%
  (put 'lexical-binding 'safe-local-variable (lambda (_) t)))

(defmacro lexical-let% (varlist &rest body)
  "Lexically bind variables according to VARLIST in parallel then eval BODY."
  (declare (indent 1) (debug let))
  `(if-lexical%
       (if-version% < 27
                    (let ,varlist ,@body)
         (let ((lexical-binding t))
           (let ,varlist ,@body)))
     (when-fn% 'lexical-let 'cl
       ;; `lexical-let' since Emacs22
       (lexical-let ,varlist ,@body))))

(defmacro lexical-let*% (varlist &rest body)
  "Lexically bind variables according to VARLIST sequentially then eval BODY."
  (declare (indent 1) (debug let))
  `(if-lexical%
       (if-version% < 27
                    (let* ,varlist ,@body)
         (let ((lexical-binding t))
           (let* ,varlist ,@body)))
     (when-fn% 'lexical-let* 'cl
       ;; `lexical-let' since Emacs22
       (lexical-let* ,varlist ,@body))))


;; end of *-lexical% macro

;;;
;; *-graphic% macro
;;;

(defmacro if-graphic% (then &rest else)
  "If in graphic mode, do THEN, else do ELSE...

Return the value of THEN or the value of the last of the ELSE’s."
  (declare (indent 1))
  (if (display-graphic-p)
      `,then
    `(progn% ,@else)))


(defmacro when-graphic% (&rest body)
  "When in graphic mode do BODY."
  (declare (indent 0))
  `(if-graphic% (progn% ,@body)))


(defmacro unless-graphic% (&rest body)
  "Unless in graphic mode do BODY."
  (declare (indent 0))
  `(if-graphic% nil ,@body))

;; end of *-graphic% macro


;;;
;; *-platform% macro
;;;

(defmacro if-platform% (os then &rest else)
  "If OS eq `system-type' yield non-nil, do THEN, else do ELSE..."
  (declare (indent 2))
  `(if% (eq system-type ,os)
       ,then
     (progn% ,@else)))

(defmacro when-platform% (os &rest body)
  "When OS eq `system-type' yield non-nil, do BODY."
  (declare (indent 1))
  `(if-platform% ,os (progn% ,@body)))

(defmacro unless-platform% (os &rest body)
  "Unless OS eq `system-type' yield non-nil do BODY."
  (declare (indent 1))
  `(if-platform% ,os nil ,@body))

;; end of if-platform% macro



;;;
;; var compile-time checking macro
;;;

(defmacro setq% (x val &optional feature)
  "Set X to the value of VAL when X is bound.

Argument FEATURE that X dependent on, load at compile time."
  ;; (declare (debug t))
  `(when% (or (and ,feature (require ,feature nil t) (boundp ',x))
              (boundp ',x))
     (setq ,x ,val)))

(defmacro if-var% (var feature then &rest else)
  "If VAR is bounded yield non-nil, do THEN, else do ELSE...

Argument FEATURE that VAR dependent on, load at compile time."
  (declare (indent 3))
  `(if% (or (and ,feature (require ,feature nil t) (boundp ',var))
            (boundp ',var))
       ,then
     (progn% ,@else)))

(defmacro when-var% (var feature &rest body)
  "When VAR is bounded yield non-nil, do BODY.

Argument FEATURE that VAR dependent on, load at compile time."
  (declare (indent 2))
  `(if-var% ,var ,feature (progn% ,@body)))

(defmacro unless-var% (var feature &rest body)
  "Unless VAR is bounded yield non-nil, do BODY.

Argument FEATURE that VAR dependent on, load at compile time."
  (declare (indent 2))
  `(if-var% ,var ,feature nil (progn% ,@body)))


;; end of var compile-time checking macro


;;; Preferred `dolist*'

(defmacro dolist* (spec &rest body)
  "Loop over a list and do DOBY.\n
Lexically \\=`do-list\\='.
Argument SPEC (VAR LIST [RESULT])."
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


;; end of Preferred `dolist*'


;;;
;; compile macro
;;;

(defun compile-unit* (file &optional only-compile)
  "Make an compile unit.\n
Argument FILE elisp source file.
Optional argument ONLY-COMPILE, see \\=`compile-and-load-file*\\='."
  (let* ((s1 (v-copy-file! file))
         (d1 (when s1
               (file-name-new-extension
                s1
                (if-native-comp% ".eln" ".elc")))))
    (when (and s1 d1)
      (vector s1 d1 only-compile nil))))

(defmacro compile-unit% (file &optional only-compile)
  "Make an compile unit at compile time.\n
Argument FILE elisp source file.
Optional argument ONLY-COMPILE: see `compile-and-load-file*'."
  (let* ((-src1- (v-copy-file! (funcall `(lambda () ,file))))
         (-dst1- (when -src1-
                   (file-name-new-extension
                    -src1-
                    (if-native-comp% ".eln" ".elc")))))
    (when (and -src1- -dst1-)
      `[,-src1- ,-dst1- ,only-compile nil])))

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
		(when u
			(compile-and-load-file*
			 (compile-unit->src u)
       (compile-unit->dst u)
			 (compile-unit->only-compile u)))))

;; end of compile macro

;;;
;; self-spec macro
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


(defmacro self-spec->% (seq &rest keys)
  "Read spec from SEQ via KEYS at compile time."
  (declare (indent 1))
  `(eval-when-compile (self-spec-> ,seq ,@keys)))


(defalias '*self-paths*
  (lexical-let%
      ((ps (list
            :prologue (emacs-home* "private/self-prologue.el")
            :env-spec (emacs-home* "private/self-env-spec.el")
            :package-spec (emacs-home* "private/self-package-spec.el")
            :epilogue (emacs-home* "private/self-epilogue.el")))
       (ss (list
            ;; exclude :prologue
            (cons :env-spec (emacs-home* "config/sample-self-env-spec.el"))
            (cons :package-spec
                  (emacs-home* "config/sample-self-package-spec.el"))
            (cons :epilogue
                  (emacs-home* "config/sample-self-epilogue.el")))))
    (lambda (&optional op k v)
      (cond ((eq :get op) (plist-get ps k))
            ((eq :put op) (setq ps (plist-put ps k v)))
            ((eq :dup op)
             (v-home! "private/")
             (dolist* (fs ss)
							 (let ((dst (plist-get ps (car fs)))
										 (src (cdr fs)))
								 (unless (file-exists-p dst)
									 (copy-file src dst t)))))
            (t ps))))
  "Define the PATH references.\n
No matter the declaration order, the executing order is:
\\=`:env-spec -> :package-spec -> :epilogue\\='")


(defalias '*self-env-spec*
  (lexical-let% ((env (list :theme nil
														:frame nil
                            :glyph nil
                            :key nil
                            :shell nil
                            :desktop nil
                            :eshell nil
                            :socks nil
                            :package nil
                            :edit nil)))
    (lambda (&optional op &rest keys)
      (cond ((eq :get op) (let ((rs env) (ks keys))
                            (while ks
                              (setq rs (plist-get rs (car ks))
                                    ks (cdr ks)))
                            rs))
            ((eq :put op) (setq env (plist-put env (car keys) (cadr keys))))
            (t env)))))


(defalias '*self-packages*
  (lexical-let% ((ps nil))
    (lambda (&optional op k v)
      (cond ((eq :get op) (list (assq k ps)))
            ((eq :put op) (setq ps (cons (cons k v) ps)))
            (t ps)))))


;;; Duplicate spec files
(*self-paths* :dup)


(defmacro package-spec-:allowed-p (&rest body)
  "If installing package be allowed then do BODY."
  (declare (indent 0))
  `(when-package%
     (when (*self-env-spec* :get :package :allowed)
       ,@body)))

;; end of self-spec macro


;; disable package initialize
(when-package% (setq package-enable-at-startup nil))


;;; <1> prologue
(compile! (compile-unit% (emacs-home* "config/fn.el"))
          (compile-unit* (*self-paths* :get :prologue)))

;;; <2> env
(compile! (compile-unit* (*self-paths* :get :env-spec))
          (compile-unit% (emacs-home* "config/graphic.el"))
          (compile-unit% (emacs-home* "config/basic.el"))
          (compile-unit% (emacs-home* "config/shells.el")))

;;; <3> epilogue
(compile!
  ;; --batch mode: disable desktop read/save
  `,(unless noninteractive
      (setq% desktop-save-mode nil 'desktop)
      (compile-unit% (emacs-home* "config/memory.el")))
  (compile-unit% (emacs-home* "config/autoloads.el")))




(provide 'boot)

;; end of boot.el
