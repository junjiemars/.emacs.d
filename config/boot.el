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
;; *-fn%: checking fn existing
;;;

(defmacro if-fn% (fn feature then &rest else)
  "If FN is bounded yield non-nil, do THEN, else do ELSE...\n
Argument FEATURE that FN dependent on, be loaded at compile time."
  (declare (indent 3) (pure t))
  `(if% (cond ((null ,feature) (fboundp ,fn))
              (t (and (require ,feature nil t)
                      (fboundp ,fn))))
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

(unless-fn% 'declare-function nil
  (defmacro declare-function (&rest _)))

;; end of *-fn% macro


;;;
;; *-var%: checking var existing
;;;

(defmacro if-var% (var feature then &rest else)
  "If VAR is bounded yield non-nil, do THEN, else do ELSE...\n
Argument FEATURE that VAR dependent on, load at compile time."
  (declare (indent 3) (pure t))
  `(if% (or (and ,feature (require ,feature nil t) (boundp ',var))
            (boundp ',var))
       ,then
     (progn% ,@else)))

(defmacro when-var% (var feature &rest body)
  "When VAR is bounded yield non-nil, do BODY.\n
Argument FEATURE that VAR dependent on, load at compile time."
  (declare (indent 2))
  `(if-var% ,var ,feature (progn% ,@body)))

(defmacro unless-var% (var feature &rest body)
  "Unless VAR is bounded yield non-nil, do BODY.\n
Argument FEATURE that VAR dependent on, load at compile time."
  (declare (indent 2))
  `(if-var% ,var ,feature nil (progn% ,@body)))

(defmacro setq% (x val &optional feature)
  "Set X to the value of VAL when X is bound.\n
Argument FEATURE that X dependent on, load at compile time."
  ;; (declare (debug t))
  `(when-var% ,x ,feature
     (setq ,x ,val)))

;; end of *-var% macro


;;;
;; *lexical*: checking lexical context
;;;

(defmacro if-lexical% (then &rest else)
  "If lexical binding is built-in do THEN, otherwise do ELSE..."
  (declare (indent 1) (pure t))
  `(if-version%
       <= 24.1
       ,then
     (progn% ,@else)))

(defmacro when-lexical% (&rest body)
  "When lexical binding is built-in do BODY."
  (declare (indent 0))
  `(if-lexical% (progn% ,@body)))

(defmacro unless-lexical% (&rest body)
  "Unless lexical binding is built-in do BODY."
  (declare (indent 0))
  `(if-lexical% nil ,@body))

(defmacro lexical-let% (varlist &rest body)
  "Lexically bind VARLIST in parallel then eval BODY."
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
  "Lexically bind VARLIST sequentially then eval BODY."
  (declare (indent 1) (debug let))
  `(if-lexical%
       (if-version% < 27
                    (let* ,varlist ,@body)
         (let ((lexical-binding t))
           (let* ,varlist ,@body)))
     (when-fn% 'lexical-let* 'cl
       ;; `lexical-let' since Emacs22
       (lexical-let* ,varlist ,@body))))

(defmacro ignore* (&rest vars)
  "Return nil, list VARS at compile time if in lexical context."
  (declare (indent 0))
  (when-lexical%
    (list 'prog1 nil (cons 'list `,@vars))))

(defun true (&rest x)
  "Return true value ignore X."
  (prog1 t (ignore* x)))

(defmacro safe-local-variable* (var &optional fn)
  "Safe local VAR with FN."
  `(put ,var 'safe-local-variable (or ,fn #'true)))

;; end of *lexical* compile-time macro

;;;
;; *-graphic%: checking graphical context
;;;

(defmacro if-graphic% (then &rest else)
  "If \\=`display-graphic-p\\=' yield non-nil, do THEN, else do ELSE..."
  (declare (indent 1))
  (if (display-graphic-p)
      `,then
    `(progn% ,@else)))

(defmacro when-graphic% (&rest body)
  "When \\=`display-graphic-p\\=' yield non-nil, do BODY."
  (declare (indent 0))
  `(if-graphic% (progn% ,@body)))

(defmacro unless-graphic% (&rest body)
  "Unless \\=`display-graphic-p\\=' yield nil, do BODY."
  (declare (indent 0))
  `(if-graphic% nil ,@body))

;; end of *-graphic% macro

;;;
;; *-platform%: checking platform identifier
;;;

(defmacro if-platform% (os then &rest else)
  "If OS eq \\=`system-type\\=' yield non-nil, do THEN, else do ELSE..."
  (declare (indent 2))
  `(if% (eq system-type ,os)
       ,then
     (progn% ,@else)))

(defmacro when-platform% (os &rest body)
  "When OS eq \\=`system-type\\=' yield non-nil, do BODY."
  (declare (indent 1))
  `(if-platform% ,os (progn% ,@body)))

(defmacro unless-platform% (os &rest body)
  "Unless OS eq \\=`system-type\\=' yield non-nil do BODY."
  (declare (indent 1))
  `(if-platform% ,os nil ,@body))

;; end of *-platform% macro

;;;
;; *-window%: checking windowing identifier
;;;

(defmacro if-window% (window then &rest else)
  "If WINDOW eq \\=`initial-window-system\\=' yield non-nil, do THEN,
else do ELSE..."
  (declare (indent 2))
  `(if% (eq initial-window-system ,window)
       ,then
     (progn% ,@else)))

(defmacro when-window% (window &rest body)
  "When WINDOW eq \\=`initial-window-system\\=' yield non-nil, do BODY."
  (declare (indent 1))
  `(if-window% ,window (progn% ,@body)))

(defmacro unless-window% (window &rest body)
  "Unless WINDOW eq \\=`initial-window-system\\=' yield non-nil, do BODY."
  (declare (indent 1))
  `(if-window% ,window nil (progn% ,@body)))

;; end of *-window% macro

;;;
;; *-noninteractive%: checking non-interactive context
;;;

(defmacro if-noninteractive% (then &rest body)
  "If \\=`noninteractive\\=' do THEN, else do BODY."
  (declare (indent 1))
  `(if% noninteractive
       ,then
     (progn% ,@body)))

(defmacro unless-noninteractive% (&rest body)
  "Unless \\=`noninteractive\\=' do BODY."
  `(if-noninteractive% nil ,@body))

;; end of *-noninteractive% macro

;;;
;; preferred lexical `dolist*'
;;;

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

;; end of preferred `dolist*'

;;;
;; compile-*: compiling instrument
;;;

(defun compile-unit* (file &optional only-compile)
  "Make an compile unit for \\=`compile!\\='."
  (inhibit-file-name-handler
    (when (and (stringp file) (file-exists-p file))
      (let ((u1 (v-comp-file! file)))
        `[,(car u1) ,(cdr u1) ,only-compile nil]))))

(defmacro compile-unit% (file &optional only-compile)
  "Make an compile unit at compile time for \\=`compile!\\='"
  (let* ((-u1- (inhibit-file-name-handler
                 (v-comp-file! (funcall `(lambda () ,file)))))
         (-src1- (car -u1-))
         (-dst1- (cdr -u1-)))
    (when -u1-
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

;; end of compile-* macro

;;;
;; self-spec*: self specifications
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
  (declare (indent 0))
  `(self-spec->%
    (list :file ,(v-home% ".exec/shell-env.el")
          :SHELL "SHELL"
          :PATH "PATH")
    ,@keys))

(defmacro tags-spec->% (&rest key)
  "Extract value from the list of spec via KEYS at compile time."
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
