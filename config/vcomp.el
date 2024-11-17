;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; vcomp.el
;;;;
;; Commentary: versioned compilation.
;;;;



;;;
;; compiled init fn
;;;

(defun path! (file)
  (mkdir! file))

(defun v-comp-file! (src)
  (make-v-comp-file! src))

;; end of compiled init fn


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
  `(if-var% ,var ,feature nil ,@body))

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
  (declare (indent 1))
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
  (declare (indent 1))
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
  (declare (indent 1) (pure t))
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
  (declare (indent 2) (pure t))
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
  (declare (indent 2) (pure t))
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
  `(if-window% ,window nil ,@body))

;; end of *-window% macro

;;;
;; *-noninteractive%: checking non-interactive context
;;;

(defmacro if-noninteractive% (then &rest body)
  "If \\=`noninteractive\\=' do THEN, else do BODY."
  (declare (indent 1) (pure t))
  `(if% noninteractive
       ,then
     (progn% ,@body)))

(defmacro unless-noninteractive% (&rest body)
  "Unless \\=`noninteractive\\=' do BODY."
  (declare (indent 0))
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


(provide 'vcomp)

;; end of vcomp.el
