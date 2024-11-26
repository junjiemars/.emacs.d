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

(defun emacs-home* (&optional file)
  (emacs-home file))

(defun path! (file)
  (mkdir* file))

(defun make-v-home* (file)
  (mkdir* (v-home file)))

(defun v-home* (&optional file)
  (v-home file))

(defun v-comp-file! (src)
  (make-v-comp-file src))



(defmacro emacs-home% (&optional file)
  "Return path of FILE under \\='~/.emacs.d\\=' at compile-time."
  (emacs-home* file))

(defmacro v-home% (&optional file)
  "Return versioned path of FILE under \\=`v-home\\=' at compile-time."
  (v-home* file))

(defmacro v-home%> (file)
  "Return the \\=`v-home\\=' FILE with the extension of compiled file."
  (concat (v-home* file) (comp-file-extension%)))

(defmacro v-home! (file)
  "Make versioned path of FILE under \\=`v-home\\=' at compile-time."
  (make-v-home* file))

;; end of compiled init fn

;;;
;; compile-*: compiling instrument
;;;

(defun compile-unit* (file &optional only-compile)
  "Make an compile unit for \\=`compile!\\='."
  (declare (pure t))
  (and (stringp file) (file-exists-p file)
       (let ((u1 (make-v-comp-file file)))
         (vector (car u1) (cdr u1) only-compile nil))))

(defmacro compile-unit% (file &optional only-compile)
  "Make an compile unit at compile time for \\=`compile!\\='"
  (declare (pure t))
  (let* ((-cu-u1- (v-comp-file! (funcall `(lambda () ,file))))
         (-cu-src1- (car -cu-u1-))
         (-cu-dst1- (cdr -cu-u1-)))
    (and -cu-u1- `[,-cu-src1- ,-cu-dst1- ,only-compile nil])))

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
  (while units
    (let ((u (car units)))
      (and u (compile-and-load-file*
              (compile-unit->src u)
              (compile-unit->dst u)
              (compile-unit->only-compile u))))
    (setq units (cdr units))))


;; end of compile-* macro

;;; `gensym*' since Emacs-26+

(defvar *gensym-counter* 0 "The counter of \\=`gensym*\\='.")

(defun gensym* (&optional prefix)
  "Generate a new uninterned symbol, PREFIX default is \"n\"."
  (make-symbol
   (concat (or prefix "n")
           (prog1 (number-to-string *gensym-counter*)
             (setq *gensym-counter* (1+ *gensym-counter*))))))

;; end of `gensym*'

(provide 'vcomp)

;; end of vcomp.el
