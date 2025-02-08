;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; shim.el
;;;;
;; Commentary: if-*.
;;;;


(defmacro emacs-home% (&optional file)
  "Return path of FILE under \\='~/.emacs.d\\=' at compile-time."
  (emacs-home* file))

(defmacro v-home% (&optional file)
  "Return versioned path of FILE under \\=`v-home\\=' at compile-time."
  (v-home* file))

(defmacro v-home%> (file)
  "Return the \\=`v-home\\=' FILE with the extension of compiled file."
  (concat (v-home* file) +comp-file-extension+))

(defmacro v-home! (file)
  "Make versioned path of FILE under \\=`v-home\\=' at compile-time."
  (make-v-home* file))

(defmacro compile-unit% (file &optional only-compile)
  "Make an compile unit at compile time for \\=`compile!\\='"
  (funcall `(lambda () (compile-unit* ,file ,only-compile))))



;;;
;; `gensym*' since Emacs-26+
;;;

(defvar *gensym-counter* 0 "The counter of \\=`gensym*\\='.")

(defun gensym* (&optional prefix)
  "Generate a new uninterned symbol, PREFIX default is \"n\"."
  (make-symbol
   (concat (or prefix "n")
           (prog1 (number-to-string *gensym-counter*)
             (setq *gensym-counter* (1+ *gensym-counter*))))))

;; end of `gensym*'

;;;
;; if-*
;;;

(defvar *nore-non-obarray* (make-vector 251 nil)
  "Interned non obarray at compile-time.")

(defun feature? (feature)
  "Return t if has the FEAUTURE, otherwise nil."
  (let ((name (symbol-name feature)))
    (cond ((intern-soft name *nore-non-obarray*) nil)
          ((featurep feature) t)
          ((require feature nil t) t)
          (t (intern name *nore-non-obarray*) nil))))

(defun fn? (fn feature)
  "Return t if the FN of FEATURE is bounded, otherwise nil."
  (let ((name (symbol-name fn)))
    (cond ((intern-soft name *nore-non-obarray*) nil)
          ((fboundp fn) t)
          (feature (cond ((and (require feature nil t) (fboundp fn)) t)
                         (t (intern name *nore-non-obarray*) nil)))
          (t (intern name *nore-non-obarray*) nil))))

(defun var? (var feature)
  "Return t if the VAR of FEATURE is bounded, otherwise nil."
  (let ((name (symbol-name var)))
    (cond ((intern-soft name *nore-non-obarray*) nil)
          ((boundp var) t)
          (feature (cond ((and (require feature nil t) (boundp var)) t)
                         (t (intern name *nore-non-obarray*) nil)))
          (t (intern name *nore-non-obarray*) nil))))

;; end of if-*

;;;
;; key
;;;

(defun key? (keymap key def)
  "Return KEY if DEF for KEY in KEYMAP, otherwise nil"
  (if (eq def (lookup-key keymap key))
      (cons key def)
    (cons key nil)))

;; end of key

;;;
;; platform
;;;

(defun emacs-arch ()
  "Return emacs architecture, 64bits or 32bits."
  (cond ((= most-positive-fixnum (1- (expt 2 61))) 64)
        ((= most-positive-fixnum (1- (expt 2 29))) 32)
        (t 16)))

;; end of platform

(provide 'shim)

;; end of shim.el
