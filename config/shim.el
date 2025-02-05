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
  (concat (v-home* file) (comp-file-extension%)))

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

(defvar *if-obarray* (make-vector 512 nil)
  "Interned obarray at compile-time.")

(defun feature? (feature)
  "Return t if has the FEAUTURE, otherwise nil."
  (let ((hasft (format "%s_ft+" feature))
        (nonft (format "%s_ft-" feature)))
    (cond ((intern-soft hasft *if-obarray*) t)
          ((intern-soft nonft *if-obarray*) nil)
          ((require feature nil t) (intern hasft *if-obarray*) t)
          (t (intern nonft *if-obarray*) nil))))

(defun fn? (fn feature)
  "Return t if the FN of FEATURE is bounded, otherwise nil."
  (let ((hasfn (format "%s_fn+" fn))
        (nonfn (format "%s_fn-" fn)))
    (cond ((intern-soft hasfn *if-obarray*) t)
          ((intern-soft nonfn *if-obarray*) nil)
          ((fboundp fn) (intern hasfn *if-obarray*) t)
          (feature (cond ((and (require feature nil t) (fboundp fn))
                          (intern hasfn *if-obarray*) t)
                         (t (intern nonfn) nil)))
          (t (intern nonfn *if-obarray*) nil))))

(defun var? (var feature)
  "Return t if the VAR of FEATURE is bounded, otherwise nil."
  (declare (indent 3))
  (let ((hasvar (format "%s_var+" var))
        (nonvar (format "%s_var-" var)))
    (cond ((intern-soft hasvar *if-obarray*) t)
          ((intern-soft nonvar *if-obarray*) nil)
          ((boundp var) (intern hasvar *if-obarray*) t)
          (feature (cond ((and (require feature nil t) (boundp var))
                          (intern hasvar *if-obarray*) t)
                         (t (intern nonvar *if-obarray*) nil)))
          (t (intern nonvar *if-obarray*) nil))))

;; (defun unintern-if* (name)
;;   "Delete NAME from \\=`*if-obarray*\\='."
;;   (unintern (format "%s_ft+" name) *if-obarray*)
;;   (unintern (format "%s_ft-" name) *if-obarray*)
;;   (unintern (format "%s_fn+" name) *if-obarray*)
;;   (unintern (format "%s_fn-" name) *if-obarray*)
;;   (unintern (format "%s_var+" name) *if-obarray*)
;;   (unintern (format "%s_var-" name) *if-obarray*))

;; end of if-*

;;;
;; key*
;;;

(defun key? (keymap key def)
  "Return KEY if DEF for KEY in KEYMAP, otherwise nil"
  (if (eq def (lookup-key keymap key))
      (cons key def)
    (cons key nil)))

;; end of key*

(provide 'shim)

;; end of shim.el
