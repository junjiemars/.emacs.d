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
;; if-*
;;;

(defvar *if-obarray* (make-vector 512 nil)
  "Interned obarray at compile-time.")

(defun if-feature (feature then &optional else)
  "If has the FEAUTURE do THEN, otherwise do ELSE."
  (declare (indent 2))
  (let ((hasft (format "%s_ft+" feature))
        (nonft (format "%s_ft-" feature)))
    (cond ((intern-soft hasft *if-obarray*) then)
          ((intern-soft nonft *if-obarray*) else)
          ((require feature nil t) (intern hasft *if-obarray*) then)
          (t (intern nonft *if-obarray*) else))))

(defun if-fn (fn feature then &optional else)
  "If the FN of FEATURE is bounded yield non-nil, do THEN, else do ELSE."
  (declare (indent 3))
  (let ((hasfn (format "%s_fn+" fn))
        (nonfn (format "%s_fn-" fn)))
    (cond ((intern-soft hasfn *if-obarray*) then)
          ((intern-soft nonfn *if-obarray*) else)
          ((fboundp fn) (intern hasfn *if-obarray*) then)
          (feature (cond ((and (require feature nil t) (fboundp fn))
                          (intern hasfn *if-obarray*) then)
                         (t (intern nonfn) else)))
          (t (intern nonfn *if-obarray*) else))))

(defun if-var (var feature then &optional else)
  "If the VAR of FEATURE is bounded yield non-nil, do THEN, else do ELSE."
  (declare (indent 3))
  (let ((hasvar (format "%s_var+" var))
        (nonvar (format "%s_var-" var)))
    (cond ((intern-soft hasvar *if-obarray*) then)
          ((intern-soft nonvar *if-obarray*) else)
          ((boundp var) (intern hasvar *if-obarray*) then)
          (feature (cond ((and (require feature nil t) (boundp var))
                          (intern hasvar *if-obarray*) then)
                         (t (intern nonvar *if-obarray*) else)))
          (t (intern nonvar *if-obarray*) else))))

;; (defun unintern-if* (name)
;;   "Delete NAME from \\=`*if-obarray*\\='."
;;   (unintern (format "%s_ft+" name) *if-obarray*)
;;   (unintern (format "%s_ft-" name) *if-obarray*)
;;   (unintern (format "%s_fn+" name) *if-obarray*)
;;   (unintern (format "%s_fn-" name) *if-obarray*)
;;   (unintern (format "%s_var+" name) *if-obarray*)
;;   (unintern (format "%s_var-" name) *if-obarray*))

;; end of if-*

(provide 'shim)

;; end of shim.el
