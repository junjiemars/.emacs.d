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
  (inhibit-file-name-handler
    (emacs-home file)))

(defun make-v-home* (file)
  (inhibit-file-name-handler
    (make-v-home file)))

(defun v-home* (&optional file)
  (inhibit-file-name-handler
    (v-home file)))

;; end of compiled init fn

;;;
;; compile-*: compiling instrument
;;;

(defun compile-unit* (file &optional only-compile)
  "Make an compile unit for \\=`compile!\\='."
  (inhibit-file-name-handler
    (and (stringp file) (file-exists-p file)
         (let ((u1 (make-v-comp-file file)))
           (vector (car u1) (cdr u1) only-compile)))))

(defun compile! (&rest units)
  "Compile and load UNITS."
  (while units
    (let ((u (car units)))
      (and u (compile-and-load-file* (aref u 0) (aref u 1) (aref u 2))))
    (setq units (cdr units))))

;; end of compile-* macro


;;;
;; `gensym*' since Emacs-26+
;;;

(defalias 'gensym*
  (let ((pre "n") (cnt 0))
    (lambda (&optional prefix)
      (let ((pre1 (or prefix pre)))
        (make-symbol
         (concat pre1
                 (prog1 (number-to-string cnt)
                   (setq cnt (1+ cnt))))))))
  "Generate a new uninterned symbol, PREFIX default is \"n\".")

 ;; end of `gensym*'

;;;
;; if-*
;;;

(defvar *nore-non-obarray*
  (if-version% < 30 (obarray-make 251) (make-vector 251 nil))
  "Interned non obarray at compile-time.")

(defun vector-take-while (pred vec)
  (let ((i 0) (l (length vec)) (s nil))
    (while (< i l)
      (let ((v (aref vec i)))
        (when (funcall pred v) (setq s (cons v s))))
      (setq i (1+ i)))
    (nreverse s)))

(defun feature? (feature)
  "Return t if has the FEAUTURE, otherwise nil."
  (cond ((featurep feature) t)
        ((intern-soft (symbol-name feature) *nore-non-obarray*) nil)
        ((require feature nil t) t)
        (t (intern (symbol-name feature) *nore-non-obarray*) nil)))

(defun fn? (fn &optional feature)
  "Return t if the FN of FEATURE is bounded, otherwise nil."
  (cond ((fboundp fn) t)
        ((intern-soft (symbol-name fn) *nore-non-obarray*) nil)
        ((and feature (require feature nil t) (fboundp fn)) t)
        (t (intern (symbol-name fn) *nore-non-obarray*) nil)))

(defun var? (var &optional feature)
  "Return t if the VAR of FEATURE is bounded, otherwise nil."
  (cond ((boundp var) t)
        ((intern-soft (symbol-name var) *nore-non-obarray*) nil)
        ((and feature (require feature nil t) (boundp var)) t)
        (t (intern (symbol-name var) *nore-non-obarray*) nil)))

;; end of if-*

(provide 'vcomp)

;; end of vcomp.el
