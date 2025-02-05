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
  (inhibit-file-name-handler
    (if (file-exists-p file)
        file
      (mkdir* file))))

(defun make-v-home* (file)
  (make-v-home file))

(defun v-home* (&optional file)
  (inhibit-file-name-handler
    (v-home file)))

(defun v-comp-file! (src)
  (make-v-comp-file src))


;; end of compiled init fn

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
;; compile-*: compiling instrument
;;;

(defun compile-unit* (file &optional only-compile)
  "Make an compile unit for \\=`compile!\\='."
  (and (stringp file) (inhibit-file-name-handler (file-exists-p file))
       (let ((u1 (v-comp-file! file)))
         (vector (car u1) (cdr u1) only-compile nil))))

(defun compile! (&rest units)
  "Compile and load UNITS."
  (while units
    (let ((u (car units)))
      (and u (compile-and-load-file* (aref u 0) (aref u 1) (aref u 2))))
    (setq units (cdr units))))

;; end of compile-* macro


(provide 'vcomp)

;; end of vcomp.el
