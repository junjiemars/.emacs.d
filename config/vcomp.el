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


(provide 'vcomp)

;; end of vcomp.el
