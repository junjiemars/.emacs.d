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



(provide 'shim)

;; end of shim.el
