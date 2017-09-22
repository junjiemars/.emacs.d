;;;; -*- lexical-binding:t -*-
;;;;
;; Moduel Management 
;;;;



(require 'ert)

(ert-deftest %init:comment ()
  (should-not (comment))
  (should-not (comment (+ 1 2 3)))
  (should-not (comment (progn (+ 1) (* 2 3)))))

(ert-deftest %init:emacs-home ()
  (if (boundp 'user-emacs-directory)
      (should (string= emacs-home user-emacs-directory))
    (should (string= emacs-home "~/.emacs.d/"))))

(ert-deftest %init:emacs-home* ()
  (should (string-match "\.emacs\.d/$" (emacs-home*)))
  (should (string-match "\.emacs\.d/config/$" (emacs-home* "config/")))
  (should (string-match "\.emacs\.d/x/y/z/$" (emacs-home* "x/" "y/" "z/")))) 
