;;;; -*- lexical-binding:t -*-
;;;;
;; traivis test
;;;;


;; Running Testing in Batch Mode:
;; emacs --batch -l ert -l ~/.emacs.d/init.el -l ~/.emacs.d/.travis-tests.el -f ert-run-tests-batch-and-exit
;;
;; Running Test Interactively:
;; M-x ert RET t


(require 'ert)

(ert-deftest %init:comment ()
  (should-not (comment))
  (should-not (comment (+ 1 2 3)))
  (should-not (comment (progn (+ 1) (* 2 3)))))

(ert-deftest %init:+emacs-home+ ()
  (if (boundp 'user-emacs-directory)
      (should (string= +emacs-home+ user-emacs-directory))
    (should (string= +emacs-home+ "~/.emacs.d/"))))

(ert-deftest %init:emacs-home* ()
  (should (string-match "\.emacs\.d/$" (emacs-home*)))
  (should (string-match "\.emacs\.d/config/$" (emacs-home* "config/")))
  (should (string-match "\.emacs\.d/x/y/z/$" (emacs-home* "x/" "y/" "z/"))))

(ert-deftest %init:v-home*|% ()
	(should (and v-dir (> (length v-dir) 0)))
	(let ((a (format "%s%s/%s" (emacs-home* "private/") v-dir "x.el")))
		(should (string= (v-home* "private/" "x.el") a))
		(should (string= (v-home% "private/" "x.el") a))))




