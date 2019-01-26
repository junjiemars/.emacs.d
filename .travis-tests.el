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

(ert-deftest %init:emacs-home* ()
  (should (string-match "\.emacs\.d/$" (emacs-home*)))
  (should (string-match "\.emacs\.d/config/$" (emacs-home* "config/")))
  (should (string-match "\.emacs\.d/x/y/z/$" (emacs-home* "x/" "y/" "z/"))))

(ert-deftest %init:file-name-base* ()
  (should (string= "x" (file-name-base* "/a/b/c/x.z"))))

(ert-deftest %init:file-name-new-extension* ()
  (should (string= "/a/x.z" (file-name-new-extension* "/a/x.el" ".z"))))

(ert-deftest %init:directory-name-p ()
  (should (directory-name-p "a/"))
  (should-not (directory-name-p "a")))

(ert-deftest %init:path! ()
  (let ((p (concat temporary-file-directory "x/")))
    (should (and (path! p) (file-exists-p p)))))

(ert-deftest %init:v-path* ()
  (should (string-match "[gt]_[.0-9]+" (v-path* "a/x.el")))
  (should (string-match "[gt]_[.0-9]+.*\\.z\\'" (v-path* "a/x.el" ".z"))))

(ert-deftest %init:v-home* ()
  (should (string-match "[gt]_[.0-9]+.*x\\.el\\'" (v-home* "x.el"))))

(ert-deftest %basic:assoc** ()
  (should (equal '(a "a") (assoc** 'a '((b "b") (a "a")))))
  (should (equal '("a" a) (assoc** "a" '(("b" b) ("a" a)) #'string=))))

(ert-deftest %basic:split-string* ()
  (should (equal '("a" "b" "c") (split-string* "a,b,,cXX" "," t "XX"))))

(ert-deftest %basic:string-trim> ()
  (should (eq nil (string-trim> nil "X")))
  (should (string= "abc" (string-trim> "abc \n  ")))
  (should (string= "abc" (string-trim> "abcXX" "XX"))))

(ert-deftest %basic:string-trim< ()
  (should (eq nil (string-trim< nil "X")))
  (should (string= "abc" (string-trim< "  \n abc")))
  (should (string= "abc" (string-trim< "XXabc" "XX"))))

(ert-deftest %basic:string-trim>< ()
  (should (eq nil (string-trim>< nil "X" "Z")))
  (should (string= "abc" (string-trim>< " \n abc \n ")))
  (should (string= "abc" (string-trim>< "ZZabcXX" "XX" "ZZ"))))

;; end of file
