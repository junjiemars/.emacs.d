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
    (should (and (path! p) (file-exists-p p)))
    (should (and (eq nil (delete-directory p))
                 (eq nil (file-exists-p p))))))

(ert-deftest %init:v-path* ()
  (should (string-match "[gt]_[.0-9]+" (v-path* "a/x.el")))
  (should (string-match "[gt]_[.0-9]+.*\\.z\\'" (v-path* "a/x.el" ".z"))))

(ert-deftest %init:v-home* ()
  (should (directory-name-p (v-home* nil)))
  (should (string-match "[gt]_[.0-9]+" (v-home* nil)))
  (should (string-match "[gt]_[.0-9]+.*x\\.el\\'" (v-home* "x.el"))))

(ert-deftest %init:v-home% ()
  (should (directory-name-p (v-home% nil)))
  (should (string-match "[gt]_[.0-9]+" (v-home% nil)))
  (should (string-match "[gt]_[.0-9]+.*x\\.el\\'" (v-home% "x.el"))))

(ert-deftest %init:progn% ()
  (should (eq nil (progn%)))
  (should (equal '(+ 1 2) (macroexpand '(progn% (+ 1 2)))))
  (should (equal '(progn (+ 1 2) (* 3 4)) (macroexpand '(progn% (+ 1 2) (* 3 4))))))

(ert-deftest %init:if% ()
  (should (= 3 (if% t (+ 1 2))))
  (should (= 12 (if% nil (+ 1 2) (* 3 4))))
  (should (equal '(+ 1 2) (macroexpand '(if% t (+ 1 2) (* 3 4)))))
  (should (equal '(progn (* 3 4) (* 5 6))
                 (macroexpand '(if% nil (+ 1 2) (* 3 4) (* 5 6))))))

(ert-deftest %init:when% ()
  (should (eq nil (when% t)))
  (should (eq nil (when% nil)))
  (should (= 3 (when% t (+ 1 2))))
  (should (equal '(progn (+ 1 2) (* 3 4))
                 (macroexpand '(when% t (+ 1 2) (* 3 4))))))

(ert-deftest %init:unless% ()
  (should (eq nil (unless% t)))
  (should (eq nil (unless% nil)))
  (should (= 3 (unless% nil (+ 1 2))))
  (should (equal '(progn (+ 1 2) (* 3 4))
                 (macroexpand '(unless% nil (+ 1 2) (* 3 4))))))

(ert-deftest %init:version-supported-if ()
  (should (version-supported-if < 0 t))
  (should (version-supported-if < "0" t))
  (should (version-supported-if < '0 t))
  (should (= 12 (version-supported-if < 1000 (+ 1 2) (* 3 4))))
  (should (equal '(progn (* 3 4) (* 5 6))
                 (macroexpand '(version-supported-if
                                   < 1000
                                   (+ 1 2)
                                 (* 3 4)
                                 (* 5 6))))))

(ert-deftest %init:version-supported-when ()
  (should (version-supported-when < 0 t))
  (should (eq nil (version-supported-when < 1000 (+ 1 2))))
  (should (equal '(progn (+ 1 2) (* 3 4))
                 (macroexpand '(version-supported-when
                                   < 0
                                 (+ 1 2)
                                 (* 3 4))))))

(ert-deftest %strap:lexical-supported-if/when/unless ()
  (if (lexical-supported-if t nil)
      (should (and (lexical-supported-when t)
                   (not (lexical-supported-unless t))
                   (equal '(progn (+ 1 2) (* 3 4))
                          (macroexpand '(lexical-supported-when
                                          (+ 1 2) (* 3 4))))))
    (should (and (not (lexical-supported-when t))
                 (lexical-supported-unless t)
                 (equal '(progn (+ 1 2) (* 3 4))
                        (macroexpand '(lexical-supported-unless
                                        (+ 1 2) (* 3 4))))))))

(ert-deftest %strap:graphic/terminal-supported-p ()
  (if (graphic-supported-if t nil)
      (should (and (graphic-supported-p t)
                   (not (terminal-supported-p t))
                   (equal '(progn (+ 1 2) (* 3 4))
                          (macroexpand '(graphic-supported-p
                                          (+ 1 2) (* 3 4))))))
    (should (and (not (graphic-supported-p t))
                 (terminal-supported-p t)
                 (equal '(progn (+ 1 2) (* 3 4))
                        (macroexpand '(terminal-supported-p
                                        (+ 1 2) (* 3 4))))))))

(ert-deftest %strap:version-supported-p ()
  (should (version-supported-p < 0))
  (should (not (version-supported-p < 1000))))

(ert-deftest %strap:platform-supported-if/when/unless ()
  (cond ((platform-supported-if 'darwin t)
         (should (and (platform-supported-when 'darwin t)
                      (not (platform-supported-unless 'darwin t)))))
        ((platform-supported-if 'gnu/linux t)
         (should (and (platform-supported-when 'gnu/linux t)
                      (not (platform-supported-unless 'gnu/linux t)))))
        ((platform-supported-if 'windows-nt t)
         (should (and (platform-supported-when 'windows-nt t)
                      (not (platform-supported-unless 'windows-nt t)))))))

(ert-deftest %strap:setq% ()
  "uncompleted..."
  (when% t
    (should (eq nil (setq% zzz 'xx)))
    (should (eq nil (setq% zzz nil)))))

(ert-deftest %strap:if/when/unless-fn% ()
  (should (and (if-fn% 'should 'ert t)
               (when-fn% 'should 'ert t)
               (not (unless-fn% 'should 'ert t))))
  (should (equal '(progn (+ 1 2) (* 3 4))
                 (macroexpand '(when-fn% 'should 'ert (+ 1 2) (* 3 4)))))
  (should (equal '(progn (+ 1 2) (* 3 4))
                 (macroexpand '(unless-fn% 'shouldxxx 'ert (+ 1 2) (* 3 4))))))

(ert-deftest %strap:if/when-var% ()
  (should (and (if-var% ert-batch-backtrace-right-margin 'ert t)
               (when-var% ert-batch-backtrace-right-margin 'ert t)
               (not (when-var% ert-batch-backtrace-right-marginxxx 'ert t)))))

(ert-deftest %strap:gensym ()
  (should (string-match "^g[0-9]+" (format "%s" (gensym))))
  (should (string-match "^X[0-9]+" (format "%s" (gensym "X")))))

(ert-deftest %strap:ignore* ()
  (lexical-supported-if
      (should (eq nil (macroexpand '(ignore* a b))))
    (should (eq nil (macroexpand '(ignore* a b))))))

(ert-deftest %basic:assoc** ()
  (should (equal '(a "a") (assoc** 'a '((b "b") (a "a")))))
  (should (equal '("a" a) (assoc** "a" '(("b" b) ("a" a)) #'string=))))

(ert-deftest %basic:alist-get* ()
  (should (eq nil (alist-get* nil nil)))
  (should (eq nil (alist-get* 'a nil)))
  (should (equal '(aa) (alist-get* 'a '((a aa)))))
  (should (equal '(aa) (alist-get* "a" '(("a" aa)) nil nil #'string=))))

(ert-deftest %basic:remove** ()
  (should (eq nil (remove** nil nil)))
  (should (eq nil (remove** 'a nil)))
  (should (equal '(a) (remove** 'b '(a b))))
  (should (equal '("a") (remove** "b" '("a" "b") :test #'string=))))

(ert-deftest %basic:member** ()
  (should (eq nil (member** nil nil)))
  (should (eq nil (member** 'a nil)))
  (should (equal '(a) (member** 'a '(b a))))
  (should (equal '("a") (member** "a" '("b" "a") :test #'string=))))

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

(ert-deftest %basic:match-string* ()
  (should (eq nil (match-string* nil nil 0)))
  (should (string= "abc" (match-string* "xx\\(abc\\)xx" "xxxabcxx" 1))))

(ert-deftest %basic:save-sexp-to-file ()
  (when% t
    (let ((f (emacs-home* "private/xxx.el")))
      (should (and (save-sexp-to-file '(defvar xxx t) f)
                   (file-exists-p f)
                   (and (load f t) xxx)))
      (delete-file f)
      (should (not (file-exists-p f))))))

(ert-deftest %basic:save/read-str-to/from-file ()
  (let ((f (emacs-home* "private/xxx.el")))
    (should (and (save-str-to-file "abc" f)
                 (string= "abc" (read-str-from-file f))))
    (delete-file f)
    (should (not (file-exists-p f)))))


;; end of file
