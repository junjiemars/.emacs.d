;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; .test.el
;;;;


;;
;; Running Test Interactively:
;; M-x ert RET t


(require 'ert)

;;;
;; env
;;;

(ert-deftest %a:env ()
  (should (message "# nore-emacs = %s" (nore-emacs)))
  (should (message "# emacs-home* = %s" (emacs-home*)))
  (should (message "# system-type = %s" system-type))
  (should (message "# platform-arch = %s" (platform-arch)))
  (should (message "# emacs-arch = %s" (emacs-arch)))
  (should (message "# sh = %s" (or (executable-find "sh") "")))
  (should (message "# system-configuration = %s" system-configuration))
  (should (message "# system-configuration-options = %s"
                   system-configuration-options))
  (should (message "# system-configuration-features = %s"
                   (when (boundp 'system-configuration-features)
                     system-configuration-features))))


;;;
;; init
;;;


(ert-deftest %b:init:comment ()
  (should-not (comment))
  (should-not (comment (+ 1 2 3)))
  (should-not (comment (progn (+ 1) (* 2 3)))))

(ert-deftest %b:init:gensym* ()
  (should (string-match "^n[0-9]+" (format "%s" (gensym*))))
  (should (string-match "^X[0-9]+" (format "%s" (gensym* "X")))))

(ert-deftest %b:init:emacs-home* ()
  (should (file-exists-p (emacs-home*)))
  (should (file-exists-p (emacs-home* "config/")))
  (should (file-exists-p (emacs-home* "private/"))))

(ert-deftest %b:init:path! ()
  (let ((p (concat temporary-file-directory
                   (make-temp-name (symbol-name (gensym*)))
                   "/")))
    (should (null (file-exists-p p)))
    (should (path! p))
    (should (file-exists-p p))))

(ert-deftest %b:init:v-path ()
  (should (string-match "[gt]_[.0-9]+" (v-path "a/x.el")))
  (should (string-match "[gt]_[.0-9]+/" (v-path "a/b/c/"))))

(ert-deftest %b:init:v-home ()
  (should (directory-name-p (v-home)))
  (should (string-match "[gt]_[.0-9]+" (v-home)))
  (should (string-match "[gt]_[.0-9]+.*x\\.el\\'" (v-home "x.el"))))

(ert-deftest %b:init:v-home% ()
  (should (directory-name-p (v-home%)))
  (should (string-match "[gt]_[.0-9]+" (v-home%)))
  (should (string-match "[gt]_[.0-9]+.*x\\.el\\'" (v-home% "x.el"))))

(ert-deftest %b:init:v-home%> ()
  (should (file-name-nondirectory (v-home%> nil)))
  (should (string-match "[gt]_[.0-9]+.*x\\.el[cn]?\\'" (v-home%> "x.el"))))

(ert-deftest %b:init:progn% ()
  (should-not (progn%))
  (should (equal '(+ 1 2) (macroexpand '(progn% (+ 1 2)))))
  (should (equal '(progn (+ 1 2) (* 3 4))
                 (macroexpand '(progn% (+ 1 2) (* 3 4))))))

(ert-deftest %b:init:if% ()
  (should (= 3 (if% t (+ 1 2))))
  (should (= 12 (if% nil (+ 1 2) (* 3 4))))
  (should (equal '(+ 1 2) (macroexpand '(if% t (+ 1 2) (* 3 4)))))
  (should (equal '(progn (* 3 4) (* 5 6))
                 (macroexpand '(if% nil (+ 1 2) (* 3 4) (* 5 6))))))

(ert-deftest %b:init:when% ()
  (should-not (when% t))
  (should-not (when% nil))
  (should (= 3 (when% t (+ 1 2))))
  (should (equal '(progn (+ 1 2) (* 3 4))
                 (macroexpand '(when% t (+ 1 2) (* 3 4))))))

(ert-deftest %b:init:unless% ()
  (should-not (unless% t))
  (should-not (unless% nil))
  (should (= 3 (unless% nil (+ 1 2))))
  (should (equal '(progn (+ 1 2) (* 3 4))
                 (macroexpand '(unless% nil (+ 1 2) (* 3 4))))))

(ert-deftest %b:init:if-version% ()
  (should (if-version% < 0 t))
  (should (= 12 (if-version% < 10000 (+ 1 2) (* 3 4))))
  (should (equal '(progn (* 3 4) (* 5 6))
                 (macroexpand '(if-version% < 1000
                                   (+ 1 2)
                                 (* 3 4)
                                 (* 5 6))))))

(ert-deftest %b:init:when-version% ()
  (should (when-version% < 0 t))
  (should-not (when-version% < 10000 (+ 1 2)))
  (should (equal '(progn (+ 1 2) (* 3 4))
                 (macroexpand '(when-version%
                                   < 0
                                 (+ 1 2)
                                 (* 3 4))))))

(ert-deftest %b:init:time ()
  (should (null (time "")))
  (should (= 6 (time "a" (+ 1 2 3))))
  (should (= 120 (time "a" (+ 1 2 3) (* 4 5 6)))))

;; end of init

;;;
;; boot
;;;


(ert-deftest %c:boot:if/when/unless-lexical% ()
  (if (if-lexical% t nil)
      (should (and (when-lexical% t)
                   (not (unless-lexical% t))
                   (equal '(progn (+ 1 2) (* 3 4))
                          (macroexpand '(when-lexical%
                                          (+ 1 2) (* 3 4))))))
    (should (and (not (when-lexical% t))
                 (unless-lexical% t)
                 (equal '(progn (+ 1 2) (* 3 4))
                        (macroexpand '(unless-lexical%
                                        (+ 1 2) (* 3 4))))))))

(ert-deftest %c:boot:if/when/unless-graphic% ()
  (if (if-graphic% t nil)
      (should (and (when-graphic% t)
                   (not (unless-graphic% t))
                   (equal '(progn (+ 1 2) (* 3 4))
                          (macroexpand '(when-graphic%
                                          (+ 1 2) (* 3 4))))))
    (should (and (not (when-graphic% t))
                 (unless-graphic% t)
                 (equal '(progn (+ 1 2) (* 3 4))
                        (macroexpand '(unless-graphic%
                                        (+ 1 2) (* 3 4))))))))

(ert-deftest %c:boot:if/when/unless-platform% ()
  (cond ((if-platform% 'darwin t)
         (should (and (when-platform% 'darwin t)
                      (not (unless-platform% 'darwin t)))))
        ((if-platform% 'gnu/linux t)
         (should (and (when-platform% 'gnu/linux t)
                      (not (unless-platform% 'gnu/linux t)))))
        ((if-platform% 'windows-nt t)
         (should (and (when-platform% 'windows-nt t)
                      (not (unless-platform% 'windows-nt t)))))
        ((if-platform% 'cygwin t)
         (should (and (when-platform% 'cygwin t)
                      (not (unless-platform% 'cygwin)))))))

(ert-deftest %c:boot:if/when/unless-window% ()
	(cond ((if-window% 'mac t)
         (should (and (when-window% 'mac t)
                      (not (unless-window% 'mac t)))))
				((if-window% 'ns t)
         (should (and (when-window% 'ns t)
                      (not (unless-window% 'ns)))))
        ((if-window% 'pgtk t)
         (should (and (when-window% 'pgtk t)
                      (not (unless-window% 'pgtk t)))))
        ((if-window% 'w32 t)
         (should (and (when-window% 'w32 t)
                      (not (unless-window% 'w32 t)))))))

(ert-deftest %c:boot:setq% ()
  (should-not (setq% zzz 'xx))
  (should-not (setq% zzz nil)))

(ert-deftest %c:boot:if/when/unless-fn% ()
  (should (null (if-fn% 'shouldx 'ert t)))
  (should (and (if-fn% 'should 'ert t)
               (when-fn% 'should 'ert t)
               (not (unless-fn% 'should 'ert t))))
  (should (equal '(progn (+ 1 2) (* 3 4))
                 (macroexpand '(when-fn% 'should 'ert
                                 (+ 1 2) (* 3 4)))))
  (should (equal '(progn (+ 1 2) (* 3 4))
                 (macroexpand '(unless-fn% 'shouldx 'ert
                                 (+ 1 2) (* 3 4))))))

(ert-deftest %c:boot:if/when/unless-var% ()
  (should (and (if-var% ert-batch-backtrace-right-margin 'ert t)
               (when-var% ert-batch-backtrace-right-margin 'ert t)
               (not (unless-var% ert-batch-backtrace-right-margin 'ert))
               (unless-var% ert-batch-xxx 'ert t))))

(ert-deftest %c:boot:dolist* ()
  (should (equal '(a nil b c)
                 (let ((lst nil))
                   (dolist* (x '(a nil b c) (nreverse lst))
                     (push x lst)))))
  (should (catch 'found
            (dolist* (x '(a b c))
              (when (eq 'b x) (throw 'found t))))))

(ert-deftest %c:boot:self-spec-> ()
  (should (null (self-spec-> nil nil)))
  (should (null (self-spec-> '(a 1) nil)))
  (should (= 1 (self-spec-> '(a 1) 'a)))
  (should (= 1 (self-spec-> '(a (b (c 1))) 'a 'b 'c))))

(ert-deftest %c:boot:self-spec->% ()
  (should (null (self-spec->% nil nil)))
  (should (null (self-spec->% '(a 1) nil)))
  (should (= 1 (self-spec->% '(a 1) 'a)))
  (should (= 1 (self-spec->% '(a (b (c 1))) 'a 'b 'c))))


;; end of boot


;;;
;; fn
;;;

(unintern "if-feature-ert%")
(unintern "if-feature-ertxxx%")
(defmacro-if-feature% ert)
(defmacro-if-feature% ertxxx)

(unintern "if-fn-ert-delete-test%")
(unintern "if-fn-ert-delete-testxxx%")
(defmacro-if-fn% ert-delete-test ert)
(defmacro-if-fn% ert-delete-testxxx ert)

(ert-deftest %d:fn:emacs-arch ()
  (should (> (emacs-arch) 0)))

(ert-deftest %d:fn:defmacro-if-feature% ()
	(unwind-protect
			(progn
				(should (= 3 (if-feature-ert% (+ 1 2) (* 3 4))))
				(should (= 12 (if-feature-ertxxx% (+ 1 2) (* 3 4)))))
		(unintern "if-feature-ert%")
		(unintern "if-feature-ertxxx%")))

(ert-deftest %d:fn:defmacro-if-fn% ()
	(unwind-protect
			(progn
				(should (= 3 (if-fn-ert-delete-test% (+ 1 2) (* 3 4))))
				(should (= 12 (if-fn-ert-delete-testxxx% (+ 1 2) (* 3 4))))))
  (unintern "if-fn-ert-delete-test%")
	(unintern "if-fn-ert-delete-testxxx%"))

(ert-deftest %d:fn:flatten ()
  (should (equal '(nil) (flatten nil)))
  (should (equal '(a) (flatten 'a)))
  (should (equal '(a b) (flatten '(a (b)))))
  (should (equal '(a b c) (flatten '(a (b (c)))))))

(ert-deftest %d:fn:take ()
  (should-not (take 3 nil))
  (should (equal '(1 2 3) (take 3 (range 1 10 1))))
  (should (= 3 (length (take 3 (range 1 10 1)))))
  (should (= 10 (length (take 100 (range 1 10 1))))))

(ert-deftest %d:fn:drop ()
  (should-not (drop 3 nil))
  (should (equal '(8 9 10) (drop 7 (range 1 10 1))))
  (should (= 3 (length (drop 7 (range 1 10 1)))))
  (should (= 0 (length (drop 100 (range 1 10 1))))))

(ert-deftest %d:fn:drop-while ()
  (should-not (drop-while nil nil))
  (should-not (drop-while (lambda (x) (= x 1)) nil))
  (should (= 1 (car (drop-while (lambda (x) (< x 1))
                                (range 1 10 1)))))
  (should (equal '(2 3) (drop-while (lambda (x) (= x 1))
                                    (range 1 3 1))))
  (should (= 10 (length (drop-while (lambda (x) (> x 3))
                                    (range 1 10 1))))))

(ert-deftest %d:fn:ignore* ()
  (if-lexical%
      (should-not (let ((a 1) (b 2)) (ignore* a b)))
    (should-not (let ((a 1) (b 2)) (ignore* a b)))))

(ert-deftest %d:fn:take-while ()
  (should-not (take-while nil nil))
  (should-not (take-while (lambda (x) (= x 1)) nil))
  (should-not (take-while (lambda (x) (>= x 1))
                          (range 1 10 1)))
  (should (equal '(1 2) (take-while (lambda (x) (> x 2))
                                    (range 1 3 1))))
  (should (= 2 (length (take-while (lambda (x) (>= x 3))
                                   (range 1 10 1))))))

(ert-deftest %d:fn:push! ()
  (should (equal '(a b c) (let ((s '(a b c)))
                            (push! 'a s t)
                            s)))
  (should (equal (list (list "a" "b") (list "a" "bb"))
                 (let ((x (list "b")))
                   (list (push! "a" x)
                         (let ((x (list "bb")))
                           (push! "a" x)
                           (push! "a" x t)))))))

(ert-deftest %d:fn:append! ()
  (should (equal '(a) (let ((s nil)) (append! 'a s))))
  (should (equal '(a b c) (let ((s '(a b c)))
                            (append! 'c s t)
                            s)))
  (should (equal (list (list "a" "b") (list "aa" "b"))
                 (let ((x (list "a")))
                   (list (append! "b" x t)
                         (let ((x (list "aa")))
                           (append! "b" x t)
                           (append! "b" x t)))))))

(ert-deftest %d:fn:insert! ()
  (should (equal '(a X b) (let ((s '(a b)))
                            (insert! 'X s 1)))))

(ert-deftest %d:fn:assoc** ()
  (should (equal '(a 1) (assoc** 'a '((b 2) (a 1)))))
  (should (equal '(a 1) (assoc** 'a '((b 2) (a 1)) :test #'eq)))
  (let ((lst '((b 2) (a 1))))
    (should (equal (assoc** 'a lst :test #'eq) (assq 'a lst))))
  (should (equal '("a" a) (assoc** "a" '(("b" b) ("a" a)) :test #'string=)))
  (let ((k "a") (lst '(("b" b) ("a" a))))
    (should (equal (assoc** k lst :test #'string=) (assoc-string k lst)))))

(ert-deftest %d:fn:mapcar** ()
  (should (equal '(a b c) (mapcar** #'identity '(a b c))))
  (should (equal '((a 1) (b 2) (c 3))
                 (mapcar** #'list '(a b c) '(1 2 3)))))

(ert-deftest %d:fn:remove-if* ()
  (should-not (remove-if* nil nil))
  (should-not (remove-if* (lambda (x) (eq x 'a)) nil))
  (should (equal '(b c) (remove-if* (lambda (x) (eq x 'a)) '(a b c))))
  (should (equal '("a") (remove-if* (lambda (x) (string= x "b"))
                                    '("a" "b"))))
  (should (equal '((1 "a"))
                 (remove-if* (lambda (x) (string= x "b"))
                             '((1 "a") (2 "b") (2 "b") (2 "b"))
                             :key #'cadr)))
  (should (equal '((1 "a") (2 "b"))
                 (remove-if* (lambda (x) (string= x "b"))
                             '((1 "a") (2 "b") (2 "b") (2 "b"))
                             :key #'cadr :count 2)))
  (should (equal '((1 "a"))
                 (remove-if* (lambda (x) (string= x "b"))
                             '((1 "a") (2 "b") (2 "b") (2 "b"))
                             :key #'cadr :count 3)))
  (should (equal '((1 "a") (2 "b"))
                 (remove-if* (lambda (x) (string= x "c"))
                             '((1 "a") (2 "b") (3 "c"))
                             :key #'cadr :from-end t)))
  (should (equal '((1 "a") (2 "b"))
                 (remove-if* (lambda (x) (string= x "b"))
                             '((1 "a") (2 "b") (2 "b") (2 "b"))
                             :key #'cadr :start 2)))
  (should (equal '((1 "a") (2 "b") (2 "b"))
                 (remove-if* (lambda (x) (string= x "b"))
                             '((1 "a") (2 "b") (2 "b") (2 "b"))
                             :key #'cadr :end 2))))

(ert-deftest %d:fn:member-if* ()
  (should-not (member-if* nil nil))
  (should-not (member-if* (lambda (x) (eq x 'a)) nil))
  (should (equal '(a) (member-if* (lambda (x) (eq x 'a)) '(b a))))
  (should (equal '("a") (member-if* (lambda (x) (string= x "a"))
                                    '("b" "a"))))
  (should (equal '((3 "c")) (member-if* (lambda (x) (string= x "c"))
                                        '((1 "a") (2 "b") (3 "c"))
                                        :key #'cadr))))

(ert-deftest %d:fn:every* ()
  (should (every* #'stringp "" "a" "b"))
  (should (every* #'< '(1 2 3) '(2 3 4)))
  (should-not (every* #'< '(1 2 3) '(2 3 3))))

(ert-deftest %d:fn:some* ()
  (should (some* #'characterp "abc"))
  (should (some* #'< '(1 2 3) '(1 2 4)))
  (should-not (some* #'< '(1 2 3) '(1 2 3))))

(ert-deftest %d:fn:loop* ()
  (should (equal '(1 2 3) (loop* for i from 1 to 3 collect i)))
  (should (= 3 (loop* for x in '(a b c)
                      count x)))
  (should (= 5050 (loop* for i from 1 to 100 sum i)))
  (should (string= "b" (loop* for d in '("a" "b" "c")
                              when (string= d "b")
                              return d)))
  (should (string= "b" (loop* for d in '("a" "b" "c")
                              with d1 = nil
                              do (setq d1 (concat d "/" "1"))
                              when (string= d1 "b/1")
                              return d))))

(ert-deftest %d:fn:fluid-let ()
  (let ((x 123))
    (fluid-let (x 456)
      (should (= x 456)))
    (should (= x 123))))

(ert-deftest %d:fn:strchr ()
  (should (null (strchr nil nil)))
  (should (null (strchr nil ?a)))
  (should (null (strchr "abc" ?d)))
  (should (= 2 (strchr "abc" ?c))))

(ert-deftest %d:fn:strrchr ()
  (should (null (strrchr nil nil)))
  (should (null (strrchr nil ?a)))
  (should (null (strrchr "abc" ?d)))
  (should (= 0 (strrchr "abc" ?a))))

(ert-deftest %d:fn:split-string* ()
  (should (equal '("a" "b" "c")
                 (split-string* "a,b,,cXX" "," t "XX")))
  (should (equal '("a" "b" "c")
                 (split-string* "a,b@@cXX" "[,@]" t "XX")))
  (should (equal '("a" "b" "c")
                 (split-string* "a,b@@cXX" ",\\|@" t "XX")))
  (should (equal '("a" "b")
                 (split-string* "a,,b" "," t)))
  (should (equal '("a" "" "b")
                 (split-string* "a,,b" "," nil)))
  (should (equal '("a" "b")
                 (split-string* "a, b " "," t " "))))

(ert-deftest %d:fn:string-trim> ()
  (should-not (string-trim> nil "X"))
  (should (string= "abc" (string-trim> "abc \n  ")))
  (should (string= "abc" (string-trim> "abcXX" "XX")))
  (should (string= "abc" (string-trim> "abcXX" "X+"))))

(ert-deftest %d:fn:string-trim< ()
  (should-not (string-trim< nil "X"))
  (should (string= "abc" (string-trim< "  \n abc")))
  (should (string= "abc" (string-trim< "XXabc" "XX")))
  (should (string= "abc" (string-trim< "XXabc" "X+"))))

(ert-deftest %d:fn:string-trim>< ()
  (should-not (string-trim>< nil "X" "Z"))
  (should (string= "abc" (string-trim>< " \n abc \n ")))
  (should (string= "abc" (string-trim>< "ZZabcXX" "X+" "Z+"))))

(ert-deftest %d:fn:string-match* ()
  (should-not (string-match* nil nil 0))
  (should-not (string-match* nil 123 0))
  (should (string= "XXabcXX"
                   (string-match* "XX\\(abc\\)XX" "XXabcXX" 0)))
  (should-not (string-match* "XX\\(abc\\)XX" "XXabcXX" 2))
  (should (string= "abc"
                   (string-match* "XX\\(abc\\)XX" "XXabcXX" 1))))

(ert-deftest %d:fn:file-name-base* ()
  (should (string= "x" (file-name-base* "x")))
  (should (string= "x" (file-name-base* "x.z")))
  (should (string= "x" (file-name-base* "/a/b.c/x.z"))))

(ert-deftest %d:fn:directory-name-p ()
  (should (directory-name-p "a/"))
  (should-not (directory-name-p "a")))

(ert-deftest %d:fn:make-thread* ()
  (should (= 6 (make-thread* (lambda () (* 2 3)) t))))

(ert-deftest %d:fn:posix-path ()
  (should-not (posix-path nil))
  (let ((p "c:/a/b/c.c"))
    (should (string= p (posix-path "c:/a/b/c.c")))
    (should (string= p (posix-path "c:\\a\\b\\c.c")))))

(ert-deftest %d:fn:save/read-sexp-to/from-file ()
  (let ((f1 (concat temporary-file-directory
                    (make-temp-name (symbol-name (gensym*)))))
        (f2 (concat temporary-file-directory
                    (make-temp-name (symbol-name (gensym*)))))
        (s1 '(defvar test%fn-srstff t))
        (t1 (make-hash-table :test 'string-hash=)))
    (should (and (save-sexp-to-file s1 f1)
                 (equal s1 (read-sexp-from-file f1))))
    (puthash "a" 1 t1)
    (puthash "b" 2 t1)
    (should (= 2 (hash-table-count t1)))
    (should (and (save-sexp-to-file t1 f2)
                 (= 2 (gethash "b" (read-sexp-from-file f2)))))))

(ert-deftest %d:fn:save/read-str-to/from-file ()
  (let ((f (concat temporary-file-directory
                   (symbol-name (gensym*)))))
    (should (and (save-str-to-file "abc" f)
                 (string= "abc" (read-str-from-file f))))))

(ert-deftest %d:fn:shell-command* ()
  (should (let ((x (shell-command* "echo" "a")))
            (and (= (car x) 0) (string= (cdr x) "a\n"))))
  (should (let ((x (shell-command* "wc" "-c"
                                   (emacs-home* "test.el"))))
            (or (= (car x) 0)
                (= (car x) 127)))))

(ert-deftest %d:fn:executable-find% ()
  (if (eq system-type 'windows-nt)
      (should (executable-find% "dir"))
    (should (executable-find% "ls"))
    (should (executable-find% (concat "l" "s")))
    (should (executable-find% "ls" (lambda (ls) ls)))))

(ert-deftest %d:fn:platform-arch ()
  (should (platform-arch))
  (should (consp (platform-arch))))

;; end of `fn'


;;;
;; basic
;;;

(ert-deftest %e:basic:file-in-dirs-p ()
  (should-not (file-in-dirs-p nil nil))
  (should-not (file-in-dirs-p (emacs-home* "init.el") nil))
  (should-not (file-in-dirs-p (emacs-home* "init.el")
                              (list (emacs-home* "config/"))))
  (should (file-in-dirs-p (emacs-home* "init.el")
                          (list (emacs-home*))))
  (should (file-in-dirs-p (emacs-home* "init.elx")
                          (list (emacs-home*))))
  (should (file-in-dirs-p (emacs-home* "init.el")
                          (list (string-trim> (emacs-home*) "/")))))

(ert-deftest %e:basic:file-name-nondirectory% ()
  (should (string= "c.c" (file-name-nondirectory% "/a/b/c.c")))
  (should (string= "c.c" (file-name-nondirectory%
                          (concat "/a/b/" "c.c")))))


(ert-deftest %e:basic:ssh-remote-/p/>ids/>user@host ()
  (should (and (null (ssh-remote-p nil))
               (null (ssh-remote-p "/xxh:abc:/a/b.c"))
               (string= "/sshx:pi:"
                        (ssh-remote-p "/sshx:pi:/a/b.c"))
               (string= "/ssh:pi@circle:"
                        (ssh-remote-p
                         "/ssh:pi@circle:/a/b/c.d"))
               (string= "/ssh:u@h.i.j:"
                        (ssh-remote-p
                         "/ssh:u@h.i.j:/a/b.c"))))
  (should (and (null (ssh-remote->ids nil))
               (equal '("abc") (ssh-remote->ids "abc"))
               (equal '("sshx" "pi")
                      (ssh-remote->ids "/sshx:pi:"))
               (equal '("ssh" "u" "h")
                      (ssh-remote->ids "/ssh:u@h:"))
               (equal '("ssh" "u" "h.i.j")
                      (ssh-remote->ids "/ssh:u@h.i.j:/a/b.c"))))
  (should (and (null (ssh-remote->user@host nil))
               (string= "pi@circle"
                        (ssh-remote->user@host
                         "/ssh:pi@circle:/a/b.c"))
               (string= "u@h.i.j"
                        (ssh-remote->user@host
                         "/sshx:u@h.i.j:/a/b.c")))))

(ert-deftest %e:basic:path+ ()
  (should-not (path+ nil))
  (should (string= "a/" (path+ "a")))
  (should (string= "a/b/c/" (path+ "a/" "b/" "c/")))
  (should (string= "a/b/c/" (path+ "a/" "b" "c"))))

(ert-deftest %e:basic:path- ()
  (should-not (path- nil))
  (should (string= "a/b/" (path- "a/b/c")))
  (should (string= "a/b/" (path- "a/b/c/"))))

(ert-deftest %e:basic:path-depth ()
  (should (= 0 (path-depth nil)))
  (should (= 0 (path-depth "")))
  (should (= 1 (path-depth "/")))
  (should (= 1 (path-depth "./")))
  (should (= 0 (path-depth "abc")))
  (should (= 1 (path-depth "/abc")))
  (should (= 4 (path-depth "//a//b")))
  (should (= 1 (path-depth "\\a" "\\\\")))
  (should (= 2 (path-depth "/a/b")))
  (should (= 3 (path-depth "/a/b/")))
  (should (= 3 (path-depth "/a/b/c"))))

(ert-deftest %e:basic:dir-iterate ()
  (should (string-match
           "init\\.el\\'"
           (catch 'out
             (dir-iterate (emacs-home*)
                          (lambda (f _)
                            (string= "init.el" f))
                          nil
                          (lambda (a)
                            (throw 'out a))
                          nil))))
  (should (string-match
           "/config/"
           (catch 'out
             (dir-iterate (emacs-home*)
                          nil
                          (lambda (f _)
                            (string= "config/" f))
                          nil
                          (lambda (a)
                            (throw 'out a))))))
  (let ((matched nil))
    (dir-iterate (emacs-home*)
                 (lambda (f _)
                   (string-match "init\\.el\\'\\|basic\\.el\\'" f))
                 (lambda (d _)
                   (string-match "config" d))
                 (lambda (f)
                   (setq matched (cons f matched)))
                 nil)
    (should (= 2 (length matched)))))

(ert-deftest %e:basic:dir-backtrack ()
  (should (catch 'out
            (dir-backtrack (emacs-home* "config/")
                           (lambda (d fs)
                             (when (string-match "config/" d)
                               (throw 'out t))))))
  (should (catch 'out
            (dir-backtrack (emacs-home* "config/basic.el")
                           (lambda (d fs)
                             (dolist* (x fs)
                               (when (string= "init.el" x)
                                 (throw 'out t)))))))
  (should (= 2 (let ((prefered nil)
                     (count 0)
                     (std '("init.el" ".git/")))
                 (dir-backtrack (emacs-home* "config/basic.el")
                                (lambda (d fs)
                                  (dolist* (x fs)
                                    (when (or (string= "init.el" x)
                                              (string= ".git/" x))
                                      (push x prefered)))))
                 (dolist* (x prefered count)
                   (when (member-if* (lambda (z) (string= z x))
                                     std)
                     (setq count (1+ count))))))))

(ert-deftest %e:basic:if-key% ()
  (should (string= "defined"
                   (if-key% (current-global-map) (kbd "C-x C-c")
                            (lambda (def)
                              (eq def #'save-buffers-kill-terminal))
                            "defined"
                     "undefined")))
  (should (string= "undefined"
                   (if-key% (current-global-map) (kbd "C-x C-c")
                            (lambda (def)
                              (not (eq def #'xxx)))
                            "undefined"))))

(ert-deftest %e:basic:if-region-active ()
  ;; interactive
  (unless noninteractive
    (with-temp-buffer
      (insert "a bb")
      (should (= 12 (if-region-active (+ 1 2) (* 3 4))))
      (set-mark (point-min))
      (set-mark (point-max))
      (should (= 3 (if-region-active (+ 1 2) (* 3 4)))))))

(ert-deftest %e:basic:unless-region-active ()
  ;; interactive
  (unless noninteractive
    (with-temp-buffer
      (insert "a bb")
      (should (= 12 (unless-region-active (* 3 4))))
      (set-mark (point-min))
      (set-mark (point-max))
      (should-not (unless-region-active (+ 1 2) (* 3 4))))))

(ert-deftest %e:basic:symbol@ ()
  ;; interactive
  (unless noninteractive
    (with-temp-buffer
      (insert "a bb")
      (goto-char (point-min))
      (should (null (car (symbol@))))
      (set-mark (point-min))
      (set-mark (point-max))
      (should (string= "a bb" (cdr (symbol@)))))))


;; end of basic

;;;
;; shells
;;;

(ert-deftest %f:shells:shells-spec->% ()
  ;; (should (= 6 (length (shells-spec->%))))
  (should (string= "shell-env"
                   (file-name-base* (shells-spec->% :file)))))

(ert-deftest %f:shells:shell-env->/<- ()
  (should (*default-shell-env* :put! :xx "aa"))
  (should (string= "aa" (*default-shell-env* :get :xx))))

(ert-deftest %f:shells:echo-var ()
  (should (string= "1" (let ((process-environment '("A=1")))
                         (echo-var "A")))))

(ert-deftest %f:shells:paths->var ()
  (let ((path-separator ":"))
    (should (string= "a:b:c" (paths->var '("a" "b" "c"))))
    (should (string= "b" (paths->var '("a" "b" "c")
                                     (lambda (x) (string= "b" x)))))
    (should (string= "" (paths->var '("a" "b" "c")
                                    (lambda (x) (file-exists-p x)))))))

(ert-deftest %f:shells:var->paths ()
  (let ((path-separator ":"))
    (should (equal '("a" "b" "c") (var->paths "a:b:c")))))

(ert-deftest %f:shells:setenv* ()
  (should (member "D=44"
                  (let ((process-environment '("A=1" "B=2" "C=3")))
                    (setenv* "D" "44"))))
  (should (member "C=4"
                  (let ((process-environment '("A=1" "B=2" "C=3")))
                    (setenv* "C" "4")))))

(ert-deftest %f:shells:copy-env-vars! ()
  (should (member "A=1"
                  (let ((env '(("A" . "1") ("B" . "2") ("C" . "3")))
                        (process-environment '("B=2")))
                    (copy-env-vars! env '("A"))
                    process-environment))))

(ert-deftest %f:shells:spin-env-vars! ()
  (should (member "C=22"
                  (let ((process-environment '("A=1" "B=2" "C=3")))
                    (spin-env-vars! '(("C" . "22")))
                    process-environment))))

;; end of shells


(comment
 (ert-deftest %g:module:install/delete-package!1 ()
   (unless *repository-initialized*
     (initialize-package-repository!)
     (setq *repository-initialized* t))
   (should-not (delete-package!1 nil))
   (should-not (install-package!1 nil))
   (let ((already (assq 'htmlize package-alist)))
     (if already
         (progn
           (should (delete-package!1 'htmlize))
           (should (install-package!1 'htmlize)))
       (should (install-package!1 'htmlize)))
     (should (delete-package!1 'htmlize))
     (should (install-package!1 '(:name 'htmlize)))
     (should (delete-package!1 'htmlize))
     (should (install-package!1 '(:name 'htmlize :version 1.54)))
     (should (delete-package!1 'htmlize))
     (when already (should (install-package!1 'htmlize))))))


;;;
;; conditional: cc
;;;

(ert-deftest %q:cc:+cc*-compiler-bin+ ()
  (when-platform% 'windows-nt
    (should (message "# (executable-find \"cc-env.bat\") = %s"
                     (or (executable-find "cc-env.bat") "")))
    (should (message "# (executable-find \"cl\") = %s"
                     (or (executable-find "cl") ""))))
  (should (message "# (executable-find \"gcc\") = %s"
                   (or (executable-find "gcc") "")))
  (should (message "# (executable-find \"clang\") = %s"
                   (or (executable-find "clang") "")))
  (should (message "# (executable-find \"make\") = %s"
                   (or (executable-find "make") "")))
  (when-var% +cc*-compiler-bin+ nil
    (should (message "# +cc*-compiler-bin+ = %s"
                     (or (cc*-compiler) "")))))

(ert-deftest %q:cc:cc*-system-include ()
  (when-fn% 'cc*-system-include nil
    (should (message "# cc*-system-include = %s"
                     (or (cc*-system-include t) "")))))

;; end of cc

;;;
;; conditional: trans
;;;

(ert-deftest %r:trans:roman->arabic ()
  (when-fn% 'roman->arabic nil
    (should (= 1990 (roman->arabic "MCMXC")))
    (should (= 2008 (roman->arabic "MMVIII")))
    (should (= 1666 (roman->arabic "MDCLXVI")))))

(ert-deftest %r:trans:chinese->arabic ()
  (when-fn% 'chinese->arabic nil
    (should (= 91234567 (chinese->arabic
                         (split-string*
                          "玖仟壹佰贰拾叁万肆仟伍佰陆拾柒" "" t)
                         0)))
    (should (= 91234567 (chinese->arabic
                         (split-string*
                          "玖仟壹佰贰拾叁万肆仟伍佰陆拾零柒" "" t)
                         0)))
    (should (= 678991234567 (chinese->arabic
                             (split-string*
                              "陆仟柒佰捌拾玖亿玖仟壹佰贰拾叁万肆仟伍佰陆拾柒"
                              "" t)
                             0)))))

;; end of trans


;; end of test.el
