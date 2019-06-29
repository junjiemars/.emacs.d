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

(ert-deftest %init:gensym* ()
  (should (string-match "^g[0-9]+" (format "%s" (gensym*))))
  (should (string-match "^X[0-9]+" (format "%s" (gensym* "X")))))

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

(ert-deftest %init:if-version% ()
  (should (if-version% < 0 t))
  (should (if-version% < "0" t))
  (should (if-version% < '0 t))
  (should (= 12 (if-version% < 1000 (+ 1 2) (* 3 4))))
  (should (equal '(progn (* 3 4) (* 5 6))
                 (macroexpand '(if-version%
                                   < 1000
                                   (+ 1 2)
                                 (* 3 4)
                                 (* 5 6))))))

(ert-deftest %init:when-version% ()
  (should (when-version% < 0 t))
  (should (eq nil (when-version% < 1000 (+ 1 2))))
  (should (equal '(progn (+ 1 2) (* 3 4))
                 (macroexpand '(when-version%
                                   < 0
                                 (+ 1 2)
                                 (* 3 4))))))

(ert-deftest %strap:if/when/unless-lexical% ()
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

(ert-deftest %strap:if/when/unless-graphic% ()
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

(ert-deftest %strap:when-version% ()
  (should (when-version% < 0 t))
  (should (not (when-version% < 1000 t))))

(ert-deftest %strap:if/when/unless-platform% ()
  (cond ((if-platform% 'darwin t)
         (should (and (when-platform% 'darwin t)
                      (not (unless-platform% 'darwin t)))))
        ((if-platform% 'gnu/linux t)
         (should (and (when-platform% 'gnu/linux t)
                      (not (unless-platform% 'gnu/linux t)))))
        ((if-platform% 'windows-nt t)
         (should (and (when-platform% 'windows-nt t)
                      (not (unless-platform% 'windows-nt t)))))))

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

(ert-deftest %strap:ignore* ()
  (if-lexical%
      (should (eq nil (macroexpand '(ignore* a b))))
    (should (eq nil (macroexpand '(ignore* a b))))))

(ert-deftest %strap:dolist* ()
  (let ((lst nil))
    (should (equal '(a nil b c)
                   (dolist* (x '(a nil b c) (nreverse lst))
                     (push x lst))))))

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

(ert-deftest %basic:remove-if* ()
  (should (eq nil (remove-if* nil nil)))
  (should (equal '(a b c) (remove-if* nil '(a b c))))
  (should (equal '(b c) (remove-if* (lambda (x)
                                      (eq 'a x))
                                    '(a b c))))
  (should (equal '(c) (remove-if* (lambda (x)
                                    (or (eq 'a x) (eq 'b x)))
                                  '(a b c))))
  (should (equal '("b" "c") (remove-if* (lambda (x)
                                          (string= "a" x))
                                        '("a" "b" "c")))))

(ert-deftest %basic:member** ()
  (should (eq nil (member** nil nil)))
  (should (eq nil (member** 'a nil)))
  (should (equal '(a) (member** 'a '(b a))))
  (should (equal '("a") (member** "a" '("b" "a") :test #'string=))))

(ert-deftest %basic:every* ()
  (should (every* nil))
  (should (every* #'stringp))
  (should-not (every* #'stringp nil))
  (should (every* #'stringp "" "a" "b"))
  (should-not (every* #'stringp "" "a" nil "b")))

(ert-deftest %basic:split-string* ()
  (should (equal '("a" "b" "c")
                 (split-string* "a,b,,cXX" "," t "XX")))
  (should (equal '("a" "b" "c")
                 (split-string* "a,b@@cXX" "[,@]" t "XX"))))

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
  (should (eq nil (match-string* nil 123 0)))
  (should (string= "abc" (match-string* "xx\\(abc\\)xx" "xxxabcxx" 1))))

(ert-deftest %basic:string=* ()
  (should (string=* "a" "a"))
  (should (string=* "a" "a" "a"))
  (should-not (string=* "a" "a" "a" "b")))

(ert-deftest %basic:char=* ()
  (should (char=* ?a ?a))
  (should (char=* ?a ?a ?a))
  (should-not (char=* ?a ?a ?a ?b)))

(ert-deftest %basic:save-sexp-to-file ()
  (let ((f (emacs-home* "private/x.el")))
    (should (and (save-sexp-to-file '(defvar test%basic-sstf1 t) f)
                 (file-exists-p f)
                 (and (load f t) test%basic-sstf1)))
    (should (or (delete-file f)
                (not (file-exists-p f))))))

(ert-deftest %basic:save/read-str-to/from-file ()
  (let ((f (emacs-home* "private/x.el")))
    (should (and (save-str-to-file "abc" f)
                 (string= "abc" (read-str-from-file f))))
    (should (or (delete-file f)
                (not (file-exists-p f))))))

(ert-deftest %basic:save-hash-table-to-file ()
  (let ((x (make-hash-table :test 'string-hash=))
        (f (emacs-home* "private/x.tbl")))
    (puthash "a" 1 x)
    (puthash "b" 2 x)
    (should (= 2 (hash-table-count x)))
    (save-hash-table-to-file 'test%basic-shttf1 x f 'string-hash=)
    (should (and (file-exists-p f)
                 (load f)
                 (= 2 (gethash "b" test%basic-shttf1))))
    (should (or (delete-file f)
                (not (file-exists-p f))))))

(ert-deftest %basic:remote-norm-file/id/>user@host ()
  (should (and (null (remote-norm-file nil))
               (null (remote-norm-file "/xxh:abc:/a/b/c"))
               (string= "/sshx:pi:"
                        (remote-norm-file "/sshx:pi:/a/b/c.d"))
               (string= "/ssh:pi@circle:"
                        (remote-norm-file "/ssh:pi@circle:/a/b/c.d"))))
  (should (and (null (remote-norm-id nil))
               (equal '("abc") (remote-norm-id "abc"))
               (equal '("sshx" "pi") (remote-norm-id "/sshx:pi:"))
               (equal '("ssh" "pi" "circle")
                      (remote-norm-id "/ssh:pi@circle:"))
               (equal '("sshx" "pi")
                      (remote-norm-id "/sshx:pi:"))))
  (should (and (null (remote-norm->user@host nil))
               (string= "pi@circle"
                        (remote-norm->user@host "/ssh:pi@circle:"))
               (string= "pi"
                        (remote-norm->user@host "/sshx:pi:")))))

(ert-deftest %basic:take ()
  (should (eq nil (take 3 nil)))
  (should (equal '(1 2 3) (take 3 (range 1 10 1))))
  (should (= 3 (length (take 3 (range 1 10 1)))))
  (should (= 10 (length (take 100 (range 1 10 1))))))

(ert-deftest %basic:drop-while ()
  (should (eq nil (drop-while nil nil)))
  (should (eq nil (drop-while (lambda (x) (= x 1)) nil)))
  (should (eq nil (drop-while (lambda (x) (< x 1))
                              (range 1 10 1))))
  (should (equal '(2 3) (drop-while (lambda (x) (= x 1))
                                    (range 1 3 1))))
  (should (= 7 (length (drop-while (lambda (x) (>= x 3))
                                   (range 1 10 1))))))

(ert-deftest %basic:take-while ()
  (should (eq nil (take-while nil nil)))
  (should (eq nil (take-while (lambda (x) (= x 1)) nil)))
  (should (eq nil (take-while (lambda (x) (>= x 1))
                              (range 1 10 1))))
  (should (equal '(1 2) (take-while (lambda (x) (> x 2))
                                    (range 1 3 1))))
  (should (= 2 (length (take-while (lambda (x) (>= x 3))
                                   (range 1 10 1))))))

(ert-deftest %basic:path+ ()
  (should-not (path+ nil))
  (should (string= "a/" (path+ "a")))
  (should (string= "a/b/c/" (path+ "a/" "b/" "c/")))
  (should (string= "a/b/c/" (path+ "a/" "b" "c"))))

(ert-deftest %basic:path- ()
  (should-not (path- nil))
  (should (string= "a/b/" (path- "a/b/c")))
  (should (string= "a/b/" (path- "a/b/c/"))))

(ert-deftest %basic:dir-iterate ()
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
                   (not (string-match "\\.git\\'\\|[gt]_.*/\\'" d)))
                 (lambda (f)
                   (push f matched))
                 nil)
    (should (= 2 (length matched)))))

(ert-deftest %basic:dir-backtrack ()
  (should (catch 'out
            (dir-backtrack (emacs-home* "config/")
                           (lambda (d fs)
                             (when (string-match "\\.emacs\\.d/\\'" d)
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
                   (when (member** x std :test #'string=)
                     (setq count (1+ count))))))))

(ert-deftest %basic:executable-find% ()
  (if-platform% 'windows-nt
      (should (executable-find% "dir"))
    (should (executable-find% "ls"))
    (should (executable-find% (concat "l" "s")))
    (should (executable-find% "ls"
                              (lambda (ls)
                                (let ((x (shell-command* "sh"
                                           "--version")))
                                  (car x)))))))

(when-platform% 'windows-nt
  (ert-deftest %basic:windows-posix-path ()
    (should (eq nil (windows-nt-posix-path nil)))
    (should (string= "c:/a/b/c.c"
                     (windows-nt-posix-path "c:/a/b/c.c")))
    (should (string= "c:/a/b/c.c"
                     (windows-nt-posix-path "c:\\a\\b\\c.c")))
    (should (string= "c:/a/B/c.c"
                     (windows-nt-posix-path "C:\\a\\B\\c.c")))))

(ert-deftest %basic:if-key% ()
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

;; end of file
