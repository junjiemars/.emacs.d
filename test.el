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

;;; init

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
    (should (and (null (delete-directory p))
                 (null (file-exists-p p))))))

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
  (should-not (progn%))
  (should (equal '(+ 1 2) (macroexpand '(progn% (+ 1 2)))))
  (should (equal '(progn (+ 1 2) (* 3 4)) (macroexpand '(progn% (+ 1 2) (* 3 4))))))

(ert-deftest %init:if% ()
  (should (= 3 (if% t (+ 1 2))))
  (should (= 12 (if% nil (+ 1 2) (* 3 4))))
  (should (equal '(+ 1 2) (macroexpand '(if% t (+ 1 2) (* 3 4)))))
  (should (equal '(progn (* 3 4) (* 5 6))
                 (macroexpand '(if% nil (+ 1 2) (* 3 4) (* 5 6))))))

(ert-deftest %init:when% ()
  (should-not (when% t))
  (should-not (when% nil))
  (should (= 3 (when% t (+ 1 2))))
  (should (equal '(progn (+ 1 2) (* 3 4))
                 (macroexpand '(when% t (+ 1 2) (* 3 4))))))

(ert-deftest %init:unless% ()
  (should-not (unless% t))
  (should-not (unless% nil))
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
  (should-not (when-version% < 1000 (+ 1 2)))
  (should (equal '(progn (+ 1 2) (* 3 4))
                 (macroexpand '(when-version%
                                   < 0
                                 (+ 1 2)
                                 (* 3 4))))))

 ;; end of init

;;;;
;; boot
;;;;

(ert-deftest %boot:if/when/unless-lexical% ()
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

(ert-deftest %boot:if/when/unless-graphic% ()
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

(ert-deftest %boot:when-version% ()
  (should (when-version% < 0 t))
  (should (not (when-version% < 1000 t))))

(ert-deftest %boot:if/when/unless-platform% ()
  (cond ((if-platform% 'darwin t)
         (should (and (when-platform% 'darwin t)
                      (not (unless-platform% 'darwin t)))))
        ((if-platform% 'gnu/linux t)
         (should (and (when-platform% 'gnu/linux t)
                      (not (unless-platform% 'gnu/linux t)))))
        ((if-platform% 'windows-nt t)
         (should (and (when-platform% 'windows-nt t)
                      (not (unless-platform% 'windows-nt t)))))))

(ert-deftest %boot:setq% ()
  "uncompleted..."
  (when% t
    (should-not (setq% zzz 'xx))
    (should-not (setq% zzz nil))))

(ert-deftest %boot:if/when/unless-fn% ()
  (should (and (if-fn% 'should 'ert t)
               (when-fn% 'should 'ert t)
               (not (unless-fn% 'should 'ert t))))
  (should (equal '(progn (+ 1 2) (* 3 4))
                 (macroexpand '(when-fn% 'should 'ert (+ 1 2) (* 3 4)))))
  (should (equal '(progn (+ 1 2) (* 3 4))
                 (macroexpand '(unless-fn% 'shouldxxx 'ert (+ 1 2) (* 3 4))))))

(ert-deftest %boot:if/when-var% ()
  (should (and (if-var% ert-batch-backtrace-right-margin 'ert t)
               (when-var% ert-batch-backtrace-right-margin 'ert t)
               (not (when-var% ert-batch-backtrace-right-marginxxx 'ert t)))))

(ert-deftest %boot:ignore* ()
  (if-lexical%
      (should-not (macroexpand '(ignore* a b)))
    (should-not (macroexpand '(ignore* a b)))))

(ert-deftest %boot:dolist* ()
  (should (equal '(a nil b c)
                 (let ((lst nil))
                   (dolist* (x '(a nil b c) (nreverse lst))
                     (push x lst)))))
  (should (catch 'found
            (dolist* (x '(a b c))
              (when (eq 'b x) (throw 'found t))))))

(ert-deftest %boot:defmacro-if-feature% ()
  (defmacro-if-feature% ert "just for ert test.")
  (defmacro-if-feature% ertxxx "just for negative ert test.")
  (should (= 3 (if-feature-ert% (+ 1 2) (* 3 4))))
  (should (= 12 (if-feature-ertxxx% (+ 1 2) (* 3 4)))))


 ;; end of boot


;;;;
;; fns
;;;;

(ert-deftest %fns:flatten ()
  (should (equal '(nil) (flatten nil)))
  (should (equal '(a) (flatten 'a)))
  (should (equal '(a b) (flatten '(a (b)))))
  (should (equal '(a b c) (flatten '(a (b (c)))))))

(ert-deftest %fns:take ()
  (should-not (take 3 nil))
  (should (equal '(1 2 3) (take 3 (range 1 10 1))))
  (should (= 3 (length (take 3 (range 1 10 1)))))
  (should (= 10 (length (take 100 (range 1 10 1))))))

(ert-deftest %fns:drop ()
  (should-not (drop 3 nil))
  (should (equal '(8 9 10) (drop 7 (range 1 10 1))))
  (should (= 3 (length (drop 7 (range 1 10 1)))))
  (should (= 0 (length (drop 100 (range 1 10 1))))))

(ert-deftest %fns:drop-while ()
  (should-not (drop-while nil nil))
  (should-not (drop-while (lambda (x) (= x 1)) nil))
  (should (= 1 (car (drop-while (lambda (x) (< x 1))
                                (range 1 10 1)))))
  (should (equal '(2 3) (drop-while (lambda (x) (= x 1))
                                    (range 1 3 1))))
  (should (= 10 (length (drop-while (lambda (x) (> x 3))
                                    (range 1 10 1))))))

(ert-deftest %fns:take-while ()
  (should-not (take-while nil nil))
  (should-not (take-while (lambda (x) (= x 1)) nil))
  (should-not (take-while (lambda (x) (>= x 1))
                          (range 1 10 1)))
  (should (equal '(1 2) (take-while (lambda (x) (> x 2))
                                    (range 1 3 1))))
  (should (= 2 (length (take-while (lambda (x) (>= x 3))
                                   (range 1 10 1))))))

(ert-deftest %fns:push! ()
  (should (equal (list (list "a" "b") (list "a" "bb"))
                 (let ((x (list "b")))
                   (list (push! "a" x)
                         (let ((x (list "bb")))
                           (push! "a" x)
                           (push! "a" x nil t))))))
  (should (equal (list (list "a" "b") (list "aa" "b"))
                 (let ((x (list "a")))
                   (list (push! "b" x t)
                         (let ((x (list "aa")))
                           (push! "b" x t)
                           (push! "b" x t t)))))))

(ert-deftest %fns:assoc** ()
  (should (equal '(a "a") (assoc** 'a '((b "b") (a "a")))))
  (should (equal '("a" a) (assoc** "a" '(("b" b) ("a" a)) #'string=))))

(ert-deftest %fns:mapcar** ()
  (should (equal '(a b c) (mapcar** #'identity '(a b c))))
  (should (equal '((a 1) (b 2) (c 3))
                 (mapcar** #'list '(a b c) '(1 2 3)))))

(ert-deftest %fns:remove-if* ()
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

(ert-deftest %fns:member-if* ()
  (should-not (member-if* nil nil))
  (should-not (member-if* (lambda (x) (eq x 'a)) nil))
  (should (equal '(a) (member-if* (lambda (x) (eq x 'a)) '(b a))))
  (should (equal '("a") (member-if* (lambda (x) (string= x "a"))
                                    '("b" "a"))))
  (should (equal '((3 "c")) (member-if* (lambda (x) (string= x "c"))
                                         '((1 "a") (2 "b") (3 "c"))
                                         :key #'cadr))))

(ert-deftest %fns:every* ()
  (should (every* #'stringp "" "a" "b"))
  (should (every* #'< '(1 2 3) '(2 3 4)))
  (should-not (every* #'< '(1 2 3) '(2 3 3))))

(ert-deftest %fns:some* ()
  (should (some* #'characterp "abc"))
  (should (some* #'< '(1 2 3) '(1 2 4)))
  (should-not (some* #'< '(1 2 3) '(1 2 3))))

(ert-deftest %fns:loop* ()
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

(ert-deftest %fns:fluid-let ()
  (let ((x 123))
    (fluid-let (x 456)
      (should (= x 456)))
    (should (= x 123))))

(ert-deftest %fns:split-string* ()
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

(ert-deftest %fns:string-trim> ()
  (should-not (string-trim> nil "X"))
  (should (string= "abc" (string-trim> "abc \n  ")))
  (should (string= "abc" (string-trim> "abcXX" "XX")))
  (should (string= "abc" (string-trim> "abcXX" "X+"))))

(ert-deftest %basic:string-trim< ()
  (should-not (string-trim< nil "X"))
  (should (string= "abc" (string-trim< "  \n abc")))
  (should (string= "abc" (string-trim< "XXabc" "XX")))
  (should (string= "abc" (string-trim< "XXabc" "X+"))))

(ert-deftest %fns:string-trim>< ()
  (should-not (string-trim>< nil "X" "Z"))
  (should (string= "abc" (string-trim>< " \n abc \n ")))
  (should (string= "abc" (string-trim>< "ZZabcXX" "X+" "Z+"))))

(ert-deftest %fns:match-string* ()
  (should-not (match-string* nil nil 0))
  (should-not (match-string* nil 123 0))
  (should (string= "XXabcXX"
                   (match-string* "XX\\(abc\\)XX" "XXabcXX" 0)))
  (should-not (match-string* "XX\\(abc\\)XX" "XXabcXX" 2))
  (should (string= "abc"
                   (match-string* "XX\\(abc\\)XX" "XXabcXX" 1))))

(ert-deftest %fns:executable-find% ()
  (if-platform% 'windows-nt
      (should (executable-find% "dir"))
    (should (executable-find% "ls"))
    (should (executable-find% (concat "l" "s")))
    (should (executable-find% "ls"
                              (lambda (ls)
                                (let ((x (shell-command* "sh"
                                           "--version")))
                                  (car x)))))))

 ;; end of fns


;;;;
;; basic
;;;;

(ert-deftest %basic:buffer-file-name* ()
  (should (null (buffer-file-name* (get-buffer "*scratch*")))))

(ert-deftest %basic:file-in-dirs-p ()
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

(ert-deftest %basic:file-name-nondirectory% ()
  (should (string= "c.c" (file-name-nondirectory% "/a/b/c.c")))
  (should (string= "c.c" (file-name-nondirectory%
                          (concat "/a/b/" "c.c")))))

(ert-deftest %basic:save-sexp-to-file ()
  (let ((f (emacs-home* "private/sexp"))
        (s1 '(defvar test%basic-sstf1 t))
        (t1 (make-hash-table :test 'string-hash=)))
    (should (and (save-sexp-to-file s1 f)
                 (file-exists-p f)
                 (equal s1 (car (read-from-string
                                 (read-str-from-file f))))))
    (puthash "a" 1 t1)
    (puthash "b" 2 t1)
    (should (= 2 (hash-table-count t1)))
    (should (and (save-sexp-to-file t1 f)
                 (file-exists-p f)
                 (= 2 (gethash "b" (car (read-from-string
                                         (read-str-from-file f)))))))
    (should (or (delete-file f)
                (not (file-exists-p f))))))

(ert-deftest %basic:save/read-str-to/from-file ()
  (let ((f (emacs-home* "private/str")))
    (should (and (save-str-to-file "abc" f)
                 (string= "abc" (read-str-from-file f))))
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

(ert-deftest %basic:path+ ()
  (should-not (path+ nil))
  (should (string= "a/" (path+ "a")))
  (should (string= "a/b/c/" (path+ "a/" "b/" "c/")))
  (should (string= "a/b/c/" (path+ "a/" "b" "c"))))

(ert-deftest %basic:path- ()
  (should-not (path- nil))
  (should (string= "a/b/" (path- "a/b/c")))
  (should (string= "a/b/" (path- "a/b/c/"))))

(ert-deftest %basic:path-depth ()
  (should (= 0 (path-depth nil)))
  (should (= 0 (path-depth "")))
  (should (= 1 (path-depth "/")))
  (should (= 1 (path-depth "abc")))
  (should (= 2 (path-depth "/abc")))
  (should (= 2 (path-depth "\\a" "\\\\")))
  (should (= 3 (path-depth "/a/b")))
  (should (= 4 (path-depth "/a/b/c"))))

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
                   (string-match "config" d))
                 (lambda (f)
                   (setq matched (cons f matched)))
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
                   (when (member-if* (lambda (z) (string= z x))
                                     std)
                     (setq count (1+ count))))))))

(ert-deftest %basic:platform-arch ()
  (should (platform-arch))
  (should (consp (platform-arch))))

(ert-deftest %basic:emacs-arch ()
  (should (> (emacs-arch) 0)))

(ert-deftest %basic:buffer-major-mode ()
  (should (eq 'fundamental-mode (buffer-major-mode))))


(ert-deftest %basic:posix-path ()
  (should-not (posix-path nil))
  (let ((p "c:/a/b/c.c"))
    (should (string= p (posix-path "c:/a/b/c.c")))
    (should (string= p (posix-path "c:\\a\\b\\c.c")))))

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

(ert-deftest %basic:region-active-if ()
  ;; interactive
  (unless noninteractive
    (with-temp-buffer
      (insert "a bb")
      (should (= 12 (region-active-if (+ 1 2) (* 3 4))))
      (set-mark (point-min))
      (set-mark (point-max))
      (should (= 3 (region-active-if (+ 1 2) (* 3 4)))))))



(ert-deftest %basic:region-active-unless ()
  ;; interactive
  (unless noninteractive
    (with-temp-buffer
      (insert "a bb")
      (should (= 12 (region-active-unless (* 3 4))))
      (set-mark (point-min))
      (set-mark (point-max))
      (should-not (region-active-unless (+ 1 2) (* 3 4))))))


(ert-deftest %basic:symbol@ ()
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

;;;;
;; shells
;;;;

(ert-deftest %shells:shells-spec->% ()
  (should (= 6 (length (shells-spec->%))))
  (should (string= "shell-env"
                   (file-name-base* (shells-spec->% :file)))))

(ert-deftest %shells:shell-env->/<- ()
  (should (*default-shell-env* :put! :xx "aa"))
  (should (string= "aa" (*default-shell-env* :get :xx))))

(ert-deftest %shells:var->paths ()
  (should (null (var->paths 1)))
  (should (var->paths (getenv "PATH"))))

(ert-deftest %shells:paths->var ()
  (let ((path-separator ":"))
    (should (string= "a:b:c" (paths->var '("a" "b" "c"))))
    (should (string= "b" (paths->var '("a" "b" "c")
                                     (lambda (x) (string= "b" x)))))
    (should (string= "" (paths->var '("a" "b" "c")
                                    (lambda (x) (file-exists-p x)))))))

 ;; end of shells


(comment
 (ert-deftest %module:install/delete-package!1 ()
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

;;;;
;; cc*
;;;;

(when-var% +cc*-compiler-bin+ nil
  (ert-deftest %cc*:+cc*-compiler-bin+ ()
    (should (or +cc*-compiler-bin+ t))))

(when-fn%
    'cc*-check-include nil
  (ert-deftest %cc*:cc*-check-include ()
    (should (or (cc*-check-include) t))))

(when-fn%
    'cc*-system-include nil
  (ert-deftest %cc*:cc*-system-include ()
    (message "# v-home!=%s" (v-home! ".exec/"))
    (message "# v-home*=%s" (v-home* ".exec/"))
    (message "# file=%s" (v-home! "exec/cc-inc-native.el"))
    (message "# file=%s" (v-home* "exec/cc-inc-native.el"))
    (should (or (cc*-system-include) t))))


;;;;
;; trans
;;;;

(when-fn%
    'roman->arabic nil
  (ert-deftest %trans:roman->arabic ()
    (should (= 1990 (roman->arabic (split-string* "MCMXC" "" t) 0)))
    (should (= 2008 (roman->arabic (split-string* "MMVIII" "" t) 0)))
    (should (= 1666 (roman->arabic (split-string* "MDCLXVI" "" t) 0)))))

(when-fn%
    'chinese->arabic nil
  (ert-deftest %trans:chinese->arabic ()
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



;; eof
