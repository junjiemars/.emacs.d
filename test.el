;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; .test.el
;;;;


;;
;; Running Test Interactively:
;; M-x eval-buffer
;; M-x ert RET t


(require 'ert)

(setq ert-batch-backtrace-right-margin 2048)

;;;
;; env
;;;

(ert-deftest %a:env ()
  (should (message "# (nore-emacs) = %s" (nore-emacs)))
  (should (message "# EMACS_HOME = %s" (getenv "EMACS_HOME")))
  (should (message "# ~/.emacs.d = %s" (expand-file-name "~/.emacs.d")))
  (should (message "# system-type = %s" system-type))
  (should (message "# platform-arch = %s" (platform-arch)))
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

(ert-deftest %b:init:emacs-home ()
  (should (file-exists-p (emacs-home)))
  (should (file-exists-p (emacs-home "config/")))
  (should (file-exists-p (emacs-home "private/"))))

(ert-deftest %b:init:file-name-sans-extension* ()
  (should-not (file-name-sans-extension* ""))
  (should (string-equal "a" (file-name-sans-extension* "a.b")))
  (should (string-equal "/a/b/c" (file-name-sans-extension* "/a/b/c.d"))))

(ert-deftest %b:init:mkdir* ()
  (let ((p (concat temporary-file-directory (make-temp-name "mkdir*") "/")))
    (should (null (file-exists-p p)))
    (should (mkdir* p))
    (should (file-exists-p p))))

(ert-deftest %b:init:v-path ()
  (should (string-match "[gt]_[.0-9]+" (v-path "a/x.el")))
  (should (string-match "[gt]_[.0-9]+/" (v-path "a/b/c/"))))

(ert-deftest %b:init:v-home ()
  (should (directory-name-p (v-home)))
  (should (string-match "[gt]_[.0-9]+" (v-home)))
  (should (string-match "xxx/[gt]_[.0-9]+/yyy" (v-home "xxx/yyy"))))

(ert-deftest %b:init:progn% ()
  (should-not (progn%))
  (should (= 3 (progn% (+ 1 2))))
  (should (eq 'progn (car (macroexpand '(progn% (+ 1 2) (* 3 4)))))))

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
  (should (eq (if-version% <= 22.1 t) (if-version% > 22.1 nil t)))
  (should (= 12 (if-version% > 22.1 (+ 1 2) (* 3 4))))
  (should (equal '(progn (* 3 4) (* 5 6))
                 (macroexpand '(if-version%
                                   < 1000
                                   (+ 1 2)
                                 (* 3 4)
                                 (* 5 6))))))

(ert-deftest %b:init:when-version% ()
  (should (eq (when-version% <= 22.1 t) (null (when-version% > 22.1 t))))
  (should (= 12 (when-version% <= 22.1 (+ 1 2) (* 3 4)))))

(ert-deftest %b:init:if/when-native-comp% ()
  (should (eq (if-native-comp% nil t) (null (when-native-comp% t)))))

(ert-deftest %b:init:comp-file-extension% ()
  (should (or (string-equal ".eln" (comp-file-extension%))
              (string-equal ".elc" (comp-file-extension%)))))

(ert-deftest %b:init:make-v-comp-file ()
  (let ((fs (make-v-comp-file (make-temp-file "nore-mvcf-"))))
    (should (and (consp fs)
                 (file-exists-p (car fs))
                 (null (file-exists-p (cdr fs)))))))

(ert-deftest %b:init:time ()
  (should (null (time "")))
  (should (= 6 (time "a" (+ 1 2 3))))
  (should (= 120 (time "a" (+ 1 2 3) (* 4 5 6)))))

;; end of init

;;;
;; vcomp
;;;

(ert-deftest %c:vcomp:emacs-home* ()
  (should (string= (emacs-home) (emacs-home*))))

(ert-deftest %c:vcomp:path! ()
  (let ((p (concat temporary-file-directory (make-temp-name "path!") "/")))
    (should (null (file-exists-p p)))
    (should (path! p))
    (should (file-exists-p p))))

(ert-deftest %c:vcomp:make-v-home* ()
  (should (string-match "[gt]_[.0-9]+" (make-v-home* "xxx"))))

(ert-deftest %c:vcomp:v-home* ()
  (should (string= (v-home) (v-home*)))
  (should (string= (v-home "xxx/yyy") (v-home* "xxx/yyy"))))

(ert-deftest %c:vcomp:gensym* ()
  (should (string-match "^n[0-9]+" (format "%s" (gensym*))))
  (should (string-match "^X[0-9]+" (format "%s" (gensym* "X")))))

;; end of vcomp

;;;
;; `fn'
;;;

(ert-deftest %d:fn:if/when/unless-feature% ()
  (unintern "has-ft-ert%")
  (unintern "non-ft-ertxxx%")
  (should (and (if-feature% ert t)
               (when-feature% ert t)
               (null (unless-feature% t))))
  (should (and (if-feature% ertxxx nil t)
               (unless-feature% ertxxx t)
               (null (when-feature% ertxxx nil)))))

(ert-deftest %d:fn:if/when/unless-fn% ()
  (should (and (if-fn% should ert t)
               (when-fn% should ert t)
               (null (unless-fn% should ert t))))
  (should (and (if-fn% shouldXXX nil nil t)
               (unless-fn% shouldXXX nil t)
               (null (when-fn% shouldXXX t)))))

(ert-deftest %d:fn:if/when/unless-var% ()
  (should (and (if-var% ert-batch-backtrace-right-margin ert t)
               (when-var% ert-batch-backtrace-right-margin ert t)
               (not (unless-var% ert-batch-backtrace-right-margin ert t))
               (unless-var% ert-batch-xxx ert t))))

(ert-deftest %d:fn:setq% ()
  (should-not (setq% zzz "zzz"))
  (should-not (setq% zzz "xxx" xxx)))

(ert-deftest %d:fn:v-home% ()
  (should (directory-name-p (v-home%)))
  (should (string-match "[gt]_[.0-9]+" (v-home%)))
  (should (string-match "[gt]_[.0-9]+.*x\\.el\\'" (v-home% "x.el"))))

(ert-deftest %d:fn:v-home%> ()
  (should (string-match "\\.el[cn]$" (file-name-nondirectory (v-home%> nil))))
  (should (string-match "[gt]_[.0-9]+.*x\\.el[cn]?$" (v-home%> "x"))))

(ert-deftest %d:fn:emacs-arch ()
  (should (= (logand (emacs-arch) 2) 0)))

(ert-deftest %d:fn:platform-arch ()
  (should (platform-arch)))

(ert-deftest %d:fn:if/when/unless-lexical% ()
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

(ert-deftest %d:fn:if/when/unless-graphic% ()
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

(ert-deftest %d:fn:if/when/unless-platform% ()
  (cond ((if-platform% darwin t)
         (should (and (when-platform% darwin t)
                      (not (unless-platform% darwin t)))))
        ((if-platform% gnu/linux t)
         (should (and (when-platform% gnu/linux t)
                      (not (unless-platform% gnu/linux t)))))
        ((if-platform% windows-nt t)
         (should (and (when-platform% windows-nt t)
                      (not (unless-platform% windows-nt t)))))
        ((if-platform% 'cygwin t)
         (should (and (when-platform% cygwin t)
                      (not (unless-platform% cygwin)))))))

(ert-deftest %d:fn:if/when/unless-window% ()
	(cond ((if-window% mac t)
         (should (and (when-window% mac t)
                      (null (unless-window% mac t)))))
				((if-window% ns t)
         (should (and (when-window% ns t)
                      (null (unless-window% ns)))))
        ((if-window% pgtk t)
         (should (and (when-window% pgtk t)
                      (null (unless-window% pgtk t)))))
        ((if-window% w32 t)
         (should (and (when-window% w32 t)
                      (null (unless-window% w32 t)))))))

(ert-deftest %d:fn:if/when/unless-interactive% ()
  (should (eq (if-interactive% t) (when-interactive% t)))
  (should (eq (when-interactive% t) (not (unless-interactive% t))))
  (should (eq (not (when-interactive% t)) (unless-interactive% t))))

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
  (should
   (= 6 (let ((a 1))
          (make-thread*
           (lambda ()
             (thread-yield*)
             (*
              ;; cannot capture the free `a'
              (unwind-protect a 1)
              2 3))
           t)))))

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
        (t1 (make-hash-table :test 'nore-emacs-string-hash=)))
    (should (and (save-sexp-to-file s1 f1)
                 (equal s1 (read-sexp-from-file f1))))
    (should (eq 'nore-emacs-string-hash= (hash-table-test t1)))
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
  (should (let ((x (shell-command* "wc" "-l" (emacs-home* "test.el"))))
            (or (= (car x) 0)
                (= (car x) 127)))))

(ert-deftest %d:fn:executable-find* ()
  (if (eq system-type 'windows-nt)
      (should (executable-find* "dir"))
    (should (executable-find* "ls"))
    (should (executable-find* (concat "l" "s")))
    (should (executable-find* "ls" (lambda (ls) ls)))))

(ert-deftest %d:fn:file-in-dirs-p ()
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

(ert-deftest %d:fn:path+ ()
  (should-not (path+ nil))
  (should (string= "a/" (path+ "a")))
  (should (string= "a/b/c/" (path+ "a/" "b/" "c/")))
  (should (string= "a/b/c/" (path+ "a/" "b" "c"))))

(ert-deftest %d:fn:path- ()
  (should-not (path- nil))
  (should (string= "a/b/" (path- "a/b/c")))
  (should (string= "a/b/" (path- "a/b/c/"))))

(ert-deftest %d:fn:path-depth ()
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

(ert-deftest %d:fn:kbd% ()
  (should (equal (kbd% "<tab> <ret>") (kbd "<tab> <ret>")))
  (should (string= (kbd% "C-c f i") (kbd "C-c f i"))))

(ert-deftest %d:fn:if-key% ()
  (let ((b (if-key% (current-global-map) (kbd "C-c C-c C-c") #'+ 1 0)))
    (cond ((= b 0)
           (should
            (eq #'+ (define-key% (current-global-map) "" #'+))))
          ((= b 1)
           (should
            (null (define-key% (current-global-map) "" nil)))))))

(ert-deftest %d:fn:if-region-active ()
  ;; interactive
  (unless noninteractive
    (with-temp-buffer
      (insert "a bb")
      (should (= 12 (if-region-active (+ 1 2) (* 3 4))))
      (set-mark (point-min))
      (set-mark (point-max))
      (should (= 3 (if-region-active (+ 1 2) (* 3 4)))))))

(ert-deftest %d:fn:unless-region-active ()
  ;; interactive
  (unless noninteractive
    (with-temp-buffer
      (insert "a bb")
      (should (= 12 (unless-region-active (* 3 4))))
      (set-mark (point-min))
      (set-mark (point-max))
      (should-not (unless-region-active (+ 1 2) (* 3 4))))))

;; (ert-deftest %d:fn:symbol@ ()
;;   ;; interactive
;;   (unless noninteractive
;;     (with-temp-buffer
;;       (insert "a bb")
;;       (goto-char (point-min))
;;       (should (null (car (symbol@))))
;;       (set-mark (point-min))
;;       (set-mark (point-max))
;;       (should (string= "a bb" (cdr (symbol@)))))))

;; end of `fn'

;;;
;; `boot'
;;;

(ert-deftest %e:boot:self-spec->/<- ()
  (should (null (self-spec-> nil nil)))
  (should (null (self-spec-> '(a 1) nil)))
  (should (= 1 (self-spec-> '(a 1) 'a)))
  (should (= 1 (self-spec-> '(a (b (c 1))) 'a 'b 'c)))
  (should (= 1 (self-spec-> (self-spec<- 'a 1 nil) 'a)))
  (should (= 1 (self-spec-> (self-spec<- 'c 1 '(a (b (c 3))) 'a 'b) 'c))))

(ert-deftest %e:boot:*self-paths* ()
  (should (*self-paths*)))

;; end of `boot'


;;;
;; shells
;;;

(ert-deftest %f:shells:shell-env->/<- ()
  (should (*default-shell-env* :put! :xx "aa"))
  (should (string= "aa" (*default-shell-env* :get :xx))))

(ert-deftest %f:shells:echo-var ()
  (should (string= "1" (let ((process-environment
                              (cons "A=1" process-environment)))
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
;; `ed'
;;;

(ert-deftest %h:ed:version-strncmp ()
  (should (= 0 (version-strncmp "" "")))
  (should (= 1 (version-strncmp "1" "")))
  (should (= -1 (version-strncmp "" "1")))
  (should (= 0 (version-strncmp "1" "1")))
  (should (= 0 (version-strncmp "1.0" "1")))
  (should (= 1 (version-strncmp "1.21" "1.2.12" 2)))
  (should (= 0 (version-strncmp "1.2.3.1" "1.2.3.2" 3)))
  (should (= -1 (version-strncmp "1.2.3.1" "1.2.3.2" 4))))

;; end of `ed'

;;;
;; `cls'
;;;

(ert-deftest %j:cls:assoc** ()
  (should (equal '(a 1) (assoc** 'a '((b 2) (a 1)))))
  (should (equal '(a 1) (assoc** 'a '((b 2) (a 1)) :test #'eq)))
  (let ((lst '((b 2) (a 1))))
    (should (equal (assoc** 'a lst :test #'eq) (assq 'a lst))))
  (should (equal '("a" a) (assoc** "a" '(("b" b) ("a" a)) :test #'string=)))
  (let ((k "a") (lst '(("b" b) ("a" a))))
    (should (equal (assoc** k lst :test #'string=) (assoc-string k lst)))))

(ert-deftest %j:cls:mapcar** ()
  (should (equal '(a b c) (mapcar** #'identity '(a b c))))
  (should (equal '((a 1) (b 2) (c 3))
                 (mapcar** #'list '(a b c) '(1 2 3)))))

(ert-deftest %j:cls:remove-if* ()
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

(ert-deftest %j:cls:member-if* ()
  (should-not (member-if* nil nil))
  (should-not (member-if* (lambda (x) (eq x 'a)) nil))
  (should (equal '(a) (member-if* (lambda (x) (eq x 'a)) '(b a))))
  (should (equal '("a") (member-if* (lambda (x) (string= x "a"))
                                    '("b" "a"))))
  (should (equal '((3 "c")) (member-if* (lambda (x) (string= x "c"))
                                        '((1 "a") (2 "b") (3 "c"))
                                        :key #'cadr))))

(ert-deftest %j:cls:every* ()
  (should (every* #'stringp "" "a" "b"))
  (should (every* #'< '(1 2 3) '(2 3 4)))
  (should-not (every* #'< '(1 2 3) '(2 3 3))))

(ert-deftest %j:cls:some* ()
  (should (some* #'characterp "abc"))
  (should (some* #'< '(1 2 3) '(1 2 4)))
  (should-not (some* #'< '(1 2 3) '(1 2 3))))

(ert-deftest %j:cls:loop* ()
  (should (equal '(1 2 3) (loop* for i from 1 to 3 collect i)))
  (should (equal '(2 4 6) (loop* for i from 1 to 3 collect (* 2 i))))
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
                              return d)))
  (should (equal [0 1 4 9] (let ((v (make-vector 4 0)))
                             (loop* for i from 1 upto 3
                                    do (aset v i (* i i))
                                    finally return v)))))

;; end of `cls'

;;;
;; conditional: `cc'
;;;

(ert-deftest %q:cc:cc*-cc ()
  (should (message "# cc-env.bat = %s" (or (executable-find "cc-env.bat") "")))
  (should (message "# cl = %s" (or (executable-find "cl") "")))
  (should (message "# gcc = %s" (or (executable-find "gcc") "")))
  (should (message "# clang = %s" (or (executable-find "clang") "")))
  (should (message "# make = %s" (or (executable-find "make") "")))
  (when (fboundp 'cc*-cc)
    (should (message "# (cc*-cc) = %s" (or (cc*-cc) "")))))

(ert-deftest %q:cc:cc*-system-include ()
  (when (fboundp 'cc*-system-include)
    (should (message "# (cc*-system-include) = %s"
                     (or (cc*-system-include :read) "")))))

;; end of `cc'

;;;
;; conditional: `tags'
;;;

(ert-deftest %r:tags:dir-iterate ()
  (when (fboundp 'dir-iterate)
    (should (string= "init.el"
                     (catch 'br
                       (dir-iterate (emacs-home*)
                                    (lambda (f &rest _)
                                      (when (string= "init.el" f)
                                        (throw 'br f)))))))
    (should (string-match "init\\.el$"
                          (catch 'br
                            (dir-iterate (emacs-home*)
                                         (lambda (f &rest _)
                                           (string= "init.el" f))
                                         nil
                                         (lambda (a _)
                                           (throw 'br a))))))
    (should (string= "config"
                     (catch 'br
                       (dir-iterate (emacs-home*)
                                    nil
                                    (lambda (f &rest _)
                                      (when (string= "config" f)
                                        (throw 'br f)))))))
    (should (string-match "config$"
                          (catch 'br
                            (dir-iterate (emacs-home*)
                                         nil
                                         (lambda (f &rest _)
                                           (string= "config" f))
                                         nil
                                         (lambda (a &rest _)
                                           (throw 'br a))))))
    (should (string= "xy"
                     (catch 'br
                       (dir-iterate (emacs-home*)
                                    (lambda (f &rest _)
                                      (string= "test.el" f))
                                    (lambda (d &rest _)
                                      (string= ".emacs.d" d))
                                    (lambda (a env)
                                      (let ((k1 (plist-get env :k1))
                                            (k2 (plist-get env :k2)))
                                        (throw 'br (format "%s%s" k1 k2))))
                                    nil
                                    (list :k1 "x" :k2 "y")))))
    (should (= 1 (let ((fcnt 0) (dcnt 0))
                   (catch 'br
                     (dir-iterate
                      (emacs-home*)
                      (lambda (f &rest _)
                        (string= "tags.el" f))
                      (lambda (d a &rest _)
                        (unless (cond ((string= ".git" d) t)
                                      ((string-match "^[gt]_[0-9]+" d) t)
                                      (t nil))
                          (string-match "config" a)))
                      (lambda (f &rest _)
                        (throw 'br (setq fcnt (1+ fcnt)))))))))))



;;;
;; conditional: `trans'
;;;


(ert-deftest %s:trans:encode-url ()
  (when (fboundp 'encode-url)
    (let ((ss "https://a/b/ ?c=中abc"))
      (let* ((s1 (with-temp-buffer (insert ss) (encode-url)))
             (d1 (with-temp-buffer (insert s1) (decode-url))))
        (should (string= ss d1))))))

(ert-deftest %s:trans:encode-base64 ()
  (when (fboundp 'encode-base64)
    (let ((ss "https://a/b/ ?c=中abc"))
      (let* ((s1 (with-temp-buffer (insert ss) (encode-base64)))
             (d1 (with-temp-buffer (insert s1) (decode-base64))))
        (should (string= ss d1))))))

(ert-deftest %s:trans:encode-ipv4 ()
  (when (fboundp 'encode-ipv4)
    (let ((ss "192.168.2.1"))
      (let* ((s1 (with-temp-buffer (insert ss) (encode-ipv4)))
             (d1 (with-temp-buffer (insert (number-to-string s1))
                                   (decode-ipv4))))
        (should (string= ss d1))))))

(ert-deftest %s:trans:roman->arabic ()
  (when (fboundp 'roman->arabic)
    (should (= 1990 (roman->arabic "MCMXC")))
    (should (= 2008 (roman->arabic "MMVIII")))
    (should (= 1666 (roman->arabic "MDCLXIVII")))
    (should (= 2025 (roman->arabic "MMXXV")))))

(ert-deftest %s:trans:chinese->arabic ()
  (when (fboundp 'chinese->arabic)
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

 ;; end of conditional `trans'

;;;
;; `ssh'
;;;

(ert-deftest %u:ssh:ssh-remote-/p/>ids/>user@host ()
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

;; end of `ssh'

 ;; end of test.el
