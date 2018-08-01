;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; basic.el
;;;;



;; `load-path' versioned dirs
(add-to-list 'load-path (v-home% "config/") nil #'string=)
(add-to-list 'load-path (v-home% "private/") nil #'string=)


;; Versioned Dirs: .*

;; Auto-save
(setq auto-save-list-file-prefix (v-home! ".save/" "auto-"))

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(v-home! ".backup/"))))

;; eww bookmarks
(version-supported-when
    <= 24.4
  (setq% eww-bookmarks-directory (v-home! ".bookmarks/") eww))

;; Bookmark: file in which to save bookmarks
(setq% bookmark-default-file
       (v-home! ".bookmarks/" "emacs.bmk") bookmark)

;; Eshell
(setq% eshell-directory-name (v-home! ".eshell/") eshell)

;; Games: a directory for game scores
(setq% gamegrid-user-score-file-directory (v-home! ".games/") gamegrid)

;; Ido saved state between invocations
(setq% ido-save-directory-list-file (v-home! ".ido/" "ido.last") ido)

;; Image dired: where thumbnail images are stored.
(setq% image-dired-dir (v-home! ".dired/image/") image-dired)

;; Savehist: save minibuffer history
(setq% savehist-file (v-home! ".minibuffer/" "history") savehist)

;; Recentf: save the recent list into
(setq% recentf-save-file (v-home! ".recentf/" "recentf") recentf)

;; Rmail
(setq rmail-file-name (v-home! ".mail/" "RMAIL"))

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(setq% save-place-file (v-home! ".places/" "places") saveplace)

;; Server
(setq% server-auth-dir (v-home! ".server/") server)

;; Semantic
(version-supported-when <= 23
  (setq% semanticdb-default-save-directory
	 (v-home! ".semantic/db/") semantic/db-file))

;; Tramp
(version-supported-when <= 23
  (version-supported-when > 24
    (eval-when-compile (require 'tramp)))
  (setq% tramp-persistency-file-name
	 (v-home! ".tramp/" "tramp") tramp-cache))

;; Url
(setq% url-configuration-directory (v-home! ".url/") url)


 ;; Versioned Dirs


;; Platform Related Functions

(platform-supported-when windows-nt
  
  (defmacro windows-nt-posix-path (path)
    "Return posix path from Windows PATH which can be recognized on`system-type'."
    `(when ,path
			 (let ((p (replace-regexp-in-string "\\\\" "/" ,path)))
				 (if (string-match "^\\([A-Z]:/\\)" p)
						 (replace-match (downcase (match-string 1 p)) t t p)
					 p)))))


(platform-supported-when windows-nt
	
  (defmacro windows-nt-unix-path (path)
    "Return unix paths from PATH which can be recognized on `system-type'."
    `(when ,path
			 (replace-regexp-in-string
				";" ":"
				(replace-regexp-in-string "^\\([a-zA-Z]\\):/" "/\\1/"
																	(windows-nt-posix-path ,path))))))


 ;; end of Platform Related Functions


;; Strings

(defsubst string-trim> (s &optional rr)
  "Remove whitespaces or the matching of RR at the end of S."
  (let ((r (if rr (concat rr "\\'") "[ \t\n\r]+\\'" )))
    (if (string-match r s)
				(replace-match "" t t s)
      s)))


(defsubst string-trim< (s &optional lr)
  "Remove leading whitespace or the matching of LR from S."
  (let ((r (if lr (concat "\\`" lr) "\\`[ \t\n\r]+")))
    (if (string-match r s)
				(replace-match "" t t s)
      s)))


(defsubst string-trim>< (s &optional rr lr)
  "Remove leading and trailing whitespace or the matching of LR/RR from S."
  (let ((s1 (string-trim> s rr)))
    (string-trim< s1 lr)))


(defsubst match-string* (regexp string num &optional start)
  "Return string of text match for REGEXP in STRING.

Return nil if NUMth pair didn’t match, or there were less than NUM pairs.
NUM specifies which parenthesized expression in the REGEXP.
If START is non-nil, start search at that index in STRING.

See `string-match' and `match-string'."
  (when (and string (string-match regexp string start))
    (substring string (match-beginning num) (match-end num))))


 ;; end of Strings


;; Compatible Functions

(unless-fn% with-eval-after-load nil
  (defmacro with-eval-after-load (file &rest body)
    "Execute BODY after FILE is loaded.

FILE is normally a feature name, but it can also be a file name,
in case that file does not provide any feature.  See ‘eval-after-load’
for more details about the different forms of FILE and their semantics."
    `(eval-after-load ,file
       `(funcall ,(lambda ()
										(progn% ,@body))))))


(version-supported-if
		<= 26.1
		(defalias 'assoc** 'assoc)
	(defmacro  assoc** (key list &optional testfn)
		"Return non-nil if KEY is equal to the car of an element of LIST.

The value is actually the first element of LIST whose car equals KEY.
Equality is defined by TESTFN if non-nil or by `equal' if nil."
		(if-fn% cl-assoc cl-lib
						;; cl-assoc autoload
						`(cl-assoc ,key ,list :test ,testfn)
			(when-fn% assoc* cl
				`(with-no-warnings
					 (require 'cl)
					 (assoc* ,key ,list :test ,testfn))))))


(if-fn% alist-get nil
				(version-supported-if
						<= 26
						(defalias 'alist-get* 'alist-get)
					(defmacro alist-get* (key alist &optional default remove testfn)
						"Return the value associated with KEY in ALIST, using `assq'.
If KEY is not found in ALIST, return DEFAULT.

This is a generalized variable suitable for use with `setf'.
When using it to set a value, optional argument REMOVE non-nil
means to remove KEY from ALIST if the new value is `eql' to DEFAULT.

If KEY is not found in ALIST, returns DEFAULT. 

There're no `alist-get' function with TESTFN argument definition in Emacs26-."
						(ignore* testfn)
						`(alist-get ,key ,alist ,default ,remove)))
	(defmacro alist-get* (key alist &optional default remove testfn)
		"Return the value associated with KEY in ALIST, using `assq'.
If KEY is not found in ALIST, return DEFAULT.

This is a generalized variable suitable for use with `setf'.
When using it to set a value, optional argument REMOVE non-nil
means to remove KEY from ALIST if the new value is `eql' to DEFAULT.

If KEY is not found in ALIST, returns DEFAULT. 

There're no `alist-get' function definition in Emacs25-."
		(ignore* remove testfn) ;;silence byte-compiler.
		`(let ((x (assoc** ,key ,alist ,testfn)))
			 (if (consp x)
					 (cdr x)
				 ,default))))


(version-supported-if
    <= 24.4
    (defalias 'split-string* 'split-string)
  (defun split-string* (string &optional separators omit-nulls trim)
    "Split STRING into substrings bounded by matches for SEPARATORS, 
like `split-string' Emacs 24.4+"
    (if trim
				(delete ""
								(mapcar (lambda (s)
													(if (and (stringp trim) (> (length trim) 0))
															(string-trim>< s trim trim)
														(string-trim>< s)))
												(split-string string separators omit-nulls)))
      (split-string string separators omit-nulls))))


(unless-fn% directory-name-p nil
  (defsubst directory-name-p (name)
    "Returns t if NAME ends with a directory separator character."
    (let ((len (length name))
					(lastc ?.))
      (if (> len 0)
					(setq lastc (aref name (1- len))))
      (or (= lastc ?/)
					(and (memq system-type '(windows-nt ms-dos))
							 (= lastc ?\\))))))



 ;; end of Compatible Functions


;; File Functions

(defun save-sexp-to-file (sexp file)
  "Save SEXP to FILE. 

Returns the name of FILE when successed otherwise nil."
  (when (and (save-excursion
               (let ((sexp-buffer (find-file-noselect file)))
                 (set-buffer sexp-buffer)
                 (erase-buffer)
                 (print sexp sexp-buffer)
                 (save-buffer)
                 (kill-buffer sexp-buffer)))
             (file-exists-p file))
    file))


(defun save-str-to-file (str file)
  "Save STR to FILE. 

Returns the name of FILE when successed otherwise nil."
  (progn
    (with-temp-file file (insert str))
    (when (file-exists-p file)
      file)))


(defun dir-iterate (dir ff df fn dn)
  "Iterating DIR, if FF file-fitler return t then call FN, 
and if DF dir-filter return t then call DN and then iterate into deeper DIR.

   (defun FILE-FILTER (file-name absolute-name))
   (defun DIR-FILTER (dir-name absolute-name))
   (defun FILE-FUNCTION (absolute-name))
   (defun DIR-FUNCTION (absolute-name))

Notes:
Should jump over dummy symbol-links that point to self or parent directory.

Examples:
1. iterate DIR and output every absolute-name to *Message* buffer:
  (dir-iterate DIR (lambda (f a) t) (lambda (d a) t) (message \"%s\" a) nil)

2. iterate DIR with level=1 and output every absolute-name to *Message* buffer:
  (dir-iterate DIR (lambda (f a) t) nil (message \"%s\" a))

3. iterate DIR and output every subdir's absolute-name to *Message* buffer:
  (dir-iterate DIR nil (lambda (d a) t) nil (message \"%s\" a))
"
  (dolist (f (file-name-all-completions "" dir))
    (unless (member f '("./" "../"))
      (let ((a (expand-file-name f dir)))
        (if (directory-name-p f)
            (when (and df (let ((ln (file-symlink-p (directory-file-name a))))
														(if (not ln) t
															(not (or (string= "." ln)
																			 (and (>= (length a) (length ln))
																						(string=
																						 ln
																						 (substring a 0 (length ln))))))))
											 (funcall df f a))
							(and dn (funcall dn a))
							(dir-iterate a ff df fn dn))
          (when (and ff (funcall ff f a))
						(and fn (funcall fn a))))))))


(defmacro shell-command* (command &rest args)
	"Return a cons cell (code . output) after execute COMMAND in inferior shell.

See `shell-command' and `shell-command-to-string'."
	(declare (indent 1))
	`(with-temp-buffer
		 (cons (call-process shell-file-name nil (current-buffer) nil
												 shell-command-switch
												 (mapconcat #'identity
																		(cons ,command (list ,@args)) " "))
					 (buffer-string))))


(defmacro executable-find% (command &optional prefer)
  "Search for COMMAND in `exec-path' and return the absolute file name 
at compile-time when PREFER is nil, same as `executable-find'.

Search for COMMAND in %PATH% or $PATH and return the absolute file name 
at compile-time when PREFER is non nil.

Return nil if no COMMAND found or no PREFER command found.
Return the first matched one, if multiple COMMANDs had been found
and `funcall' PREFER returns t.
"
  (if prefer
      (let ((cmd (shell-command* (platform-supported-if windows-nt
																		 "where"
																	 "command -v")
									 command)))
				(when (zerop (car cmd))
					(let* ((ss (cdr cmd))
								 (path (split-string* ss "\n" t))
								 (p (cond ((and (consp path) (functionp prefer))
													 (catch 'prefer
														 (dolist (x path)
															 (when (funcall prefer
																							(shell-quote-argument
																							 (platform-supported-if windows-nt
																									 (windows-nt-posix-path x)
																								 x)))
																 (throw 'prefer x)))
														 nil))
													((consp path) (car path))
													(t path))))
						`,(when p (platform-supported-if windows-nt
													(windows-nt-posix-path p)
												p)))))
    (let ((path (executable-find command)))
      (ignore* prefer)
      `,path)))


(defsubst env-path+ (path)
	"Add PATH to %PATH% on Windows and $PATH on *UNX."
	(when path
		(let ((p (string-trim> (getenv "PATH") path-separator)))
			(setenv "PATH" (concat p path-separator path)))))


 ;; end of File Functions


;; Clean Emacs' user files

(defun clean-saved-user-files (&optional all)
  "Clean saved user files except current `emacs-version'.

Clean all when ALL is t,
otherwise default to keep the directories of current `emacs-version'."
  (let ((dirs (list `,(emacs-home* ".save/")
                    `,(emacs-home* ".backup/")
                    `,(emacs-home* ".bookmarks/")
                    `,(emacs-home* ".desktop/")
                    `,(emacs-home* ".eshell/")
										`,(emacs-home* ".exec/")
                    `,(emacs-home* ".games/")
                    `,(emacs-home* ".ido/")
                    `,(emacs-home* ".dired/")
                    `,(emacs-home* ".minibuffer/")
                    `,(emacs-home* ".places/")
                    `,(emacs-home* ".recentf/")
                    `,(emacs-home* ".semantic/")
                    `,(emacs-home* ".server/")
                    `,(emacs-home* ".tags/")
                    `,(emacs-home* ".tramp/")
                    `,(emacs-home* ".url/"))))
    (dolist (d dirs)
      (when (file-exists-p d)
        (dolist (f (directory-files d nil "^[gt]_.*$"))
          (when (or all
                    (not (string-match
                          (concat "^[gt]_" emacs-version) f)))
            (message "#Clean saved user file: %s" (concat d f))
            (platform-supported-if windows-nt
                (shell-command (concat "rmdir /Q /S " (concat d f)))
              (shell-command (concat "rm -r " (concat d f))))))))))


(defun reset-emacs ()
  "Clean all compiled file and desktop, then restart Emacs."
  (progn
    (clean-saved-user-files t)
    (clean-compiled-files)
    (setq kill-emacs-hook nil)
    (kill-emacs 0)))






;; Socks

(defmacro feature-socks-supported-p (&rest body)
  "If socks feature supported then do BODY."
  (declare (indent 0))
  `(version-supported-when < 22
     (when-var% url-gateway-method url
								,@body)))

(feature-socks-supported-p

  (defun toggle-socks! (&optional arg)
    "Toggle `url-gatewary-method' to socks or native.
With prefix argument ARG, `url-gatewary-method' via socks if ARG is 
positive, otherwise via native."
    (interactive "P")
    (let ((native `((:url-gateway-method . native)
										(:socks-server . nil)))
					(socks `((:url-gateway-method . socks)
									 (:socks-server
										. 
										,(list "Default server"
													 (self-spec->*env-spec :socks :server)
													 (self-spec->*env-spec :socks :port)
													 (self-spec->*env-spec :socks :version))))))
      (require 'url)
      (if (null arg)
					(if (eq url-gateway-method 'native)
							(setq-default
							 url-gateway-method (alist-get* :url-gateway-method socks)
							 socks-server (alist-get* :socks-server socks))
						(setq-default
						 url-gateway-method (alist-get* :url-gateway-method native)
						 socks-server (alist-get* :socks-server native)))
				(setq-default
				 url-gateway-method (alist-get* :url-gateway-method socks)
				 socks-server (alist-get* :socks-server socks)))
      (message "socks%s as url gateway %s"
							 (or (when (eq url-gateway-method 'socks)
										 (cdr (alist-get* :socks-server socks)))
									 "")
							 (if (eq url-gateway-method 'native)
									 "disabled" "enabled")))))

(feature-socks-supported-p
	(when (self-spec->*env-spec :socks :allowed)
		(toggle-socks! t)))





;; Computations

(defalias 'range #'number-sequence)


;; use `pp' `pp-eval-expression' or `pp-eval-last-sexp'
(if-fn% cl-prettyprint cl
	;;;###autoload
	(defalias 'pprint #'cl-prettyprint)
  (when-fn% cl-prettyexpand cl
		;;;###autoload
    (defalias 'pprint #'cl-prettyexpand)))


(defun take (n seq)
  "Returns a sequence of the first N items in SEQ.

Or all items if SEQ has fewer items than N."
  (let ((acc nil) (n1 n) (s1 seq))
    (while (and (> n1 0) s1)
      (setq acc (cons (car s1) acc)
            n1 (1- n1) s1 (cdr s1)))
    (nreverse acc)))


(defun drop-while (pred seq)
  "Returns a sequence of successive items from SEQ after the item 
for which (PRED item) returns t."
  (let ((s seq) (w nil))
    (while (and (not w) (car s))
      (if (funcall pred (car s))
          (setq w t)
        (setq s (cdr s))))
    (cdr s)))


(defun take-while (pred seq)
  "Returns a sequence of successive items from SEQ before the item 
for which (PRED item) returns t."
  (let ((s seq) (w nil) (s1 nil))
    (while (and (not w) (car s))
      (if (funcall pred (car s))
          (setq w t)
        (setq s1 (cons (car s) s1)
              s (cdr s))))
    (nreverse s1)))


 ;; end of Computations


;; keymap

(defmacro define-key* (keymap key def feature &optional accept-default)
  "`define-key' as DEF if KEY does not existing in the KEYMAP of FEATURE.
"
  `(when-var% ,keymap ,feature
	      (unless (lookup-key ,keymap ,key ,accept-default)
		(define-key ,keymap ,key ,def))))


 ;; end of keymap


(defmacro safe-local-variable* (var)
  "Safe local VAR in -*- line, see `enable-local-variables'"
  `(put ,var 'safe-local-variable
	(lambda (x) (ignore* x) t)))


;; shell scripts
(defun set-sh-mode! ()
  (setq% sh-basic-offset tab-width sh-script)
  ;; obsolete variable as of 26.1
  ;; (setq% sh-indentation tab-width sh-script)
  )


;; linum mode
(def-feature-supported-p linum nil
  "If `linum' feature supported then do BODY, requires Emacs-23.1+")


;; semantic
(def-feature-supported-p semantic)


;; default web browser: eww
(def-feature-supported-p eww nil
  "If `eww' feature supported then do BODY, requires Emacs-24.4+")


;; compilation
(defun colorize-compilation-buffer! ()
  "Colorize *compilation* buffer."
  (when (eq major-mode 'compilation-mode)
    (when-fn% ansi-color-apply-on-region ansi-color
      (let ((buffer-read-only nil))
				(require 'ansi-color)
				(ansi-color-apply-on-region
				 (if-var% compilation-filter-start compile
									compilation-filter-start (point-min))
				 (point-max))))))


;; add versioned "config/" to $PATH
(env-path+ (v-home! ".exec/"))
