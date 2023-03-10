;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; vlang.el
;;;;
;;; https://vlang.io
;;;
;;; fetures:
;;; 1. syntax highlight.
;;; 2. format.
;;; 3*. REPL.
;;;
;;; bugs:
;;;
;;;;;


;; (require 'comint)
;; (require 'thingatpt)



;; variable declarations


(defgroup vlang nil
  "Run a vlang process in a buffer."
  :group 'vlang)


(defcustom% vlang-program
  (cond ((executable-find%
          "v"
          (lambda (vlang)
            (let ((x (shell-command* "v" "version")))
              (zerop (car x)))))
         "v")
        (t "v"))
  "Program invoked by the `v' command."
  :type 'string
  :group 'vlang)


(defalias '*vlang*
  (lexical-let% ((b))
    (lambda (&optional n)
      (cond (n (setq b (get-buffer-create n)))
            ((or (null b) (not (buffer-live-p b)))
             (setq b (get-buffer-create "*vlang*")))
            (t b))))
  "The current *vlang* process buffer.")


(defconst +vlang-mode-syntax-table+
  (let ((table (make-syntax-table)))
    ;; operators
    (dolist* (i '(?+ ?- ?* ?/ ?% ?& ?| ?= ?! ?< ?>))
      (modify-syntax-entry i "." table))

    ;; / punctuation
    (modify-syntax-entry ?/ ". 124" table)

    ;; /* */ comments
    (modify-syntax-entry ?* ". 23bn" table)

    ;; \n as comment ender
    (modify-syntax-entry ?\n ">" table)

    ;; string
    (modify-syntax-entry ?\` "\"" table)
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?\" "\"" table)

    ;; _ as word
    (modify-syntax-entry ?_ "w" table) table))


(defun vlang-font-lock-defaults ()
  "Set font lock defaults for the current buffer."
  `((,(regexp-opt '("string" "bool"
                    "i8" "i16" "int" "i64" "i128"
                    "byte" "u16" "u32" "u64" "u128"
                    "rune"
                    "f32" "f64"
                    "byteptr" "voidptr" "charptr" "size_t"
                    "any" "any_int" "any_float"
                    "it")
                  'words)
     . font-lock-builtin-face)

    (,(regexp-opt '("import"
                    "break" "continue" "return" "goto"
                    "defer" "panic" "error"
                    "in" "is" "or"
                    "go" "inline" "live"
                    "as" "assert"  "unsafe" "mut"
                    "__global" "C")
                  'words)
     . font-lock-warning-face)

    ("[@#$][A-Za-z_]*[A-Z-a-z0-9_]*"
     . font-lock-warning-face)

    (,(regexp-opt '("type" "interface" "struct" "enum" "fn")
                  'words)
     . font-lock-keyword-face)

    (,(regexp-opt '("module" "pub" "const")
                  'words)
     . font-lock-preprocessor-face)

    ("\\(->\\|=>\\|\\.>\\|:>\\|:=\\|\\.\\.\\||\\)"
     1 font-lock-keyword-face)

    ("\\($?[.,;]+\\)"
     1 font-lock-comment-delimiter-face)

    ("\\($?[+-/*//%~=<>]+\\)$?,?"
     1 font-lock-negation-char-face)

    ("\\($?[?^!&]+\\)"
     1 font-lock-warning-face)

    ("[^+-/*//%~^!=<>]\\([=:]\\)[^+-/*//%~^!=<>]"
     1 font-lock-comment-delimiter-face)

    ("\\(\\[\\|\\]\\|[(){}]\\)"
     1 font-lock-comment-delimiter-face)

    ("[ \t/+-/*//=><([{,;&|%]\\([0-9][A-Za-z0-9_]*\\)"
     1 font-lock-constant-face)

    (,(regexp-opt '()
                  'words)
     . font-lock-builtin-face)

    ("\\(?:fn\\)\s+\\($?[a-z_][A-Za-z0-9_]*\\)"
     1 font-lock-function-name-face)

    ("\\([A-Z][A-Za-z0-9_]*\\)"
     1 font-lock-type-face)

    (,(regexp-opt '("false" "true" "none")
                  'words)
     . font-lock-constant-face)

    ("\\([a-z_]$?[a-z0-9_]?+\\)$?[ \t]?(+"
     1 font-lock-function-name-face)

    ("\\(?:(\\|,\\)\\([a-z_][a-z0-9_']*\\)\\([^ \t\r\n,:)]*\\)"
     1 font-lock-variable-name-face)

    ("\\(?:(\\|,\\)[ \t]+\\([a-z_][a-z0-9_']*\\)\\([^ \t\r\n,:)]*\\)"
     1 font-lock-variable-name-face)

    ("[.]$?[ \t]?\\($?_[1-9]$?[0-9]?*\\)"
     1 font-lock-variable-name-face)

    (,(regexp-opt '("if" "else" "for" "match")
                  'words)
     . font-lock-keyword-face)

    ("\\('[\\].'\\)"
     1 font-lock-constant-face)

    ("\\($?_?[a-z]+[a-z_0-9]*\\)"
     1 font-lock-variable-name-face)))


(defun vlang-format-buffer ()
  "Format the current buffer."
  (interactive)
  (when (eq major-mode 'vlang-mode)
    (shell-command (concat  vlang-program
                            " fmt -w " (buffer-file-name)))
    (revert-buffer :ignore-auto :noconfirm t)))


(defvar vlang-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "\C-c\C-f" #'vlang-format-buffer)
    m))


(define-derived-mode vlang-mode prog-mode
  "V"
  "Major mode for editing `vlang' files."
  :syntax-table +vlang-mode-syntax-table+
  :keymap vlang-mode-map

  (setq-local require-final-newline mode-require-final-newline)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local comment-start "/*")
  (setq-local comment-end "*/")
  (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
  (setq-local font-lock-defaults `(,(vlang-font-lock-defaults)))
  (font-lock-flush))



(provide 'vlang)


;; end of vlang.el
