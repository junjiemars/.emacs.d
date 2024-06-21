;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-rust.el
;;;;

;;; require

(declare-function make-dir-ctags (v-home%> "config/tags"))

;; end of require

;;; sysroot

(defun rust*-sysroot-spec ()
  "Return rust sysroot spec."
  (let ((rc (shell-command* "~/.cargo/bin/rustc"
              "--print sysroot 2>/dev/null")))
    (when (zerop (car rc))
      (let ((sysroot (path+ (string-trim> (cdr rc)))))
        (list
         :sysroot sysroot
         :hash
         (let ((h (shell-command* (concat sysroot "bin/rustc")
                    "-vV")))
           (when (zerop (car h))
             (string-match* "^commit-hash: \\([a-z0-9]+\\)"
                            (cdr h) 1)))
         :etc (path+ sysroot "lib/rustlib/etc")
         :src (path+ sysroot "lib/rustlib/src")
         :tag
         (let ((ctags (concat (tags-spec->% :root) "ctags.rust")))
           (prog1 ctags
             (unless (file-exists-p ctags)
               (save-str-to-file
                "--langdef=Rust
--langmap=Rust:.rs
--regex-Rust=/^[ \\t]*(#\\[[^\\]]\\][ \\t]*)*(pub[ \\t]+)?(extern[ \\t]+)?(\"[^\"]+\"[ \\t]+)?(unsafe[ \\t]+)?fn[ \\t]+([a-zA-Z0-9_]+)/\\6/f,functions,function definitions/
--regex-Rust=/^[ \\t]*(pub[ \\t]+)?type[ \\t]+([a-zA-Z0-9_]+)/\\2/T,types,type definitions/
--regex-Rust=/^[ \\t]*(pub[ \\t]+)?enum[ \\t]+([a-zA-Z0-9_]+)/\\2/g,enum,enumeration names/
--regex-Rust=/^[ \\t]*(pub[ \\t]+)?struct[ \\t]+([a-zA-Z0-9_]+)/\\2/s,structure names/
--regex-Rust=/^[ \\t]*(pub[ \\t]+)?mod[ \\t]+([a-zA-Z0-9_]+)/\\2/m,modules,module names/
--regex-Rust=/^[ \\t]*(pub[ \\t]+)?(static|const)[ \\t]+(mut[ \\t]+)?([a-zA-Z0-9_]+)/\\4/c,consts,static constants/
--regex-Rust=/^[ \\t]*(pub[ \\t]+)?(unsafe[ \\t]+)?trait[ \\t]+([a-zA-Z0-9_]+)/\\3/t,traits,traits/
--regex-Rust=/^[ \\t]*(pub[ \\t]+)?(unsafe[ \\t]+)?impl([ \\t\\n]*<[^>]*>)?[ \\t]+(([a-zA-Z0-9_:]+)[ \\t]*(<[^>]*>)?[ \\t]+(for)[ \\t]+)?([a-zA-Z0-9_]+)/\\5 \\7 \\8/i,impls,trait implementations/
--regex-Rust=/^[ \\t]*macro_rules![ \\t]+([a-zA-Z0-9_]+)/\\1/d,macros,macro definitions/"
                ctags)))))))))

(defalias 'rust*-sysroot
  (lexical-let% ((b (rust*-sysroot-spec)))
    (lambda (&optional op)
      (cond ((eq op :new) (setq b (rust*-sysroot-spec)))
            (op (plist-get b op))
            (t b))))
  "Rust sysroot.")

;; end of sysroot

;;;
;; debug
;;;

(defun rust*-debug-spec ()
  "Return rust debugger spec."
  (let ((w (get-buffer-create* (symbol-name (gensym*)) t))
        (x (concat "/rustc/" (rust*-sysroot :hash)))
        (s (path+ (rust*-sysroot :src) "rust"))
        (f (concat (rust*-sysroot :etc)
                   (if-platform% 'gnu/linux
                       "gdb_load_rust_pretty_printers.py"
                     "lldb_commands"))))
    (unwind-protect
        (catch 'br
          (prog1 f
            (with-current-buffer w
              (insert-file-contents-literally* f)
              (goto-char (point-min))
              (when (re-search-forward
                     (concat (if-platform% 'gnu/linux
                                 "set substitute-path"
                               "^settings set target\\.source-map")
                             " " x)
                     nil t)
                (throw 'br f))
              (goto-char (point-min))
              (when (re-search-forward
                     (if-platform% 'gnu/linux
                         "set substitute-path"
                       "^settings set target\\.source-map")
                     nil t)
                (delete-line))
              (goto-char (point-max))
              (forward-line 1)
              (insert
               (if-platform% 'gnu/linux
                   (format "gdb.execute('set substitute-path %s %s')"
                           x s)
                 (format "settings set target.source-map %s %s"
                         x s)))
              (write-region* (point-min) (point-max) f nil :slient))))
      (when w (kill-buffer w)))))

(defalias 'rust*-make-debug!
  (lexical-let% ((b (rust*-debug-spec)))
    (lambda (&optional op)
      (cond ((eq op :new) (setq b (rust*-debug-spec)))
            (t b))))
  "Make rust source debuggable.")

;; end of debug

;;;
;; tags
;;;

(defun rust*-tags-spec ()
  "Return rust tags spec."
  (format "%srust.%s.TAGS" (tags-spec->% :root)
          (rust*-sysroot :hash)))

(defalias 'rust*-make-tags
  (lexical-let% ((b (rust*-tags-spec)))
    (lambda (&optional op)
      (cond ((eq op :new)
             (setq b (make-dir-ctags
                      (rust*-sysroot :src)
                      (rust*-tags-spec)
                      (rust*-sysroot :tag))))
            (t (and b (file-exists-p b) b)))))
  "Make rust tags.")

;; end of tags

(defun use-rust-init! ()
  "On \\=`rust\\=' initialization."
  (when-var% *tags-option-history* 'tags
    (let ((p (and (file-exists-p (rust*-sysroot :tag))
                  (concat "--options=" (rust*-sysroot :tag)))))
      (push! p *tags-option-history* t))) )

  ;; compile-time
  ;; (comment
  ;;  (unless (rust*-sysroot)
  ;;    (rust*-sysroot :new)
  ;;    (rust*-make-debug! :new)
  ;;    (rust*-make-tags :new)))




(provide 'use-rust)

;; end of use-rust.el
