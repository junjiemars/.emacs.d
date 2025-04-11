;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-rust.el
;;;;
;; features:
;;; 1. `rust' environment info.
;;; 2. make `rust-lldb' debugging environment.
;;; 3. make tags file.
;;;;
;; references:
;;; 1. https://www.rust-lang.org/
;;; 2. https://rustc-dev-guide.rust-lang.org/debugging-support-in-rustc.html
;;;;


;;; require

(eval-when-compile
  (require 'ed (v-home%> "config/ed"))
  (require 'tags (v-home%> "config/tags"))
  (require 'xrefs (v-home%> "config/xrefs")))

;; end of require

;;; sysroot

(defun rust*-sysroot-spec ()
  "Rust sysroot spec."
  (let ((rc (shell-command* "~/.cargo/bin/rustc --print sysroot 2>/dev/null")))
    (when (= 0 (car rc))
      (let ((sysroot (string-trim> (cdr rc))))
        (list
         :sysroot sysroot
         :hash (let ((h (shell-command* (path+ sysroot "bin/rustc -vV"))))
                 (when (= 0 (car h))
                   (string-match* "^commit-hash: \\([a-z0-9]+\\)"
                                  (cdr h) 1)))
         :etc (path+ sysroot "lib/rustlib/etc")
         :src (path+ sysroot "lib/rustlib/src")
         :tag (let ((ctags (concat (tags-spec->* :root) "ctags.rust")))
                (unless (file-exists-p ctags)
                  (copy-file (emacs-home* "config/use_rust.ctags") ctags))
                ctags))))))

(defalias 'rust*-sysroot
  (let ((b (rust*-sysroot-spec)))
    (lambda (&optional op)
      (cond ((and op (eq op :new)) (setq b (rust*-sysroot-spec)))
            (op (plist-get b op))
            (t b))))
  "Rust sysroot.")

;; end of sysroot

;;;
;; debug
;;;

(defun rust*-debug-spec ()
  "Rust debugging spec."
  (let ((w (get-buffer-create* (symbol-name (gensym*)) t))
        (x (concat "/rustc/" (rust*-sysroot :hash)))
        (s (path+ (rust*-sysroot :src) "rust"))
        (f (path+ (rust*-sysroot :etc)
             (if-platform% gnu/linux
                 "gdb_load_rust_pretty_printers.py"
               "lldb_commands"))))
    (unwind-protect
        (catch :br
          (prog1 f
            (with-current-buffer w
              (insert-file-contents-literally* f)
              (goto-char (point-min))
              (when (re-search-forward
                     (concat (if-platform% gnu/linux
                                 "set substitute-path"
                               "^settings set target\\.source-map")
                             " " x)
                     nil t)
                (throw :br f))
              (goto-char (point-min))
              (when (re-search-forward
                     (if-platform% gnu/linux
                         "set substitute-path"
                       "^settings set target\\.source-map")
                     nil t)
                (delete-line*))
              (goto-char (point-max))
              (forward-line 1)
              (insert
               (if-platform% gnu/linux
                   (format "gdb.execute('set substitute-path %s %s')"
                           x s)
                 (format "settings set target.source-map %s %s"
                         x s)))
              (write-region* (point-min) (point-max) f nil :slient))))
      (when w (kill-buffer w)))))

(defalias 'rust*-debug-env-make!
  (let ((b (rust*-debug-spec)))
    (lambda (&optional op)
      (cond ((and op (eq op :new)) (setq b (rust*-debug-spec)))
            (t b))))
  "Make rust source debuggable.")

;; end of debug

;;;
;; tags
;;;

(defun rust*-make-tags (&optional renew)
  "Make rust tags."
  (let ((file (format "%srust.%s.TAGS"
                      (tags-spec->* :root) (rust*-sysroot :hash))))
    (cond (renew (make-dir-ctags
                  (rust*-sysroot :src) file (rust*-sysroot :tag)))
          (t file))))

;; end of tags

(defun use-rust-init! ()
  "On \\=`rust\\=' initialization."
  (xref*-read-only-dirs :push (rust*-sysroot :sysroot))
  (rust*-debug-env-make!))



(provide 'use-rust)

;; end of use-rust.el
