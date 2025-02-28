;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; progs.el
;;;;
;; Commentary: essential for programming.
;;;;


;;; require

;; `new-line*'
(require% 'ed (v-home%> "config/ed"))

;; end of require

;;;
;; bind `insert-char*' to [C-x 8 RET] for ancient Emacs
;; greek letters C-x 8 <RET> greek small letter lambda
;;;

(defmacro unless-key-insert-char% (&rest body)
  "Unless [C-x 8 RET] key bind to \\=`insert-char\\='."
  (declare (indent 0))
  (if-key% (current-global-map) "8" #'insert-char
           `(comment ,@body)
    `(progn% ,@body)))

(unless-key-insert-char%
  (defun insert-char* (character &optional count inherit)
    "Interactive \\=`insert-char\\=' for ancient Emacs."
    (interactive
     (list (read-string "Insert character (Unicode or hex number): ")
           (prefix-numeric-value current-prefix-arg)
           t))
    (let ((c (cond ((string-match
                     "\\`#[xX][0-9a-fA-F]+\\|#[oO][0-7]+\\'"
                     character)
                    (ignore-errors (read character)))
                   ((string-match "\\?\\\\u[0-9a-fA-F]" character)
                    (ignore-errors (read character)))
                   ((string-match "\\`[0-9a-fA-F]+\\'" character)
                    (ignore-errors (read (concat "?\\u" character)))))))
      (unless (characterp c)
        (error "%s" "Invalid character"))
      (insert-char c count inherit))))

;; end of `insert-char*'

;;;
;; Comment
;;;

(defun toggle-comment (&optional n)
  "Toggle N lines\\=' comment on current line or region."
  (interactive "p")
  (let ((begin (if-region-active (region-beginning)
                 (if (and (< n 0))
                     (save-excursion
                       (forward-line (1+ n))
                       (line-beginning-position))
                   (line-beginning-position))))
        (end (if-region-active (region-end)
               (if (and (> n 0))
                   (save-excursion
                     (forward-line (1- n))
                     (line-end-position))
                 (line-end-position)))))
    (comment-or-uncomment-region begin end)
    (unless-region-active (forward-line n))
    (beginning-of-line)))

;; end of Comment

;;;
;; surround region
;;;

(defun surround-region (&optional begin end)
  "Surround region with BEGIN or END string.\n
If \\=`current-prefix-arg\\=' is nil or equals to 0, then repeat 1 times.
If \\=`current-prefix-arg\\=' > 0, then repeat n times.
If \\=`current-prefix-arg\\=' < 0, then repeat n time with END in reversed."
  (interactive (list (read-string "surround region begin with: "
                                  "<space>")
                     (read-string "surround region end with: "
                                  "<begin>")))
  (let ((bounds (if-region-active
                    (cons (region-beginning) (region-end))
                  (cons (point) (point)))))
    (let* ((n (if current-prefix-arg (abs current-prefix-arg) 1))
           (r (and current-prefix-arg (< current-prefix-arg 0)))
           (begin (let ((o (if (string= "<space>" begin) " " begin))
                        (n1 n))
                    (while (> n1 1)
                      (setq o (concat o o)
                            n1 (1- n1)))
                    o))
           (end (let ((cc (if (string= "<begin>" end)
                              begin
                            (let ((c end)
                                  (n2 n))
                              (while (> n2 1)
                                (setq c (concat c c)
                                      n2 (1- n2)))
                              c))))
                  (if r (reverse cc) cc))))
      (with-current-buffer (current-buffer)
        (goto-char (car bounds))
        (insert begin)
        (goto-char (+ (cdr bounds) (length begin)))
        (insert end)))))

;; end of `surround-region'

;;;
;; string case
;;;

(defun downcase* (string)
  "Return down case of STRING."
  (when (stringp string)
    (let ((ss (copy-sequence string))
          (len (length string))
          (i 0))
      (prog1 ss
        (while (< i len)
          (let ((c (aref ss i)))
            (when (and (>= c ?A) (<= c ?Z))
              (aset ss i (+ c 32))))
          (setq i (1+ i)))))))

(defun camelize (string)
  "Return camel case of STRING."
  (when (and (stringp string) (> (length string) 0))
    (cond ((string-match
            "[a-zA-Z]+_\\([_a-zA-Z0-9]+\\)*" string)
           (let ((ss (split-string* string "_")))
             (concat (downcase* (car ss))
                     (mapconcat #'capitalize (cdr ss) ""))))
          ((string-match
            "[A-Z][a-z0-9]+\\([A-Z][a-z0-9]*\\)*" string)
           (concat (downcase* (substring string 0 1))
                   (substring string 1)))
          ((string-match
            "[_A-Z0-9]+" string)
           (downcase* string))
          (t string))))

;; end of string case

;;;
;; buffer
;;;

(defun echo-buffer-name ()
  "Echo the qualified buffer name of current buffer.\n
And copy the qualified buffer name to kill ring."
  (interactive)
  (let ((name (if (eq 'dired-mode major-mode)
                  (expand-file-name default-directory)
                (or (buffer-file-name)
                    (buffer-name))))
        (default-directory (emacs-home%)))
    (kill-new name t)
    (message "%s" name)))

(defun echo-buffer-coding-system (&optional buffer)
  "Echo the coding system of current buffer or BUFFER."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (if (called-interactively?)
        (message "%s" buffer-file-coding-system)
      buffer-file-coding-system)))

;; end of buffer

;;;
;; ido
;;;

(defun on-ido-init! ()
  "On \\=`ido\\=' intialization."
  (if-fn% ido-mode ido
          (progn
            (ido-mode t)
            (define-global-key% "5r" #'ido-find-file-read-only-other-frame)
            (define-global-key% "4r" #'ido-find-file-read-only-other-window)
            (define-global-key% "" #'ido-find-file-read-only)
            (setq% ido-enable-flex-matching t ido))
    ;; default view file keybindings
    (define-global-key% "5r" #'view-file-other-frame)
    (define-global-key% "4r" #'view-file-other-window)
    (define-global-key% "" #'view-file))
  (when-version% > 28 (require 'dired-x nil t)))

;; end of ido

;;;
;; `minibuffer'
;;;

(defun on-minibuffer-init! ()
  (let ((km (if-var% minibuffer-local-completion-map minibuffer
                     minibuffer-local-completion-map
              minibuffer-local-map))
        (kf (if-fn% minibuffer-complete minibuffer
                    #'minibuffer-complete
              (if-fn% completion-at-point minibuffer
                      #'completion-at-point
                #'lisp-complete-symbol))))
    (define-key km (kbd% "TAB") kf)
    (define-key km (kbd% "C-M-i") kf)))

;; end of `minibuffer'

;;;
;; `sort'
;;;

(defun on-sort-init! ()
  "On \\=`sort\\=' intialization."
  (define-global-key% "sf" #'sort-fields)
  (define-global-key% "sn" #'sort-numeric-fields)
  (define-global-key% "sx" #'sort-regexp-fields)
  (define-global-key% "sl" #'sort-lines)
  (define-global-key% "sr" #'reverse-region)
  (define-global-key% "sd" #'delete-duplicate-lines))

(when-fn% whitespace-mode whitespace
  (defun on-whitespace-init! ()
    "On \\=`sort\\=' intialization."
    (define-global-key% (kbd "C-x x SPC") #'whitespace-mode)
    (unless-graphic%
      (with-eval-after-load 'whitespace
        (set-face-background 'whitespace-space nil)))))

;; end of `sort'

;;;
;; env
;;;

(defun on-progs-env! ()
  ;; count region
  (unless-fn% count-words-region simple
    (defalias 'count-words-region #'count-lines-region
      "\\=`count-lines-region\\=' had been obsoleted since Emacs24.1+"))
  ;; title bar with full path
  (when-graphic%
    (setq% frame-title-format "%b (%f)"))
  ;; ignore ring bell
  (setq% ring-bell-function 'ignore)
  ;; treat `read-only-mode' as `view-mode'
  (setq view-read-only t)
  ;; Changes all yes/no questions to y/n type
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; shows all options when running apropos. For more info,
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
  ;;enable apropos-do-all, but slower
  (setq% apropos-do-all t apropos)
  ;; save before kill
  (setq% save-interprogram-paste-before-kill t simple)
  ;; mouse yank commands yank at point instead of at click.
  (setq% mouse-yank-at-point t mouse)
  ;; no need for .# files when editing
  (setq% create-lockfiles nil)
  ;; enable upcase/downcase region
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  ;; fix some terminal theme confused with background and foreground.
  (unless-graphic%
    (set-face-background 'region +term-background-color+)
    (set-face-foreground 'region +term-foreground-color+)
    (set-face-background 'match +term-background-color+)
    (set-face-foreground 'match +term-foreground-color+)
    (when% (facep 'completions-highlight)
      (set-face-background 'completions-highlight +term-background-color+)
      (set-face-foreground 'completions-highlight +term-foreground-color+))))

;; end of `on-progs-env!'

;;;
;; key
;;;

(defun on-progs-key! ()
  ;; line
  (define-global-key% (kbd "C-o") #'open-next-line)
  (define-global-key% (kbd "C-M-o") #'open-previous-line)
  ;; comment
  (define-global-key% (kbd "C-x M-;") #'toggle-comment)
  (define-global-key% ";" #'comment-indent)
  ;; surround
  (define-global-key% "r[" #'surround-region)
  ;; `imenu'
  (define-global-key% (kbd "M-g i") #'imenu)
  ;; `insert-char*'
  (unless-key-insert-char%
    (define-global-key% "8" #'insert-char*))
  ;; open file or url at point
  (when-fn% find-file-at-point ffap
    (define-global-key% "ff" #'find-file-at-point))
  ;; shows a list of buffers
  (define-global-key% "" #'ibuffer)
  ;; interactive query replace key bindings.
  (define-global-key% (kbd "M-%") #'query-replace-regexp)
  (define-global-key% (kbd "C-M-%") #'query-replace)
  ;; register:
  ;; `C-x r g' and `C-x r i' are all bound to insert-register
  ;; let `C-x r g' do `string-insert-rectangle'
  (define-global-key% "rg" #'string-insert-rectangle)
  (define-global-key% "rv" #'view-register)
  ;; line
  (define-key (current-global-map) (kbd% "RET") #'newline*)
  (define-global-key% (kbd "C-j") #'newline-and-indent)
  ;; buffers
  (define-global-key% "xc" #'clone-buffer)
  (define-global-key% "xn" #'echo-buffer-name)
  (define-global-key% "xt" #'toggle-truncate-lines)
  (define-global-key% (kbd "C-x RET =") #'echo-buffer-coding-system)
  (define-global-key% "xg"
               (if-fn% revert-buffer-quick nil
                       #'revert-buffer-quick
                 #'revert-buffer))
  ;; line number mode
  (define-key (current-global-map) "xl"
              (if-fn% display-line-numbers-mode display-line-numbers
                      (prog1 #'display-line-numbers-mode
                        (setq% display-line-numbers-type 'relative
                               display-line-numbers)
                        (setq% display-line-numbers-current-absolute nil
                               display-line-numbers))
                (if-fn% linum-mode linum
                        #'linum-mode
                  #'(lambda (&optional _)
                      (interactive)
                      (error (kbd% "%s") "No line number mode found")))))
  (define-global-key% "xr" #'rename-buffer)
  (when-fn% toggle-word-wrap simple
    (define-global-key% "xw" #'toggle-word-wrap))
  (define-global-key% "xu" #'rename-uniquely)
  ;; `messages-buffer-mode'
  (when-version% > 24.4
    ;; fix: no quit key to hide *Messages* buffer for ancient Emacs
    ;; [DEL] for `scroll-down'
    ;; [SPC] for `scroll-up'
    (with-current-buffer (get-buffer "*Messages*")
      (if-fn% read-only-mode nil
              (read-only-mode 1)
        (toggle-read-only t))
      (local-set-key "q" #'quit-window)
      (local-set-key (kbd% "DEL") #'scroll-down)
      (local-set-key (kbd% "SPC") #'scroll-up))))

;; end of `on-progs-key!'

;;;
;; mode
;;;

(defun on-progs-mode! ()
  ;; no cursor blinking, it's distracting
  (when-fn% blink-cursor-mode nil (blink-cursor-mode 0))
  ;; enable `column-number-mode'
  (setq% column-number-mode t simple)
  ;; highlights matching parenthesis
  (inhibit-blinking (show-paren-mode 1))
  ;; enable save minibuffer history
  (if-version%
      <= 24
      (savehist-mode)
    (savehist-mode t))
  ;; enable save-place
  (if-version%
      <= 25.1
      (save-place-mode t)
    (setq% save-place t saveplace))
  (require 'view nil t)
  ;; fix: `uniquify' may not be autoloaded on ancient Emacs.
  (when-version% > 24
    (when% (fluid-let (byte-compile-warnings nil)
             (require 'uniquify nil t))
      (require 'uniquify)
      (setq uniquify-buffer-name-style 'post-forward-angle-brackets))))

;; end of `on-progs-mode!'

(defun on-progs-init! ()
  (on-progs-env!)
  (on-progs-key!)
  (on-progs-mode!)
  (on-ido-init!)
  (on-minibuffer-init!)
  (on-sort-init!)
  (when-fn% whitespace-mode whitespace
    (on-whitespace-init!)))



(provide 'progs)


;; end of progs.el
