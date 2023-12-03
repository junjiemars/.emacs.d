;;; on-inter-autoload.el --- editing -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-inter-autoload.el
;;;;


;; Title bar with full path
(when-graphic%
  (setq% frame-title-format "%b (%f)"))


;; Ignore ring bell
(setq% ring-bell-function 'ignore)


;; Changes all yes/no questions to y/n type
;; (defalias 'yes-or-no-p 'y-or-n-p)


;; Shows all options when running apropos. For more info,
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
;;enable apropos-do-all, but slower
(setq% apropos-do-all t 'apropos)


;; Save before kill
(setq% save-interprogram-paste-before-kill t 'simple)

;; Mouse yank commands yank at point instead of at click.
(setq% mouse-yank-at-point t 'mouse)


;; no need for .# files when editing
(setq% create-lockfiles nil)


;; enable upcase/downcase region
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)


;;; Messages' keys

(when-version% > 24.4
  ;; fix: no quit key to hide *Messages* buffer for ancient Emacs
  ;; [DEL] for `scroll-down'
  ;; [SPC] for `scroll-up'
  (with-current-buffer (get-buffer "*Messages*")
    (if-fn% 'read-only-mode nil
            (read-only-mode 1)
      (toggle-read-only t))
    (local-set-key (kbd "q") #'quit-window)
    (local-set-key (kbd "DEL") #'scroll-down)
    (local-set-key (kbd "SPC") #'scroll-up)))

;; end of Message' keys


;; fix: `uniquify' may not be autoloaded on ancient Emacs.
(when-version% > 24
  (when% (fluid-let (byte-compile-warnings nil)
           (require 'uniquify nil t))
    (require 'uniquify)
    (setq uniquify-buffer-name-style 'post-forward-angle-brackets)))

;; end of `uniquify'


;;; Makes killing/yanking interact with the clipboard
;;; See also: http://emacswiki.org/emacs/CopyAndPaste

(if-version%
    <= 24.1
    (setq% select-enable-clipboard t)
  (setq% x-select-enable-clipboard t))
(if-version%
    <= 25.1
    (setq% select-enable-primary t 'select)
  (setq% x-select-enable-primary t 'select))

(unless-graphic%
  (unless-platform% 'windows-nt
    (eval-when-compile

      (defmacro _defun_x_select_text*_ (bin &rest args)
        "Define `x-select-text*'"
        `(defun x-select-text* (text &optional _unused_)
           "Copy TEXT to system clipboard."
           (with-temp-buffer
             (insert text)
             (call-process-region (point-min) (point-max)
                                  ,bin
                                  nil 0 nil
                                  ,@args))))

      (defmacro _defun_x_selection_value*_ (bin &rest args)
        "Define `x-selection-value*'"
        `(defun x-selection-value* ()
           "Paste from system clipboard."
           (let ((out (shell-command* ,bin ,@args)))
             (when (zerop (car out))
               (cdr out)))))

      (defmacro _enable_x_select_clipboard!_ ()
        "Enable `x-select-clipboard'"
        `(progn
           (setq interprogram-cut-function #'x-select-text*)
           (setq interprogram-paste-function #'x-selection-value*)))))

  (when-platform% 'darwin
    (_defun_x_select_text*_ "pbcopy")
    (_defun_x_selection_value*_ "pbpaste")
    (_enable_x_select_clipboard!_))

  (when-platform% 'gnu/linux
    (when% (executable-find% "xsel")
      (_defun_x_select_text*_ "xsel" "--clipboard" "--input")
      (_defun_x_selection_value*_ "xsel" "--clipboard" "--output")
      (_enable_x_select_clipboard!_))))


;; end of Makes killing/yanking interact with the clipboard


;;; open-*-line fn
;;; control indent or not: `open-next-line' and `open-previous-line'.
;;; see also: https://www.emacswiki.org/emacs/OpenNextLine

(defun open-next-line (n &optional indent)
  "Move to the next line and then open N lines, like vi\\=' o
command.\n
Optional argument INDENT whether to indent lines. See also
\\=`open-line\\='."
  (interactive (list (prefix-numeric-value
                      (if (consp current-prefix-arg)
                          1
                        current-prefix-arg))
                     (if current-prefix-arg
                         (y-or-n-p "Indent? ")
                       t)))
  (barf-if-buffer-read-only)
  (end-of-line)
  (open-line n)
  (forward-line 1)
  (when indent
    (indent-according-to-mode)))

(defun open-previous-line (n &optional indent)
  "Open N lines above the current one, like vi\\=' O command.\n
Optional argument INDENT whether to indent lines. See also
\\=`open-line\\=' and \\=`split-line\\='."
  (interactive (list (prefix-numeric-value
                      (if (consp current-prefix-arg)
                          1
                        current-prefix-arg))
                     (if current-prefix-arg
                         (y-or-n-p "Indent:? ")
                       t)))
  (barf-if-buffer-read-only)
  (beginning-of-line)
  (open-line n)
  (and indent (indent-according-to-mode)))


;; end of open-*-line


;; Greek letters C-x 8 <RET> greek small letter lambda

;; bind `insert-char*' to [C-x 8 RET] for ancient Emacs
(if-key% (current-global-map)
    (kbd "C-x 8 RET")
    (lambda (def) (not (eq def #'insert-char)))
    (progn
      (defun insert-char* (character &optional count inherit)
        "Interactive `insert-char' for ancient Emacs."
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
            (error "Invalid character"))
          (insert-char c count)))
      (define-key% (current-global-map) (kbd "C-x 8 RET") #'insert-char*)))

;; end of `insert-char*'


(unless-fn% 'count-words-region 'simple
  (defalias 'count-words-region #'count-lines-region
    "\\=`count-lines-region\\=' had been obsoleted since Emacs24.1+"))

;; end of `count-lines-region'


;;; Comment

(defun toggle-comment (&optional n)
  "Toggle N lines\\=' comment on current line or region."
  (interactive "p")
  (let ((begin (region-active-if (region-beginning)
                 (if (and (< n 0))
                     (save-excursion
                       (forward-line (1+ n))
                       (line-beginning-position))
                   (line-beginning-position))))
        (end (region-active-if (region-end)
               (if (and (> n 0))
                   (save-excursion
                     (forward-line (1- n))
                     (line-end-position))
                 (line-end-position)))))
    (comment-or-uncomment-region begin end)
    (region-active-unless (forward-line n))
    (beginning-of-line)))

;; end of Comment


(defun surround-region (&optional begin end)
  "Surround region with BEGIN or END string.\n
If \\=`current-prefix-arg\\=' is nil or equals to 0, then repeat 1 times.
If \\=`current-prefix-arg\\=' > 0, then repeat n times.
If \\=`current-prefix-arg\\=' < 0, then repeat n time with END in reversed."
  (interactive (list (read-string "surround region begin with: "
                                  "<space>")
                     (read-string "surround region end with: "
                                  "<begin>")))
  (let ((bounds (region-active-if
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


(defun camelize (string &optional separator)
  "Return camel case of STRING."
  (let ((ss (split-string* (or string "") (or separator "_"))))
    (concat (downcase (car ss))
            (mapconcat #'capitalize (cdr ss) ""))))

;; end of camelize


;;; Clean Emacs' user files

(defun clean-versioned-dirs (dirs &optional scope)
  "Clean versioned SCOPEd DIRS."
  (dolist* (d dirs)
    (when (and d (file-exists-p d))
      (dolist* (f (directory-files d nil "^[gt]_.*$"))
        (when (cond ((eq :8 scope) t)
                    ((eq :< scope)
                     (< (string-to-number
                         (string-match* "^[gt]_\\(.*\\)$" f 1))
                        +emacs-version+))
                    (t (null (string-match
                              (format
                               "^[gt]_%s\\'"
                               (regexp-quote
                                (number-to-string +emacs-version+)))
                              f))))
          (if-platform% 'windows-nt
              (shell-command (concat "rmdir /Q /S " (concat d f)))
            (shell-command (concat "rm -r " (concat d f)))))))))

(defun reset-emacs ()
  "Clean all compiled files and dot files, then kill Emacs."
  (interactive)
	(when (yes-or-no-p "Reset emacs?")
		(clean-versioned-dirs
		 (delq nil
					 (mapcar
						(lambda (d)
							(unless (member d '(".git" ".gitignore" ".github"))
								(concat (emacs-home* d) "/")))
						(directory-files (emacs-home*) nil "^\\.[a-z]+")))
		 :8)
		(clean-compiled-files)
		(setq kill-emacs-hook nil)
		(kill-emacs 0)))

;; end of Clean Emacs' user files


;;; Keys

;; Line
(define-key% (current-global-map) (kbd "C-o") #'open-next-line)
(define-key% (current-global-map) (kbd "C-M-o") #'open-previous-line)

;; Comment
(define-key% (current-global-map) (kbd "C-x M-;") #'toggle-comment)
(define-key% (current-global-map) (kbd "C-x ;") #'comment-indent)

;; Surround
(define-key% (current-global-map) (kbd "C-x r [") #'surround-region)

;; `imenu'
(define-key% (current-global-map) (kbd "M-g i") #'imenu)

;; end of Keys


;;; inter modes

;; No cursor blinking, it's distracting
(when-fn% 'blink-cursor-mode nil (blink-cursor-mode 0))

;; Highlights matching parenthesis
(show-paren-mode 1)

;; Enable `column-number-mode'
(setq% column-number-mode t 'simple)

;; Enable save minibuffer history
(if-version%
    <= 24
    (savehist-mode)
  (savehist-mode t))

;; Enable save-place
(if-version%
    <= 25.1
    (save-place-mode t)
  (setq% save-place t 'saveplace))

(require 'view nil t)
(when-fn% 'ido-mode 'ido (ido-mode t))
(when-version% > 28 (require 'dired-x nil t))

;; inter modes

;; end of on-inter-autoload.el
