;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; progs.el
;;;;
;; Commentary: essential for programming.
;;;;

;;; killing/yanking with the clipboard
;;; See also: http://emacswiki.org/emacs/CopyAndPaste

(defmacro when-x-select% (&rest body)
  "When x-select do BODY."
  (declare (indent 0))
  `(unless-graphic%
     (unless-platform% 'windows-nt
       (if-platform% 'darwin
           (when% (and (executable-find% "pbcopy")
                       (executable-find% "pbpaste"))
             ,@body)
         (if-platform% 'gnu/linux
             (when% (executable-find% "xsel")
               ,@body))))))

(when-x-select%
  (defmacro defun-x-kill* (bin &rest args)
    (declare (indent 1))
    `(defun x-kill* (text &optional _)
       "Copy TEXT to system clipboard."
       (with-temp-buffer
         (insert text)
         (call-process-region (point-min) (point-max)
                              ,bin
                              nil 0 nil
                              ,@args)))))

(when-x-select%
  (defmacro defun-x-yank* (bin &rest args)
    (declare (indent 1))
    `(defun x-yank* ()
       "Paste from system clipboard."
       (let ((out (shell-command* ,bin ,@args)))
         (when (zerop (car out))
           (cdr out))))))

(when-x-select%
  (when-platform% 'darwin
    (defun-x-kill* "pbcopy")))
(when-x-select%
  (when-platform% 'darwin
    (defun-x-yank* "pbpaste")))

(when-x-select%
  (when-platform% 'gnu/linux
    (defun-x-kill* "xsel" "--clipboard" "--input")))
(when-x-select%
  (when-platform% 'gnu/linux
    (defun-x-yank* "xsel" "--clipboard" "--output")))

;; end of killing/yanking with the clipboard


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


;;; bind `insert-char*' to [C-x 8 RET] for ancient Emacs

(defmacro unless-key-insert-char% (&rest body)
  "Unless [C-x 8 RET] key bind to \\=`insert-char\\='."
  (declare (indent 0))
  (if-key% (current-global-map) (kbd "C-x 8 RET")
           (lambda (def) (null (eq def #'insert-char)))
    `(progn% ,@body)
    `(comment ,@body)))

(unless-key-insert-char%
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
        (error "%s" "Invalid character"))
      (insert-char c count))))

;; end of `insert-char*'

;;; `count-lines-region'

(unless-fn% 'count-words-region 'simple
  (defalias 'count-words-region #'count-lines-region
    "\\=`count-lines-region\\=' had been obsoleted since Emacs24.1+"))

;; end of `count-lines-region'

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


(defun camelize (string &optional separator)
  "Return camel case of STRING."
  (let ((ss (split-string* (or string "") (or separator "_"))))
    (concat (downcase (car ss))
            (mapconcat #'capitalize (cdr ss) ""))))

;; end of camelize

;;;
;; `recenter-top-bottom' for Emacs23.2-
;;;

(defmacro unless-fn-recenter-top-bottom% (&rest body)
  `(unless-fn% 'recenter-top-bottom nil
     ,@body))

(unless-fn-recenter-top-bottom%
 (defvar recenter-last-op nil
   "Indicates the last recenter operation performed.\n
Possible values: \\=`top\\=', \\=`middle\\=', \\=`bottom\\=',
integer or float numbers.  It can also be nil, which means the
first value in \\=`recenter-positions\\='."))

(unless-fn-recenter-top-bottom%
 (defvar recenter-positions '(middle top bottom)
   "Cycling order for \\=`recenter-top-bottom\\='.
A list of elements with possible values \\=`top\\=',
\\=`middle\\=', \\=`bottom\\=', integer or float numbers that
define the cycling order for the command
\\=`recenter-top-bottom\\='.\n Top and bottom destinations are
\\=`scroll-margin\\=' lines from the true window top and bottom.
Middle redraws the frame and centers point vertically within the
window.  Integer number moves current line to the specified
absolute window-line.  Float number between 0.0 and 1.0 means the
percentage of the screen space from the top.  The default cycling
order is middle -> top -> bottom."))

(unless-fn-recenter-top-bottom%
 (defun recenter-top-bottom (&optional arg)
   "Move current buffer line to the specified window line.
With no prefix argument, successive calls place point according
  o the cycling order defined by \\=`recenter-positions\\='.\n A
prefix argument is handled like \\=`recenter\\=': With numeric
prefix ARG, move current line to window-line ARG.  With plain
\\=`C-u\\=', move current line to window center."
   (interactive "P")
   (cond (arg (recenter arg))
         (t (setq recenter-last-op
                  (if (eq this-command last-command)
                      (car (or (cdr (memq recenter-last-op
                                          recenter-positions))
                               recenter-positions))
                    (car recenter-positions)))
            (let ((this-scroll-margin
                   (min (max 0 scroll-margin)
                        (truncate (/ (window-body-height) 4.0)))))
              (cond ((eq recenter-last-op 'middle)
                     (recenter))
                    ((eq recenter-last-op 'top)
                     (recenter this-scroll-margin))
                    ((eq recenter-last-op 'bottom)
                     (recenter (- -1 this-scroll-margin)))
                    ((integerp recenter-last-op)
                     (recenter recenter-last-op))
                    ((floatp recenter-last-op)
                     (recenter (round (* recenter-last-op
                                         (window-height)))))))))))

;; end of `recenter-top-bottom'

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
                    (buffer-name)))))
    (kill-new name)
    (message "%s" name)))

(defun get-buffer-coding-system (&optional buffer)
  "Return the coding system of current buffer or BUFFER."
  (interactive)
  (with-current-buffer (or buffer
                           (current-buffer))
    (if (called-interactively-p*)
        (message "%s" buffer-file-coding-system)
      buffer-file-coding-system)))

;; end of buffer

;;;
;; env
;;;

(defun on-progs-env! ()
  ;; title bar with full path
  (when-graphic%
    (setq% frame-title-format "%b (%f)"))
  ;; ignore ring bell
  (setq% ring-bell-function 'ignore)
  ;; keep `view-mode' when quit
  ;; (when-var% view-mode-map 'view
  ;;   (define-key% view-mode-map (kbd "q") #'quit-window))
  ;; treat `read-only-mode' as `view-mode'
  (setq view-read-only t)
  ;; Changes all yes/no questions to y/n type
  ;; (defalias 'yes-or-no-p 'y-or-n-p)

  ;; shows all options when running apropos. For more info,
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
  ;;enable apropos-do-all, but slower
  (setq% apropos-do-all t 'apropos)
  ;; save before kill
  (setq% save-interprogram-paste-before-kill t 'simple)
  ;; mouse yank commands yank at point instead of at click.
  (setq% mouse-yank-at-point t 'mouse)
  ;; no need for .# files when editing
  (setq% create-lockfiles nil)
  ;; enable upcase/downcase region
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  ;; messages' keys
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
  ;; enable select
  (if-version%
      <= 24.1
      (setq% select-enable-clipboard t)
    (setq% x-select-enable-clipboard t))
  (if-version%
      <= 25.1
      (setq% select-enable-primary t 'select)
    (setq% x-select-enable-primary t 'select))
  (when-x-select%
    (setq interprogram-cut-function #'x-kill*
          interprogram-paste-function #'x-yank*)))

;; end of `on-progs-env!'

;;;
;; key
;;;

(defun on-progs-key! ()
  ;; line
  (define-key% (current-global-map) (kbd "C-o") #'open-next-line)
  (define-key% (current-global-map) (kbd "C-M-o") #'open-previous-line)
  ;; comment
  (define-key% (current-global-map) (kbd "C-x M-;") #'toggle-comment)
  (define-key% (current-global-map) (kbd "C-x ;") #'comment-indent)
  ;; surround
  (define-key% (current-global-map) (kbd "C-x r [") #'surround-region)
  ;; `imenu'
  (define-key% (current-global-map) (kbd "M-g i") #'imenu)
  ;; `insert-char*'
  (unless-key-insert-char%
    (define-key% (current-global-map) (kbd "C-x 8 RET")
                 #'insert-char*))
  ;; lookup dictionary
  (define-key% (current-global-map) (kbd "M-s d") 'lookup-dict)
  ;; open file or url at point
  (when-fn% 'find-file-at-point 'ffap
    (define-key% (current-global-map) (kbd "C-c f f") #'find-file-at-point))
  ;; shows a list of buffers
  (define-key% (current-global-map) (kbd "C-x C-b") #'ibuffer)
  ;; interactive query replace key bindings.
  (define-key% (current-global-map) (kbd "M-%") #'query-replace-regexp)
  (define-key% (current-global-map) (kbd "C-M-%") #'query-replace)
  ;; register:
  ;; `C-x r g' and `C-x r i' are all bound to insert-register
  ;; let `C-x r g' do `string-insert-rectangle'
  (define-key% (current-global-map) (kbd "C-x r g") #'string-insert-rectangle)
  (define-key% (current-global-map) (kbd "C-x r v") #'view-register)
  ;; line
  (when-fn% 'electric-newline-and-maybe-indent 'electric
    ;; Default behaviour of RET
    ;; https://lists.gnu.org/archive/html/emacs-devel/2013-10/msg00490.html
    ;; electric-indent-mode: abolition of `newline' function is not
    ;; the Right Thing
    ;; https://lists.gnu.org/archive/html/emacs-devel/2013-10/msg00407.html
    (define-key% (current-global-map) (kbd "RET")
                 #'electric-newline-and-maybe-indent)
    (define-key% (current-global-map) (kbd "C-j") #'newline))
  ;; sorting
  (define-key% (current-global-map) (kbd "C-c s f") #'sort-fields)
  (define-key% (current-global-map) (kbd "C-c s n") #'sort-numeric-fields)
  (define-key% (current-global-map) (kbd "C-c s x") #'sort-regexp-fields)
  (define-key% (current-global-map) (kbd "C-c s l") #'sort-lines)
  (define-key% (current-global-map) (kbd "C-c s r") #'reverse-region)
  (define-key% (current-global-map) (kbd "C-c s d") #'delete-duplicate-lines)
  ;; windows
  (define-key% (current-global-map) (kbd "C-c w l") #'windmove-left)
  (define-key% (current-global-map) (kbd "C-c w r") #'windmove-right)
  (define-key% (current-global-map) (kbd "C-c w u") #'windmove-up)
  (define-key% (current-global-map) (kbd "C-c w d") #'windmove-down)
  ;; buffers
  (declare-function browse-url-default-browser "browse-file" t)
  (define-key% (current-global-map) (kbd "C-l") #'recenter-top-bottom)
  ;; (define-key% (current-global-map) (kbd "C-x x B") #'browse-file)
  (define-key% (current-global-map) (kbd "C-x x c") #'clone-buffer)
  (define-key% (current-global-map) (kbd "C-x x n") #'echo-buffer-name)
  (define-key% (current-global-map) (kbd "C-x x t") #'toggle-truncate-lines)
  (define-key% (current-global-map) (kbd "C-x RET =")
               #'get-buffer-coding-system)
  (define-key% (current-global-map) (kbd "C-x x g")
               (if-fn% 'revert-buffer-quick nil
                       #'revert-buffer-quick
                 #'revert-buffer))
  (define-key% (current-global-map) (kbd "C-x x l")
               (if-fn% 'display-line-numbers-mode 'display-line-numbers
                       #'display-line-numbers-mode
                 (if-fn% 'linum-mode 'linum
                         #'linum-mode
                   #'(lambda ()
                       (interactive)
                       (user-error "%s" "No line mode found")))))
  (define-key% (current-global-map) (kbd "C-x x r") #'rename-buffer)
  (when-fn% 'toggle-word-wrap 'simple
    (define-key% (current-global-map) (kbd "C-x x w") #'toggle-word-wrap))
  (when-fn% 'whitespace-mode 'whitespace
    (define-key% (current-global-map) (kbd "C-x x SPC") #'whitespace-mode))
  (define-key% (current-global-map) (kbd "C-x x u") #'rename-uniquely))

;; end of `on-progs-key!'

;;;
;; mode
;;;

(defun on-progs-mode! ()
  (inhibit-gc
    ;; no cursor blinking, it's distracting
    (when-fn% 'blink-cursor-mode nil (blink-cursor-mode 0))
    ;; enable `column-number-mode'
    (setq% column-number-mode t 'simple)
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
      (setq% save-place t 'saveplace))
    (require 'view nil t)
    (when-fn% 'ido-mode 'ido (ido-mode t))
    ;; fix: `uniquify' may not be autoloaded on ancient Emacs.
    (when-version% > 24
      (when% (fluid-let (byte-compile-warnings nil)
               (require 'uniquify nil t))
        (require 'uniquify)
        (setq uniquify-buffer-name-style 'post-forward-angle-brackets)))))

;; end of `on-progs-mode!'

(defun on-progs-init! ()
  (inhibit-gc
    (on-progs-env!)
    (on-progs-key!)
    (on-progs-mode!)))



(provide 'progs)


;; end of progs.el
