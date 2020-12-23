;;; on-edit-autoload.el --- editing -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-edit-autoload.el
;;;;
;;; Commentary:
;;

;;; Code:

(unless-graphic%
  ;; above version 23 transient-mark-mode is enabled by default
  (when-version% > 23 (transient-mark-mode t))
  ;; fix some terminal theme confused with background and foreground.
  (set-face-background 'region "white")
  (set-face-foreground 'region "black"))


;; No cursor blinking, it's distracting
(when-fn% 'blink-cursor-mode nil (blink-cursor-mode 0))


;; Full path in title bar
(when-graphic%
  (setq% frame-title-format "%b (%f)"))


;; Ignore ring bell
(setq% ring-bell-function 'ignore)


;; Meta key for Darwin
(when-platform% 'darwin
  (when-var% mac-option-modifier nil
    (unless% (eq 'meta (plist-get mac-option-modifier :ordinary))
      (plist-put mac-option-modifier :ordinary 'meta))))


;; Changes all yes/no questions to y/n type
;; (defalias 'yes-or-no-p 'y-or-n-p)


;; Highlights matching parenthesis
(show-paren-mode 1)


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


;; :edit
(when (*self-env-spec* :get :edit :allowed)
  ;; default `tab-width'
  (setq-default tab-width (*self-env-spec* :get :edit :tab-width))
  ;; `standard-indent'
  (setq-default standard-indent (*self-env-spec* :get :edit :standard-indent))
  ;; default `auto-save-default'
  (setq auto-save-default (*self-env-spec* :get :edit :auto-save-default))
  ;; allow `narrow-to-region'
  (put 'narrow-to-region 'disabled
       (not (*self-env-spec* :get :edit :narrow-to-region))))


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


;; fix: `uniquify' may not be autoloaded on ancient Emacs.
(when-version% > 24
  (when% (fluid-let (byte-compile-warnings nil)
           (require 'uniquify nil t))
    (require 'uniquify)
    (setq uniquify-buffer-name-style 'post-forward-angle-brackets)))


;; Mark thing at point

(defun thing-at-point-bounds-of-string-at-point ()
  "Return the bounds of the double quoted string at point.

Internal function used by `bounds-of-thing-at-point'."
  (save-excursion
    (let ((beg (nth 8 (syntax-ppss))))
      (when beg
        (goto-char beg)
        (forward-sexp)
        (cons (1+  beg) (1- (point)))))))

(put 'string 'bounds-of-thing-at-point
     'thing-at-point-bounds-of-string-at-point)


(eval-when-compile

  (defmacro _mark_thing@_ (begin end)
    "Mark THING at point."
    `(progn
       ,begin
       (set-mark (point))
       ,end)))

(defun mark-symbol@ ()
  "Mark the symbol at point."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (_mark_thing@_ (goto-char (car bounds))
                     (goto-char (cdr bounds))))))

(defun mark-filename@ ()
  "Mark the filename at point."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'filename)))
    (when bounds
      (_mark_thing@_ (goto-char (car bounds))
                     (goto-char (cdr bounds))))))

(defun mark-word@ (&optional n)
  "Mark the word at point.

If prefix N is non nil, mark word away from point then forward
or backword to words boundary."
  (interactive "p")
  (let ((bounds (if current-prefix-arg
                    (cons (point)
                          (save-excursion
                            (forward-word
                             (if (consp current-prefix-arg)
                                 1
                               n))
                            (point)))
                  (bounds-of-thing-at-point 'word))))
    (when bounds
      (_mark_thing@_  (goto-char (car bounds))
                      (goto-char (cdr bounds))))))

(defun mark-line@ (&optional indent)
  "Mark the line at point.

If prefix INDENT is non-nil mark the indent line."
  (interactive "P")
  (_mark_thing@_ (if indent
                     (back-to-indentation)
                   (beginning-of-line))
                 (end-of-line)))

(defun mark-sexp@ (&optional n)
  "Mark the sexp at point.

If prefix N is non nil, mark sexp away from point then forward
or backword to sexps boundary."
  (interactive "p")
  (let ((bounds (if current-prefix-arg
                    (cons (point)
                          (save-excursion
                            (forward-sexp
                             (if (consp current-prefix-arg)
                                 1
                               n))
                            (point)))
                  (bounds-of-thing-at-point 'list))))
    (when bounds
      (_mark_thing@_ (goto-char (car bounds))
                     (goto-char (cdr bounds))))))

(defun mark-defun@ (&optional n)
  "Mark function at point.

If prefix N is non-nil, then forward or backward N functions."
  (interactive "p")
  (let ((bounds (if current-prefix-arg
                    (cons (point)
                          (save-excursion
                            (forward-thing 'defun
                             (if (consp current-prefix-arg)
                                 1
                               n))
                            (point)))
                  (bounds-of-thing-at-point 'defun))))
    (when bounds
      (_mark_thing@_ (goto-char (cdr bounds))
                     (goto-char (car bounds))))))

(defun mark-string@ (&optional arg)
  "Mark unquoted string at point.

If prefix ARG then mark quoted string."
  (interactive "P")
  (let ((bounds (bounds-of-thing-at-point 'string)))
    (when bounds
      (_mark_thing@_ (goto-char (if arg
                                    (1- (car bounds))
                                  (car bounds)))
                     (goto-char (if arg
                                    (+ (cdr bounds) 1)
                                  (cdr bounds)))))))

(unless-fn% 'thing-at-point-bounds-of-list-at-point 'thingatpt
  ;; fix the wrong behavior of (bounds-of-thing-at-point 'list) on
  ;; ancient Emacs.
  (defun thing-at-point-bounds-of-list-at-point ()
    "Return the bounds of the list at point.
  [Internal function used by `bounds-of-thing-at-point'.]"
    (save-excursion
      (let* ((st (parse-partial-sexp (point-min) (point)))
             (beg (or (and (eq 4 (car (syntax-after (point))))
                           (not (nth 8 st))
                           (point))
                      (nth 1 st))))
        (when beg
          (goto-char beg)
          (forward-sexp)
          (cons beg (point))))))

  (put 'list 'bounds-of-thing-at-point
       'thing-at-point-bounds-of-list-at-point))

(unless% (or (get 'defun 'beginning-of-defun)
             (get 'defun 'end-of-defun))
  ;; fix wrong behavior (bounds-of-thing-at-point 'defun) on ancient
  ;; Emacs.
  (put 'defun 'beginning-op 'beginning-of-defun)
  (put 'defun 'end-op       'end-of-defun)
  (put 'defun 'forward-op   'end-of-defun))



 ;; end of Mark thing at point


;; Makes killing/yanking interact with the clipboard
;; See also: http://emacswiki.org/emacs/CopyAndPaste

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


 ;; Makes killing/yanking interact with the clipboard


;; open-*-line fn
;; control indent or not: `open-next-line' and `open-previous-line'.
;; see also: https://www.emacswiki.org/emacs/OpenNextLine

(defun open-next-line (n &optional indent)
  "Move to the next line and then open N lines, like vi's o command.

Optional argument INDENT whether to indent lines.
See also `open-line'."
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
  "Open N lines above the current one, like vi's O command.

Optional argument INDENT whether to indent lines.
See also `open-line' and `split-line'."
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
  (when indent
    (indent-according-to-mode)))


 ;; end of open-*-line


(defun echo-buffer-file-name (&optional arg)
  "Echo the file name of current buffer.

If prefix argument ARG is non-nil then copy function `buffer-file-name' to
kill ring.  If prefix argument ARG is nil then copy
`file-name-nondirectory' of function `buffer-file-name' to kill ring."
  (interactive "P")
  (let* ((n (if (or (eq 'dired-mode major-mode)
                    (eq 'ibuffer-mode major-mode))
                (user-error* "Type \"w\" or \"C-u 0 w\" instead, in %s"
                  mode-name)
              (let ((b (buffer-file-name)))
                (if b b (user-error* "Not a file buffer")))))
         (n1 (if arg
                 n
               (file-name-nondirectory n))))
    (kill-new n1)
    (message "%s" n1)))


 ;; end of `echo-buffer-file-name'


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


(defun get-buffer-coding-system (&optional buffer)
  "Return the coding system of current buffer or BUFFER."
  (interactive)
  (with-current-buffer (or buffer
                           (current-buffer))
    (if (called-interactively-p*)
        (message "%s" buffer-file-coding-system)
      buffer-file-coding-system)))


 ;; end of `current-buffer-coding-system'


(unless-fn% 'count-words-region 'simple
  (defalias 'count-words-region #'count-lines-region
    "`count-lines-region' had been obsoleted since Emacs24.1+"))


(define-key (current-global-map) (kbd "C-x C-v") #'view-file)
;; (define-key (current-global-map) (kbd "C-x C-F") #'ido-find-alternate-file)

 ;; end of ido


(defun multi-occur-in-matching-major-mode (&optional mode)
  "Show all lines matching REGEXP in buffers specified by `major-mode'.

See also: `multi-occur-in-matching-buffers'.
Optional argument MODE `major-mode'."
  (interactive
   (list (read-from-minibuffer
          "List lines in buffers whose major-mode match regexp: "
          (symbol-name major-mode))))
  (multi-occur
   (remove-if* (lambda (buffer)
                 (or (null buffer)
                     (with-current-buffer buffer
                       (unless (string= mode (symbol-name major-mode))
                         buffer))))
               (buffer-list))
   (car (occur-read-primary-args))))


 ;; end of Sorting


;;; Kill word/symbol/line

(defun kill-whole-word (&optional n)
  "Kill current word.

With prefix N, do it N times forward if positive, or move
backwards N times if negative."
  (interactive "p")
  (kill-region (goto-char
                (let ((b (bounds-of-thing-at-point 'word)))
                  (unless b
                    (save-excursion
                      (forward-word (if (>= n 0) 1 -1)))
                    (setq b (bounds-of-thing-at-point 'word))
                    (unless b (user-error* "%s" "No word found")))
                  (if (>= n 0) (car b) (cdr b))))
               (progn
                 (forward-word n)
                 (point))))


(defun kill-whole-symbol (&optional n)
  "Kill current symbol.

With prefix N, do it N times forward if positive, or move
backwards N times if negative."
  (interactive "p")
  (kill-region (goto-char
                (let ((b (bounds-of-thing-at-point 'symbol)))
                  (unless b
                    (save-excursion
                      (forward-symbol (if (>= n 0) 1 -1)))
                    (setq b (bounds-of-thing-at-point 'symbol))
                    (unless b (user-error* "%s" "No symbol found")))
                  (if (>= n 0) (car b) (cdr b))))
               (progn
                 (forward-symbol n)
                 (point))))


 ;; end of kill symbol/word/line


;; Comment
(defun toggle-comment (&optional n)
  "Toggle N lines' comment on current line or region."
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




;;;;
;; Keys
;;;;

;; Buffer
(define-key% (current-global-map) (kbd "C-c b n") #'echo-buffer-file-name)
(define-key% (current-global-map) (kbd "C-x RET =") #'get-buffer-coding-system)

;; Kill
(define-key% (current-global-map) (kbd "C-o") #'open-next-line)
(define-key% (current-global-map) (kbd "C-M-o") #'open-previous-line)

(define-key% (current-global-map) (kbd "C-x M-d") #'kill-whole-word)
(define-key% (current-global-map) (kbd "C-x M-s") #'kill-whole-symbol)
(define-key% (current-global-map) (kbd "C-x M-DEL") #'kill-whole-line)

;; Mark
(define-key% (current-global-map) (kbd "C-c m s") #'mark-symbol@)
(define-key% (current-global-map) (kbd "C-c m f") #'mark-filename@)
(define-key% (current-global-map) (kbd "C-c m l") #'mark-line@)
(define-key% (current-global-map) (kbd "C-c m q") #'mark-string@)
(define-key% (current-global-map) (kbd "M-@") #'mark-word@)
(define-key% (current-global-map) (kbd "C-M-@") #'mark-sexp@)
(define-key% (current-global-map) (kbd "C-M-SPC") #'mark-sexp@)
(define-key% (current-global-map) (kbd "C-M-h") #'mark-defun@)

;; comment
(define-key% (current-global-map) (kbd "C-x M-;") #'toggle-comment)




(provide 'on-edit-autoload)

;;; on-edit-autoload.el ends here
