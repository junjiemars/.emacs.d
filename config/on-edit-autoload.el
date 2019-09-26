;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-edit-autoload.el
;;;;


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


;; no need for ~ files when editing
(setq% create-lockfiles nil)



;; enable upcase/downcase region
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)


;; enable column number mode
(setq% column-number-mode t 'simple)


;; comments
(defun toggle-comment ()
  "Toggle comment on current line or region."
  (interactive)
  (comment-or-uncomment-region
   (region-active-if (region-beginning) (line-beginning-position))
   (region-active-if (region-end) (line-end-position)))
  (if-fn% 'next-logical-line nil
          (next-logical-line)
    (forward-line)))

;; toggle comment key strike
(define-key (current-global-map) (kbd "C-c ;") #'toggle-comment)

;; auto org-mode
(when-version% >= 23
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode)))


;; :edit
(when (self-spec->*env-spec :edit :allowed)
  ;; default tab-width
  (setq-default tab-width (self-spec->*env-spec :edit :tab-width))
  ;; default auto-save-default
  (setq auto-save-default (self-spec->*env-spec :edit :auto-save-default)))


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
  (when% (with-var byte-compile-warnings
           (setq byte-compile-warnings nil)
           (require 'uniquify nil t))
    (require 'uniquify)
    (setq uniquify-buffer-name-style 'post-forward-angle-brackets)))


;; Mark thing at point

(defun thing-at-point-bounds-of-string-at-point ()
  "Return the bounds of the double quoted string at point.
 [Internal function used by `bounds-of-thing-at-point'.]"
  (save-excursion
    (let ((beg (nth 8 (syntax-ppss))))
      (when beg
        (goto-char beg)
        (forward-sexp)
        (cons (1+  beg) (1- (point)))))))

(put 'string 'bounds-of-thing-at-point
     'thing-at-point-bounds-of-string-at-point)


(eval-when-compile
  
  (defmacro _mark-thing@ (begin end)
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
      (_mark-thing@ (goto-char (car bounds))
                    (goto-char (cdr bounds))))))

(defun mark-filename@ ()
  "Mark the filename at point."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'filename)))
    (when bounds
      (_mark-thing@ (goto-char (car bounds))
                    (goto-char (cdr bounds))))))

(defun mark-word@ ()
  "Mark the word at point."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (when bounds
      (_mark-thing@ (goto-char (car bounds))
                    (goto-char (cdr bounds))))))

(defun mark-line@ (&optional arg)
  "Mark the line at point.

If prefix arugment ARG is non-nil should mark the whole line."
  (interactive "P")
  (_mark-thing@ (if arg
                    (back-to-indentation)
                  (beginning-of-line))
                (end-of-line)))

(defun mark-list@ ()
  "Mark the list at point.

More accurate than `mark-sexp'."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'list)))
    (when bounds
      (_mark-thing@ (goto-char (car bounds))
                    (goto-char (cdr bounds))))))

(defun mark-defun@ ()
  "Mark the function at point.

More accurate than `mark-defun'."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'defun)))
    (when bounds
      (_mark-thing@ (goto-char (cdr bounds))
                    (goto-char (car bounds))))))

(defun mark-string@ ()
  "Make the string at point."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'string)))
    (when bounds
      (_mark-thing@ (goto-char (car bounds))
                    (goto-char (cdr bounds))))))

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


(define-key (current-global-map) (kbd "C-c m s") #'mark-symbol@)
(define-key (current-global-map) (kbd "C-c m f") #'mark-filename@)
(define-key (current-global-map) (kbd "C-c m w") #'mark-word@)
(define-key (current-global-map) (kbd "C-c m l") #'mark-line@)
(define-key (current-global-map) (kbd "C-c m a") #'mark-list@)
(define-key (current-global-map) (kbd "C-c m d") #'mark-defun@)
(define-key (current-global-map) (kbd "C-c m q") #'mark-string@)

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
      
      (defmacro _defun-x-select-text* (bin &rest args)
        "Define `x-select-text*'"
        `(defun x-select-text* (text &optional _unused_)
           "Copy TEXT to system clipboard."
           (with-temp-buffer
             (insert text)
             (call-process-region (point-min) (point-max)
                                  ,bin
                                  nil 0 nil
                                  ,@args))))

      (defmacro _defun-x-selection-value* (bin &rest args)
        "Define `x-selection-value*'"
        `(defun x-selection-value* ()
           "Paste from system clipboard."
           (let ((out (shell-command* ,bin ,@args)))
             (when (zerop (car out))
               (cdr out)))))

      (defmacro _enable-x-select-clipboard! ()
        "Enable `x-select-clipboard'"
        `(progn
           (setq interprogram-cut-function #'x-select-text*)
           (setq interprogram-paste-function #'x-selection-value*)))))
  
  (when-platform% 'darwin
    (_defun-x-select-text* "pbcopy")
    (_defun-x-selection-value* "pbpaste")
    (_enable-x-select-clipboard!))

  (when-platform% 'gnu/linux
    (when% (executable-find% "xsel")
      (_defun-x-select-text* "xsel" "--clipboard" "--input")
      (_defun-x-selection-value* "xsel" "--clipboard" "--output")
      (_enable-x-select-clipboard!))))


 ;; Makes killing/yanking interact with the clipboard


;; isearch

(defmacro symbol@ ()
  "Return the symbol at point."
  `(let ((ss (region-active-if
                 (prog1
                     (buffer-substring (region-beginning)
                                       (region-end))
                   (setq mark-active nil))
               (thing-at-point 'symbol))))
     (and (stringp ss) (substring-no-properties ss))))


(defvar isearch-toggle-symbol@*
  (lexical-let% ((toggle))
    (lambda (&optional n)
      (if n (setq toggle (not toggle)) toggle)))
  "Toggle `symbol@' for `isearch-forward*' and `isearch-backward*'.")


(eval-when-compile
  
  (defmacro _defun-isearch-forward-or-backward (direction)
    "Define `isearch-forward*' or `isearch-backward*'"
    (let ((fn (intern (format "isearch-%s*" (symbol-name direction))))
          (dn (intern (format "isearch-%s" (symbol-name direction)))))
      `(defun ,fn (&optional arg)
         ,(format "Do incremental region or symbol search %s."
                  `,(symbol-name direction))
         (interactive "P")
         (,dn nil 1)
         (when (funcall isearch-toggle-symbol@* arg)
           (let ((s (symbol@)))
             (when s (isearch-yank-string s))))))))


(_defun-isearch-forward-or-backward forward)
(_defun-isearch-forward-or-backward backward)

(define-key (current-global-map) (kbd "C-s") #'isearch-forward*)
(define-key (current-global-map) (kbd "C-r") #'isearch-backward*)


 ;; end of isearch


;; hippie

(with-eval-after-load 'hippie-exp
  (setq% hippie-expand-try-functions-list
         '(try-complete-file-name-partially
           try-complete-file-name
           try-expand-all-abbrevs
           ;; try-expand-list
           ;; try-expand-line
           try-expand-dabbrev
           try-expand-dabbrev-all-buffers
           try-expand-dabbrev-from-kill
           try-complete-lisp-symbol-partially
           try-complete-lisp-symbol)
         'hippie-exp))


;; Key binding to use "hippie expand" for text autocompletion
;; See also: http://www.emacswiki.org/emacs/HippieExpand
(define-key (current-global-map) (kbd "M-/") #'hippie-expand)

 ;; end of hippie


;; open-*-line fn
;; control indent or not: `open-next-line' and `open-previous-line'.
;; see also: https://www.emacswiki.org/emacs/OpenNextLine

(defun open-next-line (n)
  "Move to the next line and then open N lines, like vi's o command.

See also `open-line'."
  (interactive "*p")
  (end-of-line)
  (open-line n)
  (forward-line 1)
  (indent-according-to-mode))

(defun open-previous-line (n)
  "Open N lines above the current one, like vi's O command.

See also `open-line' and `split-line'."
  (interactive "*p")
  (beginning-of-line)
  (open-line n)
  (indent-according-to-mode))

(defun toggle-open-line-indent! (&optional arg)
  "Toggle whether indent or not when `open-line' or `split-line'.

With prefix argument ARG, indent as default when ARG is non-nil."
  (interactive "P")
  (let ((indent? (cond ((or arg
                            (eq 'open-line
                                (lookup-key
                                 (current-global-map) (kbd "C-o"))))
                        (cons #'open-next-line #'open-previous-line))
                       (t (cons #'open-line #'split-line)))))
    (define-key (current-global-map) (kbd "C-o") (car indent?))
    (define-key (current-global-map) (kbd "C-M-o") (cdr indent?))
    (message "indent open-line %s"
             (if (eq #'open-line (car indent?))
                 "disabled"
               "enabled"))))

(toggle-open-line-indent! t)

 ;; end of open-*-line


;; tramp
(with-eval-after-load 'tramp
  (when% (executable-find% "ssh")
    ;; ssh faster than scp on ancient Emacs?
    (setq% tramp-default-method "ssh")))


(defun echo-buffer-file-name (&optional arg)
  "Echo the file name of current buffer.

If prefix argument ARG is positive then copy `buffer-file-name'
to kill ring. If prefix argument ARG is negative then copy the
directory name of `buffer-file-name' to kill ring."
  (interactive "p")
  (let ((n (if (eq 'dired-mode major-mode)
               default-directory
             (buffer-file-name))))
    (message "%s" n)
    (when (and n arg)
      (kill-new (if (> arg 0) n (file-name-directory n))))))

(define-key (current-global-map) (kbd "C-c n") #'echo-buffer-file-name)

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

      (define-key (current-global-map) (kbd "C-x 8 RET") #'insert-char*)))

 ;; end of `insert-char*'


(unless-fn% 'count-words-region 'simple
  (defalias 'count-words-region #'count-lines-region
    "`count-lines-region' had been obsoleted since Emacs24.1+"))


;; ido-mode allows you to more easily navigate choices. For example,
;; when you want to switch buffers, ido presents you with a list
;; of buffers in the the mini-buffer. As you start to type a buffer's
;; name, ido will narrow down the list of buffers to match the text
;; you've typed in
;; http://www.emacswiki.org/emacs/InteractivelyDoThings
(make-thread* (ido-mode t))


(defun multi-occur-in-matching-major-mode (&optional mode)
  "Show all lines matching REGEXP in buffers specified by `major-mode'."
  (interactive
   (list (read-from-minibuffer
          "List lines in buffers whose major-mode match regexp: "
          (symbol-name major-mode))))
  (multi-occur (remove-if* (lambda (buffer)
                             (with-current-buffer buffer
                               (unless (string= mode (symbol-name major-mode))
                                 buffer)))
                   (buffer-list))
               (car (occur-read-primary-args))))




;; Encode/Decode url, base64

(eval-when-compile

  (defmacro _encode/decode-url* (encode? &optional coding-system)
    "Encode/Decode region into *encode/decode-url-output* buffer."
    (let ((s (gensym*)))
      `(let ((,s (string-trim>< (region-active-if
                                    (buffer-substring (region-beginning)
                                                      (region-end))))))
         (with-current-buffer
             (switch-to-buffer-other-window
              (if ,encode?
                  "*encode-url-output*"
                "*decode-url-output*"))
           (delete-region (point-min) (point-max))
           (insert (if ,encode?
                       (if-version% < 23
                                    (url-hexify-string ,s nil)
                         (url-hexify-string ,s))
                     (decode-coding-string (url-unhex-string ,s)
                                           (or ,coding-system 'utf-8))))))))

  (defmacro _encode-base64* (encode &optional coding-system)
    "Encode region with base64 into *encode-base64-output* buffer."
    (let ((s (gensym*))
          (c (gensym*)))
      `(let ((,s (string-trim>< (region-active-if
                                    (buffer-substring (region-beginning)
                                                      (region-end)))))
             (,c (or ,coding-system 'utf-8)))
         (with-current-buffer
             (switch-to-buffer-other-window
              (if ,encode
                  "*encode-base64-output*"
                "*decode-base64-output*"))
           (delete-region (point-min) (point-max))
           (insert (if ,encode
                       (base64-encode-string
                        (if (multibyte-string-p ,s)
                            (encode-coding-string ,s ,c)
                          ,s))
                     (decode-coding-string
                      (base64-decode-string ,s) ,c))))))))

(defun encode-url* ()
  "Encode region into *encode-url-output* buffer."
  (interactive)
  (_encode/decode-url* t))

(defun decode-url* (&optional coding-system)
  "Decode region into *encode-url-output* buffer."
  (interactive "zCoding system for decode (default utf-8): ")
  (_encode/decode-url* nil coding-system))

(defun encode-base64* (&optional coding-system)
  "Encode region with base64 into *encode-base64-output* buffer."
  (interactive "zCoding system for encode (default utf-8): ")
  (_encode-base64* t coding-system))

(defun decode-base64* (&optional coding-system)
  "Decode region with base64 into *decode-base64-output* buffer."
  (interactive "zCoding system for decode (default utf-8): ")
  (_encode-base64* nil coding-system))




;; end of file
