;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; guds.el
;;;;
;;
;; common gud vars and fns for `gud-cdb' and `gud-lldb'.
;;;;




(require 'gud)



;;;;
;;  variables                                       ;
;;;;


;; (defface gud-breakpoint-enabled
;;   '((t
;;      :foreground "red1"
;;      :weight bold))
;;   "Face for enabled breakpoint icon in fringe."
;;   :group 'gud)


;;;;
;; functions
;;;;


(defmacro gud*-def (key fn)
  "Define KEY bind to FN.

Avoid bugs in `gud-format-command' and `gud-find-c-expr'."
  `(progn
     ,(if key `(local-set-key ,(concat "\C-c" key) ,fn))
     ,(if key `(global-set-key (vconcat gud-key-prefix ,key) ,fn))))


(defun c-last-expr ()
  "Return the position of left side of the last C expression."
  (save-excursion
    (catch 'break
      (let ((ori (point)))
        (while (not (or (bobp)
                        (char-equal (char-before) ?\;)
                        (char-equal (char-before) ?\n)))
          (cond
           ;; right parenthesis
           ((char-equal (char-before) ?\))
            (backward-list))
           ;; word
           ((char-equal (char-syntax (char-before)) ?w)
            (let ((cur (point))
                  (idx 1))
              (when (<= (- cur idx) (point-min))
                (throw 'break (point-min)))
              (while (and (char-before (- cur idx))
                          (char-equal (char-syntax (char-before (- cur idx))) ?w))
                (setq idx (1+ idx)))
              (when (and (> idx 2)
                         (string-match
                          "var"
                          (buffer-substring-no-properties
                           (- cur idx) cur)))
                (throw 'break cur))
              (backward-word)))
           ;; whitespace
           ((char-equal (char-syntax (char-before)) ?\ )
            (while (char-equal (char-syntax (char-before)) ?\ )
              (backward-char)))
           ;; comma
           ((char-equal (char-before) ?,)
            (throw 'break (point)))
           ;; assignment
           ((char-equal (char-before) ?=)
            (let ((cur (point))
                  (idx 1))
              (when (<= (- cur idx) (point-min))
                (throw 'break (point-min)))
              (while (or (char-equal (char-before (- cur idx)) ?=)
                         (char-equal (char-before (- cur idx)) ?!))
                (setq idx (1+ idx)))
              (if (< idx 2)
                  (throw 'break cur)
                (backward-char idx))))
           ;; > >>
           ((char-equal (char-before) ?>)
            (when (string-match
                   "^[> \t]+"
                   (buffer-substring-no-properties
                    (line-beginning-position)
                    (point)))
              (throw 'break (point)))
            (backward-char))
           ;; _
           ((char-equal (char-before) ?_)
            (let ((cur (point))
                  (idx 1))
              (while (and (char-before (- cur idx))
                          (char-equal (char-before (- cur idx)) ?_))
                (setq idx (1+ idx)))
              (when (>= idx 3)
                (throw 'break cur))
              (backward-char)))
           ;; dot
           ((char-equal (char-before) ?.)
            (let ((cur (point))
                  (idx 1))
              (while (and (char-before (- cur idx))
                          (char-equal (char-before (- cur idx)) ?.))
                (setq idx (1+ idx)))
              (when (>= idx 3)
                (throw 'break cur))
              (backward-char)))
           ;; punctuation
           ((char-equal (char-syntax (char-before)) ?.)
            (let ((cur (point)))
              (when (or (= ori cur)
                        (and (< cur ori)
                             (let ((idx 0))
                               (while (and (< (+ cur idx) ori)
                                           (char-equal (char-syntax
                                                        (char-after (+ cur idx)))
                                                       ?\ ))
                                 (setq idx (1+ idx)))
                               (= (+ cur idx) ori))))
                (throw 'break ori))
              (backward-char)))
           ;; default
           (t (throw 'break (point)))))))
    (while (char-equal (char-syntax (char-after)) ?\ )
      (forward-char))
    (point)))


(defun gud*-find-c-last-expr ()
  "Return the C expression that surrounds point."
  (let* ((bs1 (cons (c-last-expr) (point)))
         (bs2 (bounds-of-thing-at-point 'symbol))
         (expr (buffer-substring-no-properties (car bs1) (cdr bs1)))
         (symb (if bs2
                   (buffer-substring-no-properties (car bs2) (cdr bs2))
                 "")))
    (if (> (length symb) (length expr))
        symb
      expr)))


;; (defface gud-breakpoint-disabled
;;   '((((class color) (min-colors 88)) :foreground "grey70")
;;     ;; Ensure that on low-color displays that we end up something visible.
;;     (((class color) (min-colors 8) (background light))
;;      :foreground "black")
;;     (((class color) (min-colors 8) (background dark))
;;      :foreground "white")
;;     (((type tty) (class mono))
;;      :inverse-video t)
;;     (t :background "gray"))
;;   "Face for disabled breakpoint icon in fringe."
;;   :group 'gud)



;; (defun gud-put-string (putstring pos &optional dprop &rest sprops)
;;   "Put string PUTSTRING in front of POS in the current buffer.
;; PUTSTRING is displayed by putting an overlay into the current buffer with a
;; `before-string' string that has a `display' property whose value is
;; PUTSTRING."
;;   (let ((string (make-string 1 ?x))
;;         (buffer (current-buffer)))
;;     (setq putstring (copy-sequence putstring))
;;     (let ((overlay (make-overlay pos pos buffer))
;;           (prop (or dprop
;;                     (list (list 'margin 'left-margin) putstring))))
;;       (put-text-property 0 1 'display prop string)
;;       (if sprops
;;           (add-text-properties 0 1 sprops string))
;;       (overlay-put overlay 'gud-breakpoint t)
;;       (overlay-put overlay 'before-string string))))


;; (defun gud-line-positions (line)
;;   "Return a pair of LINE beginning and end positions."
;;   (let ((offset (1+ (- line (line-number-at-pos)))))
;;     (cons
;;      (line-beginning-position offset)
;;      (line-end-position offset))))


;; (defun gud-remove-strings ()
;;   "Remove strings between `gud-line-positions' in current buffer.
;; Remove only strings that were put in `current-buffer' with calls
;; to `gud-put-string'."
;;   (let ((pos (gud-line-positions (line-number-at-pos))))
;;     (dolist* (o (overlays-in (1- (car pos)) (1+ (cdr pos))))
;;       (when (or (overlay-get o 'gud-breakpoint)
;;                 (overlay-get o 'before-string))
;;         (delete-overlay o)))))


;; (defun gud-breakpoint-p ()
;;   "Return t if 'gud-breakpoint had been set, otherwise nil."
;;   (let ((pos (gud-line-positions (line-number-at-pos))))
;;     (catch 'gud-breakpoint
;;       (dolist* (o (overlays-in (1- (car pos)) (1+ (cdr pos))))
;;         (when (overlay-get o 'gud-breakpoint)
;;           (throw 'gud-breakpoint t))))))


;; (defun gud-toggle-breakpoint-notation ()
;;   "Toggle the notation of current breakpoint."
;;   (let ((enabled (gud-breakpoint-p)))
;;     (if enabled
;;         (gud-remove-strings)
;;       (when (< left-margin-width 2)
;;         (save-current-buffer
;;           (setq left-margin-width 2)
;;           (let ((source-window (get-buffer-window (current-buffer) 0)))
;;             (when source-window
;;               (set-window-margins source-window
;;                                   left-margin-width
;;                                   right-margin-width)))))
;;       (gud-put-string (propertize "B"
;;                                   'face 'gud-breakpoint-enabled)
;;                       (car (gud-line-positions (line-number-at-pos)))))
;;     (not enabled)))





(provide 'guds)
