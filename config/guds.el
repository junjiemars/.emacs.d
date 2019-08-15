;;;; -*- lexical-binding:t -*-
;;;;
;; guds.el
;;;;
;;
;;;;

;;;;
;; require
;;;;


(require 'gud)



;;;;
;;  variables                                       ;
;;;;


(defface gud-breakpoint-enabled
  '((t
     :foreground "red1"
     :weight bold))
  "Face for enabled breakpoint icon in fringe."
  :group 'gud)


(defface gud-breakpoint-disabled
  '((((class color) (min-colors 88)) :foreground "grey70")
    ;; Ensure that on low-color displays that we end up something visible.
    (((class color) (min-colors 8) (background light))
     :foreground "black")
    (((class color) (min-colors 8) (background dark))
     :foreground "white")
    (((type tty) (class mono))
     :inverse-video t)
    (t :background "gray"))
  "Face for disabled breakpoint icon in fringe."
  :group 'gud)



(defun gud-put-string (putstring pos &optional dprop &rest sprops)
  "Put string PUTSTRING in front of POS in the current buffer.
PUTSTRING is displayed by putting an overlay into the current buffer with a
`before-string' string that has a `display' property whose value is
PUTSTRING."
  (let ((string (make-string 1 ?x))
        (buffer (current-buffer)))
    (setq putstring (copy-sequence putstring))
    (let ((overlay (make-overlay pos pos buffer))
          (prop (or dprop
                    (list (list 'margin 'left-margin) putstring))))
      (put-text-property 0 1 'display prop string)
      (if sprops
          (add-text-properties 0 1 sprops string))
      (overlay-put overlay 'put-break t)
      (overlay-put overlay 'before-string string))))


(defun gud-remove-strings (start end &optional buffer)
  "Remove strings between START and END in BUFFER.
Remove only strings that were put in BUFFER with calls to `gdb-put-string'.
BUFFER nil or omitted means use the current buffer."
  (unless buffer
    (setq buffer (current-buffer)))
  (dolist (overlay (overlays-in start end))
    (when (overlay-get overlay 'put-break)
      (delete-overlay overlay))))


(defun gud-remove-breakpoint-icons (start end &optional remove-margin)
  ""
  (gud-remove-strings start end)
  (if (display-images-p)
      (remove-images start end))
  (when remove-margin
    (setq left-margin-width 0)
    (let ((window (get-buffer-window (current-buffer) 0)))
      (if window
          (set-window-margins
           window left-margin-width right-margin-width)))))


(defun gud-line-positions (line)
  "Return a pair of LINE beginning and end positions."
  (let ((offset (1+ (- line (line-number-at-pos)))))
    (cons
     (line-beginning-position offset)
     (line-end-position offset))))


(defun gud-put-breakpoint-icon (enabled bptno &optional line)
  ""
  (let ((posns (gud-line-positions (or line (line-number-at-pos))))
        (source-window (get-buffer-window (current-buffer) 0)))
    (gud-remove-breakpoint-icons (- (car posns) 1)
                                 (+ (cdr posns) 1))
    (when (< left-margin-width 2)
      (save-current-buffer
        (setq left-margin-width 2)
        (when source-window
          (set-window-margins source-window
                              left-margin-width
                              right-margin-width))))
    (gud-put-string (propertize (if enabled "B" "b")
                                'face (if enabled
                                          'gud-breakpoint-enabled
                                        'gud-breakpoint-disabled))
                    (+ start 1))))





(provide 'guds)


