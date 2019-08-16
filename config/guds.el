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
      (overlay-put overlay 'gud-breakpoint t)
      (overlay-put overlay 'before-string string))))


(defun gud-line-positions (line)
  "Return a pair of LINE beginning and end positions."
  (let ((offset (1+ (- line (line-number-at-pos)))))
    (cons
     (line-beginning-position offset)
     (line-end-position offset))))


(defun gud-remove-strings ()
  "Remove strings between `gud-line-positions' in current buffer.
Remove only strings that were put in `current-buffer' with calls
to `gud-put-string'."
  (let ((pos (gud-line-positions (line-number-at-pos))))
    (dolist* (o (overlays-in (1- (car pos)) (1+ (cdr pos))))
      (when (or (overlay-get o 'gud-breakpoint)
                (overlay-get o 'before-string))
        (delete-overlay o)))))


(defun gud-breakpoint-p ()
  "Return t if 'gud-breakpoint had been set, otherwise nil."
  (let ((pos (gud-line-positions (line-number-at-pos))))
    (catch 'gud-breakpoint
      (dolist* (o (overlays-in (1- (car pos)) (1+ (cdr pos))))
        (when (overlay-get o 'gud-breakpoint)
          (throw 'gud-breakpoint t))))))


(defun gud-toggle-breakpoint-notation ()
  "Toggle the notation of current breakpoint."
  (let ((enabled (gud-breakpoint-p)))
    (if enabled
        (gud-remove-strings)
      (when (< left-margin-width 2)
        (save-current-buffer
          (setq left-margin-width 2)
          (let ((source-window (get-buffer-window (current-buffer) 0)))
            (when source-window
              (set-window-margins source-window
                                  left-margin-width
                                  right-margin-width)))))
      (gud-put-string (propertize "B"
                                  'face (if (not enabled)
                                            'gud-breakpoint-enabled
                                          'gud-breakpoint-disabled))
                      (car (gud-line-positions (line-number-at-pos)))))
    (not enabled)))





(provide 'guds)


