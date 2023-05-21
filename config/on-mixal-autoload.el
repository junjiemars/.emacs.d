;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-mixal-autoload.el
;;;;


(defun mixal*-fix ()
  "Fix some data in `mixal-mode'."
  (when-var% mixal-operation-codes-alist 'mixal-mode
    (let ((aa '((HLT :a 5 :ai 3 :w 2 :wi 4 :l 7)
                (SLA :a 6 :ai 3 :w 0 :wi 4 :l 7)
                (SRA :a 6 :ai 3 :w 1 :wi 4 :l 7)
                (SLAX :a 6 :ai 3 :w 2 :wi 4 :l 7)
                (SRAX :a 6 :ai 3 :w 3 :wi 4 :l 7)
                (SLC :a 6 :ai 3 :w 4 :wi 4 :l 7)
                (SRC :a 6 :ai 3 :w 5 :wi 4 :l 7)
                (INCA :a 48 :ai 3 :w 0 :wi 4 :l 7)
                (INCX :a 55 :ai 3 :w 0 :wi 4 :l 7)
                (INC1 :a 49 :ai 3 :w 0 :wi 4 :l 7)
                (INC2 :a 50 :ai 3 :w 0 :wi 4 :l 7)
                (INC3 :a 51 :ai 3 :w 0 :wi 4 :l 7)
                (INC4 :a 52 :ai 3 :w 0 :wi 4 :l 7)
                (INC5 :a 53 :ai 3 :w 0 :wi 4 :l 7)
                (INC6 :a 54 :ai 3 :w 0 :wi 4 :l 7)
                (DECA :a 48 :ai 3 :w 1 :wi 4 :l 7)
                (DECX :a 55 :ai 3 :w 1 :wi 4 :l 7)
                (DEC1 :a 49 :ai 3 :w 1 :wi 4 :l 7)
                (DEC2 :a 50 :ai 3 :w 1 :wi 4 :l 7)
                (DEC3 :a 51 :ai 3 :w 1 :wi 4 :l 7)
                (DEC4 :a 52 :ai 3 :w 1 :wi 4 :l 7)
                (DEC5 :a 53 :ai 3 :w 1 :wi 4 :l 7)
                (DEC6 :a 54 :ai 3 :w 1 :wi 4 :l 7))))
      (dolist* (x aa)
        (let ((a1 (assoc** (car x) mixal-operation-codes-alist))
              (a (plist-get (cdr x) :a))
              (ai (plist-get (cdr x) :ai))
              (w (plist-get (cdr x) :w))
              (wi (plist-get (cdr x) :wi))
              (l (plist-get (cdr x) :l)))
          (when (and (< (length a1) l) (= (nth ai a1) a))
            (setcdr a1 (cdr (seq-ins! w a1 wi)))))))))


(defun mixal*-describe-equipment ()
  "Display the peripheral devices."
  (interactive)
  (with-output-to-temp-buffer (buffer-name (get-buffer-create "*Help*"))
    (princ "MIX has a fair amount of input-output equipment.

Each device is given a number as follows:")
    (terpri) (terpri)
	  (princ "Unit number          Peripheral device              Block size")
    (terpri)
	  (princ "      t     Tape unit number t (0 ≤ t ≤ 7)           100 words")
    (terpri)
	  (princ "      d     Disk or drum unit number d (8 ≤ d ≤ 15)  100 words")
    (terpri)
    (princ "     16     Card reader                               16 words")
    (terpri)
    (princ "     17     Card punch                                16 words")
    (terpri)
    (princ "     18     Line printer                              16 words")
    (terpri)
    (princ "     19     Typewriter terminal                       16 words")
    (terpri)
    (princ "     20     Paper tape                                16 words")
    (terpri)))


;; `mixvm'
(when% (executable-find% "mixvm")
  (compile! (compile-unit% (emacs-home* "config/mixvm.el") t))
  (autoload 'mixvm (v-home%> "config/mixvm")
    "Run mixvm on program FILE in buffer *gud-FILE*." t))


(with-eval-after-load 'mixal-mode
  (mixal*-fix)
  (define-key% mixal-mode-map (kbd "C-h C-e") #'mixal*-describe-equipment))


;; end of on-mixal-autoload.el
