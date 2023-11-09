;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-mixal-autoload.el
;;;;


(defun mixal*-fix-operation-codes-alist ()
  "Fix some data in `mixal-mode'."
  (when-var% mixal-operation-codes-alist 'mixal-mode
    (let ((aa '((NUM :a 5 :ai 3 :w 0 :wi 4 :l 7)
                (CHAR :a 5 :ai 3 :w 1 :wi 4 :l 7)
                (HLT :a 5 :ai 3 :w 2 :wi 4 :l 7)
                (SLA :a 6 :ai 3 :w 0 :wi 4 :l 7)
                (SRA :a 6 :ai 3 :w 1 :wi 4 :l 7)
                (SLAX :a 6 :ai 3 :w 2 :wi 4 :l 7)
                (SRAX :a 6 :ai 3 :w 3 :wi 4 :l 7)
                (SLC :a 6 :ai 3 :w 4 :wi 4 :l 7)
                (SRC :a 6 :ai 3 :w 5 :wi 4 :l 7)
                (JMP :a 39 :ai 3 :w 0 :wi 4 :l 7)
                (JSJ :a 39 :ai 3 :w 1 :wi 4 :l 7)
                (JOV :a 39 :ai 3 :w 2 :wi 4 :l 7)
                (JNOV :a 39 :ai 3 :w 3 :wi 4 :l 7)
                (JL :a 39 :ai 3 :w 4 :wi 4 :l 7)
                (JE :a 39 :ai 3 :w 5 :wi 4 :l 7)
                (JG :a 39 :ai 3 :w 6 :wi 4 :l 7)
                (JGE :a 39 :ai 3 :w 7 :wi 4 :l 7)
                (JNE :a 39 :ai 3 :w 8 :wi 4 :l 7)
                (JLE :a 39 :ai 3 :w 9 :wi 4 :l 7)
                (JAN :a 40 :ai 3 :w 0 :wi 4 :l 7)
                (JAZ :a 40 :ai 3 :w 1 :wi 4 :l 7)
                (JAP :a 40 :ai 3 :w 2 :wi 4 :l 7)
                (JANN :a 40 :ai 3 :w 3 :wi 4 :l 7)
                (JANZ :a 40 :ai 3 :w 4 :wi 4 :l 7)
                (JANP :a 40 :ai 3 :w 5 :wi 4 :l 7)
                (JXN :a 47 :ai 3 :w 0 :wi 4 :l 7)
                (JXZ :a 47 :ai 3 :w 1 :wi 4 :l 7)
                (JXP :a 47 :ai 3 :w 2 :wi 4 :l 7)
                (JXNN :a 47 :ai 3 :w 3 :wi 4 :l 7)
                (JXNZ :a 47 :ai 3 :w 4 :wi 4 :l 7)
                (JXNP :a 47 :ai 3 :w 5 :wi 4 :l 7)
                (INCA :a 48 :ai 3 :w 0 :wi 4 :l 7)
                (DECA :a 48 :ai 3 :w 1 :wi 4 :l 7)
                (ENTA :a 48 :ai 3 :w 2 :wi 4 :l 7)
                (ENNA :a 48 :ai 3 :w 3 :wi 4 :l 7)
                (INC1 :a 49 :ai 3 :w 0 :wi 4 :l 7)
                (DEC1 :a 49 :ai 3 :w 1 :wi 4 :l 7)
                (ENT1 :a 49 :ai 3 :w 2 :wi 4 :l 7)
                (ENN1 :a 49 :ai 3 :w 3 :wi 4 :l 7)
                (INC2 :a 50 :ai 3 :w 0 :wi 4 :l 7)
                (DEC2 :a 50 :ai 3 :w 1 :wi 4 :l 7)
                (INC3 :a 51 :ai 3 :w 0 :wi 4 :l 7)
                (DEC3 :a 51 :ai 3 :w 1 :wi 4 :l 7)
                (INC4 :a 52 :ai 3 :w 0 :wi 4 :l 7)
                (DEC4 :a 52 :ai 3 :w 1 :wi 4 :l 7)
                (INC5 :a 53 :ai 3 :w 0 :wi 4 :l 7)
                (DEC5 :a 53 :ai 3 :w 1 :wi 4 :l 7)
                (INC6 :a 54 :ai 3 :w 0 :wi 4 :l 7)
                (DEC6 :a 54 :ai 3 :w 1 :wi 4 :l 7)
                (INCX :a 55 :ai 3 :w 0 :wi 4 :l 7)
                (DECX :a 55 :ai 3 :w 1 :wi 4 :l 7))))
      (dolist* (x aa)
        (let* ((a1 (assoc** (car x) mixal-operation-codes-alist))
               (d1 (cdr x))
               (a (plist-get d1 :a))
               (ai (plist-get d1 :ai))
               (w (plist-get d1 :w))
               (wi (plist-get d1 :wi))
               (l (plist-get d1 :l)))
          (when (and (< (length a1) l) (= (nth ai a1) a))
            (setcdr a1 (cdr (seq-ins! w a1 wi)))))))))


(defun mixal*-describe-equipment ()
  "Show help MIX's peripheral devices."
  (interactive)
  (with-output-to-temp-buffer (buffer-name (get-buffer-create "*Help*"))
    (princ "MIX has a fair amount of input-output equipment.

Each device is given a number as follows:")
    (terpri) (terpri)
    (princ "Unit number           Peripheral device               Block size")
    (terpri)
    (princ "      t      Tape unit number t (0 ≤ t ≤ 7)            100 words")
    (terpri)
    (princ "      d      Disk or drum unit number d (8 ≤ d ≤ 15)   100 words")
    (terpri)
    (princ "     16      Card reader                                16 words")
    (terpri)
    (princ "     17      Card punch                                 16 words")
    (terpri)
    (princ "     18      Line printer                               24 words")
    (terpri)
    (princ "     19      Typewriter terminal                        14 words")
    (terpri)
    (princ "     20      Paper tape                                 14 words")
    (terpri)))


(defun mixal*-describe-register ()
  "Show help for MIX's registers."
  (interactive)
  (with-output-to-temp-buffer (buffer-name (get-buffer-create "*Help*"))
    (princ "There are nine registers in MIX.") (terpri) (terpri)
    (princ "                                      [*]:    [+]:") (terpri)
    (princ "  rA = register A                     JL(4) < N(0)") (terpri)
    (princ "  rX = register X                     JE(5) = Z(1)") (terpri)
    (princ " rAX = registers A and X as one       JG(6) > P(2)") (terpri)
    (princ " rIi = index register i, 1 ≤ i ≤ 6   JGE(7) ≥ NN(3)") (terpri)
    (princ "  rJ = register J                    JNE(8) ≠ NZ(4)") (terpri)
    (princ "  CI = comparation indicator         JLE(9) ≤ NP(5)") (terpri)))


(defun mixal*-describe-alphameric ()
  "Show help for MIX's alphameric characters."
  (interactive)
  (with-output-to-temp-buffer (buffer-name (get-buffer-create "*Help*"))
    (let* ((am (vector "⊔" "A" "B" "C" "D" "E" "F" "G" "H" "I"
                       "Θ" "J" "K" "L" "M" "N" "O" "P" "Q" "R"
                       "Φ" "Π" "S" "T" "U" "V" "W" "X" "Y" "Z"
                       "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
                       "." "," "(" ")" "+" "-" "*" "/" "=" "$"
                       "<" ">" "@" ";" ":" "'"))
           (l (length am))
           (h 12) (w 5) (i 0) (j 0))
      (princ "MIX alphameric characters.") (terpri) (terpri)
      (princ " Code Char | Code Char | Code Char | Code Char | Code Char")
			(terpri)
      (while (< i h)
        (while (and (< j w) (< (+ i (* h j)) l))
          (let ((p (+ i (* h j))))
            (princ (format "  %02d    %s  %s"
                           p
                           (aref am p)
                           (if (= j (- w 1)) "" "|"))))
          (setq j (1+ j)))
        (terpri)
        (setq i (1+ i) j 0)))))


;; `mixvm'
(when% (executable-find% "mixvm")
  (compile! (compile-unit% (emacs-home* "config/mixvm.el") t))
  (autoload 'mixvm (v-home%> "config/mixvm.el")
    "Run mixvm on program FILE in buffer *gud-FILE*." t))


(with-eval-after-load 'mixal-mode
  (mixal*-fix-operation-codes-alist)
  (define-key% mixal-mode-map (kbd "C-h C-o") nil)
  (define-key% mixal-mode-map (kbd "C-c C-h o")
    #'mixal-describe-operation-code)
  (define-key% mixal-mode-map (kbd "C-c C-h r") #'mixal*-describe-register)
  (define-key% mixal-mode-map (kbd "C-c C-h a") #'mixal*-describe-alphameric)
  (define-key% mixal-mode-map (kbd "C-c C-h e") #'mixal*-describe-equipment))


;; end of on-mixal-autoload.el
