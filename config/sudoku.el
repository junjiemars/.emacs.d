;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; sudoku.el
;;;;
;;
;; features:
;; 1*
;; 2*
;;
;;;;
;;
;;;;


(defvar *sudoku-option-history* (list "easy" "medium" "hard")
  "Sudoku option history list.")


(defalias '*sudoku*
  (lexical-let% ((b))
    (lambda (&optional n)
      (cond ((not (null n))
             (setq b (get-buffer-create n)))
            ((or (null b) (not (buffer-live-p b)))
             (setq b (get-buffer-create "*sudoku*")))
            (t b))))
  "The current *sudoku* process buffer.")


(defalias '*sudoku-puzzle*
  (lexical-let% ((b (v-home% ".sudoku/puzzle.el"))
                 (v))
    (lambda (&optional k i1 i2 n)
      (cond ((eq :file k) b)
            ((eq :set! k) (setq v i1))
            ((eq :row k) (let ((row (* (% i1 9) 9)))
                           (vector (aref v (+ row 0))
                                   (aref v (+ row 1))
                                   (aref v (+ row 2))
                                   (aref v (+ row 3))
                                   (aref v (+ row 4))
                                   (aref v (+ row 5))
                                   (aref v (+ row 6))
                                   (aref v (+ row 7))
                                   (aref v (+ row 8)))))
            ((eq :col k) (let ((col (% i1 9)))
                           (vector (aref v (+ col (* 0 9)))
                                   (aref v (+ col (* 1 9)))
                                   (aref v (+ col (* 2 9)))
                                   (aref v (+ col (* 3 9)))
                                   (aref v (+ col (* 4 9)))
                                   (aref v (+ col (* 5 9)))
                                   (aref v (+ col (* 6 9)))
                                   (aref v (+ col (* 7 9)))
                                   (aref v (+ col (* 8 9))))))
            ((eq :sqr k) (let ((row (* (* i1 3) 9))
                               (col (% (* i2 3) 9)))
                           (vector (aref v (+ row col (* 0 9) 0))
                                   (aref v (+ row col (* 0 9) 1))
                                   (aref v (+ row col (* 0 9) 2))
                                   (aref v (+ row col (* 1 9) 0))
                                   (aref v (+ row col (* 1 9) 1))
                                   (aref v (+ row col (* 1 9) 2))
                                   (aref v (+ row col (* 2 9) 0))
                                   (aref v (+ row col (* 2 9) 1))
                                   (aref v (+ row col (* 2 9) 2)))))
            ((eq :cell k) (let ((row (* (% i1 9) 9))
                                (col (% i2 9)))
                            (aref v (+ row col))))
            ((eq :cell! k) (let ((row (* (% i1 9) 9))
                                 (col (% i2 9)))
                             (if (numberp n)
                                 (aset v (+ row col) n)
                               (aref v (+ row col)))))
            (t v))))
  "The `sudoku' puzzle.")


(defun sudoku-puzzle-make (level)
  "Make sudoku puzzle at LEVEL."
  [0 7 0 2 1 8 4 0 6
   0 0 0 5 0 4 0 0 0
   2 0 0 0 0 0 0 9 5
   4 0 8 6 5 0 3 0 7
   0 0 7 0 0 0 6 0 0
   6 0 1 0 8 7 2 0 9
   7 6 0 0 0 0 0 0 4
   0 0 0 4 0 6 0 0 0
   1 0 5 8 2 9 0 6 0])


(defun sudoku-puzzle-save ()
  "Save sudoku's puzzle to file."
  (save-sexp-to-file `(*sudoku-puzzle* :set!
                                       ,(*sudoku-puzzle*))
                     (*sudoku-puzzle* :file)))

(defun sudoku-puzzle-load ()
  "Load sudoku's puzzle from file."
  (load (*sudoku-puzzle* :file)))


(defalias '*sudoku-board*
  (lexical-let% ((o) (d) (p))
    (lambda (&optional k c1 c2)
      (cond ((eq :right k) (when (< (cdr p) (cdr d))
                             (with-current-buffer (*sudoku*)
                               (let ((n 1))
                                 (forward-char)
                                 (while (not (plist-get
                                              (text-properties-at (point))
                                              :puzzle))
                                   (forward-char)
                                   (setq n (1+ n)))
                                 (setq p (cons (car p) (+ (cdr p) n)))))))
            ((eq :left k) (when (> (cdr p) (cdr o))
                            (with-current-buffer (*sudoku*)
                              (let ((n 1))
                                (backward-char)
                                (while (not (plist-get
                                             (text-properties-at (point))
                                             :puzzle))
                                  (backward-char)
                                  (setq n (1+ n)))
                                (setq p (cons (car p) (- (cdr p) n)))))))
            ((eq :down k) (when (< (car p) (car d))
                            (with-current-buffer (*sudoku*)
                              (let ((n 0))
                                (next-line 1)
                                (while (not (plist-get
                                             (text-properties-at (point))
                                             :puzzle))
                                  (next-line 1)
                                  (setq n (1+ n)))
                                (setq p (cons (+ (car p) n) (cdr p)))))))
            ((eq :up k) (when (> (car p) (car o))
                          (with-current-buffer (*sudoku*)
                            (let ((n 1))
                              (previous-line 1)
                              (while (not (plist-get
                                           (text-properties-at (point))
                                           :puzzle))
                                (previous-line 1)
                                (setq n (1+ n)))
                              (setq p (cons (- (car p) n) (cdr p)))))))
            ((eq :pos! k) (with-current-buffer (*sudoku*)
                            (goto-char (point-min))
                            (forward-line (1- (car c1)))
                            (forward-char (cdr c1))
                            (setq p c1)))
            ((eq :cor! k) (setq o c1 d c2 p c1))
            ((eq :cor k) (list o d))
            (t p))))
  "The `sudoku' board.")


(defun sudoku-board-make (puzzle)
  "Make sudoku board with PUZZLE."
  (with-current-buffer (*sudoku*)
    (setq buffer-read-only nil)
    (erase-buffer)
    (goto-char 0)
    (let ((s "+---------+---------+---------+\n")
          (v "| %s  %s  %s | %s  %s  %s | %s  %s  %s |\n")
          (u "_")
          (row 0))
      (while (< row 9)
        (when (= 0 (% row 3))
          (insert s))
        (insert (apply #'format v
                       (mapcar
                        #'(lambda (x)
                            (propertize
                             (cond ((= x 0) u)
                                   (t (number-to-string x)))
                             :puzzle x))
                        (append (*sudoku-puzzle* :row row) nil))))
        (setq row (1+ row)))
      (insert s)))
  (*sudoku-board* :cor! (cons 2 2) (cons 12 28))
  (*sudoku-board* :pos! (cons 12 2))
  (switch-to-buffer (*sudoku*)))




(defun sudoku-board-move ())


(defvar sudoku-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "\C-p" nil)))

(defun sudoku-mode ()
  "Play `sudoku'."
  (kill-all-local-variables)
  (use-local-map sudoku-mode-map)
  (setq buffer-read-only t)
  (buffer-disable-undo))

(defun sudoku (&optional level)
  "Play sudoku in LEVEL."
  (interactive
   (list (let ((exists (and (file-exists-p
                             (*sudoku-puzzle* :file))
                            (null current-prefix-arg))))
           (read-string (format "Sudoku %s: "
                                (if exists "load" "level"))
                        (if exists
                            (*sudoku-puzzle* :file)
                          (car *sudoku-option-history*))
                        (if exists
                            nil
                          '*sudoku-option-history*)))))
  (v-home! ".sudoku/")
  (if (file-exists-p level)
      (sudoku-puzzle-load)
    (*sudoku-puzzle* :set! (sudoku-puzzle-make (intern level))))
  (sudoku-board-make (*sudoku-puzzle*)))


;;; eof


