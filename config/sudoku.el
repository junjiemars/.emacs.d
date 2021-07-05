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
            ((eq :row k) (let ((row (* i1 9)))
                           (vector (aref v (+ row 0))
                                   (aref v (+ row 1))
                                   (aref v (+ row 2))
                                   (aref v (+ row 3))
                                   (aref v (+ row 4))
                                   (aref v (+ row 5))
                                   (aref v (+ row 6))
                                   (aref v (+ row 7))
                                   (aref v (+ row 8)))))
            ((eq :col k) (vector (aref v (+ (* 0 9) i1))
                                 (aref v (+ (* 1 9) i1))
                                 (aref v (+ (* 2 9) i1))
                                 (aref v (+ (* 3 9) i1))
                                 (aref v (+ (* 4 9) i1))
                                 (aref v (+ (* 5 9) i1))
                                 (aref v (+ (* 6 9) i1))
                                 (aref v (+ (* 7 9) i1))
                                 (aref v (+ (* 8 9) i1))))
            ((eq :cell k) (aref v (+ (* i1 9) i2)))
            ((eq :cell! k) (aset v (+ (* i1 9) i2) n))
            (t v))))
  "The `sudoku' current puzzle.")


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
  (save-sexp-to-file `(*sudoku-puzzle*
                       ,(*sudoku-puzzle* :puzzle))
                     (*sudoku-puzzle* :file)))

(defun sudoku-puzzle-load ()
  "Load sudoku's puzzle from file."
  (load (*sudoku-puzzle* :file)))


(defun sudoku-board-make (puzzle)
  "Make sudoku board with PUZZLE."
  puzzle)


(defun sudoku (&optional level)
  "Play sudoku in LEVEL."
  (interactive (list (let ((exists (and (file-exists-p
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
  (sudoku-board-make (if (file-exists-p level)
                         (*sudoku-puzzle*)
                       (sudoku-puzzle-make (intern level)))))



;;; eof


