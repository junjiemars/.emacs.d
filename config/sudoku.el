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


(defalias '*sudoku-puzzle-current*
  (lexical-let% ((b (v-home% ".sudoku/puzzle.el"))
                 (v))
    (lambda (&optional k i1 i2 n)
      (cond ((eq :file k) b)
            ((eq :set! k) (setq v i1))
            ((eq :row k) (let ((mr (* i1 9)))
                           (vector (aref v (+ mr 0))
                                   (aref v (+ mr 1))
                                   (aref v (+ mr 2))
                                   (aref v (+ mr 3))
                                   (aref v (+ mr 4))
                                   (aref v (+ mr 5))
                                   (aref v (+ mr 6))
                                   (aref v (+ mr 7))
                                   (aref v (+ mr 8)))))
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
  )


(defun sudoku-puzzle-save ()
  "Save sudoku's puzzle to file."
  (save-sexp-to-file `(*sudoku-puzzle-current*
                       ,(*sudoku-puzzle-current* :puzzle))
                     (*sudoku-puzzle-current* :file)))

(defun sudoku-puzzle-load ()
  "Load sudoku's puzzle from file."
  (load (*sudoku-puzzle-current* :file)))


(defun sudoku-board-make (puzzle)
  "Make sudoku board with PUZZLE."
  )

(defun sudoku-board-new (level)
  "New sudoku at LEVEL."
  (sudoku-board-make (sudoku-puzzle-make level)
                     (sudoku-help-message)))



(defun sudoku (&optional level)
  "Play sudoku in LEVEL."
  (interactive (list (read-string "Sudoku level: "
                                  (car *sudoku-option-history*)
                                  '*sudoku-option-history*)))
  (v-home! ".sudoku/")
  (message "%s" (intern level)))



;;; eof


