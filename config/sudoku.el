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
    (lambda (&optional n)
      (cond ((eq :file n) b)
            ((eq :puzzle n) v)
            (t (setq v n)))))
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


