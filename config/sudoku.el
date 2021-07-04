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


(defun sudoku (&optional level)
  "Play sudoku in LEVEL."
  (interactive (list (read-string "Sudoku level: "
                                  (car *sudoku-option-history*)
                                  '*sudoku-option-history*)))
  (message "%s" level))



;;; eof


