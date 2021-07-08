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
  (lexical-let% ((b (v-home% ".games/sudoku/puzzle"))
                 (v))
    (lambda (&optional k d i j n)
      (cond ((eq :file k) b)
            ((eq :set! k) (setq v d))
            ((eq :row k) (let ((row (* (% (cond ((eq :1d d) (/ i 9))
                                                (t i))
                                          9)
                                       9)))
                           (vector (aref v (+ row 0))
                                   (aref v (+ row 1))
                                   (aref v (+ row 2))
                                   (aref v (+ row 3))
                                   (aref v (+ row 4))
                                   (aref v (+ row 5))
                                   (aref v (+ row 6))
                                   (aref v (+ row 7))
                                   (aref v (+ row 8)))))
            ((eq :col k) (let ((col (% (or i d) 9)))
                           (vector (aref v (+ col (* 0 9)))
                                   (aref v (+ col (* 1 9)))
                                   (aref v (+ col (* 2 9)))
                                   (aref v (+ col (* 3 9)))
                                   (aref v (+ col (* 4 9)))
                                   (aref v (+ col (* 5 9)))
                                   (aref v (+ col (* 6 9)))
                                   (aref v (+ col (* 7 9)))
                                   (aref v (+ col (* 8 9))))))
            ((eq :sqr k) (let ((row (cond ((eq :1d d)
                                           (* (/ (/ (% i 81) 9) 3) 3 9))
                                          (t (* (* i 3) 9))))
                               (col (cond ((eq :1d d) (% (* j 3) 9))
                                          (t (* (/ (% (% i 81) 9) 3) 3)))))
                           (cons (cons row col)
                                 (vector
                                  (aref v (+ row col (* 0 9) 0))
                                  (aref v (+ row col (* 0 9) 1))
                                  (aref v (+ row col (* 0 9) 2))
                                  (aref v (+ row col (* 1 9) 0))
                                  (aref v (+ row col (* 1 9) 1))
                                  (aref v (+ row col (* 1 9) 2))
                                  (aref v (+ row col (* 2 9) 0))
                                  (aref v (+ row col (* 2 9) 1))
                                  (aref v (+ row col (* 2 9) 2))))))
            ((eq :cell k) (cond ((eq :1d d) (aref v i))
                                (t (aref v (+ (* (% i 9) 9) j)))))
            ((eq :cell! k) (cond ((eq :1d d) (aset v i j))
                                 (t (aset v (+ (* (% i 9) 9) j) n))))
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
  (save-sexp-to-file (*sudoku-puzzle*)
                     (*sudoku-puzzle* :file)))

(defun sudoku-puzzle-load ()
  "Load sudoku's puzzle from file."
  (*sudoku-puzzle* :set! (car (read-from-string
                               (read-str-from-file
                                (*sudoku-puzzle* :file))))))


(defun sudoku-puzzle-row-validate (index)
  "Validate sudokus' row puzzle at INDEX."
  (catch 'conflict
    (let ((r (*sudoku-puzzle* :row :1d index)))
      (unless (= 45 (+ (aref r 0)
                       (aref r 1)
                       (aref r 2)
                       (aref r 3)
                       (aref r 4)
                       (aref r 5)
                       (aref r 6)
                       (aref r 7)
                       (aref r 8)))
        (throw 'conflict (cons :row index))))))

(defun sudoku-puzzle-col-validate (index)
  "Validate sudoku's column puzzle at INDEX."
  (catch 'conflict
    (let ((c (*sudoku-puzzle* :col :1d index)))
      (unless (= 45 (+ (aref c 0)
                       (aref c 1)
                       (aref c 2)
                       (aref c 3)
                       (aref c 4)
                       (aref c 5)
                       (aref c 6)
                       (aref c 7)
                       (aref c 8)))
        (throw 'conflict (cons :col col))))))

(defun sudoku-puzzle-sqr-validate (index)
  "Validate sudoku's square puzzle at INDEX."
  (catch 'conflict
    (let* ((sqr (*sudoku-puzzle* :sqr :1d index))
           (s (cdr sqr)))
      (unless (= 45 (+ (aref s 0)
                       (aref s 1)
                       (aref s 2)
                       (aref s 3)
                       (aref s 4)
                       (aref s 5)
                       (aref s 6)
                       (aref s 7)
                       (aref s 8)))
        (throw 'conflict (cons :sqr (car sqr)))))))


(defalias '*sudoku-board*
  (lexical-let% ((o) (d) (p))
    (lambda (&optional k c1 c2)
      (cond ((eq :next k) (with-current-buffer (*sudoku*)
                            (let* ((c11 (cons (cond ((> (car c1) (car d))
                                                     (car d))
                                                    ((< (car c1) (car o))
                                                     (car o))
                                                    (t (car c1)))
                                              (cond ((> (cdr c1) (cdr d))
                                                     (cdr d))
                                                    ((< (cdr c1) (cdr o))
                                                     (cdr o))
                                                    (t (cdr c1)))))
                                   (v (cond ((> (- (car p) (car c11)) 0) -1)
                                            ((< (- (car p) (car c11)) 0) 1)
                                            (t 0)))
                                   (h (cond ((> (- (cdr p) (cdr c11)) 0) -1)
                                            ((< (- (cdr p) (cdr c11)) 0) 1)
                                            (t 0)))
                                   (v1 0)
                                   (h1 0))
                              (when (not (= v 0))
                                (setq v1 v)
                                (next-line v)
                                (while (not (get-text-property (point)
                                                               :puzzle))
                                  (next-line v)
                                  (setq v1 (+ v1 v))))
                              (when (not (= h 0))
                                (setq h1 h)
                                (forward-char h)
                                (while (not (get-text-property (point)
                                                               :puzzle))
                                  (forward-char h)
                                  (setq h1 (+ h1 h))))
                              (setq p (cons (+ (car p) v1) (+ (cdr p) h1))))))
            ((eq :mov k) (with-current-buffer (*sudoku*)
                           (let* ((c11 (cons (cond ((> (car c1) (car d))
                                                    (car d))
                                                   ((< (car c1) (car o))
                                                    (car o))
                                                   (t (car c1)))
                                             (cond ((> (cdr c1) (cdr d))
                                                    (cdr d))
                                                   ((< (cdr c1) (cdr o))
                                                    (cdr o))
                                                   (t (cdr c1)))))
                                  (v (- (car p) (car c11)))
                                  (h (- (cdr p) (cdr c11)))
                                  (v1 (abs v))
                                  (h1 (abs h)))
                             (next-line (- v))
                             (forward-char (- h))
                             (setq p (cons (+ (- v) (car p))
                                           (+ (- h) (cdr p)))))))
            ((eq :ori k) (with-current-buffer (*sudoku*)
                           (goto-char (point-min))
                           (next-line (1- (car o)))
                           (forward-char (cdr o))
                           (setq p o)))
            ((eq :dia k) (with-current-buffer (*sudoku*)
                           (goto-char (point-min))
                           (next-line (1- (car d)))
                           (forward-char (cdr d))
                           (setq p d)))
            ((eq :pos! k) (with-current-buffer (*sudoku*)
                            (goto-char (point-min))
                            (next-line (1- (car c1)))
                            (forward-char (cdr c1))
                            (setq p c1)))
            ((eq :cor! k) (setq o c1 d c2 p c1))
            ((eq :cor k) (list o d))
            ((eq :pos k) p)
            (t (list :ori o :dia d :pos p)))))
  "The `sudoku' board.")


(defun sudoku-board-make (puzzle)
  "Make sudoku board with PUZZLE."
  (switch-to-buffer (*sudoku*))
  (with-current-buffer (*sudoku*)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (goto-char 0)
      (let ((s "+---------+---------+---------+\n")
            (v "| %s  %s  %s | %s  %s  %s | %s  %s  %s |\n")
            (u "_")
            (row 0)
            (idx 0))
        (while (< row 9)
          (when (= 0 (% row 3))
            (insert s))
          (insert (apply #'format v
                         (mapcar
                          #'(lambda (x)
                              (let ((ps (propertize
                                         (cond ((= x 0) u)
                                               (t (number-to-string x)))
                                         :puzzle (cons idx x)
                                         :zero (= x 0))))
                                (setq idx (1+ idx))
                                ps))
                          (append (*sudoku-puzzle* :row :2d row) nil))))
          (setq row (1+ row)))
        (insert s))))
  (*sudoku-board* :cor! (cons 2 2) (cons 12 28))
  (*sudoku-board* :ori))


(defun sudoku-board-move-right ()
  "Move one step right."
  (interactive)
  (let ((pos (*sudoku-board* :pos)))
    (*sudoku-board* :next (cons (car pos)
                                (1+ (cdr pos))))))

(defun sudoku-board-move-left ()
  "Move one step left."
  (interactive)
  (let ((pos (*sudoku-board* :pos)))
    (*sudoku-board* :next (cons (car pos)
                                (1- (cdr pos))))))

(defun sudoku-board-move-down ()
  "Move one step down."
  (interactive)
  (let ((pos (*sudoku-board* :pos)))
    (*sudoku-board* :next (cons (1+ (car pos))
                                (cdr pos)))))

(defun sudoku-board-move-up ()
  "Move one step up."
  (interactive)
  (let ((pos (*sudoku-board* :pos)))
    (*sudoku-board* :next (cons (1- (car pos))
                                (cdr pos)))))

(defun sudoku-board-move-leftmost ()
  "Move to leftmost point."
  (interactive)
  (let* ((cor (*sudoku-board*))
         (ori (plist-get cor :ori))
         (pos (plist-get cor :pos)))
    (*sudoku-board* :mov (cons (car pos) (cdr ori)))))

(defun sudoku-board-move-rightmost ()
  "Move to rightmost point."
  (interactive)
  (let* ((cor (*sudoku-board*))
         (dia (plist-get cor :dia))
         (pos (plist-get cor :pos)))
    (*sudoku-board* :mov (cons (car pos) (cdr dia)))))

(defun sudoku-board-move-topmost ()
  "Move to topmost point."
  (interactive)
  (let* ((cor (*sudoku-board*))
         (ori (plist-get cor :ori))
         (pos (plist-get cor :pos)))
    (*sudoku-board* :mov (cons (car ori) (cdr pos)))))

(defun sudoku-board-move-bottom ()
  "Move to bottom point."
  (interactive)
  (let* ((cor (*sudoku-board*))
         (dia (plist-get cor :dia))
         (pos (plist-get cor :pos)))
    (*sudoku-board* :mov (cons (car dia) (cdr pos)))))


(defun sudoku-board-cell-fill (num property)
  "Fill board's cell with NUM and PROPERTY."
  (let ((buffer-read-only nil))
    (with-current-buffer (*sudoku*)
      (let ((c (string-to-char (number-to-string num)))
            (tp (text-properties-at (point)))
            (pos (point)))
        (when (plist-get tp :zero)
          (delete-char 1)
          (cond ((char= ?0 c) (insert-char ?_ 1))
                (t (insert-char c 1)))
          (forward-char -1)
          (set-text-properties pos (1+ pos) tp)
          (put-text-property pos (1+ pos)
                             (car property)
                             (cdr property))
          (let ((cell (cons (car (plist-get tp :puzzle)) num)))
            (put-text-property pos (1+ pos) :puzzle cell)
            (*sudoku-puzzle* :cell! :1d (car cell) (cdr cell))))))))


(defun sudoku-board-cell-erase ()
  "Erase at point."
  (interactive)
  (sudoku-board-cell-fill 0 (cons 'face nil)))

(defun sudoku-board-cell-1 (&optional color)
  "Input 1 at point."
  (interactive)
  (sudoku-board-cell-fill 1 (cons 'face 'underline)))

(defun sudoku-board-cell-2 (&optional color)
  "Input 2 at point."
  (interactive)
  (sudoku-board-cell-fill 2 (cons 'face 'underline)))

(defun sudoku-board-cell-3 (&optional color)
  "Input 3 at point."
  (interactive)
  (sudoku-board-cell-fill 3 (cons 'face 'underline)))

(defun sudoku-board-cell-4 (&optional color)
  "Input 4 at point."
  (interactive)
  (sudoku-board-cell-fill 4 (cons 'face 'underline)))

(defun sudoku-board-cell-5 (&optional color)
  "Input 5 at point."
  (interactive)
  (sudoku-board-cell-fill 5 (cons 'face 'underline)))

(defun sudoku-board-cell-6 (&optional color)
  "Input 6 at point."
  (interactive)
  (sudoku-board-cell-fill 6 (cons 'face 'underline)))

(defun sudoku-board-cell-7 (&optional color)
  "Input 7 at point."
  (interactive)
  (sudoku-board-cell-fill 7 (cons 'face 'underline)))

(defun sudoku-board-cell-8 (&optional color)
  "Input 8 at point."
  (interactive)
  (sudoku-board-cell-fill 8 (cons 'face 'underline)))

(defun sudoku-board-cell-9 (&optional color)
  "Input 9 at point."
  (interactive)
  (sudoku-board-cell-fill 9 (cons 'face 'underline)))

(defun sudoku-board-disabled-key ()
  "Disabled key."
  (interactive))

(defun sudoku-quit ()
  "Quit `sudoku'."
  (interactive)
  (sudoku-puzzle-save)
  (kill-buffer (*sudoku*)))


(defvar sudoku-mode-map
  (let ((m (make-sparse-keymap)))

    (define-key m "q" #'sudoku-quit)

    (define-key m [right] #'sudoku-board-move-right)
    (define-key m "\C-f" #'sudoku-board-move-right)
    (define-key m "l" #'sudoku-board-move-right)
    (define-key m "d" #'sudoku-board-move-right)

    (define-key m [left] #'sudoku-board-move-left)
    (define-key m "\C-b" #'sudoku-board-move-left)
    (define-key m "h" #'sudoku-board-move-left)
    (define-key m "a" #'sudoku-board-move-left)

    (define-key m [up] #'sudoku-board-move-up)
    (define-key m "\C-p" #'sudoku-board-move-up)
    (define-key m "k" #'sudoku-board-move-up)
    (define-key m "w" #'sudoku-board-move-up)

    (define-key m [down] #'sudoku-board-move-down)
    (define-key m "\C-n" #'sudoku-board-move-down)
    (define-key m "j" #'sudoku-board-move-down)
    (define-key m "s" #'sudoku-board-move-down)

    (define-key m [home] #'sudoku-board-move-leftmost)
    (define-key m "\C-a" #'sudoku-board-move-leftmost)
    (define-key m [end] #'sudoku-board-move-rightmost)
    (define-key m "\C-e" #'sudoku-board-move-rightmost)

    (define-key m "\M-<" #'sudoku-board-move-topmost)
    (define-key m "\M->" #'sudoku-board-move-bottom)

    (define-key m "0" #'sudoku-board-cell-erase)
    (define-key m "x" #'sudoku-board-cell-erase)
    (define-key m "\C-d" #'sudoku-board-cell-erase)

    (define-key m "1" #'sudoku-board-cell-1)
    (define-key m "2" #'sudoku-board-cell-2)
    (define-key m "3" #'sudoku-board-cell-3)
    (define-key m "4" #'sudoku-board-cell-4)
    (define-key m "5" #'sudoku-board-cell-5)
    (define-key m "6" #'sudoku-board-cell-6)
    (define-key m "7" #'sudoku-board-cell-7)
    (define-key m "8" #'sudoku-board-cell-8)
    (define-key m "9" #'sudoku-board-cell-9)

    (define-key m "\C-k" #'sudoku-board-disabled-key)
    ;; (define-key m "\C-h" #'sudoku-board-disabled-key)

    m)
  "The keymap of `sudoku-mode'.")





(defun sudoku-mode ()
  "Toggle sudoku's mode'.

The following commands are available:
\\{sudoku-mode-map}"
  :group 'sudoku-mode
  (interactive)
  (with-current-buffer (*sudoku*)
    (kill-all-local-variables)
    (use-local-map sudoku-mode-map)
    (setq major-mode 'sudoku-mode
          mode-name  "Sudoku")
    (setq buffer-read-only t)))


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
  (v-home! ".games/sudoku/")
  (if (file-exists-p level)
      (sudoku-puzzle-load)
    (*sudoku-puzzle* :set! (sudoku-puzzle-make (intern level))))
  (sudoku-board-make (*sudoku-puzzle*))
  (sudoku-mode))


(provide 'sudoku)

;;; eof
