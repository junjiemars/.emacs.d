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
;; http://play.websudoku.com/?level=
;;;;


(defalias '*sudoku*
  (lexical-let% ((b))
    (lambda (&optional n)
      (cond ((not (null n))
             (setq b (get-buffer-create n)))
            ((or (null b) (not (buffer-live-p b)))
             (setq b (get-buffer-create "*sudoku*")))
            (t b))))
  "The current *sudoku* process buffer.")


(defalias '+sudoku-file+
  (lexical-let% ((d (v-home! ".games/sudoku/"))
                 (p (v-home% ".games/sudoku/puzzle"))
                 (b (v-home% ".games/sudoku/board")))
    (lambda (&optional k)
      (cond ((eq :dir k) d)
            ((eq :puzzle k) p)
            ((eq :board k) b)
            (t b))))
  "The sudoku's files.")


(defvar *sudoku-option-history* (list "easy" "medium" "hard")
  "Sudoku option history list.")


(defalias '*sudoku-color*
  (lexical-let% ((b "black")
                 (g "gray")
                 (w "red"))
    (lambda (&optional k c)
      (cond ((eq :b k) b)
            ((eq :g k) g)
            ((eq :w k) w)
            ((eq :b! k) (setq b c))
            ((eq :g! k) (setq g c))
            ((eq :w! k) (setq w c))
            (t (list n g w)))))
  "The sudoku's colors.")


(defalias '*sudoku-puzzle*
  (lexical-let% ((v))
    (lambda (&optional k d i j n)
      (cond ((eq :set! k) (setq v d))

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
                                          (t (* (* (% i 3) 3) 9))))
                               (col (cond ((eq :1d d)
                                           (* (/ (% (% i 81) 9) 3) 3))
                                          (t (* (% j 3) 3)))))
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
  (ignore* level)
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
  "Save sudoku's puzzle to `+sudoku-file+'."
  (save-sexp-to-file (*sudoku-puzzle*)
                     (+sudoku-file+ :puzzle)))

(defun sudoku-puzzle-load ()
  "Load sudoku's puzzle from `+sudoku-file+'."
  (*sudoku-puzzle* :set! (car (read-from-string
                               (read-str-from-file
                                (+sudoku-file+ :puzzle))))))

(defun sudoku-puzzle-complete (block)
  "Check sudoku's block puzzle is complete."
  (when (and block (= 9 (length block)))
    (= 45 (+ (aref block 0)
             (aref block 1)
             (aref block 2)
             (aref block 3)
             (aref block 4)
             (aref block 5)
             (aref block 6)
             (aref block 7)
             (aref block 8)))))

(defun sudoku-puzzle-unique (block)
  "Check sudoku's block puzzle is unique."
  (catch 'conflict
    (when (and block (= 9 (length block)))
      (let ((i 0)
            (j 0))
        (while (< i 9)
          (setq j (+ i 1))
          (while (< j 9)
            (when (and (/= 0 (aref block i))
                       (= (aref block i) (aref block j)))
              (throw 'conflict nil))
            (setq j (1+ j)))
          (setq i (1+ i)
                j (+ i 1)))
        t))))

(defun sudoku-puzzle-row-complete (index)
  "Check sudokus' row puzzle at INDEX is complete."
  (catch 'conflict
    (let ((r (*sudoku-puzzle* :row :1d index)))
      (unless (sudoku-puzzle-complete r)
        (throw 'conflict :conflict)))))

(defun sudoku-puzzle-row-unique (index)
  "Check sudokus' row puzzle at INDEX is unique."
  (catch 'conflict
    (let ((r (*sudoku-puzzle* :row :1d index)))
      (unless (sudoku-puzzle-unique r)
        (throw 'conflict :conflict)))))


(defun sudoku-puzzle-col-complete (index)
  "Check sudoku's column puzzle at INDEX is complete."
  (catch 'conflict
    (let ((c (*sudoku-puzzle* :col :1d index)))
      (unless (sudoku-puzzle-complete c)
        (throw 'conflict :conflict)))))

(defun sudoku-puzzle-col-unique (index)
  "Check sudoku's column puzzle at INDEX is unique."
  (catch 'conflict
    (let ((c (*sudoku-puzzle* :col :1d index)))
      (unless (sudoku-puzzle-unique c)
        (throw 'conflict :conflict)))))


(defun sudoku-puzzle-sqr-complete (index)
  "Check sudoku's square puzzle at INDEX is complete."
  (catch 'conflict
    (let ((sqr (*sudoku-puzzle* :sqr :1d index)))
      (unless (sudoku-puzzle-complete (cdr sqr))
        (throw 'conflict :conflict)))))

(defun sudoku-puzzle-sqr-unique (index)
  "Check sudoku's square puzzle at INDEX is unique."
  (catch 'conflict
    (let ((sqr (*sudoku-puzzle* :sqr :1d index)))
      (unless (sudoku-puzzle-unique (cdr sqr))
        (throw 'conflict :conflict)))))


(defalias '*sudoku-board*
  (lexical-let% ((o) (d) (p))
    (lambda (&optional k i j)
      (cond ((eq :cor! k) (setq o i d j p i))
            ((eq :pos k) p)

            ((eq :in k)
             (with-current-buffer (*sudoku*)
               (let ((row (line-number-at-pos))
                     (col (current-column)))
                 (and (>= row (car o)) (<= row (car d))
                      (>= col (cdr o)) (<= col (cdr d))))))
            
            ((or (eq :nex k) (eq :nex! k))
             (with-current-buffer (*sudoku*)
               (let* ((i1 (cond ((> i (car d)) (car d))
                                ((< i (car o)) (car o))
                                (t i)))
                      (j1 (cond ((> j (cdr d)) (cdr d))
                                ((< j (cdr o)) (cdr o))
                                (t j)))
                      (v (cond ((> (- (car p) i1) 0) -1)
                               ((< (- (car p) i1) 0) 1)
                               (t 0)))
                      (h (cond ((> (- (cdr p) j1) 0) -1)
                               ((< (- (cdr p) j1) 0) 1)
                               (t 0)))
                      (v1 0)
                      (h1 0))
                 (when (/= v 0)
                   (setq v1 v)
                   (let ((col (current-column)))
                     (forward-line v)
                     (while (< (current-column) col)
                       (forward-char 1)))
                   (while (not (get-text-property (point)
                                                  :puzzle))
                     (let ((col (current-column)))
                       (forward-line v)
                       (while (< (current-column) col)
                         (forward-char 1)))
                     (setq v1 (+ v1 v))))
                 (when (/= h 0)
                   (setq h1 h)
                   (forward-char h)
                   (while (not (get-text-property (point)
                                                  :puzzle))
                     (forward-char h)
                     (setq h1 (+ h1 h))))
                 (when (eq :nex! k)
                   (setq p (cons (+ (car p) v1)
                                 (+ (cdr p) h1)))))))

            ((or (eq :mov k) (eq :mov! k))
             (with-current-buffer (*sudoku*)
               (let* ((v (- (car p) (cond ((> i (car d)) (car d))
                                          ((< i (car o)) (car o))
                                          (t i))))
                      (h (- (cdr p) (cond ((> j (cdr d)) (cdr d))
                                          ((< j (cdr o)) (cdr o))
                                          (t j)))))
                 (when (/= v 0)
                   (let ((col (current-column)))
                     (forward-line (- v))
                     (while (< (current-column) col)
                       (forward-char 1))))
                 (when (/= h 0)
                   (forward-char (- h)))
                 (when (eq :mov! k)
                   (setq p (cons (+ (- v) (car p))
                                 (+ (- h) (cdr p))))))))

            ((or (eq :ori k) (eq :ori! k))
             (with-current-buffer (*sudoku*)
               (goto-char (point-min))
               (forward-line (1- (car o)))
               (forward-char (cdr o))
               (when (eq :ori! k)
                 (setq p o))))

            ((or (eq :dia k) (eq :dia! k))
             (with-current-buffer (*sudoku*)
               (goto-char (point-min))
               (forward-line (1- (car d)))
               (forward-char (cdr d))
               (when (eq :dia! k)
                 (setq p d))))

            ((eq :prop k)
             (with-current-buffer (*sudoku*)
               (text-properties-at (point))))

            ((eq :props k)
             (with-current-buffer (*sudoku*)
               (let ((ps)
                     (max (1- (point-max)))
                     (n 0))
                 (save-excursion
                   (goto-char (point-min))
                   (while (< n max)
                     (let ((ts (text-properties-at (point))))
                       (when (and ts (plist-get ts :puzzle))
                         (setq ps (append ps (list ts)))))
                     (forward-char 1)
                     (setq n (1+ n))))
                 ps)))

            (t (list :ori o :dia d :pos p)))))
  "The `sudoku' board.")


(defun sudoku-board-make (puzzle)
  "Make sudoku's board with PUZZLE."
  (let ((i 0)
        (bs))
    (while (< i 81)
      (let ((x (aref puzzle i)))
        (setq bs (append bs (list (list :puzzle (cons i x)
                                        :zero (= x 0))))
              i (1+ i))))
    bs))


(defun sudoku-board-draw (board)
  "Draw sudoku BOARD."
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
          (insert
           (apply #'format v
                  (mapcar
                   #'(lambda (x)
                       (prog1 
                           (apply #'propertize
                                  (let ((n (cdr (plist-get x :puzzle))))
                                    (cond ((= n 0) u)
                                          (t (number-to-string n))))
                                  x)
                         (setq idx (1+ idx))))
                   (take 9 board))))
          (setq board (drop 9 board)
                row (1+ row)))
        (insert s))))
  (*sudoku-board* :cor! (cons 2 2) (cons 12 28))
  (*sudoku-board* :ori!))


(defun sudoku-board-move ()
  "Move to board."
  (or (*sudoku-board* :in)
      (not (let* ((pos (*sudoku-board* :pos)))
             (*sudoku-board* :ori!)
             (*sudoku-board* :mov! (car pos) (cdr pos))))))


(defun sudoku-board-move-right ()
  "Move one step right."
  (interactive)
  (when (sudoku-board-move)
    (let ((pos (*sudoku-board* :pos)))
      (*sudoku-board* :nex! (car pos) (1+ (cdr pos))))))

(defun sudoku-board-move-left ()
  "Move one step left."
  (interactive)
  (when (sudoku-board-move)
    (let ((pos (*sudoku-board* :pos)))
      (*sudoku-board* :nex! (car pos) (1- (cdr pos))))))

(defun sudoku-board-move-down ()
  "Move one step down."
  (interactive)
  (when (sudoku-board-move)
    (let ((pos (*sudoku-board* :pos)))
      (*sudoku-board* :nex! (1+ (car pos)) (cdr pos)))))

(defun sudoku-board-move-up ()
  "Move one step up."
  (interactive)
  (when (sudoku-board-move)
    (let ((pos (*sudoku-board* :pos)))
      (*sudoku-board* :nex! (1- (car pos)) (cdr pos)))))

(defun sudoku-board-move-leftmost ()
  "Move to leftmost point."
  (interactive)
  (when (sudoku-board-move)
    (let ((cor (*sudoku-board*)))
      (*sudoku-board* :mov!
                      (car (plist-get cor :pos))
                      (cdr (plist-get cor :ori))))))

(defun sudoku-board-move-rightmost ()
  "Move to rightmost point."
  (interactive)
  (when (sudoku-board-move)
    (let ((cor (*sudoku-board*)))
      (*sudoku-board* :mov!
                      (car (plist-get cor :pos))
                      (cdr (plist-get cor :dia))))))

(defun sudoku-board-move-topmost ()
  "Move to topmost point."
  (interactive)
  (when (sudoku-board-move)
    (let ((cor (*sudoku-board*)))
      (*sudoku-board* :mov!
                      (car (plist-get cor :ori))
                      (cdr (plist-get cor :pos))))))

(defun sudoku-board-move-bottom ()
  "Move to bottom point."
  (interactive)
  (when (sudoku-board-move)
    (let ((cor (*sudoku-board*)))
      (*sudoku-board* :mov!
                      (car (plist-get cor :dia))
                      (cdr (plist-get cor :pos))))))


(defun sudoku-board-input (num &rest properties)
  "Input on sudoku's board with NUM and PROPERTIES."
  (declare (indent 1))
  (catch 'block
    (let ((buffer-read-only nil))
      (with-current-buffer (*sudoku*)
        (let ((c (string-to-char (number-to-string num)))
              (tp (*sudoku-board* :prop))
              (pos (point)))
          (when (plist-get tp :zero)
            (delete-char 1)
            (cond ((char= ?0 c) (insert-char ?_ 1))
                  (t (insert-char c 1)))
            (forward-char -1)
            (set-text-properties pos (1+ pos) tp)

            (dolist* (x properties)
              (put-text-property pos (1+ pos)
                                 (car x)
                                 (cdr x)))

            (let* ((idx (car (plist-get tp :puzzle)))
                   (cell (cons idx num)))
              (put-text-property pos (1+ pos) :puzzle cell)
              (*sudoku-puzzle* :cell! :1d idx (cdr cell))

              (let ((f (list :underline t
                             :foreground (*sudoku-color* :w))))
                (when (eq :conflict (sudoku-puzzle-sqr-unique idx))
                  (put-text-property pos (1+ pos) 'face f)
                  (throw 'block nil))

                (when (eq :conflict (sudoku-puzzle-row-unique idx))
                  (put-text-property pos (1+ pos) 'face f)
                  (throw 'block nil))

                (when (eq :conflict (sudoku-puzzle-col-unique idx))
                  (put-text-property pos (1+ pos) 'face f)
                  (throw 'block nil)))
              
              (put-text-property
               pos (1+ pos)
               'face 'underline))))))))


(defun sudoku-board-input-erase ()
  "Erase at point."
  (interactive)
  (sudoku-board-input 0 (cons 'face nil)))


(defun sudoku-board-input-1 (&optional color)
  "Input 1 at point."
  (interactive)
  (ignore* color)
  (sudoku-board-input 1 (cons 'face 'underline)))

(defun sudoku-board-input-2 (&optional color)
  "Input 2 at point."
  (interactive)
  (ignore* color)
  (sudoku-board-input 2 (cons 'face 'underline)))

(defun sudoku-board-input-3 (&optional color)
  "Input 3 at point."
  (interactive)
  (ignore* color)
  (sudoku-board-input 3 (cons 'face 'underline)))

(defun sudoku-board-input-4 (&optional color)
  "Input 4 at point."
  (interactive)
  (ignore* color)
  (sudoku-board-input 4 (cons 'face 'underline)))

(defun sudoku-board-input-5 (&optional color)
  "Input 5 at point."
  (interactive)
  (ignore* color)
  (sudoku-board-input 5 (cons 'face 'underline)))

(defun sudoku-board-input-6 (&optional color)
  "Input 6 at point."
  (interactive)
  (ignore* color)
  (sudoku-board-input 6 (cons 'face 'underline)))

(defun sudoku-board-input-7 (&optional color)
  "Input 7 at point."
  (interactive)
  (ignore* color)
  (sudoku-board-input 7 (cons 'face 'underline)))

(defun sudoku-board-input-8 (&optional color)
  "Input 8 at point."
  (interactive)
  (ignore* color)
  (sudoku-board-input 8 (cons 'face 'underline)))

(defun sudoku-board-input-9 (&optional color)
  "Input 9 at point."
  (interactive)
  (ignore* color)
  (sudoku-board-input 9 (cons 'face 'underline)))


(defun sudoku-board-disabled-key ()
  "Disabled key."
  (interactive))


(defun sudoku-board-save ()
  "Save sudoku's board to `+sudoku-file+'."
  (let ((b (*sudoku-board* :props)))
    (save-sexp-to-file b (+sudoku-file+ :board))))


(defun sudoku-board-load ()
  "Load sudoku's board from `+sudoku-file+'."
  (let ((b (car (read-from-string
                 (read-str-from-file
                  (+sudoku-file+ :board))))))
    (*sudoku-puzzle*
     :set!
     (let ((i 0)
           (p (make-vector (* 9 9) 0)))
       (dolist* (x b p)
         (aset p i (cdr (plist-get x :puzzle)))
         (setq i (1+ i)))))
    (sudoku-board-draw b)))


(defun sudoku-quit ()
  "Quit `*sudoku*'."
  (interactive)
  (when (and current-prefix-arg
             (yes-or-no-p "Save board? "))
     (sudoku-board-save))
  (kill-buffer (*sudoku*)))


(defvar sudoku-mode-map
  (let ((m (make-sparse-keymap)))

    (define-key m "q" #'sudoku-quit)

    (define-key m [right] #'sudoku-board-move-right)
    (define-key m "\C-f" #'sudoku-board-move-right)
    (define-key m "l" #'sudoku-board-move-right)

    (define-key m [left] #'sudoku-board-move-left)
    (define-key m "\C-b" #'sudoku-board-move-left)
    (define-key m "h" #'sudoku-board-move-left)

    (define-key m [up] #'sudoku-board-move-up)
    (define-key m "\C-p" #'sudoku-board-move-up)
    (define-key m "k" #'sudoku-board-move-up)

    (define-key m [down] #'sudoku-board-move-down)
    (define-key m "\C-n" #'sudoku-board-move-down)
    (define-key m "j" #'sudoku-board-move-down)

    (define-key m [home] #'sudoku-board-move-leftmost)
    (define-key m "\C-a" #'sudoku-board-move-leftmost)
    (define-key m [end] #'sudoku-board-move-rightmost)
    (define-key m "\C-e" #'sudoku-board-move-rightmost)

    (define-key m "\M-<" #'sudoku-board-move-topmost)
    (define-key m "\M->" #'sudoku-board-move-bottom)

    (define-key m "0" #'sudoku-board-input-erase)
    (define-key m "x" #'sudoku-board-input-erase)
    (define-key m "\C-d" #'sudoku-board-input-erase)

    (define-key m "1" #'sudoku-board-input-1)
    (define-key m "2" #'sudoku-board-input-2)
    (define-key m "3" #'sudoku-board-input-3)
    (define-key m "4" #'sudoku-board-input-4)
    (define-key m "5" #'sudoku-board-input-5)
    (define-key m "6" #'sudoku-board-input-6)
    (define-key m "7" #'sudoku-board-input-7)
    (define-key m "8" #'sudoku-board-input-8)
    (define-key m "9" #'sudoku-board-input-9)

    (define-key m "\C-k" #'sudoku-board-disabled-key)
    (define-key m "\C-l" #'sudoku-board-disabled-key)
    (define-key m [mouse-1] #'sudoku-board-disabled-key)
    (define-key m [down-mouse-1] #'sudoku-board-disabled-key)
    (define-key m [drag-mouse-1] #'sudoku-board-disabled-key)
    (define-key m [double-mouse-1] #'sudoku-board-disabled-key)

    ;; (define-key m "\C-h" #'sudoku-board-disabled-key)

    m)
  "The keymap of `sudoku-mode'.")



(defun sudoku-mode ()
  "Switch to sudoku's mode'.

The following commands are available:
\\{sudoku-mode-map}"
  (interactive)
  (with-current-buffer (*sudoku*)
    (kill-all-local-variables)
    (use-local-map sudoku-mode-map)
    (setq major-mode 'sudoku-mode
          mode-name  "Sudoku")
    (setq buffer-read-only t)
    (buffer-disable-undo)))


(defun sudoku (&optional level)
  "Play sudoku in LEVEL."
  (interactive
   (list (let ((exists (and (file-exists-p (+sudoku-file+ :board))
                            (null current-prefix-arg))))
           (read-string (format "Sudoku %s: "
                                (if exists "load" "level"))
                        (if exists
                            (+sudoku-file+ :board)
                          (car *sudoku-option-history*))
                        (if exists
                            nil
                          '*sudoku-option-history*)))))
  (if (file-exists-p level)
      (sudoku-board-load)
    (sudoku-board-draw (sudoku-board-make
                        (*sudoku-puzzle*
                         :set!
                         (sudoku-puzzle-make (intern level))))))
  (sudoku-mode))



(provide 'sudoku)

;;; eof
