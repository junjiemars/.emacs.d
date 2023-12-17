;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; sudoku.el
;;;;
;;
;; features:
;; 1. save/load puzzle to/from file.
;; 2. save/load board to/from file.
;; 3. input validation.
;; 4. new/reload.
;; 5* zen mode.
;; 6* step by step solver.
;; 7* auto generate puzzle.
;;
;;;;
;; references:
;; https://pi.math.cornell.edu/~mec/Summer2009/Mahmood/Count.html
;; http://www.gatsby.ucl.ac.uk/~turner/TeaTalks/Sudoku/Bertrand_and_Javis.pdf
;;;;


(defalias '*sudoku*
  (lexical-let% ((b))
    (lambda (&optional n)
      (cond (n (setq b (get-buffer-create n)))
            ((or (null b) (not (buffer-live-p b)))
             (setq b (get-buffer-create "*sudoku*")))
            (t b))))
  "The sudoku's process buffer.")


(defalias '+sudoku-file+
  (lexical-let*% ((d (v-home! ".games/"))
                  (p (concat d "sudoku-puzzle"))
                  (b (concat d "sudoku-board")))
    (lambda (&optional k)
      (cond ((eq :dir k) d)
            ((eq :puzzle k) p)
            ((eq :board k) b))))
  "The sudoku's files.")


(defconst +sudoku-level+ '(easy medium hard sandbox)
  "Sudoku levels.")


(defconst +sudoku-dimension+ '(4x4 9x9)
  "Sudoku dimension.")


(defvar *sudoku-level-history* nil
  "Sudoku level history list.")

(defvar *sudoku-dimension-history* nil
  "Sudoku dimension history list.")


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
            (t (list b g w)))))
  "The sudoku's colors.")


(defalias '*sudoku-idiom*
  (lexical-let% ((s '("a busted clock still be right twice a day"
                      "even a blind pig can find an acorn once in a while"
                      "every dog has its day")))
    (lambda ()
      (nth (random (length s)) s)))
  "The sudoku's idiom.")


(defalias '*sudoku-puzzle-d*
  (lexical-let% ((l) (d) (s) (sum))
    (lambda (&optional k n)
      (cond ((eq :set! k) (let* ((n1 (length n))
                                 (sqr1 (sqrt n1))
                                 (d1 (floor sqr1)))
                            (setq d d1
                                  s (floor (sqrt d1))
                                  sum (apply #'+ (range 1 d1 1))
                                  l n1)))
            ((eq :d k) d)
            ((eq :len k) l)
            ((eq :sqr k) s)
            ((eq :sum k) sum)
            (t (list :len l :d d :sqr s)))))
  "The sudoku's dimensions.")


(defmacro sudoku-puzzle-1d (i j)
  "Transform sudoku's puzzle from 2d(I,J) to 1d."
  (let ((d (gensym*)))
    `(let ((,d (*sudoku-puzzle-d* :d)))
       (+ (* (% ,i ,d) ,d)
          (% ,j ,d)))))

(defmacro sudoku-puzzle-2d (i)
  "Transform sudoku's puzzle from 1d(I) to 2d."
  (let ((d (gensym*))
        (l (gensym*))
        (d2 (gensym*)))
    `(let* ((,d (*sudoku-puzzle-d* :d))
            (,l (*sudoku-puzzle-d* :len))
            (,d2 (/ (% ,i ,l) ,d)))
       (cons ,d2 (% ,i ,d)))))


(defmacro sudoku-puzzle-row (i)
  "Locate row via 1d(I)."
  (let ((d (gensym*)))
    `(let ((,d (*sudoku-puzzle-d* :d)))
       (% (/ ,i ,d) ,d))))

(defmacro sudoku-puzzle-col (i)
  "Locate col via 1d(I)."
  (let ((d (gensym*)))
    `(let ((,d (*sudoku-puzzle-d* :d)))
       (% ,i ,d))))

(defmacro sudoku-puzzle-sqr (i)
  "Locate sqr via 1d(I)."
  (let ((d (gensym*))
        (l (gensym*))
        (s (gensym*)))
    `(let* ((,d (*sudoku-puzzle-d* :d))
            (,l (*sudoku-puzzle-d* :len))
            (,s (*sudoku-puzzle-d* :sqr)))
       (cons (/ (/ (% ,i ,l) ,d) ,s)
             (/ (% (% ,i ,l) ,d) ,s)))))


(defmacro sudoku-puzzle-vec-row (matrix index)
  "Return MATRIX's row vector on INDEX"
  (let ((d (gensym*))
        (m (gensym*))
        (r (gensym*)))
    `(let* ((,d (*sudoku-puzzle-d* :d))
            (,m ,matrix)
            (,r (* (sudoku-puzzle-row ,index) ,d)))
       (apply #'vector (mapcar** (lambda (a)
                                   (aref ,m (+ ,r a)))
                                 (range 0 (1- ,d) 1))))))

(defmacro sudoku-puzzle-vec-col (matrix index)
  "Return MATRIX's col vector on INDEX."
  (let ((d (gensym*))
        (m (gensym*))
        (c (gensym*)))
    `(let* ((,d (*sudoku-puzzle-d* :d))
            (,m ,matrix)
            (,c (% (sudoku-puzzle-col ,index) ,d)))
       (apply #'vector (mapcar** (lambda (x)
                                   (aref ,m (+ ,c (* x ,d))))
                                 (range 0 (1- ,d) 1))))))


(defmacro sudoku-puzzle-vec-sqr (matrix index)
  "Return MATRIX's sqr vector on INDEX."
  (let ((d (gensym*))
        (s (gensym*))
        (m (gensym*))
        (s1 (gensym*))
        (r (gensym*))
        (c (gensym*)))
    `(let* ((,d (*sudoku-puzzle-d* :d))
            (,s (*sudoku-puzzle-d* :sqr))
            (,m ,matrix)
            (,s1 (sudoku-puzzle-sqr ,index))
            (,r (* (car ,s1) ,s ,d))
            (,c (* (cdr ,s1) ,s)))
       (apply #'vector (mapcar** (lambda (x)
                                   (aref ,m (+ ,r (* (/ x ,s) ,d)
                                               ,c (% x ,s))))
                                 (range 0 (1- ,d) 1))))))



(defalias '*sudoku-puzzle*
  (lexical-let% ((v))
    (lambda (&optional k i n)
      (cond ((eq :set! k) (progn (*sudoku-puzzle-d* :set! i)
                                 (setq v i)))
            ((eq :row k) (sudoku-puzzle-vec-row v i))
            ((eq :col k) (sudoku-puzzle-vec-col v i))
            ((eq :sqr k) (sudoku-puzzle-vec-sqr v i))
            ((eq :box k) (aref v (% i (*sudoku-puzzle-d* :len))))
            ((eq :box! k) (aset v (% i (*sudoku-puzzle-d* :len)) n))
            (t v))))
  "The `sudoku' puzzle in 1-dimension vector.")


(defalias '*sudoku-puzzle-make*
  (lexical-let% ((l) (d) (c)
                 (xs (list
                      'easy
                      (list
                       '4x4 [4 0 0 3
                               0 2 1 0
                               0 3 4 0
                               1 0 0 0]
                       '9x9 [0 7 0 2 1 8 4 0 6
                               0 0 0 5 0 4 0 0 0
                               2 0 0 0 0 0 0 9 5
                               4 0 8 6 5 0 3 0 7
                               0 0 7 0 0 0 6 0 0
                               6 0 1 0 8 7 2 0 9
                               7 6 0 0 0 0 0 0 4
                               0 0 0 4 0 6 0 0 0
                               1 0 5 8 2 9 0 6 0])
                      'medium
                      (list
                       '4x4 [0 3 0 4
                               4 0 0 0
                               0 0 0 1
                               0 0 2 0]
                       '9x9 [0 0 0 0 0 2 4 3 1
                               0 0 3 7 0 9 0 2 8
                               0 0 8 0 0 0 6 0 7
                               0 0 5 0 8 1 0 0 9
                               0 0 4 0 2 0 1 0 0
                               9 0 0 3 4 0 7 0 0
                               8 0 2 0 0 0 3 0 0
                               1 3 0 5 0 4 8 0 0
                               7 5 6 2 0 0 0 0 0])
                      'hard
                      (list
                       '4x4 [0 0 0 2
                               2 0 4 0
                               0 3 0 0
                               1 0 0 0]
                       '9x9 [0 7 6 0 4 2 5 0 3
                               0 3 0 0 0 1 0 0 0
                               0 0 0 0 0 5 0 8 0
                               9 0 7 2 0 8 0 3 0
                               5 0 0 0 6 0 0 0 8
                               0 1 0 4 0 7 9 0 6
                               0 9 0 7 0 0 0 0 0
                               0 0 0 5 0 0 0 7 0
                               7 0 2 8 1 0 0 3 4]))))
    (lambda (&optional k level dimension)
      (cond ((eq :new! k)
             (setq c
                   (cond ((eq 'sandbox level)
                          (make-vector (cond ((eq '4x4 dimension)
                                              (* 4 4))
                                             (t (* 9 9)))
                                       0))
                         (t (plist-get
                             (plist-get xs
                                        (setq l (or level l)))
                             (setq d (or dimension d)))))))
            ((eq :rld k) c)
            (t c))))
  "Make sudoku puzzle at LEVEL.")


(defun sudoku-puzzle-save ()
  "Save sudoku's puzzle to \\=`+sudoku-file+\\='."
  (save-sexp-to-file (*sudoku-puzzle*)
                     (+sudoku-file+ :puzzle)))

(defun sudoku-puzzle-load ()
  "Load sudoku's puzzle from \\=`+sudoku-file+\\='."
  (*sudoku-puzzle* :set! (read-sexp-from-file
                          (+sudoku-file+ :puzzle))))


(defmacro sudoku-puzzle-vec-complete (vector)
  "Predicate sudoku puzzle's VECTOR is complete."
  (let ((len (gensym*))
        (sum (gensym*))
        (i (gensym*)))
    `(let ((,len (length ,vector))
           (,sum 0)
           (,i 0))
       (while (< ,i ,len)
         (setq ,sum (+ ,sum (aref ,vector ,i))
               ,i (1+ ,i)))
       (= ,sum (*sudoku-puzzle-d* :sum)))))

(defmacro sudoku-puzzle-vec-unique (vector)
  "Predicate sudoku puzzle's VECTOR is unique."
  (let ((len (gensym*))
        (i (gensym*))
        (j (gensym*)))
    `(catch 'conflict
       (let ((,len (length ,vector))
             (,i 0)
             (,j 0))
         (while (< ,i ,len)
           (setq ,j (+ ,i 1))
           (while (< ,j ,len)
             (when (and (/= 0 (aref ,vector ,i))
                        (= (aref ,vector ,i) (aref ,vector ,j)))
               (throw 'conflict nil))
             (setq ,j (1+ ,j)))
           (setq ,i (1+ ,i)
                 ,j (+ ,i 1)))
         t))))


(defun sudoku-puzzle-solved-p (&optional idx)
  "Predicate sudoku's puzzle is resovled."
  (catch 'block
    (let ((d (*sudoku-puzzle-d* :d))
          (sqr (*sudoku-puzzle-d* :sqr))
          (i 0)
          (j 0))

      (when idx
        (unless (sudoku-puzzle-vec-unique (*sudoku-puzzle* :row idx))
          (throw 'block :unique))
        (unless (sudoku-puzzle-vec-unique (*sudoku-puzzle* :col idx))
          (throw 'block :unique))
        (unless (sudoku-puzzle-vec-unique (*sudoku-puzzle* :sqr idx))
          (throw 'block :unique)))

      (while (< i d)
        (unless (sudoku-puzzle-vec-unique (*sudoku-puzzle*
                                           :row
                                           (sudoku-puzzle-1d i 0)))
          (throw 'block :unique))
        (setq i (1+ i)))

      (while (< j d)
        (unless (sudoku-puzzle-vec-unique (*sudoku-puzzle*
                                           :col
                                           (sudoku-puzzle-1d 0 j)))
          (throw 'block :unique))
        (setq j (1+ j)))

      (setq i 0 j 0)
      (while (< i d)
        (while (< j d)
          (unless (sudoku-puzzle-vec-unique (*sudoku-puzzle*
                                             :sqr
                                             (sudoku-puzzle-1d i j)))
            (throw 'block :unique))
          (setq j (+ j sqr)))
        (setq j 0 i (+ i sqr)))

      (setq i 0)
      (while (< i d)
        (unless (sudoku-puzzle-vec-complete (*sudoku-puzzle*
                                             :row
                                             (sudoku-puzzle-1d i 0)))
          (throw 'block :complete))
        (setq i (1+ i)))

      (setq j 0)
      (while (< j d)
        (unless (sudoku-puzzle-vec-complete (*sudoku-puzzle*
                                             :col
                                             (sudoku-puzzle-1d 0 j)))
          (throw 'block :complete))
        (setq j (1+ j)))

      (setq i 0 j 0)
      (while (< i d)
        (while (< j d)
          (unless (sudoku-puzzle-vec-complete (*sudoku-puzzle*
                                               :sqr
                                               (sudoku-puzzle-1d i j)))
            (throw 'block :complete))
          (setq j (+ j sqr)))
        (setq j 0 i (+ i sqr))))
    :solve))


(defalias '*sudoku-board*
  (lexical-let% ((o) (d) (p))
    (lambda (&optional k i j)
      (cond ((eq :cor! k) (setq o i d j p i))
            ((eq :pos k) p)

            ((eq :in k)
             (with-current-buffer (*sudoku*)
               (let ((row (line-number-at-pos))
                     (col (current-column)))
                 (and (<= (car o) row) (<= row (car d))
                      (<= (cdr o) col) (<= col (cdr d))))))

            ((eq :nex! k)
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
                   (while (not (get-text-property (point) :puzzle))
                     (let ((col (current-column)))
                       (forward-line v)
                       (while (< (current-column) col)
                         (forward-char 1)))
                     (setq v1 (+ v1 v))))
                 (when (/= h 0)
                   (setq h1 h)
                   (forward-char h)
                   (while (not (get-text-property (point) :puzzle))
                     (forward-char h)
                     (setq h1 (+ h1 h))))
                 (setq p (cons (+ (car p) v1)
                               (+ (cdr p) h1))))))

            ((eq :mov! k)
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
                 (setq p (cons (+ (- v) (car p))
                               (+ (- h) (cdr p)))))))

            ((eq :ori! k)
             (with-current-buffer (*sudoku*)
               (goto-char (point-min))
               (forward-line (1- (car o)))
               (forward-char (cdr o))
               (setq p o)))

            ((eq :dia! k)
             (with-current-buffer (*sudoku*)
               (goto-char (point-min))
               (forward-line (1- (car d)))
               (forward-char (cdr d))
               (setq p d)))

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
        (bs)
        (len (*sudoku-puzzle-d* :len)))
    (while (< i len)
      (let ((x (aref puzzle i)))
        (setq bs (append bs (list (list :puzzle (cons i x)
                                        :zero (= x 0))))
              i (1+ i))))
    bs))


(defun sudoku-board-draw (board)
  "Draw sudoku BOARD."
  (switch-to-buffer (*sudoku*))
  (let ((d (*sudoku-puzzle-d* :d))
        (sqr (*sudoku-puzzle-d* :sqr))
        (w 3))
    (with-current-buffer (*sudoku*)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (goto-char 0)
        (let ((s (let ((s1 0) (s2 0) (ss))
                   (while (< s1 sqr)
  	                 (setq ss (concat ss "+")
  				                 s2 0)
  	                 (while (< s2 sqr)
  		                 (setq ss (concat ss (make-string w ?-))
  					                 s2 (1+ s2)))
  	                 (setq s1 (1+ s1)))
                   (concat ss "+\n")))
              (v (let ((s1 0) (s2 0) (ss))
                   (while (< s1 sqr)
  	                 (setq ss (concat ss "|")
  				                 s2 0)
  	                 (while (< s2 sqr)
  		                 (setq ss (concat ss " %s ")
  					                 s2 (1+ s2)))
  	                 (setq s1 (1+ s1)))
                   (concat ss "|\n")))
              (u "_")
              (row 0)
              (idx 0))
          (while (< row d)
            (when (= 0 (% row sqr))
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
                     (take d board))))
            (setq board (drop d board)
                  row (1+ row)))
          (insert s))))
    (*sudoku-board* :cor!
                    (cons 2 2)
                    (cons (+ 1 d (1- sqr))
                          (+ (* w d) (mod d 2))))
    (*sudoku-board* :ori!)))


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
    (with-current-buffer (*sudoku*)
      (let ((inhibit-read-only t))
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
                   (cell (cons idx num))
                   (f (list :underline t
                            :foreground (*sudoku-color* :w))))

              (put-text-property pos (1+ pos) :puzzle cell)
              (*sudoku-puzzle* :box! idx (cdr cell))

              (let ((rc (sudoku-puzzle-solved-p idx)))
                (cond ((eq :unique rc)
                       (put-text-property pos (1+ pos) 'face f)
                       (throw 'block nil))
                      ((eq :solve rc)
                       (message "Solved, %s." (*sudoku-idiom*)))))

              (put-text-property pos (1+ pos)
                                 'face 'underline))))))))


(defun sudoku-board-input-erase ()
  "Erase at point."
  (interactive)
  (sudoku-board-input 0 (cons 'face nil)))


(defmacro sudoku-board-input-n (n)
  "Input N at point."
  (let ((d (gensym*)))
    `(let ((,d (*sudoku-puzzle-d* :d)))
       (if (>= ,d ,n)
           (sudoku-board-input ,n (cons 'face 'underline))
         (message "%d is invalid on %sx%s" ,n ,d ,d)))))


(defun sudoku-board-input-1 ()
  "Input 1 at point."
  (interactive)
  (sudoku-board-input-n 1))

(defun sudoku-board-input-2 ()
  "Input 2 at point."
  (interactive)
  (sudoku-board-input-n 2))

(defun sudoku-board-input-3 ()
  "Input 3 at point."
  (interactive)
  (sudoku-board-input-n 3))

(defun sudoku-board-input-4 ()
  "Input 4 at point."
  (interactive)
  (sudoku-board-input-n 4))

(defun sudoku-board-input-5 ()
  "Input 5 at point."
  (interactive)
  (sudoku-board-input-n 5))

(defun sudoku-board-input-6 ()
  "Input 6 at point."
  (interactive)
  (sudoku-board-input-n 6))

(defun sudoku-board-input-7 ()
  "Input 7 at point."
  (interactive)
  (sudoku-board-input-n 7))

(defun sudoku-board-input-8 ()
  "Input 8 at point."
  (interactive)
  (sudoku-board-input-n 8))

(defun sudoku-board-input-9 ()
  "Input 9 at point."
  (interactive)
  (sudoku-board-input-n 9))


(defun sudoku-board-disabled-key ()
  "Disabled key."
  (interactive))


(defun sudoku-board-save ()
  "Save sudoku's board to \\=`+sudoku-file+\\='."
  (let ((b (*sudoku-board* :props)))
    (save-sexp-to-file b (+sudoku-file+ :board))))


(defun sudoku-board-load ()
  "Load sudoku's board from \\=`+sudoku-file+\\='."
  (let ((b (read-sexp-from-file (+sudoku-file+ :board))))
    (*sudoku-puzzle*
     :set!
     (let ((i 0)
           (p (make-vector (length b) 0)))
       (dolist* (x b p)
         (aset p i (cdr (plist-get x :puzzle)))
         (setq i (1+ i)))))
    (sudoku-board-draw b)
    (sudoku-mode)))


(defun sudoku-quit ()
  "Quit \\=`*sudoku*\\='."
  (interactive)
  (when (and current-prefix-arg
             (yes-or-no-p "Save board? "))
     (sudoku-board-save))
  (kill-buffer (*sudoku*)))

(defun sudoku-save ()
  "Save \\=`*sudoku*\\='."
  (interactive)
  (sudoku-board-save))

(defun sudoku-new (level dimension)
  "New \\=`*sudoku*\\=' with LEVEL and DIMENSION."
  (interactive)
  (sudoku-board-draw
   (sudoku-board-make
    (*sudoku-puzzle*
     :set! (copy-sequence
            (*sudoku-puzzle-make* :new! level dimension)))))
  (sudoku-mode))

(defun sudoku-reload ()
  "Reload \\=`*sudoku*\\='."
  (interactive)
  (sudoku-board-draw (sudoku-board-make
                      (*sudoku-puzzle*
                       :set!
                       (copy-sequence
                        (*sudoku-puzzle-make* :rld)))))
  (sudoku-mode))


(defvar sudoku-mode-map
  (let ((m (make-sparse-keymap)))

    (define-key m "q" #'sudoku-quit)
    (define-key m "s" #'sudoku-save)
    (define-key m "N" #'sudoku-new)
    (define-key m "G" #'sudoku-reload)
    ;; (define-key m "H" #'sudoku-solve)

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
  "Switch to sudoku's mode'.\n
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


(defun sudoku (&optional level dimension)
  "Play sudoku on optional LEVEL and DIMENSION."
  (interactive
   (cond ((and (file-exists-p (+sudoku-file+ :board))
               (null current-prefix-arg))
          (list (completing-read
                 "Sudoku load (file): "
                 (+sudoku-file+ :board) nil nil
                 (+sudoku-file+ :board))
                nil))
         (t (list (completing-read
                   (format "Sudoku level (%s): "
                           (mapconcat #'symbol-name
                                      +sudoku-level+ "|"))
                   +sudoku-level+ nil nil
                   (or (car *sudoku-level-history*)
                       (symbol-name (car +sudoku-level+)))
                   '*sudoku-level-history*)
                  (completing-read
                   (format "Sudoku dimension (%s): "
                           (mapconcat #'symbol-name
                                      +sudoku-dimension+ "|"))
                   +sudoku-dimension+ nil nil
                   (or (car *sudoku-dimension-history*)
                       (symbol-name (car +sudoku-dimension+)))
                   '*sudoku-dimension-history*)))))
  (if (file-exists-plevel)
      (sudoku-board-load)
    (sudoku-new (intern level) (intern dimension))))



(provide 'sudoku)

;;; eof
