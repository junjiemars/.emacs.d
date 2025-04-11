;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; sudoku.el
;;;;
;; features:
;;; 1. save/load puzzle to/from file.
;;; 2. save/load board to/from file.
;;; 3. input validation.
;;; 4. new/reload.
;;; 5* zen mode.
;;; 6* step by step solver.
;;; 7* auto generate puzzle.
;;;;
;; references:
;;; https://pi.math.cornell.edu/~mec/Summer2009/Mahmood/Count.html
;;; http://www.gatsby.ucl.ac.uk/~turner/TeaTalks/Sudoku/Bertrand_and_Javis.pdf
;;;;

;;; env

(defun sudoku-spec->* (spec)
  "The spec of \\=`sudoku\\='."
  (cond ((and spec (eq spec :puzzle)) (v-home% ".games/sudoku-puzzle"))
        ((and spec (eq spec :board )) (v-home% ".games/sudoku-board"))
        ((and spec (eq spec :sample)) (v-home% ".games/sudoku-sample"))
        ((and spec (eq spec :dir!)) (v-home! ".games/"))
        ((and spec (eq spec :red)) "red")
        ((and spec (eq spec :idiom*))
         (let ((s '("a busted clock still be right twice a day"
                    "even a blind pig can find an acorn once in a while"
                    "every dog has its day")))
           (nth (random (length s)) s)))))

(defun *sudoku* ()
  "The sudoku\\='s process buffer."
  (get-buffer-create "*sudoku*"))

(defconst +sudoku-level+ '(easy medium hard sandbox)
  "Sudoku levels.")

(defconst +sudoku-dimension+ '(4x4 9x9)
  "Sudoku dimension.")

(defvar *sudoku-level-history* nil
  "Sudoku level history list.")

(defvar *sudoku-dimension-history* nil
  "Sudoku dimension history list.")

;; end of env

;;;
;; puzzle
;;;

(defun sudoku--puzzle-spec (matrix)
  (let* ((n1 (length matrix))
         (sqr1 (sqrt n1))
         (d1 (floor sqr1)))
    (vector d1 (floor (sqrt d1)) (apply #'+ (number-sequence 1 d1 1)) n1)))

(defun sudoku--puzzle-spec-> (spec key)
  (cond ((and spec key (eq :dim key)) (aref spec 0))
        ((and spec key (eq :sqr key)) (aref spec 1))
        ((and spec key (eq :sum key)) (aref spec 2))
        ((and spec key (eq :len key)) (aref spec 3))))

(defun sudoku--puzzle-make (level dimension)
  "Make puzzle at LEVEL and DIMENSION."
  (cond ((eq 'sandbox level)
         (make-vector (cond ((eq '4x4 dimension) (* 4 4))
                            (t (* 9 9)))
                      0))
        (t (let ((dst (sudoku-spec->* :sample)))
             (unless (file-exists-p dst)
               (copy-file (emacs-home% "config/sample-sudoku-puzzle.el") dst))
             (plist-get (plist-get (read-sexp-from-file dst) level)
                        dimension)))))

(defun sudoku--puzzle-1d (d i j)
  "Transform to 1d from 2d(I,J)."
  (+ (* (% i d) d) (% j d)))

(defun sudoku--puzzle-row (d i)
  "Locate row via 1d(I)."
  (% (/ i d) d))

(defun sudoku--puzzle-col (d i)
  "Locate col via 1d(I)."
  (% i d))

(defun sudoku--puzzle-sqr (d l s i)
  "Locate sqr via 1d(I)."
  (cons (/ (/ (% i l) d) s)
        (/ (% (% i l) d) s)))

(defun sudoku--puzzle-vec-row (spec matrix index)
  "Return MATRIX\\='s row vector at INDEX of SPEC."
  (let* ((d (sudoku--puzzle-spec-> spec :dim))
         (m matrix)
         (r (* (sudoku--puzzle-row d index) d))
         (v (make-vector d 0)))
    (dolist (x (number-sequence 0 (1- d) 1) v)
      (aset v x (aref m (+ r x))))))

(defun sudoku--puzzle-vec-col (spec matrix index)
  "Return MATRIX's col vector at INDEX of SPEC."
  (let* ((d (sudoku--puzzle-spec-> spec :dim))
         (m matrix)
         (c (% (sudoku--puzzle-col d index) d))
         (v (make-vector d 0)))
    (dolist (x (number-sequence 0 (1- d) 1) v)
      (aset v x (aref m (+ c (* x d)))))))

(defun sudoku--puzzle-vec-sub (spec matrix index)
  "Return MATRIX\\='s sub vector at INDEX of SPEC."
  (let* ((d (sudoku--puzzle-spec-> spec :dim))
         (l (sudoku--puzzle-spec-> spec :len))
         (s (sudoku--puzzle-spec-> spec :sqr))
         (m matrix)
         (s1 (sudoku--puzzle-sqr d l s index))
         (r (* (car s1) s d))
         (c (* (cdr s1) s))
         (v (make-vector d 0)))
    (dolist (x (number-sequence 0 (1- d) 1) v)
      (aset v x (aref m (+ r (* (/ x s) d) c (% x s)))))))

(defun sudoku--puzzle-box (spec index)
  "Return box vector at INDEX of SPEC."
  (% index (sudoku--puzzle-spec-> spec :len)))

(defalias '*sudoku-puzzle*
  (let ((v nil) (c nil) (s nil))
    (lambda (&optional k i n)
      (cond ((and i k (eq :row k)) (sudoku--puzzle-vec-row s c i))
            ((and i k (eq :col k)) (sudoku--puzzle-vec-col s c i))
            ((and i k (eq :sub k)) (sudoku--puzzle-vec-sub s c i))
            ((and i k (eq :box k)) (aref c (sudoku--puzzle-box s i)))
            ((and i k (eq :box! k)) (aset c (sudoku--puzzle-box s i) n))
            ((and i k (eq :new! k)) (setq s (sudoku--puzzle-spec i)
                                          v i
                                          c (copy-sequence v)))
            ((and k (eq :dim k)) (sudoku--puzzle-spec-> s :dim))
            ((and k (eq :sqr k)) (sudoku--puzzle-spec-> s :sqr))
            ((and k (eq :sum k)) (sudoku--puzzle-spec-> s :sum))
            ((and k (eq :len k)) (sudoku--puzzle-spec-> s :len))
            ((and k (eq :pre! k)) (setq c (copy-sequence v)))
            (t c))))
  "The \\=`sudoku\\=' puzzle in 1-dimension vector.")

(defun sudoku--puzzle-vec-complete? (vector)
  "Predicate sudoku puzzle\\='s VECTOR is complete."
  (let ((len (length vector))
        (sum 0)
        (i 0))
    (while (< i len)
      (setq sum (+ sum (aref vector i))
            i (1+ i)))
    (= sum (*sudoku-puzzle* :sum))))

(defun sudoku--puzzle-vec-unique? (vector)
  "Predicate sudoku puzzle\\='s VECTOR is unique."
  (catch :br
    (let ((len (length vector))
          (i 0)
          (j 0))
      (while (< i len)
        (setq j (+ i 1))
        (while (< j len)
          (when (and (/= 0 (aref vector i))
                     (= (aref vector i) (aref vector j)))
            (throw :br nil))
          (setq j (1+ j)))
        (setq i (1+ i)
              j (+ i 1)))
      t)))

(defun sudoku-puzzle-solved-p (&optional idx)
  "Predicate sudoku\\='s puzzle is resovled."
  (catch :br
    (let ((d (*sudoku-puzzle* :dim))
          (sqr (*sudoku-puzzle* :sqr))
          (i 0) (j 0))

      (when idx
        (unless (sudoku--puzzle-vec-unique? (*sudoku-puzzle* :row idx))
          (throw :br :unique))
        (unless (sudoku--puzzle-vec-unique? (*sudoku-puzzle* :col idx))
          (throw :br :unique))
        (unless (sudoku--puzzle-vec-unique? (*sudoku-puzzle* :sub idx))
          (throw :br :unique)))

      (while (< i d)
        (unless (sudoku--puzzle-vec-unique?
                 (*sudoku-puzzle* :row (sudoku--puzzle-1d d i 0)))
          (throw :br :unique))
        (setq i (1+ i)))

      (while (< j d)
        (unless (sudoku--puzzle-vec-unique?
                 (*sudoku-puzzle* :col (sudoku--puzzle-1d d 0 j)))
          (throw :br :unique))
        (setq j (1+ j)))

      (setq i 0 j 0)
      (while (< i d)
        (while (< j d)
          (unless (sudoku--puzzle-vec-unique?
                   (*sudoku-puzzle* :sub (sudoku--puzzle-1d d i j)))
            (throw :br :unique))
          (setq j (+ j sqr)))
        (setq j 0 i (+ i sqr)))

      (setq i 0)
      (while (< i d)
        (unless (sudoku--puzzle-vec-complete?
                 (*sudoku-puzzle* :row (sudoku--puzzle-1d d i 0)))
          (throw :br :complete))
        (setq i (1+ i)))

      (setq j 0)
      (while (< j d)
        (unless (sudoku--puzzle-vec-complete?
                 (*sudoku-puzzle* :col (sudoku--puzzle-1d d 0 j)))
          (throw :br :complete))
        (setq j (1+ j)))

      (setq i 0 j 0)
      (while (< i d)
        (while (< j d)
          (unless (sudoku--puzzle-vec-complete?
                   (*sudoku-puzzle* :sub (sudoku--puzzle-1d d i j)))
            (throw :br :complete))
          (setq j (+ j sqr)))
        (setq j 0 i (+ i sqr))))
    :solve))

;; end of puzzle

;;;
;; board
;;;

(defun sudoku--board-in? (o d)
  (with-current-buffer (*sudoku*)
    (let ((row (line-number-at-pos))
          (col (current-column)))
      (and (<= (car o) row) (<= row (car d))
           (<= (cdr o) col) (<= col (cdr d))))))

(defun sudoku--board-nex! (o d p i j)
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
        (while (null (get-text-property (point) :puzzle))
          (let ((col (current-column)))
            (forward-line v)
            (while (< (current-column) col)
              (forward-char 1)))
          (setq v1 (+ v1 v))))
      (when (/= h 0)
        (setq h1 h)
        (forward-char h)
        (while (null (get-text-property (point) :puzzle))
          (forward-char h)
          (setq h1 (+ h1 h))))
      (cons (+ (car p) v1) (+ (cdr p) h1)))))

(defun sudoku--board-mov! (o d p i j)
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
      (cons (+ (- v) (car p)) (+ (- h) (cdr p))))))

(defun sudoku--board-ori! (o)
  (with-current-buffer (*sudoku*)
    (goto-char (point-min))
    (forward-line (1- (car o)))
    (forward-char (cdr o))
    o))

(defun sudoku--board-dia! (d)
  (with-current-buffer (*sudoku*)
    (goto-char (point-min))
    (forward-line (1- (car d)))
    (forward-char (cdr d))
    d))

(defun sudoku--board-prop ()
  (with-current-buffer (*sudoku*)
    (text-properties-at (point))))

(defun sudoku--board-props ()
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

(defalias '*sudoku-board*
  (let ((o) (d) (p))
    (lambda (&optional k i j)
      (cond ((and k (eq :cor! k)) (setq o i d j p i))
            ((and k (eq :pos k)) p)
            ((and k (eq :in? k)) (sudoku--board-in? o d))
            ((and k (eq :nex! k)) (setq p (sudoku--board-nex! o d p i j)))
            ((and k (eq :mov! k)) (setq p (sudoku--board-mov! o d p i j)))
            ((and k (eq :ori! k)) (setq p (sudoku--board-ori! o)))
            ((and k (eq :dia! k)) (setq p (sudoku--board-dia! d)))
            ((and k (eq :prop k)) (sudoku--board-prop))
            ((and k (eq :props k)) (sudoku--board-props))
            (t (list :ori o :dia d :pos p)))))
  "The \\=`sudoku\\=' board.")

(defun sudoku-board-make (puzzle)
  "Make sudoku\\='s board with PUZZLE."
  (let ((i 0)
        (bs nil)
        (len (*sudoku-puzzle* :len)))
    (while (< i len)
      (let ((x (aref puzzle i)))
        (setq bs (append bs (list (list :puzzle (cons i x)
                                        :zero (= x 0))))
              i (1+ i))))
    bs))

(defun sudoku-board-draw (board)
  "Draw sudoku\\='s BOARD."
  (switch-to-buffer (*sudoku*))
  (let ((d (*sudoku-puzzle* :dim))
        (sqr (*sudoku-puzzle* :sqr))
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
             (apply
              #'format v
              (let ((xs nil))
                (dolist (x (take d board) (nreverse xs))
                  (setq xs (cons (apply #'propertize
                                        (let ((n (cdr (plist-get x :puzzle))))
                                          (cond ((= n 0) u)
                                                (t (number-to-string n))))
                                        x)
                                 xs))
                  (setq idx (1+ idx))))))
            (setq board (drop d board)
                  row (1+ row)))
          (insert s))))
    (*sudoku-board* :cor!
                    (cons 2 2)
                    (cons (+ 1 d (1- sqr))
                          (+ (* w d) (mod d 2))))
    (*sudoku-board* :ori!)))

(defun sudoku--board-move ()
  "Move on board."
  (or (*sudoku-board* :in?)
      (null (let* ((pos (*sudoku-board* :pos)))
              (*sudoku-board* :ori!)
              (*sudoku-board* :mov! (car pos) (cdr pos))))))

(defun sudoku-board-move-right ()
  "Move one step right."
  (interactive)
  (when (sudoku--board-move)
    (let ((pos (*sudoku-board* :pos)))
      (*sudoku-board* :nex! (car pos) (1+ (cdr pos))))))

(defun sudoku-board-move-left ()
  "Move one step left."
  (interactive)
  (when (sudoku--board-move)
    (let ((pos (*sudoku-board* :pos)))
      (*sudoku-board* :nex! (car pos) (1- (cdr pos))))))

(defun sudoku-board-move-down ()
  "Move one step down."
  (interactive)
  (when (sudoku--board-move)
    (let ((pos (*sudoku-board* :pos)))
      (*sudoku-board* :nex! (1+ (car pos)) (cdr pos)))))

(defun sudoku-board-move-up ()
  "Move one step up."
  (interactive)
  (when (sudoku--board-move)
    (let ((pos (*sudoku-board* :pos)))
      (*sudoku-board* :nex! (1- (car pos)) (cdr pos)))))

(defun sudoku-board-move-leftmost ()
  "Move to leftmost point."
  (interactive)
  (when (sudoku--board-move)
    (let ((cor (*sudoku-board*)))
      (*sudoku-board*
       :mov! (car (plist-get cor :pos)) (cdr (plist-get cor :ori))))))

(defun sudoku-board-move-rightmost ()
  "Move to rightmost point."
  (interactive)
  (when (sudoku--board-move)
    (let ((cor (*sudoku-board*)))
      (*sudoku-board*
       :mov! (car (plist-get cor :pos)) (cdr (plist-get cor :dia))))))

(defun sudoku-board-move-topmost ()
  "Move to topmost point."
  (interactive)
  (when (sudoku--board-move)
    (let ((cor (*sudoku-board*)))
      (*sudoku-board*
       :mov! (car (plist-get cor :ori)) (cdr (plist-get cor :pos))))))

(defun sudoku-board-move-bottom ()
  "Move to bottom point."
  (interactive)
  (when (sudoku--board-move)
    (let ((cor (*sudoku-board*)))
      (*sudoku-board*
       :mov! (car (plist-get cor :dia)) (cdr (plist-get cor :pos))))))

(defun sudoku--board-input (num &rest properties)
  "Input on sudoku\\='s board with NUM and PROPERTIES."
  (declare (indent 1))
  (catch :br
    (with-current-buffer (*sudoku*)
      (let ((inhibit-read-only t)
            (c (string-to-char (number-to-string num)))
            (tp (*sudoku-board* :prop))
            (pos (point)))

        (when (plist-get tp :zero)
          (delete-char 1)
          (cond ((char-equal ?0 c) (insert-char ?_ 1))
                (t (insert-char c 1)))
          (forward-char -1)
          (set-text-properties pos (1+ pos) tp)

          (dolist (x properties)
            (put-text-property pos (1+ pos) (car x) (cdr x)))

          (let* ((idx (car (plist-get tp :puzzle)))
                 (cell (cons idx num))
                 (f (list :underline t :foreground (sudoku-spec->* :red))))

            (put-text-property pos (1+ pos) :puzzle cell)
            (*sudoku-puzzle* :box! idx (cdr cell))

            (let ((rc (sudoku-puzzle-solved-p idx)))
              (cond ((and rc (eq :unique rc))
                     (put-text-property pos (1+ pos) 'face f)
                     (throw :br nil))
                    ((and rc (eq :solve rc))
                     (message "Solved, %s." (sudoku-spec->* :idiom*)))))

            (put-text-property pos (1+ pos) 'face 'underline)))))))

(defun sudoku-board-input-erase ()
  "Erase at point."
  (interactive)
  (sudoku--board-input 0 (cons 'face nil)))

(defun sudoku-board-input-ignore ()
  "Ignore input."
  (interactive))

(defun sudoku--board-input-n (n)
  "Input N at point."
  (let ((d (*sudoku-puzzle* :dim)))
    (if (>= d n)
        (sudoku--board-input n (cons 'face 'underline))
      (message "%d is invalid on %sx%s" n d d))))

(defun sudoku-board-input-1 ()
  "Input 1 at point."
  (interactive)
  (sudoku--board-input-n 1))

(defun sudoku-board-input-2 ()
  "Input 2 at point."
  (interactive)
  (sudoku--board-input-n 2))

(defun sudoku-board-input-3 ()
  "Input 3 at point."
  (interactive)
  (sudoku--board-input-n 3))

(defun sudoku-board-input-4 ()
  "Input 4 at point."
  (interactive)
  (sudoku--board-input-n 4))

(defun sudoku-board-input-5 ()
  "Input 5 at point."
  (interactive)
  (sudoku--board-input-n 5))

(defun sudoku-board-input-6 ()
  "Input 6 at point."
  (interactive)
  (sudoku--board-input-n 6))

(defun sudoku-board-input-7 ()
  "Input 7 at point."
  (interactive)
  (sudoku--board-input-n 7))

(defun sudoku-board-input-8 ()
  "Input 8 at point."
  (interactive)
  (sudoku--board-input-n 8))

(defun sudoku-board-input-9 ()
  "Input 9 at point."
  (interactive)
  (sudoku--board-input-n 9))

(defun sudoku--board-save ()
  "Save sudoku\\='s board to file."
  (save-sexp-to-file (*sudoku-board* :props) (sudoku-spec->* :board)))

;; end of board

;;;
;; new
;;;

(defun sudoku-quit ()
  "Quit \\=`*sudoku*\\='."
  (interactive)
  (when (and current-prefix-arg (yes-or-no-p "Save board? "))
    (sudoku--board-save))
  (kill-buffer (*sudoku*)))

(defun sudoku-save ()
  "Save \\=`*sudoku*\\='."
  (interactive)
  (sudoku--board-save))

(defun sudoku-new (&optional level dimension)
  "New \\=`*sudoku*\\=' with LEVEL and DIMENSION."
  (interactive)
  (sudoku-board-draw
   (sudoku-board-make
    (*sudoku-puzzle* :new! (sudoku--puzzle-make level dimension))))
  (sudoku-mode))

(defun sudoku-load (file)
  "Load sudoku\\='s board from FILE."
  (let ((b (read-sexp-from-file file)))
    (*sudoku-puzzle*
     :new!
     (let ((i 0)
           (p (make-vector (length b) 0)))
       (dolist (x b p)
         (aset p i (cdr (plist-get x :puzzle)))
         (setq i (1+ i)))))
    (sudoku-board-draw b)
    (sudoku-mode)))

(defun sudoku-reload ()
  "Reload \\=`*sudoku*\\='."
  (interactive)
  (sudoku-board-draw
   (sudoku-board-make
    (*sudoku-puzzle* :pre!)))
  (sudoku-mode))

;; end of new

;;;
;; sudoku-mode
;;;

(defun sudoku--prompt ()
  (let ((board (sudoku-spec->* :board))
        (resume? nil))
    (when (and (null current-prefix-arg) (file-exists-p board))
      (setq resume? (yes-or-no-p "Continue the last game?")))
    (cond (resume? (list board nil nil))
          (t (list
              nil                         ; no file
              (intern
               (completing-read
                (format "Choose level (%s): "
                        (mapconcat #'symbol-name +sudoku-level+ "|"))
                +sudoku-level+ nil nil
                (or (car *sudoku-level-history*)
                    (symbol-name (car +sudoku-level+)))
                '*sudoku-level-history*))
              (intern
               (completing-read
                (format "Choose dimension (%s): "
                        (mapconcat #'symbol-name +sudoku-dimension+ "|"))
                +sudoku-dimension+ nil nil
                (or (car *sudoku-dimension-history*)
                    (symbol-name (car +sudoku-dimension+)))
                '*sudoku-dimension-history*)))))))

(defun sudoku--mode-keymap ()
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

    (define-key m "\C-k" #'sudoku-board-input-ignore)
    (define-key m "\C-l" #'sudoku-board-input-ignore)
    (define-key m [mouse-1] #'sudoku-board-input-ignore)
    (define-key m [down-mouse-1] #'sudoku-board-input-ignore)
    (define-key m [drag-mouse-1] #'sudoku-board-input-ignore)
    (define-key m [double-mouse-1] #'sudoku-board-input-ignore)

    ;; (define-key m "\C-h" #'sudoku-board-input-ignore)
    m))

(defun sudoku-mode ()
  "Switch to sudoku\\='s mode'.\n
The following commands are available:
\\{sudoku-mode-map}"
  (interactive)
  (with-current-buffer (*sudoku*)
    (kill-all-local-variables)
    (use-local-map (sudoku--mode-keymap))
    (buffer-disable-undo)
    (setq major-mode 'sudoku-mode
          mode-name  "Sudoku"
          buffer-read-only t)))

(defun sudoku (&optional resume level dimension)
  "Play sudoku with LEVEL and DIMENSION or RESUME your last session."
  (interactive (sudoku--prompt))
  (sudoku-spec->* :dir!)
  (if resume
      (sudoku-load resume)
    (sudoku-new level dimension)))

;; end of `sudoku-mode'

(provide 'sudoku)

;;; end of sudoku.el
