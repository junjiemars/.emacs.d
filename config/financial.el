;;;; -*- lexical-binding:t -*-
;;;;
;; Financial Calculations
;;;;




(defmacro /. (dividend &rest divisors)
  "Return first float-point DIVIDEND divided by all the remaining DIVISORS.

\(fn DIVIDEND DIVISORS...\)"
  (declare (indent 1))
  `(/ (+ ,dividend 0.0) ,@divisors))


(defmacro rate. (R &optional periods)
  "Return the rate of R on the spedified periods.

\(fn RATE &optional PERIODS\)"
  (let ((rt (make-symbol "rt")))
    `(let ((,rt (if ,periods ,periods 1)))
       (/. ,R ,rt))))


(defun interest (P R T &optional periods)
  "Return the earned simple interest.

\(fn PRINCIPAL RATE TIMES &optional PERIODS\)"
  (let ((R1 (rate. R periods)))
    (* P R1 T)))


(defun future-value (P R T &optional periods)
  "Return the future value using simple interest.

\(fn PRINCIPAL RATE TIMES &optional PERIODS\)"
  (let ((R1 (rate. R periods)))
    (* P (+ 1 (* R1 T)))))


(defun future-value+ (P R T &optional periods)
  "Return the future value using compound interest.

\(fn PRINCIPAL RATE TIMES &optional PERIODS\)"
  (let ((R1 (rate. R periods)))
    (* P (expt (+ 1 R1) T))))


(defun interest+ (P R T &optional periods)
  "Return the earned compound interest.

\(fn PRINCIPAL RATE TIMES &optional PERIODS\)"
  (let ((R1 (rate. R periods)))
    (- (future-value+ P R1 T) P)))


(defun present-value (F R T &optional periods)
  "Return the present value using simple interest. 

\(fn FUTURE-VALUE RATE TIMES &optional PERIODS\)"
  (let ((R1 (rate. R periods)))
    (/. F (+ 1 (* R1 T)))))


(defun present-value+ (F R T &optional periods)
  "Return the present value using compound interest.

\(fn FUTURE-VALUE RATE TIMES &optional PERIODS\)"
  (let ((R1 (rate. R periods)))
    (/. F (expt (+ 1 R1) T))))


(defun times (F P R &optional periods)
  "Return the times using simple interest.

\(fn FUTURE-VALUE PRINCIPAL RATE &optional PERIODS\)"
  (let ((R1 (rate. R periods)))
    (/. (- (/. F P) 1) R1)))


(defun times+ (F P R &optional periods)
  "Return the times using compound interest.

\(fn FUTURE-VALUE PRINCIPAL RATE &optional PERIODS\)"
  (let ((R1 (rate. R periods)))
    (/. (log (/. F P)) (log (+ 1 R1)))))


(defun rate (F P T &optional periods)
  "Return the simple interest rate.

\(fn FUTURE-VALUE PRINCIPAL TIMES &optional PERIODS\)"
  (let ((R (/. (- (/. F P) 1) T))
        (rt (if periods periods 1)))
    (* R rt)))


(defun rate+ (F P T &optional periods)
  "Return the compound interest rate.

\(fn FUTURE-VALUE PRINCIPAL TIMES &optional PERIODS\)"
  (let ((R  (- (expt (/. F P) (/. 1 T)) 1))
        (rt (if periods periods 1)))
    (* R rt)))
