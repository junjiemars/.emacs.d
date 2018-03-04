;;;; -*- lexical-binding:t -*-
;;;;
;; financial calculations
;;;;




(defmacro /. (dividend &rest divisors)
  "Return first float-point DIVIDEND divided by all the remaining DIVISORS."
  (declare (indent 1))
  `(/ (+ ,dividend 0.0) ,@divisors))


(defmacro rate. (rate &optional periods)
  "Return the periodic rate of RATE in the spedified periods."
  `(/. ,rate (if ,periods ,periods 1)))


(defun interest (principal rate times &optional periods)
  "Return the earned simple interest."
  (* principal (rate. rate periods) times))


(defun future-value (principal rate times &optional periods)
  "Return the future value using simple interest."
  (* principal (+ 1 (* (rate. rate  periods) times))))


(defun future-value+ (principal rate times &optional periods)
  "Return the future value using compound interest."
  (* principal (expt (+ 1 (rate. rate periods)) times)))


(defun interest+ (principal rate times &optional periods)
  "Return the earned compound INTEREST."
  (- (future-value+ principal (rate. rate periods) times) principal))


(defun present-value (futrue rate times &optional periods)
  "Return the present value using simple interest."
  (/. future (+ 1 (* (rate. R periods) times))))


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

(provide 'financial)
