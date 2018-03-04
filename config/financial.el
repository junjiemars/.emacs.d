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
  "Return the earned compound interest."
  (- (future-value+ principal (rate. rate periods) times) principal))


(defun present-value (future rate times &optional periods)
  "Return the present value using simple interest."
  (/. future (+ 1 (* (rate. rate periods) times))))


(defun present-value+ (future rate times &optional periods)
  "Return the present value using compound interest."
  (/. future (expt (+ 1 (rate. rate periods)) times)))


(defun times (future principal rate &optional periods)
  "Return the times using simple interest."
  (/. (- (/. future principal) 1) (rate. rate periods)))


(defun times+ (future principal rate &optional periods)
  "Return the times using compound interest."
  (/. (log (/. future principal))
    (log (+ 1 (rate. rate periods)))))


(defun rate (future principal times &optional periods)
  "Return the simple interest rate."
  (* (/. (- (/. future principal) 1) times)
     (if periods periods 1)))


(defun rate+ (future principal times &optional periods)
  "Return the compound interest rate."
  (* (- (expt (/. future principal) (/. 1 times)) 1)
     (if periods periods 1)))


(provide 'financial)
