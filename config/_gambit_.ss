;;

(define (_gambit_:complete-symbol substr)
  (let ((ss (open-output-string)))
    (apropos substr ss)
    (write (get-output-string ss))))
