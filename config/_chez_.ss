(library (_chez_)
  (export _chez_:complete-symbol)
  (import (chezscheme))

	(define string-prefix?
		(lambda (x y)
			(let ([n (string-length x)])
				(and (fx<= n (string-length y))
						 (let prefix? ([i 0])
							 (or (fx= i n)
									 (and (char=? (string-ref x i)
																(string-ref y i))
												(prefix? (fx+ i 1)))))))))

	(define (write-to-string x)
		(with-output-to-string
			(lambda ()
				(write x))))

	(define (_chez_:complete-symbol prefix . rest)
		rest
		(sort string-ci<?
					(filter (lambda (s)
										(string-prefix? prefix s))
									(map write-to-string
											 (environment-symbols
												(interaction-environment)))))))

(import (_chez_))

;;; eof
