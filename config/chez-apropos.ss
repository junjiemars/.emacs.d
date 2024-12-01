;;; chez-emacs: from `(emacs-home% \"config/chez.el\")'
(library (chez-emacs)
    (export chez-emacs/apropos)
  (import (chezscheme))
  (define (chez-emacs/apropos what max)
    (let* ([lst (apropos-list what (interaction-environment))]
           [tbl (make-eq-hashtable)]
           [xs (let f ([ls lst] [acc tbl])
                 (cond [(null? ls) (hashtable-keys acc)]
                       [(symbol? (car ls))
                        (f (cdr ls)
                           (begin (hashtable-set! acc (car ls) '())
                                  acc))]
                       [else (f (cdar ls) acc)]))])
      (map symbol->string
           (if (> (vector-length xs) max)
               (let v->l ([n max] [ss '()])
                 (if (= n 0)
                     ss
                     (v->l (- n 1) (cons (vector-ref xs n) ss))))
               (vector->list xs))))))
(import (chez-emacs))
;;; eof
