;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; cls.el
;;;;
;; Commentary: common lisp fn.
;;;;



;; Load cl-lib/cl at runtime
(eval-when-compile
  (if-version%
      <= 24.1
      (require 'cl-lib)
    (require 'cl)))

(fset 'assoc**
      (if-fn% cl-assoc cl-lib
              (prog1 #'cl-assoc
                (declare-function cl-assoc 'cl-lib))
        #'assoc*))

(fset 'mapcar**
      (if-fn% cl-mapcar cl-lib
              (prog1 #'cl-mapcar
                (declare-function cl-mapcar 'cl-lib))
        #'mapcar*))

(fset 'remove-if*
      (if-fn% cl-remove-if cl-lib
              (prog1 #'cl-remove-if
                (declare-function cl-remove-if 'cl-lib))
        #'remove-if))

(fset 'member-if*
      (if-fn% cl-member-if cl-lib
              (prog1 #'cl-member-if
                (declare-function cl-member-if 'cl-lib))
        #'member-if))

(fset 'every*
      (if-fn% cl-every cl-lib
              (prog1 #'cl-every
                (declare-function cl-every 'cl-lib))
        #'every))

(fset 'some*
      (if-fn% cl-some cl-lib
              (prog1 #'cl-some
                (declare-function cl-some 'cl-lib))
        #'some))

(fset 'loop*
      (if-fn% cl-loop cl-lib
              (prog1 #'cl-loop
                (declare-function cl-loop 'cl-lib))
        #'loop))

;; end of common-lisp

(provide 'cls)

;; end of cls.el
