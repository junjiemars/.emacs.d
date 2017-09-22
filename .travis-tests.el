;;;; -*- lexical-binding:t -*-
;;;;
;; Moduel Management 
;;;;



(require 'ert)

(ert-deftest %init:comment ()
  (should (eq nil (comment (+ 1 2 3)))))
