;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; sample-self-prologue.el: specify the prologue of yourself
;;   should be run before `(*self-paths* :get :env-spec)'
;;
;;;;

(comment
 (*self-paths* :put :env-spec nil)
 (*self-paths* :put :package-spec nil)
 (*self-paths* :put :epilogue nil))

;; eof
