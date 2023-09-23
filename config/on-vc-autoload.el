;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-vc-autoload.el
;;;;


(defmacro-if-feature% vc)
(defmacro-if-feature% magit)


(defmacro when-feature-vc% (&rest body)
  "When \\=`vc\\=', do BODY."
  (if-feature-vc%
      `(progn% ,@body)))


(when-feature-vc%

  (defvar *vc-frontend*
    `("*" ,(if-feature-magit% "magit"))
    "The fontend of VC.")


   (defvar *vc-history* nil
    "The choosing history of VC."))


(when-feature-vc%

 (defun vc-dir* (&optional frontend)
   "Show the VC status."
   (interactive
    (list (if current-prefix-arg
              (read-string (format "Choose (%s) "
                                   (mapconcat #'identity
                                              *vc-frontend*
                                              "|"))
                           (or (car *vc-history*)
                               (car *vc-frontend*))
                           '*vc-history*)
            (car *vc-frontend*))))

   (call-interactively
    (cond ((string= "*" frontend) #'vc-dir)
          ((string= "magit" frontend)
           (if-feature-magit% #'magit-status #'vc-dir))
          (t #'vc-dir))))

 ;; general `vc-dir*' keybind
 (with-eval-after-load 'vc
   (define-key (current-global-map) (kbd "C-x v d") #'vc-dir*)))


 ; end of on-vc-autoload.el
