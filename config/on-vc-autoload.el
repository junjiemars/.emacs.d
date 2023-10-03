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
      (when-fn% 'vc-dir 'vc-dir
        `(progn% ,@body))))


(when-feature-vc%

  (defvar *vc-frontend*
    `("*" ,(if-feature-magit% "magit"))
    "The fontends of VC.")

   (defvar *vc-frontend-history* nil
    "The VC frontend choosing history list."))


(when-feature-vc%

 (defun vc-dir* (&optional frontend)
   "Show the VC status."
   (interactive
    (list (if current-prefix-arg
              (completing-read
               (format "Choose (%s) " (mapconcat #'identity
                                                 *vc-frontend*
                                                 "|"))
               *vc-frontend* nil nil (car *vc-frontend-history*)
               '*vc-frontend-history* (car *vc-frontend*))
            (car *vc-frontend*))))
   (call-interactively
    (cond ((string= "*" frontend) #'vc-dir)
          ((string= "magit" frontend)
           (if-feature-magit% #'magit-status #'vc-dir))
          (t #'vc-dir)))))


(when-feature-vc%
 ;; general `vc-dir'
 (define-key% (current-global-map) (kbd "C-x v d") #'vc-dir*))


;; end of on-vc-autoload.el
