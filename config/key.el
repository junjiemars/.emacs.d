;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; key.el
;;;;
;; Commentary: self modifier keys.
;;;;


(defun key-spec->* (&optional key)
  "Extract :key from env-spec via KEY."
  (cond (key (env-spec->* :key key))
        (t (env-spec->* :key))))



(defun self-key-init! ()
  "Initialize key spec from \\=`*self-env-spec*\\='."
  (when-graphic%
    (when (key-spec->* :allowed)
      (let ((modifier (key-spec->* :modifier)))
        (dolist* (x modifier)
          (set (car x) (cdr x)))))
    ;; disable suspend-frame
    (when-platform% 'darwin
      (when-fn% suspend-frame frame
        (fset 'suspend-frame
              (lambda () (interactive)
                (user-error
                 "%s" "A monkey will always behave like a monkey")))))))

 ; end of key.el
