;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; key.el
;;;;
;; Commentary: self modifier keys.
;;;;


(defmacro key-spec->* (&rest keys)
  "Extract :key from env-spec via KEYS."
  (declare (indent 0))
  `(*self-env-spec* :get :key ,@keys))

(defun self-key-init! ()
  "Initialize key spec from \\=`*self-env-spec*\\='."
  (when-graphic%
    (when (key-spec->* :allowed)
      (let ((modifier (key-spec->* :modifier)))
        (dolist* (x modifier)
          (set (car x) (cdr x)))))
    ;; disable suspend-frame
    (when-platform% 'darwin
      (when-fn% 'suspend-frame 'frame
        (fset 'suspend-frame
              (lambda () (interactive)
                (user-error
                 "%s" "A monkey will always behave like a monkey")))))))

 ; end of key.el
