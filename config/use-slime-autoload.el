;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-slime-autoload.el
;;;;


(eval-when-compile

  (defmacro _common_lisp_implementations_ (&rest lispspec)
    "LISPSPEC such as:
'sbcl
'(ecl :init slime-init-command)
'(acl (\"acl7\" \"-quiet\")
      :coding-system emacs-mule)"
    (declare (indent 0))
    `(let ((lisps nil))
       (dolist* (x (list ,@lispspec) lisps)
         (let* ((lisp (if (consp x) (car x) x))
                (bin (executable-find (symbol-name lisp))))
           (when bin
             (if (consp x)
                 (if (consp (cadr x))
                     (push x lisps)
                   (push (list lisp (append (list bin) (cdr x))) lisps))
               (push (list lisp (list bin)) lisps))))))))


(defun set-slime-lisp-implementations! ()
  "More easy way to set `slime-lisp-implementations'."
  (setq% slime-lisp-implementations
         (_common_lisp_implementations_
          'acl
          'ccl
          'clasp
          'ecl
          'sbcl)
         'slime))


(when-fn% 'slime-show-source-location 'slime
  (defadvice slime-show-source-location (after
                                         slime-show-source-location-after
                                         compile)
    "Show the Common LisP's source location in `view-mode'."
    (with-current-buffer (current-buffer)
      (view-mode 1))))


(with-eval-after-load 'slime

  (set-slime-lisp-implementations!)

  (when-fn% 'slime-setup 'slime
    (slime-setup '(slime-fancy slime-asdf)))

  (when-fn% 'slime-selector 'slime
    (define-key (current-global-map) (kbd "C-c s s") #'slime-selector))

  (when-fn% 'slime-show-source-location 'slime
    (ad-activate #'slime-show-source-location t)))


;; end of file
