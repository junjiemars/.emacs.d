;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-slime-autoload.el
;;;;


(defun set-slime-lisp-implementations! ()
  "More easy way to set `slime-lisp-implementations'."
  (setq% slime-lisp-implementations
         (let ((impls nil))
           (dolist* (x '(acl ccl clasp ecl sbcl) impls)
             (let* ((impl (if (consp x) (car x) x))
                    (bin (executable-find (symbol-name impl))))
               (when bin (push (list impl (list bin)) impls)))))
         'slime))


(when-fn% 'slime-show-source-location 'slime
  (defadvice slime-show-source-location (after
                                         slime-show-source-location-after
                                         disable)
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
    (ad-enable-advice #'slime-show-source-location 'after
                      "slime-show-source-location-after")
    (ad-activate #'slime-show-source-location t)))


;; end of file
