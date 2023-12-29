;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-slime-autoload.el
;;;;


(defalias '*slime-lisp-implementations*
  (lexical-let%
      ((b (let ((ns nil))
            (dolist* (x '(sbcl ecl acl) ns)
              (let ((bin (executable-find (symbol-name x))))
                (when bin (push! (list x (list bin)) ns)))))))
    (lambda (&optional n)
      (setq% slime-lisp-implementations
             (if n (push! n b) b)
             'slime)))
  "Parameterized \\=`slime-lisp-implementations\\='.")


(defalias '*slime-source-locations*
  (lexical-let% ((b '()))
    (lambda (&optional n)
      (cond (n (setq b (cons n b)))
            (t b))))
  "Parameterized source locations for \\=`slime\\='.")


(when-fn% 'slime-show-source-location 'slime
  (defadvice slime-show-source-location (after
                                         slime-show-source-location-after
                                         disable)
    "Show the Common LisP's source location in \\=`view-mode\\='."
    (with-current-buffer (current-buffer)
      (dolist* (ss (*slime-source-locations*))
        (when (and (stringp ss)
                   (string-match ss (buffer-file-name (current-buffer))))
          (view-mode 1))))))


(defun use-slime-init! ()
  "On \\=`slime\\=' initialization."
  (*slime-lisp-implementations*)
  (when-fn% 'slime-setup 'slime
    (slime-setup '(slime-fancy slime-asdf)))
  (when-fn% 'slime-selector 'slime
    (define-key% (current-global-map) (kbd "C-c s s") #'slime-selector))
  (when-fn% 'slime-show-source-location 'slime
    (ad-enable-advice #'slime-show-source-location 'after
                      "slime-show-source-location-after")
    (ad-activate #'slime-show-source-location t)))


;; `slime' aftere load
(with-eval-after-load 'slime
  (use-slime-init!))


;; end of file
