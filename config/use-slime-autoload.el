;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-slime-autoload.el
;;;;


(defalias '*slime-lisp-implementations*
  (lexical-let% ((ls (let ((ns))
                       (dolist* (x '(sbcl ecl acl ccl clasp) ns)
                         (let ((bin (executable-find (symbol-name x))))
                           (when bin (push (list x (list bin)) ns)))))))
    (lambda (&optional new)
      (setq% slime-lisp-implementations
             (if new (push new ls) ls)
             'slime)))
  "Parameterized set `slime-lisp-implementations'.")


(when-fn% 'slime-show-source-location 'slime
  (defadvice slime-show-source-location (after
                                         slime-show-source-location-after
                                         disable)
    "Show the Common LisP's source location in `view-mode'."
    (when (or (not (buffer-file-name (current-buffer)))
              (let ((src (getenv "SLIME_CLI_SRC")))
                (and src (match-string* src
                                        (buffer-file-name (current-buffer))
                                        0))))
      (with-current-buffer (current-buffer)
        (view-mode 1)))))


(with-eval-after-load 'slime

  (*slime-lisp-implementations*)

  (when-fn% 'slime-setup 'slime
    (slime-setup '(slime-fancy slime-asdf)))
  (when-fn% 'slime-selector 'slime
    (define-key (current-global-map) (kbd "C-c s s") #'slime-selector))

  (when-fn% 'slime-show-source-location 'slime
    (ad-enable-advice #'slime-show-source-location 'after
                      "slime-show-source-location-after")
    (ad-activate #'slime-show-source-location t)))


;; end of file
