;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; reset.el
;;;;


(defun clean-versioned-dirs (dirs &optional scope)
  "Clean versioned SCOPEd DIRS."
  (dolist (d dirs)
    (setq d (concat (emacs-home* d) "/"))
    (when (and d (file-exists-p d))
      (dolist (f (directory-files d nil "^[gt]_[0-9]+[.0-9]*"))
        (when (cond ((and scope (eq :8 scope)) t)
                    ((and scope (eq :< scope))
                     (let ((v (string-match*
                               (format "^%s\\([.0-9]+\\)$" +v-prefix+)
                               f 1)))
                       (and v (< (string-to-number v) +emacs-version+))))
                    (t (string-equal (v-name) f)))
          (shell-command* (if-platform% windows-nt
                              (concat "cmd /Q /C rmdir /Q /S")
                            (concat "rm -r "))
            (shell-quote-argument (concat d f))))))))

(defun reset-emacs (&optional do? ver?)
  "Clean all compiled files and dot files, then kill Emacs."
  (interactive)
  (when (if-interactive%
            (or do? (yes-or-no-p "Reset emacs?"))
          do?)
    (let ((ds (let ((xs nil)
                    (d1 (directory-files (emacs-home%) nil "^\\.[a-z]+")))
                (dolist (d d1 xs)
                  (unless (member d '(".git" ".gitignore" ".github"))
                    (setq xs (cons d xs))))))
          (ns `("private" "config" "theme")))
      (clean-versioned-dirs (nconc ds ns) ver?))
    (setq kill-emacs-hook nil)
    (setq% kill-emacs-query-functions nil)
    (kill-emacs 0)))



(provide 'reset)

;; end of reset.el
