;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; scratch.el
;;;;

;;; env

(defun scratch-spec->* (recipe spec)
  (cond ((and recipe (eq :* recipe))
         (cond ((and spec (eq spec :txt))
                (substitute-command-keys initial-scratch-message))
               ((and spec (eq spec :mod))
                (lisp-interaction-mode))
               ((and spec (eq spec :pos))
                (goto-char (point-max))
                (forward-line 1))))
        ((and recipe (eq :org recipe))
         (cond ((and spec (eq spec :txt))
                (read-file*
                 (dup-file
                  (emacs-home% "config/scratch.org")
                  (v-home% ".exec/scratch.org"))))
               ((and spec (eq spec :mod))
                (org-mode))
               ((and spec (eq spec :pos))
                (goto-char (point-max)))))
        ((and recipe (eq :tex recipe))
         (cond ((and spec (eq spec :txt))
                ;; for CJK:
                ;; % \\usepackage{xeCJK}
                ;; % \\setCJKmainfont{SimSong}
                ;; % \\setmainfont{Times New Roman}
                ;; % (setq TeX-engine 'xetex)
                (read-file*
                 (dup-file
                  (emacs-home% "config/scratch.tex")
                  (v-home% ".exec/scratch.txt"))))
               ((and spec (eq spec :mod))
                (latex-mode))
               ((and spec (eq spec :pos))
                (goto-char (point-max))
                (forward-line -3))))
        ((and recipe (eq :meta recipe))
         (cond ((and spec (eq spec :list))
                (mapcar #'keyword->string `(:* :org :tex)))))))

(defvar *scratch-recipe-history* nil
  "The scratch recipe choosing history list.")

(defun scratch--prompt ()
  (let ((recipes (scratch-spec->* :meta :list)))
    (list (if current-prefix-arg
              (completing-read
               (format "Choose (%s) "
                       (mapconcat #'identity recipes "|"))
               recipes
               nil nil (car *scratch-recipe-history*)
               '*scratch-recipe-history* (car recipes))
            (car recipes)))))

;; end of env

(defun scratch (&optional recipe)
  "New a *scratch* buffer by RECIPE or switch to the existing one."
  (interactive (scratch--prompt))
  (switch-to-buffer
   (let ((n (format "*%s*" (if (string-equal "*" recipe)
                               "scratch"
                             (concat "scratch/" recipe))))
         (r (string->keyword recipe)))
     (with-current-buffer (get-buffer-create n)
       (when (= 0 (buffer-size))
         (insert (scratch-spec->* r :txt))
         (scratch-spec->* r :mod)
         (scratch-spec->* r :pos))
       (current-buffer)))))



(provide 'scratch)

;; end of scratch.el
