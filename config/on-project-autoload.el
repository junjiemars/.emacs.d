;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-project-autoload.el
;;;;


(defmacro-if-feature% project)


(defmacro when-feature-project% (&rest body)
  "When \\=`project\\=', do BODY."
  (if-feature-project%
      `(progn% ,@body)
    `(comment ,@body)))


;;; `project'

(when-feature-project%

 (defalias 'project*-root
   (lexical-let% ((b (emacs-home* ".project/root.el"))
                  (c '()))
     (lambda (&optional op sexp)
       "OP SEXP"
       (cond ((eq op :cache)
              (if sexp
                  (catch 'br
                    (dolist* (s1 c)
                      (when (string= s1 sexp)
                        (throw 'br s1))))
                c))
             ((eq op :read)
              (setq c (read-sexp-from-file b)))
             ((eq op :save)
              (when sexp (save-sexp-to-file sexp b)))
             (t c))))
   "The \\=`project-root\\=' cache.
ROOT must be absolute but can be nested."))


(when-feature-project%

 (defun project*-try-root (dir)
   (let ((d (project*-root :cache dir)))
     (when d (list 'vc 'Git d)))))


(when-feature-project%

 (defun on-project-init! ()
   "On \\=`project\\=' initialization."
   (project*-root :read)
   (push! #'project*-try-root project-find-functions)))

;; end of `project'


;;; `project' after load
(when-feature-project%
 (with-eval-after-load 'project
   (on-project-init!))
 (when-fn% 'project-find-file 'project
   (define-key% (current-global-map) (kbd "C-x p f")
                #'project-find-file)))


;; end of on-project-autoload.el
