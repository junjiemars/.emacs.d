;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; ed.el
;;;;
;; Commentary: non-`prog-mode' editing.
;;;;


;;; require

;; end of require

(defmacro delete-line* ()
  "Delete current line."
  `(if-fn% 'delete-line nil
           (delete-line)
     (let ((inhibit-field-text-motion t))
       (delete-region (line-beginning-position)
                      (line-beginning-position 2)))))

(defun newline* (&optional arg)
  "Raw newline."
  (interactive "*P")
  (let ((electric-indent-mode nil))
    (when-version% > 26
      (when-lexical% (ignore* electric-indent-mode)))
    (if-version% <= 24.4
                 (newline arg 'interactive)
      (newline arg))))

(defmacro shell-format-buffer (mode alternate tmpfile &rest shell*)
  "Format the current buffer via SHELL\\=*."
  (declare (indent 0))
  `(with-current-buffer (current-buffer)
     (when (eq major-mode ,mode)
       ,alternate
       (let* ((p (point))
              (bs (if-region-active
                      (cons (region-beginning) (region-end))
                    (cons (point-min) (point-max))))
              (rs (buffer-substring-no-properties (car bs) (cdr bs)))
              (f (save-str-to-file
                  rs
                  (concat ,(path! (emacs-home* "scratch/")) ,tmpfile)))
              (ss (let ((x (shell-command* ,@shell* f)))
                    (and (= 0 (car x))
                         (read-str-from-file f)))))
         (unless (string= rs ss)
           (save-excursion
             (save-restriction
               (delete-region (car bs) (cdr bs))
               (insert ss)))
           (goto-char p))
         (when (file-exists-p f) (delete-file f))))))

(defmacro symbol@ (&optional thing)
  "Return the (cons \\='region|nil THING) at point."
  (let ((ss (gensym*)))
    `(if-region-active
         (let ((,ss (buffer-substring-no-properties
                     (region-beginning)
                     (region-end))))
           (setq mark-active nil)
           (cons 'region ,ss))
       (let ((,ss (thing-at-point (or ,thing 'symbol))))
         (and ,ss (cons nil (substring-no-properties ,ss)))))))

;; (when-version% >= 25
;;   (declare-function forward-whitespace "subr"))

;; ;; elisp

;; (defmacro pprint (form)
;;   "Insert a pretty-printed rendition of a Lisp FORM in current buffer."
;;   `(cl-prettyprint ,form))
;; (autoload 'cl-prettyprint "cl-extra")


;; ;;; xml

;; (defun pp*-xml (begin end &optional arg)
;;   "Pretty pprint XML region."
;;   (interactive (list (region-beginning) (region-end)
;;                      current-prefix-arg))
;;   (save-excursion
;;     (if (and (numberp arg) (< arg 0))
;;         (let ((s (replace-regexp-in-string
;;                   ">[ \t\n]+<" "><"
;;                   (delete-and-extract-region begin end))))
;;           (goto-char begin)
;;           (insert s)
;;           (set-mark (point)))
;;       (with-current-buffer (current-buffer)
;;         (eval-when-compile (require 'sgml-mode))
;;         (progn
;;           (declare-function sgml-pretty-print "sgml-mode")
;;           (require 'sgml-mode)
;;           (sgml-pretty-print begin end))))))

;; ;; end of xml

;; ;;; json

;; (defun pp*-json (&optional minify)
;;   "Pretty print Json region or current buffer."
;;   (interactive "P")
;;   (let ((begin (if-region-active (region-beginning) (point-min)))
;;         (end (if-region-active (region-end) (point-max))))
;;     (if minify
;;         (make-thread*
;;          (lambda ()
;;            (narrow-to-region begin end)
;;            (goto-char (point-min))
;;            (while (forward-whitespace 1)
;;              (unless (bounds-of-thing-at-point 'string)
;;                (let ((bw (bounds-of-thing-at-point 'whitespace)))
;;                  (delete-whitespace-rectangle (car bw)
;;                                               (cdr bw)))))
;;            (goto-char (point-min))
;;            (while (and (forward-line 1)
;;                        (< (point) (point-max)))
;;              (delete-char -1 t))))
;;       (if-fn% 'json-pretty-print 'json
;;               (make-thread* (lambda ()
;;                               (json-pretty-print begin end)))
;;         (message (propertize "No implemented"
;;                              'face 'font-lock-warning-face))))))

;; ;; end of json



(provide 'ed)

;; end of ed.el
