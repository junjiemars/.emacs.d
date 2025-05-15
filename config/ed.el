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

;;;
;; line
;;;

(defun delete-line* ()
  "Delete current line."
  (if-fn% delete-line nil
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

;;;
;; open-next/previous-line fn
;; control indent or not: `open-next-line' and `open-previous-line'.
;; see also: https://www.emacswiki.org/emacs/OpenNextLine
;;;

(defun open-next-line (n &optional indent)
  "Move to the next line and then open N lines, like vi\\=' \\=`o\\=' command.\n
Optional argument INDENT whether to indent lines. See also \\=`open-line\\='."
  (interactive (list (prefix-numeric-value
                      (if (consp current-prefix-arg)
                          1
                        current-prefix-arg))
                     (if current-prefix-arg
                         (y-or-n-p "Indent? ")
                       t)))
  (barf-if-buffer-read-only)
  (end-of-line)
  (open-line n)
  (forward-line 1)
  (and indent (indent-according-to-mode)))

(defun open-previous-line (n &optional indent)
  "Open N lines above the current one, like vi\\=' \\=`O\\=' command.\n
Optional argument INDENT whether to indent lines. See also \\=`open-line\\='."
  (interactive (list (prefix-numeric-value
                      (if (consp current-prefix-arg)
                          1
                        current-prefix-arg))
                     (if current-prefix-arg
                         (y-or-n-p "Indent:? ")
                       t)))
  (barf-if-buffer-read-only)
  (beginning-of-line)
  (open-line n)
  (and indent (indent-according-to-mode)))

;; end of line

;;;
;; file
;;;

(defun dup-file (src dst &optional force)
  "Dup SRC to DST when DST does not exist or FORCE."
  (inhibit-file-name-handler
    (when (or force (null (file-exists-p dst)))
      (copy-file src dst t)))
  dst)

(defun file-in-dirs-p (file dirs)
  "Return the matched dir if FILE in DIRS, otherwise nil."
  (when (and (stringp file) (consp dirs))
    (inhibit-file-name-handler
      (let ((case-fold-search (when-platform% windows-nt t))
            (d (file-name-directory file)))
        (catch :br
          (dolist (x dirs)
            (and (stringp x)
                 (eq 't (compare-strings
                         x 0 (length x) d 0 (length x)
                         case-fold-search))
                 (throw :br x))))))))

;; end of file

;;;
;; keyword
;;;

(defun keyword->string (keyword)
  (substring-no-properties (symbol-name keyword) 1))

(defun string->keyword (string)
  (intern (concat ":" string)))

;; end of keyword

;;;
;; parse
;;;


 ;; end of parse

;;;
;; prompt

(defun read-string-prompt (prompt &optional history option)
  (list (funcall (if-fn% read-shell-command nil
                         #'read-shell-command
                   #'read-string)
                 prompt
                 (or (car (symbol-value history)) option)
                 history)))

(defun select-region-prompt ()
  (if-region-active
      (list (region-beginning) (region-end))
    (list (point-min) (point-max))))

;; end of prompt


;;;
;; version
;;;

(defun vstrncmp (v1 v2 &optional n)
  "Return 0 if V1 equals V2, < 0 if V1 less than V2, otherwise > 0.\n
If optional N is non-nil compare no more than N parts, default N is 4."
  (let ((n (or n 4)) (n1 0) (n2 0))
    (cond ((= 0 n) 0)
          ((= 0 (length v1) (length v2)) 0)
          ((= 0 (length v1))
           (- (string-to-number
               (substring-no-properties v2 0 (strchr v2 ?.)))))
          ((= 0 (length v2))
           (string-to-number (substring-no-properties v1 0 (strchr v1 ?.))))
          ((progn (setq n1 (strchr v1 ?.) n2 (strchr v2 ?.))
                  (string-equal (substring-no-properties v1 0 n1)
                                (substring-no-properties v2 0 n2)))
           (vstrncmp (and n1 (substring-no-properties v1 (1+ n1)))
                     (and n2 (substring-no-properties v2 (1+ n2)))
                     (setq n (1- n))))
          (t (- (string-to-number (substring-no-properties v1 0 n1))
                (string-to-number (substring-no-properties v2 0 n2)))))))

;; end of version


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
;;       (if-fn% json-pretty-print json
;;               (make-thread* (lambda ()
;;                               (json-pretty-print begin end)))
;;         (message (propertize "No implemented"
;;                              'face 'font-lock-warning-face))))))

;; ;; end of json



(provide 'ed)

;; end of ed.el
