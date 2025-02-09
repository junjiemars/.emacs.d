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
;; interactive checking
;;;

(defmacro called-interactively? (&optional kind)
  "Return t if called by \\=`call-interactively\\='."
  (if-fn% called-interactively-p nil
          `(called-interactively-p ,kind)
    `(prog1
         (interactive-p)
       (ignore* ,kind))))

;; end of interactive checking

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

(defun file-in-dirs-p (file dirs)
  "Return the matched dir if FILE in DIRS, otherwise nil."
  (when (and (stringp file) (consp dirs))
    (inhibit-file-name-handler
      (let ((case-fold-search (when-platform% windows-nt t))
            (d (file-name-directory file)))
        (catch 'br
          (dolist (x dirs)
            (and (stringp x)
                 (eq 't (compare-strings
                         x 0 (length x) d 0 (length x)
                         case-fold-search))
                 (throw 'br x))))))))

;; end of file

;;;
;; buffer
;;;

(defmacro shell-format-buffer (modes alternate src shell*)
  "Format the current buffer via SHELL\\=*."
  (declare (indent 1))
  `(with-current-buffer (current-buffer)
     (when (catch 'br
             (dolist (x ,modes)
               (and (eq x major-mode)
                    (throw 'br t))))
       ,alternate
       (let ((p (point))
             (bs (if-region-active
                     (cons (region-beginning) (region-end))
                   (cons (point-min) (point-max)))))
         (let ((rs (buffer-substring-no-properties (car bs) (cdr bs)))
               (s1 ,src))
           (when (and rs s1)
             (let ((ss (when (save-str-to-file rs s1)
                         (let ((dst (funcall ,shell* s1)))
                           (and dst (read-str-from-file dst))))))
               (unless (or (null ss) (string= rs ss))
                 (save-excursion
                   (save-restriction
                     (delete-region (car bs) (cdr bs))
                     (insert ss)))
                 (goto-char p)))))))))

;; end of buffer

;;;
;; version
;;;

(defun version-strncmp (v1 v2 &optional n)
  "Return 0 if V1 equals V2, -1 if V1 less than V2, otherwise 1.\n
If optional N is non-nil compare no more than N parts, default N is 4."
  (let ((l1 (length v1)) (l2 (length v2))
        (nv1 0) (nv2 0)
        (n (or (and (integerp n) (> n 0) n) 4))
        (i 0) (j1 0) (j2 0) (k1 0) (k2 0))
    (cond ((and (= l1 0) (= l2 0)) 0)
          ((and (= l1 0) (> l2 0)) -1)
          ((and (> l1 0) (= l2 0) 1))
          (t (catch 'br
               (while (< i n)
                 (setq nv1
                       (catch 'br1
                         (when (= j1 l1) (throw 'br1 0))
                         (while (< j1 l1)
                           (when (= ?. (aref v1 j1))
                             (throw 'br1
                                    (string-to-number
                                     (substring-no-properties
                                      v1 k1
                                      (prog1 j1
                                        (setq j1 (1+ j1) k1 j1))))))
                           (setq j1 (1+ j1)))
                         (string-to-number
                          (substring-no-properties v1 k1 j1)))
                       nv2
                       (catch 'br2
                         (when (= j2 l2) (throw 'br2 0))
                         (while (< j2 l2)
                           (when (= ?. (aref v2 j2))
                             (throw 'br2
                                    (string-to-number
                                     (substring-no-properties
                                      v2 k2
                                      (prog1 j2
                                        (setq j2 (1+ j2) k2 j2))))))
                           (setq j2 (1+ j2)))
                         (string-to-number
                          (substring-no-properties v2 k2 j2))))
                 (cond ((< nv1 nv2) (throw 'br -1))
                       ((> nv1 nv2) (throw 'br 1))
                       ((= i (- n 1)) (throw 'br 0))
                       ((and (= j1 l1) (= j2 l2)) (throw 'br 0))
                       (t (setq i (1+ i))))))))))

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
