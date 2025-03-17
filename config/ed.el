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
;; parse
;;;

(defun parse-xml-entity (str &optional dtd)
  (let ((es `(("&lt;"   . "<")
              ("&gt;"   . ">")
              ("&apos;" . "'")
              ("&quot;" . "\"")
              ("&amp;"  . "&")
              ("&#\\([0-9]+\\);" . 0)
              ,@dtd)))
    (dolist (e1 es str)
      (let ((start 0))
        (while (string-match (car e1) str start)
          (setq str (replace-match
                     (cond ((stringp (cdr e1)) (cdr e1))
                           ((integerp (cdr e1))
                            (char-to-string
                             (string-to-number
                              (substring-no-properties
                               str (match-beginning 1) (match-end 1)))))
                           ((functionp (cdr e1)) (funcall (cdr e1) str))
                           (t str))
                     nil nil str)
                start (match-beginning 0)))))))

 ;; end of parse

;;;
;; shell
;;;

(defun shell-format-region (&optional beg end buf)
  (let ((tmp (get-buffer-create* (symbol-name (gensym*)) t))
        (cur (with-current-buffer buf (point))))
    (unwind-protect
        (let ((rc (call-process-region
                   nil nil "clang-format" nil tmp nil
                   "-output-replacements-xml"
                   "-fallback-style" "gnu"
                   "-offset" (number-to-string (1- (position-bytes beg)))
                   "-length" (number-to-string
                              (- (position-bytes end) (position-bytes beg)))
                   "-cursor" (number-to-string (1- (position-bytes cur))))))
          (when (and rc (= rc 0))
            (with-current-buffer tmp
              (goto-char (point-min))
              (when (search-forward-regexp "incomplete_format='false'" nil t 1)
                (when (search-forward-regexp "<cursor>\\([0-9]+\\)</cursor>"
                                             nil t 1)
                  (setq cur (string-to-number
                             (buffer-substring-no-properties
                              (match-beginning 1) (match-end 1)))))
                (goto-char (point-max))
                (while (search-backward-regexp
                        (concat
                         "<replacement offset='\\([0-9]+\\)'"
                         " length='\\([0-9]+\\)'>\\(.*?\\)"
                         "</replacement>")
                        nil t)
                  (let ((off (string-to-number
                              (buffer-substring-no-properties
                               (match-beginning 1) (match-end 1))))
                        (len (string-to-number
                              (buffer-substring-no-properties
                               (match-beginning 2) (match-end 2))))
                        (txt (buffer-substring-no-properties
                              (match-beginning 3) (match-end 3))))
                    (with-current-buffer buf
                      (let ((lhs (byte-to-position (1+ off)))
                            (rhs (byte-to-position (1+ (+ off len)))))
                        (when (and (<= beg lhs) (>= end rhs))
                          (and (< lhs rhs) (delete-region lhs rhs))
                          (when txt
                            (goto-char lhs)
                            (insert (parse-xml-entity txt))))))))
                (and cur (setq cur (byte-to-position cur)))))))
      (and tmp (kill-buffer tmp))
      cur)))

(defun shell-format-buffer (&optional beg end)
  "Format the region in (BEG,END) of current buffer via SHELL\\=*."
  (interactive (if-region-active
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (cond ((when-feature% eglot
           (and (fboundp 'eglot-managed-p) (eglot-managed-p)))
         (eglot-format beg end))
        ((executable-find% "clang-format")
         (save-excursion
           (let ((cur (shell-format-region beg end (current-buffer))))
             (and cur (goto-char cur)))))
        (t (error "%s" "No formater found"))))

;; end of shell

;;;
;; version
;;;

(defun vstrncmp (v1 v2 &optional n)
  "Return 0 if V1 equals V2, < 0 if V1 less than V2, otherwise > 0.\n
If optional N is non-nil compare no more than N parts, default N is 4."
  (let ((n (or n 4)) (l1 (length v1)) (l2 (length v2)))
    (cond ((and (= l1 0) (= l2 0)) 0)
          ((and (= l1 0) (> l2 0))
           (- (string-to-number
               (substring-no-properties v2 0 (strchr v2 ?.)))))
          ((and (> l1 0) (= l2 0))
           (string-to-number (substring-no-properties v1 0 (strchr v1 ?.))))
          ((= n 1)
           (- (string-to-number
               (substring-no-properties v1 0 (strchr v1 ?.)))
              (string-to-number
               (substring-no-properties v2 0 (strchr v2 ?.)))))
          (t (let* ((i (strchr v1 ?.)) (j (strchr v2 ?.))
                    (s1 (string-to-number (substring-no-properties v1 0 i)))
                    (s2 (string-to-number (substring-no-properties v2 0 j))))
               (cond ((null (= s1 s2)) (- s1 s2))
                     ((and (null i) (null j)) (- s1 s2))
                     ((and (null i) j)
                      (vstrncmp
                       nil (substring-no-properties v2 (1+ j)) (1- n)))
                     ((and i (null j))
                      (vstrncmp
                       (substring-no-properties v1 (1+ i)) nil (1- n)))
                     (t (vstrncmp
                         (substring-no-properties v1 (1+ i))
                         (substring-no-properties v2 (1+ j))
                         (1- n)))))))))

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
