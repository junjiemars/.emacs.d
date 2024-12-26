;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; dict.el
;;;;

;;; require

;; end of require


(defalias '*dict-defs*
  (let
      ((b `(("bing"
             ("url" . "https://cn.bing.com/dict/search?q=")
             ("pron-us" . (("<meta name=\"description\".*?美\\[" . 1)
                           "\\].*?英\\[\\(.+?\\)\\]，"
                           . (dict-fn-norm-pron-us)))
             ("pron-uk" . (("<meta name=\"description\".*?美.*?英\\[" . 1)
                           "\\]，" . (dict-fn-norm-pron-uk)))
             ("meta" . (("<meta name=\"description\".*?英\\[\\(.+?\\)\\]，?")
                        "\" /><" . (dict-fn-norm-punc))))
            ("camb"
             ("url" . "https://dictionary.cambridge.org/dictionary/english/")
             ("pron-us" . (("<span class=\"ipa dipa lpr-2 lpl-1\">" . 2)
                           "<" . (dict-fn-norm-pron-us)))
             ("pron-uk" . (("<span class=\"ipa dipa lpr-2 lpl-1\">" . 1)
                           "<" . (dict-fn-norm-pron-uk)))
             ("meta" .
              (("<meta name=\"description\" content=\".*? definition: ")
               " Learn" . (dict-fn-decode-html-char))))
            ("longman"
             ("url" . "https://www.ldoceonline.com/dictionary/")
             ("pron-uk" . (("<span class=\"PRON\">")
                           "</span>" . (dict-fn-remove-html-tag
                                        dict-fn-norm-pron-uk)))
             ("meta" . (("<span class=\"DEF\">")
                        "</span>" . (dict-fn-remove-html-tag))))
            ("webster"
             ("url" . "https://www.merriam-webster.com/dictionary/")
             ("pron-us" . (("title=\"How to pronounce.*?(audio)\">" . 1)
                           "<img" . (dict-fn-decode-html-char
                                     dict-fn-norm-pron-us)))
             ("meta"
              .
              (("<meta name=\"description\" content=\"The meaning of ")
               " How to use" . (dict-fn-remove-html-tag)))))))
    (lambda (&optional n)
      (cond (n (let ((x (assoc-string (car n) b)))
                 (if x (setcdr x (cdr n))
                   (setq b (cons n b)))))
            (t b))))
  "Dictionaries using by \\=`lookup-dict\\='.")

(defvar *dict-name-history* nil
  "Dictionary choosing history list.")

(defvar *dict-style-history* nil
  "Dictionary style choosing history list.")

(defun dict-def-find (dict)
  "Find dictionary's definition in \\=`*dict-defs*\\='."
  (let ((dd (cdr (assoc-string (or (car dict)
                                   (car *dict-name-history*)
                                   (caar (*dict-defs*)))
                               (*dict-defs*)))))
    (list (cons 'dict (list dd))
          (cons 'style
                (or (cdr dict)
                    (list (let ((xs nil))
                            (dolist (x dd (nreverse xs))
                              (let ((x1 (car x)))
                                (unless (string= x1 "url")
                                  (setq xs (cons x1 xs))))))))))))

(defalias '*dict-debug-log*
  (let ((b `( :log nil ;; t
                       :dict ,(emacs-home% ".dict/dict.log")
                       :lookup ,(emacs-home% ".dict/lookup.log"))))
    (lambda (w &optional n)
      (cond (n (plist-put b w n))
            (t (plist-get b w)))))
  "Dictonary logging switch.")

(defun dict-fn-norm-pron-us (ss)
  "Normalize the pronounce of SS to us."
  (format "|%s|" (string-match* "^[/]?\\(.*?\\)[/]?$"
                                (string-trim>< ss) 1)))

(defun dict-fn-norm-pron-uk (ss)
  "Normalize the pronounce of SS to uk."
  (format "/%s/" (string-match* "^[/]?\\(.*?\\)[/]?$"
                                (string-trim>< ss) 1)))

(defun dict-fn-norm-punc (ss)
  "Normalize the punctuation of SS to en."
  (with-temp-buffer
    (insert ss)
    (let ((punc '(("，\s*" . ", ")
                  ("；\s*" . "; ")
                  ("：\s*" . ": ")
                  ("（" . "(")
                  ("）" . ")")
                  ("“" . "\"")
                  ("”" . "\"")
                  ("\n" . " ")
                  ("[ ]+" . " "))))
      (dolist (s punc)
        (goto-char (point-min))
        (while (search-forward-regexp (car s) nil t)
          (replace-match (cdr s))))
      (buffer-substring (point-min) (point-max)))))

(defun dict-fn-decode-char (ss)
  "Decode &#[0-9]+; to string."
  (with-temp-buffer
    (insert ss)
    (goto-char (point-min))
    (while (search-forward-regexp "&#\\([0-9]+\\);" nil t)
      (replace-match (char-to-string (string-to-number (match-string 1)))))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun dict-fn-decode-html-char (ss)
  "Decode &#[a-z]+; to string."
  (with-temp-buffer
    (insert ss)
    (let ((m '(("&nbsp;" . " ")
               ("&#lt;" . "<")
               ("&#gt;" . ">")
               ("&hellip;" .  "..."))))
      (dolist (s m)
        (goto-char (point-min))
        (while (search-forward (car s) nil t)
          (replace-match (cdr s))))
      (buffer-substring (point-min) (point-max)))))

(defun dict-fn-remove-html-tag (ss)
  "Remove html tags."
  (with-temp-buffer
    (insert ss)
    (let ((tags `( "<.*?>"
                   "</[a-zA-Z]+>"
                   "/>")))
      (dolist (x tags)
        (goto-char (point-min))
        (while (search-forward-regexp x nil t)
          (replace-match "" t t)))
      (string-trim><
       (buffer-substring-no-properties (point-min) (point-max))))))

;; end of definition


(defun dict-lookup-pipeline (dict style)
  "Pipeline dictionary lookup."
  (let ((xs nil))
    (dolist (x style (nreverse xs))
      (goto-char (point-min))
      (let* ((re (cdr (assoc-string x dict)))
             (b (re-search-forward (caar re) nil t (cdar re)))
             (e (and b (re-search-forward (cadr re) nil t)
                     (re-search-backward (cadr re) nil t)))
             (html (and b e (< b e)
                        (buffer-substring-no-properties b e)))
             (fns (cddr re))
             (txt html))
        (setq xs (cons (cons x (when (> (length html) 0)
                                 (dolist (fn fns txt)
                                   (if (functionp fn)
                                       (setq txt (funcall fn txt))
                                     txt))))
                       xs))))))

(defun on-lookup-dict (status &rest args)
  "Callback after \\=`lookup-dict\\='."
  (declare (indent 1))
  (let ((err (plist-get :error status)))
    (when err
      (kill-buffer)
      (error "Panic, %s" err)))
  (set-buffer-multibyte t)
  (when (*dict-debug-log* :log)
    (write-region (point-min) (point-max) (path! (*dict-debug-log* :dict))))
  (let* ((dict (cadr (assq 'dict args)))
         (style (cadr (assq 'style args)))
         (ss (dict-lookup-pipeline dict style)))
    (when (*dict-debug-log* :log)
      (save-sexp-to-file ss (path! (*dict-debug-log* :lookup))))
    (kill-buffer (current-buffer))
    (message "%s" (if (car ss)
                      (propertize (string-trim>
                                   (mapconcat #'identity
                                              (mapcar #'cdr ss)
                                              " "))
                                  'face 'font-lock-comment-face)
                    (propertize "No match"
                                'face 'font-lock-warning-face)))
    (when-var% execute-extended-command--binding-timer simple
      (sit-for 30))))

;; end of `on-lookup-dict'

(defmacro url-retrieve*
    (url callback &optional cbargs silent inhibit-cookies)
  "Retrieve URL asynchronously and call CALLBACK with CBARGS when
finished."
  (when-fn% url-retrieve url
    (if-version%
        <= 24
        `(url-retrieve ,url ,callback ,cbargs ,silent ,inhibit-cookies)
      (ignore* silent inhibit-cookies)
      `(url-retrieve ,url ,callback ,cbargs))))

(defun dict-lookup-retrieve (what url def)
  (let ((url-history-track nil))
    (when-version% > 25
      (ignore* url-history-track))
    (url-retrieve*
     (concat url (url-hexify-string what))
     #'on-lookup-dict def t t)))

(defun lookup-dict (what &optional dict)
  "Lookup WORD in DICT then show the result in the echo area."
  (interactive
   (list (read-string "Lookup dict for " (cdr (symbol@ 'word)))
         (when current-prefix-arg
           (let* ((ns (mapcar #'car (*dict-defs*)))
                  (d (completing-read
                      (format "Choose (%s) " (mapconcat #'identity ns "|"))
                      ns nil nil
                      (or (car *dict-name-history*)
                          (car ns))
                      '*dict-name-history*
                      (car ns)))
                  (dd (cdr (assoc-string d (*dict-defs*))))
                  (sr (let ((xs nil))
                        (dolist (x dd (nreverse xs))
                          (let ((x1 (car x)))
                            (unless (string= x1 "url")
                              (setq xs (cons x1 xs)))))))
                  (ss (completing-read
                       (format "Choose (all|%s) "
                               (mapconcat #'identity sr ","))
                       (cons "all" sr) nil nil
                       (car *dict-style-history*)
                       '*dict-style-history*
                       (car sr))))
             (cons d (if (and (stringp ss)
                              (or (string= "all" ss)
                                  (string-match* "\\(all\\)" ss 1)))
                         `(, sr)
                       `(,(split-string* ss "," t "[ \n]*"))))))))
  (let* ((def (dict-def-find dict))
         (url (cdr (assoc-string "url" (cadr (assq 'dict def))))))
    (make-thread* (lambda () (dict-lookup-retrieve what url def)))))

;; end of `lookup-dict'



(provide 'dict)

;; end of dict.el
