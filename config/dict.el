;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; dict.el
;;;;


(defalias '*dict-defs*
  (lexical-let%
      ((b `(("bing"
             ("url" . "https://cn.bing.com/dict/search?q=")
             ("meta"
              .
              (("<meta name=\"description\" content=\"必应词典为您提供.+的释义，")
               "\" /><" .
               (,(lambda (ss)
                   (with-temp-buffer
                     (insert ss)
                     (let ((s1 '(("美\\[\\(.+?\\)\\]" . "|%s|")
                                 ("，?英\\[\\(.+?\\)\\]，?" . " /%s/ "))))
                       (mapc
                        (lambda (s)
                          (goto-char (point-min))
                          (while (search-forward-regexp (car s) nil t)
                            (replace-match (format (cdr s)
                                                   (match-string 1)))))
                        s1)
                       (buffer-substring (point-min) (point-max)))))
                dict-fn-norm-zh-punc)))
             ("sounds-like" . (("<div class=\"df_wb_a\">音近词</div>")
                               "</div></div></div>"
                               .
                               (dict-fn-remove-html-tag)))
             ("spelled-like" . (("<div class=\"df_wb_a\">形近词</div>")
                                "</div></div></div>" .
                                (dict-fn-remove-html-tag))))
            ("camb/zh"
             ("url"
              .
              "https://dictionary.cambridge.org/dictionary/english-chinese-simplified/")
             ("pron-us" . (("<span class=\"ipa dipa lpr-2 lpl-1\">" . 2)
                           "<" .
                           (,(lambda (x)
                               (format "|%s|" x)))))
             ("pron-uk" . (("<span class=\"ipa dipa lpr-2 lpl-1\">" . 1)
                           "<" .
                           (,(lambda (x)
                               (format "/%s/" x)))))
             ("meta" . (("<meta itemprop=\"headline\" content=\".+translate: ")
                        "Learn" .
                        (dict-fn-decode-char
                         dict-fn-decode-html-char
                         dict-fn-norm-zh-punc))))
            ("camb/en"
             ("url" . "https://dictionary.cambridge.org/dictionary/english/")
             ("pron-us" . (("<span class=\"ipa dipa lpr-2 lpl-1\">" . 2)
                           "<" .
                           (,(lambda (x)
                               (format "|%s|" x)))))
             ("pron-uk" . (("<span class=\"ipa dipa lpr-2 lpl-1\">" . 1)
                           "<"
                           .
                           (,(lambda (x)
                               (format "/%s/" x)))))
             ("meta"
              .
              (("<meta name=\"description\" content=\".*? definition: ")
               "Learn" .
               (dict-fn-decode-html-char))))
            ("longman"
             ("url" . "https://www.ldoceonline.com/dictionary/")
             ("pron-uk" . (("<span class=\"PRON\">")
                           "</span>" .
                           (dict-fn-remove-html-tag
                            ,(lambda (x)
                               (format "/%s/" x)))))
             ("meta" . (("<span class=\"DEF\">")
                        "</span>" .
                        (dict-fn-remove-html-tag)))))))
    (lambda (&optional n)
      (if n (let ((x (assoc** (car n) b :test #'string=)))
              (if x (setcdr x (cdr n))
                (setq b (cons n b))))
        b)))
  "Dictionaries using by `lookup-dict'.")


(defalias 'dict-find-def
  (lexical-let% ((b))
    (lambda  (&optional dict)
      (cond
       ((or (consp dict) (not b))
        (setq b
              (let ((dd (cdr (assoc** (or (car dict)
                                          (caar (*dict-defs*)))
                                      (*dict-defs*) :test #'string=))))
                (list (cons 'dict (list dd))
                      (cons 'style
                            (or (cdr dict)
                                (list (remove-if*
                                       (lambda (x) (string= x "url"))
                                       (mapcar #'car dd)))))))))
       (t b))))
  "Find DICT's definition in `*dict-defs*'.")


(defalias '*dict-debug-log*
  (lexical-let% ((b `(logging  nil
                      dict ,(emacs-home* ".dict/dict.log")
                      lookup ,(emacs-home* ".dict/lookup.log"))))
    (lambda (w &optional n)
      (if n (plist-put b w n) (plist-get b w))))
  "Dictonary logging switch.")

(defvar *dict-name-history* nil
  "Dictionary choosing history list.")

(defvar *dict-style-history* nil
  "Dictionary style choosing history list.")


(defun dict-fn-norm-zh-punc (ss)
  "Replace zh's punctuations to en's."
  (with-temp-buffer
    (insert ss)
    (let ((punc '(("，\s*" . ", ")
                  ("；\s*" . "; ")
                  ("：\s*" . ": ")
                  ("（" . "(")
                  ("）" . ")")
                  ("“" . "\"")
                  ("”" . "\""))))
      (dolist* (s punc)
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
      (replace-match (char-to-string
                      (string-to-number (match-string 1)))))
    (buffer-substring (point-min) (point-max))))

(defun dict-fn-decode-html-char (ss)
  "Decode &#[a-z]+; to string."
  (with-temp-buffer
    (insert ss)
    (let ((m '(("&nasp;" . " ")
               ("&#lt;" . "<")
               ("&#gt;" . ">")
               ("&hellip;" .  "..."))))
      (dolist* (s m)
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
      (dolist* (x tags)
        (goto-char (point-min))
        (while (search-forward-regexp x nil t)
          (replace-match "" t t)))
      (buffer-substring (point-min) (point-max)))))


(defun on-lookup-dict (status &rest args)
  "Callback after `lookup-dict'."
  (declare (indent 1))
  (let ((err (plist-get :error status)))
    (when err
      (message "%s" (propertize "Network error"
                                'face
                                'font-lock-warning-face))
      (kill-buffer)
      (user-error* "!%s in on-lookup-dict" err)))
  (set-buffer-multibyte t)
  (when (*dict-debug-log* 'logging)
    (write-region (point-min) (point-max)
                  (path! (*dict-debug-log* 'dict))))
  (let* ((dict (cadr (assoc** 'dict args :test #'eq)))
         (style (cadr (assoc** 'style args :test #'eq)))
         (ss (mapcar
              (lambda (x)
                (goto-char (point-min))
                (let* ((re (cdr (assoc** x dict :test #'string=)))
                       (b (re-search-forward (caar re) nil t (cdar re)))
                       (e (and b (re-search-forward (cadr re) nil t)))
                       (html (and b e (< b e)
                                  (buffer-substring-no-properties
                                   b (- e (length (cadr re))))))
                       (fns (cddr re))
                       (txt html))
                  (cons x (when (and (not (null html)) (> (length html) 0))
                            (dolist* (fn fns txt)
                              (if (functionp fn)
                                  (setq txt (funcall fn txt))
                                txt))))))
              style)))
    (when (*dict-debug-log* 'logging)
      (save-sexp-to-file ss (path! (*dict-debug-log* 'lookup))))
    (message "%s" (if (car ss)
                      (propertize (string-trim> (mapconcat #'identity
                                                           (mapcar #'cdr ss)
                                                           " "))
                                  'face 'font-lock-comment-face)
                    (propertize "No match"
                                'face 'font-lock-warning-face)))))


(defun lookup-dict (what &optional dict)
  "Lookup WORD in DICT then show the result in the echo area."
  (interactive
   (list (read-string "Lookup dict for " (cdr (symbol@ 'word)))
         (when current-prefix-arg
           (let* ((ns (mapcar #'car (*dict-defs*)))
                  (d (read-string (format "Choose (%s) "
                                          (mapconcat #'identity ns "|"))
                                  (or (car *dict-name-history*)
                                      (car ns))
                                  '*dict-name-history*))
                  (dd (cdr (assoc** d (*dict-defs*) :test #'string=)))
                  (sr (remove-if* (lambda (x) (string= x "url"))
                                  (mapcar #'car dd)))
                  (ss (read-string
                       (format "Choose (all|%s) "
                               (mapconcat #'identity sr ","))
                       (or (car *dict-style-history*)
                           "all")
                       '*dict-style-history*)))
             (cons d (if (and (stringp ss)
                              (or (string= "all" ss)
                                  (match-string* "\\(all\\)" ss 1)))
                         `(, sr)
                       `(,(split-string* ss "," t "[ \n]*"))))))))
  (let* ((d1 (dict-find-def dict))
         (url (cdr (assoc** "url" (cadr (assoc** 'dict d1 :test #'eq))
                            :test #'string=))))
    (make-thread* (lambda ()
                    (url-retrieve*
                     (concat url (url-hexify-string what))
                     #'on-lookup-dict d1 t t)))))



(provide 'dict)

;; end of dict.el
