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
             ("pron-us" . (("<meta name=\"description\".*?美\\[" . 1)
                           "\\].*?英\\[\\(.+?\\)\\]，"
                           . (dict-fn-norm-pron-us)))
             ("pron-uk" . (("<meta name=\"description\".*?美.*?英\\[" . 1)
                           "\\]，" . (dict-fn-norm-pron-uk)))
             ("meta" . (("<meta name=\"description\".*?英\\[\\(.+?\\)\\]，?")
                        "\" /><" . (dict-fn-norm-punc)))
             ("sounds-like" . (("<div class=\"df_wb_a\">音近词</div>")
                               "</div></div></div>"
                               . (dict-fn-remove-html-tag)))
             ("spelled-like" . (("<div class=\"df_wb_a\">形近词</div>")
                                "</div></div></div>"
                                . (dict-fn-remove-html-tag))))
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
             ("meta" . (("<meta name=\"description\" content=\"The meaning of ")
                        " How to use" . (dict-fn-remove-html-tag)))
             ("suggestion" . (("<p class=\"spelling-suggestions\">" . 1)
                              "</div>" . (dict-fn-remove-html-tag
                                          dict-fn-norm-punc)))))))
    (lambda (&optional n)
      (if n (let ((x (assoc** (car n) b :test #'string=)))
              (if x (setcdr x (cdr n))
                (setq b (cons n b))))
        b)))
  "Dictionaries using by \\=`lookup-dict\\='.")


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
  "Find DICT's definition in \\=`*dict-defs*\\='.")


(defalias '*dict-debug-log*
  (lexical-let% ((b `(:logging nil ;; t
                      :dict ,(emacs-home* ".dict/dict.log")
                      :lookup ,(emacs-home* ".dict/lookup.log"))))
    (lambda (w &optional n)
      (if n (plist-put b w n) (plist-get b w))))
  "Dictonary logging switch.")

(defvar *dict-name-history* nil
  "Dictionary choosing history list.")

(defvar *dict-style-history* nil
  "Dictionary style choosing history list.")

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
    (let ((m '(("&nbsp;" . " ")
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

;; end of definition

(defun on-lookup-dict (status &rest args)
  "Callback after \\=`lookup-dict\\='."
  (declare (indent 1))
  (let ((err (plist-get :error status)))
    (when err
      (message "%s" (propertize "Network error"
                                'face
                                'font-lock-warning-face))
      (kill-buffer)
      (user-error "!%s in on-lookup-dict" err)))
  (set-buffer-multibyte t)
  (when (*dict-debug-log* :logging)
    (write-region (point-min) (point-max)
                  (path! (*dict-debug-log* :dict))))
  (let* ((dict (cadr (assoc** 'dict args :test #'eq)))
         (style (cadr (assoc** 'style args :test #'eq)))
         (ss (mapcar
              (lambda (x)
                (goto-char (point-min))
                (let* ((re (cdr (assoc** x dict :test #'string=)))
                       (b (re-search-forward (caar re) nil t (cdar re)))
                       (e (and b (re-search-forward (cadr re) nil t)
                               (re-search-backward (cadr re) nil t)))
                       (html (and b e (< b e)
                                  (buffer-substring-no-properties b e)))
                       (fns (cddr re))
                       (txt html))
                  (cons x (when (> (length html) 0)
                            (dolist* (fn fns txt)
                              (if (functionp fn)
                                  (setq txt (funcall fn txt))
                                txt))))))
              style)))
    (when (*dict-debug-log* :logging)
      (save-sexp-to-file ss (path! (*dict-debug-log* :lookup))))
    (message "%s" (if (car ss)
                      (propertize (string-trim>
                                   (mapconcat #'identity
                                              (mapcar #'cdr ss)
                                              " "))
                                  'face 'font-lock-comment-face)
                    (propertize "No match"
                                'face 'font-lock-warning-face)))))

;; end of `on-lookup-dict'


(defun lookup-dict (what &optional dict)
  "Lookup WORD in DICT then show the result in the echo area."
  (interactive
   (list (read-string "Lookup dict for " (cdr (symbol@ 'word)))
         (when current-prefix-arg
           (let* ((ns (mapcar #'car (*dict-defs*)))
                  (d (completing-read
                      (format "Choose (%s) "
                              (mapconcat #'identity ns "|"))
                      ns nil nil (car *dict-name-history*)
                      '*dict-name-history* (car ns)))
                  (dd (cdr (assoc** d (*dict-defs*) :test #'string=)))
                  (sr (remove-if* (lambda (x) (string= x "url"))
                                  (mapcar #'car dd)))
                  (ss (completing-read
                       (format "Choose (all|%s) "
                               (mapconcat #'identity sr ","))
                       (cons "all" sr) nil nil (car *dict-style-history*)
                       '*dict-style-history* (car sr))))
             (cons d (if (and (stringp ss)
                              (or (string= "all" ss)
                                  (string-match* "\\(all\\)" ss 1)))
                         `(, sr)
                       `(,(split-string* ss "," t "[ \n]*"))))))))
  (let* ((d1 (dict-find-def dict))
         (url (cdr (assoc** "url" (cadr (assoc** 'dict d1 :test #'eq))
                            :test #'string=))))
    (make-thread* (lambda ()
                    (url-retrieve*
                     (concat url (url-hexify-string what))
                     #'on-lookup-dict d1 t t)))))

;; end of `lookup-dict'


(define-key% (current-global-map) (kbd "M-s d") 'lookup-dict)

(provide 'dict)

;; end of dict.el
