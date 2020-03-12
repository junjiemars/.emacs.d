;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; dicts.el
;;;;


(defvar *dicts*
  '(("bing"
     ("url" . "https://cn.bing.com/dict/search?q=")
     ("meta" . ("<meta name=\"description\" content=\"必应词典为您提供.+的释义，"
                "/><"
                .
                nil)))
    ("cambridge"
     ("url" . "https://dictionary.cambridge.org/dictionary/english-chinese-simplified/")
     ("pron-uk" . ("<span class=\"ipa dipa lpr-2 lpl-1\">"
                   "<"
                   .
                   nil))
     ("meta" . ("<meta itemprop=\"headline\" content=\".+translate: "
                "Learn"
                .
                (dict-fn-decode-char
                 dict-fn-decode-html-char)))))
  
  "Dictionaries using by `lookup-dict'.")

(defvar *dict-name-history* nil
  "Dictionary choosing history list.")

(defvar *dict-style-history* nil
  "Dictionary style choosing history list.")


(defun dict-fn-decode-char (ss)
  "Decode &#[0-9]+; to string."
  (with-temp-buffer
    (insert ss)
    (goto-char (point-min))
    (while (search-forward-regexp "&#\\([0-9]+\\);" nil t)
      (replace-match (char-to-string
                      (string-to-number (match-string 1)))))
    (buffer-substring-no-properties
     (point-min) (point-max))))

(defun dict-fn-decode-html-char (ss)
  "Decode &#[a-z]+; to string."
  (with-temp-buffer
    (insert ss)
    (let ((m '(("&nasp;" . " ")
               ("&#lt;" . "<")
               ("&#gt;" . ">")
               ("&hellip;" .  "..."))))
      (mapc (lambda (s)
              (goto-char (point-min))
              (while (search-forward (car s) nil t)
                (replace-match (cdr s))))
            m)
      (buffer-substring-no-properties
       (point-min) (point-max)))))

(defun dict-fn-remove-html-tag (ss)
  "Remove html tags."
  (with-temp-buffer
    (insert ss)
    (let ((tags `( "<.*?>"
                   "</[a-zA-Z]+>"
                   "/>")))
      (mapc (lambda (x)
              (goto-char (point-min))
              (while (search-forward-regexp x nil t)
                (replace-match "" t t)))
            tags)
      (buffer-substring-no-properties
       (point-min) (point-max)))))


(defun on-lookup-dict (status &rest args)
  "Callback after `lookup-dict'."
  (declare (indent 1))
  (when (or (null status) (assoc** :error status #'eq))
    (message (propertize "Network error" 'face 'font-lock-comment-face)))
  (set-buffer-multibyte t)
  (comment
   (write-region (point-min) (point-max)
                 (path! (emacs-home* ".dict/dict.log"))))
  (let* ((dict (cadr (assoc** 'dict args #'eq)))
         (style (cadr (assoc** 'style args #'eq)))
         (ss (mapcar
              (lambda (x)
                (goto-char (point-min))
                (let* ((re (cdr (assoc** x dict #'string=)))
                       (b (re-search-forward (car re) nil t))
                       (e (and b (re-search-forward (cadr re) nil t)))
                       (html (and b e (< b e)
                                  (buffer-substring-no-properties
                                   b (- e (length (cadr re))))))
                       (fns (cddr re))
                       (txt html))
                  (when (and (not (null html)) (> (length html) 0))
                    (dolist* (fn fns txt)
                      (setq txt (if (functionp fn)
                                    (funcall fn txt)
                                  txt)))
                    (cons x txt))))
              style)))
    (comment
     (save-sexp-to-file ss (path! (emacs-home* ".dict/lookup.log"))))
    (message (propertize
              (if (and ss (> (length ss) 0))
                  (string-trim> (mapconcat #'identity
                                           (mapcar #'cdr ss)
                                           " "))
                "No result")
              'face
              'font-lock-comment-face))))


(defun lookup-dict (what &optional dict)
  "Lookup WORD in DICT then show the result in the echo area."
  (interactive
   (list (read-string "lookup dict for: " (cdr (symbol@)))
         (when current-prefix-arg
           (let* ((ns (mapcar #'car *dicts*))
                  (d (read-string (format "Choose (%s): "
                                          (mapconcat #'identity ns "|"))
                                  (or (car *dict-name-history*)
                                      (car ns))
                                  '*dict-name-history*))
                  (dd (cdr (assoc** d *dicts* #'string=)))
                  (sr (remove** "url" (mapcar #'car dd) :test #'string=))
                  (ss (read-string
                       (format "Choose (all|%s): "
                               (mapconcat #'identity sr ","))
                       (or (car *dict-style-history*)
                           "all")
                       '*dict-style-history*)))
             `((dict . ,(list dd))
               (style . ,(if (and (stringp ss)
                                  (or (string= "all" ss)
                                      (match-string* "\\(all\\)" ss 1)))
                             `(, sr)
                           `(,(split-string* ss "," t "[ \n]*")))))))))
  (let* ((d1 (if (null dict)
                 (list (cons 'dict (list (cdar *dicts*)))
                       (cons 'style (list (remove** "url"
                                                    (mapcar #'car
                                                            (cdar *dicts*))
                                                    :test #'string=))))
               dict))
         (url (cdr (assoc** "url" (cadr (assoc** 'dict d1 #'eq))
                            #'string=))))
    (url-retrieve* (concat url (url-hexify-string what))
                   #'on-lookup-dict
                   d1
                   t
                   t)))


(define-key (current-global-map) (kbd "C-c f d") #'lookup-dict)


;; end of dicts.el
