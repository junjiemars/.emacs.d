;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; dicts.el
;;;;


(eval-when-compile
  (require 'url)
  (require 'url-util)
  (require 'xml))


(defvar *dicts*
  '(("bing"
     ("url" . "https://cn.bing.com/dict/search?q=")
     ("meta" . ("<meta name=\"description\" content=\"必应词典为您提供.+的释义，"
                "[^\"]+\""
                .
                nil)))
    ("cambridge"
     ("url" . "https://dictionary.cambridge.org/dictionary/english-chinese-simplified/")
     ("meta" . ("<meta itemprop=\"headline\" content=\".+translate: "
                "[^L]+L"
                .
                "xml"))))
  
  "Dictionaries using by `lookup-dict'.")


(defun on-lookup-dict (status &rest args)
  "Callback when `lookup-dict'."
  (declare (indent 1))
  (when (or (null status) (assoc** :error status #'eq))
    (message (propertize "Network error" 'face 'font-lock-comment-face)))
  (set-buffer-multibyte t)
  (write-region (point-min) (point-max)
                (path! (emacs-home* ".dict/log")))
  (let ((dict (remove-if* (lambda (x) (eq 'url (car x)))
                  (cadr (assoc** 'dict args #'eq))))
        (style (cadr (assoc** 'style args #'eq)))
        (ss nil))
    (dolist* (x style (nreverse ss))
      (goto-char (point-min))
      (let* ((re (cdr (assoc** x dict #'string=)))
             (b (re-search-forward (car re) nil t))
             (e (when b (re-search-forward (cadr re) nil t)))
             (d (cddr re)))
        (when (and b e (< b e))
          (push (cons x (let ((html (buffer-substring-no-properties
                                     b (1- e))))
                          (cond ((string= "xml" d)
                                 (with-temp-buffer
                                   (save-excursion
                                     (insert "<foo>" html "</foo>"))
                                   (with-no-warnings
                                     (require 'xml)
                                     (string-trim>< (xml-parse-string)
                                                    "</foo>"
                                                    "<foo>"))))
                                (t html))))
                ss))))
    (message (propertize (if (and ss (> (length ss) 0))
                             (string-trim> (mapconcat #'identity
                                                      (mapcar #'cdr ss)
                                                      "[ \n]"))
                           "No result")
                         'face
                         'font-lock-comment-face))))


(defun lookup-dict (what &optional dict)
  "Show the explanation of WORD from DICT in the echo area."
  (interactive
   (list (read-string "lookup dict for: " (cdr (symbol@)))
         (when current-prefix-arg
           (let* ((ns (mapcar #'car *dicts*))
                  (d (read-string (format "Choose (%s): "
                                          (mapconcat #'identity ns "|"))
                                  (car ns)))
                  (dd (cdr (assoc** d *dicts* #'string=)))
                  (s (mapcar #'car dd))
                  (sr (remove-if* (lambda (x) (string= "url" x)) s))
                  (ss (read-string
                       (format "Choose (all|%s): "
                               (mapconcat #'identity sr ",")))))
             `((dict . ,(list dd))
               (style . ,(if (and (stringp ss)
                                  (string= "all" ss))
                             sr
                           `(,(remove-if* (lambda (x)
                                            (string= "all" x))
                                  (split-string* ss "," t "[ \n]*"))))))))))
  (let* ((d1 (if (null dict)
                 (list (cons 'dict (list (cdar *dicts*)))
                       (cons 'style (list (remove-if* (lambda (x)
                                                        (string= "url" x))
                                              (mapcar #'car
                                                      (cdar *dicts*))))))
               dict))
         (url (cdr (assoc** "url" (cadr (assoc** 'dict d1 #'eq))
                            #'string=))))
    (url-retrieve (concat url (url-hexify-string what))
                  #'on-lookup-dict
                  d1
                  t
                  t)))


(define-key (current-global-map) (kbd "C-c d") #'lookup-dict)


;; end of dicts.el
