;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; dicts.el
;;;;


(eval-when-compile
  (require 'url)
  (require 'url-util))


(defvar *dicts*
  '(("bing"
     (url . "https://cn.bing.com/dict/search?q=")
     (meta . ("<meta name=\"description\" content=\"必应词典为您提供.*的释义，"
              .
              "[^\"]+\""))))
  
  "Dictionaries using by `lookup-dict'.")


(defun on-lookup-dict (status &rest args)
  "Callback when `lookup-dict'."
  (declare (indent 1))
  (set-buffer-multibyte t)
  ;; (write-region (point-min) (point-max) (emacs-home* "private/dict.txt"))
  (let ((dict (remove-if* (lambda (x)
                            (eq 'url (car x)))
                  (cdadr (assoc** 'dict args #'eq))))
        (style (cdr (assoc** 'style args #'eq)))
        (ss nil))
    (dolist* (x style (nreverse ss))
      (goto-char (point-min))
      (let* ((re (cdr (assoc** x dict #'eq)))
             (b (re-search-forward (car re) nil t))
             (e (when b (re-search-forward (cdr re) nil t))))
        (when (and b e (< b e))
          (push (let ((html (cons x (buffer-substring-no-properties
                                     b (1- e)))))
                  ;; todo: translate html to text
                  html)
                ss))))
    (message (propertize (if (and ss (> (length ss) 0))
                             (mapconcat #'identity
                                        (mapcar #'cdr ss)
                                        "")
                           "No result")
                         'face
                         'font-lock-comment-face))))


(defun lookup-dict (what &optional dict)
  "Show the explanation of WORD from DICT in the echo area."
  (interactive
   (list (read-string "lookup dict for: " (cdr (symbol@)))
         (when current-prefix-arg
           (let* ((n (mapconcat #'identity
                                (mapcar #'car *dicts*)
                                "|"))
                  (d (read-string (format "Choose (%s): " n) n))
                  (dd (assoc** d *dicts* #'string=))
                  (s (mapcar (lambda (x)
                               (symbol-name (car x)))
                             (cdr dd)))
                  (sr (remove-if* (lambda (x)
                                    (string= "url" x))
                          s))
                  (ss (read-string (format "Choose (all|%s): "
                                           (mapconcat #'identity
                                                      sr
                                                      ",")))))
             `((dict . ,dd)
               (style . ,(if (string= "all" ss)
                             sr
                           ss)))))))
  (let* ((d1 (if (null dict)
                 `((dict ,(car *dicts*))
                   (style ,(remove-if* (lambda (x)
                                         (eq 'url x))
                               (mapcar #'car
                                       (cdr (car *dicts*))))))
               dict))
         (url (cdr (assoc** 'url (cdadr (assoc** 'dict d1 #'eq)) #'eq))))
    (url-retrieve (concat url (url-hexify-string what))
                  #'on-lookup-dict
                  `((what . ,(decode-coding-string what 'utf-8))
                    (dict . ,(cdr (assoc** 'dict d1 #'eq)))
                    (style . ,(cadr (assoc** 'style d1 #'eq))))
                  t
                  t)))


(define-key (current-global-map) (kbd "C-c d") #'lookup-dict)

;; end of dicts.el
