;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; dict.el
;;;;
;; Commentary: lookup dictionary
;;;;


;;; require

;; end of require

;;;
;; env
;;;

(defun dict-spec->* (dict spec)
  (cond ((and dict (eq dict :bing))
         (cond ((and spec (eq spec :url))
                "https://cn.bing.com/dict/search?q=")
               ((and spec (eq spec :AmE))
                `(("<meta name=\"description\".*?美\\[" . 1)
                  "\\].*?英\\[\\(.+?\\)\\]，" . (dict--norm-AmE)))
               ((and spec (eq spec :BrE))
                `(("<meta name=\"description\".*?美.*?英\\[" . 1)
                  "\\]，" . (dict--norm-BrE)))
               ((and spec (eq spec :meta))
                `(("<meta name=\"description\".*?英\\[\\(.+?\\)\\]，?")
                  "\" /><" . (dict--norm-punc)))))
        ((and dict (eq dict :cambridge))
         (cond ((and spec (eq spec :url))
                "https://dictionary.cambridge.org/dictionary/english/")
               ((and spec (eq spec :AmE))
                `(("<span class=\"ipa dipa lpr-2 lpl-1\">" . 2)
                  "<" . (dict--norm-AmE)))
               ((and spec (eq spec :BrE))
                `(("<span class=\"ipa dipa lpr-2 lpl-1\">" . 1)
                  "<" . (dict--norm-BrE)))
               ((and spec (eq spec :meta))
                `(("<meta name=\"description\" content=\".*? definition: ")
                  " Learn" . (dict--decode-html-char)))))
        ((and dict (eq dict :longman))
         (cond ((and spec (eq spec :url))
                "https://www.ldoceonline.com/dictionary/")
               ((and spec (eq spec :BrE))
                `(("<span class=\"PRON\">")
                  "</span>" . (dict--remove-html-tag
                               dict--norm-BrE)))
               ((and spec (eq spec :meta))
                `(("<span class=\"DEF\">")
                  "</span>" . (dict--remove-html-tag)))))
        ((and dict (eq dict :webster))
         (cond ((and spec (eq spec :url))
                "https://www.merriam-webster.com/dictionary/")
               ((and spec (eq spec :AmE))
                `(("title=\"How to pronounce.*?(audio)\">" . 1)
                  "<img" . (dict--decode-html-char
                            dict--norm-AmE)))
               ((and spec (eq spec :meta))
                `(("<meta name=\"description\" content=\"The meaning of ")
                  " How to use" . (dict--remove-html-tag)))))
        ((and dict (eq dict :meta))
         (cond ((and spec (eq spec :list))
                (mapcar #'keyword->string
                        `(:bing :cambridge :longman :webster)))
               ((and spec (eq spec :bing)) `(:AmE :BrE :meta))
               ((and spec (eq spec :cambridge)) `(:AmE :BrE :meta))
               ((and spec (eq spec :longman)) `(:BrE :meta))
               ((and spec (eq spec :webster)) `(:AmE :meta))))))

(defvar *dict-name-history* nil
  "Dictionary choosing history list.")

(defvar *dict-spec-history* nil
  "Dictionary spec choosing history list.")

(defalias '*dict-debug-log*
  (let ((b `( :log nil ;; t
              :dict ,(emacs-home% ".dict/dict.log")
              :lookup ,(emacs-home% ".dict/lookup.log"))))
    (lambda (w &optional n)
      (cond (n (plist-put b w n))
            (t (plist-get b w)))))
  "Dictonary logging switch.")

;; end of env

;;;
;; pipeline
;;;

(defun dict--norm-AmE (ss _)
  (format "|%s|"
          (string-match* "^[/]?\\(.*?\\)[/]?$" (string-trim>< ss) 1)))

(defun dict--norm-BrE (ss _)
  (format "/%s/"
          (string-match* "^[/]?\\(.*?\\)[/]?$" (string-trim>< ss) 1)))

(defun dict--norm-punc (ss _)
  (strawk ss `(("，\s*" . ", ")
               ("；\s*" . "; ")
               ("：\s*" . ": ")
               ("（" . "(")
               ("）" . ")")
               ("“" . "\"")
               ("”" . "\"")
               ("\n" . " ")
               ("[ ]+" . " "))))

(defun dict--decode-html-char (ss _)
  (strawk ss `(("&nbsp;" . " ")
               ("&#lt;" . "<")
               ("&#gt;" . ">")
               ("&hellip;" .  "..."))))

(defun dict--remove-html-tag (ss _)
  (strawk ss `(("^[ ]+" . nil)
               ("<.*?>" . nil)
               ("</[a-zA-Z]+>" . nil)
               ("/>" . nil))))

(defun dict--lookup-pipeline (what dict specs)
  (let ((xs nil))
    (dolist (x specs (nreverse xs))
      (goto-char (point-min))
      (let ((re (dict-spec->* dict x)))
        (when re
          (let* ((b (re-search-forward (caar re) nil t (cdar re)))
                 (e (and b (re-search-forward (cadr re) nil t)
                         (re-search-backward (cadr re) nil t)))
                 (html (and b e (< b e) (buffer-substring-no-properties b e)))
                 (fns (cddr re))
                 (txt html))
            (setq xs (cons (cons x (when (> (length html) 0)
                                     (dolist (fn fns txt)
                                       (if (functionp fn)
                                           (setq txt (funcall fn txt what))
                                         txt))))
                           xs))))))))

;; end of pipeline

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
  (let ((ss (dict--lookup-pipeline (plist-get args :what)
                                   (plist-get args :dict)
                                   (plist-get args :specs))))
    (when (*dict-debug-log* :log)
      (write-sexp-to-file ss (path! (*dict-debug-log* :lookup))))
    (kill-buffer (current-buffer))
    (let ((txt (string-trim> (mapconcat #'identity (mapcar #'cdr ss) " "))))
      (message "%s" (if (> (length txt) 0)
                        (propertize txt 'face 'font-lock-comment-face)
                      (propertize "No match" 'face 'font-lock-warning-face))))
    (when-var% execute-extended-command--binding-timer simple
      (sit-for 30))))

;; end of `on-lookup-dict'

(defmacro url-retrieve*
    (url callback &optional cbargs silent inhibit-cookies)
  "Retrieve URL asynchronously and call CALLBACK with CBARGS when finished."
  (if-version%
      <= 24
      `(url-retrieve ,url ,callback ,cbargs ,silent ,inhibit-cookies)
    (ignore* silent inhibit-cookies)
    `(url-retrieve ,url ,callback ,cbargs)))

(defun dict-lookup-retrieve (what dict specs)
  "Return the meta of WHAT."
  (let ((url-history-track nil)
        (url (dict-spec->* dict :url))
        (cbargs (list :what what :dict dict :specs specs)))
    (when-version% > 25
      (ignore* url-history-track))
    (url-retrieve*
     (concat url (url-hexify-string what)) #'on-lookup-dict cbargs t t)))

(defun dict--lookup-prompt ()
  (list
   (read-string "Lookup dict for " (symbol@* 'word))
   (let ((dict nil) (specs nil) (all "all"))
     (when current-prefix-arg
       (setq dict
             (let ((ds (dict-spec->* :meta :list)))
               (completing-read
                (format "Choose (%s) " (mapconcat #'identity ds "|"))
                ds
                nil nil
                (or (car *dict-name-history*) (car ds))
                '*dict-name-history*
                (car ds)))
             specs
             (let ((ss (mapcar
                        #'keyword->string
                        (dict-spec->* :meta (string->keyword dict)))))
               (completing-read
                (format "Choose (all|%s) " (mapconcat #'identity ss ","))
                (cons all ss)
                nil nil
                (car *dict-spec-history*)
                '*dict-spec-history*
                (mapconcat #'identity ss ",")))))
     (setq dict
           (cond ((and (null dict) (null (car *dict-name-history*))) :bing)
                 ((car *dict-name-history*)
                  (string->keyword (car *dict-name-history*)))
                 (t (string->keyword dict)))
           specs
           (cond ((and (null specs) (null (car *dict-spec-history*)))
                  (dict-spec->* :meta dict))
                 ((string-equal all specs) (dict-spec->* :meta dict))
                 ((string-equal all (car *dict-spec-history*))
                  (dict-spec->* :meta dict))
                 (t (let ((ss (split-string*
                               (or specs (car *dict-spec-history*))
                               "," t "[ \n]*")))
                      (mapcar #'string->keyword ss)))))
     (cons dict specs))))

(defun lookup-dict (what &optional dict)
  "Lookup WORD in DICT then show the result in the echo area."
  (interactive (dict--lookup-prompt))
  (make-thread*
   (lambda ()
     (dict-lookup-retrieve what (car dict) (cdr dict)))))

;; end of `lookup-dict'

(provide 'dict)

;; end of dict.el
