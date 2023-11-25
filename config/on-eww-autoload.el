;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-eww-autoload.el
;;;;


(eval-when-compile (require 'browse-url))


(defmacro when-feature-eww% (&rest body)
  "When \\=`eww\\=', do BODY."
  (if-feature-eww%
      `(progn% ,@body)))


;;; autoload
(when-feature-eww%
 (autoload 'browse-url-default-browser "browse-url")
 (autoload 'browse-url-url-encode-chars "browse-ulr"))


(when-feature-eww%

 (defun toggle-browser! (&optional arg)
   "Toggle default browser to `eww' or not.\n
With prefix argument ARG, `eww' as default browser if ARG is
non-nil, otherwise not.  See also: `browser-url-browser-function'."
   (interactive "P")
   (setq browse-url-browser-function
         (if (null arg)
             (if (eq browse-url-browser-function
                     'browse-url-default-browser)
                 #'eww-browse-url
               #'browse-url-default-browser)
           #'eww-browse-url))
   (message "eww as default browser %s"
            (if (eq browse-url-browser-function
                    'browse-url-default-browser)
                "disabled"
              "enabled"))))


(when-feature-eww%

 (defun set-eww-mode! ()
   (toggle-truncate-lines nil)))


(defalias '*web-defs*
  (lexical-let% ((b `(("bing" "https://www.bing.com/"
                       . "search?ensearch=1&q=")
                      ("duck" "https://duckduckgo.com/"
                       . "?q=")
                      ("google" "https://www.google.com/"
                       . "search?q=")
                      ("math" "https://mathworld.wolfram.com/"
                       . "search?query=")
                      ("so" "https://stackoverflow.com/"
                       . "search?q=")
                      ("wiki" "https://en.wikipedia.org/"
                       . "w/index.php?search="))))
    (lambda (&optional n)
      (if n (let ((x (assoc** (car n) b :test #'string=)))
              (if x (setcdr x (cdr n))
                (setq b (cons n b))))
        b)))
  "Searching engines using by \\=`lookup-web\\='.")

(defun on-eww-init! ()
  "On \\=`eww\\=' initialization."
  (add-hook 'eww-mode-hook #'set-eww-mode!)
  (when (consp (*web-defs*))
    (setq% eww-search-prefix
           (concat (car (cdar (*web-defs*)))
                   (cdr (cdar (*web-defs*)))))))

;;; `eww' after load
(when-feature-eww%
 (with-eval-after-load 'eww
   (on-eww-init!)))


;; end of `eww'


;;; `lookup-web' find web via search engine
;;; eww also has a new `eww-search-words' supports searching via web.


(defalias 'web-find-def
  (lexical-let% ((b '()))
    (lambda  (&optional en)
      (cond ((or en (not b))
             (setq b (assoc** (or en (caar (*web-defs*)))
                              (*web-defs*) :test #'string=)))
            (t b))))
  "Find WEB's definition in \\=`*web-defs*\\='.")


(defvar *lookup-web-history* nil
  "Searching history using by \\=`lookup-web\\='.")


(defun lookup-web (what &optional engine)
  "Lookup web via search ENGINE."
  (interactive
   (list (read-string "Lookup web for " (cdr (symbol@)))
         (when current-prefix-arg
           (let ((se (mapcar #'car (*web-defs*))))
             (completing-read (format "Choose (%s) "
                                      (mapconcat #'identity se "|"))
                              (mapcar #'car (*web-defs*))
                              nil nil (car *lookup-web-history*)
                              '*lookup-web-history* (car se))))))
  (let ((en (web-find-def engine)))
    (make-thread*
     (lambda ()
       (funcall browse-url-browser-function
                (let ((en1 (cdr en)))
                  (require 'browse-url)
                  (browse-url-url-encode-chars (concat (car en1)
                                                       (cdr en1)
                                                       what)
                                               "[ '()!`\"]")))))))

;;; `eww-search-words' and `webjump' more leaner than `lookup-web'.

(define-key% (current-global-map) (kbd "M-s w") #'lookup-web)
(define-key% (current-global-map) (kbd "C-c f w") #'lookup-web)
(define-key% (current-global-map) (kbd "M-s M-b") #'eww-list-bookmarks)

;; end of `lookup-web'


;; end of on-eww-autoload.el
