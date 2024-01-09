;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; ewws.el
;;;;


(eval-when-compile (require 'browse-url))

;;; autoload
(autoload 'browse-url-default-browser "browse-url")
(autoload 'browse-url-url-encode-chars "browse-url")

(defun toggle-browser! (&optional arg)
  "Toggle default browser.\n
With prefix argument ARG, \\=`eww\\=' as default browser if ARG
is non-nil, otherwise is not. See also:
\\=`browser-url-browser-function\\='."
  (interactive "P")
  (setq browse-url-browser-function
        (if (null arg)
            (if (eq browse-url-browser-function
                    'browse-url-default-browser)
                #'eww-browse-url
              #'browse-url-default-browser)
          #'eww-browse-url))
  (unless-noninteractive%
   (message "eww as default browser %s"
            (if (eq browse-url-browser-function
                    'browse-url-default-browser)
                "disabled"
              "enabled"))))

(defun eww*-truncate-lines ()
  "Disable \\=`eww\\=' truncate long lines."
  (toggle-truncate-lines nil))

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
      (if n (let ((x (assoc-string (car n) b)))
              (if x (setcdr x (cdr n))
                (setq b (cons n b))))
        b)))
  "Searching engines using by \\=`lookup-web\\='.")

(defun on-eww-init! ()
  "On \\=`eww\\=' initialization."
  (add-hook 'eww-mode-hook #'eww*-truncate-lines)
  (when (consp (*web-defs*))
    (setq% eww-search-prefix
           (concat (car (cdar (*web-defs*)))
                   (cdr (cdar (*web-defs*)))))))

;; end of `eww'

;;; `lookup-web' find web via search engine
;;; eww also has a new `eww-search-words' supports searching via web.

(defalias 'web-find-def
  (lexical-let% ((b '()))
    (lambda  (&optional en)
      (cond ((or en (not b))
             (setq b (assoc-string
                      (or en (caar (*web-defs*))) (*web-defs*))))
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

;; end of `lookup-web'

(provide 'ewws)

;; end of ewws.el
