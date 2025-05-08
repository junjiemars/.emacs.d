;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; ewws.el
;;;;

;;; require

(require 'browse-url)

;; end of require

;;; env

(defun eww-spec->* (recipe spec)
  (cond ((and recipe (eq :bing recipe))
         (cond ((and spec (eq spec :web))
                "https://www.bing.com/")
               ((and spec (eq spec :qry))
                "search?ensearch=1&q=")))
        ((and recipe (eq :duck recipe))
         (cond ((and spec (eq spec :web))
                "https://duckduckgo.com/")
               ((and spec (eq spec :qry))
                "?q=")))
        ((and recipe (eq :google recipe))
         (cond ((and spec (eq spec :web))
                "https://www.google.com/")
               ((and spec (eq spec :qry))
                "search?q=")))
        ((and recipe (eq :math recipe))
         (cond ((and spec (eq spec :web))
                "https://mathworld.wolfram.com/")
               ((and spec (eq spec :qry))
                "search?query=")))
        ((and recipe (eq :so recipe))
         (cond ((and spec (eq spec :web))
                "https://stackoverflow.com/")
               ((and spec (eq spec :qry))
                "search?q=")))
        ((and recipe (eq :wiki recipe))
         (cond ((and spec (eq spec :web))
                "https://en.wikipedia.org/")
               ((and spec (eq spec :qry))
                "w/index.php?search=")))
        ((and recipe (eq :meta recipe))
         (cond ((and spec (eq spec :list))
                (mapcar #'keyword->string
                        `(:bing :duck :google :math :so :wiki)))))))

 ;; end of env

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
  (when-interactive%
    (message "eww as default browser %s"
             (if (eq browse-url-browser-function
                     'browse-url-default-browser)
                 "disabled"
               "enabled"))))

(defun eww*-truncate-lines ()
  "Disable \\=`eww\\=' truncate long lines."
  (toggle-truncate-lines nil))

(defun on-eww-init! ()
  "On \\=`eww\\=' initialization."
  (add-hook 'eww-mode-hook #'eww*-truncate-lines)
  (setq% eww-search-prefix (eww-spec->* :bing :web)))

;; end of `eww'

;;; `lookup-web' find web via search engine

;; eww also has a new `eww-search-words' supports searching via web.

(defvar *lookup-web-history* nil
  "Searching history using by \\=`lookup-web\\='.")

(defun lookup-web--prompt ()
  (let ((recipes (eww-spec->* :meta :list)))
    (list (read-string "Lookup web for " (symbol@*))
          (if current-prefix-arg
              (completing-read
               (format "Choose (%s) "
                       (mapconcat #'identity recipes "|"))
               (car recipes)
               nil nil (car *lookup-web-history*)
               '*lookup-web-history* (car recipes))
            (car recipes)))))

(defun lookup-web (what &optional recipe)
  "Lookup web via search RECIPE."
  (interactive (lookup-web--prompt))
  (make-thread*
   (lambda ()
     (funcall browse-url-browser-function
              (browse-url-url-encode-chars
               (concat (eww-spec->* recipe :web)
                       (eww-spec->* recipe :qry)
                       what)
               "[ '()!`\"]")))))

;; end of `lookup-web'

(provide 'ewws)

;; end of ewws.el
