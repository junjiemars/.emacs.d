;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-eww-autoload.el
;;;;


(eval-when-compile (require 'browse-url))


(if-feature-eww%
  
  (defun toggle-browser! (&optional arg)
    "Toggle default browser to `eww' or not.
With prefix argument ARG, `eww' as default browser if ARG is
non-nil, otherwise not.  See also: `browser-url-browser-function'."
    (interactive "P")
    (setq browse-url-browser-function
          (if (null arg)
              (if (eq browse-url-browser-function 'browse-url-default-browser)
                  #'eww-browse-url
                #'browse-url-default-browser)
            #'eww-browse-url))
    (message "eww as default browser %s"
             (if (eq browse-url-browser-function 'browse-url-default-browser)
                 "disabled"
               "enabled"))))


(if-feature-eww%

  (defun set-eww-mode! ()
    (toggle-truncate-lines nil)))


(if-feature-eww%

    (with-eval-after-load 'eww
      (add-hook 'eww-mode-hook #'set-eww-mode!)))


;; find web via search engine

(defvar *search-engines*
  '(("bing" "https://www.bing.com/"
     . "search?ensearch=1&q=")
    ("duck" "https://duckduckgo.com/"
     . "?q=")
    ("google" "https://www.google.com/"
     . "search?q=")
    ("so" "https://stackoverflow.com/"
     . "search?q=")
    ("wiki" "https://en.wikipedia.org/"
     . "w/index.php?search="))
  "Search engines using by `find-web@'.")

(defun find-web@ (what &optional engine)
  "Find web via search ENGINE."
  (interactive
   (list (let ((w (cdr (symbol@))))
           (read-string (format "Find web for: " w) w))
         (when current-prefix-arg
           (read-string (format "Choose (%s): "
                                (mapconcat
                                 #'identity
                                 (mapcar #'car *search-engines*)
                                 "|"))))))
  (let* ((e1 (if (or (null engine)
                     (string= "" engine))
                 "bing"
               engine))
         (e2 (cdr (assoc** e1 *search-engines* #'string=)))
         (url (concat (car e2) (cdr e2) what))
         (encoded (progn (require 'browse-url)
                         (browse-url-url-encode-chars url "[ '()]"))))
    (make-thread*
     (funcall browse-url-browser-function encoded)
     t)))


(define-key (current-global-map) (kbd "C-c f w") #'find-web@)

 ;; end of `find-web@'



;; end of file
