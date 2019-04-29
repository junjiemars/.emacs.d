;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-eww-autoload.el
;;;;


(eval-when-compile (require 'browse-url))


(feature-eww-supported-p
  
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


(feature-eww-supported-p

  (defun set-eww-mode! ()
    (toggle-truncate-lines nil)))


(feature-eww-supported-p

  (with-eval-after-load 'eww
    (add-hook 'eww-mode-hook #'set-eww-mode!)))


;; find web via search engine

(defun find-web@ (engine)
  "Find web via search ENGINE."
  (interactive "sfind-web@ bing|duck|google|so|wiki|: ")
  (let* ((w (cdr (assoc** (let ((x (string-trim>< engine)))
                            (if (string= "" x)
                                "bing"
                              x))
                          '(("bing" "https://www.bing.com/"
                             . "search?q=")
                            ("duck" "https://duckduckgo.com/"
                             . "?q=")
                            ("google" "https://www.google.com/"
                             . "search?q=")
                            ("so" "https://stackoverflow.com/"
                             . "search?q=")
                            ("wiki" "https://en.wikipedia.org/"
                             . "w/index.php?search="))
                          #'string=)))
         (w1 (if w w (cons engine "")))
         (url (concat (car w1)
                      (let ((s (_symbol@)))
                        (when s (concat (cdr w1) s)))))
         (encoded (progn (require 'browse-url)
                         (browse-url-url-encode-chars url "[ '()]"))))
    (_threading-call
     (progn
       (message "find-web@: %s" encoded)
       (funcall browse-url-browser-function encoded))
     t)))

(define-key (current-global-map) (kbd "C-c f w") #'find-web@)

 ;; end of `find-web@'



;; end of file
