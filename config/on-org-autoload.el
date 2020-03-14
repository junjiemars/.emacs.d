;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-org-autoload.el
;;;;


(defmacro-if-feature% ox-reveal)

;; fix: Warning (bytecomp): `org-bookmark-jump-unhide' fn might not be
;; defined at runtime.
(when-fn% 'org-bookmark-jump-unhide 'org
  (autoload 'org-bookmark-jump-unhide "org"))


(defun char-pixel-width (s)
  "Return the width in pixel of S."
  (let ((glyphs (with-temp-buffer
                  (insert s)
                  (font-get-glyphs (font-at 0 nil s)
                                   1 (goto-char 2)))))
    (when (and (vectorp glyphs)
               (> (length glyphs) 0)
               (> (length (aref glyphs 0)) 4))
      (aref (aref glyphs 0) 4))))


(with-eval-after-load 'org
  ;; load `ox-reveal' if it had been installed.
  (if-feature-ox-reveal%
      (when-var% org-reveal-root 'ox-reveal
        (require 'ox-reveal)
        (setq org-reveal-root
              (let ((root (expand-file-name "~/.reveal.js/")))
                (if (file-exists-p root)
                    root
                  ;; "https://cdn.jsdelivr.net/reveal.js/3.8.0/"
                  "https://pagecdn.io/lib/reveal/3.8.0/")))))

  ;; disable _ sub-superscripts
  (when-var% org-use-sub-superscripts 'org
    (setq org-use-sub-superscripts nil))

  ;; disable invisible edit
  (when-var% org-catch-invisible-edits 'org t)

  ;; define keys
  (define-key% (current-global-map) (kbd "C-c o l") #'org-store-link)
  (define-key% (current-global-map) (kbd "C-c o a") #'org-agenda)
  (when-fn% 'org-capture 'org
    (define-key% (current-global-map) (kbd "C-c o c") #'org-capture))
  (when-fn% 'org-switchb 'org
    (define-key% (current-global-map) (kbd "C-c o s") #'org-switchb)))


;; end of on-org-autoload.el
