;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-font-autoload.el
;;;;


(defsubst self-glyph-font! (name size scripts)
  "Set glyph font's NAME and SIZE in graphic mode."
  (when-fn% 'set-fontset-font nil
    (let ((fs (font-spec :family name :size size)))
      (mapc (lambda (c)
              (if-version%
                  <= 23
                  (set-fontset-font t c fs nil 'prepend)
                (set-fontset-font t c fs)))
            scripts))))


(defsubst char-width* (char)
  "Return width in pixels of CHAR in graphic mode."
  (let* ((s (char-to-string char))
         (glyphs (with-temp-buffer
                   (insert s)
                   (font-get-glyphs (font-at 0 nil s) 1 2))))
    (when (and (vectorp glyphs)
               (> (length glyphs) 0)
               (> (length (aref glyphs 0)) 4))
      (aref (aref glyphs 0) 4))))


;; Load glyph font
(mapc (lambda (glyph)
        (when (plist-get glyph :allowed)
          (let ((name (plist-get glyph :name))
                (size (plist-get glyph :size))
                (scale (plist-get glyph :scale))
                (scripts (plist-get glyph :scripts)))
            (self-glyph-font! name size scripts)
            (when scale
              (let ((w1 (char-width* ?a))
                    (w2 (char-width* #x4e2d)))
                (when (and w1 w2 (> w1 0) (> w2 0))
                  (add-to-list
                   'face-font-rescale-alist
                   (cons (concat ".*" name ".*")
                         (/ (* w1 (or (and (numberp scale) (> scale 0) scale)
                                      1))
                            (+ w2 0.0))))))))))
      (self-spec->*env-spec :glyph-font))




;; end of file
