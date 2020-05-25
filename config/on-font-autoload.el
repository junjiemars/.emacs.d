;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-font-autoload.el
;;;;


(defmacro self-glyph-font! (name size fontsets)
  "Set glyph font's NAME and SIZE in graphic mode."
  `(when-font-exist%
       ,name
     (when-fn% 'set-fontset-font nil
       (let ((fs (font-spec :family ,name :size ,size)))
         (mapc (lambda (c)
                 (if-version%
                     <= 23
                     (set-fontset-font t c fs nil 'prepend)
                   (set-fontset-font t c fs)))
               ,fontsets)))))


(defmacro char-width* (char)
  "Return width in pixels of CHAR in graphic mode."
  `(let* ((s (char-to-string ,char))
          (glyphs (with-temp-buffer
                    (insert s)
                    (font-get-glyphs (font-at 0 nil s) 1 2))))
     (when (and (vectorp glyphs)
                (> (length glyphs) 0)
                (> (length (aref glyphs 0)) 4))
       (aref (aref glyphs 0) 4))))


;; Load glyph font
(when (self-spec->*env-spec :glyph-font :allowed)
  (self-glyph-font! (self-spec->*env-spec :glyph-font :name)
                    (self-spec->*env-spec :glyph-font :size)
                    (self-spec->*env-spec :glyph-font :fontsets))
  (when (self-spec->*env-spec :glyph-font :scale)
    (let ((w1 (char-width* ?a))
          (w2 (char-width* #x4e2d)))
      (when (and w1 w2 (> w1 0) (> w2 0))
        (add-to-list
         'face-font-rescale-alist
         (cons (concat ".*"
                       (self-spec->*env-spec :glyph-font :name)
                       ".*")
               (/ (* w1 (let ((n (self-spec->*env-spec :glyph-font :scale)))
                          (or (and (numberp n) (> n 0) n)
                              1)))
                  (+ w2 0.0))))))))


;; end of file
