;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-font-autoload.el
;;;;


(defmacro self-cjk-font! (name size)
  "Set CJK font's NAME and SIZE in graphic mode."
  `(when-font-exist%
       ,name
     (when-fn% 'set-fontset-font nil
       (let ((fs (font-spec :family ,name :size ,size)))
         (mapc (lambda (c)
                 (if-version%
                     <= 23
                     (set-fontset-font t c fs nil 'prepend)
                   (set-fontset-font t c fs)))
               '(han kana cjk-misc))))))


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


;; Load cjk font
(when (self-spec->*env-spec :cjk-font :allowed)
  (self-cjk-font! (self-spec->*env-spec :cjk-font :name)
                  (self-spec->*env-spec :cjk-font :size))
  (when (self-spec->*env-spec :cjk-font :scale)
    (let ((w1 (char-width* ?a))
          (w2 (char-width* #x4e2d)))
      (when (and w1 w2 (> w1 0) (> w2 0))
        (add-to-list
         'face-font-rescale-alist
         (cons (concat ".*"
                       (self-spec->*env-spec :cjk-font :name)
                       ".*")
               (/ (* w1 (let ((n (self-spec->*env-spec :cjk-font :scale)))
                          (or (and (numberp n) (> n 0) n)
                              1)))
                  (+ w2 0.0))))))))


;; end of file
