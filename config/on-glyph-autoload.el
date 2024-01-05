;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-font-autoload.el
;;;;


(defmacro glyph-spec->* (&rest keys)
  "Extract glyph :from env-spec via KEYS."
  (declare (indent 0))
  `(*self-env-spec* :get :glyph ,@keys))

(defmacro self-glyph-font! (name size scripts)
  "Set glyph font's NAME and SIZE in graphic mode."
  (let ((n (gensym*))
        (s (gensym*))
        (rs (gensym*)))
    `(when-fn% 'set-fontset-font nil
       (let* ((,n ,name)
              (,s ,size)
              (,rs ,scripts)
              (fs (font-spec :family ,n :size ,s)))
         (dolist* (c ,rs)
           (if-version%
               <= 23
               (set-fontset-font t c fs nil 'prepend)
             (set-fontset-font t c fs)))))))


(defmacro char-width* (char)
  "Return width in pixels of CHAR in graphic mode."
  (let ((c (gensym*)))
    `(let* ((,c ,char)
            (s (char-to-string ,c))
            (glyphs (lexical-let%
                        ((format-alist nil)
                         (coding-system-for-write 'no-conversion))
                      (with-temp-buffer
                        (insert s)
                        (font-get-glyphs (font-at 0 nil s) 1 2)))))
       (when (and (vectorp glyphs)
                  (> (length glyphs) 0)
                  (> (length (aref glyphs 0)) 4))
         (aref (aref glyphs 0) 4)))))


;; Load glyph font
(defun self-glyph-init! ()
  "Initialize glyph spec from \\=`*self-env-spec*\\='."
  (dolist* (g (glyph-spec->*))
    (when (plist-get g :allowed)
      (let ((name (plist-get g :name))
            (size (plist-get g :size))
            (scale (plist-get g :scale))
            (scripts (plist-get g :scripts)))
        (self-glyph-font! name size scripts)
        (when scale
          (let ((w1 (char-width* ?a))
                (w2 (char-width* #x4e2d)))
            (when (and w1 w2 (> w1 0) (> w2 0))
              (push! (cons (concat ".*" name ".*")
                           (/ w1 (+ w2 0.0)))
                     face-font-rescale-alist))))))))


(make-thread* #'self-glyph-init!)

;; end of on-glyph-autoload.el
