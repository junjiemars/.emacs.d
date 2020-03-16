;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; boot.el
;;;;


;; basic UI

;; Disable menu bar
(when-fn%  'menu-bar-mode nil (menu-bar-mode -1))

;; Disable tool bar
(when-graphic% (when-fn% 'tool-bar-mode nil (tool-bar-mode -1)))

;; Disable scroll bar
(when-fn% 'scroll-bar-mode nil (scroll-bar-mode -1))

;; Go straight to scratch buffer on startup
(setq% inhibit-splash-screen t)


 ;; end of basic UI


;; Theme and Font

(defmacro when-theme% (&rest body)
  (declare (indent 0))
  `(when-graphic%
     (when-version% < 23
       ,@body)))


(defmacro when-font% (&rest body)
  (declare (indent 0))
  `(when-graphic%
     ,@body))

;; font supported

(when-font%

  (defmacro when-font-exist% (font &rest body)
    "If FONT exists then do BODY."
    (declare (indent 1))
    `(when% (and ,font (find-font (font-spec :name ,font)))
       ,@body)))

(when-font%
  
  (defmacro self-default-font! (name size)
    "Set default font by NAME and SIZE in graphic mode."
    `(when-font-exist% ,name
       (when (and (numberp ,size) (> ,size 0))
         (let ((font (format "%s-%s" ,name ,size)))
           (add-to-list 'default-frame-alist
                        (cons 'font font))
           (set-face-attribute 'default nil :font font))))))

(when-font%
  
  ;; Load default font
  (when (self-spec->*env-spec :font :allowed)
    (self-default-font! (self-spec->*env-spec :font :name)
                        (self-spec->*env-spec :font :size))))

(when-font%

  (defmacro self-cjk-font! (name size)
    "Set CJK font's NAME and SIZE in graphic mode."
    `(when-font-exist%
      ,name
      (when-fn% 'set-fontset-font nil
        (let ((ff (frame-parameter nil 'font))
              (fs (font-spec :family ,name :size ,size)))
          (mapc (lambda (c)
                  (if-version%
                      <= 23
                      (set-fontset-font ff c fs nil 'prepend)
                    (set-fontset-font ff c fs)))
                '(han kana cjk-misc)))))))

(when-font%

  (defmacro char-width* (char)
    "Return width in pixels of CHAR in graphic mode."
    `(let* ((s (char-to-string ,char))
            (glyphs (with-temp-buffer
                      (insert s)
                      (font-get-glyphs (font-at 0 nil s) 1 2))))
       (when (and (vectorp glyphs)
                  (> (length glyphs) 0)
                  (> (length (aref glyphs 0)) 4))
         (aref (aref glyphs 0) 4)))))

(when-font%

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
                            (or (and (numberp n) n)
                                1)))
                    (+ w2 0.0)))))))))

 ;; end of when-font%

;; theme supported

(when-theme%

  (defmacro self-load-theme! (name &optional dir)
    "`load-theme' by NAME.
If DIR is nil then load the built-in `customize-themes' by NAME."
    `(when ,name
       (when (and ,dir (file-exists-p ,dir))
         (setq custom-theme-directory ,dir))
       (if-version% >= 24.1
                    (load-theme ,name)
         (load-theme ,name t)))))


;; Load theme
(when-theme%

  (when (and (self-spec->*env-spec :theme :allowed)
             (self-spec->*env-spec :theme :name))
    (cond
     ((self-spec->*env-spec :theme :custom-theme-directory)
      ;; load theme from :custom-theme-directory
      (if (self-spec->*env-spec :theme :compile)
          (progn
            (compile! (compile-unit*
                       (concat
                        (self-spec->*env-spec :theme
                                              :custom-theme-directory)
                        (symbol-name (self-spec->*env-spec :theme
                                                           :name))
                        "-theme.el")
                       t t))
            (self-load-theme!
             (self-spec->*env-spec :theme :name)
             (concat (self-spec->*env-spec :theme :custom-theme-directory)
                     (v-path* "/"))))
        (self-load-theme!
         (self-spec->*env-spec :theme :name)
         (self-spec->*env-spec :theme :custom-theme-directory))))

     ;; load builtin theme
     (t (self-load-theme! (self-spec->*env-spec :theme :name))))))


 ;; end of when-theme%


;; end of file

