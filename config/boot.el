;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; boot.el
;;;;


;; basic ui

;; Disable menu bar
(when-fn%  menu-bar-mode nil (menu-bar-mode -1))

;; Disable tool bar
(graphic-supported-p (when-fn% tool-bar-mode nil (tool-bar-mode -1)))

;; Disable scroll bar
(when-fn% scroll-bar-mode nil (scroll-bar-mode -1))

;; Go straight to scratch buffer on startup when graphic supported
(graphic-supported-if
    (setq% inhibit-splash-screen t)
  (setq% inhibit-splash-screen nil))


 ;; end of basic ui


;; Theme and Font

(defmacro theme-supported-p (&rest body)
  (declare (indent 1))
  `(graphic-supported-p
     (version-supported-when
         < 23
       ,@body)))


(defmacro font-supported-p (&rest body)
  (declare (indent 1))
  `(graphic-supported-p
     ,@body))


(font-supported-p
    
    (defsubst font-exists-p (font)
      "Return t if FONT exists."
      (when (find-font (font-spec :name font))
	t)))

(font-supported-p
    
    (defsubst self-default-font! (font)
      "Set default FONT in graphic mode."
      (when (font-exists-p font)
	(add-to-list 'default-frame-alist (cons 'font font))
	(set-face-attribute 'default nil :font font))))

(font-supported-p
    
    ;; Load default font
    (when (self-spec->*env-spec :font :allowed)
      (self-default-font! (self-spec->*env-spec :font :name))))

(font-supported-p

    (defsubst self-cjk-font! (name size)
      "Set CJK font's NAME and SIZE in graphic mode."
      (when (font-exists-p name)
	(when-fn% set-fontset-font nil
	  (dolist (c '(han kana cjk-misc))
	    (set-fontset-font (frame-parameter nil 'font)
			      c (font-spec :family name
					   :size size)))))))

(font-supported-p
    
    ;; Load cjk font
    (when (self-spec->*env-spec :cjk-font :allowed)
      (self-cjk-font! (self-spec->*env-spec :cjk-font :name)
		      (self-spec->*env-spec :cjk-font :size))))

;; End of font-supported-p

(theme-supported-p
    
    (defsubst self-load-theme! (name &optional dir)
      "`load-theme' by NAME.

If DIR is nil then load the built-in `customize-themes' by NAME."
      (when (and dir (file-exists-p dir))
				(setq custom-theme-directory dir))
      (version-supported-if >= 24.1
														(load-theme name)
				(load-theme name t))))


(theme-supported-p

		(defmacro theme-spec->* (&rest keys)
			"Extract value from the list of :theme spec via KEYS at runtime."
			(declare (indent 0))
			`(self-spec->*env-spec :theme ,@keys)))


;; Load theme
(theme-supported-p

    (when (theme-spec->* :allowed)
      (cond ((and (theme-spec->* :name)
									(theme-spec->* :path))
						 ;; load theme from :path
						 (if (theme-spec->* :compile)
								 (when (compile!
												 (compile-unit
													(concat (theme-spec->* :path)
																	(symbol-name (theme-spec->* :name))
																	"-theme.el")
													t nil t))
									 (self-load-theme! (theme-spec->* :name)
																		 (concat (theme-spec->* :path)
																						 +v-dir+ "/")))
							 (self-load-theme! (theme-spec->* :name)
																 (theme-spec->* :path))))

						;; load builtin theme
						((theme-spec->* :name)
						 (self-load-theme! (theme-spec->* :name))))))


 ;; end of theme-supported-p

