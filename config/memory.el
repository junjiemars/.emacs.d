;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; memory.el
;;;;


;; Read/Save desktop


(defvar *self-previous-env-spec* nil)


(defmacro memory-spec->% (&rest keys)
  "Extract value from a list of spec via KEYS at compile time."
  `(self-spec->%
       (list :source ,(v-home! ".exec/env-spec.el")
             :compiled ,(v-home! ".exec/env-spec.elc"))
     ,@keys))


(defun save-env-spec! ()
  (save-sexp-to-file (*self-env-spec*)
                     (memory-spec->% :source)))

(add-hook 'kill-emacs-hook #'save-env-spec! t)


(defmacro load-env-spec ()
  `(when (file-exists-p (memory-spec->% :source))
     (setq *self-previous-env-spec*
           (car (read-from-string (read-str-from-file
                                   (memory-spec->% :source)))))))

(load-env-spec)


(when-theme%
  
  (defmacro theme-changed-p (previous current)
    "Return (previous . current)"
    `(cond
      ((not (eq (self-spec-> ,previous :allowed)
                (self-spec-> ,current :allowed)))
       (cons (self-spec-> ,previous :allowed)
             (self-spec-> ,current :allowed)))
      ((or (not (eq (self-spec-> ,previous :name)
                    (self-spec-> ,current :name)))
           (not (string= (self-spec-> ,previous :custom-theme-directory)
                         (self-spec-> ,current :custom-theme-directory))))
       (cons nil t)))))

(when-theme%
  
  (defmacro switch-theme! (previous current)
    `(let ((p->c (theme-changed-p ,previous ,current)))
       (when (consp p->c)
         (if (and (car p->c) (not (cdr p->c)))
             (when (self-spec-> ,previous :name)
               (self-load-theme!
                (self-spec-> ,previous :name)
                (self-spec-> ,previous :custom-theme-directory))
               (disable-theme (self-spec-> ,previous :name)))
           (when (and (self-spec-> ,current :name)
                      (self-spec-> ,current :allowed))
             (enable-theme (self-spec-> ,current :name))))
         (setq% desktop-restore-frames t 'desktop)))))


;; Read desktop
(defun self-desktop-read! ()
  "Read the desktop of the previous Emacs instance."
  (unless-graphic%
    (when-version% <= 24.4
      (when-version% > 25
        (setq% desktop-restore-forces-onscreen nil 'desktop))))

  (when (and (*self-env-spec* :get :desktop :allowed)
             (file-exists-p (v-home% ".desktop/")))
    (when-theme%
      (when (consp (theme-changed-p
                    (self-spec-> *self-previous-env-spec* :theme)
                    (*self-env-spec* :get :theme)))
        (setq% desktop-restore-frames nil 'desktop)))
    (setq% desktop-restore-eager
           (*self-env-spec* :get :desktop :restore-eager) 'desktop)
    (desktop-read (v-home% ".desktop/"))))


 ;; end of Read desktop



;; Save desktop
(defun self-desktop-save! ()
  "Save the desktop of the current Emacs instance."
  (when (*self-env-spec* :get :desktop :allowed)
    (let ((f (*self-env-spec* :get :desktop :files-not-to-save)))
      (when f (setq% desktop-files-not-to-save f 'desktop)))

    (let ((b (*self-env-spec* :get :desktop :buffers-not-to-save)))
      (when b (setq% desktop-buffers-not-to-save b 'desktop)))

    (let ((m (*self-env-spec* :get :desktop :modes-not-to-save)))
      (setq% desktop-modes-not-to-save
             (append '(tags-table-mode) m) 'desktop))

    (when-theme%
      (switch-theme! (self-spec-> *self-previous-env-spec* :theme)
                     (*self-env-spec* :get :theme)))

    (when-graphic%
      (when-version% <= 26
        (when-platform% 'darwin
          ;; fix: title bar text color broken #55
          ;; https://github.com/d12frosted/homebrew-emacs-plus/issues/55#issuecomment-408317248
          (mapc (lambda (x)
                  (push* x frameset-filter-alist))
                '((ns-transparent-titlebar . unbound)
                  (ns-appearance . unbound))))))
    
    (if-version% >= 23
                 (desktop-save (v-home! ".desktop/"))
      (desktop-save (v-home! ".desktop/") t))))


(add-hook 'kill-emacs-hook #'self-desktop-save! t)


;; end of file
