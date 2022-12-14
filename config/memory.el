;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; memory.el
;;;;


;; Read desktop
(defun self-desktop-read! ()
  "Read the desktop of the previous Emacs instance."
  (unless-graphic%
    (when-version% <= 24.4
      (when-version% > 25
        (setq% desktop-restore-forces-onscreen nil 'desktop))))

  (when (and (*self-env-spec* :get :desktop :allowed)
             (file-exists-p (v-home% ".desktop/")))
    (setq% desktop-restore-eager
           (*self-env-spec* :get :desktop :restore-eager) 'desktop)

    (desktop-read (v-home% ".desktop/"))

    ;; remove unnecessary hooks of `desktop'
    (remove-hook 'kill-emacs-hook 'desktop--on-kill)
    (if-var% kill-emacs-query-functions nil
             (progn
               (remove-hook 'kill-emacs-query-functions 'desktop-kill)
               (add-hook 'kill-emacs-query-functions #'self-desktop-save! t))
      (add-hook 'kill-emacs-hook #'self-desktop-save! t))))


 ;; end of Read desktop


;; Save desktop
(defun self-desktop-save! ()
  "Save the desktop of the current Emacs instance."
  (when (*self-env-spec* :get :desktop :allowed)

    (setq% desktop-files-not-to-save
           (*self-env-spec* :get :desktop :files-not-to-save)
           'desktop)

    (setq% desktop-buffers-not-to-save
           (*self-env-spec* :get :desktop :buffers-not-to-save)
           'desktop)

    (setq% desktop-modes-not-to-save
           (*self-env-spec* :get :desktop :modes-not-to-save)
           'desktop)

    (when-graphic%
      (when-version% <= 26
        (when-platform% 'darwin
          ;; fix: title bar text color broken #55
          ;; https://github.com/d12frosted/homebrew-emacs-plus/issues/55#issuecomment-408317248
          (mapc (lambda (x)
                  (push! x frameset-filter-alist))
                '((ns-transparent-titlebar . unbound)
                  (ns-appearance . unbound))))))

    (if-version% >= 23
                 (desktop-save (v-home! ".desktop/"))
      (desktop-save (v-home! ".desktop/") t))))


;; end of file
