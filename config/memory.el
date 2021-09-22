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
    (setq% desktop-save-mode t 'desktop)
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


(add-hook 'kill-emacs-hook #'self-desktop-save! t)


;; end of file
