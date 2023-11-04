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
  ;; disable `desktop-restore-forces-onscreen'
  (setq% desktop-restore-forces-onscreen nil 'desktop)
  (let ((desk (*self-env-spec* :get :desktop)))
    (when (and (self-spec-> desk :allowed)
               (file-exists-p (v-home% ".desktop/")))
      ;; restrict eager
      (setq% desktop-restore-eager
             (self-spec-> desk :restore-eager)
             'desktop)

      (desktop-read (v-home% ".desktop/"))

      ;; remove unnecessary hooks of `desktop'
      (if-fn% 'desktop--on-kill 'desktop
              (remove-hook 'kill-emacs-hook 'desktop--on-kill)
        (remove-hook 'kill-emacs-hook 'desktop-kill))
      (if-var% kill-emacs-query-functions nil
               (progn
                 (remove-hook 'kill-emacs-query-functions 'desktop-kill)
                 (add-hook 'kill-emacs-query-functions #'self-desktop-save! t))
        (add-hook 'kill-emacs-hook #'self-desktop-save! t)))))


 ;; end of Read desktop


;; Save desktop
(defun self-desktop-save! ()
  "Save the desktop of the current Emacs instance."
  (let ((desk (*self-env-spec* :get :desktop)))
    (when (self-spec-> desk :allowed)

      (setq% desktop-files-not-to-save
             (self-spec-> desk :files-not-to-save)
             'desktop)

      (setq% desktop-buffers-not-to-save
             (self-spec-> desk :buffers-not-to-save)
             'desktop)

      (setq% desktop-modes-not-to-save
             (self-spec-> desk :modes-not-to-save)
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

      (when-theme% (self-theme-load! t))

      (if-version% >= 23
                   (desktop-save (v-home! ".desktop/"))
        (desktop-save (v-home! ".desktop/") t)))))


;; end of file
