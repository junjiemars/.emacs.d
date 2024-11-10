;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; memo.el
;;;
;; Commentary: read/save session
;;;;

(defmacro desktop-spec->* (&rest keys)
  "Extract :desktop from env-spec via KEYS."
  (declare (indent 0))
  `(*self-env-spec* :get :desktop ,@keys))



;;;
;; read
;;;

(defun self-desktop-read! ()
  "Read the desktop of the previous Emacs instance."
  (when (desktop-spec->* :allowed)
    (setq% desktop-dirname (v-home! ".desktop/") 'desktop)
    (inhibit-blinking
      ;; restrict eager
      (setq% desktop-restore-eager 0 'desktop)
      ;; (setq% desktop-restore-frames nil 'desktop)
      ;; disable stupid resizing
      (setq% desktop-restore-forces-onscreen nil 'desktop)
      ;; (setq% desktop-restore-in-current-display t 'desktop)
      ;; (setq% desktop-restore-reuses-frames t 'desktop)
      ;; quiet
      (setq% desktop-lazy-verbose nil 'desktop)
      ;; `desktop-read'
      (desktop-read desktop-dirname))

    ;; remove unnecessary hooks of `kill-emacs-hook'
    (setq kill-emacs-hook
          (if-fn% 'desktop--on-kill 'desktop
                  (delq 'desktop--on-kill kill-emacs-hook)
            (delq 'desktop-kill kill-emacs-hook)))

    ;; remove unnecessary hooks of `kill-emacs-query-functions'
    (if-var% kill-emacs-query-functions nil
             (progn
               (setq kill-emacs-query-functions
                     (delq 'desktop-kill
                           kill-emacs-query-functions))
               (append! #'self-desktop-save!
                        kill-emacs-query-functions))
      (append! #'self-desktop-save! kill-emacs-hook))))

;; end of read

;;;
;; save
;;;

(defun self-desktop-save! ()
  "Save the desktop of the current Emacs instance."
  (when (desktop-spec->* :allowed)

    (setq% desktop-files-not-to-save
           (let ((ss (desktop-spec->* :files-not-to-save)))
             (concat
              "\\.desktop$"
              "\\|~$"
              "\\|\\.elc\\|\\.eln$"
              "\\|\\.el\\.gz$"
              "\\|\\.[tT][aA][gG][sS]?$"
              "\\|\\.[lL][oO][gG]$"
              "\\|^/sudo:\\|^/sshx?:\\|ftp:"
              (when ss (concat "\\|" ss))))
           'desktop)

    (setq% desktop-buffers-not-to-save
           (let ((ss (desktop-spec->* :buffers-not-to-save)))
             (concat
              "\\*.*?\\*"
              (and ss (concat "\\|" ss))))
           'desktop)

    (setq% desktop-modes-not-to-save
           (append '(archive-mode
                     dired-mode
                     eww-mode
                     rmail-mode
                     special-mode
                     tags-table-mode
                     vc-dir-mode)
                   (desktop-spec->* :modes-not-to-save))
           'desktop)

    (when-window% 'ns
      (when-version% <= 26
        ;; fix: title bar text color broken #55
        ;; https://github.com/d12frosted/homebrew-emacs-plus/issues/55#issuecomment-408317248
        (mapc (lambda (x)
                (push! x frameset-filter-alist))
              '((ns-transparent-titlebar . unbound)
                (ns-appearance . unbound)))))

    ;; disable line number mode
    (when-fn% 'global-display-line-numbers-mode 'display-line-numbers
      (global-display-line-numbers-mode -1))

    (when-theme% (self-theme-init! t))

    (if-version% >= 23
                 (unwind-protect
                     (desktop-save desktop-dirname)
                   (desktop-release-lock))
      (desktop-save desktop-dirname t))))

;; end of save


(provide 'memo)


;; end of memo.el
