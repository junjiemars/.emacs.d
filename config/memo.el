;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; memo.el
;;;
;; Commentary: read/save session
;;;;

(defmacro without-frame-resize (&rest body)
  (declare (indent 0))
  `(let ((frame-inhibit-implied-resize t)
         (inhibit-redisplay t))
     (when-version% >= 25
       (ignore* frame-inhibit-implied-resize))
     (progn% ,@body)))

;;;
;; read
;;;

(defun self-desktop-read! ()
  "Read the desktop of the previous Emacs instance."
  (let ((desk (*self-env-spec* :get :desktop)))
    (when (and (self-spec-> desk :allowed)
               (file-exists-p (v-home% ".desktop/")))
      ;; restrict eager
      (setq% desktop-restore-eager 0 'desktop)

      ;; disable offscreen onscreen
      (setq% desktop-restore-forces-onscreen nil 'desktop)

      ;; `desktop-read'
      (without-frame-resize
        (desktop-read (v-home% ".desktop/")))

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
        (append! #'self-desktop-save! kill-emacs-hook)))))

;; end of read

;;;
;; save
;;;

(defun self-desktop-save! ()
  "Save the desktop of the current Emacs instance."
  (let ((desk (*self-env-spec* :get :desktop)))
    (when (self-spec-> desk :allowed)

      (setq% desktop-files-not-to-save
             (let ((ss (self-spec-> desk :files-not-to-save)))
               (concat "\\(\\.el\\.gz\\)\\|"
                       desktop-files-not-to-save
                       (if ss (concat "\\|" ss) "")))
             'desktop)

      (setq% desktop-buffers-not-to-save
             (let ((ss (self-spec-> desk :buffers-not-to-save)))
               (concat "\\(\\.[tT][aA][gG][sS]?\\|\\.[lL][oO][gG]\\)"
                       desktop-buffers-not-to-save
                       (if ss (concat "\\|" ss) "")))
             'desktop)

      (setq% desktop-modes-not-to-save
             (append '(archive-mode
                       eww-mode
                       flymake-mode
                       rmail-mode
                       tags-table-mode)
                     (self-spec-> desk :modes-not-to-save))
             'desktop)

      (when-window% 'ns
        (when-version% <= 26
            ;; fix: title bar text color broken #55
            ;; https://github.com/d12frosted/homebrew-emacs-plus/issues/55#issuecomment-408317248
          (mapc (lambda (x)
                  (push! x frameset-filter-alist))
                '((ns-transparent-titlebar . unbound)
                  (ns-appearance . unbound)))))

      (when-theme% (self-theme-init! t))

      (if-version% >= 23
                   (unwind-protect
                       (desktop-save (v-home! ".desktop/"))
                     (desktop-release-lock))
        (desktop-save (v-home! ".desktop/") t)))))

;; end of save


(provide 'memo)


;; end of memo.el
