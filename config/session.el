;;;; -*- lexical-binding:t -*-
;;;;
;; Session 
;;;;




;; Read/Save desktop


(defun self-desktop-read! ()
  
  (terminal-supported-p
    (version-supported-when = 24.4
      (setq-default desktop-restore-forces-onscreen nil)))

  (self-safe-call*
   "env-spec"
   (let ((desktop (plist-get *val* :desktop)))
     (when (and desktop
                (plist-get desktop :allowed))
       (desktop-read (v-home* ".desktop/"))))))


(defun self-desktop-save! ()
  (self-safe-call*
   "env-spec"
   (let ((desktop (plist-get *val* :desktop)))
     (when (and desktop
                (plist-get desktop :allowed))
       
       (let ((f (plist-get desktop :files-not-to-save)))
         (when f
           (setq-default desktop-files-not-to-save f)))
       (let ((b (plist-get desktop :buffers-not-to-save)))
         (when b
           (setq-default desktop-buffers-not-to-save b)))
       (let ((m (plist-get desktop :modes-not-to-save)))
         (setq-default desktop-modes-not-to-save
                       (append '(tags-table-mode) m)))
       
       (version-supported-if
           >= 23
           (desktop-save (v-home! ".desktop/"))
         (desktop-save (v-home! ".desktop/") t))))))


(add-hook 'after-init-hook #'self-desktop-read!)

(add-hook 'kill-emacs-hook #'self-desktop-save!)
