;;;;
;; Navigation
;;;;


;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
;; But be care for some operations will slowdown Emacs



;; Toggle recentf-mode
(defmacro toggle-recentf-mode (&optional disable)
  "Toggle recentf-mode, disable recentf-mode unconditional when `disable' is non-nil."
  `(cond (,disable (recentf-mode -1))
         ((or (not (boundp 'recentf-mode))
              (null recentf-mode))
          ;; (setq-default recentf-auto-cleanup 'never)
          ;; recentf-list
          (setq-default recentf-max-saved-items 8)
          (recentf-mode 1))
         (t (recentf-mode -1))))



;; ido-mode allows you to more easily navigate choices. For example,
;; when you want to switch buffers, ido presents you with a list
;; of buffers in the the mini-buffer. As you start to type a buffer's
;; name, ido will narrow down the list of buffers to match the text
;; you've typed in
;; http://www.emacswiki.org/emacs/InteractivelyDoThings
(ido-mode t)



;; Where to save ido.last
(setq-default ido-save-directory-list-file (v-home! ".ido/" "ido.last"))

;; smex
(package-supported-p
  (setq-default smex-save-file (v-home! ".smex/" ".smex-items")))

;; Where to save semantic db
;; Semantic is a suite of Emacs libraries and utilties for parsing
;; source code
(package-supported-p
  (setq-default semanticdb-default-save-directory
                (v-home! ".semanticdb/")))



;; This allows partial matches, e.g. "tl" will match "Tyrion Lannister"
(safe-setq ido-enable-flex-matching t)

;; Turn this behavior off because it's annoying
(safe-setq ido-use-filename-at-point nil)

;; Don't try to match file across all "work" directories; only match files
;; in the current directory displayed in the minibuffer
(safe-setq ido-auto-merge-work-directories-length -1)

;; Includes buffer names of recently open files, even if they're not
;; open now
(safe-setq ido-use-virtual-buffers t)


(defun self-ido-ubiquitous! ()
  ;; This enables ido in all contexts where it could be useful, not just
  ;; for selecting buffer and file names
  
  (ido-ubiquitous-mode 1)

  ;; Enhances M-x to allow easier execution of commands. Provides
  ;; a filterable list of possible commands in the minibuffer
  ;; http://www.emacswiki.org/emacs/Smex
  ;; https://github.com/nonsequitur/smex
  
  (global-set-key (kbd "M-x")
                  (lambda ()
                    (interactive)
                    (or (boundp 'smex-cache)
                        (smex-initialize))
                    (smex)))
  
  (safe-setq smex-save-file (v-home! ".smex/" ".smex-items")))


(append-to-emacs-startup-hook #'self-ido-ubiquitous!)

