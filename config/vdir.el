;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; vdir.el
;;;;
;; Commentary: versioned directories.
;;;;

;; `abbrev'
(setq% abbrev-file-name (v-home! ".abbrev/defs") 'abbrev)

;; auto-save
(setq% auto-save-list-file-prefix (v-home! ".save/auto-"))

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq% backup-directory-alist `(("." . ,(v-home! ".backup/"))))

;; `calc'
(setq% calc-settings-file (v-home! ".calc/calc.el") 'calc)

;;; `eww', requires Emacs-24.4+
(defmacro-if-feature% eww)
(defmacro when-feature-eww% (&rest body)
  (declare (indent 0))
  (if-feature-eww%
      `(progn% ,@body)
    `(comment ,@body)))
(when-feature-eww%
  (setq eww-bookmarks-directory (v-home! ".bookmarks/")))

;; `bookmark': file in which to save bookmarks
(setq% bookmark-default-file (v-home! ".bookmarks/emacs.bmk") 'bookmark)

;; `eshell'
(setq% eshell-directory-name (v-home! ".eshell/") 'eshell)

;; `gamegrid': a directory for game scores
(setq% gamegrid-user-score-file-directory (v-home! ".games/") 'gamegrid)

;; `ido' saved state between invocations
(setq% ido-save-directory-list-file (v-home! ".ido/ido.last") 'ido)

;; `image-dired': where thumbnail images are stored
(setq% image-dired-dir (v-home! ".dired/") 'image-dired)

;; `multisession': where multisession variables stored
(setq% multisession-directory (v-home! ".multisession/") 'multisession)

;; `nsm': Network Security Manager
(setq% nsm-settings-file (v-home! ".nsm/security.data") 'nsm)

;; `package': default `package-usr-dir'
(when-package%
  (defconst package*-user-dir (v-home! ".elpa/")))


;;; `project' builtin since Emacs-26+
(defmacro-if-feature% project)
(defmacro when-feature-project% (&rest body)
  (declare (indent 0))
  (if-feature-project%
      `(progn% ,@body)
    `(comment ,@body)))
(when-feature-project%
  (setq% project-list-file (v-home! ".project/list") 'project))

;; Savehist: save minibuffer history
(setq% savehist-file (v-home! ".minibuffer/history") 'savehist)

;; `recentf': save the recent list into
(setq% recentf-save-file (v-home! ".recentf/recentf") 'recentf)

;; `rmail'
(setq% rmail-file-name (v-home! ".mail/RMAIL") 'rmail)

;; `saveplace'
;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(setq% save-place-file (v-home! ".places/places") 'saveplace)

;; `server'
(setq% server-auth-dir (v-home! ".server/") 'server)

;; `tramp'
(setq% tramp-persistency-file-name (v-home! ".tramp/cache")
       (if-version% > 24 'tramp
             'tramp-cache))

;;; `transient'
(defmacro-if-feature% transient)
(defmacro when-feature-transient% (&rest body)
  "When \\=`transient\\=', do BODY."
  (declare (indent 0))
  (if-feature-transient%
      `(progn% ,@body)
    `(comment ,@body)))
(when-feature-transient%
  (setq transient-save-history nil))

;;; `treesit': builtin since Emacs-29+
(defmacro-if-feature% treesit)
(defmacro when-feature-treesit% (&rest body)
  (declare (indent 0))
  (if-feature-treesit%
      (if-fn% 'treesit-available-p nil
              (if% (treesit-available-p)
		              `(progn% ,@body)
                `(comment ,@body))
        `(comment ,@body))
    `(comment ,@body)))
(when-feature-treesit%
  (setq treesit-extra-load-path `(,(v-home% ".treesit/"))))

;; `url'
(setq% url-configuration-directory (v-home! ".url/") 'url)


;; end of vdir.el
