;;;; -*- lexical-binding:t -*-
;;;;
;; C
;;;;


(defun check-vcvarsall-bat ()
  (let ((vswhere
         (concat 
          (windows-nt-posix-path (getenv "PROGRAMFILES"))
          " (x86)/Microsoft Visual Studio/Installer/vswhere.exe")))
    (when (file-exists-p vswhere)
      (let ((bat
             (windows-nt-posix-path
              (concat
               (string-trim
                (shell-command-to-string
                 (concat
                  "\"" vswhere "\" "
                  "-nologo -latest -property installationPath")))
               "/VC/Auxiliary/Build/vcvarsall.bat"))))
        (when (file-exists-p bat)
          bat)))))


(defun make-cc-env-bat ()
  (let ((vcvarsall (check-vcvarsall-bat))
        (arch (downcase (getenv "PROCESSOR_ARCHITECTURE")))
        (where (concat
                (expand-file-name semanticdb-default-save-directory)
                "cc-env.bat")))
    (when vcvarsall
      (save-string-to-file 
       (concat
        "@echo off\n"
        "cd /d \"" (file-name-directory vcvarsall) "\"\n"
        "call vcvarsall.bat " arch "\n"
        "echo \"%INCLUDE%\"\n")
       where)
      (when (file-exists-p where)
        where))))

(defun check-cc-include ()
  (let ((cc-env-bat (make-cc-env-bat)))
    (car (nreverse 
          (split-string 
           (shell-command-to-string cc-env-bat)
           "\n" t "\"")))))

;; (defun c-turn-on-eldoc-mode ()
;;   "Enable c-eldoc-mode"
;;   (interactive)
;;   (add-function :before-until
;;                 (local 'eldoc-documentation-function)
;;                 #'c-eldoc-semantic-ia-show-summary)
;;   (enable-eldoc-mode))

;; (defun c-eldoc-semantic-ia-show-summary ()
;;   (interactive)
;;   (semantic-ia-show-summary (point)))

;; (when semantic-mode
;;   (add-hook 'c-mode-hook #'c-turn-on-eldoc-mode))

