;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-docview-autoload.el
;;;;


(if% (or (executable-find% "gswin64c")
         (executable-find% "gswin32c"))
    
    (with-eval-after-load 'doc-view
      (setq% doc-view-ghostscript-program
             (or (executable-find% "gswin64c")
                 (executable-find% "gswin32c"))
             'doc-view))

  (when% (executable-find% "mutool")

    (defun make-mudraw-bat (mutool)
      "Make mudraw.bat in `exec-path' for mutool.exe ."
      (save-str-to-file
       (concat "@echo off\n"
               (format "REM mudraw.bat for %s on Windows\n" mutool)
               "REM generated by More Reasonable Emacs https://github.com/junjiemars/.emacs.d\n\n"
               (concat "setlocal EnableDelayedExpansion\n"
                       "\n"
                       "mutool draw %*\n"))
       (v-home% ".exec/mudraw.bat")))

    (defun make-pdfinfo-bat (mutool)
      "Make pdfinfo.bat in `exec-path' for mutool.exe ."
      (save-str-to-file
       (concat "@echo off\n"
               (format "REM pdfinfo.bat for %s on Windows\n" mutool)
               "REM generated by More Reasonable Emacs https://github.com/junjiemars/.emacs.d\n\n"
               (concat "setlocal EnableDelayedExpansion\n"
                       "\n"
                       "mutool info %*\n"))
       (v-home% ".exec/pdfinfo.bat")))

    (defadvice doc-view-mode-p (after doc-view-mode-p-after compile)
      "fix: the builtin `doc-view-mode-p' does not support mupdf."
      (when (eq 'pdf (ad-get-arg 0))
        (setq ad-return-value t)))


    (with-eval-after-load 'doc-view
      (let ((mutool (executable-find% "mutool")))
        (when (and (make-mudraw-bat mutool)
                   (make-pdfinfo-bat mutool))
          (ad-activate #'doc-view-mode-p t))))))


;; end of file
