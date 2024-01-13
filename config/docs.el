;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; docs.el
;;;;

(defmacro if-bin-gswin64/32c% (then &rest body)
  (declare (indent 1))
  `(if% (or (executable-find% "gswin64c")
            (executable-find% "gswin32c"))
       ,then
     (progn% ,@body)))

(defmacro unless-bin-gswin64/32c% (&rest body)
  (declare (indent 0))
  (if-bin-gswin64/32c%
      `(comment ,body)
    (when% (executable-find% "mutool")
      `(progn% ,@body))))

(defmacro when-bin-mudraw% (&rest body)
  (declare (indent 0))
  `(if% (and (executable-find% "mudraw")
             (executable-find% "pdfinfo"))
       ,then
     (progn% ,@body)))

(unless-bin-gswin64/32c%
  (defun make-mudraw-bat (mutool)
    "Make mudraw.bat in \\=`exec-path\\=' for mutool.exe ."
    (save-str-to-file
     (concat "@echo off\n"
             (format "REM mudraw.bat for %s on Windows\n" mutool)
             "REM generated by Nore Emacs " (nore-emacs) "\n\n"
             "setlocal EnableDelayedExpansion\n\n"
             "mutool draw %*\n")
     (v-home% ".exec/mudraw.bat"))))

(unless-bin-gswin64/32c%
  (defun make-pdfinfo-bat (mutool)
    "Make pdfinfo.bat in \\=`exec-path\\=' for mutool.exe ."
    (save-str-to-file
     (concat "@echo off\n"
             (format "REM pdfinfo.bat for %s on Windows\n" mutool)
             "REM generated by Nore Emacs " (nore-emacs) "\n\n"
             "setlocal EnableDelayedExpansion\n\n"
             "mutool info %*\n")
     (v-home% ".exec/pdfinfo.bat"))))

(when-bin-mudraw%
  ;; fix: the builtin \\=`doc-view-mode-p\\=' does not support mupdf.
  (defadvice doc-view-mode-p
      (after doc-view-mode-p-after first compile disable)
    (when (eq 'pdf (ad-get-arg 0))
      (setq ad-return-value t))))

(defun on-doc-view-init! ()
  "On \\=`doc-view\\=' initialization."
  (if-bin-gswin64/32c%
      (setq% doc-view-ghostscript-program
             (or (executable-find% (format "gswin%dc" (emacs-arch)))
                 (executable-find% "gswin32c")
                 (executable-find% "gswin64c"))
             'doc-view)
    ;; unless gswin64c/gswin32c
    (when% (executable-find% "mutool")
      (unless (executable-find% "mudraw")
        (make-mudraw-bat (executable-find% "mutool")))
      (unless (executable-find% "pdfinfo")
        (make-pdfinfo-bat (executable-find% "mutool")))
      (when-bin-mudraw%
        (ad-enable-advice #'doc-view-mode-p 'after
                          "doc-view-mode-p-after")
        (ad-activate #'doc-view-mode-p t)))))



(provide 'docs)

;; end of docs.el
