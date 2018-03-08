;;;; -*- lexical-binding:t -*-
;;;;
;; cc
;;;;



(platform-supported-when windows-nt
  
  (defun check-vcvarsall-bat ()
    (let ((vswhere
           (concat 
            (windows-nt-posix-path (getenv "PROGRAMFILES"))
            " (x86)/Microsoft Visual Studio/Installer/vswhere.exe")))
      (when (file-exists-p vswhere)
        (let ((bat
               (windows-nt-posix-path
                (concat
                 (string-trim>
                  (shell-command-to-string
                   (concat
                    "\"" vswhere "\" "
                    "-nologo -latest -property installationPath")))
                 "/VC/Auxiliary/Build/vcvarsall.bat"))))
          (when (file-exists-p bat)
            bat))))))


(platform-supported-when windows-nt
  
  (defun make-cc-env-bat ()
    (let ((vcvarsall (check-vcvarsall-bat))
          (arch (downcase (getenv "PROCESSOR_ARCHITECTURE")))
          (where (expand-file-name (v-home% "config/" ".cc-env.bat"))))
      (when vcvarsall
        (save-str-to-file 
         (concat
          "@echo off\n"
          "cd /d \"" (file-name-directory vcvarsall) "\"\n"
          "call vcvarsall.bat " arch "\n"
          "echo \"%INCLUDE%\"\n") where)))))


(platform-supported-if
    windows-nt
    (defun check-cc-include ()
      (let ((cc-env-bat (make-cc-env-bat)))
        (when cc-env-bat
          (var->paths
           (car (nreverse 
                 (split-string%
                  (shell-command-to-string cc-env-bat)
                  "\n" t "\"")))))))

  (defun check-cc-include ()
    (take-while
     (lambda (p)
       (string-match "End of search list." p))
     (drop-while
      (lambda (p)
        (string-match "#include <...> search starts here:" p))
      (split-string%
       (shell-command-to-string
        "echo '' | cc -v -E 2>&1 >/dev/null -")
       "\n" t "[ \t\n]")))))


(defvar system-cc-include nil
  "The system include paths used by the C language.

This should be set with `system-cc-include'")

(defun system-cc-include (cached)
  "Returns a list of system include directories. 

Load `system-cc-include' from file when CACHED is t, 
otherwise check cc include on the fly."
  (let ((c (v-home% "config/" ".cc-inc.el")))
    (if (and cached (file-exists-p (concat c "c")))
        (progn
          (load (concat c "c"))
          system-cc-include)
      (let ((paths
             (platform-supported-if windows-nt
                 (check-cc-include)
               (platform-supported-if darwin
                   (mapcar (lambda (x)
                             (string-trim> x " (framework directory)"))
                           (check-cc-include))
                 (check-cc-include)))))
        (when (save-sexp-to-file
               `(setq system-cc-include ',paths) c)
          (byte-compile-file c))
        (setq system-cc-include paths)))))


(provide 'cc)
