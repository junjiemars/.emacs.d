;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; gud-cdb.el: Grand Unified Debugger mode for running CDB
;;;;
;;;;
;;
;; Improver: Junjie Mars
;; Origin: https://raw.githubusercontent.com/4DA/kd-gud/master/cdb-gud.el
;; Author: Stephan Doll <stephan_doll at dantz.com>
;; Maintainer: Aaron Brady <abrady0 at yahoo dot com> :
;; Version: 1.0 (January 30, 2002)
;; Version: 1.4 (January 10, 2007) : updated to handle latest cdb
;; Version: 1.5 (January 26, 2011) : helper functions for debugging running processes
;; frames a little better. also added helper functions. see
;; 'cdbDebugChoice' at bottom for example.
;; Version: 1.5 (October 19, 2008) : parse fixes for latest debug tools.
;; Version: 1.6 (June 26, 2018) : improved in Nore Emacs. see
;; https://github.com/junjiemars/.emacs.d
;;
;; This file is NOT part of GNU Emacs but the same permissions apply.
;; This is free software (needed for emacswiki upload.pl)
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package provides Emacs (GUD) integration for Microsoft's CDB
;; debugger.  (CDB is the text-mode version of WinDbg).  For more
;; details about the Emacs' debugger integration, read "Running
;; Debuggers Under Emacs" in the Emacs manual.
;;
;; To install this package:
;;
;;    - Download and install the latest version of the `Windows SDK'
;;      from https://developer.microsoft.com/en-US/windows/downloads/windows-10-sdk
;;      Add cdb to your PATH environment.
;;
;;    - If you want key bindings similar to MS' GUI debuggers, add the
;;      following:
;;
;;      (global-set-key [f9]    'gud-break)
;;      (global-set-key [f7]    'gud-tbreak)
;;      (global-set-key [f8]    'gud-step)
;;      (global-set-key [f10]   'gud-next)
;;      (global-set-key [f5]    'gud-cont)
;;      (global-set-key [f11]   'gud-finish)
;;
;;
;;; Here is a simple tutorial:
;;
;; In Emacs, run
;;
;;     M-: (require 'gud-cdb)
;;     M-x cdb <name of your exe>
;;
;; This will open a new Emacs buffer "*gud-xxx*".  In it you will get a
;; CDB command prompt '0:000> '.  (CDB commands are documented in the
;; 'Debugging tools for Windows' online help).  To get to the begin of
;; your code, type:
;;
;;      'g main' <Enter> (or 'g WinMain' if you have a GUI application).
;;
;; CDB will load the application and break at your main() function.
;; Emacs should open another window with your main() source file and show
;; a little '>' were the debugger stopped.  You now can set more
;; breakpoints in your sources, single-step, etc.  To use the common VC++
;;
;; You can also issue additional commands from the CDB command prompt --
;; e.g.:
;;
;;     - '?'   Displays cdb's help
;;
;;     - 'dv'  Displays local variables
;;
;;     - 'dt' or '??' shows the content of a single variable.
;;
;;;;
;; Refine Targets:
;; 1. Start or attach a process.
;; 2. Source code debugging.
;; 3. Commands autocompletion and history.
;; 4. Frame, register buffers.
;;;;
;;;;
;; Sample C code:
;; generated via Nore (https://github.com/junjiemars/nore)
;; %userprofile%/.cc-env.bat && bash ./configure --new
;; %userprofile%/.cc-env.bat && make -k -C e:/lab/c clean test
;;
;;;;
;; Register/Unregister WinDbg
;; /HKEY_LOCAL_MACHINE/SOFTWARE/Microsoft/Windows NT/CurrentVersion/AeDebug
;; set Auto to 1 (register) or 0 (unregister)
;;;;

;;;;
;; require
;;;;

;; (eval-when-compile (require 'cl))

(eval-when-compile (require 'guds))

(require 'gud)



;;;;
;; gud-cdb-* variables
;;;;


(defvar gud-cdb-history nil
  "History of argument lists passed to cdb.")

(defcustom% gud-cdb-directories nil
  "*A list of directories that cdb should search for source code.
If nil, only source files in the program directory
will be known to cdb.

The file names should be absolute, or relative to the directory
containing the executable being debugged."
  :type '(choice (const :tag "Current Directory" nil)
                 (repeat :value ("")
                         directory))
  :group 'gud)

(defcustom% gud-cdb-command-line-hook nil
  "Hook run by `cdb' on command line."
  :type 'hook
  :group 'gud)

(defcustom% gud-cdb-mode-hook nil
  "Mode hook run by `cdb' before `gud-cdb-init-hook'."
  :type 'hook
  :group 'gud)

(defcustom% gud-cdb-init-hook nil
  "Hook run by `cdb' process."
  :type 'hook
  :group 'gud)


(defconst +cdb-prompt-regexp+ "^\\(?:[0-9]:[0-9][0-9][0-9]> *\\)"
  "Regexp pattern of `cdb' prompt.")


(defvar *cdb-symbol-alist*
  (list '("$argreg" . nil)
        '("$csp" . "the current call stack pointer")
        '("$ea" . "the effective address of the last instruction that was executed")
        '("$ea2" . "the second effective address of the last instruction that was executed")
        '("$exp" . "the last expression that was evaluated")
        '("$exentry" . "the address of the entry point of the first executable of the current process")
        '("$frame" . "the current frame index")
        '("$ip" . "the instruction pointer register")
        '("$peb" . "the address of the process environment block (PEB) of the current process")
        '("$ra" . "the return address that is currently on the stack")
        '("$retreg" . "the primary return value register")
        '("$retreg64" . "the primary return value register in 64-bits format")
        '("$pagesize" . "the number of bytes in one page of memory")
        '("$proc" . "the address of the current process (that is, the address of the EPROCESS block)")
        '("$ptrsize" . "the size of a pointer")
        '("$teb" . "the address of the thread environment block (TEB) of the current thread")
        '("$thread" . "the address of the current thread")
        '("$tid" . "the thread ID for the current thread")
        '("$tpid" . "the process ID (PID) for the process that owns the current thread"))
  "A list of `cdb' symbol.")


;; ;; buffer local variables

;; (make-variable-buffer-local 'gud-marker-acc)
(make-variable-buffer-local 'gud-output-acc)


;;;;
;;  gud-* declarations
;;;;


(when-fn% 'declare-function nil
  (declare-function gud-statement "gud" (arg))
  (declare-function gud-until     "gud" (arg))
  (declare-function gud-pv        "gud" (arg))
  (declare-function gud-print     "gud" (arg))
  (declare-function gud-down      "gud" (arg))
  (declare-function gud-up        "gud" (arg))
  (declare-function gud-jump      "gud" (arg))
  (declare-function gud-finish    "gud" (arg))
  (declare-function gud-cont      "gud" (arg))
  (declare-function gud-next      "gud" (arg))
  (declare-function gud-stepi     "gud" (arg))
  (declare-function gud-step      "gud" (arg))
  (declare-function gud-remove    "gud" (arg))
  (declare-function gud-tbreak    "gud" (arg))
  (declare-function gud-break     "gud" (arg)))

 ;; end of gud-* declarations


;;;;
;; cdb-*
;;;;


(defun cdb-command-line-list-source ()
  "List source options.

cdb [options]:
  -c \"<command>\" executes the given debugger command at the first debugger.
  -lines requests that line number information be used if present.

This function is an example for `gud-cdb-command-line-hook', we can do the same
via: `M-x cdb -c \"l+*;l-s\" -lines <debuggee>'.

-lines option must be included, display line number.
l-s means do not display source code in `cdb' command line.
"
  (list "-c" "l+*;l-s" "-lines"))


(defun cdb-file-name (filename)
  "Transform a relative FILENAME to an absolute one.

Return absolute filename when FILENAME existing or it's existing
in `gud-cdb-directories'.
"
  (or (let ((f (expand-file-name filename)))
        (when (file-exists-p f) f))
      (loop* for d in gud-cdb-directories
             with p = nil
             do (setq p (concat d "/" filename))
             when (file-exists-p p)
             return p)))


(defun cdb-annotate-completion (s)
  "Return annotated the completion S argument or nil."
  (let ((a (cdr (assoc** s *cdb-symbol-alist* #'string=))))
    (when a (concat "    ;; " a))))

(defun cdb-completion-at-point ()
  "Return the data to complete the `cdb' command before point."
  (interactive)
  (let* ((x (bounds-of-thing-at-point 'symbol))
         (start (car x))
         (end (cdr x)))
    (list start end *cdb-symbol-alist*
          . (':annotation-function #'cdb-annotate-completion))))


(defun cdb-set-syntax-table! ()
  "Specify special character in `syntax-table'."
  (modify-syntax-entry ?` "_" (syntax-table))
  (modify-syntax-entry ?! "_" (syntax-table))
  (modify-syntax-entry ?. "_" (syntax-table)))


;; (defun cdb-toggle-breakpoint ()
;;   "Enable/disable breakpoint at current line of breakpoints buffer."
;;   (save-excursion
;;     (beginning-of-line)
;;     (let ((enabled (if-fn% 'gud-toggle-breakpoint-notation 'guds
;;                            (progn (require 'guds)
;;                                   (gud-toggle-breakpoint-notation))
;;                      t)))
;;       (if enabled
;;           "bu `%d%f:%l`"
;;         ;; bc doesn't work now and `linum-mode' slow down breakpoing
;;         ;; notataion
;;         "bl"))))



;;;;
;; gud-cdb-*
;;;;


;; set default `gud-cdb-command-line-hook'
(add-hook 'gud-cdb-command-line-hook #'cdb-command-line-list-source)

;; set default `gud-cdb-mode-hook'
(add-hook 'gud-cdb-mode-hook #'cdb-set-syntax-table!)


(defun gud-cdb-massage-args (file args)
  "As the 2nd argument: message-args of `gud-common-init'.

`gud' callback it once when run `cdb'.

The job of the massage-args method is to modify the given list of
debugger arguments before running the debugger.
"
  (ignore* file)
  (append (loop* for o in gud-cdb-command-line-hook
                 append (funcall o))
          args))


(defun gud-cdb-marker-filter (string)
  "As the 3rd argument: marker-filter of `gud-common-init'.

The job of the marker-filter method is to detect file/line
markers in strings and set the global gud-last-frame to indicate
what display action (if any) should be triggered by the marker.
Note that only whatever the method *returns* is displayed in the
buffer; thus, you can filter the debugger's output, interpreting
some and passing on the rest.
"
  ;; (setq gud-marker-acc (concat gud-marker-acc string))
  (cond ((or
          ;; g
          ;; Breakpoint 0 hit
          ;; e:\lab\c\src\c.c(9)
          ;; c!main:
          ;; 00007ff7`5a036580 4889542410      mov     qword ptr [rsp+10h],rdx ss:000000c5`9b0ff788=0000000000000000
          (string-match "^\\(.*\\)(\\([0-9]+\\))\n" string)
          ;; .frame
          ;; 00 000000f0`f44ffea0 00007ff7`b6f173c7 algo_algo_!test_comp_str+0x1c [e:\lab\c\src\c.c @ 9]
          (string-match "\\[\\(.*\\) @ \\([0-9]+\\)\\]$" string))
         (setq gud-last-frame (cons (match-string 1 string)
                                    (string-to-number (match-string 2 string)))))

        ((or (string-match "quit:" string)
             (string-match "ntdll!NtTerminateProcess\\+0x[0-9a-z]+:" string))
         ;; ModLoad: 00007ffe`9d340000 00007ffe`9d351000   C:\WINDOWS\System32\kernel.appcore.dll
         ;; ModLoad: 00007ffe`9ecc0000 00007ffe`9ed5d000   C:\WINDOWS\System32\msvcrt.dll
         ;; ModLoad: 00007ffe`9f140000 00007ffe`9f25f000   C:\WINDOWS\System32\RPCRT4.dll
         ;; ntdll!NtTerminateProcess+0x14:
         ;; 00007ffe`a10005f4 c3              reto
         (setq gud-last-last-frame nil)
         (gud-display-frame)
         (setq gud-overlay-arrow-position nil)))
  string)


(defun gud-cdb-find-file (filename)
  "As the optional argument: find-file of `gud-common-init'.

`gud' callback it just when `gud-cdb-command-line-list-source'
had been called first.

The job of the find-file method is to visit and return the buffer
indicated by the car of gud-tag-frame.  This may be a file name,
a tag name, or something else."
  (save-excursion
    (let ((f (cdb-file-name filename)))
      (if f
          (find-file-noselect f t)
        (find-file-noselect filename 'nowarn)))))



(defun gud-cdb (command-line)
  "Run cdb on program FILE in buffer *gud-FILE*.

The directory containing FILE becomes the initial working
directory and source-file directory for your debugger."
  (interactive (list (gud-query-cmdline 'cdb)))

  (gud-common-init command-line
                   #'gud-cdb-massage-args
                   #'gud-cdb-marker-filter
                   #'gud-cdb-find-file)

  (set (make-local-variable 'gud-minor-mode) 'cdb)

  (gud-def gud-break
           "bu `%d%f:%l`"
           ;; (progn (ignore* arg)
           ;;        (gud-call (cdb-toggle-breakpoint)))
           "\C-b" "Set breakpoint at current line.")
  (gud-def gud-tbreak
           "bu /1 `%d%f:%l` "
           "\C-t" "Set temporary breakpoint at current line.")
  (gud-def gud-step
           "t "
           "\C-s" "Step one source line with display.")
  (gud-def gud-next
           "p "
           "\C-n" "Step one line (skip functions).")
  (gud-def gud-cont
           "g "
           "\C-r" "Continue with display.")
  (gud-def gud-finish
           "g @$ra "
           "\C-f" "Finish executing current function.")
  (gud-def gud-jump
           "g `%d%f:%l` "
           "\C-j" "Set execution address to current line.")
  (gud-def gud-print
           "?? %e "
           "\C-p" "Evaluate C expression at point.")

  (setq comint-prompt-regexp +cdb-prompt-regexp+)
  (setq comint-prompt-read-only t)

  ;; M-{ and M-}
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'paragraph-start) +cdb-prompt-regexp+)

  (run-hooks 'gud-cdb-mode-hook)

  (loop* for x in gud-cdb-init-hook
         when (functionp x) do (funcall x))

  (add-hook 'completion-at-point-functions
            #'cdb-completion-at-point nil 'local))


(provide 'gud-cdb)


;; gud-cdb.el ends here
