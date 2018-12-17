;;;; -*- lexical-binding:t -*-
;;;;
;; gud-cdb.el: Grand Unified Debugger mode for running CDB
;;;;
;; improved from
;; https://raw.githubusercontent.com/4DA/kd-gud/master/cdb-gud.el
;;;;
;;
;; Author: Stephan Doll <stephan_doll at dantz.com>
;; Maintainer: Aaron Brady <abrady0 at yahoo dot com> :
;; Version: 1.0 (January 30, 2002)
;; Version: 1.4 (January 10, 2007) : updated to handle latest cdb
;; Version: 1.5 (January 26, 2011) : helper functions for debugging running processes
;; frames a little better. also added helper functions. see
;; 'cdbDebugChoice' at bottom for example.
;; Version: 1.5 (October 19, 2008) : parse fixes for latest debug tools.
;; Version: 1.6 (June 26, 2018) : improved in More Reasonable Emacs. see
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

;;;;
;; require
;;;;

(eval-when-compile (require 'cl))

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

(defcustom% gud-cdb-init-hook nil
  "Hook run by `lldb' process."
  :type 'hook
  :group 'gud)


(defconst +cdb-prompt-regexp+ "^\\(?:[0-9]:[0-9][0-9][0-9]> *\\)"
  "Regexp pattern of `cdb' prompt.")


(defvar *cdb-symbol-alist*
  (list "lm" "lmD" "lmv"
        "bl" "bu" "bp"
        "$ea" "$ea2" "$exp"
        "$ip" "$exentry" "$ra" "$retreg" "$csp"
        "$proc" "$thread" "$tpid" "$tid"
        "$peb" "$teb"
        "$frame"
        "$ptrsize" "$pagesize"
        "$argreg")
  "A list of `cdb' symbol.")


;; ;; buffer local variables

;; (make-variable-buffer-local 'gud-marker-acc)
(make-variable-buffer-local 'gud-output-acc)


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
      (loop for d in gud-cdb-directories
            do (let ((p (concat d "/" filename)))
                 (when (file-exists-p p) (return p))))))


(defun cdb-completion-at-point ()
  "Return the data to complete the `cdb' command before point."
  (interactive)
  (let* ((x (bounds-of-thing-at-point 'symbol))
         (start (car x))
         (end (cdr x)))
    (list start end *cdb-symbol-alist* . nil)))





;;;;
;; gud-cdb-*
;;;;


;; set default `gud-cdb-command-line-hook'
(add-hook 'gud-cdb-command-line-hook #'cdb-command-line-list-source)


(defun gud-cdb-massage-args (file args)
  "As the 2nd argument: message-args of `gud-common-init'.

`gud' callback it once when run `cdb'.

The job of the massage-args method is to modify the given list of
debugger arguments before running the debugger.
"
  (ignore* file)
  (append (loop for o in gud-cdb-command-line-hook
                append (funcall o)) args))


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
  (cond ((string-match "^\\(.*\\)(\\([0-9]+\\))\n" string)
         ;; Breakpoint 0 hit
         ;; e:\lab\c\src\c.c(9)
         ;; c!main:
         ;; 00007ff7`5a036580 4889542410      mov     qword ptr [rsp+10h],rdx ss:000000c5`9b0ff788=0000000000000000
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
         (setq gud-overlay-arrow-position nil))
        )
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





(defun cdb (command-line)
  "Run cdb on program FILE in buffer *gud-FILE*.

The directory containing FILE becomes the initial working
directory and source-file directory for your debugger."
  (interactive (list (gud-query-cmdline 'cdb)))

  (gud-common-init command-line
                   #'gud-cdb-massage-args
                   #'gud-cdb-marker-filter
                   #'gud-cdb-find-file)

  (set (make-local-variable 'gud-minor-mode) 'cdb)

  (gud-def gud-break  "bu `%d%f:%l` "     "\C-b" "Set breakpoint at current line.")
  (gud-def gud-tbreak "bu /1 `%d%f:%l` "  "\C-t" "Set temporary breakpoint at current line.")
  (gud-def gud-step   "t "                "\C-s" "Step one source line with display.")
  (gud-def gud-next   "p "                "\C-n" "Step one line (skip functions).")
  (gud-def gud-cont   "g "                "\C-r" "Continue with display.")
  (gud-def gud-finish "g @$ra "           "\C-f" "Finish executing current function.")
  (gud-def gud-jump   "g `%d%f:%l` "      "\C-j" "Set execution address to current line.")
  (gud-def gud-print  "?? %e "            "\C-p" "Evaluate C expression at point.")

  (setq comint-prompt-regexp +cdb-prompt-regexp+)
  (setq comint-prompt-read-only t)

  ;; M-{ and M-} 
  (setq (make-local-variable 'paragraph-separate) "\\'")
  (setq (make-local-variable 'paragraph-start) +cdb-prompt-regexp+)

  (run-hooks 'gud-cdb-mode-hook)
  (loop for x in gud-cdb-init-hook
        when (functionp x) do (funcall x))

  (add-hook 'comint-dynamic-complete-functions
            #'cdb-completion-at-point nil 'local))


;; (defun gud-cdb-goto-stackframe (text token indent)
;;   "Goto the stackframe described by TEXT, TOKEN, and INDENT."
;;   (speedbar-with-attached-buffer
;;    (gud-display-line (nth 2 token) (string-to-number (nth 3 token)))
;;    (gud-basic-call (concat ".frame " (nth 1 token)))))

;; (defvar gud-cdb-complete-in-progress)

;; (defvar gud-cdb-fetched-stack-frame nil
;;   "Stack frames we are fetching from CDB.")

;; (defvar gud-cdb-fetched-stack-frame-list nil
;;   "List of stack frames we are fetching from CDB.")

;; (defun gud-cdb-get-stackframe (buffer)
;;   "Extract the current stack frame out of the GUD CDB BUFFER."
;;   (let ((newlst nil)
;;         (gud-cdb-fetched-stack-frame-list nil))
;;     (gud-cdb-run-command-fetch-lines "kn " buffer)
;;     (if (and (car gud-cdb-fetched-stack-frame-list)
;;              (string-match "No stack" (car gud-cdb-fetched-stack-frame-list)))
;;         ;; Go into some other mode???
;;         nil
;;       (while gud-cdb-fetched-stack-frame-list
;;         (let ((e (car gud-cdb-fetched-stack-frame-list))
;;               (name nil) (num nil))
;;           (if (not (string-match "^\\([0-9a-f]+\\) [0-9a-f]* [0-9a-f]* \\([[a-zA-Z_0-9:$~!+]*\\).*$" e))
;;               nil
;;             (setq num (match-string 1 e)
;;                   name (match-string 2 e))
;;             (setq newlst
;;                   (cons
;;                    (if (string-match
;;                         "\\([-0-9a-zA-Z\\_.:]+\\) @ \\([0-9]+\\)" e)
;;                        (list name num (match-string 1 e)
;;                              (match-string 2 e))
;;                      (list name num))
;;                    newlst))))
;;         (setq gud-cdb-fetched-stack-frame-list
;;               (cdr gud-cdb-fetched-stack-frame-list)))
;;       (nreverse newlst))))


;; (defun gud-cdb-run-command-fetch-lines (command buffer)
;;   "Run COMMAND, and return when `gud-cdb-fetched-stack-frame-list' is full.
;; BUFFER is the GUD buffer in which to run the command."
;;   (save-excursion
;;     (set-buffer buffer)
;;     (if (save-excursion
;;           (goto-char (point-max))
;;           (forward-line 0)
;;           (not (looking-at comint-prompt-regexp)))
;;         nil
;;        ;; Much of this copied from CDB complete, but I'm grabbing the stack
;;       ;; frame instead.
;;       (let ((gud-marker-filter 'gud-cdb-speedbar-stack-filter))
;;         ;; Issue the command to CDB.
;;         (gud-basic-call command)
;;         (setq gud-cdb-complete-in-progress t)
;;         ;; Slurp the output.
;;         (while gud-cdb-complete-in-progress
;;           (accept-process-output (get-buffer-process gud-comint-buffer) 15))
;;         (setq gud-cdb-fetched-stack-frame nil
;;               gud-cdb-fetched-stack-frame-list
;;               (nreverse gud-cdb-fetched-stack-frame-list))))))


;; ;; *********************************************************************************
;; ;; cdb helpers  
;; ;; *********************************************************************************

;; (defun cdb-pidFromExe (&optional exe-name-regexp predicate)
;;   (interactive)
;;   (cdr (cdb-promptPidAndExe exe-name-regexp predicate)))

;; (defun cdb-promptPidAndExe (&optional exe-name-regexp predicate)
;;   (interactive)
;;   (let
;;      ((tlist)
;;       (exe)
;;       (exes))
;;    (setq tlist (shell-command-to-string "tlist"))
;;    (setq exes
;;        (loop for n from 0 for i in (reverse (split-string tlist "\n"))
;;          collect 
;;          (progn
;;            (if (string-match "\\(^[ 0-9]+ \\)\\(.*\\)" i)
;;              (cons (match-string 2 i) (match-string 1 i))))))
;;    (if exe-name-regexp 
;;      (setq exe (loop for i in exes if (string-match exe-name-regexp (car i)) return (car i)))
;;      (setq exe (completing-read "in: " ;; prompt 
;;                   exes ;; table
;;                   predicate ;; predicat (setq predicate (lambda (c) (string-match "mapserver.exe" (car c))))
;;                   nil ;; require match
;;                   nil ;; initial input
;;                   nil ;; hist
;;                   nil ;; def
;;                   nil ;; inherit input method
;;                   )))
;;    (cons exe (string-to-number (cdr (assoc exe exes))))))

;; (defun cdbAttach (exe-pid)
;;   (interactive (funcall (lambda () (list (cdb-pidFromExe)))))
;;    (if exe-pid
;;      (progn
;;        (gud-call (format ".attach 0n%i; " exe-pid)))))

;; (defun cdbDebugChoice (&optional predicate)
;;   "if you want to debug a program running on windows, call this function and it will give you the list of running processes and
;;    allow you to attach to one of them."
;;   (interactive)
;;   (let* ((exepidpair)
;;       (pid))
;;    (setq exepidpair (cdb-promptPidAndExe nil predicate))
;;    (setq pid (cdr exepidpair))
;;    (cdb (format "cdb -p %i" pid))
;;    (rename-buffer (format "*gud-%s*" (car (string-split "\\s-+" (car exepidpair) 1))) t)))

;; (defun cdbSetIP ()
;;   "set instruction pointer to current point. if you are in source code and want to change the current line to mark"
;;   (interactive)
;;   (gud-call (format "r eip = %s" (cdbLineNoKill))))
;; (defalias 'eip 'cdbSetIP)

;; (defun cdbLineNoKill ()
;;   "get current line in cdb format"
;;   (concat "`" (buffer-file-name) ":" (number-to-string (count-lines (point-min) (1+ (point)))) "`"))

;; (defun cdbLine ()
;;   "kill current line in cdb format"
;;   (interactive)
;;   (message (kill-new (cdbLineNoKill))))
;; (defalias 'xl 'cdbLine)

;; ;; ----------------------------------------
;; ;; auto-complete support

;; (defvar cdb-ac-match-limit t
;;   "limit for the number of matches to be collected in the cdb buffer or the src buffer for the current frame")

;; (defun cdb-ac-candidate-words-in-buffer (prefix)
;;   "cdb wants the words from the buffer for the current frame. the `ac-candidate-words-in-buffer' doesn't support this, but this version takes explicit params to do this"
;;   (let ((i 0)
;;         candidate
;;         candidates
;;         (regexp (concat "\\_<" (regexp-quote prefix) "\\(\\sw\\|\\s_\\)+\\_>")))
;;     (save-excursion
;;      (goto-char 0)
;;       ;; Search forward
;;       (while (and (or (eq cdb-ac-match-limit t)
;;                       (< i limit))
;;                   (re-search-forward regexp nil t))
;;         (setq candidate (match-string-no-properties 0))
;;         (unless (member candidate candidates)
;;           (push candidate candidates)
;;           (incf i)))
;;       (nreverse candidates))))

;; (defun cdb-ac-candidates ()
;;   "list of potentially matching auto-complete words"
;; ;;  (debug)
;;   (cond
;;    ((looking-back "\\(0x\\)[0-9]+") nil)
;;    (t
;;    (append
;;     (setq foo (ac-candidate-words-in-buffer))
;;     (if (and gud-last-last-frame (car gud-last-last-frame) (find-buffer-visiting (car gud-last-last-frame)))
;;       (with-current-buffer (find-buffer-visiting (car gud-last-last-frame))
;;         (cdb-ac-candidate-words-in-buffer ac-prefix)
;;         )
;;       )
;;     ))
;;    )
;;   )

;; (defvar cdb-ac-sources 
;;   '((candidates . cdb-ac-candidates) 
;;    (requires . 3)))

;; (defun cdb-ac-mode-init ()
;;   (interactive)
;;   "set up the auto complete variables for cdb"
;;   (auto-complete-mode t)
;; ;;  (ac-define-dictionary-source
;; ;;   ac-source-cdb-keywords
;; ;;   ("g" "k" "kn" "p")) ;; not really necessary with the 3 character requirement
;;   (setq ac-sources '(cdb-ac-sources
;;             ;; ac-source-cdb-keywords
;;             )))

;; (if (require 'auto-complete nil t)
;;      (add-hook 'cdb-mode-hook 'cdb-ac-mode-init))

;; ;;; cdb-gud.el ends here

(provide 'gud-cdb)
