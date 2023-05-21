;;;; -*- lexical-binding:t -*-
;;; mixvm.el --- mdk's mixvm / Emacs gud interaction
;; Copyright (C) 2001, 2019 Free Software Foundation, Inc.

;; Author: Philip Ellis King <pking@pdq.net>
;; Maintainer: Philip Ellis King <pking@pdq.net>
;; Created: 12 Feb 2001
;; Version: 0.2
;; Keywords: tools


;;; Commentary:
;; mixvm.el provides an interface between mdk's mixvm and Emacs,
;; via gud.  Place this file in your load-path, optionally adding
;; the following line to your .emacs file:
;; (autoload 'mixvm "mixvm" "mixvm/gud interaction" t)
;; Initiate a mdk/gud session with the command mixvm, gud will
;; reflect the current line in the source file buffer.

;; (mixvm.el is based on a study of gdb, perldb, and pdb as found
;; in gud.el, and rubydb3x.el distributed with the source code to
;; the Ruby language.

;;; Change Log:
;; Lexical and native-compiled.
;; Version 0.2
;; Initial release
;;;


;;; Code:
(require 'gud)
(provide 'mixvm)


;;;;
;;  gud-* declarations
;;;;

(declare-function gud*-find-c-last-expr "guds")
(declare-function gud-basic-call    "gud")
(declare-function gud-break         "gud")
(declare-function gud-call          "gud")
(declare-function gud-cont          "gud")
(declare-function gud-common-init   "gud")
(declare-function gud-down          "gud")
(declare-function gud-finish        "gud")
(declare-function gud-jump          "gud")
(declare-function gud-next          "gud")
(declare-function gud-print         "gud")
(declare-function gud-pv            "gud")
(declare-function gud-query-cmdline "gud")
(declare-function gud-remove        "gud")
(declare-function gud-statement     "gud")
(declare-function gud-step          "gud")
(declare-function gud-stepi         "gud")
(declare-function gud-tbreak        "gud")
(declare-function gud-until         "gud")
(declare-function gud-up            "gud")

 ;; end of gud-* declarations


;;; History of argument lists passed to mixvm.
(defvar gud-mixvm-history nil)

;; rubydb3x provided good examples of xemacs/emacs
;; compatibility (not interested at the moment)
(defun gud-mixvm-massage-args (file args)
  (ignore* file)
  (cons "--emacs" args))

;; There's no guarantee that Emacs will hand the filter the entire
;; marker at once; it could be broken up across several strings.  We
;; might even receive a big chunk with several markers in it.  If we
;; receive a chunk of text which looks like it might contain the
;; beginning of a marker, we save it here between calls to the
;; filter.
(defvar gud-mixvm-marker-acc "")
(make-variable-buffer-local 'gud-mixvm-marker-acc)

(defun gud-mixvm-marker-filter (string)
  (setq gud-mixvm-marker-acc (concat gud-mixvm-marker-acc string))
  (let ((output ""))

    ;; Process all the complete markers in this chunk.
    (while (string-match "\032\032mixvm:\\([^:]+\\):\\([0-9]+\\)"
                         gud-mixvm-marker-acc)
      (setq

       ;; Extract the frame position from the marker.
       gud-last-frame
       (cons (substring gud-mixvm-marker-acc (match-beginning 1) (match-end 1))
             (string-to-number (substring gud-mixvm-marker-acc
                                          (match-beginning 2)
                                          (match-end 2))))

       ;; Append any text before the marker to the output we're going
       ;; to return - we don't include the marker in this text.
       output (concat output
                      (substring gud-mixvm-marker-acc 0 (match-beginning 0)))

       ;; Set the accumulator to the remaining text.
       gud-mixvm-marker-acc (substring gud-mixvm-marker-acc (match-end 0))))

    ;; Does the remaining text look like it might end with the
    ;; beginning of another marker?  If it does, then keep it in
    ;; gud-mixvm-marker-acc until we receive the rest of it.  Since we
    ;; know the full marker regexp above failed, it's pretty simple to
    ;; test for marker starts.
    ;; (note: \\' matches the end of the string (Perl's '$'))
    (if (string-match "\032.*\\'" gud-mixvm-marker-acc)
        (progn
          ;; Everything before the potential marker start can be output.
          (setq output (concat output (substring gud-mixvm-marker-acc
                                                 0 (match-beginning 0))))

          ;; Everything after, we save, to combine with later input.
          (setq gud-mixvm-marker-acc
                (substring gud-mixvm-marker-acc (match-beginning 0))))

      (setq output (concat output gud-mixvm-marker-acc)
            gud-mixvm-marker-acc ""))

    output))

;; See gdb for more comprehensive example
;; pek: it bugs me that this is run for EVERY interactive
;; mixvm command, should we cache some info somewhere?
(defun gud-mixvm-find-file (file)
  (find-file-noselect file))


(defvar mixvm-minibuffer-local-map nil
  "Keymap for minibuffer prompting of mixvm startup command.")
(if mixvm-minibuffer-local-map
    ()
  (setq mixvm-minibuffer-local-map (copy-keymap minibuffer-local-map))
  (define-key
    mixvm-minibuffer-local-map "\C-i" 'comint-dynamic-complete-filename))


(defcustom gud-mixvm-command-name "mixvm"
  "File name for executing the mixvm debugger.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'gud)


(defun mixvm*-fix ()
  "Fix some data in `mixal-mode'."
  (when-var% mixal-operation-codes-alist 'mixal-mode
    (let ((aa '((HLT :a 5 :ai 3 :w 2 :wi 4 :l 7)
                (SLA :a 6 :ai 3 :w 0 :wi 4 :l 7)
                (SRA :a 6 :ai 3 :w 1 :wi 4 :l 7)
                (SLAX :a 6 :ai 3 :w 2 :wi 4 :l 7)
                (SRAX :a 6 :ai 3 :w 3 :wi 4 :l 7)
                (SLC :a 6 :ai 3 :w 4 :wi 4 :l 7)
                (SRC :a 6 :ai 3 :w 5 :wi 4 :l 7)
                (INCA :a 48 :ai 3 :w 0 :wi 4 :l 7)
                (INCX :a 55 :ai 3 :w 0 :wi 4 :l 7)
                (INC1 :a 49 :ai 3 :w 0 :wi 4 :l 7)
                (INC2 :a 50 :ai 3 :w 0 :wi 4 :l 7)
                (INC3 :a 51 :ai 3 :w 0 :wi 4 :l 7)
                (INC4 :a 52 :ai 3 :w 0 :wi 4 :l 7)
                (INC5 :a 53 :ai 3 :w 0 :wi 4 :l 7)
                (INC6 :a 54 :ai 3 :w 0 :wi 4 :l 7)
                (DECA :a 48 :ai 3 :w 1 :wi 4 :l 7)
                (DECX :a 55 :ai 3 :w 1 :wi 4 :l 7)
                (DEC1 :a 49 :ai 3 :w 1 :wi 4 :l 7)
                (DEC2 :a 50 :ai 3 :w 1 :wi 4 :l 7)
                (DEC3 :a 51 :ai 3 :w 1 :wi 4 :l 7)
                (DEC4 :a 52 :ai 3 :w 1 :wi 4 :l 7)
                (DEC5 :a 53 :ai 3 :w 1 :wi 4 :l 7)
                (DEC6 :a 54 :ai 3 :w 1 :wi 4 :l 7))))
      (dolist* (x aa)
        (let ((a1 (assoc** (car x) mixal-operation-codes-alist))
              (a (plist-get (cdr x) :a))
              (ai (plist-get (cdr x) :ai))
              (w (plist-get (cdr x) :w))
              (wi (plist-get (cdr x) :wi))
              (l (plist-get (cdr x) :l)))
          (when (and (< (length a1) l) (= (nth ai a1) a))
            (setcdr a1 (cdr (seq-ins! w a1 wi)))))))))


;;;###autoload
(defun mixvm (command-line)
  "Run mixvm on program FILE in buffer `*gud-FILE*'.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger."
  (interactive
   (list (read-from-minibuffer "Run mixvm (like this): "
                               (if (consp gud-mixvm-history)
                                   (car gud-mixvm-history)
                                 (concat gud-mixvm-command-name " "))
                               mixvm-minibuffer-local-map nil
                               '(gud-mixvm-history . 1))))
  (mixvm*-fix)
  (gud-common-init command-line 'gud-mixvm-massage-args
                   'gud-mixvm-marker-filter 'gud-mixvm-find-file)

  (gud-def gud-break "sbp %l" "\C-b" "Set breakpoint at current line.")
  (gud-def gud-remove "cbp %l" "\C-d" "Remove breakpoint at current line")
  (gud-def gud-step "next" "\C-s" "Step one source line with display.")
  (gud-def gud-next "next" "\C-n" "Step one line.")
  (gud-def gud-stepi "next" "\C-i" "Step one line.")
  (gud-def gud-cont "run" "\C-r" "Continue with display.")
  (gud-def gud-finish "run" "\C-f" "Finish executing current function.")
  (gud-def gud-print "weval %e" "\C-p" "Evaluate expression at point.")

  (setq comint-prompt-regexp "^MIX > ")
  (set (make-local-variable 'paragraph-start) comint-prompt-regexp)
  (run-hooks 'mixvm-mode-hook))

;;; mixvm.el ends here
