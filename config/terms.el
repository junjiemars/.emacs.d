;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; terms.el
;;;;


;;; require

(require% 'term)

 ;; end of require

(defun term*-unify-shell-prompt ()
  "Unify the shell prompt in \\=`term\\='."
  (interactive)
  (unless (eq 'term-mode (buffer-local-value 'major-mode (current-buffer)))
    (user-error "%s" "Not in term-mode"))
  (let ((proc (get-buffer-process (current-buffer))))
    (unless proc
      (user-error "%s" "No term process"))
    (process-send-string
     proc
     (format
      "
export INSIDE_EMACS=\"%s,term:\"%s
case \"`basename $SHELL`\" in
  zsh)
    export PS1='%s'
    export PROMPT_COMMAND=''
    ;;
  *)
    export PS1='%s'
    export PROMPT_COMMAND=''
    ;;
esac
"
      emacs-version term-protocol-version
      (or (and (*self-env-spec* :get :shell :allowed)
               (*self-env-spec* :get :shell :prompt :zsh))
          "%n@%m %1~ %# ")
      (or (and (*self-env-spec* :get :shell :allowed)
               (*self-env-spec* :get :shell :prompt :bash))
          "\\u@\\h \\W \\$ ")))))

(when-platform% windows-nt
  (defun ansi-term* (&rest _)
    (interactive)
    (set-window-buffer
     (selected-window)
     (make-comint-in-buffer "ansi-term" nil "cmd"))
    (call-interactively (symbol-function '_ansi-term_))))

(defun on-term-init! ()
  "On \\=`term\\=' initialization."
  (when-platform% windows-nt
    (defadvice* '_ansi_term_ 'ansi-term #'ansi-term*)))



(provide 'terms)

;; end of terms.el
