;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; terms.el
;;;;


(defun term*-unify-shell-prompt ()
  "Unify the shell prompt in \\=`term\\='."
  (interactive)
  (unless (eq 'term-mode (buffer-local-value
                          'major-mode (current-buffer)))
    (user-error "%s" "Current buffer not in term-mode"))
  (let ((proc (get-buffer-process (current-buffer))))
    (unless proc
      (user-error "%s" "Current buffer has no process"))
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

(when-platform% 'windows-nt
  (defadvice ansi-term (before ansi-term-before first compile disable)
    (set-window-buffer
     (selected-window)
     (make-comint-in-buffer "ansi-term" nil "cmd"))))

(defun on-term-init! ()
  "On \\=`term\\=' initialization."
  (when-platform% 'windows-nt
  	(ad-enable-advice #'ansi-term 'before "ansi-term-before")
  	(ad-activate #'ansi-term t)))



(provide 'terms)

;; end of terms.el
