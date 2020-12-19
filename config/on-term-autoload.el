;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-term-autoload.el
;;;;


(defun term-unify-shell-prompt ()
  "Unify the shell prompt in `term'."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (unless proc
      (user-error "Current buffer has no process"))
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



;; eof
