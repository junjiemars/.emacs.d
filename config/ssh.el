;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; ssh.el
;;;;
;; Commentary: ssh remote.
;;;;


;;; require

;; end of require

(defmacro ssh-remote-p (file)
  "Return an identification when FILE specifies a location on a
remote system.\n
On ancient Emacs, \\=`file-remote-p\\=' will return a vector."
  `(string-match* "^\\(/sshx?:[_-a-zA-Z0-9]+@?[._-a-zA-Z0-9]+:\\)" ,file 1))

(defmacro ssh-remote->ids (remote)
  "Norm the REMOTE to (method {user|id} [host]) form."
  (let ((r (gensym*)))
    `(let ((,r ,remote))
       (and (stringp ,r)
            (split-string* ,r "[:@]" t "\\(^/[^ssh].*$\\|^/\\)")))))

(defun ssh-remote->user@host (remote)
  "Norm the REMOTE to {user|id}[@host] form."
  (let ((rid (ssh-remote->ids remote)))
    (when (consp rid)
      (concat (cadr rid)
              (and (car (cddr rid))
                   (concat "@" (car (cddr rid))))))))



(provide 'ssh)

;; end of ssh.el
