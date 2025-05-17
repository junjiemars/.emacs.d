;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; modules.el
;;;;


;;; require

(declare-function package-installed-p "package")

;; end of require


(defun module-spec->* (&optional key)
  "Extract :module from env-spec via KEY."
  (cond (key (*self-env-spec* :get :module key))
        (t (*self-env-spec* :get :module))))

(defmacro module-unit-spec->* (module key)
  "Extract unit spec from MODULE via KEY."
  `(plist-get ,module ,key))



(defalias '*package-init-repo*
  (let ((b))
    (lambda (&optional n)
      (if n (setq b n) b)))
  "Indicate \\=`package*-init-repo!\\=' whether has been called.")

(defun package*-init-repo! ()
  (setq% package-check-signature
         (module-spec->* :package-check-signature) package)
  (let ((archives (module-spec->* :package-archives)))
    (and archives (setq package-archives archives)))
  (when-var% package-archive-priorities package
    (let ((priorities (module-spec->* :package-archive-priorities)))
      (and priorities (setq package-archive-priorities priorities))))
  (package-refresh-contents))

(defun package*-delete! (package)
  "Delete PACKAGE."
  (when (and package (symbolp package))
    (if-version%
        <= 25.0
        (package-delete (cadr (assq package package-alist)) t nil)
      (if-version%
          <= 24.4
          (package-delete (cadr (assq package package-alist)))
        (package-delete
         (symbol-name package)
         (mapconcat
          #'identity
          (let ((xs nil))
            (dolist (x (aref (cdr (assq package package-alist)) 0)
                       (nreverse xs))
              (setq xs (cons (number-to-string x) xs))))
          "."))))))

(defun package*-install! (package &optional file)
  "Install PACKAGE optional via FILE."
  (condition-case err
      (progn
        (if file
            (package-install-file file)
          (unless (*package-init-repo*)
            (package*-init-repo!)
            (*package-init-repo* t))
          (if-version%
              <= 25.0
              (package-install package t)
            (package-install package)))
        t)
    (error (prog1 nil
             (message "Panic, package*-install!(%s): %s"
                      package err)))))

(defun package*-parse-spec! (spec &optional remove-unused)
  "Parse SPEC, install, REMOVE-UNUSED packages."
  (dolist (s spec)
    (let ((ss (cdr s)) (ps nil))
      (when (consp ss)
        (setq ps (module-unit-spec->* ss :packages))
        (cond ((module-unit-spec->* ss :cond)
               (let ((n nil) (f nil) (ns 0) (np 0) (cs nil))
                 (dolist (p ps)
                   (if (consp p)
                       (setq n (car p) f (cdr p))
                     (setq n p f nil))
                   (and n (setq ns (1+ ns))
                        (or (package-installed-p n) (package*-install! n f))
                        (setq np (1+ np))))
                 (and (= ns np)
                      (setq cs (module-unit-spec->* ss :compile))
                      (apply #'compile! cs))))
              (remove-unused
               (dolist (p ps)
                 (let ((n (if (consp p) (car p) p)))
                   (and n (package-installed-p n)
                        (package*-delete! n))))))))))

(defun self-module-init! ()
  "Initialize :package spec from \\=`*self-env-spec*\\='."
  ;; chmod .module/gnupg dir
  (when-var% package-gnupghome-dir package
    (eval-when-compile
      (set-file-modes (v-home! ".module/gnupg/") #o700))
    (setq package-gnupghome-dir (v-home% ".module/gnupg/")))
  (when-version%
      <= 25.1
    (setq custom-file (v-home! ".module/packages.el")))
  ;; define package user dir
  (setq% package-user-dir package*-user-dir package)
  ;; load self :packages-spec
  (compile! (compile-unit* (*self-paths* :get :mod-spec)))
  (package-initialize)
  (package*-parse-spec! (*self-mod-spec*) (module-spec->* :remove-unused))
  t)

;; end of package*

(provide 'modules)

;; end of modules.el
