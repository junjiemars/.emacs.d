;;; on-edit-autoload.el --- editing -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-key-autoload.el
;;;;
;;; Commentary:
;;


(defun self-keys-init! ()
  "Initialize key spec from \\=`*self-env-spec*\\='."
  (let ((key (*self-env-spec* :get :key)))
    (when (self-spec-> key :allowed)
      (let ((modifier (self-spec-> key :modifier)))
        (dolist* (x modifier)
          (set (car x) (cdr x)))))))


(make-thread* #'self-keys-init!)

;; end of `self-keys-init!'


(defun edit-keys-init! ()
  "Initialize editing keys."

  ;; Lookup dictionary
  (define-key% (current-global-map) (kbd "M-s d") 'lookup-dict)

  ;; Open file or url at point
  (when-fn% 'find-file-at-point 'ffap
    (define-key% (current-global-map) (kbd "C-c f f") #'find-file-at-point))

  ;; Shows a list of buffers
  (define-key% (current-global-map) (kbd "C-x C-b") #'ibuffer)

  ;; Interactive query replace key bindings.
  (define-key% (current-global-map) (kbd "M-%") #'query-replace-regexp)
  (define-key% (current-global-map) (kbd "C-M-%") #'query-replace)

  ;; Register
  ;; `C-x r g' and `C-x r i' are all bound to insert-register
  ;; let `C-x r g' do `string-insert-rectangle'
  (define-key% (current-global-map) (kbd "C-x r g") #'string-insert-rectangle)
  (define-key% (current-global-map) (kbd "C-x r v") #'view-register)

  ;; Line
  (when-fn% 'electric-newline-and-maybe-indent 'electric
    ;; Default behaviour of RET
    ;; https://lists.gnu.org/archive/html/emacs-devel/2013-10/msg00490.html
    ;; electric-indent-mode: abolition of `newline' function is not
    ;; the Right Thing
    ;; https://lists.gnu.org/archive/html/emacs-devel/2013-10/msg00407.html
    (define-key% (current-global-map) (kbd "RET")
      #'electric-newline-and-maybe-indent)
    (define-key% (current-global-map) (kbd "C-j") #'newline))

  ;; Sorting
  (define-key% (current-global-map) (kbd "C-c s f") #'sort-fields)
  (define-key% (current-global-map) (kbd "C-c s n") #'sort-numeric-fields)
  (define-key% (current-global-map) (kbd "C-c s x") #'sort-regexp-fields)
  (define-key% (current-global-map) (kbd "C-c s l") #'sort-lines)
  (define-key% (current-global-map) (kbd "C-c s r") #'reverse-region)
  (define-key% (current-global-map) (kbd "C-c s d") #'delete-duplicate-lines))


(make-thread* #'edit-keys-init!)

;; end of `edit-keys-init!'


 ; end of on-key-autoload.el
