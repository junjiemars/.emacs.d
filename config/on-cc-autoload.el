;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-cc-autoload.el
;;;;
;; Commentary: `cc' autoload.
;;;;

(require 'cc (v-home%> "config/cc"))

;;;
;; `cc-mode'
;;;

(defun on-cc-mode-init! ()
  "On \\=`cc-mode\\=' initialization."
  ;; load styles
  (c-add-style (car cc*-style-nginx) (cdr cc*-style-nginx))
  ;; keymap:
  ;; find include file
  (when-fn-ff-find-other-file%
   (when-var% c-mode-map 'cc-mode
     (define-key% c-mode-map (kbd "C-c f i")
                  #'cc*-find-include-file))
   ;; for c++, add include via `cc*-extra-include'
   (when-var% c++mode-map 'cc-mode
       (define-key% c++-mode-map (kbd "C-c f i")
                    #'cc*-find-include-file)))
  ;; indent line or region
  (when-fn% 'c-indent-line-or-region 'cc-cmds
    (define-key% c-mode-map
                 (kbd "TAB") #'c-indent-line-or-region))
  ;; dump predefined macros
  (define-key% c-mode-map (kbd "C-c #")
               #'cc*-dump-predefined-macros)
  ;; raw newline
  (define-key% c-mode-map (kbd "RET") #'newline*)
  ;; align style
  (define-key% c-mode-map (kbd "C-c |") #'cc*-style-align-entire)
  ;; `subword-mode'
  (define-key% c-mode-map (kbd "C-c C-w")
               (if-fn% 'subword-mode 'subword
                       #'subword-mode
                 #'c-subword-mode)))

;; default `c-mode-hook'
;; involving useless `macrostep-c-mode-hook'.
(setq% c-mode-hook nil 'cc-mode)

(with-eval-after-load 'cc-mode
  (on-cc-mode-init!))

;; end of `cc-mode'

;;;
;; `man'
;;;

;;; `man' after load
(when-var% manual-program 'man
  (when% (executable-find% manual-program)
    (with-eval-after-load 'man
      ;; fix cannot find include path on Darwin in `Man-mode'
      (setq% Man-header-file-path (cc*-system-include t) 'man))))

;; end of `man'

;;;
;; `cmacexp'
;;;

(defmacro-if-feature% cmacexp)

(defmacro when-feature-cmacexp% (&rest body)
  (declare (indent 0))
  (if-feature-cmacexp%
      `(progn% ,@body)
    `(comment ,@body)))

(when-feature-cmacexp%
  (defadvice c-macro-expand (around c-macro-expand-around disable)
    "Expand C macros in the region, using the C preprocessor."
    (let ((m (cc*-macro-expand (current-buffer))))
      (setq% c-macro-buffer-name
             (plist-get m :c-macro-buffer-name)
             'cmacexp)
      (setq% c-macro-preprocessor
             (plist-get m :c-macro-preprocessor)
             'cmacexp)
      ad-do-it)))

(when-feature-cmacexp%
  (defun on-cmacexp-init! ()
    "On \\=`cmacexp\\=' initialization."
    ;; [C-c C-e] `c-macro-expand' in `cc-mode'
    (setq% c-macro-prompt-flag t 'cmacexp)
    (ad-enable-advice #'c-macro-expand
                      'around "c-macro-expand-around")
    (ad-activate #'c-macro-expand t)))

;;; `cmacexp' after load
(when-feature-cmacexp%
  (with-eval-after-load 'cmacexp
    (on-cmacexp-init!)))

;; end of `cmacexp'


;; end of on-cc-autoload.el
