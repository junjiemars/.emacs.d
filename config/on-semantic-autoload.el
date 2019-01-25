;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-semantic-autoload.el
;;;;


(feature-semantic-supported-p

  (defun set-semantic-cc-env! (&optional project-includes project-roots preprocessors)
    "Use `semantic-mode' in`c-mode'.

PROJECT-INCLUDES specify C include directories
via `semantic-add-system-include',
check it by `semantic-dependency-system-include-path'.'

PROJECT-ROOTS specify C project root directories
via `semanticdb-_project-roots'.

PREPROCESSORS specify C preprocessors
via `semantic-lex-c-preprocessor-symbol-map'

Use `semantic-c-describe-environment' to describe the current C environment."
    (semantic-reset-system-include 'c-mode)

    (dolist (x (append (when-fn% 'system-cc-include nil (system-cc-include t))
                       project-includes))
      (semantic-add-system-include x 'c-mode))

    (setq% semanticdb-project-roots project-roots 'semantic/db)

    (when-fn% 'global-semantic-idle-summary-mode 'semantic
      (global-semantic-idle-summary-mode))

    (when-fn% 'semantic-ia-fast-jump 'semantic
      (define-key semantic-mode-map (kbd "C-c , f") #'semantic-ia-fast-jump))

    (when-fn% 'semantic-ia-complete-symbol 'semantic
      (define-key semantic-mode-map (kbd "C-c , TAB") #'semantic-ia-complete-symbol))

    (setq% semantic-lex-c-preprocessor-symbol-map
           preprocessors 'semantic/bovine/c)))


(feature-semantic-supported-p
  (when-fn% 'view-system-cc-include nil
    
    (defadvice semantic-decoration-include-visit (after semantic-decoration-include-visit-after compile)
      "Visit the include file in `view-mode."
      (view-system-cc-include ad-return-value))))


(feature-semantic-supported-p
  (when-fn% 'view-system-cc-include nil
    
    (with-eval-after-load 'semantic/decorate/include
      (ad-activate #'semantic-decoration-include-visit t))))


;; end of file
