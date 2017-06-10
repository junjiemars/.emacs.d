;;;;
;; Python
;;;;




(defun set-python-mode! ()
  ;; Enable paredit for Python
  (enable-paredit-mode)
  ;; Working with camel-case tokens
  (subword-mode))
(add-hook 'python-mode-hook #'set-python-mode!)

