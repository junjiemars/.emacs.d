;;; ncs.el --- boot -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;;; Commentary:
;;; native compile.
;;
;;; Code:


(when-native-comp%

  ;; slient native-comp warning
  (setq% native-comp-async-report-warnings-errors 'silent 'comp)

  (defconst +eln-load-root-dir+ (v-home! ".eln/")
    "The root dir of eln-cache loading.")

  ;; first eln load
  (push! +eln-load-root-dir+ native-comp-eln-load-path nil t)

  )


;;; eof
