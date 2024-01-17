;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-sql-autoload.el
;;;;


(declare-function 'on-sql-init! (v-home%> "config/sqls"))
(autoload 'on-sql-init! (v-home%> "config/sqls"))

;;; after-load
(with-eval-after-load 'sql
  (on-sql-init!))

;;; autoload


;; end of on-sql-autoload.el
