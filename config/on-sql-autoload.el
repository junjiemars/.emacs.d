;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-sql-autoload.el
;;;;


(when-fn% 'sql-show-sqli-buffer 'sql

  (defun sql-show-sqli-buffer* ()
    "Display the current SQLi buffer.

See `sql-show-sqli-buffer'."
    (interactive)
    (unless (get-buffer-process sql-buffer)
      (call-interactively #'sql-connect))
    (call-interactively #'sql-show-sqli-buffer)))


(when-fn% 'sql-send-magic-terminator 'sql

  (defadvice sql-send-magic-terminator
      (before sql-send-magic-terminator-before compile)
    "Send TERMINATOR to buffer BUF if its not present in STR."
    ;; terminator
    (cond ((eq 'mysql sql-product)
           (ad-set-arg 2 t)))))


(when-fn% 'sql-execute-feature 'sql

  (defun sql-desc-table (name &optional enhanced)
    "Describe the details of a database table named NAME.

Optional prefix argument ENHANCED, displays additional details
about each column."
    (interactive (list (sql-read-table-name "Table name: ")
                       current-prefix-arg))
    (let ((sqlbuf (sql-find-sqli-buffer)))
      (unless sqlbuf
        (user-error "No SQL interactive buffer found"))
      (unless name
        (user-error "No table name specified"))
      (sql-execute-feature sqlbuf (format "*Desc %s*" name)
                           :desc-table enhanced name))))


(when-fn% 'sql-execute-feature 'sql

  (defun sql-desc-plan (plan &optional enhanced)
    "Describe a query execution plan named PLAN.

Optional prefix argument ENHANCED, displays additional details."
    (interactive
     (list (region-active-if
               (buffer-substring-no-properties (region-beginning)
                                               (region-end))
             (sql-read-table-name "Plan: "))
           current-prefix-arg))
    (let ((sqlbuf (sql-find-sqli-buffer)))
      (unless sqlbuf
        (user-error "No SQL interactive buffer found"))
      (unless plan
        (user-error "No plan specified"))
      (sql-execute-feature sqlbuf (format "*Desc plan %s*"
                                          (car (split-string* plan)))
                           :desc-plan enhanced plan))))


(when-fn% 'sql-execute-feature 'sql

  (defun sql-list-code (name &optional enhanced)
    "List the code of the database object with qualified NAME. "
    (interactive (list (sql-read-table-name "Qualified name: ")
                       current-prefix-arg))
    (let ((sqlbuf (sql-find-sqli-buffer)))
      (unless sqlbuf
        (user-error* "No SQL interactive buffer found"))
      (unless name
        (user-error* "No name specified"))
      (sql-execute-feature sqlbuf (format "*List code %s*" name)
                           :list-code enhanced name))))


(when-fn% 'sql-execute-feature 'sql

  (defun sql-list-index (name &optional enhanced)
    "List the index of a database table named NAME. "
    (interactive (list (sql-read-table-name "Table name: ")
                       current-prefix-arg))
    (let ((sqlbuf (sql-find-sqli-buffer)))
      (unless sqlbuf
        (user-error* "No SQL interactive buffer found"))
      (unless name
        (user-error* "No table name specified"))
      (sql-execute-feature sqlbuf (format "*List index %s*" name)
                           :list-index enhanced name))))


;;;
;; oracle
;;;

(unless-fn% 'sql-oracle--list-object-name 'sql

  (defun sql-oracle--list-object-name (obj-name)
    (format (concat "CASE WHEN REGEXP_LIKE (%s, q'/^[A-Z0-9_#$]+$/','c')"
                    " THEN %s ELSE '\"'|| %s ||'\"' END ")
            obj-name obj-name obj-name)))


(when-fn% 'sql-oracle-restore-settings 'sql

  (defun sql-oracle-list-all* (sqlbuf outbuf enhanced _table-name)
    ;; Query from USER_OBJECTS or ALL_OBJECTS
    (let ((settings (sql-oracle-save-settings sqlbuf))
          (simple-sql
           (concat
            "SELECT INITCAP(x.object_type) AS SQL_EL_TYPE "
            ", " (sql-oracle--list-object-name "x.object_name")
            " AS SQL_EL_NAME "
            "FROM user_objects                    x "
            "WHERE x.object_type NOT LIKE '%% BODY' "
            "ORDER BY SQL_EL_TYPE, SQL_EL_NAME;"))
          (enhanced-sql
           (concat
            "SELECT INITCAP(x.object_type) AS SQL_EL_TYPE "
            ", "  (sql-oracle--list-object-name "x.owner")
            " ||'.'|| "  (sql-oracle--list-object-name "x.object_name")
            " AS SQL_EL_NAME "
            "FROM all_objects x "
            "WHERE x.object_type NOT LIKE '%% BODY' "
            "AND x.owner <> 'SYS' "
            "ORDER BY SQL_EL_TYPE, SQL_EL_NAME;")))
      (sql-redirect sqlbuf
                    (concat "SET LINESIZE 80 PAGESIZE 50000 TRIMOUT ON"
                            " TAB OFF TIMING OFF FEEDBACK OFF"))
      (sql-redirect sqlbuf
                    (list "COLUMN SQL_EL_TYPE  HEADING \"Type\" FORMAT A19"
                          "COLUMN SQL_EL_NAME  HEADING \"Name\""
                          (format "COLUMN SQL_EL_NAME  FORMAT A%d"
                                  (if enhanced 60 35))))
      (sql-redirect sqlbuf
                    (if enhanced enhanced-sql simple-sql)
                    outbuf)
      (sql-redirect sqlbuf
                    '("COLUMN SQL_EL_NAME CLEAR"
                      "COLUMN SQL_EL_TYPE CLEAR"))
      (sql-oracle-restore-settings sqlbuf settings))))


(when-fn% 'sql-oracle-restore-settings 'sql

  (defun sql-oracle-list-code (sqlbuf outbuf enhanced target)
    "List code of oracle's TARGET."
    (let ((settings (sql-oracle-save-settings sqlbuf))
          (simple-sql
           (concat
            "SELECT text"
            " FROM all_source"
            (format " WHERE name = '%s'" target)
            " ORDER BY line;"))
          (enhanced-sql nil))
      (sql-redirect sqlbuf
                    (if enhanced enhanced-sql simple-sql)
                    outbuf)
      (sql-redirect sqlbuf
                    (concat "SET LINESIZE 80 PAGESIZE 50000 TRIMOUT ON"
                            " TAB OFF TIMING OFF FEEDBACK OFF"))
      (sql-oracle-restore-settings sqlbuf settings))))



;;;
;; mysql
;;;


(defun sql-mysql-desc-table (sqlbuf outbuf enhanced table)
  "Describe mysql table."
  (let ((simple-sql
         (concat
          "SELECT *"
          " FROM information_schema.columns"
          (format " WHERE table_name = '%s'\\G" table)))
        (enhanced-sql nil))
    (sql-redirect sqlbuf
                  (if enhanced enhanced-sql simple-sql)
                  outbuf)))


(defun sql-mysql-desc-plan (sqlbuf outbuf enhanced query)
  "Describe execution plan of mysql's QUERY."
  (let ((simple-sql
         (concat
          "explain FORMAT=json "
          (string-trim> query "[\t\n\r\\g\\G;]+")
          "\\G"))
        (enhanced-sql nil))
    (sql-redirect sqlbuf
                  (if enhanced enhanced-sql simple-sql)
                  outbuf)))


(defun sql-mysql-list-code (sqlbuf outbuf enhanced target)
  "List code of mysql's TARGET."
  (let ((simple-sql
         (concat
          "SHOW CREATE"
          (format " %s\\G" target)))
        (enhanced-sql nil))
    (sql-redirect sqlbuf
                  (if enhanced enhanced-sql simple-sql)
                  outbuf)))


(defun sql-mysql-list-index (sqlbuf outbuf enhanced table)
  "List index of mysql's TABLE."
  (let ((simple-sql
         (concat
          "SHOW INDEX"
          (format " FROM %s\\G" table)))
        (enhanced-sql nil))
    (sql-redirect sqlbuf
                  (if enhanced enhanced-sql simple-sql)
                  outbuf)))


 ;; end of mysql



;;;
;; after load
;;;

(with-eval-after-load 'sql

  (when-fn% 'sql-execute-feature 'sql

    ;; oracle
    (when-fn% 'sql-oracle-restore-settings 'sql

      ;; oralce: replace `:list-all'
      (when (plist-get (cdr (assoc** 'oracle sql-product-alist))
                       :list-all)
        (plist-put (cdr (assoc** 'oracle sql-product-alist))
                   :list-all
                   #'sql-oracle-list-all*))

      ;; oracle: new `:list-code'
      (plist-put (cdr (assoc** 'oracle sql-product-alist))
                 :list-code
                 #'sql-oracle-list-code))


    ;; mysql: `sql-mysql-program'
    (unless% (executable-find% sql-mysql-program)
      (if-platform% 'darwin
          (setq sql-mysql-program
                (or (executable-find%
                     "/Applications/MySQLWorkbench.app/Contents/MacOS/mysql")
                    "mysql"))))

    ;; mysql: new `:desc-table'
    (plist-put (cdr (assoc** 'mysql sql-product-alist))
               :desc-table
               #'sql-mysql-desc-table)

    ;; mysql: new `:desc-plan'
    (plist-put (cdr (assoc** 'mysql sql-product-alist))
               :desc-plan
               #'sql-mysql-desc-plan)

    ;; mysql: new `:list-code'
    (plist-put (cdr (assoc** 'mysql sql-product-alist))
               :list-code
               #'sql-mysql-list-code)

    ;; mysql: new `:list-index'
    (plist-put (cdr (assoc** 'mysql sql-product-alist))
               :list-index
               #'sql-mysql-list-index)


    (when-fn% 'sql-send-magic-terminator 'sql
      ;; mysql: `:terminator'
      (plist-put (cdr (assoc** 'mysql sql-product-alist))
                 :terminator
                 '("\\G" . ""))

      (ad-enable-advice #'sql-send-magic-terminator 'before
                        "sql-send-magic-terminator-before")
      (ad-activate #'sql-send-magic-terminator t))


    (define-key% sql-mode-map (kbd "C-c C-l c") #'sql-list-code)
    (define-key% sql-mode-map (kbd "C-c C-l i") #'sql-list-index)
    (define-key% sql-mode-map (kbd "C-c C-d t") #'sql-desc-table)
    (define-key% sql-mode-map (kbd "C-c C-d p") #'sql-desc-plan)

    (define-key% sql-interactive-mode-map (kbd "C-c C-l c") #'sql-list-code)
    (define-key% sql-interactive-mode-map (kbd "C-c C-l i") #'sql-list-index)
    (define-key% sql-interactive-mode-map (kbd "C-c C-d t") #'sql-desc-table)
    (define-key% sql-interactive-mode-map (kbd "C-c C-d p") #'sql-desc-plan))


  (when-fn% 'sql-show-sqli-buffer 'sql
    (define-key% sql-mode-map (kbd "C-c C-z") #'sql-show-sqli-buffer*)))




 ;; end of on-sql-autoload.el
