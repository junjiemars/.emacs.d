;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; sqls.el
;;;;

;;; when-*macro

(defmacro when-sql-feature% (&rest body)
  (declare (indent 0))
  `(when-fn% 'sql-execute-feature 'sql
     ,@body))

(defmacro when-sql-oracle-feature% (&rest body)
  (declare (indent 0))
  `(when-sql-feature%
     ,@body))

(defmacro when-sql-mysql-feature% (&rest body)
  (declare (indent 0))
  `(when-sql-feature%
     ,@body))

(defmacro when-sql-oceanbase-feature% (&rest body)
  (declare (indent 0))
  `(when-sql-feature%
     ,@body))

(defmacro when-fn-sql-show-sqli-buffer% (&rest body)
  (declare (indent 0))
  `(when-fn% 'sql-show-sqli-buffer 'sql
     ,@body))

(defmacro when-fn-sql-send-magic-terminator% (&rest body)
  (declare (indent 0))
  `(when-fn% 'sql-send-magic-terminator 'sql
     ,@body))

;; end of when-* macro

;;; sqli

(when-fn-sql-show-sqli-buffer%

  (defun sql-show-sqli-buffer* ()
    "Display the current SQLi buffer."
    (interactive)
    (unless (get-buffer-process sql-buffer)
      (call-interactively #'sql-connect))
    (call-interactively #'sql-show-sqli-buffer)))


(when-fn-sql-send-magic-terminator%

  (defadvice sql-send-magic-terminator
      (before sql-send-magic-terminator-before first compile disable)
    "Send TERMINATOR to buffer BUF if its not present in STR."
    ;; terminator
    (cond ((eq 'mysql sql-product)
           (ad-set-arg 2 t)))))

;; end of sqli

;;; features

(when-sql-feature%

  (defun sql-first-word (sql)
    "Return the first word in SQL."
    (let* ((i 0) (j nil) (c (aref sql i)))
      (while (not (or (and (>= c ?A) (<= c ?Z))
                      (and (>= c ?a) (<= c ?z))))
        (setq i (1+ i) c (aref sql i)))
      (setq j i)
      (while (or (and (>= c ?A) (<= c ?Z))
                 (and (>= c ?a) (<= c ?z)))
        (setq j (1+ j) c (aref sql j)))
      (substring sql i j))))


(when-sql-feature%

  (defun sql-desc-table (name &optional enhanced)
    "Describe the details of a database table named NAME.\n
Optional prefix argument ENHANCED, displays additional details
about each column."
    (interactive (list (sql-read-table-name "Table name: ")
                       current-prefix-arg))
    (let ((sqlbuf (sql-find-sqli-buffer)))
      (unless sqlbuf
        (user-error "%s" "No SQL interactive buffer found"))
      (unless name
        (user-error "%s" "No table name specified"))
      (sql-execute-feature sqlbuf (format "*Desc %s*" name)
                           :desc-table enhanced name))))


(when-sql-feature%

  (defun sql-desc-plan (plan &optional enhanced)
    "Describe an execution plan named PLAN.\n
Optional prefix argument ENHANCED, displays additional details."
    (interactive
     (list (if-region-active
               (buffer-substring-no-properties
                (region-beginning) (region-end))
             (buffer-substring-no-properties
              (save-excursion (backward-paragraph) (point))
              (save-excursion (forward-paragraph) (point))))
           current-prefix-arg))
    (let ((sqlbuf (sql-find-sqli-buffer)))
      (unless sqlbuf
        (user-error "%s" "No SQL interactive buffer found"))
      (unless plan
        (user-error "%s" "No plan specified"))
      (sql-execute-feature sqlbuf
                           (format "*Desc plan %s*"
                                   (sql-first-word plan))
                           :desc-plan enhanced plan))))


(when-sql-feature%

  (defun sql-list-code (name &optional enhanced)
    "List the code of the database object with qualified NAME."
    (interactive (list (sql-read-table-name "Qualified name: ")
                       current-prefix-arg))
    (let ((sqlbuf (sql-find-sqli-buffer)))
      (unless sqlbuf
        (user-error "%s" "No SQL interactive buffer found"))
      (unless name
        (user-error "%s" "No name specified"))
      (sql-execute-feature sqlbuf (format "*List code %s*" name)
                           :list-code enhanced name))))


(when-sql-feature%

  (defun sql-list-index (name &optional enhanced)
    "List the index of a database table named NAME."
    (interactive (list (sql-read-table-name "Table name: ")
                       current-prefix-arg))
    (let ((sqlbuf (sql-find-sqli-buffer)))
      (unless sqlbuf
        (user-error "%s" "No SQL interactive buffer found"))
      (unless name
        (user-error "%s" "No table name specified"))
      (sql-execute-feature sqlbuf (format "*List index %s*" name)
                           :list-index enhanced name))))

;; end of features

;;;
;; oracle
;;;

(unless-fn% 'sql-oracle--list-object-name 'sql
  (defun sql-oracle--list-object-name (obj-name)
    (format (concat
             "CASE WHEN REGEXP_LIKE (%s, q'/^[A-Z0-9_#$]+$/','c')"
             " THEN %s ELSE '\"'|| %s ||'\"' END ")
            obj-name obj-name obj-name)))

(unless-fn% 'sql-oracle-restore-settings 'sql
  (defun sql-oracle-restore-settings (_ __)
    (message "unimplmented")))

(when-sql-oracle-feature%
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
            " ||'.'|| "
            (sql-oracle--list-object-name "x.object_name")
            " AS SQL_EL_NAME "
            "FROM all_objects x "
            "WHERE x.object_type NOT LIKE '%% BODY' "
            "AND x.owner <> 'SYS' "
            "ORDER BY SQL_EL_TYPE, SQL_EL_NAME;")))
      (sql-redirect
       sqlbuf
       (concat "SET LINESIZE 80 PAGESIZE 50000 TRIMOUT ON"
               " TAB OFF TIMING OFF FEEDBACK OFF"))
      (sql-redirect
       sqlbuf
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


(when-sql-oracle-feature%

  (defun sql-oracle-list-code (sqlbuf outbuf enhanced target)
    "List code of oracle's TARGET."
    (let ((settings (sql-oracle-save-settings sqlbuf))
  			  (o (upcase (if enhanced
                         ;; ignore enhanced
                         target
                       target)))
  			  (sql-var (concat
                    "VAR object_type VARCHAR2(128);"
                    "\nBEGIN\n"
                    " :object_type := '';"
                    " SELECT object_type INTO :object_type"
                    " FROM user_objects"
                    " WHERE object_name='%s';"
                    "\nEND;\n/\n"))
          (sql-ddl (concat
                    "SELECT DBMS_METADATA.GET_DDL(:object_type,'%s')"
                    " FROM DUAL;")))
      (unwind-protect
          (progn
            (sql-redirect sqlbuf (format sql-var o) outbuf)
            (sql-redirect
             sqlbuf
             (concat "SET LINESIZE 200"
                     " LONG 4096000"
                     " LONGCHUNKSIZE 4096000"
                     " PAGESIZE 0"
                     " FEEDBACK OFF"
                     " TAB OFF"
                     " TERMOUT OFF"
                     " TIMING OFF"
                     " TRIMOUT ON"
                     " VERIFY OFF;"))
            (sql-redirect sqlbuf (format sql-ddl o) outbuf))
        (sql-oracle-restore-settings sqlbuf settings)
        (sql-send-string "\n")))))


(when-sql-oracle-feature%

  (defun sql-oracle-desc-plan (sqlbuf outbuf enhanced target)
    "Describe execution plan of mysql's QUERY."
    (let ((settings (sql-oracle-save-settings sqlbuf))
  				(sql (if enhanced
                   ;; ignore enhanced
                   target
  							 target)))
      (unwind-protect
          (progn
            (sql-redirect
             sqlbuf
             (concat "SET AUTOTRACE ON EXPLAIN"))
            (sql-redirect
             sqlbuf
             (concat "SET LINESIZE 100 PAGESIZE 100"
                     " VERIFY OFF FEEDBACK OFF"
                     " TRIMOUT ON TAB OFF TIMING OFF"))
            (sql-redirect sqlbuf sql outbuf))
        (sql-oracle-restore-settings sqlbuf settings)))))

;; end of oracle

;;;
;; mysql
;;;

(when-sql-mysql-feature%

  (defun sql-mysql-norm (sql)
    "Normlize SQL."
    (with-temp-buffer
      (insert sql)
      (goto-char (point-min))
      (flush-lines "[ \t]*\\(--+\\|#+\\).*")
      (goto-char (point-min))
      (while (search-forward-regexp "[ \t\n]+" nil t)
        (replace-match " " t t))
      (buffer-substring (point-min) (point-max)))))


(when-sql-mysql-feature%

  (defun sql-mysql-desc-table (sqlbuf outbuf enhanced table)
    "Describe mysql table."
    (let ((simple-sql
           (concat
            "SHOW FULL COLUMNS "
            (format "FROM %s\\G" table)))
          (enhanced-sql nil))
      (sql-redirect sqlbuf
                    (if enhanced enhanced-sql simple-sql)
                    outbuf))))

(when-sql-mysql-feature%

  (defun sql-mysql-desc-plan (sqlbuf outbuf enhanced query)
    "Describe execution plan of mysql's QUERY."
    (let ((sql
           (concat
            "explain FORMAT=" (if enhanced "JSON " "TRADITIONAL ")
            (string-trim> (sql-mysql-norm query)
                          "[ \t\n\r\\g\\G;]+")
            "\\G")))
      (sql-redirect sqlbuf sql outbuf))))


(when-sql-mysql-feature%

  (defun sql-mysql-list-code (sqlbuf outbuf enhanced target)
    "List code of mysql's TARGET."
  	(let ((sql (concat
  							"SHOW CREATE"
  							(format " %s\\G" (if enhanced target target)))))
  		(sql-redirect sqlbuf sql outbuf))))


(when-sql-mysql-feature%

  (defun sql-mysql-list-index (sqlbuf outbuf enhanced table)
    "List index of mysql's TABLE."
    (let ((simple-sql
           (concat
            "SHOW INDEX"
            (format " FROM %s\\G" table)))
          (enhanced-sql nil))
      (sql-redirect sqlbuf
                    (if enhanced enhanced-sql simple-sql)
                    outbuf))))


;; end of mysql

;;;
;; oceanbase: oracle-mode
;;;

(defvar sql-oceanbase-program "obclient"
  "Command to start obclient by Oceanbase.")

(defvar sql-oceanbase-options '("--prompt=obclient> ")
  "List of additional options for \\=`sql-oceanbase-program\\='.")

(defvar sql-oceanbase-login-params '(user password server)
  "List of login parameters needed to connect to Oceanbase.")

(defun sql-comint-oceanbase (product options &optional buf-name)
  "Create comint buffer and connect to Oceanbase."
  ;; Put all parameters to the program (if defined) in a list and call
  ;; make-comint.
  (let ((params
         (append
          options
          (if (not (string= "" sql-user))
              (list (concat "--user=" sql-user)))
          (if (not (string= "" sql-password))
              (list (concat "--password=" sql-password)))
          (if (not (= 0 sql-port))
              (list (concat "--port=" (number-to-string sql-port))))
          (if (not (string= "" sql-server))
              (list (concat "--host=" sql-server))))))
    (sql-comint product params buf-name)))

(when-sql-oceanbase-feature%

  (defun sql-oceanbase-desc-table (sqlbuf outbuf enhanced table)
    "Describe oceanbase table."
    (let ((simple-sql
           (format "DESCRIBE %s\\G" table))
          (enhanced-sql nil))
      (sql-redirect sqlbuf
                    (if enhanced enhanced-sql simple-sql)
                    outbuf))))

(when-sql-oceanbase-feature%

  (defun sql-oceanbase-desc-plan (sqlbuf outbuf enhanced query)
    "Describe execution plan of oceanbase's QUERY."
    (let ((sql
           (concat
            "EXPLAIN FORMAT=" (if enhanced "JSON " "TRADITIONAL ")
            (string-trim> (sql-mysql-norm query)
                          "[ \t\n\r\\g\\G;]+")
            "\\G")))
      (sql-redirect sqlbuf sql outbuf))))

(when-sql-oceanbase-feature%

  (defun sql-oceanbase-list-code (sqlbuf outbuf enhanced target)
    "List code of oceanbase's TARGET."
  	(let ((sql
           (format
  					"SELECT DBMS_METADATA.GET_DDL('TABLE','%s') FROM DUAL\\G"
            (upcase (if enhanced target target)))))
  		(sql-redirect sqlbuf sql outbuf))))

;; end of oceanbase

;;;
;; init!
;;;

(defun on-sql-mysql-init! ()
  "On \\=`sql\\=' mysql initialization."
  ;; mysql: \\=`:terminator\\='
  (when-fn-sql-send-magic-terminator%
    (plist-put
     (cdr (assq 'mysql sql-product-alist))
     :terminator
     '("^.*\\G" . ""))
    (ad-enable-advice #'sql-send-magic-terminator 'before
                      "sql-send-magic-terminator-before")
    (ad-activate #'sql-send-magic-terminator t))

  ;; `sql-mysql-program'
  (setq sql-mysql-program
        (or (executable-find% "mysql")
            (when-platform% 'darwin
              (executable-find%
               "/Applications/MySQLWorkbench.app/Contents/MacOS/mysql"))
            "mysql"))
  ;; mysql features
  (when-sql-mysql-feature%
    ;; new `:desc-table'
    (plist-put (cdr (assq 'mysql sql-product-alist))
               :desc-table
               #'sql-mysql-desc-table)
    ;; new `:desc-plan'
    (plist-put (cdr (assq 'mysql sql-product-alist))
               :desc-plan
               #'sql-mysql-desc-plan)
    ;; new `:list-code'
    (plist-put (cdr (assq 'mysql sql-product-alist))
               :list-code
               #'sql-mysql-list-code)
    ;; new `:list-index'
    (plist-put (cdr (assq 'mysql sql-product-alist))
               :list-index
               #'sql-mysql-list-index)))

(defun on-sql-oracle-init! ()
  "On \\=`sql\\=' oracle initialization."
  ;; `sql-oracle-program'
  (setq sql-oracle-program
        (or (executable-find% "sqlplus")
            (when% (getenv "ORACLE_HOME")
              (or (executable-find%
                   (concat (getenv "ORACLE_HOME") "/"
                           "sqlplus"))
                  (executable-find%
                   (concat (getenv "ORACLE_HOME") "/bin/"
                           "sqlplus"))))
            "sqlplus"))
  ;; oracle features
  (when-sql-oracle-feature%
    ;; replace `:list-all'
    (when (plist-get
           (cdr (assq 'oracle sql-product-alist))
           :list-all)
      (plist-put
       (cdr (assq 'oracle sql-product-alist))
       :list-all
       #'sql-oracle-list-all*))
    ;; new `:list-code'
    (plist-put
     (cdr (assq 'oracle sql-product-alist))
     :list-code
     #'sql-oracle-list-code)
    ;; new `:desc-plan'
    (plist-put (cdr (assq 'oracle sql-product-alist))
               :desc-plan
               #'sql-oracle-desc-plan)))

(defun on-sql-oceanbase-init! ()
  "On \\=`sql\\=' oceanbase initialization."
  (when (assq 'oceanbase sql-product-alist)
    (assq-delete-all 'oceanbase sql-product-alist))
  (append! '(oceanbase
             :name "Oceanbase"
             :font-lock sql-mode-oracle-font-lock-keywords
             :sqli-program sql-oceanbase-program
             :sqli-options sql-oceanbase-options
             :sqli-login sql-oceanbase-login-params
             :sqli-comint-func sql-comint-oceanbase
             :prompt-regexp "^obclient> "
             :prompt-length 9
             :prompt-cont-regexp "^    -> "
             :syntax-alist ((?# . "< b") (?\\ . "\\"))
             :input-filter sql-remove-tabs-filter)
           sql-product-alist)
  (when-sql-oceanbase-feature%
   ;; :desc-table
   (plist-put (cdr (assq 'oceanbase sql-product-alist))
              :desc-table
              #'sql-oceanbase-desc-table)
   ;; :desc-plan
   (plist-put (cdr (assq 'oceanbase sql-product-alist))
              :desc-plan
              #'sql-oceanbase-desc-plan)
   ;; :list-code
   (plist-put (cdr (assq 'oceanbase sql-product-alist))
              :list-code
              #'sql-oceanbase-list-code)))

(defun on-sql-init! ()
  "On \\=`sql\\=' initialization."
  (on-sql-mysql-init!)
  (on-sql-oracle-init!)
  (on-sql-oceanbase-init!)
  (when-fn-sql-show-sqli-buffer%
    (define-key% sql-mode-map (kbd "C-c C-z")
                 #'sql-show-sqli-buffer*))
  ;; features' keybindings
  (when-sql-feature%
    (define-key% sql-mode-map (kbd "C-c C-l c") #'sql-list-code)
    (define-key% sql-mode-map (kbd "C-c C-l i") #'sql-list-index)
    (define-key% sql-mode-map (kbd "C-c C-l T") #'sql-desc-table)
    (define-key% sql-mode-map (kbd "C-c C-l P") #'sql-desc-plan)))

;; end of init!

(provide 'sqls)


;; end of sqls.el
