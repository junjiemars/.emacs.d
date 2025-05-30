;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; sqls.el
;;;;

;;; require


;; end of require

;;; when-*macro

(eval-when-compile
  (defmacro when-sql-feature% (&rest body)
    (declare (indent 0))
    (if-fn% sql-execute-feature sql
            `(progn% ,@body)
      `(comment ,@body))))

(eval-when-compile
  (defmacro when-sql-oracle-feature% (&rest body)
    (declare (indent 0))
    (when-sql-feature%
      `(progn% ,@body))))

(eval-when-compile
  (defmacro when-sql-mysql-feature% (&rest body)
    (declare (indent 0))
    (when-sql-feature%
      `(progn% ,@body))))

(eval-when-compile
  (defmacro when-sql-oceanbase-feature% (&rest body)
    (declare (indent 0))
    (when-sql-feature%
      `(progn% ,@body))))

(eval-when-compile
  (defmacro when-sql-show-sqli-buffer% (&rest body)
    (declare (indent 0))
    (if-fn% sql-show-sqli-buffer sql
            `(progn% ,@body)
      `(comment ,@body))))

(eval-when-compile
  (defmacro when-sql-send-magic-terminator% (&rest body)
    (declare (indent 0))
    (if-fn% sql-send-magic-terminator sql
            `(progn% ,@body)
      `(comment ,@body))))

;; end of when-* macro

;;; sqli

(when-sql-show-sqli-buffer%
 (defun sql-show-sqli-buffer* ()
   "Display the current SQLi buffer."
   (interactive)
   (with-current-buffer (current-buffer)
     (when (or current-prefix-arg
               (null (get-buffer-process sql-buffer)))
       (call-interactively #'sql-connect))
     (call-interactively #'sql-show-sqli-buffer))))

(when-sql-show-sqli-buffer%
 (defun sql-find-sqli-buffer* ()
   "Return the living \\=`sql-buffer\\='."
   (with-current-buffer (current-buffer)
     (when (and sql-buffer (get-buffer-process sql-buffer))
       sql-buffer))))

(when-sql-send-magic-terminator%
 (defun sql-send-magic-terminator* (buf str terminator)
   "Send TERMINATOR to buffer BUF if its not present in STR."
   (funcall (symbol-function'_sql-send-magic-terminator_)
            buf str (cond ((eq 'mysql sql-product) (setq terminator t))
                          ((eq 'oceanbase sql-product) (setq terminator t))
                          (t terminator)))))

;; end of sqli

;;; features

(when-sql-feature%
  (defun sql-first-word (sql)
    "Return the first word in SQL."
    (let* ((i 0) (j nil) (c (aref sql i)))
      (while (null (or (and (>= c ?A) (<= c ?Z))
                      (and (>= c ?a) (<= c ?z))))
        (setq i (1+ i) c (aref sql i)))
      (setq j i)
      (while (or (and (>= c ?A) (<= c ?Z))
                 (and (>= c ?a) (<= c ?z)))
        (setq j (1+ j) c (aref sql j)))
      (substring sql i j))))

(when-sql-feature%
  (defun sql-list-all (&optional enhanced)
    "List all database objects.\n
With optional prefix argument ENHANCED, displays additional
details or extends the listing to include other schemas objects."
    (interactive "P")
    (let ((sqlbuf (sql-find-sqli-buffer*)))
      (unless sqlbuf
        (user-error "No SQL interactive buffer found"))
      (sql-execute-feature
       sqlbuf
       (format "*List All-%s*" sql-product)
       :list-all enhanced nil)
      (with-current-buffer sqlbuf
        ;; Contains the name of database objects
        (setq-local sql-contains-names t)))))

(when-sql-feature%
  (defun sql-list-code (name &optional enhanced)
    "List the code of the database object with qualified NAME."
    (interactive (list (sql-read-table-name "Object name: ")
                       (if current-prefix-arg
                           (read-string "Object type: " "table")
                         "table")))
    (let ((sqlbuf (sql-find-sqli-buffer*)))
      (unless sqlbuf
        (user-error "%s" "No SQL interactive buffer found"))
      (unless name
        (user-error "%s" "No name specified"))
      (sql-execute-feature
       sqlbuf
       (format "*List code %s*" name)
       :list-code enhanced name))))

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
    (let ((sqlbuf (sql-find-sqli-buffer*)))
      (unless sqlbuf
        (user-error "%s" "No SQL interactive buffer found"))
      (unless plan
        (user-error "%s" "No plan specified"))
      (sql-execute-feature
       sqlbuf
       (format "*Desc plan %s*" (sql-first-word plan))
       :desc-plan enhanced plan))))

(when-sql-feature%
  (defun sql-desc-table (name &optional enhanced)
    "Describe the database table."
    (interactive (list (upcase (sql-read-table-name "Table name: "))
                       current-prefix-arg))
    (let ((sqlbuf (sql-find-sqli-buffer*)))
      (unless sqlbuf
        (user-error "%s" "No SQL interactive buffer found"))
      (unless name
        (user-error "%s" "No name specified"))
      (sql-execute-feature
       sqlbuf
       (format "*Desc table %s*" name)
       :desc-table enhanced name))))

(when-sql-feature%
  (defun sql-export-query (query &optional enhanced)
    "Export the QUERY result."
    (interactive
     (list (if-region-active
               (buffer-substring-no-properties
                (region-beginning) (region-end))
             (buffer-substring-no-properties
              (save-excursion (backward-paragraph) (point))
              (save-excursion (forward-paragraph) (point))))
           current-prefix-arg))
    (let ((sqlbuf (sql-find-sqli-buffer*)))
      (unless sqlbuf
        (user-error "%s" "No SQL interactive buffer found"))
      (unless query
        (user-error "%s" "No query specified"))
      (sql-execute-feature
       sqlbuf
       (format "*Export query %s*" (sql-first-word query))
       :export-query enhanced query))))

;; end of features

;;;
;; oracle
;;;

(unless-fn% sql-oracle--list-object-name sql
  (defun sql-oracle--list-object-name (obj-name)
    (format (concat
             "CASE WHEN REGEXP_LIKE (%s, q'/^[A-Z0-9_#$]+$/','c')"
             " THEN %s ELSE '\"'|| %s ||'\"' END ")
            obj-name obj-name obj-name)))

(unless-fn% sql-oracle-restore-settings sql
  (defun sql-oracle-restore-settings (sqlbuf saved-settings)
    (mapc
     (lambda (one-cur-setting)
       (setq saved-settings (delete one-cur-setting saved-settings)))
     (sql-oracle-save-settings sqlbuf))
    (sql-redirect sqlbuf saved-settings)))

(when-sql-oracle-feature%
  (defun sql-oracle-list-all* (sqlbuf outbuf enhanced _table-name)
    (let ((settings (sql-oracle-save-settings sqlbuf))
          (simple-sql
           (concat
            "SELECT LOWER(X.object_type) AS SQL_TYPE,"
            (sql-oracle--list-object-name "X.object_name")
            " AS SQL_NAME "
            "FROM user_objects X "
            "ORDER BY SQL_TYPE, SQL_NAME;"))
          (enhanced-sql
           (concat
            "SELECT LOWER(X.object_type) AS SQL_TYPE,"
            (sql-oracle--list-object-name "X.owner")
            " ||'.'|| "
            (sql-oracle--list-object-name "X.object_name")
            " AS SQL_NAME "
            "FROM all_objects X "
            "WHERE X.owner <> 'SYS' "
            "ORDER BY SQL_TYPE, SQL_NAME;")))
      ;; `stty size' will output rows and columns separated by a space
      ;; but the columns be determine also by the frame size.
      (unwind-protect
          (progn
            (sql-redirect
             sqlbuf
             (concat
              (format "SET LINESIZE %d PAGESIZE 50000 TRIMOUT ON"
                      (window-width))
              " TAB OFF TIMING OFF FEEDBACK OFF"))
            (sql-redirect
             sqlbuf
             (list "COLUMN SQL_TYPE  HEADING \"SQL_TYPE\" FORMAT A19"
                   "COLUMN SQL_NAME  HEADING \"SQL_NAME\""
                   "COLUMN SQL_NAME  FORMAT A60"))
            (sql-redirect
             sqlbuf (if enhanced enhanced-sql simple-sql) outbuf)
            (sql-redirect
             sqlbuf '("COLUMN SQL_NAME CLEAR"
                      "COLUMN SQL_TYPE CLEAR")))
        (sql-oracle-restore-settings sqlbuf settings)))))

(when-sql-oracle-feature%
  (defun sql-oracle-list-code (sqlbuf outbuf enhanced target)
    (let ((settings (sql-oracle-save-settings sqlbuf))
          (sql-ddl
           (concat
            (format
             "SELECT DBMS_METADATA.GET_DDL(UPPER('%s'),UPPER('%s'))"
             enhanced target)
            " FROM DUAL;")))
      (unwind-protect
          (progn
            (sql-redirect
             sqlbuf
             (concat
              (format "SET LINESIZE %d" (window-width))
              " LONG 4096000"
              " LONGCHUNKSIZE 4096000"
              " PAGESIZE 0"
              " FEEDBACK OFF"
              " TAB OFF"
              " TERMOUT OFF"
              " TIMING OFF"
              " TRIMOUT ON"
              " VERIFY OFF;"))
            (sql-redirect sqlbuf sql-ddl outbuf))
        (sql-oracle-restore-settings sqlbuf settings)
        (sql-send-string "\n")))))

(when-sql-oracle-feature%
  (defun sql-oracle-desc-plan (sqlbuf outbuf enhanced target)
    (let ((settings (sql-oracle-save-settings sqlbuf))
  			  (sql (if enhanced
                   target ;; ignore enhanced
  						   target)))
      (unwind-protect
          (progn
            (sql-redirect
             sqlbuf
             (concat "SET AUTOTRACE ON EXPLAIN"))
            (sql-redirect
             sqlbuf
             (concat
              (format "SET LINESIZE %d PAGESIZE 1000" (window-width))
              " VERIFY OFF FEEDBACK OFF"
              " TRIMOUT ON TAB OFF TIMING OFF"))
            (sql-redirect
             sqlbuf
             (format "EXPLAIN PLAN FOR %s;" sql)
             outbuf)
            (sql-redirect
             sqlbuf
             "SELECT * FROM TABLE(DBMS_XPLAN.DISPLAY(format=>'ALL'));"
             outbuf))
        (sql-oracle-restore-settings sqlbuf settings)))))

(when-sql-oracle-feature%
  (defun sql-oracle-desc-table (sqlbuf outbuf enhanced table-name)
    (let ((settings (sql-oracle-save-settings sqlbuf))
          (sql
           (concat
            "SELECT DISTINCT T.column_name,C.comments"
            " FROM all_tab_cols T "
            " INNER JOIN all_col_comments C"
            "   ON    T.table_name=C.table_name"
            "     AND T.column_name=C.column_name"
            " WHERE T.table_name=" (format "'%s'" table-name)
            (when enhanced
              " ORDER BY T.column_name ASC")
            ";")))
      (unwind-protect
          (progn
            (sql-redirect
             sqlbuf
             "SET LINESIZE 200 PAGESIZE 100")
            (sql-redirect
             sqlbuf
             "COLUMN column_name FORMAT A40;")
            (sql-redirect
             sqlbuf
             "COLUMN comments FORMAT A80;")
            (sql-redirect sqlbuf sql outbuf))
        (sql-oracle-restore-settings sqlbuf settings)))))

(when-sql-oracle-feature%
  (defun sql-oracle-export-query (sqlbuf outbuf enhanced target)
    (let ((settings (sql-oracle-save-settings sqlbuf))
  			  (sql (if enhanced
                   target ;; ignore enhanced
  						   target)))
      (unwind-protect
          (progn
            (sql-redirect sqlbuf "SET MARKUP CSV ON QUOTE ON")
            (sql-redirect sqlbuf sql outbuf))
        (sql-redirect sqlbuf "SET MARKUP CSV OFF QUOTE OFF")
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
      (while (re-search-forward "[ \t\n]+" nil t)
        (replace-match " " t t))
      (buffer-substring (point-min) (point-max)))))

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
    (let ((sql (format
  						  "SHOW CREATE %s %s\\G"
                enhanced target)))
  	  (sql-redirect sqlbuf sql outbuf))))

(when-sql-mysql-feature%
  (defun sql-mysql-desc-table (sqlbuf outbuf enhanced table)
    "Describe mysql table."
    (let ((simple-sql
           (concat
            "SHOW FULL COLUMNS "
            (format "FROM %s\\G" table)))
          (enhanced-sql nil))
      (sql-redirect
       sqlbuf (if enhanced enhanced-sql simple-sql) outbuf))))

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

(defun sql-comint-oceanbase (product options &optional _buf-name)
  "Create comint buffer and connect to Oceanbase."
  ;; Put all parameters to the program (if defined) in a list and call
  ;; make-comint.
  (let ((params
         (append
          options
          (if (null (string-equal "" sql-user))
              (list (concat "--user=" sql-user)))
          (if (null (string-equal "" sql-password))
              (list (concat "--password=" sql-password)))
          (if (null (= 0 sql-port))
              (list (concat "--port=" (number-to-string sql-port))))
          (if (null (string-equal "" sql-server))
              (list (concat "--host=" sql-server))))))
    (if-version% > 25
                 (sql-comint product params _buf-name)
      (sql-comint product params))))

(when-sql-oceanbase-feature%
  (defun sql-oceanbase-list-all (sqlbuf outbuf enhanced _table-name)
    (let ((simple-sql
           (concat
            "SELECT LOWER(X.object_type) AS SQL_TYPE,"
            (sql-oracle--list-object-name "X.object_name")
            " AS SQL_NAME "
            "FROM user_objects X "
            "ORDER BY SQL_TYPE, SQL_NAME;"))
          (enhanced-sql
           (concat
            "SELECT LOWER(x.object_type) AS SQL_TYPE,"
            (sql-oracle--list-object-name "X.owner")
            " ||'.'|| "
            (sql-oracle--list-object-name "X.object_name")
            " AS SQL_NAME "
            "FROM all_objects X "
            "WHERE AND X.owner <> 'SYS' "
            "ORDER BY SQL_TYPE, SQL_NAME;")))
      (sql-redirect
       sqlbuf (if enhanced enhanced-sql simple-sql) outbuf))))

(when-sql-oceanbase-feature%
  (defun sql-oceanbase-list-code (sqlbuf outbuf enhanced target)
    (let ((sql
           (concat
            (format
  				   "SELECT DBMS_METADATA.GET_DDL(UPPER('%s'),UPPER('%s'))"
             enhanced target)
            " FROM DUAL\\G")))
  	  (sql-redirect sqlbuf sql outbuf)
      (with-current-buffer outbuf
        (save-excursion
          (goto-char (point-min))
          (delete-line*)
          (while (re-search-forward
                  "\\(^DBMS_METADATA.*: \\|^[0-9]+ row.*\\)" nil t)
            (replace-match "")))))))

(when-sql-oceanbase-feature%
  (defun sql-oceanbase-desc-plan (sqlbuf outbuf enhanced query)
    (let ((sql
           (concat
            "EXPLAIN FORMAT=" (if enhanced "JSON " "TRADITIONAL ")
            (string-trim> (sql-mysql-norm query)
                          "[ \t\n\r\\g\\G;]+")
            "\\G")))
      (sql-redirect sqlbuf sql outbuf))))

(when-sql-oceanbase-feature%
  (defun sql-oceanbase-desc-table (sqlbuf outbuf enhanced table-name)
    (let ((simple-sql
           (concat
            "SELECT comments FROM all_tab_comments"
            (format " WHERE table_name='%s';" table-name)))
          (enhanced-sql
           (concat
            "SELECT T.column_name,C.comments FROM all_tab_cols T"
            " INNER JOIN all_col_comments C"
            " ON T.table_name=C.table_name"
            " AND T.column_name=C.column_name"
            (format " WHERE T.table_name='%s'" table-name)
            " ORDER BY T.column_name ASC;")))
      (sql-redirect
       sqlbuf (if enhanced enhanced-sql  simple-sql) outbuf))))

;; end of oceanbase

;;;
;; init!
;;;

(defun on-sql-mysql-init! ()
  "On \\=`sql\\=' mysql initialization."
  ;; mysql: \\=`:terminator\\='
  (when-sql-send-magic-terminator%
    (plist-put
     (cdr (assq 'mysql sql-product-alist))
     :terminator
     '("^.*\\G" . ""))
    (defadvice* '_sql-send-magic-terminator_
      'sql-send-magic-terminator #'sql-send-magic-terminator*))
  ;; `sql-mysql-program'
  (setq sql-mysql-program
        (or (executable-find% "mysql")
            (when-platform% darwin
              (executable-find%
               "/Applications/MySQLWorkbench.app/Contents/MacOS/mysql"))
            "mysql"))
  ;; mysql features
  (when-sql-mysql-feature%
    ;; new `:list-code'
    (plist-put (cdr (assq 'mysql sql-product-alist))
               :list-code
               #'sql-mysql-list-code)
    ;; new `:desc-plan'
    (plist-put (cdr (assq 'mysql sql-product-alist))
               :desc-plan
               #'sql-mysql-desc-plan)
    ;; new `:desc-table'
    (plist-put (cdr (assq 'mysql sql-product-alist))
               :desc-table
               #'sql-mysql-desc-table)))

(defun on-sql-oracle-init! ()
  "On \\=`sql\\=' oracle initialization."
  ;; `sql-oracle-program'
  (setq sql-oracle-program
        (or (let ((d (getenv "ORACLE_HOME")))
              (when (and d (file-exists-p d))
                (or (executable-find (concat d "/" "sqlplus"))
                    (executable-find (concat d "/bin/" "sqlplus")))))
            (executable-find% "sqlplus")
            (executable-find% "sqlplus.sh")
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
               #'sql-oracle-desc-plan)
    ;; new `:desc-table'
    (plist-put (cdr (assq 'oracle sql-product-alist))
               :desc-table
               #'sql-oracle-desc-table)
    ;; new `:export-query'
    (plist-put (cdr (assq 'oracle sql-product-alist))
               :export-query
               #'sql-oracle-export-query)))

(defun on-sql-oceanbase-init! ()
  "On \\=`sql\\=' oceanbase initialization."
  (when (assq 'oceanbase sql-product-alist)
    (setq sql-product-alist (assq-delete-all 'oceanbase sql-product-alist)))
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
  (when-sql-send-magic-terminator%
    (plist-put
     (cdr (assq 'oceanbase sql-product-alist))
     :terminator
     '("^.*\\G" . ""))
    (defadvice* '_sql-send-magic-terminator_
      'sql-send-magic-terminator #'sql-send-magic-terminator*))
  (when-sql-oceanbase-feature%
    ;; :list-all
    (plist-put (cdr (assq 'oceanbase sql-product-alist))
               :list-all
               #'sql-oceanbase-list-all)
    ;; :list-code
    (plist-put (cdr (assq 'oceanbase sql-product-alist))
               :list-code
               #'sql-oceanbase-list-code)
    ;; :desc-plan
    (plist-put (cdr (assq 'oceanbase sql-product-alist))
               :desc-plan
               #'sql-oceanbase-desc-plan)
    ;; :desc-table
    (plist-put (cdr (assq 'oceanbase sql-product-alist))
               :desc-table
               #'sql-oceanbase-desc-table)))

(defun on-sql-init! ()
  "On \\=`sql\\=' initialization."
  (on-sql-mysql-init!)
  (on-sql-oracle-init!)
  (on-sql-oceanbase-init!)
  (when-sql-show-sqli-buffer%
   (define-key% sql-mode-map "" #'sql-show-sqli-buffer*))
  ;; features' keybindings
  (when-sql-feature%
    (define-key% sql-mode-map "a" #'sql-list-all)
    (define-key% sql-mode-map "c" #'sql-list-code)
    (define-key% sql-mode-map "p" #'sql-desc-plan)
    (define-key% sql-mode-map "t" #'sql-desc-table)
    (define-key% sql-mode-map "x" #'sql-export-query)))

;; end of init!

(provide 'sqls)


;; end of sqls.el
