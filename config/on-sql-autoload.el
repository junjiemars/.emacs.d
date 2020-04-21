;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-sql-autoload.el
;;;;



(when-fn% 'sql-oracle-list-all 'sql
  
  (defun sql-oracle-list-all* (sqlbuf outbuf enhanced _table-name)
    ;; Query from USER_OBJECTS or ALL_OBJECTS
    (let ((settings (sql-oracle-save-settings sqlbuf))
          (simple-sql
           (concat
            "SELECT INITCAP(x.object_type) AS SQL_EL_TYPE "
            ", " (sql-oracle--list-object-name "x.object_name") " AS SQL_EL_NAME "
            "FROM user_objects                    x "
            "WHERE x.object_type NOT LIKE '%% BODY' "
            "ORDER BY 2, 1;"))
          (enhanced-sql
           (concat
            "SELECT INITCAP(x.object_type) AS SQL_EL_TYPE "
            ", "  (sql-oracle--list-object-name "x.owner")
            " ||'.'|| "  (sql-oracle--list-object-name "x.object_name") " AS SQL_EL_NAME "
            "FROM all_objects x "
            "WHERE x.object_type NOT LIKE '%% BODY' "
            "AND x.owner <> 'SYS' "
            "ORDER BY 2, 1;")))

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


(with-eval-after-load 'sql
  (when-fn% 'sql-oracle-product-alist 'sql
    (when (plist-get (cdr (assoc** 'oracle sql-product-alist))
                     :list-all)
      (plist-put (cdr (assoc** 'oracle sql-product-alist))
                 :list-all
                 #'sql-oracle-list-all*))))



 ;; end of file
