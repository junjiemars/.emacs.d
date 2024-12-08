;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; trans.el
;;;;
;; Commentary: transform.
;;;;


(defconst +encode-output-buffer-name+
  "*encode-output*"
  "Encode output buffer name")

(defconst +decode-output-buffer-name+
  "*decode-output*"
  "Decode output buffer name")

(defvar *endian-history*
  '("small" "big")
  "Endian choosing history list.")

(defmacro trans-with-output-buffer (buffer &rest body)
  (declare (indent 1))
  `(with-current-buffer (switch-to-buffer-other-window ,buffer)
     (erase-buffer)
     ,@body))

(defun region-extract-str (&optional properties)
  "Extract string from region or current buffer with or without PROPERTIES."
  (if-region-active
      (if properties
          (buffer-substring (region-beginning) (region-end))
        (buffer-substring-no-properties (region-beginning) (region-end)))
    (with-current-buffer (current-buffer)
      (save-excursion
        (if properties
            (buffer-substring (point-min) (point-max))
          (buffer-substring-no-properties (point-min) (point-max)))))))

;; end of vars & fns


;;;
;; Trans URL
;;;

(defun encode-url (&optional buffered)
  "Encode region via URL encoded."
  (interactive "P")
  (let* ((s (region-extract-str))
         (d (url-hexify-string s)))
    (prog1 d
      (if buffered
          (trans-with-output-buffer +encode-output-buffer-name+ (insert d))
        (when-interactive%
          (kill-new d t)
          (message "%s" d))))))

(defun decode-url (&optional buffered)
  "Decode region via URL decoded."
  (interactive "P")
  (let* ((s (region-extract-str))
         (d (decode-coding-string (url-unhex-string s) 'utf-8)))
    (prog1 d
      (if buffered
          (trans-with-output-buffer +decode-output-buffer-name+ (insert d))
        (when-interactive%
          (kill-new d t)
          (message "%s" d))))))

;; end of URL

;;;
;; Encode/Decode base64
;;;

(defun encode-base64 (&optional buffered)
  "Encode region via base64 encoded."
  (interactive "P")
  (let* ((s (region-extract-str))
         (d (base64-encode-string
             (if (multibyte-string-p s)
                 (encode-coding-string s 'utf-8)
               s)
             t)))
    (prog1 d
      (if buffered
          (trans-with-output-buffer +encode-output-buffer-name+
                                    (insert d))
        (when-interactive%
          (kill-new d t)
          (message "%s" d))))))

(defun decode-base64 (&optional buffered)
  "Decode region base64 decoded."
  (interactive "P")
  (let* ((s (region-extract-str))
         (d (decode-coding-string (base64-decode-string s) 'utf-8)))
    (prog1 d
      (if buffered
          (trans-with-output-buffer +decode-output-buffer-name+
                                    (insert d))
        (when-interactive%
          (kill-new d t)
          (message "%s" d))))))

;; end of base64

;;;
;; Trans IPv4
;;;

(defun encode-ipv4 (&optional buffered)
  "Encode IPv4 address to int."
  (interactive "P")
  (let* ((s (split-string* (or (region-extract-str) "") "\\." t)))
    (unless (or (stringp s) (= (length s) 4))
      (user-error "%s" "Not a valid IPv4 string"))
    (let* ((d (logior
               (logand (string-to-number (nth 0 s)) #xff)
               (ash (string-to-number (nth 1 s)) 8)
               (ash (string-to-number (nth 2 s)) 16)
               (ash (string-to-number (nth 3 s)) 24)))
           (d1 (number-to-string d)))
      (prog1 d
        (if buffered
            (trans-with-output-buffer +encode-output-buffer-name+
                                      (insert d1))
          (when-interactive%
            (kill-new d1 t)
            (message "%d (#o%o, #x%x)" d d d)))))))

(defun decode-ipv4 (&optional buffered)
  "Decode IPv4 address to string."
  (interactive "P")
  (let* ((s (or (region-extract-str) ""))
         (n (cond ((string-match "^[#0][xX]" s)
                   (string-to-number (substring s 2) 16))
                  ((string-match "^[#0][oO]" s)
                   (string-to-number (substring s 2) 8))
                  (t (string-to-number s))))
         (d (format "%s.%s.%s.%s"
                    (logand n #xff)
                    (ash (logand n #x0000ff00) -8)
                    (ash (logand n #x00ff0000) -16)
                    (ash (logand n #xff000000) -24))))

    (prog1 d
      (if buffered
          (trans-with-output-buffer +decode-output-buffer-name+
                                    (insert d))
        (when-interactive%
          (kill-new d t)
          (message "%s" d))))))

;; end of Trans IPv4

;; Roman/Chinese number to Arabic number

(defun roman->arabic (n)
  "Translate a Roman number R into Arabic number."
  (when (and (stringp n) (< (length n) 16))
    (let ((aa (make-vector (1+ ?X) 0))
          (i 0) (s 0) (l (length n)))
      (aset aa ?I 1)
      (aset aa ?V 5)
      (aset aa ?X 10)
      (aset aa ?L 50)
      (aset aa ?C 100)
      (aset aa ?D 500)
      (aset aa ?M 1000)
      (while (< i l)
        (let* ((cur (aref n i))
               (nxt (if (< (+ i 1) l) (aref n (+ i 1)) cur))
               (cc (aref aa cur)))
          (setq s (if (< cc (aref aa nxt))
                      (- s cc)
                    (+ s cc))
                i (1+ i))))
      s)))

(defun decode-roman-number (&optional buffered)
  "Decode Roman number into decimal number."
  (interactive "P")
  (let ((s (region-extract-str)))
    (unless (or (stringp s) (< (length s) 16))
      (user-error "%s" "Not a valid Roman number"))
    (let* ((d (roman->arabic s))
           (d1 (number-to-string d)))
      (prog1 d
        (if buffered
            (trans-with-output-buffer +decode-output-buffer-name+
                                      (insert d1))
          (when-interactive%
            (kill-new d1 t)
            (message "%d (#o%o, #x%x)" d d d)))))))


(defun chinese->arabic (n acc)
  "Translate a Chinese number N into arabic number."
  (cond ((null n) acc)
        ((stringp n) (cond ((or (string= "亿" n)
                                (string= "億" n))
                            100000000)
                           ((or (string= "万" n)
                                (string= "萬" n))
                            10000)
                           ((string= "仟" n) 1000)
                           ((string= "佰" n) 100)
                           ((string= "拾" n) 10)
                           ((string= "玖" n) 9)
                           ((string= "捌" n) 8)
                           ((string= "柒" n) 7)
                           ((or (string= "陆" n)
                                (string= "陸" n))
                            6)
                           ((string= "伍" n) 5)
                           ((string= "肆" n) 4)
                           ((string= "叁" n) 3)
                           ((string= "贰" n) 2)
                           ((string= "壹" n) 1)
                           ((string= "零" n) 0)))
        ((let ((u (chinese->arabic (car n) 0))
               (u1 (chinese->arabic (cadr n) 0)))
           (and (< u 10) (> u 0)
                (< u1 10000) (>= u1 10)))
         (chinese->arabic (cddr n)
                          (+ acc (* (chinese->arabic (car n) 0)
                                    (chinese->arabic (cadr n) 0)))))
        ((let ((u (chinese->arabic (car n) 0)))
           (>= u 10000))
         (let ((u (chinese->arabic (car n) 0)))
           (chinese->arabic (cdr n)
                            (cond ((> acc 10000)
                                   (let ((b (* (/ acc (* u 1000))
                                               (* u 1000))))
                                     (+ b (* (- acc b) u))))
                                  (t (* acc u))))))
        (t (chinese->arabic (cdr n)
                            (+ acc (chinese->arabic (car n) 0))))))


(defun decode-chinese-number (&optional buffered)
  "Decode chinese number to decimal number."
  (interactive "P")
  (let* ((ss (split-string* (or (region-extract-str) "") "" t))
         (d (chinese->arabic ss 0))
         (d1 (number-to-string d)))
    (prog1 d
      (if buffered
          (trans-with-output-buffer +decode-output-buffer-name+
                                    (insert d1))
        (when-interactive%
          (kill-new d1 t)
          (message "%d (#o%o, #x%x)" d d d))))))


;; end of Roman/Chinese number

;;;
;; Alphabet
;;;

(defun ascii-table (&optional octal)
  "Display basic ASCII table \\=[0-128\\=)."
  (interactive "P")
  (switch-to-buffer "*ASCII*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (save-excursion
      (insert (propertize (format "ASCII characters [0-128) (%s).\n\n"
                                  (if octal "Oct Dec" "Hex Dec"))
                          'face 'font-lock-type-face))
      (insert (if octal
                  (concat "  Oct  Dec  Char|  Oct  Dec  Char|"
                          "  Oct  Dec  Char|  Oct  Dec  Char\n")
                (concat "  Hex  Dec  Char|  Hex  Dec  Char|"
                        "  Hex  Dec  Char|  Hex  Dec  Char\n")))
      (let ((i -1))
        (while (< i 31)
          (insert
           (format (if octal
                       (concat " %4o %4d %4s | %4o %4d %4s |"
                               " %4o %4d %4s | %4o %4d %4s\n")
                     (concat " %4x %4d %4s | %4x %4d %4s |"
                             " %4x %4d %4s | %4x %4d %4s\n"))
                   (setq i (+ 1  i)) i (single-key-description i)
                   (setq i (+ 32 i)) i (single-key-description i)
                   (setq i (+ 32 i)) i (single-key-description i)
                   (setq i (+ 32 i)) i (single-key-description i)))
          (setq i (- i 96))))))
  (view-mode t))


(defun greek-alphabet ()
  "Display Greek alphabet."
  (interactive)
  (switch-to-buffer "*GREEK*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (save-excursion
      (insert (propertize "Greek characters (Hex Dec).\n\n"
                          'face 'font-lock-type-face))
      (insert "  Hex  Dec  Char |  Hex  Dec  Char |  Name\n")
      (let ((tbl '(
                   913 "Alpha (al-fah)"
                   914 "Beta (bay-tah)"
                   915 "Gamma (gam-ah)"
                   916 "Delta (del-ta)"
                   917 "Epsilon (ep-si-lon)"
                   918 "Zeta (zay-tah)"
                   919 "Eta (ay-tah)"
                   920 "Theta (thay-tah)"
                   921 "Iota (eye-o-tah)"
                   922 "Kappa (cap-pah)"
                   923 "Lambda (lamb-dah)"
                   924 "Mu (mew)"
                   925 "Nu (new)"
                   926 "Xi (zie)"
                   927 "Omicron (om-e-cron)"
                   928 "Pi (pie)"
                   929 "Rho (roe)"
                   931 "Sigma (sig-ma)"
                   932 "Tau (taw)"
                   933 "Upsilon (up-si-lon)"
                   934 "Phi (fie)"
                   935 "Chi (kie)"
                   936 "Psi (sigh)"
                   937 "Omega (oh-may-gah)")))
        (dolist* (s (let ((xs nil))
                      (dolist* (x (range 913 (+ 913 24)) (nreverse xs))
                        (unless (= x 930)
                          (setq xs (cons x xs))))))
          (let ((c (+ 32 s)))
            (insert (format
                     " %4x %4d %4s  | %4x %4d %4s  |  %s\n"
                     s s (text-char-description s)
                     c c (text-char-description c)
                     (plist-get tbl s))))))))
  (view-mode t))

;; end of Alphabet

(provide 'trans)

;; end of trans.el
