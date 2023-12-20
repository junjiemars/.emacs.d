;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-trans-autoload.el
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


(eval-when-compile

  (defmacro _enc_with_output_buffer_ (buffer &rest body)
    (declare (indent 0))
    `(with-current-buffer (switch-to-buffer-other-window ,buffer)
       (delete-region (point-min) (point-max))
       ,@body))

  (defmacro _endian_ ()
    `(if (= 108 (byteorder))
         "small"
       "endian")))


(defmacro region-extract-str (&optional buffer properties)
  "Extract string from region or BUFFER with or without PROPERTIES.\n
If no region actived, then extract from buffer when BUFFER is t."
  `(if-region-active
       (if ,properties
         (buffer-substring (region-beginning) (region-end))
           (buffer-substring-no-properties (region-beginning) (region-end)))
     (when ,buffer
       (with-current-buffer (current-buffer)
         (save-excursion
           (if ,properties
               (buffer-substring (point-min) (point-max))
             (buffer-substring-no-properties (point-min) (point-max))))))))


;; Trans URL

(defun encode-url ()
  "Encode region via URL encoded."
  (interactive)
  (let ((s (string-trim>< (region-extract-str t))))
    (_enc_with_output_buffer_ +encode-output-buffer-name+
                              (insert (url-hexify-string s)))))

(defun decode-url ()
  "Decode region via URL decoded."
  (interactive)
  (let ((s (string-trim>< (region-extract-str t))))
    (_enc_with_output_buffer_ +decode-output-buffer-name+
                              (insert (decode-coding-string
                                       (url-unhex-string s)
                                       'utf-8)))))


;; end of URL


;; Encode/Decode base64

(defun encode-base64 ()
  "Encode region via base64 encoded."
  (interactive)
  (let ((s (string-trim>< (region-extract-str t))))
    (_enc_with_output_buffer_ +encode-output-buffer-name+
                              (insert (base64-encode-string
                                       (if (multibyte-string-p s)
                                           (encode-coding-string s 'utf-8)
                                         s)
                                       t)))))

(defun decode-base64 ()
  "Decode region base64 decoded."
  (interactive)
  (let ((s (string-trim>< (region-extract-str t))))
    (_enc_with_output_buffer_ +decode-output-buffer-name+
                              (insert (decode-coding-string
                                       (base64-decode-string s) 'utf-8)))))


;; end of base64


;; Trans IP address

(defmacro ipv4->int (s &optional small-endian)
  "Translate IPv4 address to int from string."
  (let ((ss (gensym*)))
    `(let ((,ss (and (stringp ,s)
                     (split-string* ,s "\\." t))))
       (when (and (consp ,ss) (= 4 (length ,ss)))
         (if ,small-endian
             (logior
              (ash (string-to-number (nth 0 ,ss)) 24)
              (ash (string-to-number (nth 1 ,ss)) 16)
              (ash (string-to-number (nth 2 ,ss)) 8)
              (logand (string-to-number (nth 3 ,ss)) #xff))
           (logior
            (logand (string-to-number (nth 0 ,ss)) #xff)
            (ash (string-to-number (nth 1 ,ss)) 8)
            (ash (string-to-number (nth 2 ,ss)) 16)
            (ash (string-to-number (nth 3 ,ss)) 24)))))))


(defmacro int->ipv4 (n &optional small-endian)
  "Translate IPv4 address to string from int."
  `(when (integerp ,n)
     (if ,small-endian
         (format "%s.%s.%s.%s"
                 (ash (logand ,n #xff000000) -24)
                 (ash (logand ,n #x00ff0000) -16)
                 (ash (logand ,n #x0000ff00) -8)
                 (logand ,n #xff))
       (format "%s.%s.%s.%s"
               (logand ,n #xff)
               (ash (logand ,n #x0000ff00) -8)
               (ash (logand ,n #x00ff0000) -16)
               (ash (logand ,n #xff000000) -24)))))


(defun encode-ipv4 (&optional buffer endian)
  "Encode IPv4 address to int according to ENDIAN.\n
If BUFFER is non nil then output to \\=`+encode-output-buffer-name+\\='."
  (interactive (list (if current-prefix-arg t nil)
                     (read-string "endian: " (_endian_) '*endian-history*)))
  (let* ((n (ipv4->int (string-trim>< (region-extract-str t))
                       (string= "small" endian)))
         (out (and (integerp n)
                   (format "%d (#o%o, #x%x)" n n n))))
    (if buffer
        (_enc_with_output_buffer_ +encode-output-buffer-name+
                                  (insert out))
      (message "%s" out))))


(defun decode-ipv4 (&optional buffer endian)
  "Decode IPv4 address to string according to ENDIAN.\n
If BUFFER is non nil then output to \\=`+decode-output-buffer-name+\\='."
  (interactive (list (if current-prefix-arg t nil)
                     (read-string "endian: " (_endian_) '*endian-history*)))
  (let* ((s (string-trim>< (region-extract-str t)))
         (out (int->ipv4 (cond ((string-match "^[#0][xX]" s)
                                (string-to-number (substring s 2) 16))
                               ((string-match "^[#0][oO]" s)
                                (string-to-number (substring s 2) 8))
                               (t (string-to-number s)))
                         (string= "small" endian))))
    (if buffer
        (_enc_with_output_buffer_ +decode-output-buffer-name+
                                  (insert out))
      (message "%s" out))))


;; Roman/Chinese number to Arabic number

(defun roman->arabic (ss)
  "Translate a roman number N into arabic number."
  (when (and (stringp ss) (< (length ss) 16))
    (let ((aa (make-vector (1+ ?X) 0))
          (i 0) (s 0) (n (length ss)))
      (aset aa ?I 1)
      (aset aa ?V 5)
      (aset aa ?X 10)
      (aset aa ?L 50)
      (aset aa ?C 100)
      (aset aa ?D 500)
      (aset aa ?M 1000)
      (while (< i n)
        (let* ((cur (aref ss i))
               (nxt (if (< (+ i 1) n) (aref ss (+ i 1)) cur))
               (cc (aref aa cur)))
          (setq s (if (< cc (aref aa nxt))
                      (- s cc)
                    (+ s cc))
                i (1+ i))))
      s)))


(defun decode-roman-number (&optional arg)
  "Decode roman number into decimal number."
  (interactive "P")
  (let* ((ss (region-extract-str))
         (n (roman->arabic ss))
         (out (or (and n (format "%d (#o%o, #x%x)" n n n))
                  "unknown")))
    (if arg
        (_enc_with_output_buffer_ +decode-output-buffer-name+
                                  (insert out))
      (message "%s" out))))


(defun chinese->arabic (n acc)
  "Translate a chinese number N into arabic number."
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


(defun decode-chinese-number (&optional arg)
  "Decode chinese number to decimal number."
  (interactive "P")
  (let* ((ss (split-string* (region-extract-str t) "" t))
         (n (chinese->arabic ss 0))
         (out (format "%d (#o%o, #x%x)" n n n)))
    (if arg
        (_enc_with_output_buffer_ +decode-output-buffer-name+
                                  (insert out))
      (message "%s" out))))


;; end of Roman/Chinese number


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
        (mapc (lambda (s)
                (let ((c (+ 32 s)))
                  (insert (format
                           " %4x %4d %4s  | %4x %4d %4s  |  %s\n"
                           s s (text-char-description s)
                           c c (text-char-description c)
                           (plist-get tbl s)))))
              (remove-if* (lambda (x) (= x 930))
                          (range 913 (+ 913 24)))))))
  (view-mode t))


;; end of on-trans-autoload.el
