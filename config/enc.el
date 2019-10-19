;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; enc.el
;;;;


(defconst +encode-output-buffer-name+
  "*encode-output*"
  "Encode output buffer name")

(defconst +decode-output-buffer-name+
  "*decode-output*"
  "Decode output buffer name")


(eval-when-compile

  (defmacro _enc_with_output_buffer_ (buffer &rest body)
    (declare (indent 0))
    `(with-current-buffer (switch-to-buffer-other-window ,buffer)
       (delete-region (point-min) (point-max))
       ,@body)))


;; Encode/Decode URL

(defun encode-url* ()
  "Encode region into `+encode-output-buffer-name+' buffer."
  (interactive)
  (let ((s (string-trim>< (region-extract-str t))))
    (_enc_with_output_buffer_ +encode-output-buffer-name+
                              (insert (url-hexify-string s)))))

(defun decode-url* ()
  "Decode region into `+decode-output-buffer-name+' buffer."
  (interactive)
  (let ((s (string-trim>< (region-extract-str t))))
    (_enc_with_output_buffer_ +decode-output-buffer-name+
                              (insert (decode-coding-string
                                       (url-unhex-string s)
                                       'utf-8)))))




;; Encode/Decode base64


(defun encode-base64* ()
  "Encode region with base64 into `+encode-output-buffer-name+' buffer."
  (interactive)
  (let ((s (string-trim>< (region-extract-str t))))
    (_enc_with_output_buffer_ +encode-output-buffer-name+
                              (insert (base64-encode-string
                                       (if (multibyte-string-p s)
                                           (encode-coding-string s 'utf-8)
                                         s))))))

(defun decode-base64* ()
  "Decode region with base64 into `+encode-output-buffer-name+' buffer."
  (interactive)
  (let ((s (string-trim>< (region-extract-str t))))
    (_enc_with_output_buffer_ +decode-output-buffer-name+
                              (insert (decode-coding-string
                                       (base64-decode-string s) 'utf-8)))))


 ;; 

;; Encode/Decode IP address

(defmacro encode-ip (s &optional endian)
  "Encode IPv4 address to int."
  (let ((ss (gensym*)))
    `(let ((,ss (and (stringp ,s)
                     (split-string* ,s "\\." t))))
       (when (and (consp ,ss) (= 4 (length ,ss)))
         (if ,endian
             (logior
              (lsh (string-to-number (nth 0 ,ss)) 24)
              (lsh (string-to-number (nth 1 ,ss)) 16)
              (lsh (string-to-number (nth 2 ,ss)) 8)
              (logand (string-to-number (nth 3 ,ss)) #xff))
           (logior
            (logand (string-to-number (nth 0 ,ss)) #xff)
            (lsh (string-to-number (nth 1 ,ss)) 8)
            (lsh (string-to-number (nth 2 ,ss)) 16)
            (lsh (string-to-number (nth 3 ,ss)) 24)))))))


(defmacro decode-ip (n &optional endian)
  "Decode IPv4 address to string."
  `(when (integerp ,n)
     (if ,endian
         (format "%s.%s.%s.%s"
                 (lsh (logand ,n #xff000000) -24)
                 (lsh (logand ,n #x00ff0000) -16)
                 (lsh (logand ,n #x0000ff00) -8)
                 (logand ,n #xff))
       (format "%s.%s.%s.%s"
               (logand ,n #xff)
               (lsh (logand ,n #x0000ff00) -8)
               (lsh (logand ,n #x00ff0000) -16)
               (lsh (logand ,n #xff000000) -24)))))


(defun encode-ip* (&optional arg endian)
  "Encode IPv4 address in region to int.

If ARG is non nil then output to `+encode-output-buffer-name+'.
If ENDIAN is t then decode in small endian."
  (interactive (list (if current-prefix-arg t nil)
                     (read-string "endian: " (if (= 108 (byteorder))
                                                 "small"
                                               "big"))))
  (let* ((n (encode-ip (string-trim>< (region-extract-str t))
                       (string= "small" endian)))
         (out (and (integerp n)
                   (format "%d (#o%o, #x%x)" n n n))))
    (if arg
        (_enc_with_output_buffer_ +encode-output-buffer-name+
                                  (insert out))
      (message "%s" out))))


(defun decode-ip* (&optional arg endian)
  "Decode IPv4 address region to string.

If ARG is non nil then output to `+decode-output-buffer-name+'.
If ENDIAN is t then decode in small endian."
  (interactive (list (if current-prefix-arg t nil)
                     (read-string "endian: " (if (= 108 (byteorder))
                                                 "small"
                                               "big"))))
  (let* ((s (string-trim>< (region-extract-str t)))
         (out (decode-ip (cond ((string-match "^[#0][xX]" s)
                                (string-to-number (substring s 2) 16))
                               ((string-match "^[#0][oO]" s)
                                (string-to-number (substring s 2) 8))
                               (t (string-to-number s)))
                         (string= "small" endian))))
    (if arg
        (_enc_with_output_buffer_ +decode-output-buffer-name+
                                  (insert out))
      (message "%s" out))))


(defun roman->arabic (n acc)
  "Translate a roman number N into arabic number."
  (cond ((null n) acc)
        ((stringp n) (cond ((string= "M" n) 1000)
                           ((string= "D" n) 500)
                           ((string= "C" n) 100)
                           ((string= "L" n) 50)
                           ((string= "X" n) 10)
                           ((string= "V" n) 5)
                           ((string= "I" n) 1)))
        ((let ((u1 (roman->arabic (car n) 0) )
               (u2 (roman->arabic (cadr n) 0))))
         (< u1 u2)
         (roman->arabic (cddr n)
                        (+ acc (- (roman->arabic (cadr n) 0)
                                  (roman->arabic (car n) 0)))))
        (t (roman->arabic (cdr n)
                          (+ acc (roman->arabic (car n) 0))))))


(defun decode-roman-number ()
  "Decode roman number into decimal number."
  (interactive)
  (let ((n (split-string* (region-extract-str t) "" t)))
    (message "%s" (roman->arabic n 0))))


(defun chinese->arabic (n acc)
  "Translate a chinese number N into arabic number."
  (cond ((null n) acc)
        ((stringp n) (cond ((string= "兆" n) 1000000000000)
                           ((string= "亿" n) 100000000)
                           ((string= "万" n) 10000)
                           ((string= "仟" n) 1000)
                           ((string= "佰" n) 100)
                           ((string= "拾" n) 10)
                           ((string= "玖" n) 9)
                           ((string= "捌" n) 8)
                           ((string= "柒" n) 7)
                           ((string= "陆" n) 6)
                           ((string= "伍" n) 5)
                           ((string= "肆" n) 4)
                           ((string= "叁" n) 3)
                           ((string= "贰" n) 2)
                           ((string= "壹" n) 1)
                           ((string= "零" n) 0)))
        ((let ((u (chinese->arabic (car n) 0)))
           (and (<  u 10) (> u 0) (> (chinese->arabic (cadr n) 0) 0)))
         (chinese->arabic (cddr n)
                          (+ acc (* (chinese->arabic (car n) 0)
                                    (chinese->arabic (cadr n) 0)))))
        (t (chinese->arabic (cdr n)
                            (+ acc (chinese->arabic (car n) 0))))))


(defun decode-chinese-number ()
  "Decode chinese number to decimal number."
  (interactive)
  (let ((n (split-string* (region-extract-str t) "" t)))
    (message "%s" (chinese->arabic n 0))))




;; end of file
