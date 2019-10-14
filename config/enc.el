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


(defun encode-chinese-number (n &optional arg)
  "Encode N to chinese number."
  (interactive "P")
  (ignore* n arg))

(defun decode-chinese-number (&optional arg)
  "Decode S to decimal number."
  (interactive "P")
  (let ((out 0.0)
        (n (mapcar** #'(lambda (x y)
                         (list x (alist-get* y
                                             '(("" . 0)
                                               ("零" . 0) ("壹" . 1)
                                               ("贰" . 2) ("叁" . 3)
                                               ("肆" . 4) ("伍" . 5)
                                               ("陆" . 6) ("柒" . 7)
                                               ("捌" . 8) ("玖" . 9))
                                             nil nil #'string=)))
                     '(0.01 0.1 1 10 100 1000 10000 100000000 1000000000000)
                     (nreverse (split-string* (string-trim><
                                               (region-extract-str t))
                                              "[分角元拾佰仟万亿兆]"
                                              t
                                              " ")))))
    (dolist* (x n out)
      (setq out (+ out (* (car x) (cadr x)))))
    (if arg
        (_enc_with_output_buffer_ +decode-output-buffer-name+
                                  (insert (number-to-string out)))
      (message "%s" out))))



;; end of file
