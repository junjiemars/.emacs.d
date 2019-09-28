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


;; Encode/Decode url, base64

(eval-when-compile

  (defmacro _encode/decode-url* (encode)
    "Encode/Decode region into *encode/decode-url-output* buffer."
    (let ((s (gensym*)))
      `(let ((,s (string-trim>< (region-active-if
                                    (buffer-substring (region-beginning)
                                                      (region-end))))))
         (with-current-buffer (switch-to-buffer-other-window
                               (if ,encode
                                   +encode-output-buffer-name+
                                 +decode-output-buffer-name+))
           (delete-region (point-min) (point-max))
           (insert (if ,encode
                       (url-hexify-string ,s)
                     (decode-coding-string (url-unhex-string ,s)
                                           'utf-8)))))))

  (defmacro _encode-base64* (encode)
    "Encode region with base64 into *encode-base64-output* buffer."
    (let ((s (gensym*)))
      `(let ((,s (string-trim>< (region-active-if
                                    (buffer-substring (region-beginning)
                                                      (region-end))))))
         (with-current-buffer (switch-to-buffer-other-window
                               (if ,encode
                                   +encode-output-buffer-name+
                                 +decode-output-buffer-name+))
           (delete-region (point-min) (point-max))
           (insert (if ,encode
                       (base64-encode-string
                        (if (multibyte-string-p ,s)
                            (encode-coding-string ,s 'utf-8)
                          ,s))
                     (decode-coding-string
                      (base64-decode-string ,s) 'utf-8))))))))

(defun encode-url* ()
  "Encode region into `+encode-output-buffer-name+' buffer."
  (interactive)
  (_encode/decode-url* t))

(defun decode-url* ()
  "Decode region into `+decode-output-buffer-name+' buffer."
  (interactive)
  (_encode/decode-url* nil))

(defun encode-base64* ()
  "Encode region with base64 into `+encode-output-buffer-name+' buffer."
  (interactive)
  (_encode-base64* t))

(defun decode-base64* ()
  "Decode region with base64 into `+encode-output-buffer-name+' buffer."
  (interactive)
  (_encode-base64* nil))


 ;; 

;; Encode/Decode IP address

(defmacro encode-ip (s)
  "Encode IPv4 address to int."
  (let ((ss (gensym*)))
    `(let ((,ss (and (stringp ,s)
                     (split-string* ,s "\\." t))))
       (when (and (consp ,ss) (= 4 (length ,ss)))
         (logior
          (lsh (string-to-number (nth 0 ,ss)) 24)
          (lsh (string-to-number (nth 1 ,ss)) 16)
          (lsh (string-to-number (nth 2 ,ss)) 8)
          (logand (string-to-number (nth 3 ,ss)) #xff))))))

(defun encode-ip* (&optional arg)
  "Encode IPv4 address in region to int.

If ARG is non nil then output to `+encode-output-buffer-name+'."
  (interactive "P")
  (let* ((s (string-trim>< (region-active-if
                               (buffer-substring (region-beginning)
                                                 (region-end)))))
         (n (encode-ip s))
         (out (and (integerp n)
                   (format "%d (#o%o, #x%x)" n n n))))
    (message "%s" out)
    (when arg
      (with-current-buffer (switch-to-buffer-other-window
                            +encode-output-buffer-name+)
        (delete-region (point-min) (point-max))
        (insert out)))))


(defmacro decode-ip (n)
  "Decode IPv4 address to string."
  `(when (integerp ,n)
     (format "%s.%s.%s.%s"
             (lsh (logand ,n #xff000000) -24)
             (lsh (logand ,n #x00ff0000) -16)
             (lsh (logand ,n #x0000ff00) -8)
             (logand ,n #xff))))

(defun decode-ip* (&optional arg)
  "Decode IPv4 address region to string.

If ARG is non nil then output to `+decode-output-buffer-name+'."
  (interactive "P")
  (let* ((s (string-trim>< (region-active-if
                               (buffer-substring (region-beginning)
                                                 (region-end)))))
         (out (decode-ip (cond ((string-match "^[#0][xX]" s)
                                (string-to-number (substring s 2) 16))
                               ((string-match "^[#0][oO]" s)
                                (string-to-number (substring s 2) 8))
                               (t (string-to-number s))))))
    (message "%s" out)
    (when arg
      (with-current-buffer (switch-to-buffer-other-window
                            +decode-output-buffer-name+)
        (delete-region (point-min) (point-max))
        (insert out)))))





;; end of file
