;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; enc.el
;;;;


;; Encode/Decode url, base64

(eval-when-compile

  (defmacro _encode/decode-url* (encode)
    "Encode/Decode region into *encode/decode-url-output* buffer."
    (let ((s (gensym*)))
      `(let ((,s (string-trim>< (region-active-if
                                    (buffer-substring (region-beginning)
                                                      (region-end))))))
         (with-current-buffer
             (switch-to-buffer-other-window
              (if ,encode
                  "*encode-url-output*"
                "*decode-url-output*"))
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
         (with-current-buffer
             (switch-to-buffer-other-window
              (if ,encode
                  "*encode-base64-output*"
                "*decode-base64-output*"))
           (delete-region (point-min) (point-max))
           (insert (if ,encode
                       (base64-encode-string
                        (if (multibyte-string-p ,s)
                            (encode-coding-string ,s 'utf-8)
                          ,s))
                     (decode-coding-string
                      (base64-decode-string ,s) 'utf-8))))))))

(defun encode-url* ()
  "Encode region into *encode-url-output* buffer."
  (interactive)
  (_encode/decode-url* t))

(defun decode-url* ()
  "Decode region into *encode-url-output* buffer."
  (interactive)
  (_encode/decode-url* nil))

(defun encode-base64* ()
  "Encode region with base64 into *encode-base64-output* buffer."
  (interactive)
  (_encode-base64* t))

(defun decode-base64* ()
  "Decode region with base64 into *decode-base64-output* buffer."
  (interactive)
  (_encode-base64* nil))


 ;; 

;; Encode/Decode IP address

(defmacro encode-ip (s)
  "Encode IP address to int."
  (let ((ss (gensym*)))
    `(let ((,ss (and (stringp ,s)
                     (split-string* ,s "\\." t))))
       (when (and (consp ,ss) (= 4 (length ,ss)))
         (logior
          (lsh (string-to-number (nth 0 ,ss)) 24)
          (lsh (string-to-number (nth 1 ,ss)) 16)
          (lsh (string-to-number (nth 2 ,ss)) 8)
          (logand (string-to-number (nth 3 ,ss)) #xff))))))

(defun encode-ip* ()
  "Encode IP address region to int."
  (interactive)
  (let ((s (string-trim>< (region-active-if
                              (buffer-substring (region-beginning)
                                                (region-end))))))
    (let ((n (encode-ip s)))
      (message "%s" (format "%i (#o%o, #x%x)" n n n)))))


(defun decode-ip (n)
  "Decode IP address to string."
  (concat (number-to-string (lsh (logand n #xff000000) -24)) "."
          (number-to-string (lsh (logand n #x00ff0000) -16)) "."
          (number-to-string (lsh (logand n #x0000ff00) -8)) "."
          (number-to-string (logand n #xff))))




;; end of file
