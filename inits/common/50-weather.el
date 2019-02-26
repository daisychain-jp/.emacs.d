;; 気象庁配信の天気情報を加工して表示

(el-get-bundle request)
(require 'request)

(defvar weather-areas
  '(((url . "http://www.drk7.jp/weather/xml/14.xml")
     (pref . "神奈川県")
     (area . "西部"))
    ((url . "http://www.drk7.jp/weather/xml/22.xml")
     (pref . "静岡県")
     (area . "中部"))))

(defun weather-area-show (weather-area-alist weather-buffer)
  (lexical-let ((url (assoc-default 'url weather-area-alist))
                (pref (assoc-default 'pref weather-area-alist))
                (area (assoc-default 'area weather-area-alist))
                (buffer weather-buffer))
   (request
    url
    :parser
    (lambda ()
      (let ((xml-string (string-as-multibyte (string-as-unibyte (buffer-string)))))
        (with-temp-buffer
          (erase-buffer)
          (insert xml-string)
          (libxml-parse-xml-region (point-min) (point-max)))))
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (when data
         (with-current-buffer buffer
           (insert (format "%s %s\n" pref area))
           (mapcar
            (lambda (info-node)
              (insert
               (format "%s %s %s %s\n"
                       (dom-attr info-node 'date)
                       (apply (lambda (max-temp-node min-temp-node)
                                (format "%2d/%2d℃"
                                        (string-to-number (dom-text min-temp-node))
                                        (string-to-number (dom-text max-temp-node))))
                              (dom-by-tag (dom-child-by-tag info-node 'temperature) 'range))
                       (apply (lambda (node-1 node-2 node-3 node-4)
                                (format "%2d-%2d-%2d-%2d％"
                                        (string-to-number (dom-text node-1))
                                        (string-to-number (dom-text node-2))
                                        (string-to-number (dom-text node-3))
                                        (string-to-number (dom-text node-4))))
                              (dom-by-tag (dom-child-by-tag info-node 'rainfallchance) 'period))
                       (dom-text (dom-child-by-tag info-node 'weather)))))
            (dom-by-tag (dom-by-id data area) 'info))
           (insert "\n"))))))))

(defun weather ()
  (interactive)
  (let ((buffer (get-buffer-create "weather")))
    (mapcar (lambda (weather-area)
              (weather-area-show weather-area buffer))
            weather-areas)
    (switch-to-buffer buffer)))

(provide 'weather)
