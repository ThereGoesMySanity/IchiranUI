(require 'sb-bsd-sockets)
(load "ichiran/ichiran.asd")
(ql:quickload "ichiran")
(ichiran/conn:with-db nil (ichiran/mnt:add-errata))
(ichiran/dict:init-suffixes t)
(defparameter *localhost-address* '(127 0 0 1))

(defun make-listen-socket ()
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
			       :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (sb-bsd-sockets:socket-bind socket *localhost-address* 13535)
    (sb-bsd-sockets:socket-listen socket 1)
    socket))

(defun romanize-json (data)
  (jsown:new-js
    ("result" (loop for sentence in (car (ichiran:romanize* data)) collect 
      (jsown:new-js
        ("words" (loop for word in (nth 0 sentence) collect
          (jsown:new-js
            ("romanized" (nth 0 word))
            ("data" (ichiran/dict:word-info-gloss-json (nth 1 word))))))
        ("rank" (nth 1 sentence)))))))

(defun accept-one-stream (l)
  (let ((c (sb-bsd-sockets:socket-accept l)))
    (unwind-protect
	  (let ((stream (sb-bsd-sockets:socket-make-stream c :output t :input t)))
      (let* ((line (read-line stream))
              (json (jsown:parse line)))
        (jsown:do-json-keys (key value) json
          (let ((result 
              (cond
                ((string= "segment-gloss" key)
                  (loop for sentence in value collect (jsown:new-js ("result" (loop for word in (ichiran/dict:simple-segment sentence)
                      collect (ichiran/dict:word-info-gloss-json word))))))
                ((string= "romanize" key) (loop for sentence in value collect (romanize-json sentence))))))
            (loop for res in result do (format stream "~a~%" (jsown:to-json res))))))
      (finish-output stream)
      (sb-bsd-sockets:socket-close c)))))

(defun runloop (l)
  (accept-one-stream l)
  (runloop l))

(let ((l (make-listen-socket)))
(unwind-protect
    (runloop l)
    (progn (format t "~&Closing listen socket~%")
        (sb-bsd-sockets:socket-close l))))