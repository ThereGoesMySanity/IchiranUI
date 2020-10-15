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

(defun accept-one-stream (l)
  (let ((c (sb-bsd-sockets:socket-accept l)))
    (unwind-protect
	  (let ((stream (sb-bsd-sockets:socket-make-stream c :output t :input t)))
        (let* ((line (read-line stream))
               (res (ichiran/dict:simple-segment line)))
            (loop for word in res
                for obj = (ichiran/dict:word-info-gloss-json word)
                for text = (jsown:to-json obj)
                do (format stream " ~a~%" text)))
        (sb-bsd-sockets:socket-close c)))))

(defun runloop (l)
  (accept-one-stream l)
  (runloop l))

(let ((l (make-listen-socket)))
(unwind-protect
    (runloop l)
    (progn (format t "~&Closing listen socket~%")
        (sb-bsd-sockets:socket-close l))))