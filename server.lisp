(require 'sb-bsd-sockets)
(load "ichiran/ichiran.asd")
(ql:quickload "ichiran")
;; (ichiran/conn:with-db nil (ichiran/mnt:add-errata))
(ichiran/dict:init-suffixes t)
(defparameter *localhost-address* '(127 0 0 1))
(defvar *sem* (bt:make-semaphore :count 48))

(defun send-seq-query (seqs)
  (if (= (length seqs) 0) 
    nil
    (ichiran/conn:with-db nil (postmodern:query (format nil "with recursive reconj(src, seq) as (
	values ~{(~a, ~a)~^,~}
  union
	select r.seq, c.from from conjugation c join reconj r on r.seq=c.seq
	join entry e on e.seq=r.seq where not e.root_p
)
select * from reconj r 
join kana_text k on k.seq=r.seq
join entry e on e.seq=r.seq
where e.root_p;" seqs)))))

(defun get-conjs (wis)
  (let ((result (mapcan #'(lambda (x) (if (numberp x) (list x) x)) 
      (mapcar #'ichiran/dict:word-info-seq wis))))
    (remove-duplicates (alexandria:flatten result))))

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
    (bt:wait-on-semaphore *sem*)
    (bt:make-thread (lambda ()
      (unwind-protect
        (let ((stream (sb-bsd-sockets:socket-make-stream c :output t :input t)))
          (let* ((line (read-line stream nil ""))
                  (json (jsown:parse line)))
            (jsown:do-json-keys (key value) json
              (let ((result 
                  (cond
                    ((string= "segment-root" key)
                      (mapcar
                        #'(lambda (sentence) (jsown:new-js ("result"
                          (mapcar 
                            #'(lambda (list) (jsown:new-js 
                              ("seq" (nth 0 list))
                              ("kana" (nth 1 list))
                              ("kanji" (nth 2 list))))
                            (send-seq-query (get-conjs (ichiran/dict:simple-segment sentence)))))))
                        value))
                    ((string= "segment-gloss" key)
                      (mapcar
                        #'(lambda (sentence) (jsown:new-js ("result" (mapcar #'ichiran/dict:word-info-gloss-json (ichiran/dict:simple-segment sentence))))) 
                        value))
                    ((string= "romanize" key) (mapcar #'romanize-json value)))))
                (loop for res in result do (format stream "~a~%" (jsown:to-json res))))))
          (bt:signal-semaphore *sem*)
          (finish-output stream)
          (sb-bsd-sockets:socket-close c)))))))

(defun runloop (l)
  (accept-one-stream l)
  (runloop l))

(let ((l (make-listen-socket)))
(unwind-protect
    (runloop l)
    (progn (format t "~&Closing listen socket~%")
        (sb-bsd-sockets:socket-close l))))