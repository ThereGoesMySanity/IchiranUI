(require 'sb-bsd-sockets)
(load "ichiran/ichiran.asd")
(ql:quickload "ichiran")
;; (ichiran/conn:with-db nil (ichiran/mnt:add-errata))
(ichiran/dict:init-suffixes t)
(defparameter *localhost-address* '(127 0 0 1))
(defvar *sem* (bt:make-semaphore :count 48))

(defun sqlify (str) (if str (format nil "\'~a\'" str) "NULL"))

(defun send-seq-query (words)
  (if (= (length words) 0) 
    nil
    (ichiran/conn:with-db nil (postmodern:query 
      (format nil "
select distinct on (v)
	case when e.root_p then v.seq else coalesce(kjt.seq, kt.seq, v.seq) end as seq,
	case when e.root_p then v.kana else coalesce(kjt.best_kana, kt.text) end as kana,
	case when e.root_p then v.kanji else kjt.text end as kanji
from (values ~{~a~^,~})
		as v(seq, kana, kanji)
left join conjugation c on v.seq=c.seq
join entry e on e.seq=v.seq
left join entry ev on ev.seq=c.via
left join entry ef on ef.seq=c.from
left join conj_source_reading csr on c.id=csr.conj_id and csr.text=coalesce(v.kanji, v.kana)
left join kanji_text kjt on kjt.seq=case when ev.root_p then ev.seq when ef.root_p then ef.seq end
	and kjt.text=csr.source_text
left join kana_text kt on kt.seq=case when ev.root_p then ev.seq when ef.root_p then ef.seq end
	and kt.text=csr.source_text;"
        (mapcar #'(lambda (x) (format nil "(~a,~a,~a)" 
            (ichiran/dict:word-info-seq x) 
            (sqlify (ichiran/dict:word-info-kana x)) 
            (sqlify (if (eql (ichiran/dict:word-info-type x) :kanji) (ichiran/dict:word-info-text x) nil))))
          words))))))

(defun word-info-recursive (wis)
  (mapcan #'(lambda (x)
    (if (ichiran/dict:word-info-components x)
      (word-info-recursive (ichiran/dict:word-info-components x))
      (list x))) wis))

(defun get-conjs (wis)
    (remove-duplicates (remove-if-not #'ichiran/dict:word-info-seq (word-info-recursive wis))))


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