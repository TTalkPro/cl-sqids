(in-package #:cl-sqids)

(defclass sqids ()
  ((alphabet
     :initarg :alphabet
     :accessor alphabet)
    (min-length
      :initarg :min-length
      :accessor min-length)
    (blocklist
      :initarg :blocklist
      :accessor blocklist)))

(defmethod (setf alphabet) (nval (instance sqids))
  (error
    (make-condition 'update-immutable-slot)))

(defmethod (setf min-length) (nval (instance sqids))
  (error
    (make-condition 'update-immutable-slot)))

(defmethod (setf blocklist) (nval (instance sqids))
  (error
    (make-condition 'update-immutable-slot)))


(defmethod encode (numbers (instance sqids))
  (let ((number-list
          (typecase numbers
            (number (list numbers))
            (list numbers)
            (t (error (make-condition 'unsupported-type))))))
    (loop with alphabet =  (shuffle (copy-seq (alphabet instance)))
      and i = 0
      and tid = nil
      do (let ((ttid
                 (do-encode number-list i
                   (copy-seq alphabet)
                   (min-length instance))))
           (if (null (find ttid (blocklist instance) :test #'string=))
             (setq tid ttid)
             (incf i)))
      until (not (null tid))
      finally (return tid))))

(defmethod decode (ids (instance sqids))
  (do-decode ids
    (shuffle (copy-seq (alphabet instance)))))

(defun make-sqids (&key
                      (min-length default-length)
                      (alphabet default-alphabet)
                      (blocklist default-blocklist))
  (make-instance 'sqids :min-length min-length :alphabet alphabet :blocklist blocklist))

;; (defparameter s (make-instance 'sqids :min-length 10))
;; (encode '(111) s)
;; (decode (encode '(111) s) s)
