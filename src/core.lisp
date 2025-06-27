(in-package #:cl-sqids)

(defun shuffle (alphabet)
  (loop
    with l = (length alphabet)
    and n = (- (length alphabet) 1)
    and o = alphabet
    for i from 0
    for j downfrom n to 1
    do
    (let* ((ci (aref o i))
            (cj (aref o j))
            (r (rem (+ (char-code ci)
                      (char-code cj)
                      (* i j)) l)))
      (let ((cr (aref o r)))
        (setf (aref o r) ci)
        (setf (aref o i) cr)))
    finally (return o)))


;; (shuffle "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
;;"fwjBhEY2uczNPDiloxmvISCrytaJO4d71T0W3qnMZbXVHg6eR8sAQ5KkpLUGF9"

(defun to-id (num alphabet)
  (coerce
    (reverse
      (loop with l = (length alphabet)
        for x = num
        then (multiple-value-bind (q _) (floor x l) q)
        while (> x 0)
        nconc (list (aref alphabet (rem x l))))) 'string))

;; (to-id 111 "fwjBhEY2uczNPDiloxmvISCrytaJO4d71T0W3qnMZbXVHg6eR8sAQ5KkpLUGF9")
;; w8

(defun calculate-offset (numbers incr alphabet)
  (let ((al (length alphabet))
         (nl (length numbers)))
    (loop
      with offset = nl
      for i from 0 to (- nl 1)
      do (let* ((num (nth i numbers))
                 (pos (rem num al))
                 (code (char-code (aref alphabet pos))))
           (setq offset
             (+ offset code i)))
      finally (return (rem (+ incr offset) al)))))


(defun do-encode (numbers incr alphabet min-length)
  (when (> incr (length alphabet))
    (error (make-condition 'max-regenerate-id)))
  (let* ((offset (calculate-offset numbers incr alphabet))
          (a (concatenate 'string
               (subseq alphabet offset)
               (subseq alphabet 0 offset)))
          (prefix (aref a 0))
          (nalphabet (reverse a))
          (ids (list prefix)))

    (loop with nl = (length numbers)
      and nlast = (- (length numbers) 1)
      for i from 0 to nlast
      do (let ((separator (aref nalphabet 0))
               (nalphabet-with-separator (subseq nalphabet 1)))
           (if (< i nlast)
             (progn
               (setq ids
                 (concatenate 'string ids
                   (to-id (nth i numbers) nalphabet-with-separator)
                   (list separator)))
               (shuffle nalphabet))
             (setq ids
               (concatenate 'string ids
                 (to-id (nth i numbers) nalphabet-with-separator))))))

    (when (< (length ids) min-length)
      (let ((separator (aref nalphabet 0))
            (fill-size (- min-length (length ids) 1)))
        (shuffle nalphabet)
        (setq ids
          (concatenate 'string ids (list separator)
            (subseq nalphabet 0 (min fill-size (length nalphabet)))))))
    ids))

;; (do-encode '(111) 0 "fwjBhEY2uczNPDiloxmvISCrytaJO4d71T0W3qnMZbXVHg6eR8sAQ5KkpLUGF9" 10)
;; "LKEpHOn63V"

(defun to-number (s alphabet)
  (loop with al = (length alphabet)
    and a = 0
    and code = (coerce s 'list)
    for c in code
    do (let ((i (position c alphabet)))
           (setq a (+ (* a al) i)))
    finally (return a)))

;; (to-number "w8" "fwjBhEY2uczNPDiloxmvISCrytaJO4d71T0W3qnMZbXVHg6eR8sAQ5KkpLUGF9")
;; 111

(defun do-decode (s alphabet)
  (when (or (null s)
          (= (length s) 0))
    (return-from do-decode nil))
  (loop with code = (coerce s 'list)
    for c in code
    do (when (null (position c alphabet))
         (error (make-condition 'bad-alphabet))))
  (let* ((prefix (aref s 0))
          (offset (position prefix alphabet))
          (a (concatenate 'string
               (subseq alphabet offset)
               (subseq alphabet 0 offset)))
          (nalphabet (reverse a))
          (numbers '()))
    (loop with code = (subseq s 1)
      while (> (length code) 0)
      do (let* ((separator (aref nalphabet 0))
                (nalphabet-with-separator (subseq nalphabet 1))
                (offset (position separator code)))
           (cond
             ((null offset)
               (progn
                 (setq numbers
                   (nconc numbers (list (to-number code nalphabet-with-separator))))
                 (return nil)))
             ((> offset 0)
               (let ((chunk (subseq code 0 offset))
                      (remain (subseq code (+ offset 1))))
                 (setq code remain)
                 (setq numbers
                   (nconc numbers (list (to-number chunk nalphabet-with-separator))))
                 (shuffle nalphabet)))
             (t (return nil)))))
    numbers))
