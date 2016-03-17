(setq x 0)
(loop for i from 1 to 10000000 do
     (setq x (+ x i))
   finally (print x))


(time (let ((s 0))
        (dotimes (i 10000001)
          (declare (type fixnum s i))
          (setf s (+ s i)))
        (print s)))


(loop for i fixnum below 100000001
           sum i fixnum)


(declaim (optimize (speed 3) (safety 0)))
(time
 (do ((i 0 (1+ i)) (s 0 (+ s i)))
     ((> i 100000000) (print s))
   (declare (type fixnum s i))))



(time
 (do ((i 0 (1+ i)) (s 0 (+ s i)))
     ((> i 100000000) (print s))
   (declare (type fixnum s i))))


(time (let ((s 0))
        (dotimes (i 100000001)
          (declare (type fixnum s i))
          (setf s (+ s i)))
        (print s)))



(alexandria:with-input-from-file (stream "/tmp/some.org")
  (loop
     :for char = (read-char stream nil)
     :if (null char)
     :do (return (format t "END"))
     :else
     :do (format t "~a~%" char)))
