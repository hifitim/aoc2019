(defun read-input (file-name)
  (let ((ret nil))
    (with-open-file (stream
		     file-name
		     :direction :input)
      (loop for line = (read-line stream nil)
	 while line do (push (parse-integer line) ret)))
    ret))

(defun calculate-fuel-requirements (distance)
  (- (truncate (/ distance 3)) 2))

(defun calculate-total-fuel ()
  (loop for f in
       (mapcar #'calculate-fuel-requirements-recursive
	       (read-input "/home/tim/common-lisp/aoc2019/input_day1.txt"))
     sum f))
       
(defun calculate-fuel-requirements-recursive (distance)
  (labels ((recurse (dist total)
	     (cond
	       ((<= dist 0)
		(- total dist))
	       (t
		(let ((new-fuel (calculate-fuel-requirements dist)))
		  (recurse
		   new-fuel
		   (+ total new-fuel)))))))
    (recurse distance 0)))
		 
