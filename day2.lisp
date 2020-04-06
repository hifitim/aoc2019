(defun read-input (file-name)
  (with-open-file (stream
		   file-name
		   :direction :input)
    (split-by-delimiter (read-line stream nil) #\,)))
      
(defun split-by-delimiter (string delimiter)
  "Split a string into a list on <delimiter>."
  (declare (type string string))
  (declare (type character delimiter))
  (loop for i = 0 then (1+ j)
     as j = (position delimiter string :start i)
     collect (parse-integer (subseq string i j))
     while j))

(defun parse-intcode (intcode)
  (labels ((parse-recurse (lst idx)
	     (let ((opcode (nth idx lst)))
	       (cond
		 ((= (+ 1 idx) (length lst))
		  lst)
		 ((= opcode 1)		  
		  (parse-recurse
		   (replace-at
		    lst
		    (nth (+ 3 idx) lst)
		    (+
		     (nth (nth (+ 1 idx) lst) lst)
		     (nth (nth (+ 2 idx) lst) lst)))
		   (+ idx 4)))
		 ((= opcode 2)		  
		  (parse-recurse
		   (replace-at
		    lst
		    (nth (+ 3 idx) lst)
		    (*
		     (nth (nth (+ 1 idx) lst) lst)
		     (nth (nth (+ 2 idx) lst) lst)))
		   (+ idx 4)))
		 ((= opcode 99)
		  lst)))))
    (parse-recurse intcode 0)))

(defun replace-at (lst idx n)
  "Generate new list that's equal to <lst>, except that <n> is now at position <idx>."
  (concatenate
   'list
   (subseq lst 0 idx)
   (list n)
   (nthcdr (+ idx 1) lst)))

(defun main1 ()
  (parse-intcode (read-input "~/common-lisp/aoc2019/input_day2.txt")))

(defun main2 ()
  (let ((intcode (read-input "~/common-lisp/aoc2019/input_day2.txt")))
    (loop for noun from 0 upto 99
       do
	 (loop for verb from 0 upto 99
	    do
	      (let ((answer
		     (car (parse-intcode
			   (replace-at (replace-at intcode 1 noun) 2 verb)))))
		(if (and (> 19e6 answer) (< 20e6 answer))		    
		    (format t "Noun: ~A, Verb: ~A, Pos0: ~A~%" noun verb answer))
		(if (= 19690720 answer)
		    (format t "Found!~%Noun: ~A~%Verb: ~A~%Answer:~A~%"
			    noun verb (+ (* 100 noun) verb))))))))
	 
    
