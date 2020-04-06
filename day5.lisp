(defparameter *input-id* 5)
(defparameter *logging* nil)

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

(defun int-to-list (num)
  (map 'list
       (lambda (c)
	 (or (digit-char-p c) '-))
       (prin1-to-string num)))

(defun list-to-int (lst)
  (labels ((recurse (lst idx result)
	     (cond
	       ((null lst)
		result)
	       (t
		(recurse
		 (cdr lst)
		 (+ idx 1)
		 (+
		  result
		  (*
		   (expt 10 (- (length lst) 1))
		   (car lst))))))))
    (recurse lst 0 0)))

(defun op1 (intcode idx param-mode)
  (let ((arg1
	 (if (= (nth 0 param-mode) 0)
	     (nth (nth (+ 1 idx) intcode) intcode)
	     (nth (+ 1 idx) intcode)))
	(arg1-addr
	 (if (= (nth 0 param-mode) 0)
	     (nth (+ 1 idx) intcode)
	     nil))
	(arg2
	 (if (= (nth 1 param-mode) 0)
	     (nth (nth (+ 2 idx) intcode) intcode)
	     (nth (+ 2 idx) intcode)))
	(arg2-addr
	 (if (= (nth 1 param-mode) 0)
	     (nth (+ 2 idx) intcode)
	     nil))
	(arg3
	 (if (= (nth 2 param-mode) 0)
	     (nth (nth (+ 3 idx) intcode) intcode)
	     (nth (+ 3 idx) intcode)))
	(arg3-addr
	 (if (= (nth 2 param-mode) 0)
	     (nth (+ 3 idx) intcode)
	     nil)))
    (when *logging*
      (format t "ptr: ~A~%" idx)
      (format t "instr: ~A~%" (subseq intcode idx (+ idx 4)))
      (format t "replacing ~A (addr ~A) with ~A [~A (addr: ~A) * ~A (addr: ~A)]~%"
	      arg3 
	      arg3-addr
	      (+ arg1 arg2)
	      arg1
	      arg1-addr
	      arg2
	      arg2-addr)
      (format t "~%"))
    
    (replace-at
     intcode
     arg3-addr
     (+ arg1 arg2))))

(defun op2 (intcode idx param-mode)
  (let ((arg1
	 (if (= (nth 0 param-mode) 0)
	     (nth (nth (+ 1 idx) intcode) intcode)
	     (nth (+ 1 idx) intcode)))
	(arg1-addr
	 (if (= (nth 0 param-mode) 0)
		(nth (+ 1 idx) intcode)
		nil))
	(arg2
	 (if (= (nth 1 param-mode) 0)
	     (nth (nth (+ 2 idx) intcode) intcode)
	     (nth (+ 2 idx) intcode)))
	(arg2-addr
	 (if (= (nth 1 param-mode) 0)
		(nth (+ 2 idx) intcode)
		nil))
	(arg3
	 (if (= (nth 2 param-mode) 0)
	     (nth (nth (+ 3 idx) intcode) intcode)
	     (nth (+ 3 idx) intcode)))
	(arg3-addr
	 (if (= (nth 2 param-mode) 0)
		(nth (+ 3 idx) intcode)
		nil)))
  (when *logging*
    (format t "ptr: ~A~%" idx)
    (format t "instr: ~A~%" (subseq intcode idx (+ idx 4)))
    (format t "replacing ~A (addr ~A) with ~A [~A (addr: ~A) * ~A (addr: ~A)]~%"
	    arg3 
	    arg3-addr
	    (* arg1 arg2)
	    arg1
	    arg1-addr
	    arg2
	    arg2-addr)
    (format t "~%"))
  
  (replace-at
   intcode
   arg3-addr
   (* arg1 arg2))))

(defun op3 (intcode idx)
  (let ((arg1
	 (nth (nth (+ 1 idx) intcode) intcode))
	(arg1-addr
	 (nth (+ 1 idx) intcode)))
  (when *logging*
    (format t "ptr: ~A~%" idx)
    (format t "instr: ~A~%" (subseq intcode idx (+ idx 2)))
    (format t "input: ~A~%" *input-id*)
    (format t "replacing ~A (addr: ~A) with ~A~%"
	    arg1
	    arg1-addr
	    *input-id*)
    (format t "~%"))
  (replace-at
   intcode
   arg1-addr
   *input-id*)))

(defun op4 (intcode idx param-mode)
   (let ((arg1
	 (if (= (nth 0 param-mode) 0)
	     (nth (nth (+ 1 idx) intcode) intcode)
	     (nth (+ 1 idx) intcode)))
	 (arg1-addr
	 (if (= (nth 0 param-mode) 0)
	     (nth (+ 1 idx) intcode)
	     nil)))
     (when *logging*
       (format t "ptr: ~A~%" idx)
       (format t "instr: ~A~%" (subseq intcode idx (+ idx 2)))
       (format t "output ~A at address ~A.~%"
	       arg1
	       arg1-addr)
       (format t "~%"))
     (format t "opcode 4: ~A~%"
	     arg1)
     (when *logging*
       (format t "~%"))))

(defun op5 (intcode idx param-mode)
  (let ((arg1
	 (if (= (nth 0 param-mode) 0)
	     (nth (nth (+ 1 idx) intcode) intcode)
	     (nth (+ 1 idx) intcode)))
	(arg1-addr
	 (if (= (nth 0 param-mode) 0)
		(nth (+ 1 idx) intcode)
		nil))
	(arg2
	 (if (= (nth 1 param-mode) 0)
	     (nth (nth (+ 2 idx) intcode) intcode)
	     (nth (+ 2 idx) intcode)))
	(arg2-addr
	 (if (= (nth 1 param-mode) 0)
		(nth (+ 2 idx) intcode)
		nil)))
   (when *logging*
    (format t "ptr: ~A~%" idx)
    (format t "instr: ~A~%" (subseq intcode idx (+ idx 3)))
    (if (= arg1 0)
	(format t "NOT "))
    (format t "jumping from ~A to ~A (addr: ~A) because ~A (addr: ~A)~%"
	    idx
	    arg2
	    arg2-addr
	    arg1
	    arg1-addr)
    (format t "~%"))
   (if (not (= arg1 0))
       arg2
       (+ idx 3))))

(defun op6 (intcode idx param-mode)
   (let ((arg1
	 (if (= (nth 0 param-mode) 0)
	     (nth (nth (+ 1 idx) intcode) intcode)
	     (nth (+ 1 idx) intcode)))
	(arg1-addr
	 (if (= (nth 0 param-mode) 0)
		(nth (+ 1 idx) intcode)
		nil))
	(arg2
	 (if (= (nth 1 param-mode) 0)
	     (nth (nth (+ 2 idx) intcode) intcode)
	     (nth (+ 2 idx) intcode)))
	(arg2-addr
	 (if (= (nth 1 param-mode) 0)
		(nth (+ 2 idx) intcode)
		nil)))
   (when *logging*
    (format t "ptr: ~A~%" idx)
    (format t "instr: ~A~%" (subseq intcode idx (+ idx 3)))
    (if (not (= arg1 0))
	(format t "NOT "))
    (format t "jumping from ~A to ~A (addr: ~A) because ~A (addr: ~A) = 0~%"
	    idx
	    arg2
	    arg2-addr
	    arg1
	    arg1-addr)
    (format t "~%"))
   (if (= arg1 0)
       arg2
       (+ idx 3))))

(defun op7 (intcode idx param-mode)
  (let ((arg1
	 (if (= (nth 0 param-mode) 0)
	     (nth (nth (+ 1 idx) intcode) intcode)
	     (nth (+ 1 idx) intcode)))
	(arg1-addr
	 (if (= (nth 0 param-mode) 0)
		(nth (+ 1 idx) intcode)
		nil))
	(arg2
	 (if (= (nth 1 param-mode) 0)
	     (nth (nth (+ 2 idx) intcode) intcode)
	     (nth (+ 2 idx) intcode)))
	(arg2-addr
	 (if (= (nth 1 param-mode) 0)
		(nth (+ 2 idx) intcode)
		nil))
	(arg3
	 (if (= (nth 2 param-mode) 0)
	     (nth (nth (+ 3 idx) intcode) intcode)
	     (nth (+ 3 idx) intcode)))
	(arg3-addr
	 (if (= (nth 2 param-mode) 0)
		(nth (+ 3 idx) intcode)
		nil)))
  (when *logging*
    (format t "ptr: ~A~%" idx)
    (format t "instr: ~A~%" (subseq intcode idx (+ idx 4)))
    (format t "replacing ~A (addr ~A) with ~A because ~A (addr: ~A) < ~A (addr: ~A)~%"
	    arg3 
	    arg3-addr
	    (if (< arg1 arg2) 1 0)
	    arg1
	    arg1-addr
	    arg2
	    arg2-addr)
    (format t "~%"))
  (replace-at
   intcode
   arg3-addr
   (if (< arg1 arg2) 1 0))))

(defun op8 (intcode idx param-mode)
  (let ((arg1
	 (if (= (nth 0 param-mode) 0)
	     (nth (nth (+ 1 idx) intcode) intcode)
	     (nth (+ 1 idx) intcode)))
	(arg1-addr
	 (if (= (nth 0 param-mode) 0)
		(nth (+ 1 idx) intcode)
		nil))
	(arg2
	 (if (= (nth 1 param-mode) 0)
	     (nth (nth (+ 2 idx) intcode) intcode)
	     (nth (+ 2 idx) intcode)))
	(arg2-addr
	 (if (= (nth 1 param-mode) 0)
		(nth (+ 2 idx) intcode)
		nil))
	(arg3
	 (if (= (nth 2 param-mode) 0)
	     (nth (nth (+ 3 idx) intcode) intcode)
	     (nth (+ 3 idx) intcode)))
	(arg3-addr
	 (if (= (nth 2 param-mode) 0)
		(nth (+ 3 idx) intcode)
		nil)))
  (when *logging*
    (format t "ptr: ~A~%" idx)
    (format t "instr: ~A~%" (subseq intcode idx (+ idx 4)))
    (format t "replacing ~A (addr ~A) with ~A because ~A (addr: ~A) = ~A (addr: ~A)~%"
	    arg3 
	    arg3-addr
	    (if (= arg1 arg2) 1 0)
	    arg1
	    arg1-addr
	    arg2
	    arg2-addr)
    (format t "~%"))
  
      (replace-at
       intcode
       arg3-addr
       (if (= arg1 arg2) 1 0))))

(defun get-opcode (op-param-code)
  (if (= (length (int-to-list op-param-code)) 1)
      op-param-code
      (list-to-int
       (subseq
	(int-to-list op-param-code)
	(- (length (int-to-list op-param-code)) 2)))))

(defun get-param-mode (op-param-code)
  (if (= (length (int-to-list op-param-code)) 1)
      '(0 0 0)
      (reverse
       (let ((mode
	      (subseq
	       (int-to-list op-param-code)
	       0
	       (- (length (int-to-list op-param-code)) 2))))
	 (cond
	   ((= (length mode) 2)
	    (cons 0 mode))
	   ((= (length mode) 1)
	    (cons 0 (cons 0 mode)))
	   (t
	    mode))))))	 

(defun parse-intcode (intcode)
  (labels ((parse-recurse (lst idx)
	     ;;(format t "intcode: ~A~%" lst)
	     (let* ((op-param-code (nth idx lst))
		    (opcode (get-opcode op-param-code))
		    (param-mode (get-param-mode op-param-code)))
	       (cond
		 ((= (+ 1 idx) (length lst))
		  lst)
		 ((= opcode 1)		  
		  (parse-recurse
		   (op1 lst idx param-mode)
		   (+ idx 4)))
		 ((= opcode 2)		  
		  (parse-recurse
		   (op2 lst idx param-mode)
		   (+ idx 4)))
		 ((= opcode 3)
		  (parse-recurse
		   (op3 lst idx)
		   (+ idx 2)))
		 ((= opcode 4)
		  (op4 lst idx param-mode)
		  (parse-recurse
		   lst
		   (+ idx 2)))
		 ((= opcode 5)
		  (parse-recurse
		   lst
		   (op5 lst idx param-mode)))
		 ((= opcode 6)
		  (parse-recurse
		   lst
		   (op6 lst idx param-mode)))
		 ((= opcode 7)		  
		  (parse-recurse
		   (op7 lst idx param-mode)
		   (+ idx 4)))
		 ((= opcode 8)		  
		  (parse-recurse
		   (op8 lst idx param-mode)
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
  (parse-intcode (read-input "~/common-lisp/aoc2019/input_day5.txt"))
  (format t "fin.~%"))

