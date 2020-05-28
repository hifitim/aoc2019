(Declaim (optimize (debug 3)))

(defun read-input (file-name)
  (with-open-file (stream
		   file-name
		   :direction :input)
    (loop for line = (read-line stream nil)
       while line collect
	 (cons
	  (car (split-by-delimiter line #\)))
	  (cadr (split-by-delimiter line #\)))))))
      
(defun split-by-delimiter (string delimiter)
  "Split a string into a list on <delimiter>."
  (declare (type string string))
  (declare (type character delimiter))
  (loop for i = 0 then (1+ j)
     as j = (position delimiter string :start i)
     collect (subseq string i j)
     while j))

;; Sorts to alpha order, with COM as the first node.
(defun graph-sorter (x y)
  (cond
    ((string-equal (car x) "COM")
     t)
    ((string-equal (car y) "COM")
     nil)
    ((string< (car x) (car y))
     t)
    ((string-equal (car x) (car y))
     (string< (cdr x) (cdr y)))
    (t
     nil)))

(defun print-hash-entry (key hash-table)
  (format t "key: ~A, value: ~A~%" key (gethash key hash-table)))

(defun print-depth (depth stream)
  (loop for i from 0 upto (* depth 2)
     do (format stream " ")))

(defun count-orbits (graph)
  (let ((total 0))
    (labels
	((count-internal (graph depth)
	   (loop for k being each hash-key of graph
	      do
		(setf total (+ total depth))
		(if (> (hash-key-count (gethash k graph)) 0)
		    (count-internal (gethash k graph) (+ depth 1))))))
      (count-internal graph 1))
    total))

(defun print-graph (graph stream)
  (labels
      ((print-internal (graph depth)
	 (loop for k being each hash-key of graph
	    do
	      (print-depth depth stream)
	      (format stream "~A~%" k)
	      (if (> (hash-key-count (gethash k graph)) 0)
		  (print-internal (gethash k graph) (+ depth 1))))))
    (print-internal graph 0)))    

(defun exists-in-graph (item graph)
  (let ((found nil)
	(outer-depth 0))
    (labels
	((exists-internal (item graph depth)
	   (loop for k being each hash-key of graph
	      do
		(if (equal item k)
		    (progn
		      (setf found (gethash k graph))
		      (setf outer-depth depth))
		    (when (> (hash-key-count (gethash k graph)) 0)
		      (exists-internal item (gethash k graph) (+ depth 1)))))))
      (exists-internal item graph 1))
    (values found outer-depth)))
  
(defun add-to-graph (item parent graph)
  (let ((exists (exists-in-graph parent graph)))
    (when exists
      (setf (gethash item exists) (make-hash-table :test #'equal)))))

(defun find-orbiters (base input)
  (remove-if-not
   (lambda (x)
     (equal (car x) base))
   input))

(defun remove-from-input (orbiters input)
  (remove-if 
   (lambda (x)
     (let ((found nil))
       (loop for item in orbiters
	  do
	    (if (equal item x)
		(setf found t)))
       found))
   input))

(defun build-graph (input)
  (let ((output-graph (make-hash-table :test #'equal)))
    (labels
	((build-recurse (input next graph)
	     (cond
	       ((null input)
		graph)
	       ((equal (caar input) "COM")
		(setf (gethash (cdar input) graph) (make-hash-table :test #'equal))
		(build-recurse (cdr input) (cdar input) graph))
	       (t
		(let* ((orbiters (find-orbiters next input)))
		  (loop for o in orbiters
		     do
		       (add-to-graph (cdr o) (car o) graph)
		       (build-recurse
			(remove-from-input orbiters input)
			(cdr o)
			graph)))))))
      (build-recurse input nil output-graph))
    output-graph))

(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))

(defun hash-key-count (hash-table)
  (length (hash-keys hash-table)))


(defun main-print ()
  (with-open-file (out-stream "C:\\Users\\a0232709\\common-lisp\\aoc2019\\day6_out.txt" :direction :output)
    (print-graph
     (build-graph (sort (read-input "C:\\Users\\a0232709\\common-lisp\\aoc2019\\input_day6.txt") #'graph-sorter))
     out-stream)))

(defparameter *COM* (make-hash-table :test #'equal))
(defun main1()
  (setf *COM*
	(count-orbits (build-graph (sort (read-input "C:\\Users\\a0232709\\common-lisp\\aoc2019\\input_day6.txt") #'graph-sorter)))))

