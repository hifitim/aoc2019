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

(defun print-depth (depth)
  (loop for i from 0 upto (* depth 2)
     do (format t " ")))

(defun print-graph (graph)
  (labels
      ((print-internal (graph depth)
	 (loop for k being each hash-key of graph
	    do
	      (print-depth depth)
	      (format t "~A~%" k)
	      (if (> (hash-key-count (gethash k graph)) 0)
		  (print-internal (gethash k graph) (+ depth 1))))))
    (print-internal graph 0)))

(defun exists-in-graph (item graph)
  (let ((found nil))
    (labels
	((exists-internal (item graph)
	   (loop for k being each hash-key of graph
	      do
		(if (equal item k)
		    (setf found (gethash k graph))
		    (if (> (hash-key-count (gethash k graph)) 0)
			(exists-internal item (gethash k graph)))))))
      (exists-internal item graph)
      found)))
  
(defun add-to-graph (item parent graph)
  (let ((exists (exists-in-graph parent graph)))
    (when exists
      (setf (gethash item exists) (make-hash-table :test #'equal)))))

(defun build-graph (input)
  (let ((out-graph nil))
    (labels
	((build-recurse (input graph)
	   (cond
	     ((string-equal (caar input) "COM")
	      (setf graph (make-hash-table :test #'equal))
	      (setf (gethash (cdar input) graph) (make-hash-table :test #'equal))
	      (build-recurse (cdr input) graph))
	     ((null input)
	      graph)
	     (t
	      (add-to-graph
	       (cdar input)
	       (caar input)
	       graph)
	      (build-recurse
	       (cdr input)
	       graph)))))
	 (build-recurse input out-graph))))

(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))

(defun hash-key-count (hash-table)
  (length (hash-keys hash-table)))

(defun remove-at (n lst)
  (concatenate
   'cons
   (subseq lst 0 (- n 1))
   (subseq lst n (length lst))))

(defun insert-at (n item lst)
  (concatenate
   'cons
   (subseq lst 0 n)
   (list item)
   (subseq lst n (length lst))))

(defun order-input (input)
  (let ((output nil))
    (labels
	((order-recurse (input next ordered-input)
	   (cond
	     ((null input)
	      ordered-input)
	     ((equal (caar input) "COM")
	      (insert-at 0 (car input) ordered-input)
	      (order-recurse (cdr input) (cdar input) ordered-input))
	     
      (order-recurse input output))
    output))

(defparameter *COM* (make-hash-table :test #'equal))
(defun build-test ()
  (setf (gethash "B" *COM*) (make-hash-table :test #'equal))
  (setf (gethash "C" (gethash "B" *COM*)) (make-hash-table :test #'equal))
  (setf (gethash "G" (gethash "B" *COM*)) (make-hash-table :test #'equal))
  (setf (gethash "H" (gethash "G" (gethash "B" *COM*))) (make-hash-table :test #'equal))
  (setf (gethash "Q" (gethash "B" *COM*)) (make-hash-table :test #'equal)))

(defun main ()
  (order-input (sort (read-input "/home/tim/common-lisp/aoc2019/input_day6.txt") #'graph-sorter)))
