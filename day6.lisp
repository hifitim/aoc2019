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

(defun print-hash-entry (key value)
  (format t "key: ~A, value: ~A~%" key value))

(defun exists-in-graph (item graph)
  (labels
      ((graph-recurse (item keys graph)
	 (cond
	   ((and
	     (null keys)
	     (> (hash-key-count graph) 0))
	    (graph-recurse
	     item
	     (hash-keys (gethash (car (hash-keys graph)) graph))
	     (gethash (car (hash-keys graph)) graph)))
	   ((null keys)
	    nil)
	   ((equal item (car keys))
	    (gethash (car keys) graph))
	   (t
	    (graph-recurse item (cdr keys) graph)))))
    (graph-recurse item (hash-keys graph) graph)))
  
(defun add-to-graph (item graph)
  t)

(defun build-graph (input)
  (labels
      ((build-recurse (input graph)
	 (cond
	   ((string-equal (caar input) "COM")
	    t) ;; special case TBD for first node.
	   ((exists-in-graph (caar input) graph)
	    t) ;; not sure what to do yet.
	   (t ;; doesn't exist in graph
	    (build-recurse
	     (cdr input)
	     (add-to-graph
	      (car input)
	      graph))))))
    (build-recurse input nil)))

(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))

(defun hash-key-count (hash-table)
  (length (hash-keys hash-table)))
       
(defparameter *COM* (make-hash-table :test #'equal))
(defun build-test ()
  (setf (gethash "B" *COM*) (make-hash-table :test #'equal))
  (setf (gethash "C" (gethash "B" *COM*)) (make-hash-table :test #'equal))
  (setf (gethash "G" (gethash "B" *COM*)) (make-hash-table :test #'equal))
  (setf (gethash "H" (gethash "G" (gethash "B" *COM*))) (make-hash-table :test #'equal)))
  
