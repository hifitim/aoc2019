;; ('COM '('B '('G 'H)) 'C '('D 'I) '('E '('J '('K 'L))) 'F)

(defun read-input (file-name)
	(let ((ret nil))
		(with-open-file (stream
			file-name
			:direction :input)
		(loop for line = (read-line stream nil)
			while line do (push (parse-orbit line) ret)))
		ret))

(defun parse-orbit (orbit)
  (let* ((split-orbit (split-by-delimiter orbit #\)))
   (a (car split-orbit))
   (b (cdr split-orbit)))
  (cons a b)))

(defun split-by-delimiter (string delimiter)
  "Split a string into a list on <delimiter>."
  (declare (type string string))
  (declare (type character delimiter))
  (loop for i = 0 then (1+ j)
   as j = (position delimiter string :start i)
   collect (subseq string i j)
   while j))

(defun eql-com (item1 item2)
  (if (string= (car item1) "COM")
    t
    nil))

(defun orbit-list-sort (olist)
  (sort (sort (sort olist #'cadr-string<) #'car-string<) #'eql-com))

(defun car-string< (item1 item2)
  (and
    (string< (car item1) (car item2))))

(defun cadr-string< (item1 item2)
  (and
    (string< (cadr item1) (cadr item2))))

(defun generate-graph (orbit-list)
  (labels
    ((graph-recurse (lst ret)
      (let ((next (car lst)))
        (cond
          ((null lst)
           ret)
          ((string= "COM" (car next))
           (setf ret (make-hash-table :test #'equal))
           (setf (gethash "COM" ret) (cadr next))
           (graph-recurse
             (cdr lst)
             ret))

          (t
            ret)))))
    (let ((olist (orbit-list-sort orbit-list)))
      (format t "olist: ~A~%~%" olist)
      (graph-recurse olist nil))))

(defun exists-in-hash-table (item table)
  (labels ((hash-recurse (item table ret)
                         (loop for k being the hash-keys in table using (hash-value v)
                               do
                               (print-hash-table table)
                               (if (equal k item)
                                   t
                                   (when (eql (type-of v) 'hash-table)
                                     (hash-recurse item v ret))))))
          (hash-recurse item table nil)))

(defun print-hash-table (table)
  (loop for k being the hash-keys in table using (hash-value v)
        do
        (format t "~A: ~A~%" k v)))

(defun print-hash-table-recurse (table)
  (labels ((hash-recurse (table level)
                         (loop for k being the hash-keys in table using (hash-value v)
                               do
                               (loop for k from 0 upto (* level 2)
                                     do
                                     (format t " "))
                               (format t "~A: ~A~%" k v)
                               (if (equal (type-of v) 'hash-table)
                                   (hash-recurse v (+ level 1))))))
          (hash-recurse table 0)))

  (defun remove-at (lst idx)
    "Generate new list that's equal to <lst>, except that the item at <idx> is gone."
    (concatenate
     'list
     (subseq lst 0 idx)
     (nthcdr (+ idx 1) lst)))

  (defun replace-at (lst idx n)
    "Generate new list that's equal to <lst>, except that <n> is now at position <idx>."
    (concatenate
     'list
     (subseq lst 0 idx)
     (list n)
     (nthcdr (+ idx 1) lst)))
