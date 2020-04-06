;; input: 367479-893698
(defparameter *input* "367479-893698")

(defun main ()
  (multiple-value-bind (start end)
      (change-input *input*)
    (length
     (loop for num from start upto end
	when (meets-rules num)
	collect num))))

(defun main2 ()
  (multiple-value-bind (start end)
      (change-input *input*)
    (length
     (loop for num from start upto end
	when (meets-rules2 num)
	collect num))))

(defun change-input (input)
  (values
   (parse-integer (subseq input 0 (position #\- input)))
   (parse-integer (subseq input (+ (position #\- input) 1)))))

(defun int-to-list (num)
  (map 'list
       (lambda (c)
	 (or (digit-char-p c) '-))
       (prin1-to-string num)))

(defun correct-grouping-p (lst)
  (labels ((recurse (lst group-of-two)
	     (cond
	       ((null lst)
		group-of-two)
	       (t
		(recurse
		 (cdr lst)
		 (or
		  (= (length (car lst)) 2)
		  group-of-two))))))
    (recurse lst nil)))    

(defun break-up-list (lst)
  (let ((ret '()))
    (loop for k from 0 to (- (length lst) 1)
       do
	 (if (= k 0)
	     (push (list (nth k lst)) ret)  
	     (if (= (caar ret) (nth k lst)) 
		 (push (nth k lst) (car ret))
		 (push (list (nth k lst)) ret))))
    (reverse ret)))

(defun two-consecutive-same (lst)
  (labels ((recurse (lst prev ret)
	     (cond
	       ((null lst)
		ret)
	       (t
		(recurse
		 (cdr lst)
		 (car lst)
		 (or
		  ret
		  (= prev (car lst))))))))
    (recurse (cdr lst) (car lst) nil)))

(defun never-decreasing (lst)
  (labels ((recurse (lst prev ret)
	     (cond
	       ((null lst)
		ret)
	       (t
		(recurse
		 (cdr lst)
		 (car lst)
		 (and
		  ret
		  (or
		   (> (car lst) prev)
		   (= (car lst) prev))))))))
    (recurse (cdr lst) (car lst) t)))

(defun meets-rules (num)
  (let ((num-list (int-to-list num)))
    (and
     (two-consecutive-same num-list)
     (never-decreasing num-list))))

(defun meets-rules2 (num)
  (let ((num-list (int-to-list num)))
    (and
     (correct-grouping-p (break-up-list num-list))
     (never-decreasing num-list))))
