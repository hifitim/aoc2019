(declaim (optimize (safety 1) (debug 0) (speed 3)))

(defun read-input (file-name)
  (with-open-file (stream
		   file-name
		   :direction :input)
    (values
     (split-by-delimiter (read-line stream nil) #\,)
     (split-by-delimiter (read-line stream nil) #\,))))

(defun split-by-delimiter (string delimiter)
  "Split a string into a list on <delimiter>."
  (declare (type string string))
  (declare (type character delimiter))
  (loop for i = 0 then (1+ j)
     as j = (position delimiter string :start i)
     collect (subseq string i j)
     while j))

;; R -> x++
;; L -> x--
;; U -> y++
;; D -> y--

(defun build-mega-point-list (directions)
  (let ((len (length directions))
	(builder (get-points directions))
	(pts nil))
    (loop for idx from 0 to (- len 1)
       do
	 (setf pts (funcall builder idx)))
  pts))

(defun get-points (directions)
  (let ((points (list '(0 . 0))))
    (lambda (idx)
      (setf points
	    (concat-lists
	     points
	     (generate-points
	      (char (nth idx directions) 0)
	      (parse-integer (subseq (nth idx directions) 1))
	      (nth (- (length points) 1) points))))))) 

(defun concat-lists (l1 l2)
  (concatenate 'cons l1 l2))

(defun generate-points (direction len start)
  "Direction: RLUD, Len: how many points to generate, Start: starting coord in (x . y)."
  (cond
    ((eql direction #\R)
     (loop for x from 1 upto len
	collect
	  (cons (+ (car start) x) (cdr start))))
    ((eql direction #\L)
     (loop for x from 1 upto len
	collect
	  (cons (- (car start) x) (cdr start))))
    ((eql direction #\U)
     (loop for y from 1 upto len
	collect
	  (cons (car start) (+ (cdr start) y))))
    ((eql direction #\D)
     (loop for y from 1 upto len
	collect
	  (cons (car start) (- (cdr start) y))))))

(defun main ()
  (multiple-value-bind (wire1 wire2)
      (read-input "~/common-lisp/aoc2019/input_day3.txt")
    (let ((pts1 (build-mega-point-list wire1))
	  (pts2 (build-mega-point-list wire2)))
      ;;(setf *x1* pts1)
     ;; (setf *x2* pts2)
      (shortest-manhattan-distance (intersection pts1 pts2 :test #'dot-compare)))))

;; (defparameter *pts1* nil)
;; (defparameter *pts2* nil)
;; (defparameter *int* nil)
;; (defparameter *int1* nil)
;; (defparameter *int2* nil)
;; (defparameter *int-steps* nil)

(defun main2 ()
  (multiple-value-bind (wire1 wire2)
      (read-input "~/common-lisp/aoc2019/input_day3.txt")
    (let ((pts1 (build-mega-point-list wire1))
	  (pts2 (build-mega-point-list wire2))
	  (int nil))
      ;; (setf *pts1* pts1)
      ;; (setf *pts2* pts2)
      (setf int (intersection pts1 pts2 :test #'dot-compare))
      (multiple-value-bind (int1 int2)
	  (find-intersection-steps
	   int
	   pts1
	   pts2)
	;; (setf *int1* int1)
	;; (setf *int2* int2)
	(find-shortest-intersection-total int1 int2)))))

(defun not-eql-zero (item)
  (not (= item 0)))

(defun find-smallest (lst)
  (let ((smallest (find-if #'not-eql-zero lst)))    
    (loop for item in lst
       do
	 (if (and
	      (< item smallest)
	      (not (= 0 item)))
	     (setf smallest item)))
    smallest))    

(defun test-main ()
  (let ((pts1 (build-mega-point-list '("R8" "U5" "L5" "D3")))
	(pts2 (build-mega-point-list '("U7" "R6" "D4" "L4"))))
    (shortest-manhattan-distance (intersection pts1 pts2 :test #'dot-compare))))

(defun shortest-manhattan-distance (lst)
  (let ((shortest nil))
    (loop for item in lst
       do
	 (let ((x (abs (car item)))
	       (y (abs (cdr item))))
	 (if (and (not (= 0 (+ x y)))
		  (or (eql shortest nil)
		      ( < (+ x y) shortest)))
	     (setf shortest (+ x y)))))
    shortest))       
  
(defun dot-compare (x y)
  (and (= (car x) (car y))
       (= (cdr x) (cdr y))))

(defun find-intersection-steps (int-lst wire1 wire2)
  (values
   (loop for k in int-lst
     collect 
	(cons k (position k wire1 :test #'dot-compare)))
   (loop for k in int-lst
      collect 
	(cons k (position k wire2 :test #'dot-compare)))))

(defun find-shortest-intersection-total (w1 w2)
  (let ((shortest (+ (cdar w1) (cdar w2))))
    (loop for k from 0 upto (- (length w1) 1)
       do
	 (let ((current (+ (cdr (nth k w1)) (cdr (nth k w2)))))
	   (if (and
		(< current shortest)
		(not (= 0 current)))
	       (setf shortest current))))
    shortest))
	     
