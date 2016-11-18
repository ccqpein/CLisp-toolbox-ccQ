(defpackage #:math-tool
  (:use #:CL #:GT)
  (:nicknames #:MT)
  (:export #:standard-deviation
           #:gen-random-num
           #:sigma
           ))

(in-package #:math-tool)

(defun standard-deviation (numlist)
  "return the value include the average and standard deviation of the numlist"
  (let* ((sum (apply #'+ numlist))
         (len (length numlist))
         (aver (/ sum len))
         (SD (sqrt (loop for i in numlist sum
                       (expt (- i aver) 2)))))
    (list aver SD)))



(defun gen-random-num (n times)
  "return result is list. n is number limit, times is number of results. No same number in the result list"
  (let ((result '())
        (x))
    (dotimes (i times result)
      (tagbody
         (setf x (random n *random-state*))
         (go tag-b)
       tag-a
         (setf x (random n *random-state*))
         (go tag-b)
       tag-b
         (if (find x result)
             (go tag-a)
             (go tag-c))
       tag-c
         (setf result (append result (list x)))))
    ))



(defmacro sigma ((&rest exp) indPara times &rest paraList)
  "Improve the sigma macto from second version, this marco can do the several parameters at same time now:
For example (sigma (+ 1 2) (1 2) 4 '(1 2 3 4 5) '(5 4 3 2))
             => (+ (+ 1 5) (+ 2 4) (+ 3 3) (+ 4 2))"
  `(+ ,@(loop for tt from 0 to (1- times)
           for expT = (copy-list exp)
           do
             (loop for id from 0 to (1- (length indPara))
                  for paraTT = (eval (nth id paraList))
                do (setf (nth (nth id indPara) expT)
                         (nth tt paraTT)))
           collect expT)))
