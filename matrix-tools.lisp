(defpackage #:matrix-tool
  (:use #:CL #:GT)
  (:nicknames #:MXT)
  (:export #:*list-to-array
           #:*array-to-list
           #:array-slice
           #:array-slice-col
           #:array-multiply
           #:matrix-norm-2
           #:point-distance
           #:points-average
           ))

(in-package #:matrix-tool)

(defmacro *list-to-array (l)
  (with-gensyms (ll)
    `(let* ((,ll ,l)
            (len (length ,ll))
         (ar (make-array len :initial-contents ,ll)))
    ar)))

(defmacro *array-to-list (ar)
  (with-gensyms (a)
    `(let ((,a ,ar))
       (loop for i across ,a collect i))))

(defmacro array-slice (m i)
  "get array from matrix, m is matrix i is index"
  (with-gensyms (mm ii)
    `(let* ((,mm ,m)
            (,ii ,i)
            (dim (array-dimensions ,mm))
            (colNum (cadr dim)))
       (make-array colNum :initial-contents
                   (loop for id from 0 to (1- colNum) collect
                        (aref ,mm ,ii id))))))

(defmacro array-slice-col (m i)
  (with-gensyms (mm ii)
    `(let* ((,mm ,m)
            (,ii ,i)
            (dimens (array-dimensions ,mm))
            (rowNum (nth 0 dimens)))
       (loop for i from 0 to (1- rowNum) collect
            (aref ,mm i ,ii)))))

(defmacro array-multiply (array1 array2)
  (with-gensyms (arr1 arr2)
    `(let ((,arr1 ,array1)
           (,arr2 ,array2))
       (loop for i across ,arr1
          for ii across ,arr2 sum
            (* i ii)))))

(defun matrix-norm-2 (m)
  "matrix 2-norm"
  (let ((rowNum (- (elt (array-dimensions m) 0) 1)))
    (sqrt (loop for i from 0 to rowNum
             for a = (array-slice m i)
             sum (array-multiply a a)))))

(defun point-distance (p1 p2)
  "calculate the distance of two points, point input by array and have same length"
  (let ((len (length p1)))
    (if (/= (length p2) len)
        (print "You need make sure arrays length be same")
        (sqrt (loop for i from 0 to (1- len) sum
                   (expt (- (elt p1 i) (elt p2 i)) 2))))))

(defmacro points-average (pn num)
  "pn should be a list of lists, num means how many digits you want to calculate to average value. For example if pn is (list '(2 3 4) '(1 2 3)), result equal (3/2 5/2 7/2) if num is 3, the result will be (3/2 5/2) if num is 2"
  (with-gensyms (arrayNum len arrayList)
    `(let* ((,len ,num)
            (,arrayList ,pn)
            (,arrayNum (length ,arrayList)))
       ;(print ,len) (print ,arrayList) (print ,arrayLen)
       (loop for i from 0 to (1- ,len) collect
            (/ (loop for ar in ,arrayList sum
                    (elt ar i)) ,arrayNum)))))
