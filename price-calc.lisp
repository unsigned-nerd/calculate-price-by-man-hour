; vim: colorcolumn=101 textwidth=100

; calculate price for our software development and ict services
; todo:
;   - make sure that each cons cell in *salaries* is unique, e.g. no two john.doe's in it

(defparameter *salaries* '(
  ; {{{
  ; special force
  (john.doe   . 10000)
  (john.smith . 20500)

  ; software
  (johnson.smith   . 50000)
  (williams.jones  . 41250)
  (davis.brown     . 31210)
  (wilson.miller   . 18000)
  (taylor.moore    . 17000)
  (thomas.anderson . 17000)
  (jackson.white   . 22000)
  
  ; ict
  (adams.baker . 50120)
  (bell.murphy . 21456)))
  ; }}}

(defun get-salary (employee-name)
  (cdr (assoc employee-name *salaries*)))

(defun calculate-cost (work-hours-list)
  ; work-hours-list is an alist of employee's name and his/her work hours
  ; e.g. ((john.doe . 5) (john.smith 20) (john.doe 10))
  (reduce #'+
    (mapcar
      #'(lambda (x)
          (flet ((cost-per-hour (salary) (/ salary (* 20 8.0))))
            (* (cost-per-hour (get-salary (car x))) (cdr x))))
      work-hours-list)))

(defun calculate-price (cost)
  (* cost 2))

(defmacro prompt-for-input (var prompt-message)
  `(progn
    (format t ,prompt-message)
    (finish-output)
    (setf ,var (read))))
  
(format t "Enter 'q' as the employee name to calculate and exit.~%")

(let ((work-hours-list '()))
  (loop
    (let ((employee-name) (work-hours))
      (prompt-for-input employee-name "Please enter the employee name: ")
      (when (eql employee-name 'q)
        (format t "The price (cost*2) is ~a~%" (calculate-price (calculate-cost work-hours-list)))
        (return))
      (prompt-for-input work-hours "Please enter his/her work hours: ")
      (push (cons employee-name work-hours) work-hours-list))))
