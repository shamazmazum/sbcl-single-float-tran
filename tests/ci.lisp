(defun do-all()
  (ql:quickload :sbcl-single-float-tran/tests)
  (uiop:quit
   (if (uiop:call-function "sbcl-single-float-tran/tests:run-tests")
       0 1)))

(do-all)
