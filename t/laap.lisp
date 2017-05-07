(in-package #:laap/test)

;;; Run with: ./buildapp --output bin/foo --asdf-tree ~/quicklisp/ --load-system laap-test --entry laap/test::main --eval '(setf *debugger-hook* (lambda (c h) (declare (ignore h)) (format t "~A~%" c)))'
;;; for strace runs et al.
(defun main (&rest args)
  (loop for i below 3
     do (laap:spawn (wait-and-print))))

(test delay (done)
  (format t "foo~%")
  (laap:delay 1
      (lambda (err res)
	(format t "bar~%")
	(laap:delay 1
	    (lambda (err res)
	      (format t "baz~%")
	      (funcall done))))))

(defun current-thread-name ()
  (bt:thread-name (bt:current-thread)))

(defun wait-and-print (done)
  (let ()
    (lambda (err res)
      (format t "foo on thread ~a~%" (current-thread-name))
      (laap:delay (1+ (random 1.0))
	  (lambda (err res)
	    (format t "bar on thread ~a~%" (current-thread-name))
	    (laap:delay (1+ (random 1.0))
		(lambda (err res)
		  (format t "baz on thread ~a~%" (current-thread-name))
		  (funcall done))))))))

(test spawn-delay (done)
  (loop for i below 3
     do (laap:spawn (wait-and-print done))))
