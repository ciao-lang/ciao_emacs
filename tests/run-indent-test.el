;; Tests for indentation
;;
;; Author: Jose F. Morales 
;;
;; TODO: automatize the tests
;; TODO: test that unindented file becomes indented
;; TODO: test indentation is idempotent
;; TODO: test that ciaopp output is indented
;; TODO: ciaofmt should produce the same indentation

;; Generate current (ciao-mode)
(defun ciao-test-indent-run ()
  (with-temp-file "indent-test.pl-curr"
    (insert-file-contents "indent-test.pl")
    (ciao-mode)
    (measure-time
     (indent-region (point-min) (point-max) nil))))

(defun ciao-test-indent-brief-compare ()
  (if (eq (call-process "diff" nil nil nil "indent-test.pl" "indent-test.pl-curr") 0)
      (message "[OK]")
    (message "[??]")))

;;;; Compare with expected (diff function)
;; (diff "indent-test.pl" "indent-test.pl-curr")

(defun ciao-test-indent-check ()
  "Run and compare the results"
  (message "Running indentation tests...")
  (ciao-test-indent-run)
  (ciao-test-indent-brief-compare))

;; Compare with expected (diff command)
;; (diff "indent-test.pl" "indent-test.pl-curr")

(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

