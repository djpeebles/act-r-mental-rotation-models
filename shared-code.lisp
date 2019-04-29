;;;; =========================================================
;;;;
;;;;
;;;; =========================================================

(written-for-act-r-version "7.11.1")
(clear-all)

(defparameter *model-data* nil)
(defparameter *all-model-data* nil)
(defparameter *response-key* nil)
(defparameter *start-time* 0.0  "Time when trial starts.")

;;; ==========================================================
;;;
;;; ==========================================================

(defun record-time (model key)
  (declare (ignore model))
  (setf *response-key* key)
  (push (- (mp-time) *start-time*) *model-data*))

;; ===========================================================
;; ===========================================================

(defun average (list-of-lists)
  (let ((length (length list-of-lists)))
    (mapcar (lambda (x)
              (/ x length))
            (reduce (lambda (l1 l2)
                      (mapcar #'+ l1 l2))
                    list-of-lists))))

;; ================================================================
;; Take a list of lists ((l1a l1b l1c) (l2a l2b l2c) (l3a l3b l3c))
;; and turn it to: ((l1a l2a l3a) (l1b l2b l3b) (l3a l3b l3c))
;; ================================================================

(defun rotate (list-of-lists)
  (apply #'mapcar #'list list-of-lists))

;; ===========================================================
;; ===========================================================

(defun caclulate-ci (list-of-values)
  (multiple-value-bind (x y)
      (stats:normal-mean-ci-on-sequence list-of-values .95)
    (list x y)))

;; ===========================================================
;; ===========================================================

(defun output-data-to-file (&optional (filename "output.csv"))
  "Print model results to file for sending to R plot function."
  (with-open-file (stream
                   (translate-logical-pathname
                    (format nil "ACT-R:models;rotation-models;~a" filename))
                   :direction :output
                   :if-exists :rename-and-delete)
    (format stream "Xvariable,Humans,Model~%")
    (loop for x in *distances*
          for y in *human-data*
          for z in (average *all-model-data*)
          do (format stream "~,2f,~,2f,~,2f~%" x y z))
    (force-output stream)))
