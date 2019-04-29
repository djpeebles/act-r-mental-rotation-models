;; ===========================================================
;;; The coord-list is a two-element list of an x and y
;;; coordinate and the tmatrix is the transform matrix
;; ===========================================================

(defparameter *pattern-rotation-centre* '(75 75))

;; ===========================================================
;; ===========================================================

(defun add-ones (lst)
  "Add 1 to each coordinate list"
  (let ((new-list nil))
    (loop for ls in lst
          do (push (append ls '(1)) new-list))
    (reverse new-list)))

(defun deg-to-rad (degrees)
  "Convert degrees to radians"
  (* pi (/ degrees 180.0)))

(defun rad-to-deg (radians)
    "Convert radians to degrees"
  (* 180.0 (/ radians pi)))

(defun remove-final-zeros (coord-list)
  (mapcar #'butlast coord-list))

(defun round-to (number places)
  "Round a number to places decimal places."
  (/ (round number (/ 1 (expt 10 places))) (expt 10 places) 1.0))

(defun compute-distance (coord-list1 coord-list2)
  "Return Euclidean distance between two x-y coordinate lists."
  (let ((dist 0.0))
    (setf dist (sqrt (+ (expt (- (first coord-list1) (first coord-list2)) 2)
                        (expt (- (second coord-list1) (second coord-list2)) 2))))
    (float dist)))


;(defun compute-distance (coord-list1 coord-list2)
;  "Return Euclidean distance between two x-y coordinate lists."
;  (let ((dist 0.0))
;    (setf dist (sqrt (+ (expt (- (first coord-list1) (first coord-list2)) 2)
;                        (expt (- (second coord-list1) (second coord-list2)) 2))))
;    (float (round-to dist 2))))

(defun dot-product (a b)
  (reduce #'+ (map 'simple-vector #'* a b)))

;; =======================================================================
;; Compute the angle (in degrees) between two points relative to an origin
;; e.g., (rad-to-deg (acos (get-angle '(7 1) '(5 5) '(0 0))))
;; Vector dot product = (+ (* v1xdist v2xdist) (* v1ydist v2ydist))
;; =======================================================================

;(defun angular-displacement (coord1 coord2 origin)
;  (let ((vector-dot-product (+ (* (- (first coord1) (first origin))
;                                  (- (first coord2) (first origin)))
;                               (* (- (second coord1) (second origin))
;                                  (- (second coord2) (second origin)))))
;        (vector-magnitude1 (compute-distance origin coord1))
;        (vector-magnitude2 (compute-distance origin coord2)))
;    (rad-to-deg (acos (/ vector-dot-product
;                         (* vector-magnitude1 vector-magnitude2))))))

;; NOTE.  This version checks to see if the output is in complex format and if
;; so, just returns the real part.

(defun angular-displacement-check (coord1 coord2 origin)
  (let* ((vector-dot-product (+ (* (- (first coord1) (first origin))
                                   (- (first coord2) (first origin)))
                                (* (- (second coord1) (second origin))
                                   (- (second coord2) (second origin)))))
         (vector-magnitude1 (compute-distance origin coord1))
         (vector-magnitude2 (compute-distance origin coord2))
         (displacement (rad-to-deg (acos (/ vector-dot-product
                                            (* vector-magnitude1 vector-magnitude2))))))
    (if (complexp displacement)
        (setf displacement (realpart displacement)))
    displacement))

(defun angular-displacement (coord1 coord2 origin)
  (let ((vector-dot-product (+ (* (- (first coord1) (first origin))
                                  (- (first coord2) (first origin)))
                               (* (- (second coord1) (second origin))
                                  (- (second coord2) (second origin)))))
        (vector-magnitude1 (compute-distance origin coord1))
        (vector-magnitude2 (compute-distance origin coord2)))
    (rad-to-deg (acos (round-to (/ vector-dot-product
                                   (* vector-magnitude1 vector-magnitude2)) 5)))))


(defun angular-disparity (current-points target-points)
  (let* ((diffs (loop for cp in current-points
                      for tp in target-points
                      collect (angular-displacement cp tp *pattern-rotation-centre*)))
         (mean-diff (round-to (stats:mean (flatten diffs)) 0)))
    mean-diff))

;;; ==================================================
;;; The coord-list is a two-element list of an x and y
;;; coordinate and the tmatrix is the transform matrix
;;; ==================================================

(defun multiply-coords-and-matrix (coord-list tmatrix &optional (places 1))
  (loop for row in tmatrix
        collect (round-to (apply '+ (mapcar #'* coord-list row)) places)))

;; ===========================================================
;; ===========================================================

(defun rotate-around-point (original-points locx locy radians)
  (let ((tmatrix `((,(cos radians) ,(* -1 (sin radians)) ,(+ (* -1 locx (cos radians)) locx (* locy (sin radians))))
                   (,(sin radians) ,(cos radians) ,(+ (* -1 locx (sin radians)) (* -1 locy (cos radians)) locy))
                   (0 0 1)))
        (tm nil)
        (result-list nil)
        (final-result-list nil))
    (loop for coords in (add-ones original-points)
          do (setf tm tmatrix)
             (push (multiply-coords-and-matrix coords tm) result-list))
    (setf final-result-list (reverse (remove-final-zeros result-list)))
    final-result-list))

;; ===========================================================
;; ===========================================================

(defun rotate-clockwise-around-point (orig-pts degrees)
  (let ((radians (* -1 (deg-to-rad degrees))))
    (rotate-around-point orig-pts
                         (first *pattern-rotation-centre*)
                         (second *pattern-rotation-centre*) radians)))

(defun rotate-counter-clockwise-around-point (orig-pts degrees)
  (let ((radians (deg-to-rad degrees)))
    (rotate-around-point orig-pts
                         (first *pattern-rotation-centre*)
                         (second *pattern-rotation-centre*) radians)))

;;; ==========================================================
;;; ==========================================================

(defun generate-random-coordinate-point ()
  "Return a list of two random coordinate points"
  (list (round-to (+ 50 (random 50.0)) 1) (round-to (+ 50 (random 50.0)) 1)))

(defun create-coordinate-points-old (npoints)
  (let ((coord-list nil))
    (loop repeat npoints
          do (push (generate-random-coordinate-point) coord-list))
    (list coord-list)))

(defun create-coordinate-points (npoints)
  (let ((coord-list nil))
    (loop repeat npoints
          do (push (generate-random-coordinate-point) coord-list))
    coord-list))
