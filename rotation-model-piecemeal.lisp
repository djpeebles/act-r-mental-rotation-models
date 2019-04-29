;;;; =========================================================
;;;;
;;;;
;;;; =========================================================
;; load file containing statistics functions

(defparameter *distances* '(0 20 40 60 80 100 120 140 160 180))
(defparameter *human-data* '(2.35 2.50 2.90 3.70 5.40 5.55 5.20 5.10 7.35 6.55))

;;; holistic-human 1.65 2.10 2.30 2.85 3.05 4.20 3.85 3.85 5.20 4.75
;;; piecemeal-human 2.35 2.50 2.90 3.70 5.40 5.55 5.20 5.10 7.35 6.55

;;; holistic-best-fit 1.966 2.375 2.783 3.192 3.601 4.009 4.418 4.826 5.235 5.644
;;; piecemeal-best-fit 2.723 3.288 3.854 4.420 4.986 5.552 6.117 6.683 7.249 7.815

(actr-load "ACT-R:models;rotation-models;package.fasl")
(actr-load "ACT-R:models;rotation-models;lhstats.fasl")
(actr-load "ACT-R:models;rotation-models;shared-code.lisp")

(defparameter *rotation-increment* 8)
(defparameter *initial-rotation* 0)
(defparameter *pattern-count-threshold* 2)

;;; =====================================================================
;;; x and y coordinate points, height and width, for the sub-features and
;;; compound shape.
;;; =====================================================================

(defparameter *target-image-dimensions* '((100 100 50 50) (151 100 50 50)
                                          (100 151 50 50) (151 151 50 50)
                                          (100 100 100 100)))

(defparameter *rotated-image-dimensions* '((400 400 50 50) (451 400 50 50)
                                           (400 451 50 50) (451 451 50 50)
                                           (400 400 100 100)))

(actr-load "ACT-R:models;rotation-models;imagery-functions.lisp")
(actr-load "ACT-R:models;rotation-models;rotation-stimuli.lisp")

(defparameter *proximity-threshold* 12.0 "How close does the angle have to be to stop.")
(defvar *imaginal-delay-time* 0.1)
(defparameter *perceptual-noise* 2)

;;; ==========================================================
;;;
;;; ==========================================================

(defun transform-image (slot-to-change)
  (let ((delay (randomize-time *imaginal-delay-time*)))
    (schedule-mod-buffer-chunk 'imaginal
                               (list slot-to-change
                                     (rotate-counter-clockwise-around-point
                                      (chunk-slot-value-fct (buffer-read 'imaginal) slot-to-change)
                                      (+ *rotation-increment*
                                         (act-r-noise *perceptual-noise*))))
                               delay)
    (schedule-event-relative delay 'set-imaginal-free)))

;;; ==========================================================
;;; ==========================================================

(defun run-model (initial-rotation)
  (let* ((target-patterns (create-stimulus-patterns))
         (target-image (create-target-image *target-image-dimensions* target-patterns))
         (rotated-image (create-rotated-image target-image 'clockwise initial-rotation)))
    (delete-all-visicon-features)
    (mapcar #'add-visicon-features target-image)
    (mapcar #'add-visicon-features rotated-image)
    (setf *start-time* (mp-time))
    (run 10)))

;;; ==========================================================
;;; ==========================================================

(defun runsim (nsubjects)
  (let ((final-averages nil))
    (add-act-r-command "record-time" 'record-time "Save the current mp-time.")
    (monitor-act-r-command "output-key" "record-time")
    (dotimes (subj nsubjects)
      (format t "Subject: ~A~%" subj)
      (setf *model-data* nil)
      (loop for angle in *distances*
            do (format t "Angle: ~A~%" angle)
               (reset)
               (install-device '("motor" "keyboard"))
               (run-model angle))
      (setf *model-data* (reverse *model-data*))
      (push *model-data* *all-model-data*))
    (remove-act-r-command-monitor "output-key" "record-time")
    (remove-act-r-command "record-time")
    (format t "Human mean RTs:~A~%" *human-data*)
    (format t "Model RTs: ~A~%" *all-model-data*)
    (setf final-averages (average *all-model-data*))
    (format t "Model RTs averaged over ~A subjects:~%~A~%" nsubjects final-averages)
    (format t "95% Confidence Intervals:~%~A~%" (map 'list #'caclulate-ci (rotate *all-model-data*)))
    (correlation *human-data* final-averages)
    (mean-deviation *human-data* final-averages)
    (output-data-to-file "rotation-output.csv")
    (sb-ext:run-program
     "/usr/bin/Rscript"
     (list "/home/david/Dropbox/actr/actr7-12-10/models/rotation-models/rotation-plots.R") :input t)))

;;; ==========================================================
;;; ==========================================================

(define-model piecemeal-rotation

  (sgp :v nil
       :trace-detail low
       :esc t
       :er t
       :randomize-time t)

  (chunk-type rotation-goal state xloc pattern-count)
  (chunk-type (polygon-feature (:include visual-location)) points centre-x centre-y regular)
  (chunk-type (polygon (:include visual-object) (:include polygon-feature)) sides (polygon t) regular points centre-x centre-y)
  (chunk-type (square (:include polygon)) (sides 4) (square t))
  (chunk-type (compound-square (:include square)) sub1 sub2 sub3 sub4)
  (chunk-type imagery-chunk current-image current-color)

  (define-chunks (true isa chunk) (false isa chunk) (square isa chunk) (rotate isa chunk)
    (polygon isa chunk) (compound isa chunk) (start isa chunk) (stop isa chunk) (compare isa chunk)
    (g1 isa rotation-goal state start pattern-count 0) (encode isa chunk) (process-pattern isa chunk)
    (pattern-count isa chunk) (patterns-the-same? isa chunk) (encode-rotated-pattern isa chunk)
    (process-rotated-pattern isa chunk) (encode-normal-pattern isa chunk) (attend-normal-pattern isa chunk))

  (goal-focus g1)
  (declare-buffer-usage imaginal imagery-chunk :all)

;;; =================================================================
;;; IF   you're starting a trial,
;;; THEN look for a rotated image element on the right of the screen.
;;; =================================================================

  (p start-trial
    =goal>
     state start
   ==>
    +visual-location>
     isa visual-location
     > screen-x 300
     - color compound
     :attended nil
    =goal>
     state encode-rotated-pattern)

;;; ============================================
;;; If   there's a rotated image element on the screen,
;;; THEN attend to it.
;;; AND  start to look for the same pattern in the normal image
;;; ============================================

  (p attend-to-rotated-pattern
    =goal>
     state encode-rotated-pattern
    =visual-location>
     color =col
     screen-x =screenx
    ?visual>
     state free
   ==>
    +visual>
     isa move-attention
     screen-pos =visual-location
    !eval! (format t "Colour: ~A~%" =col)
    =goal>
     state process-rotated-pattern
     xloc =screenx)

;;; ========================================================================
;;; If   you're looking at the rotated image at the right of the screen,
;;; THEN store the points representing the global pattern in working memory,
;;; AND  look for the target compound image at the left of the screen.
;;; ========================================================================

  (p store-rotated-pattern-and-look-for-normal-pattern
    =goal>
     state process-rotated-pattern
     xloc =screenx
    =visual>
     points =points
     color =col
    ?imaginal>
     state free
    !eval! (> =screenx 300)
   ==>
    +imaginal>
     isa imagery-chunk
     current-image =points
     current-color =col
    +visual-location>
     isa visual-location
     < screen-x 300
     - color compound
    =goal>
    state encode-normal-pattern)

;;; ============================================
;;;
;;;
;;; ============================================

(P attend-to-normal-pattern
  =goal>
   state encode-normal-pattern
  =visual-location>
  ?visual>
   state free
  =imaginal>
   current-color =col
 ==>
  +visual>
   cmd move-attention
   screen-pos =visual-location
  =goal>
   state attend-normal-pattern
   =imaginal>
   =visual-location>)

;;; ============================================
;;;
;;;
;;; ============================================

(p different-normal-pattern
  =goal>
   state attend-normal-pattern
  =imaginal>
   current-color =col
  =visual>
   color =actual-col
   - color =col
 ==>
  !eval! (format t "Different colour: ~A~%" =actual-col)
  +visual-location>
   < screen-x 300
   :attended nil
   :nearest current
   - color compound
   =goal>
    state encode-normal-pattern
   =imaginal>)

;;; ============================================
;;;
;;;
;;; ============================================

(p same-normal-pattern
    =goal>
     state attend-normal-pattern
    =visual-location>
     screen-x =screenx
    =imaginal>
     current-color =col
    =visual>
     color =col
     points =target
   ==>
    !eval! (format t "Same colour: ~A Points: ~A~%" =col =target)
    =goal>
     state process-pattern
     xloc =screenx
     =imaginal>
     =visual>)

;;; ====================================================================
;;; IF   you're looking at the target pattern at the left of the screen,
;;; AND  you have the pattern of the rotated image in working memory,
;;; AND  the angular displacement between them is too large
;;; THEN rotate the pattern closer to the target.
;;; ====================================================================

  (p target-not-reached-so-move
    =goal>
     state process-pattern
     xloc =screenx
    =visual>
     points =target
    =imaginal>
     current-image =current
    !eval! (< =screenx 300)
    !bind! =angle-disp (angular-disparity =current =target)
    !eval! (> =angle-disp *proximity-threshold*)
    ?imaginal-action>
     state free
   ==>
    =visual>
    =imaginal>
    +imaginal-action>
     action transform-image
     slots (current-image))

;;; ====================================================================
;;; IF   you're looking at the target pattern at the left of the screen,
;;; AND  you have the pattern of the rotated image in working memory,
;;; AND  the angular displacement between them is sufficiently small
;;; THEN stop.
;;; ====================================================================

  (p target-reached-so-stop
    =goal>
     state process-pattern
     xloc =screenx
     pattern-count =pcount
    =visual>
     points =target
    =imaginal>
     current-image =current
    !eval! (< =screenx 300)
    !bind! =angle-disp (angular-disparity =current =target)
    !bind! =new-pcount (+ =pcount 1)
    !eval! (<= =angle-disp *proximity-threshold*)
   ==>
    =goal>
     state patterns-the-same?
     pattern-count =new-pcount
    !eval! (format t "Pattern Count: ~A~%" =new-pcount))

;;; ====================================================================
;;; IF
;;; AND
;;; AND
;;; THEN
;;; ====================================================================

  (p pattern-count-reached-so-stop
    =goal>
     state patterns-the-same?
     pattern-count 2
    ?manual>
     state free
     ==>
    !eval! (setf *rotation-increment* 8)
    =goal>
     state stop
    +manual>
     cmd press-key
     key "m")

;;; ====================================================================
;;; IF
;;; AND
;;; AND
;;; THEN
;;; ====================================================================

  (p pattern-count-not-reached-so-keep-going
    =goal>
     state patterns-the-same?
     - pattern-count 2
     ==>
    !eval! (setf *rotation-increment* 18)
    =goal>
     state start)

  )
