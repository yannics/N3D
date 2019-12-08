;; E010.lisp for environment instance <embodied system>

;;------------------------------------------------------------------

(in-package :N3D)

;;------------------------------------------------------------------
;;                                                      EXISTENCE010

;;; ALGORITHM EMBODIED SYSTEM
;;; 
;01   experiment = e1
;02   Loop(cycle++)
;03      if (mood = BORED)
;04         selfSatisfiedDuration = 0
;05         experiment = pickOtherExperiment(experiment)
;06      anticipatedResult = anticipate(experiment)
;07      if (experiment = e1)
;08         result = r1
;09      else
;10         result = r2
;11      recordTuple(experiment, result)
;12      if (result = anticipatedResult)
;13         mood = SELF-SATISFIED
;14         selfSatisfiedDuration++
;15      else
;16         mood = FRUSTRATED
;17         selfSatisfiedDuration = 0
;18      if (selfSatisfiedDuration > 3)
;19         mood = BORED
;20      print cycle, experiment, result, mood

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(defgeneric resultE010 (self &optional arg))
(defmethod resultE010 ((self existence) &optional arg)
  
  ;; add here your own conditions to get result ...
  (if (equalp (get-primitive-experience (car (existence-previous self))) (add-or-get-experiment self :label 'e1))
      (add-or-get-result self :label 'r1)
      (add-or-get-result self :label 'r2))
  ;; ---
  
  )

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(defun initE010 (&optional name)

  ;; initiate existence ...
  (let ((existence
	 (make-existence
	  :label name
	  :boredome-level 3
	  :self-satisfaction-counter 0
	  :epoch 0)))

    (add-or-get-experiment existence) ;; e<n>1
    (add-or-get-experiment existence) ;; e<n>2
    (add-or-get-result existence)     ;; r<n>1
    (add-or-get-result existence)     ;; r<n>2

    (>remanence existence (add-or-get-experiment existence :label 'e1))
    
    existence)
  
  )

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(defgeneric stepE010 (self result))
(defmethod stepE010 ((self existence) (result function)) ;; Perform one step of a "stream of intelligence"
  
  ;; Algorithm
  ;; <embodied system>
  (when (equalp "bored" (existence-mood self))
    (>remanence self (pick-other-experience self (get-primitive-experience (car (existence-previous self)))))
    (setf (existence-self-satisfaction-counter self) 0))
  
  (let ((anticipate-result (predict self (car (existence-previous self))))
	(result-self (funcall result self)))
    
    (add-or-get-interaction self (get-primitive-experience (car (existence-previous self))) result-self)
    
    (if (equalp anticipate-result result-self)
	(progn
	  (setf (existence-mood self) "self-satisfied")
	  (incf (existence-self-satisfaction-counter self)))
	(progn
	  (setf (existence-mood self) "frustrated")
	  (setf (existence-self-satisfaction-counter self) 0)))
    
    (when (> (existence-self-satisfaction-counter self) (existence-boredome-level self))
      (setf (existence-mood self) "bored"))
    
    (when *verbose* (format t "~A: ~A~A ~A~&" (existence-epoch self) (label-of (get-primitive-experience (car (existence-previous self)))) (label-of result-self) (existence-mood self))))

  (incf (existence-epoch self))
  ;; ---

  )

;;------------------------------------------------------------------
