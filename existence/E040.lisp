;; E040.lisp for environment instance <recursive self-programming>

;;------------------------------------------------------------------

(in-package :N3D)

;;------------------------------------------------------------------
;;                                                      EXISTENCE040

;;; ALGORITHM RECURSIVE SELF-PROGRAMMING
;;;
;01   createPrimitiveInteraction(e1, r1, -1)
;02   createPrimitiveInteraction(e1, r2, 1)
;03   createPrimitiveInteraction(e2, r1, -1)
;04   createPrimitiveInteraction(e2, r2, 1)
;05   while()
;06      anticipations = anticipate()
;07      experiment = selectExperiment(anticipations)
;08      intendedInteraction = experiment.intendedInteraction()
;09      enactedInteraction = enact(intendedInteraction)
;10      learn()
;11      if (enactedInteraction.valence ≥ 0)
;12         mood = PLEASED
;13      else
;14         mood = PAINED
;15
;16   function enact(intendedInteraction)
;17      if intendedInteraction.isPrimitive 
;18         experiment = intendedInteraction.experiment
;19         result = environment.getResult(experiment)
;20         return primitiveInteraction(experiment, result)
;21      else
;22         enactedPreInteraction = enact(intendedInteraction.preInteraction)
;23         if (enactedPreInteraction ≠ intendedInteraction.preInteraction)
;24            return enactedPreInteraction
;25         else
;26            enactedPostInteraction = enact(intendedInteraction.postInteraction)
;27            return getOrLearn(enactedPreInteraction, enactedPostInteraction)
;28
;29   function environment.getResult(experiment)
;30      if penultimateExperiment ≠ experiment and previousExperiment = experiment
;31         return r2
;32      else
;33         return r1

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(defgeneric resultE040 (self &optional arg))
(defmethod resultE040 ((self existence) &optional arg) ;; arg is a intented-interaction's experiment
	   
  ;; add here your own conditions to get result ...
  (if (and (not (equalp args (get-mct-nth self 1)))
	   (equalp arg (get-primitive-experience-interaction (car (existence-previous self)))))
      (add-or-get-result self :label 'r2)
      (add-or-get-result self :label 'r1))
  ;; ---
  
  )

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(defun initE040 (&optional name)

  ;; initiate existence ...
  ;; Change the valence depending on the environment to obtain better behaviors ...
  (let ((existence
	 (initE020 name)))
    
    existence)
  
  )

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(defgeneric stepE040 (self result))
(defmethod stepE040 ((self existence) (result function)) ;; Perform one step of a "stream of intelligence".

  ;; Algorithm
  ;; <recursive self-programming>
  
  ;; ---
  
  )

;------------------------------------------------------------------
