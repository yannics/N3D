;; E030.lisp for environment instance <simple motivated system>

;;------------------------------------------------------------------

(in-package :N3D)

;;------------------------------------------------------------------
;;                                                      EXISTENCE030

;;; ALGORITHM INTERACTIVE MOTIVATED SYSTEM
;;;
;01  createPrimitiveInteraction(e1, r1, -1)
;02  createPrimitiveInteraction(e1, r2, 1)
;03  createPrimitiveInteraction(e2, r1, -1)
;04  createPrimitiveInteraction(e2, r2, 1)
;05  while()
;06     contextInteraction = enactedInteraction
;07     anticipations = anticipate(enactedInteraction)
;08     experiment = selectExperiment(anticipations)
;09     if (experiment = previousExperiment)
;10        result = r1
;11     else
;12        result = r2
;13     previousExperiment = experiment
;14     enactedInteraction = getInteraction(experiment, result)
;15     if (enactedInteraction.valence ≥ 0)
;16        mood = PLEASED
;17     else
;18        mood = PAINED
;19     learnCompositeInteraction(contextInteraction, enactedInteraction)

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; PSEUDOCODE SIMPLE VERSION
;;;
;01   function learnCompositeInteraction(contextInteraction, enactedInteraction)
;02      compositeInteraction = create new tuple(contextInteraction, enactedInteraction)
;03      if compositeInteraction already in the list of known interactions 
;04         do nothing
;05      else
;06         add compositeInteraction to the list of known interactions
;07
;08   function anticipate(enactedInteraction)
;09      for each interaction in the list of known interactions
;10          if interaction.preInteraction = enactedInteraction
;11             create new anticipation(interaction.postInteraction)
;12      return the list of anticipations
;13
;14   function selectExperiment(anticipations)
;15      sort the list anticipations by decreasing valence of their proposed interaction.
;16      if anticipation[0].interaction.valence ≥ 0
;17         return anticipation[0].interaction.experiment
;18      else
;19         return another experiment than anticipation[0].interaction.experiment

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(defgeneric resultE030 (self &optional arg))
(defmethod resultE030 ((self existence) &optional arg)  ;; arg is a post-interaction's experiment
	   
  ;; add here your own conditions to get result ...
  (if (equalp arg (get-primitive-experience (car (existence-previous self))))
      (add-or-get-result self :label 'r1)
      (add-or-get-result self :label 'r2))
  ;; ---
  
  )

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(defun initE030 (&optional name)

  ;; initiate existence ...
  (let ((existence
	 (initE020 name)))

    (setf *proclivity* nil)
    existence)
  
  )

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(defgeneric stepE030 (self result))
(defmethod stepE030 ((self existence) (result function)) ;; Perform one step of a "stream of intelligence".

  ;; Algorithm
  ;; <simple motivated system>
  (let* ((post-exp (get-primitive-experience (select-anticipate self (car (existence-previous self)))))
	 (post-res (funcall result self post-exp))
	 (enacted-interaction (add-or-get-interaction self post-exp post-res :learn nil)))

    (when *verbose* (format t "Enacted ~A~A ~A~&"
			    (label-of post-exp)
			    (label-of post-res)
			    (interaction-valence enacted-interaction)))
     
    (if (>= (interaction-valence enacted-interaction) 0)
	(setf (existence-mood self) "pleased")
	(setf (existence-mood self) "pained"))

    (when (interaction-p (car (existence-previous self)))
	(add-or-get-interaction self (car (existence-previous self)) enacted-interaction :learn t))

    (>remanence self enacted-interaction)

    (when *verbose* (format t "~A: ~A~&" (existence-epoch self) (existence-mood self)))
    
    (incf (existence-epoch self)))
  ;; ---
  
  )

;------------------------------------------------------------------
