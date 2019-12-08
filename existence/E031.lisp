;; E031.lisp for environment instance <weighted motivated system>

;;------------------------------------------------------------------

(in-package :N3D)

;;------------------------------------------------------------------
;;                                                      EXISTENCE031

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
;;; PSEUDOCODE WEIGHTED VERSION
;;;
;01   function learnCompositeInteraction(contextInteraction, enactedInteraction)
;02      compositeInteraction = create new tuple(contextInteraction, enactedInteraction)
;03      if compositeInteraction already in the list of known interactions 
;04         increment compositeInteraction's weight
;05      else
;06         add compositeInteraction to the list of known interactions with a weight of 1
;07
;08   function anticipate(enactedInteraction)
;09      for each interaction in the list of known interactions
;10         if interaction.preInteraction = enactedInteraction
;11            proposedExperiment = interaction.postInteraction.experiment
;12            proclivity = interaction.weight * interaction.postInteraction.valence
;13            if an anticipation already exists for proposedExperience 
;14               add proclivity to this anticipation's proclivity
;15            else
;16               create new anticipation(proposedExperiment) with proclivity proclivity
;17      return the list of anticipations
;18
;19   function selectExperiment(anticipations)
;20      sort the list anticipations by decreasing proclivity.
;21      if anticipation[0].proclivity ≥ 0
;22         return anticipation[0].experiment
;23      else
;24         return another experiment than anticipation[0].experiment

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(defgeneric resultE031 (self &optional arg))
(defmethod resultE031 ((self existence) &optional arg)  ;; arg is a post-interaction's experiment
	   
  ;; add here your own conditions to get result ...
  (let ((t1 8)
	(t2 15))
    (if (or (<= (existence-epoch self) t1) (> (existence-epoch self) t2))
	(if (equalp arg (get-primitive-experience (car (existence-previous self))))
	    (add-or-get-result self :label 'r1)
	    (add-or-get-result self :label 'r2))
	(if (equalp arg (get-primitive-experience (car (existence-previous self))))
	    (add-or-get-result self :label 'r2)
	    (add-or-get-result self :label 'r1))))
  ;; ---
  
  )

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(defun initE031 (&optional name)

  ;; initiate existence ...
  (let ((existence
	 (initE020 name)))

    (setf *proclivity* t)
    existence)
  
  )

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(defgeneric stepE031 (self result))
(defmethod stepE031 ((self existence) (result function)) ;; Perform one step of a "stream of intelligence".

  ;; Algorithm
  ;; <weighted motivated system>
  (stepE030 self result)
  ;; ---
  
  )

;------------------------------------------------------------------
