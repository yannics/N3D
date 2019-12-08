;; E020.lisp for environment instance <motivated system>

;;------------------------------------------------------------------

(in-package :N3D)

;;------------------------------------------------------------------
;;                                                      EXISTENCE020

;;; ALGORITHM MOTIVATED SYSTEM
;;; 
;01   createPrimitiveInteraction(e1, r1, -1)
;02   createPrimitiveInteraction(e2, r2, 1)
;03   experiment = e1
;04   While()
;05      if (mood = PAINED)
;06         experiment = getOtherExperiment(experiment) 
;07      if (experiment = e1)
;08         result = r1
;09      else
;10         result = r2        
;11      enactedInteraction = getInteraction(experiment, result)
;12      if (enactedInteraction.valence ≥ 0)
;13         mood = PLEASED
;14      else
;15         mood = PAINED
;16      print experiment, result, mood

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(defgeneric resultE020 (self &optional arg))
(defmethod resultE020 ((self existence) &optional arg)
	   
  ;; add here your own conditions to get result ...
  (resultE010 self)
  ;; ---
  
  )

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(defun initE020 (&optional name)

  ;; initiate existence ...
  (let ((existence
	 (initE010 name)))
    
    (add-or-get-interaction existence (add-or-get-experiment existence :label 'e1) (add-or-get-result existence :label 'r1) :valence -1)  ;; i<n>1
    (add-or-get-interaction existence (add-or-get-experiment existence :label 'e1) (add-or-get-result existence :label 'r2) :valence 1)   ;; i<n>2 
    (add-or-get-interaction existence (add-or-get-experiment existence :label 'e2) (add-or-get-result existence :label 'r1) :valence -1)  ;; i<n>3
    (add-or-get-interaction existence (add-or-get-experiment existence :label 'e2) (add-or-get-result existence :label 'r2) :valence 1)   ;; i<n>4
    
    existence)
  )

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(defgeneric stepE020 (self result))
(defmethod stepE020 ((self existence) (result function)) ;; Perform one step of a "stream of intelligence".

  ;; Algorithm
  ;; <motivated system>
  (when (equalp "pained" (existence-mood self))
    (>remanence self (pick-other-experience self (car (existence-previous self)))))

  (let ((result-self (funcall result self)))

    (if (>= (interaction-valence (interaction-exists? self (car (existence-previous self)) result-self)) 0)
	(setf (existence-mood self) "pleased")
	(setf (existence-mood self) "pained"))

    (when *verbose* (format t "~A: ~A~A ~A~&" (existence-epoch self) (label-of (get-primitive-experience (car (existence-previous self)))) (label-of result-self) (existence-mood self))))

  (incf (existence-epoch self))
  ;; ---
  
  )

;------------------------------------------------------------------

