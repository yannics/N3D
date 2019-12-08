;; Implementation of developmental learning
;; <http://liris.cnrs.fr/ideal/mooc/>
;; in Neuromuse3 context

;------------------------------------------------------------------

(in-package :N3D)

;------------------------------------------------------------------
;                                                  GLOBAL VARIABLES

(defvar *proclivity* nil)
(defvar *remanence* 3)  ;;  cardinal of mct (see correlation with cover-value in N3)
(defvar *verbose* t)

;------------------------------------------------------------------
;                                                      IDEAL-SYSTEM

(defstruct (experiment (:print-function (lambda (p s k) (declare (ignore k)) (format s "~A" (experiment-label p))))) label str abstract-p intended-interaction)
;; label e<n>

(defstruct (result (:print-function (lambda (p s k) (declare (ignore k)) (format s "~A" (result-label p))))) label str)
;; label r<n>

(defstruct (interaction (:print-function (lambda (p s k) (declare (ignore k)) (format s "~A" (interaction-label p))))) label experiment result valence primitive-p
	   pre-interaction post-interaction weight)
;; label i<n> 

(defstruct (existence (:print-function (lambda (p s k) (declare (ignore k)) (format s "~A" (existence-label p))))) label mood experiments results interactions
	   previous boredome-level self-satisfaction-counter epoch)
;; mood can be relative to the sensorial-rate 
;; previous is a list of experiment(s) and/or interaction(s) with a cardinal equal to *remanence*

(defstruct agent existence step result)

;...................................................................

(defmethod label-of ((self experiment))
  (experiment-label self))

(defmethod label-of ((self result))
  (result-label self))

(defmethod label-of ((self interaction))
  (interaction-label self))

(defmethod label-of ((self existence))
  (existence-label self))

;...................................................................

(defmethod add-or-get-experiment ((self existence) &key label str)
  (let ((pos (position label (existence-experiments self) :key #'label-of :from-end t)))
    (if pos
	(nth pos (existence-experiments self))
	(let ((lab (read-from-string (if (existence-experiments self)
					 (format nil "E~A" (1+ (parse-integer (remove #\E (string (label-of (car (existence-experiments self))))))))
					 (format nil "E1")))))
	  (push (setf (symbol-value lab) (make-experiment :label lab :str (if (stringp str) str (format nil "experiment ~r" (parse-integer (remove #\E (string lab))))))) (existence-experiments self))
	  (car (existence-experiments self))))))

(defmethod add-or-get-experiment ((self agent) &key label str)
  (add-or-get-experiment (agent-existence self) :label label :str str))
  
(defmethod add-or-get-result ((self existence) &key label str)
   (let ((pos (position label (existence-results self) :key #'label-of :from-end t)))
    (if pos
	(nth pos (existence-results self))
	(let ((lab (read-from-string (if (existence-results self)
					 (format nil "R~A" (1+ (parse-integer (remove #\R (string (label-of (car (existence-results self))))))))
					 (format nil "R1")))))
	  (push (setf (symbol-value lab) (make-result :label lab :str (if (stringp str) str (format nil "result ~r" (parse-integer (remove #\R (string lab))))))) (existence-results self))
	  (car (existence-results self))))))

(defmethod add-or-get-result ((self agent) &key label str)
  (add-or-get-result (agent-existence self) :label label :str str))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmethod get-primitive-experience ((self interaction))
  (if (interaction-primitive-p self)
      (interaction-experiment self)
      (get-primitive-experience (interaction-pre-interaction self))))

(defmethod get-primitive-experience ((self experiment))
  (if (experiment-abstract-p self)
      (get-primitive-experience (experiment-intented-interaction self))
      self))

(defun rnd-item (lst)
  (nth (random (length lst)) lst))

(defmethod predict ((self existence) (exp experiment))
  (let ((al (loop for i in (existence-interactions self) when (equalp exp (interaction-experiment i)) collect (interaction-result i)))) 
    (if al
	(rnd-item al)
	(rnd-item (existence-results self)))))

(defmethod predict ((self existence) (int interaction))
  (predict self (get-primitive-experience int)))

(defmethod pick-experiment ((self existence))
  (rnd-item (existence-experiments self)))

(defmethod pick-other-experience ((self existence) (exp experiment))
  (let ((tmp (pick-experiment self)))
    (loop until (not (equalp exp tmp)) do (setf tmp (pick-experiment self))) tmp))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmethod interaction-exists? ((self existence) (exp experiment) (res result))
  (let ((pos (position (list exp res)
		       (mapcar #'(lambda (x) (list
					      (interaction-experiment x)
					      (interaction-result x)))
			       (existence-interactions self))
		       :test #'equalp)))
    (when pos (nth pos (existence-interactions self)))))

(defmethod add-or-get-interaction ((self existence) (exp experiment) (res result) &key (valence 0) (learn nil))
  (let ((int (interaction-exists? self exp res)))
    (if int (progn (when learn (incf (interaction-weight int)) (when (and *verbose* *proclivity*) (format t "~S valence ~S weight ~S~&" (label-of int) (interaction-valence int) (interaction-weight int)))) int)
	(let ((lab (read-from-string (if (existence-interactions self)
					 (format nil "I~A" (1+ (parse-integer (remove #\I (string (label-of (car (existence-interactions self))))))))
					 (format nil "I1")))))
	  (push (setf (symbol-value lab) (make-interaction :label lab :experiment exp :result res :valence valence :primitive-p t :weight 0)) (existence-interactions self))
	  (car (existence-interactions self))))))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmethod interaction-exists? ((self existence) (pre interaction) (post interaction))
  (let ((pos (position (list pre post)
		       (mapcar #'(lambda (x) (list
					      (interaction-pre-interaction x)
					      (interaction-post-interaction x)))
			       (existence-interactions self))
		       :test #'equalp)))
    (when pos (nth pos (existence-interactions self)))))

(defmethod add-or-get-interaction ((self existence) (pre interaction) (post interaction) &key valence (learn nil))
  (let ((int (interaction-exists? self pre post)))
    (if int (progn (when learn (incf (interaction-weight int)) (when (and *verbose* *proclivity*) (format t "~S valence ~S weight ~S~&" (label-of int) (interaction-valence int) (interaction-weight int)))) int)
	(let ((lab (read-from-string (if (existence-interactions self)
					 (format nil "I~A" (1+ (parse-integer (remove #\I (string (label-of (car (existence-interactions self))))))))
					 (format nil "I1")))))
	  (push (setf (symbol-value lab) (make-interaction :label lab :pre-interaction pre :post-interaction post :valence (+ (interaction-valence pre) (interaction-valence post)) :weight 1)) (existence-interactions self))
	  (car (existence-interactions self))
	  (when *verbose* 
	    (format t "Learned ~{~{~S~}~}~&"
		    (list
		     (if (interaction-primitive-p pre)
			 (list (label-of (interaction-experiment pre)) (label-of (interaction-result pre)))
			 (list (label-of (interaction-pre-interaction pre)) (label-of (interaction-post-interaction pre))))
		     (if (interaction-primitive-p post)
			 (list (label-of (interaction-experiment post)) (label-of (interaction-result post)))
			 (list (label-of (interaction-pre-interaction post)) (label-of (interaction-post-interaction post)))))))))))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmethod proclivity ((self interaction))
  (* (interaction-weight self) (interaction-valence (if (interaction-primitive-p self) self (interaction-post-interaction self)))))

(defmethod select-anticipate ((self existence) (context interaction))
  (let* ((anticipations (loop for i in (existence-interactions self) when (equalp context (interaction-pre-interaction i)) collect (interaction-post-interaction i)))
	 (proposed-interaction (when anticipations (car (sort anticipations #'> :key (if *proclivity* #'proclivity #'interaction-valence))))))
    (if proposed-interaction
	(if (>= (interaction-valence proposed-interaction) 0)
	    proposed-interaction
	    (progn
	      (when *verbose* (let (a b)
				(if (interaction-primitive-p proposed-interaction)
				    (setf a (interaction-experiment proposed-interaction) b (interaction-result proposed-interaction))
				    (setf a (interaction-pre-interaction proposed-interaction) b (interaction-post-interaction proposed-interaction)))
				(if *proclivity*
				    (format t "~{propose ~A proclivity ~A~&~}"
					    (N3::flatten
					     (remove-duplicates
					      (loop for i in
						 anticipations
						 collect (list
					;(format-experiment
							  (format nil "~A~A" (if (interaction-primitive-p i) (label-of (interaction-experiment i)) (label-of (interaction-pre-interaction i))) (if (interaction-primitive-p i) (label-of (interaction-result i)) (label-of (interaction-post-interaction i))))
					;(label-of i)
					;)
							  (proclivity i)))
					     :from-end t :test #'equalp)))
				(format t "afforded ~A~A ~A~&" (label-of a) (label-of b) (interaction-valence proposed-interaction)))))
	      (select-anticipate self nil)))
	(select-anticipate self nil))))

(defmethod select-anticipate ((self existence) (context null)) ;; equivalent to get-other-interaction
  (let* ((anticipations (loop for i in (existence-interactions self) when (>= (interaction-valence i) 0) collect i))
	 (proposed-interaction (rnd-item anticipations)))
    (if (interaction-primitive-p proposed-interaction)
	proposed-interaction
	(interaction-post-interaction proposed-interaction))))

(defmethod select-anticipate ((self existence) (context experiment))
  (let ((anticipations (loop for i in (existence-interactions self) when (equalp context (interaction-experiment i)) collect i)))
    (car (sort anticipations #'> :key (if *proclivity* #'proclivity #'interaction-valence)))
    ))

;...................................................................

(defmethod get-experiment-from-interaction ((self interaction))
  (if (interaction-primitive-p self)
      (interaction-experiment self)
      (interaction-pre-interaction self)))

;; list all binary trees
(defun flat-once (lst)
  (let (r) (loop for i in lst do
		(if (listp i) (dolist (e i r) (push e r)) (push nil r)))
       (reverse r)))

;; cf. http://stackoverflow.com/questions/31874784

;def gen_trees(a,b):
;    left_trees = gen_all_trees(a)
;    right_trees = gen_all_trees(b)

;    trees = []
;    for l in left_trees:
;        for r in right_trees:
;            trees.append([l,r])
;    return trees

;def gen_all_trees(items):
;    trees = []
;    if len(items) == 1:
;        trees += [items[0]]
;    else:
;        for i in range(len(items)-1, 0, -1):
;            a = items[:i]
;            b = items[i:]
;            trees += gen_trees(a,b)
;    return trees

(defun gen-trees (a b &optional trees)
  (let ((left-trees (gen-all-trees a))
	(right-trees (gen-all-trees b)))
    (loop for l in (if (listp left-trees) (flat-once left-trees) (list left-trees))
       do
	 (loop for r in (if (listp right-trees) (flat-once right-trees) (list right-trees))
	    do
	      (push (list l r) trees)))
    trees))

(defun gen-all-trees (lst)
  (if (= 1 (length lst))
      (car lst)
      (let (trees)
	(loop for i from (1- (length lst)) downto 1
	   do
	     (push (gen-trees (subseq lst 0 i) (nthcdr i lst)) trees))
	trees)))

(defun list-bin-trees (lst)
  (flat-once (gen-all-trees lst)))

;...................................................................

(defmethod >remanence ((self existence) (in t))
  (when (or (interaction-p in) (experiment-p in))
    (push in (existence-previous self))
    (when (> (length (existence-previous self)) *remanence*)
      (setf (existence-previous self) (butlast (existence-previous self))))))

(defmethod get-mct-nth ((self existence) (nth integer))
  (if (listp (existence-previous self))
      (car (last (loop for i from 0 to nth until (null (nth i (existence-previous self))) collect (nth i (existence-previous self)))))
      (existence-previous self)))

;; ...

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;; cf. http://stackoverflow.com/questions/2535478
(defmacro sq (var value)
  `(progn
      (defvar ,var nil)
      (setq ,var ,value)))

(defmacro create-agent (name) ;; name is correlated and identified as an append word to step, result and init of existence.
  `(sq ,name (make-agent :existence (funcall (read-from-string (concatenate 'string "init" (string ',name))) ',name) :step (symbol-function (read-from-string (concatenate 'string "step" (string ',name)))) :result (symbol-function (read-from-string (concatenate 'string "result" (string ',name)))))))
  
(defmethod run-agent ((self agent) &key (duration 20) (step (agent-step self)) (result (agent-result self)))
  (dotimes (i duration) (funcall step (agent-existence self) result)))

;; ...

;; -----------------------------------------------------------------
