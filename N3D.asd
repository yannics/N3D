;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package #:asdf)

(defsystem N3D
    :name "N3D"
    :author "Yann Ics"
    :licence "Copyleft 2016/2019 - all wrongs reserved"
    :maintainer "<by.cmsc@gmail.com>"
    :description "Neuromuse3Developmental -- Implementation of developmental learning <http://liris.cnrs.fr/ideal/mooc/> in a Neuromuse3 context."
    :version "1.0"
    :serial t
    :components ((:file "package")
		 (:file "N3D")
		 (:module "existence"
			  :serial t
			  :components
			  ((:file "E010")
			   (:file "E020")
                           (:file "E030")
                           (:file "E031")
			   ;(:file "E040")
			   )))
    )
