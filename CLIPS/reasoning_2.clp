

;;****************
;;* MODULE MAIN  *
;;****************


(defrule MAIN::start
  (declare (salience 10000))
  =>
  (set-fact-duplication TRUE)
  (focus LOCATION ASK-QUESTION RULES RATE))

;;***********************
;;* MODULE ASK-QUESTION *
;;***********************
(defmodule ASK-QUESTION (import QUESTIONS ?ALL))


(deffunction ASK-QUESTION::ask-question (?type ?question ?allowed-values)
    (bind ?answer INVALID-ANSWER)
    (switch ?type
        (case normal then
            (printout t ?question)
            (bind ?answer (read))
            (if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))
            (while (not (member$ ?answer ?allowed-values)) do
                (printout t ?question)
                (bind ?answer (read))
                (if (lexemep ?answer) then (bind ?answer (lowcase ?answer))))
            
        )

        (case range then
            (bind ?min (nth$ 1 ?allowed-values))
            (bind ?max (nth$ 2 ?allowed-values)) 
            (while (or (not (integerp ?answer)) (< ?answer ?min) (> ?answer ?max)) do
                (printout t ?question)
                (bind ?answer (explode$ (readline)))
                (if (eq (length$ ?answer) 0) then 
                    (bind ?answer nil) (bind ?empty TRUE) (break))
                (bind ?answer (nth$ 1 ?answer))
            )
            
        )
    )
    (return ?answer)

   
)


   (defrule ASK-QUESTION::ask-a-question
   ?f <- (question (already-asked FALSE)
                   (precursors)
                   (type ?type)
                   (the-question ?the-question)
                   (description ?the-attribute)
                   (valid-answers $?valid-answers))
   =>
   (modify ?f (already-asked TRUE))
   (assert (preference (type ?the-attribute)(answer (ask-question ?type ?the-question ?valid-answers))))

   )	

    (defrule ASK-QUESTION::precursor-is-satisfied
        ?f <- (question (precursors ?t is ?v $?rest))
        (preference (type ?t) (answer ?v))
        => 
        (modify ?f  (precursors ?rest))
    )


;;*****************************
;;* MODULE QUESTION-INFERENCE *
;;*****************************

(defmodule RULES (import QUESTIONS ?ALL)(import COMMON ?ALL))

   (defrule RULES::trip-budget-generic
        (preference (type trip-budget-generic) (answer si))
        =>
        (assert(attribute (name trip-budget-generic)(value si)(certainty 1.0)))
   )
   
   (defrule RULES::trip-budget-generic
        (preference (type trip-budget-generic) (answer no))
        =>
        (assert(attribute (name trip-budget-generic)(value no)(certainty 1.0)))
   )

   ;;-------TRIP LENGTH

   (defrule RULES::trip-length
        (preference (type trip-length) (answer ?s&:(and(> ?s 1)(< ?s 30))))
        =>
        (assert(attribute(name trip-length)(value ?s)(certainty 1.0)))

   )


    ;;-------TRIP BUDGET

   (defrule RULES::trip-budget-generic
        (preference (type trip-budget-generic)(answer si))
        =>
        (assert(attribute(name trip-budget-generic)(value si)(certainty 1.0)))
   )

    (defrule RULES::trip-budget-generic
        (preference (type trip-budget-generic)(answer no))
        =>
        (assert(attribute(name trip-budget-generic)(value no)(certainty 1.0)))
        (assert(attribute(name trip-budget)(value 5000)(certainty 1.0)))
   )

   (defrule RULES::trip-budget
        (preference (type trip-budget)(answer ?a&:(and(> ?a 200)(< ?a 5001))))
        =>
        (assert(attribute(name trip-budget)(value ?a)(certainty 1.0)))
   )


   ;;------MORE REGION

    (defrule RULES::trip-more-region-generic
        (preference (type trip-more-region-generic)(answer si))
        =>
        (assert(attribute(name trip-more-region-generic)(value si)(certainty 1.0)))
    )

    (defrule RULES::trip-more-region-generic
        (preference (type trip-more-region-generic)(answer no))
        =>
        (assert(attribute(name trip-more-region-generic)(value no)(certainty 1.0)))
        (assert(attribute(name trip-more-region)(value 1)(certainty 1.0)))
    )


    (defrule RULES::trip-more-region
        (preference (type trip-more-region)(answer ?a&:(and(> ?a 2)(< ?a 7))))
        =>
        (assert(attribute(name trip-more-region)(value ?a)(certainty 1.0))) 
    )

;; -------MORE LOCATION
    (defrule RULES::trip-more-location-generic
        (preference (type trip-more-location-generic)(answer si))
        =>
        (assert(attribute(name trip-more-location-generic)(value si)(certainty 1.0)))
    )

    (defrule RULES::trip-more-location-generic
        (preference (type trip-more-location-generic)(answer no))
        =>
        (assert(attribute(name trip-more-location-generic)(value no)(certainty 1.0)))
        (assert(attribute(name trip-more-location)(value 1)(certainty 1.0)))
    )


    (defrule RULES::trip-more-location
        (preference (type trip-more-location)(answer ?a&:(and(> ?a 2)(< ?a 11))))
        =>
        (assert(attribute(name trip-more-location)(value ?a)(certainty 1.0))) 
    )

;;--------------FOOD
    (defrule RULES::food
        (preference (type food)(answer ?a&:(and (> ?a 0)(< ?a 6))))
    =>
        
        (assert (attribute (name tourism-type)(value enogastronomico)(certainty (* 0.6 (/ ?a 5)))))
    )

;;--------------RELIGION
    (defrule RULES::religion
        (preference (type religion)(answer ?a&:(and (> ?a 0)(< ?a 6))))
    =>
        
        (assert (attribute (name tourism-type)(value religioso)(certainty (* 0.6 (/ ?a 5)))))
    )
;;--------------CULTURE
    (defrule RULES::culture
        (preference (type culture)(answer ?a&:(and (> ?a 0)(< ?a 6))))
    =>
       
        (assert (attribute (name tourism-type)(value culturale)(certainty (* 0.6 (/ ?a 5)))))
    )

;;--------------MOUNTAIN

    (defrule RULES::montain-yes
        (preference (type mountain)(answer si))
    =>
        (assert (attribute (name tourism-type)(value montano)(certainty 0.6)))
    )
    
    (defrule RULES::montain-no
        (preference (type mountain)(answer no))
    =>
        (assert (attribute (name tourism-type)(value montano)(certainty -1.0)))
    )

;;--------------NATURALISTIC
    (defrule RULES::naturalistic
        (preference (type naturalistic)(answer ?a&:(and (> ?a 0)(< ?a 6))))
    =>
        (assert (attribute (name tourism-type)(value naturalistico)(certainty (* 0.6 (/ ?a 5)))))
    )
;;--------------BALNEARE-LACUSTRE
    (defrule RULES::balneare
        (preference (type balneare-lacustre)(answer balneare))
    =>
        (assert (attribute (name tourism-type)(value balneare)(certainty 0.6)))
        (assert (attribute (name tourist-type)(value lacustre)(certainty 0.4)))
    )
    (defrule RULES::lacustre
        (preference (type balneare-lacustre)(answer lacustre))
    =>
        (assert (attribute (name tourism-type)(value balneare)(certainty 0.4)))
        (assert (attribute (name tourist-type)(value lacustre)(certainty 0.6)))
    )
;;--------------SPORT-TERMALE
    (defrule RULES::termale
        (preference (type sport-termale)(answer relax))
    =>
        (assert (attribute (name tourism-type)(value sportivo) (certainty 0.2)))
        (assert (attribute (name tourism-type)(value termale) (certainty 0.4)))
  
    )
       (defrule RULES::sport
        (preference (type sport-termale)(answer sport))
    =>
        (assert (attribute (name tourism-type)(value sportivo) (certainty 0.4)))
        (assert (attribute (name tourism-type)(value termale) (certainty 0.2)))
  
    )

;;--------------COSTO
    (defrule RULES::costo-economico
        (preference (type costo)(answer economico))
    =>
        (assert (attribute (name stelle-hotel)(value 1)(certainty 0.4)))
        (assert (attribute (name stelle-hotel)(value 2)(certainty 0.2)))
        (assert (attribute (name stelle-hotel)(value 3)(certainty -0.2)))
        (assert (attribute (name stelle-hotel)(value 4)(certainty -0.4)))
    )
    (defrule RULES::costo-normale 
        (preference (type costo)(answer normale))
    =>
        (assert (attribute (name stelle-hotel)(value 1)(certainty -0.2)))
        (assert (attribute (name stelle-hotel)(value 2)(certainty 0.4)))
        (assert (attribute (name stelle-hotel)(value 3)(certainty 0.4)))
        (assert (attribute (name stelle-hotel)(value 4)(certainty -0.2)))
    )
      (defrule RULES::costo-costoso
        (preference (type costo)(answer costoso))
    =>
        (assert (attribute (name stelle-hotel)(value 1)(certainty -0.4)))
        (assert (attribute (name stelle-hotel)(value 2)(certainty -0.2)))
        (assert (attribute (name stelle-hotel)(value 3)(certainty 0.2)))
        (assert (attribute (name stelle-hotel)(value 4)(certainty 0.4)))
    )
;;---------------------

    ;;(defrule trova-location
      ;;  (attribute (name ?n)(value ?t)(certainty ?a ))
      ;;  (location-tourism (location-name ?l)(tourism-type ?t)(score ?cf))
      ;;  (test (eq ?a ?cf))
    ;;=>
      ;;  (printout t " la migliore localita' per un turismo " ?t " secondo i gusti dell'utente e' " ?l crlf)
    ;;)

   


;;*********************
;;* MODULE RATE *
;;*********************

(defmodule RATE (import COMMON ?ALL) (import HOTEL ?ALL)(import LOCATION ?ALL))

(defrule rate-hotel-by-stars
    (attribute (name stelle-hotel)(value ?s)(certainty ?cf))
    (hotel (name ?h)(location ?l)(stars ?s))
=>
    (assert (attribute (name the-hotel-in ?l) (value ?h)(certainty ?cf)))
)

 (defrule rate-location-type
        (attribute (name tourism-type)(value ?v)(certainty ?a))
        (location-tourism(location-name ?l)(tourism-type ?v)(score ?cf))
        =>
        (assert(attribute(name rate-tourism-type)(value ?l)(certainty (* ?a ?cf))))  
    )


;; PATH

(defrule build-singleton-path
    (location (name ?r))
    =>
    (assert (path (locations ?r) (length 1) (total-distance 0.0)))
)

(defrule build-path
    (path (locations $?rs ?lr) (length ?len) (total-distance ?td))
    
    (attribute (name trip-length) (value ?tl))
    (test (< ?td (* ?*MAX-KM-DAY* ?tl))) ;vincolo distanza al giorno
    (test (< ?len (+ ?tl 1))) ;vincolo durata viaggio
    (loc-to-loc (location-src ?lr) (location-dst ?nr) (distance ?d)) 
    (test (eq (member$ ?nr (create$ ?rs ?lr)) FALSE))
=>
    (if (< (+ ?td ?d) (* ?*MAX-KM-DAY* ?tl)) then
        (assert (path (locations ?rs ?lr ?nr) (length (+ ?len 1)) (total-distance (+ ?td ?d))))
    )
    
)