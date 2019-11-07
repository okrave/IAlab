
;;****************
;;* MODULE MAIN  *
;;****************


(defrule MAIN::start
  (declare (salience 10000))
  =>
  (set-fact-duplication TRUE)
  (focus ASK-QUESTION RULES))

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
                   (attribute ?the-attribute)
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

(defmodule RULES (import QUESTIONS ?ALL)(import LOCATION ?ALL))


(deftemplate RULES::attribute
   (slot name)
   (slot value)
   (slot certainty (type FLOAT) (range -1.0 1.0) (default 0.0)))


   (defrule RULES::verify
        (preference (type trip-type)(answer montagna))
        =>
        (assert(attribute(name trip-type)(value montagna)(certainty 0.4)))
        (assert(attribute(name trip-type)(value mare)(certainty -0.4)))
   )



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
        
        (assert (attribute (name turismo-enogastronomico)(value enogastronomico)(certainty ?a)))
    )

;;--------------RELIGION
    (defrule RULES::religion
        (preference (type religion)(answer ?a&:(and (> ?a 0)(< ?a 6))))
    =>
        
        (assert (attribute (name turismo-religioso)(value religioso)(certainty ?a)))
    )
;;--------------CULTURE
    (defrule RULES::culture
        (preference (type culture)(answer ?a&:(and (> ?a 0)(< ?a 6))))
    =>
       
        (assert (attribute (name turismo-culturale)(value culturale)(certainty ?a)))
    )
;;---------------------

    (defrule trova-location
        (attribute (name ?n)(value ?t)(certainty ?a ))
        (location-tourism (location-name ?l)(tourism-type ?t)(score ?cf))
        (test (eq ?a ?cf))
    =>
        (printout t " la migliore localita' per un turismo " ?t " secondo i gusti dell'utente e' " ?l crlf)
    )
