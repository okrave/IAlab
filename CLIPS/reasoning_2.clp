
(defmodule MAIN (export ?ALL))



(deftemplate question
    (slot importance (type INTEGER)) ;; Un valore da 0-3 per indicare l'importanza della domanda la domanda 0 verrà fatta prima della domanda 3
    (slot attribute (default ?NONE))
    (slot the-question (default ?NONE))
    (slot type (default normal))
    (multislot valid-answers (default ?NONE))
    (slot skippable (default TRUE))
    (slot already-asked (default FALSE))
    (multislot precursors (default ?DERIVE))
)

(deffacts question-list
    (question (type range)(attribute trip-length)(importance 0) (the-question "Quanti giorni vuoi che la vacanza duri? valore tra [1,30]") (valid-answers 1 30) (skippable FALSE))
    (question (attribute trip-budget-generic)(importance 0) (the-question "Hai un budget massimo? [Si, No]") (valid-answers Si No si no) (skippable FALSE))
    (question (type range)(attribute trip-budget)(importance 0) (the-question "Qual'è il tuo budget? valore tra [200,5000]") (valid-answers 200 5000) (skippable FALSE)(precursors budget-limit-generic is si))
    (question (attribute trip-more-region-generic)(importance 0) (the-question "Vuoi visitare più regioni? [Si, No]") (valid-answers Si No si no) (skippable FALSE))
    (question (type range)(attribute trip-more-region)(importance 0) (the-question "Quante regioni vorresti visitare? valore tra [2,6]") (valid-answers  2 6) (skippable FALSE)(precursors trip-more-region-generic is si))
    (question (attribute trip-more-location-generic) (importance 0) (the-question "Vuoi visitare più location? [Si,No]") (valid-answers Si No si no) (skippable FALSE))
    (question (attribute trip-more-location)(importance 0) (the-question "Quante location vorresti visitare? [3,4,5,6,7,8,9,10]") (valid-answers  3 4 5 6 7 8 9 10) (skippable FALSE)(precursors trip-more-location-generic is si))
    (question (attribute trip-type)(importance 0) (the-question "Quale tipologia di viaggio vuoi fare? [Montagna, Mare]") (valid-answers  montagna mare) (skippable FALSE))

)

(deftemplate MAIN::attribute
   (slot name)
   (slot value)
   (slot certainty (type FLOAT) (range -1.0 1.0) (default 0.0)))
   
(deftemplate MAIN::preference
    (slot type)
    (slot answer)
)

(deffunction MAIN::ask-question (?type ?question ?allowed-values)
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


   (defrule MAIN::ask-a-question
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

    (defrule MAIN::precursor-is-satisfied
        ?f <- (question (precursors ?t is ?v $?rest))
        (preference (type ?t) (answer ?v))
        => 
        (modify ?f  (precursors ?rest))
    )




   (defrule MAIN::verify
        (preference (type trip-type)(answer montagna))
        =>
        (assert(attribute(name trip-type)(value montagna)(certainty 0.4)))
        (assert(attribute(name trip-type)(value mare)(certainty -0.4)))
   )



   (defrule MAIN::trip-budget-generic
        (preference (type trip-budget-generic) (answer si))
        =>
        (assert(attribute (name trip-budget-generic)(value si)(certainty 1.0)))
   )
   
   (defrule MAIN::trip-budget-generic
        (preference (type trip-budget-generic) (answer no))
        =>
        (assert(attribute (name trip-budget-generic)(value no)(certainty 1.0)))
   )

   ;;-------TRIP LENGTH

   (defrule MAIN::trip-length
        (preference (type trip-length) (answer ?s&:(and(> ?s 1)(< ?s 30))))
        =>
        (assert(attribute(name trip-length)(value ?s)(certainty 1.0)))

   )


    ;;-------TRIP BUDGET

   (defrule MAIN::trip-budget-generic
        (preference (type trip-budget-generic)(answer si))
        =>
        (assert(attribute(name trip-budget-generic)(value si)(certainty 1.0)))
   )

    (defrule MAIN::trip-budget-generic
        (preference (type trip-budget-generic)(answer no))
        =>
        (assert(attribute(name trip-budget-generic)(value no)(certainty 1.0)))
        (assert(attribute(name trip-budget)(value 5000)(certainty 1.0)))
   )

   (defrule MAIN::trip-budget
        (preference (type trip-budget)(answer ?a&:(and(> ?a 200)(< ?a 5001))))
        =>
        (assert(attribute(name trip-budget)(value ?a)(certainty 1.0)))
   )


   ;;------MORE REGION

    (defrule MAIN::trip-more-region-generic
        (preference (type trip-more-region-generic)(answer si))
        =>
        (assert(attribute(name trip-more-region-generic)(value si)(certainty 1.0)))
    )

    (defrule MAIN::trip-more-region-generic
        (preference (type trip-more-region-generic)(answer no))
        =>
        (assert(attribute(name trip-more-region-generic)(value no)(certainty 1.0)))
        (assert(attribute(name trip-more-region)(value 1)(certainty 1.0)))
    )


    (defrule MAIN::trip-more-region
        (preference (type trip-more-region)(answer ?a&:(and(> ?a 2)(< ?a 7))))
        =>
        (assert(attribute(name trip-more-region)(value ?a)(certainty 1.0))) 
    )

;; -------MORE LOCATION
        (defrule MAIN::trip-more-location-generic
        (preference (type trip-more-location-generic)(answer si))
        =>
        (assert(attribute(name trip-more-location-generic)(value si)(certainty 1.0)))
    )

    (defrule MAIN::trip-more-location-generic
        (preference (type trip-more-location-generic)(answer no))
        =>
        (assert(attribute(name trip-more-location-generic)(value no)(certainty 1.0)))
        (assert(attribute(name trip-more-location)(value 1)(certainty 1.0)))
    )


    (defrule MAIN::trip-more-location
        (preference (type trip-more-location)(answer ?a&:(and(> ?a 2)(< ?a 11))))
        =>
        (assert(attribute(name trip-more-location)(value ?a)(certainty 1.0))) 
    )



