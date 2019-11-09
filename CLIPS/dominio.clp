;; COMMON
(defmodule COMMON (export ?ALL))

(defglobal
    ?*MAX-TOURISM-SCORE* = 5
)

(deftemplate attribute
   (multislot name)
   (slot value)
   (slot certainty (type FLOAT) (range -1.0 1.0) (default 0.0))
)



;;---------- COMBINE CERTAINTIES ------------
  
(defrule COMMON::combine-certainties-both-positive
    (declare (auto-focus TRUE))
    ?fact1 <- (attribute (name $?n) (value ?v) (certainty ?C1&:(>= ?C1 0.0)))
    ?fact2 <- (attribute (name $?n) (value ?v) (certainty ?C2&:(>= ?C2 0.0)))
    (test (neq ?fact1 ?fact2))
=>
    (retract ?fact1)
   ;; (bind ?C3 (- (+ ?C1 ?C2) (* ?C1 ?C2)))
   (bind ?C3 (+ ?C1 ?C2))
    (modify ?fact2 (certainty ?C3))
)
    
(defrule COMMON::combine-certainties-both-negative
    (declare (auto-focus TRUE))
    ?fact1 <- (attribute (name $?n) (value ?v) (certainty ?C1&:(<= ?C1 0.0)))
    ?fact2 <- (attribute (name $?n) (value ?v) (certainty ?C2&:(<= ?C2 0.0)))
    (test (neq ?fact1 ?fact2))
=>
    (retract ?fact1)
    (bind ?C3 (+ (+ ?C1 ?C2) (* ?C1 ?C2)))
    (modify ?fact2 (certainty ?C3))
)

(defrule COMMON::combine-certainties-negative-positive
    (declare (auto-focus TRUE))
    ?fact1 <- (attribute (name $?n) (value ?v) (certainty ?C1))
    ?fact2 <- (attribute (name $?n) (value ?v) (certainty ?C2))
    (test (neq ?fact1 ?fact2))
    (test (< (* ?C1 ?C2) 0.0))
=>
    (retract ?fact1)
    (bind ?C3 (/ (+ ?C1 ?C2) (- 1 (min (abs ?C1) (abs ?C2) ))))
    (modify ?fact2 (certainty ?C3))
)


;; QUESTIONS
(defmodule QUESTIONS (export ?ALL))

   
(deftemplate QUESTIONS::preference
    (slot type)
    (slot answer)
)

(deftemplate QUESTIONS::question
    (slot importance (type INTEGER)) ;; Un valore da 0-3 per indicare l'importanza della domanda la domanda 0 verrà fatta prima della domanda 3
    (slot description (default ?NONE)) ;;ho cambiato attribute in description per non confondersi con gli attribute asseriti
    (slot the-question (default ?NONE))
    (slot type (default normal))
    (multislot valid-answers (default ?NONE))
    (slot skippable (default TRUE))
    (slot already-asked (default FALSE))
    (multislot precursors (default ?DERIVE))
)

(deffacts QUESTIONS::question-list
    ;;(question (type range)(description trip-length)(importance 0) (the-question "Quanti giorni vuoi che la vacanza duri? valore tra [1,30]") (valid-answers 1 30) (skippable FALSE))
    ;;(question (description trip-budget-generic)(importance 0) (the-question "Hai un budget massimo? [Si, No]") (valid-answers Si No si no) (skippable FALSE))
    ;;(question (type range)(description trip-budget)(importance 0) (the-question "Qual'è il tuo budget? valore tra [200,5000]") (valid-answers 200 5000) (skippable FALSE)(precursors budget-limit-generic is si))
    ;;(question (description trip-more-region-generic)(importance 0) (the-question "Vuoi visitare più regioni? [Si, No]") (valid-answers Si No si no) (skippable FALSE))
    ;;(question (type range)(description trip-more-region)(importance 0) (the-question "Quante regioni vorresti visitare? valore tra [2,6]") (valid-answers  2 6) (skippable FALSE)(precursors trip-more-region-generic is si))
    ;;(question (description trip-more-location-generic) (importance 0) (the-question "Vuoi visitare più location? [Si,No]") (valid-answers Si No si no) (skippable FALSE))
    ;;(question (description trip-more-location)(importance 0) (the-question "Quante location vorresti visitare? [3,4,5,6,7,8,9,10]") (valid-answers  3 4 5 6 7 8 9 10) (skippable FALSE)(precursors trip-more-location-generic is si))
    ;;
    ;;(question (type range)(attribute people-number)(importance 0)(the-question "Quante persone vogliono andare in vacanza? tra [2,10] ")(valid-answers 2 10)(skippable FALSE))
    (question (type range)(description food)(importance 0)(the-question "Quanto è importante per te il buon cibo? tra [1,5] ")(valid-answers 1 5)(skippable FALSE))
    (question (type range)(description religion)(importance 0)(the-question "Quanto è importante per te l'aspetto religioso di una località? tra [1,5]")(valid-answers 1 5)(skippable FALSE))
    (question (type range)(description culture)(importance 0)(the-question "Quanto è importante per te l'aspetto culturale di una località? tra [1,5]")(valid-answers 1 5)(skippable FALSE))
    (question (description mountain)(importance 0)(the-question " Ti piacciono i luoghi montani? [si,no] " )(valid-answers si no)(skippable FALSE))
    (question (type range)(description naturalistic)(importance 0)(the-question " Quanto dai importanza all'aspetto naturalistico di una localita' ? tra [1,5] ")(valid-answers 1 5))
    (question (description balneare-lacustre)(importance 0)(the-question " Preferisci un turismo balneare o di tipo lacustre ? [balneare lacustre]" )(valid-answers balneare lacustre))
    (question (description sport-termale)(importance 0)(the-question " In vacanza vuoi rilassarti o mantenerti attivo facendo sport ? [relax sport] ")(valid-answers relax sport))
    (question (description costo)(importance 0)(the-question " Vuoi fare un viaggo economico o più costoso? [economico normale costoso] ")(valid-answers economico normale costoso)(skippable FALSE))
)

;;MODULE LOCATION

(defmodule LOCATION (export ?ALL))

(deftemplate location
    (slot name (default ?NONE))
    (slot region (default ?NONE))
)

(deftemplate loc-to-loc
    (slot location-src (default ?NONE))
    (slot location-dst (default ?NONE))
    (slot distance (type FLOAT))
)

(deftemplate location-tourism
    (slot location-name (default ?NONE))
    (slot tourism-type (default ?NONE))
    (slot score(type INTEGER) (range 1 5))
)

(deftemplate provalocation
    (slot name (default ?NONE))
    (slot region (default ?NONE))
    (slot altitude (type FLOAT))
    (slot longitude (type FLOAT))
)

(deffacts location-list
    (location (name agrigento) (region sicilia))
    (location (name palermo) (region sicilia))
    (location (name catania) (region sicilia))
    (location (name reggio) (region calabria))
    (location (name salerno) (region calabria))
    (location (name scilla) (region calabria))
    (location (name bari) (region puglia))
    (location (name lecce) (region puglia))
    (location (name brindisi) (region puglia))
    (location (name roma) (region lazio))
    (location (name latina) (region lazio))
    (location (name frosinone) (region lazio))
    (location (name pisa) (region toscana))
    (location (name siena) (region toscana))
    (location (name lucca) (region toscana))
    (location (name genova) (region liguria))    
    (location (name savona) (region liguria))    
    (location (name laspezia) (region liguria))    
    (location (name milano) (region lombardia))    
    (location (name pavia) (region lombardia))    
    (location (name bergamo) (region lombardia))    
    (location (name asti) (region piemonte)) 
    (location (name torino) (region piemonte)) 
    (location (name pinerolo) (region piemonte))   
    
)

(deffacts location-tourism-list
    (location-tourism (location-name agrigento)(tourism-type balneare)(score 5))
    (location-tourism (location-name agrigento)(tourism-type naturalistico)(score 3))
    (location-tourism (location-name agrigento)(tourism-type culturale)(score 5))
    (location-tourism (location-name agrigento)(tourism-type enogastronomico)(score 4))
    (location-tourism (location-name palermo)(tourism-type balneare)(score 4))
    (location-tourism (location-name palermo)(tourism-type naturalistico)(score 3))
    (location-tourism (location-name palermo)(tourism-type culturale)(score 4))
    (location-tourism (location-name palermo)(tourism-type religioso)(score 5))
    (location-tourism (location-name palermo)(tourism-type enogastronomico)(score 5))

)



(deffacts provalista-location
    (provalocation (name agrigento)(region sicilia) (altitude 37.31) (longitude 12.58))
    (provalocation (name palermo)(region sicilia) (altitude 38.14) (longitude 13.31))
)

(defrule calcolo-distance
    (provalocation (name ?n)(region ?r)(altitude ?a)(longitude ?l))
    (provalocation (name ?n1&:(neq ?n1 ?n))(region ?r1)(altitude ?a1)(longitude ?l1))
    =>

    (assert(loc-to-loc(location-src ?n)(location-dst ?n1)(distance (sqrt(- (** ?l 2) (** ?l1 2)))))) 
    (assert(loc-to-loc(location-src ?n1)(location-dst ?n)(distance (sqrt(- (** ?l 2) (** ?l1 2))))))

)

;;MODULE HOTEL

(defmodule HOTEL (export ?ALL))

(deftemplate HOTEL::hotel
    (slot name (default ?NONE))
    (slot location (default ?NONE))
    (slot stars (type INTEGER) (range 1 4))
    (slot empty (type INTEGER) (range 0 ?VARIABLE))
    (slot capacity (type INTEGER) (range 1 ?VARIABLE))
)

(deffacts list-hotel
    (hotel (name morandi)(location agrigento)(stars 3)(empty 100)(capacity 300))
    (hotel (name empedocle)(location agrigento)(stars 4)(empty 0)(capacity 400))
)
;; MODULE TRIP

(defmodule TRIP (export ?ALL))

(deftemplate TRIP::trip
    (multislot locations)
    (multislot hotels)
    (slot cost (type INTEGER) (range 0 ?VARIABLE))
    (slot days (type INTEGER) (range 0 ?VARIABLE))
    (slot tot-dist (type INTEGER) (range 0 ?VARIABLE))    
)


