;; COMMON
(defmodule COMMON (export ?ALL))

(defglobal
    ?*MAX-TOURISM-SCORE* = 5
    ?*MAX-KM-DAY* = 100
    ?*HOTEL-BASE-COST* = 50
    ?*HOTEL-ADDITIONAL-COST* = 25
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
    (question (type range)(description trip-length)(importance 0) (the-question "Quanti giorni vuoi che la vacanza duri? valore tra [1,30]") (valid-answers 1 30) (skippable FALSE))
    ;;(question (description trip-budget-generic)(importance 0) (the-question "Hai un budget massimo? [Si, No]") (valid-answers Si No si no) (skippable FALSE))
    ;;(question (type range)(description trip-budget)(importance 0) (the-question "Qual'è il tuo budget? valore tra [200,5000]") (valid-answers 200 5000) (skippable FALSE)(precursors budget-limit-generic is si))
    ;;(question (description trip-more-region-generic)(importance 0) (the-question "Vuoi visitare più regioni? [Si, No]") (valid-answers Si No si no) (skippable FALSE))
    ;;(question (type range)(description trip-more-region)(importance 0) (the-question "Quante regioni vorresti visitare? valore tra [2,6]") (valid-answers  2 6) (skippable FALSE)(precursors trip-more-region-generic is si))
    ;;(question (description trip-more-location-generic) (importance 0) (the-question "Vuoi visitare più location? [Si,No]") (valid-answers Si No si no) (skippable FALSE))
    ;;(question (description trip-more-location)(importance 0) (the-question "Quante location vorresti visitare? [3,4,5,6,7,8,9,10]") (valid-answers  3 4 5 6 7 8 9 10) (skippable FALSE)(precursors trip-more-location-generic is si))
    ;;
    (question (type range)(description people-number)(importance 0)(the-question "Quante persone vogliono andare in vacanza? tra [2,10] ")(valid-answers 2 10)(skippable FALSE))
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
    (slot altitude (type FLOAT))
    (slot longitude (type FLOAT))
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


(deffacts location-list
    (location (name agrigento) (region sicilia)(altitude 37.31) (longitude 12.58))
    (location (name palermo) (region sicilia)(altitude 38.14) (longitude 13.31))
    (location (name catania) (region sicilia)(altitude 37.51) (longitude 15.08))
    (location (name scilla) (region calabria)(altitude 38.14) (longitude 13.31))
    (location (name reggio) (region calabria)(altitude 38.10) (longitude 15.66))
    (location (name salerno) (region calabria)(altitude 40.41) (longitude 14.46))
    (location (name bari) (region puglia)(altitude 41.07) (longitude 16.53))
    (location (name lecce) (region puglia)(altitude 40.21) (longitude 18.11))
    (location (name brindisi) (region puglia)(altitude 40.39) (longitude 17.56))
    (location (name roma) (region lazio)(altitude 41.54) (longitude 12.31))
    (location (name latina) (region lazio)(altitude 41.28) (longitude 12.51))
    (location (name frosinone) (region lazio)(altitude 41.38) (longitude 13.22))
    (location (name pisa) (region toscana)(altitude 43.43) (longitude 10.24))
    (location (name siena) (region toscana)(altitude 43.19) (longitude 11.18))
    (location (name lucca) (region toscana)(altitude 43.51) (longitude 10.31))
    (location (name genova) (region liguria)(altitude 44.25) (longitude 08.55))    
    (location (name savona) (region liguria)(altitude 44.19) (longitude 08.28))    
    (location (name laspezia) (region liguria)(altitude 44.07) (longitude 09.51))    
    (location (name milano) (region lombardia)(altitude 45.28) (longitude 09.11))    
    (location (name pavia) (region lombardia)(altitude 45.11) (longitude 09.11))    
    (location (name bergamo) (region lombardia)(altitude 45.42) (longitude 09.40))    
    (location (name asti) (region piemonte)(altitude 44.53) (longitude 08.11)) 
    (location (name torino) (region piemonte)(altitude 45.04) (longitude 07.42)) 
    (location (name pinerolo) (region piemonte)(altitude 45.10) (longitude 08.00))    

    
)


(defrule calcolo-distance
    (location (name ?n)(region ?r)(altitude ?a)(longitude ?l))
    (location (name ?n1&:(neq ?n1 ?n))(region ?r1)(altitude ?a1)(longitude ?l1))
    =>

    ;; equaizione per calcolare km dati due punti  
    ;;R= 6372 R * arccos(sin(latA) * sin(latB) + cos(latA) * cos(latB) * cos(lonA-lonB))
    ;;(* R (acos(+ (* (sin latA)(cos latB)) (* (*(cos latA) (cos(latB))) (cos (- lonA LonB))))))
    ;; trasformazione decimale/radianti : 40° × π / 180°
    (bind ?a1_rad (* ?a1 (/ (pi) 180)))
    (bind ?a_rad (* ?a (/ (pi) 180)))
    (bind ?l1_rad (* ?l1 (/ (pi) 180)))
    (bind ?l_rad (* ?l (/ (pi) 180)))
    (bind ?km_distance (* 6372.795 (acos (+ (* (sin ?a_rad)(sin ?a1_rad)) (* (* (cos ?a_rad) (cos ?a1_rad)) (cos (- ?l_rad ?l1_rad))) )) ))
    ;;(bind ?euclidian_distance (sqrt(+ (** (- ?l ?l1) 2) (** (- ?a ?a1) 2))))
    (assert(loc-to-loc(location-src ?n)(location-dst ?n1)(distance ?km_distance))) 

)

(defrule location-turism-list-random-creation
    (location (name ?n) (region ?r))
    =>
    ;;(seed (round (time)))
    (assert(location-tourism (location-name ?n)(tourism-type balneare)(score  (random 1 5))))
    (assert(location-tourism (location-name ?n)(tourism-type naturale)(score  (random 1 5))))
    (assert(location-tourism (location-name ?n)(tourism-type culturale)(score  (random 1 5))))
    (assert(location-tourism (location-name ?n)(tourism-type enogastronomico)(score  (random 1 5))))
)

;;MODULE HOTEL

(defmodule HOTEL (export ?ALL) (import LOCATION ?ALL))

(deftemplate HOTEL::hotel
    (slot name (default ?NONE))
    (slot location (default ?NONE))
    (slot stars (type INTEGER) (range 1 4))
    (slot empty (type INTEGER) (range 0 ?VARIABLE))
    (slot capacity (type INTEGER) (range 1 ?VARIABLE))
)


(defrule list-hotel-random-creation
    (location (name ?n) (region ?r))
    =>
    
    (assert(hotel (name (str-cat "morandi-" ?n))(location ?n)(stars (random 1 5))(empty (random 100 300))(capacity 300)))
    (assert(hotel (name (str-cat "empedocle-" ?n))(location ?n)(stars (random 1 5))(empty (random 100 300))(capacity 300)))
    (assert(hotel (name (str-cat "leonardo-" ?n))(location ?n)(stars (random 1 5))(empty (random 100 300))(capacity 300)))
    (assert(hotel (name (str-cat "sciascia-" ?n))(location ?n)(stars (random 1 5))(empty (random 100 300))(capacity 300)))

)

;;PATH  (assert (path (resorts ?r) (length 1) (total-distance 0)))

(deftemplate path
    (slot path-id (default-dynamic (gensym*)))
    (multislot locations)
    (slot length (type INTEGER))
    (slot total-distance (type FLOAT))
    
)

;; MODULE TRIP

(defmodule TRIP (export ?ALL))

(deftemplate TRIP::trip
    (multislot locations)
    (multislot hotels (default ND ND ND ND ND))
    (multislot costs (type INTEGER) (default 0 0 0 0 0))
    (multislot days (type INTEGER) (range 0 ?VARIABLE))
    (slot tot-dist (type INTEGER) (range 0 ?VARIABLE))    
)

(deftemplate TRIP::average-location-cf
    (slot value)
)

(deftemplate TRIP::banned-path
    (slot path-id)
)