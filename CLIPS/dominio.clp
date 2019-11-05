;;MODULE QUESTION

(defmodule MAIN (export ?ALL))



(defmodule QUESTIONS (export ?ALL))

(deftemplate QUESTIONS::question
    (slot importance (type INTEGER)) ;; Un valore da 0-3 per indicare l'importanza della domanda la domanda 0 verrà fatta prima della domanda 3
    (slot attribute (default ?NONE))
    (slot the-question (default ?NONE))
    (multislot valid-answers (default ?NONE))
    (slot skippable (default TRUE))
    (slot already-asked (default FALSE))
    (multislot precursors (default ?DERIVE))
)

(deffacts question-list
    (question (attribute trip-length)(importance 0) (the-question "Quanti giorni vuoi che la vacanza duri? valore tra [1,30]") (valid-answers 1 30) (skippable FALSE))
    (question (attribute trip-budget-generic)(importance 0) (the-question "Hai un budget massimo? [Si, No]") (valid-answers Si No si no) (skippable FALSE))
    (question (attribute trip-budget)(importance 0) (the-question "Qual'è il tuo budget? valore tra [200,5000]") (valid-answers 200 5000) (skippable FALSE)(precursors budget-limit-generic is yes))
    (question (attribute trip-more-region-generic)(importance 0) (the-question "Vuoi visitare più regioni? [Si, No]") (valid-answers Si No si no) (skippable FALSE))
    (question (attribute trip-more-region)(importance 0) (the-question "Quante regioni vorresti visitare? valore tra [2,6]") (valid-answers  2 6) (skippable FALSE)(precursors trip-more-region-generic is yes))
    (question (attribute trip-more-location-generic) (importance 0) (the-question "Vuoi visitare più location? [Si,No]") (valid-answers Si No si no) (skippable FALSE))
    (question (attribute trip-more-region)(importance 0) (the-question "Quante location vorresti visitare? valore tra [2,10]") (valid-answers  2 10) (skippable FALSE)(precursors trip-more-location-generic is yes))
    (question (attribute trip-type)(importance 0) (the-question "Quale tipologia di viaggio vuoi fare? [Montagna, Mare]") (valid-answers  montagna mare) (skippable FALSE))

)

;;MODULE LOCATION

(defmodule LOCATION (export ?ALL))

(deftemplate MAIN::location
    (slot name (default ?NONE))
    (slot region (default ?NONE))
)

(deftemplate MAIN::loc-to-loc
    (slot location-src (default ?NONE))
    (slot location-dst (default ?NONE))
    (slot distance (type FLOAT))
)

(deftemplate MAIN::location-tourism-list
    (slot location-name (default ?NONE))
    (slot tourism-type (default ?NONE))
    (slot score(type INTEGER) (range 1 5))
)

(deftemplate MAIN::provalocation
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

;;(deffacts loc-to-loc-list
    ;;(loc-to-loc (location-src palermo)(location-dst agrigento)(distance))
    ;;(loc-to-loc (location-src palermo)(location-dst catania)(distance))
    ;;(loc-to-loc (location-src palermo)(location-dst reggio)(distance))
    ;;(loc-to-loc (location-src palermo)(location-dst salerno)(distance))
    ;;(loc-to-loc (location-src agrigento)(location-dst catania)(distance))
    ;;(loc-to-loc (location-src agrigento)(location-dst bari)(distance))
    ;;(loc-to-loc (location-src agrigento)(location-dst salerno)(distance))
    ;;(loc-to-loc (location-src catania)(location-dst brindisi)(distance))
    ;;(loc-to-loc (location-src catania)(location-dst scilla)(distance))
    ;;(loc-to-loc (location-src catania)(location-dst roma)(distance))
    ;;(loc-to-loc (location-src reggio)(location-dst salerno)(distance))
    ;;(loc-to-loc (location-src reggio)(location-dst scilla)(distance))
    ;;(loc-to-loc (location-src reggio)(location-dst bari)(distance))
    ;;(loc-to-loc (location-src reggio)(location-dst latina)(distance))
    ;;(loc-to-loc (location-src salerno)(location-dst scilla)(distance))
    ;;(loc-to-loc (location-src salerno)(location-dst brindisi)(distance))
    ;;(loc-to-loc (location-src salerno)(location-dst frosinone)(distance))
    ;;(loc-to-loc (location-src scilla)(location-dst lecce)(distance))
    ;;(loc-to-loc (location-src scilla)(location-dst roma)(distance))
    ;;(loc-to-loc (location-src roma)(location-dst latina)(distance))
    ;;(loc-to-loc (location-src roma)(location-dst frosinone)(distance))
    ;;(loc-to-loc (location-src roma)(location-dst siena)(distance))
    ;;(loc-to-loc (location-src frosinone)(location-dst latina)(distance))

;;)

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


