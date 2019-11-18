(deftemplate hotel
    (slot name (default ?NONE))
    (slot location (default ?NONE))
    (slot stars (type INTEGER) (range 1 5))
    (slot empty (type INTEGER) (range 0 ?VARIABLE))
    (slot capacity (type INTEGER) (range 1 ?VARIABLE))
)

(deftemplate attribute
   (multislot name)
   (slot value)
   (slot certainty (type FLOAT) (range -1.0 1.0) (default 0.0))
)

(deftemplate location-tourism
    (slot location-name (default ?NONE))
    (slot tourism-type (default ?NONE))
    (slot score(type INTEGER) (range 1 5))
)

(defrule combine-certainties-both-positive
    (declare (auto-focus TRUE))
    ?fact1 <- (attribute (name $?n) (value ?v) (certainty ?C1&:(>= ?C1 0.0)))
    ?fact2 <- (attribute (name $?n) (value ?v) (certainty ?C2&:(>= ?C2 0.0)))
    (test (neq ?fact1 ?fact2))
=>
    (retract ?fact1)
   ;; (bind ?C3 (- (+ ?C1 ?C2) (* ?C1 ?C2)))
    ;; mi arriveranno in c1 e c2 valori compresi tra 0 1 utilizzo l'eq: c1 +c2 / max(c1) + max(c2)
    (bind ?C3 (/ (+ ?C1 ?C2) 2))
    (modify ?fact2 (certainty ?C3))
)


(defrule combine-certainties-both-negative
    (declare (auto-focus TRUE))
    ?fact1 <- (attribute (name $?n) (value ?v) (certainty ?C1&:(<= ?C1 0.0)))
    ?fact2 <- (attribute (name $?n) (value ?v) (certainty ?C2&:(<= ?C2 0.0)))
    (test (neq ?fact1 ?fact2))
=>
    (retract ?fact1)
    (bind ?C3 (/(+ ?C1 ?C2) 2))
    (modify ?fact2 (certainty ?C3))
)

(defrule combine-certainties-negative-positive
    (declare (auto-focus TRUE))
    ?fact1 <- (attribute (name $?n) (value ?v) (certainty ?C1))
    ?fact2 <- (attribute (name $?n) (value ?v) (certainty ?C2))
    (test (neq ?fact1 ?fact2))
    ;; Se è vero c1<c2
    (test (< (- ?C1 ?C2) 0.0))
=>
    (retract ?fact1)
    (bind ?C3 (/ (+ ?C2 ?C1) 2))
    (modify ?fact2 (certainty ?C3))
)

;;-------------------------------PERSONALIZZAZIONE--------------------------

    
(deffacts init
        ;;scelta utente normale
        (attribute (name stelle-hotel)(value 1)(certainty -0.2))
        (attribute (name stelle-hotel)(value 2)(certainty 0.4))
        (attribute (name stelle-hotel)(value 3)(certainty 0.4))
        (attribute (name stelle-hotel)(value 4)(certainty -0.2))

        (attribute (name tourism-type)(value enogastronomico)(certainty 0.6))
        (attribute (name tourism-type)(value religioso)(certainty 0.3))
        (attribute (name tourism-type)(value culturale)(certainty 0.3))
        (attribute (name tourism-type)(value balneare)(certainty 0.3))
        
        (attribute (name the-people-number)(value 4))

        ;; Valutazione per stelle
        (hotel (name empedocle)(location agrigento)(stars 1))
        (hotel (name sciascia)(location agrigento)(stars 2));; mi aspetto valore 1
        (hotel (name winner)(location agrigento)(stars 3));; mi aspetto valore 1
        (hotel (name astor)(location agrigento)(stars 4))

        ;; Valutazione per empty
        (hotel (name empedocle)(location palermo)(stars 2)(empty 300)(capacity 300))
        (hotel (name sciascia)(location palermo)(stars 2)(empty 200)(capacity 300))
        (hotel (name winner)(location palermo)(stars 2)(empty 50)(capacity 300))
        (hotel (name astor)(location palermo)(stars 2)(empty 10)(capacity 300))


        ;;
        (location-tourism (location-name agrigento)(tourism-type balneare)(score  2))
        (location-tourism (location-name agrigento)(tourism-type naturale)(score  3))
        (location-tourism (location-name agrigento)(tourism-type culturale)(score  4))
        (location-tourism (location-name agrigento)(tourism-type enogastronomico)(score  3))

        (location-tourism (location-name palermo)(tourism-type balneare)(score  2))
        (location-tourism (location-name palermo)(tourism-type naturale)(score  3))
        (location-tourism (location-name palermo)(tourism-type culturale)(score  4))
        (location-tourism (location-name palermo)(tourism-type enogastronomico)(score  5))


)


(defrule rate-hotel-by-stars
    (attribute (name stelle-hotel)(value ?s)(certainty ?cf))
    (hotel (name ?h)(location ?l)(stars ?s))
=>
    (assert (attribute (name the-hotel-in ?l) (value ?h)(certainty (/ ?cf 0.4))))
)

 (defrule rate-location-type
        (attribute (name tourism-type)(value ?v)(certainty ?a))
        (location-tourism(location-name ?l)(tourism-type ?v)(score ?cf))
        =>
        (assert(attribute(name rate-tourism-type)(value ?l)(certainty (/ (* ?a ?cf) 3))))  
    )

(defrule rate-hotel-by-availability
    (attribute (name the-people-number)(value ?p))
    (hotel (name ?h) (location ?l) (empty ?e&:(> ?e ?p)) (capacity ?c))
=>
    (bind ?ncf (/ ?e ?c))
    (assert (attribute (name the-hotel-in ?l)(value ?h)(certainty ?ncf)))
)