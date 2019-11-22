;;****************
;;* MODULE MAIN  *
;;****************


(defrule MAIN::start
  (declare (salience 1000))
  =>
  (set-fact-duplication TRUE)
  (focus LOCATION HOTEL)
)


(deffacts MAIN::define-phase-sequence
    (phase-sequence ASK-QUESTION RULES RATE PATH BUILD-TRIP RATE-TRIP PRINT-RESULTS END-RULE)
)

(defrule MAIN::change-phase
    (declare (salience -1000))
    ?list <- (phase-sequence ?next-phase $?other-phases)
=>
    (printout t "prossima fase: ")
    (printout t ?next-phase )
    ;; Runno il primo focus
    (focus ?next-phase)
    (retract ?list)
    ;; una volta runnato il primo focus runno il secondo e metto il primo in coda di modo che quando finisco tutti rirunno il primo
    (assert (phase-sequence ?other-phases ?next-phase)) 
)

;;(defrule MAIN::start
  ;;(declare (salience 10000))
  ;;=>
  ;;(set-fact-duplication TRUE)
  ;;(focus LOCATION HOTEL ASK-QUESTION RULES RATE PATH BUILD-TRIP RATE-TRIP END-RULE))

;;***********************
;;* MODULE ASK-QUESTION *
;;***********************
(defmodule ASK-QUESTION (import QUESTIONS ?ALL) (import COMMON ?ALL))

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
   (iteration (i ?i))
   ?f <- (question 
                   (importance ?i)
                   (already-asked FALSE)
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
    
    ;; La prima domanda che fa se siamo in un iterazione > 0 
    (defrule ASK-QUESTION::ask-the-end-question
        (declare (salience 10000))
        (iteration (i ?i))
        (test (> ?i 0))
        =>
        (printout t "Entra in ask-the-end-question")
        (bind ?q "Are you happy with one of the suggested trips? [yes, no] ")
        (bind ?va (create$ yes no))
        (bind ?answer (ask-question normal ?q ?va))
        (if (eq ?answer yes) then 
            (printout t "Thank you for using our expert system. Have a good vacation!" crlf crlf)
            (halt)
            ;;(reset)
        )
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
        (preference (type trip-length) (answer ?s&:(and(> ?s 1)(< ?s 7))))
        =>
        (assert(attribute(name trip-length)(value ?s)(certainty 1.0)))
   )

   ;;-------TRIP LOCATION
      (defrule RULES::trip-number
        (preference (type trip-number-location) (answer ?s&:(and(> ?s 1)(< ?s 30))))
         
        =>
        
        (assert(attribute(name trip-number-location)(value ?s)(certainty 1.0)))


   )

    ;;-------- LOC-HOTEL PREFERENCE
    (defrule RULES::hotel-loc-preference
        (printout t "entra in trip prima")
        (preference (type hotel-loc-preference)(answer ?s))
        ?a <- (attribute(name hotel-loc-preference))
        =>
        (printout t "entra in trip dopo")
        (retract ?a)
        (assert(attribute (name hotel-loc-preference)(value ?s)(certainty 1.0)))

        
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

    (defrule RULES::trip-visit-region-generic
        (preference (type trip-visit-region-generic)(answer ?as))
        =>
        (assert(attribute(name trip-visit-region-generic)(value ?as)(certainty 1.0)))

    )

    (defrule RULES::trip-visit-region
        (preference (type trip-visit-region)(answer ?as))
        =>
        (assert(attribute(name trip-visit-region)(value ?as)(certainty 1.0)))

    )

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
        ;; valore massimo 0.6  
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
    ;;--------------PEOPLE NUMB

    (defrule people-number
        (preference (type people-number)(answer ?a))
    =>
        (assert (attribute (name the-people-number)(value ?a)(certainty 1.0)))
    )



;;*********************
;;* MODULE RATE *
;;*********************

(defmodule RATE (import COMMON ?ALL) (import HOTEL ?ALL)(import LOCATION ?ALL))



;;------ RATE HOTEL

(defrule rate-hotel-by-stars
    (attribute (name stelle-hotel)(value ?s)(certainty ?cf))
    (hotel (name ?h)(location ?l)(stars ?s))
=>
    (assert (attribute (name the-hotel-in ?l) (value ?h)(certainty (* (/ ?cf 0.4) 0.7))))
)

(defrule rate-hotel-by-availability
    (attribute (name the-people-number)(value ?p))
    (hotel (name ?h) (location ?l) (empty ?e&:(> ?e ?p)) (capacity ?c))
=>
    (bind ?ncf (* 0.7 (/ ?e ?c)))
    (assert (attribute (name the-hotel-in ?l)(value ?h)(certainty ?ncf)))
)

(defrule rate-hotel-by-availability-full
    
    (attribute (name the-people-number) (value ?p))
    (hotel (name ?h) (location ?l) (empty ?e&:(< ?e ?p)))
=>
    ;; DOMANDA PROF è giusto andare ad eliminare l'hotel in quanto non utilizzabile (perchè pieno)
    (assert (attribute (name the-hotel-in ?l) (value ?h) (certainty -1.0)))
)



;;------ RATE LOCATION

 (defrule rate-location-type
        (attribute (name tourism-type)(value ?v)(certainty ?a))
        (location-tourism(location-name ?l)(tourism-type ?v)(score ?cf))
        =>
        (assert(attribute(name rate-tourism-type)(value ?l)(certainty (/ (* ?a ?cf) (* ?a 5)))))
 )

 (defrule rate-location-by-region
        (attribute(name trip-visit-region)(value ?l)(certainty ?a))
        (location (name ?na)(region ?l))
        =>
        (assert(attribute(name rate-tourism-type)(value ?na)(certainty ?a)))
 )


;;----------------------------- PATH


;; I path simili sono quelli aventi stesse città ma ordine diverso, stabiliamo quindi che un viaggio è un insieme di città in cui non è importante l'ordine

(defmodule PATH (import COMMON ?ALL) (import HOTEL ?ALL)(import LOCATION ?ALL)(export ?ALL))

(deftemplate best-hotel-in-location
    (slot location-name)
    (slot best-hotel)
    (slot score(type FLOAT))

)


(defrule fill-best-hotel-in-location
        (location (name ?r))
        (attribute (name the-hotel-in ?r) (value ?h) (certainty ?hcf))
        (not (attribute (name the-hotel-in ?r) (value ?h2&~?h) (certainty ?hcf2&:(> ?hcf2 ?hcf))))
        (hotel (name ?h) (location ?r) (stars ?s))
        =>

        (assert(best-hotel-in-location(location-name ?r)(best-hotel ?h)(score ?hcf)))
)

(defrule build-singleton-path-simple
    (location (name ?r))    
    =>
    (assert (path (locations ?r) (length 1) (total-distance 0.0)(score 0.0)))
)


;; Ritorna tutti i path con numero location < trip-length in cui le distanze tra una citta e l'altra sia minore di max-km-day e il totale dei km sia minore di trip-length * max-km-day < del trip
(defrule build-path-by-hotel
    (path (locations $?rs ?lr) (length ?len) (total-distance ?td)(score ?scr))
    
    (attribute (name trip-length) (value ?tl))
    (test (<= (+ ?len  1) ?tl)) ;vincolo numero giorni
    (loc-to-loc (location-src ?lr) (location-dst ?nr) (distance ?d)) 
    (test (< ?d ?*MAX-KM-DAY*)) ;;vincolo distanza giornaliera
    (test (eq (member$ ?nr (create$ ?rs ?lr)) FALSE))
    =>
    (if (< (+ ?td ?d) (* ?*MAX-KM-DAY* ?tl)) then
        (assert (path (locations ?rs ?lr ?nr) (length (+ ?len 1)) (total-distance (+ ?td ?d))(score ?scr)))
    )    
)



(defrule delete-similar-path
    (declare (salience 100))
    ?p1 <- (path (path-id ?id1)(locations $?rs))
    ?p2 <- (path (path-id ?id2&:(neq ?id2 ?id1))(locations $?rs1))
    (test (subsetp ?rs ?rs1))
    (test (subsetp ?rs1 ?rs))
    =>
    (retract ?p1)
)

(defrule pruning-location-number-path
    (attribute (name trip-number-location) (value ?tl))
    ?p <- (path (length ?len))
    ;; Se la differenza tra la durata del viaggio e la durata del path è maggiore di 1
    (test (neq ?len ?tl))
    =>
    (retract ?p)
)




 ;;----------------------------------TRIP

;; Trip con sole le location

;;(defrule build-simple-trip

;;)



;; prima valutare gli hotel in quanto sono indipendi rispetto le città. io posso andare a valutare gli hotel  solo in base alle stelle e numero di posti disponibili

;; dopo valutare le location  che è dipendente dalla tipologia di viaggio e dall'esistenza di hotel che possa soddisfare stelle-numero posti

(defmodule BUILD-TRIP (import COMMON ?ALL) (import HOTEL ?ALL) (import TRIP ?ALL))

(defrule BUILD-TRIP::average-location-cf
    (declare (salience 500))
    (attribute (name rate-tourism-type))
=>
    (bind ?sum 0)
    (bind ?count 0)
    (do-for-all-facts ((?f attribute)) (eq ?f:name (create$ tourism-type))
    (bind ?sum (+ ?sum ?f:certainty))
    (bind ?count (+ ?count 1)))
    
    (printout t "media: " (/ ?sum ?count) crlf)
    (assert (average-location-cf (value (/ ?sum ?count))))
)

(defrule BUILD-TRIP::path-pruning-strict
    (declare (salience 400))
    (average-location-cf (value ?a))
    (path (path-id ?id) (locations $?rl ?r $?rr))
        ;; un path viene bannato se le città che lo pongono sono sotto la media o non esistono hotel con cf maggiore di 0.2

    (or (attribute (name rate-tourism-type) (value ?r) (certainty ?cfr&:(< ?cfr ?a)))
        (not (attribute (name the-hotel-in ?r) (certainty ?cfh&:(>= ?cfh 0.2)))))
=>
    (assert (banned-path (path-id ?id)))
)


(defrule BUILD-TRIP::build-trip
    (declare (salience 300))
    
    (path (path-id ?id) (locations $?rs) (length ?len)(score ?scr))
    (not (banned-path (path-id ?id)))
    (attribute (name trip-length) (value ?ds))
=>
    (assert (trip (locations ?rs)(tot-dist ?len)(duration ?len)))
)

;; Inserisce al campo days il minimo periodo di giorni in una citta, avremo un vettore in cui ci saranno tanti 1 quanti sono le città
(defrule fill-trip-days-basic
    (iteration (i ?i))
    (test (eq ?i 0))
    ?t <- (trip (locations $?rl ?r $?rr)(days $?d))

    =>
    (bind ?count (member$ ?r (create$ $?rl ?r $?rr)))
    (modify ?t (days (replace$ ?d ?count ?count 1)))

)


(defmodule RATE-TRIP (import PATH ?ALL)(import COMMON ?ALL) (import HOTEL ?ALL)(import TRIP ?ALL)(import LOCATION ?ALL))


(defrule fill-trip-hotel-and-costs
    (declare (salience 1000))
    ?t <- (trip (locations $?rl ?r $?rr) (hotels $?hs) (days $?ds) (costs $?cs)(hotel-score $?hscore))
    (best-hotel-in-location(location-name ?r)(best-hotel ?h)(score ?hcf))
    (hotel (name ?h)(location ?r)(stars ?s))
    (attribute (name the-people-number) (value ?p))    
    =>
    (bind ?count (member$ ?r (create$ ?rl ?r ?rr))) ;; assegna a count la posizione di ?r rispetto la lista delle location del trip
    (bind ?daily-cost (+ ?*HOTEL-BASE-COST* (* ?s ?*HOTEL-ADDITIONAL-COST*)))
    (bind ?cost-all-people (* (max 1 (div ?p 2)) ?daily-cost))
   
    ;;(bind ?cost-all-days (* (nth$ ?index ?ds) ?cost-all-people))
    (modify ?t (hotels (replace$ ?hs ?count ?count ?h))(costs (replace$ ?cs ?count ?count ?cost-all-people))(hotel-score (replace$ ?hscore ?count ?count ?hcf))) ;;il replace vuole 4 argomenti il primo è quello da sotituire, l'ultimo quello che rimpiazza il primo, quelli di mezzo sono interi che indicano l'indice del valore del multislot da sostituire 
)

;;Non funziona
;;(defrule add-trip-hotel-score
    ;;(declare (salience 10))
    ;;(printout t "prima entra in add-trip-hotel-score")
    ;;?t <- (trip (locations $?rl ?r $?rr) (hotels $?hs)(hotel-score $?ls))
    ;;(best-hotel-in-location(location-name ?r)(best-hotel ?h)(score ?hcf))

    ;;=>
    ;;(printout t "entra in add-trip-hotel-score")
    ;;(bind ?count (member$ ?r (create$ ?rl ?r ?rr))) ;; assegna a count la posizione di ?r rispetto la lista delle location del trip
    ;;(modify ?t (hotel-score (replace$ ?hs ?count ?count ?hcf))) ;;il replace vuole 4 argomenti il primo è quello da sotituire, l'ultimo quello che rimpiazza il primo, quelli di mezzo sono interi che indicano l'indice del valore del multislot da sostituire 
;;)


(defrule add-trip-location-score
    (declare (salience 1000))
    ?t <- (trip (locations $?rl ?r $?rr)(location-score $?ls))
    (attribute (name rate-tourism-type)(value ?r)(certainty ?cf))
    =>
    (bind ?count (member$ ?r (create$ ?rl ?r ?rr))) ;; assegna a count la posizione di ?r rispetto la lista delle location del trip
    (modify ?t (location-score (replace$ ?ls ?count ?count ?cf))) ;;il replace vuole 4 argomenti il primo è quello da sotituire, l'ultimo quello che rimpiazza il primo, quelli di mezzo sono interi che indicano l'indice del valore del multislot da sostituire 
)


(deffunction fill-days(?numberOfLoc ?remainDay $?tripLocScore)
 
    (bind ?supportList (create$ 0 0 0 0 0))
    (bind ?counter 1)
    (loop-for-count ?numberOfLoc
        (bind ?supportList (replace$ ?supportList ?counter ?counter 1))
        (bind ?counter (+ ?counter 1))
    )
    (bind ?numberDay ?remainDay)
    (loop-for-count ?numberDay
        (if (> ?remainDay 0) then
            (bind ?maxValue (max (expand$ ?tripLocScore)))
            (printout t "maxValue di " ?tripLocScore " è " ?maxValue crlf)
            (bind ?index (member$ ?maxValue ?tripLocScore))
            (bind ?ls2 ?maxValue)
          

            (bind ?supportValue (nth$ ?index ?supportList))
            (bind ?supportList (replace$ ?supportList ?index ?index (+ ?supportValue 1)))
            (bind ?remainDay (- ?remainDay 1))
            (bind ?tripLocScore (replace$ ?tripLocScore ?index ?index (- ?maxValue  0.2)))
        
        )
    
    )
    (return ?supportList)

)


(defrule fill-trip-days-by-hotel
    (declare (salience 100))
    ;;(attribute (name hotel-loc-preference)(value hotel)(certainty ?cf))
    ?t <- (trip (locations $?rl ?r $?rr)(days $?d)(tot-dist ?td)(hotel-score $?hotelScore)(duration ?dur))
    (attribute (name trip-length)(value ?len))    
    (test (< ?dur ?len))
    =>
    (bind ?rd (- ?len ?td))
    (printout t "prima della funzione :")
    (printout t $?hotelScore crlf)
    (bind ?supportList (fill-days (length$ (create$ $?rl ?r $?rr)) ?rd ?hotelScore))
    (modify ?t (days ?supportList)(duration ?len))
)

;; Funzione che ritorta true se una la somma di una lista di interi è uguale ad un intero
(deffunction sum-list-equal-integer (?integer $?list)
    (bind ?result 0)
    (foreach ?number $?list
        (bind ?result (+ ?result ?number))
    )

    (return (eq ?result ?integer))
)

;;Rule che printa i trip che hanno la somma dei giorni di un trip diversa dall'attributo trip-length 
(defrule check-number-day
    (declare (salience 20))
    ?t <- (trip (locations $?rl ?r $?rr)(days $?d)(tot-dist ?td)(hotel-score $?hotelScore)(duration ?dur))
    =>
    (bind ?result (sum-list-equal-integer ?dur ?d))
    (if (eq ?result FALSE) then
        (printout t "---------ERRORE nella lista: ")
        (printout t (create$ $?rl ?r $?rr))
        (printout t "  trip-length: ")
        (printout t ?dur)
        (printout t "  è diverso dalla durata del trip: ")
        (printout t ?d crlf)
    )

    

)


;;---- DA FARE
;;(defrule fill-trip-days-by-location    
  ;;  (declare (salience 100))    
    ;;(attribute (name hotel-loc-preference)(value location)(certainty ?cf))
    ;;?t <- (trip (locations $?rl ?r $?rr)(days $?d)(tot-dist ?td)(location-score ?locationScore)(duration ?dur))
    ;;(attribute (name trip-length)(value ?len))    
    ;;(test (< ?dur ?len))
    ;;=>
    ;;(bind ?rd (- ?len ?td))
    ;;(bind ?supportList (fill-days (length$ (create$ $?rl ?r $?rr)) ?rd ?locationScore))
    ;;(printout  t "lista giorni: " ?supportList crlf)
    ;;(modify ?t (days ?supportList)(duration ?len))

;;)



(defrule rate-trip-by-locations
    (declare (salience 10))   
    (trip (trip-id ?id)(locations $?ll ?l $?lr)(days $?ds)(duration ?dr)(hotel-score $?hhs)(location-score $?lls)(tot-dist ?nl&:(> ?nl 1)))
    (attribute (name rate-tourism-type)(value ?l)(certainty ?lcf))
    (attribute (name trip-length)(value ?td))
=>
    (bind ?locLen (length$ (create$ $?ll ?l $?lr)))
    (bind ?count (member$ ?l (create$ ?ll ?l ?lr)))
    (bind ?d (nth$ ?count ?ds))
    
    ;;(bind ?ncf (/ (sum-integer-list ?lls) ?nl))
    (bind ?ncf ?lcf)
    ;;(printout t (create$ $?ll ?l $?lr) " con hotel score: " $?hhs " e location-score: " $?lls " la valutazione è: " (+ ?ncf (* ?locLen 0.05)) crlf)
    (assert (attribute (name the-trip)(value ?id)(certainty (+ ?ncf (* ?nl 0.017)))  ))

)

(defrule rate-trip-by-hotels
    (declare (salience 10))   
    (trip (trip-id ?id)(locations $?ll ?l $?lr)(hotels $?hs)(days $?ds)(duration ?dr)(total-score ?ts)(hotel-score $?hhs)(location-score $?lls)(tot-dist ?nl&:(> ?nl 1)))
    (attribute (name the-hotel-in ?l)(value ?h)(certainty ?hcf)) ;; valutare se conviene usare l'attributo best-hotel-in
    (test (eq ?h (nth$ (member$ ?l (create$ ?ll ?l ?lr)) ?hs)))
    (attribute (name trip-length)(value ?td))
=>
    (bind ?locLen (length$ (create$ $?ll ?l $?lr)))
    (bind ?count (member$ ?l (create$ ?ll ?l ?lr)))
    (bind ?d (nth$ ?count ?ds))
    ;;(bind ?ncf (/ (sum-integer-list ?hhs) ?nl))
    (bind ?ncf ?hcf)
    ;;(printout t (create$ $?ll ?l $?lr) " con hotel score: " $?hhs " e location-score: " $?lls " la valutazione è: " (+ ?ncf (* ?locLen 0.05)) crlf)

    (assert (attribute (name the-trip) (value ?id) (certainty (+ ?ncf (* ?nl 0.015)))))
)

(defrule rate-trip-by-region
    (declare (salience 10))
    (trip (trip-id ?id)(locations $?ll ?l $?lr)(hotels $?hs)(days $?ds)(duration ?dr)(total-score ?ts)(hotel-score $?hhs)(location-score $?lls)(tot-dist ?nl&:(> ?nl 1)))
    (attribute (name trip-visit-region)(value ?r))
    (location (name ?l)(region ?r))
    =>
    (assert (attribute (name the-trip) (value ?id) (certainty 0.90)))    

)

(deffunction sum-integer-list ($?list)
    (bind ?result 0)
    (foreach ?number $?list
        (bind ?result (+ ?result ?number))
    )

    return ?result
)

(defrule rate-trip-by-budget
    (declare (salience 10))
    (attribute (name trip-budget)(value ?r))
    (trip (trip-id ?id)(tot-dist ?nl&:(> ?nl 1))(costs $?costs))
    

    =>
    (bind ?tc (sum-integer-list ?costs) ) 

    (if (< ?tc ?r) then
        (assert (attribute (name the-trip) (value ?id) (certainty 0.95)))
    )
)



;;---- DA FARE
;;(defrule fill-trip-days-by-location    


;;)



(defmodule PRINT-RESULTS (import COMMON ?ALL) (import TRIP ?ALL))

(defrule results-header
   (declare (salience 500))
   (iteration (i ?i))
=>
   (printout t  crlf crlf)
   (printout t " >>>>>>>>>>>>>>>   SELECTED TRIPS (ITERATION " (+ ?i 1) ")  <<<<<<<<<<<<<<<"  crlf)
   (printout t  crlf)
   (assert (printed-trips 0))
)

(defrule print-and-remove-best-trip
    ?fact1 <- (printed-trips ?p)
    (test (< ?p 5))
    ?fact2 <- (attribute (name the-trip)(value ?tid)(certainty ?tcf))
    (not (attribute (name the-trip)(value ?tid2&~?tid)(certainty ?tcf2&:(> ?tcf2 ?tcf))))
    (test (> ?tcf ?*MIN-PRINT-CF*))
    (trip (trip-id ?tid)(locations $?lc)(hotels $?hs)(days $?ds)(costs $?cs)(duration ?dr))
 
=>
    (retract ?fact1)
    (assert (printed-trips (+ ?p 1)))
    (retract ?fact2)
    (bind ?total-cost (+ (expand$ ?cs) 0))
    (printout t  crlf)
    (printout t " Trip suggestion " (+ ?p 1) " with certainty: " (/ (round (* ?tcf 1000)) 10) "%" crlf)
    (printout t "  - Resorts to visit: " ?lc crlf)
    (printout t "  - Hotels: " (subseq$ ?hs 1 ?dr ) crlf)
    (printout t "  - Days partitioning: " ?ds crlf)
    (printout t "  - Daily costs: " (subseq$ ?cs 1 ?dr ) "  |  Total cost: " ?total-cost crlf) 
    (printout t  crlf)
    (printout t "      _____________________________________________________" crlf)
    (printout t  crlf)

)

(defmodule END-RULE (import COMMON ?ALL) (import TRIP ?ALL))

(defrule END-RULE::on-exit
    (declare (salience 1000))
    ?fact <- (iteration (i ?i))
=>
    (retract ?fact)
    (assert (iteration (i (+ ?i 1))))  ;;increment iteration number

    (pop-focus)
)

(defrule END-RULE::plsstop
    (declare (salience 400))
    (iteration (i ?i))
=>
    (halt)
) 