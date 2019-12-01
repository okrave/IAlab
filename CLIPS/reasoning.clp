
;;------------------------------------------------ MODULE MAIN  


(defrule MAIN::start
  (declare (salience 1000))
  =>
  (set-fact-duplication TRUE)
  (focus LOCATION HOTEL)
)


(deffacts MAIN::sequenza-moduli
    (sequenza-moduli ASK-QUESTION REGOLE VALUTAZIONE PATH COSTRUZIONE-ITINERARIO VALUTAZIONE-ITINERARIO STAMPA-RISULTATI REGOLE-FINALI)
)

(defrule MAIN::alterazione-sequenza
    (declare (salience -1000))
    ?lista <- (sequenza-moduli ?prossimo-modulo $?altri-moduli)
=>
    (printout t "prossima fase: ")
    (printout t ?prossimo-modulo )
    ;; Runno il primo focus
    (focus ?prossimo-modulo)
    (retract ?lista)
    ;; una volta runnato il primo focus runno il secondo e metto il primo in coda di modo che quando finisco tutti rirunno il primo
    (assert (sequenza-moduli ?altri-moduli ?prossimo-modulo)) 
)

;;---------------------------------- MODULE ASK-QUESTION *

(defmodule ASK-QUESTION (import DOMANDE ?ALL) (import COMMON ?ALL))

(deffunction ASK-QUESTION::ask-question (?tipo ?domanda ?valori-consentiti)
    (bind ?risposta INVALID-ANSWER)
    (switch ?tipo
        (case normal then
            (printout t ?domanda)
            (bind ?risposta (read))
            (if (lexemep ?risposta) then (bind ?risposta (lowcase ?risposta)))
            (while (not (member$ ?risposta ?valori-consentiti)) do
                (printout t ?domanda)
                (bind ?risposta (read))
                (if (lexemep ?risposta) then (bind ?risposta (lowcase ?risposta))))
            
        )

        (case range then
            (bind ?min (nth$ 1 ?valori-consentiti))
            (bind ?max (nth$ 2 ?valori-consentiti)) 
            (while (or (not (integerp ?risposta)) (< ?risposta ?min) (> ?risposta ?max)) do
                (printout t ?domanda)
                (bind ?risposta (explode$ (readline)))
                (if (eq (length$ ?risposta) 0) then 
                    (bind ?risposta nil)(break))
                (bind ?risposta (nth$ 1 ?risposta))
            )
            
        )
    )
    (return ?risposta)

   
)


   (defrule ASK-QUESTION::ask-a-question
   (iterazione (i ?i))
   ?f <- (domanda 
                   (importanza ?i)
                   (chiesto FALSE)
                   (precursori)
                   (tipo ?tipo)
                   (la-domanda ?la-domanda)
                   (descrizione ?descrizione-domanda)
                   (risposte-valide $?risposte-valide))
   =>
   (modify ?f (chiesto TRUE))
   (assert (preferenze (tipo ?descrizione-domanda)(risposta (ask-question ?tipo ?la-domanda ?risposte-valide))))

   )	

    (defrule ASK-QUESTION::precursor-is-satisfied
        ?f <- (domanda (precursori ?t is ?v $?rest))
        (preferenze (tipo ?t) (risposta ?v))
        => 
        (modify ?f  (precursori ?rest))
    )
    
    ;; La prima domanda che fa se siamo in un iterazione > 0 
    (defrule ASK-QUESTION::ask-the-end-question
        (declare (salience 10000))
        (iterazione (i ?i))
        (test (> ?i 0))
        =>
        (printout t "Entra in ask-the-end-domanda")
        (bind ?q "Sei soddisfatto dell'itinerario proposto? [si, no] ")
        (bind ?va (create$ si no))
        (bind ?risposta (ask-question normal ?q ?va))
        (if (eq ?risposta si) then 
            (printout t "Grazie per aver utilizzato la nostra applicazione. Buona vacanza!!" crlf crlf)
            (halt)
        )

        (if (and (eq ?risposta no) (> ?i 3)) then 
            (printout t "Mi dispiace, non abbiamo trovato soluzioni. " crlf crlf)
            (halt)
        )
        
    )



;;----------------------------------------- MODULE REGOLE


(defmodule REGOLE (import DOMANDE ?ALL)(import COMMON ?ALL))

   (defrule REGOLE::budget-viaggio-generico
        (preferenze (tipo budget-viaggio-generico) (risposta ?r))
        =>
        (assert(attributo (nome budget-viaggio-generico)(valore ?r)(certezza 1.0)))
   )
   
   ;;--------- Periodo viaggio
    (defrule REGOLE:periodo-viaggio-estate
        (preferenze (tipo periodo-viaggio)(risposta estate))
        =>
        (assert(attributo (nome periodo-viaggio)(valore estate)(certezza 1.0)))
    )

    (defrule REGOLE:periodo-viaggio-inverno
        (preferenze (tipo periodo-viaggio)(risposta inverno))
        =>
        (assert(attributo (nome periodo-viaggio)(valore inverno)(certezza 1.0)))
    )

    (defrule REGOLE:periodo-viaggio-indifferente
        (preferenze (tipo periodo-viaggio)(risposta indifferente))
        =>
        (assert(attributo (nome periodo-viaggio)(valore indifferente)(certezza 1.0)))
    )

   ;;------- DURATA VIAGGIO

   (defrule REGOLE::durata-viaggio
        (preferenze (tipo durata-viaggio) (risposta ?s&:(and(> ?s 1)(< ?s 7))))
        =>
        (assert(attributo(nome durata-viaggio)(valore ?s)(certezza 1.0)))
   )


    ;;-------ITINERARIO BUDGET
   (defrule REGOLE::budget-viaggio-generico
        (preferenze (tipo budget-viaggio-generico)(risposta si))
        =>
        (assert(attributo(nome budget-viaggio-generico)(valore si)(certezza 1.0)))
   )

    (defrule REGOLE::budget-viaggio-generico
        (preferenze (tipo budget-viaggio-generico)(risposta no))
        =>
        (assert(attributo(nome budget-viaggio-generico)(valore no)(certezza 1.0)))
        (assert(attributo(nome budget-viaggio)(valore 5000)(certezza 1.0)))
   )

   (defrule REGOLE::budget-viaggio
        (preferenze (tipo budget-viaggio)(risposta ?a&:(and(> ?a 200)(< ?a 5001))))
        =>
        (assert(attributo(nome budget-viaggio)(valore ?a)(certezza 1.0)))
   )


   ;;------ REGIONI

    (defrule REGOLE::viaggio-regione-generico
        (preferenze (tipo viaggio-regione-generico)(risposta ?as))
        =>
        (assert(attributo(nome viaggio-regione-generico)(valore ?as)(certezza 1.0)))

    )

    (defrule REGOLE::viaggio-regione
        (preferenze (tipo viaggio-regione)(risposta ?as))
        =>
        (assert(attributo(nome viaggio-regione)(valore ?as)(certezza 1.0)))

    )

    (defrule REGOLE::viaggio-piu-regioni-generico-si

        (preferenze (tipo viaggio-piu-regioni-generico)(risposta si))
        =>
        (assert(attributo(nome viaggio-piu-regioni-generico)(valore si)(certezza 1.0)))
    )

    (defrule REGOLE::viaggio-piu-regioni-generico-no
        (preferenze (tipo viaggio-piu-regioni-generico)(risposta no))
        =>
        (assert(attributo(nome viaggio-piu-regioni-generico)(valore no)(certezza 1.0)))
        (assert(attributo(nome viaggio-più-regioni)(valore 1)(certezza 1.0)))
    )


    (defrule REGOLE::viaggio-più-regioni
        (preferenze (tipo viaggio-più-regioni)(risposta ?a&:(and(> ?a 2)(< ?a 7))))
        =>
        (assert(attributo(nome viaggio-più-regioni)(valore ?a)(certezza 1.0))) 
    )



    ;;---------  NUMERO LOCATION
    (defrule REGOLE::numero-location
        (preferenze (tipo numero-location) (risposta ?s&:(and(>= ?s 1)(< ?s 8))))         
        =>
        
        (assert(attributo(nome numero-location)(valore ?s)(certezza 1.0)))
    )

    (defrule REGOLE::numero-location-generico
        (preferenze (tipo numero-location-generico)(risposta si))
        =>
        (assert(attributo(nome numero-location-generico)(valore si)(certezza 1.0)))
    )

    (defrule REGOLE::numero-location-generico
        (preferenze (tipo numero-location-generico)(risposta no))
        =>
        (assert(attributo(nome numero-location-generico)(valore no)(certezza 1.0)))
    )

;;-------------- MANGIARE
    (defrule REGOLE::mangiare
        (preferenze (tipo enogastronomico)(risposta ?a&:(and (> ?a 0)(< ?a 6))))
    =>
        ;; valore massimo 0.6  
        (assert (attributo (nome tipo-turismo)(valore enogastronomico)(certezza (* 0.6 (/ ?a 5)))))
    )

;;--------------RELIGIONE
    (defrule REGOLE::religione
        (preferenze (tipo religione)(risposta ?a&:(and (> ?a 0)(< ?a 6))))
    =>
        
        (assert (attributo (nome tipo-turismo)(valore religioso)(certezza (* 0.6 (/ ?a 5)))))
    )
;;--------------CULTURA
    (defrule REGOLE::cultura
        (preferenze (tipo culturale)(risposta ?a&:(and (> ?a 0)(< ?a 6))))
    =>
       
        (assert (attributo (nome tipo-turismo)(valore culturale)(certezza (* 0.6 (/ ?a 5)))))
    )

;;--------------MONTAGNA

    (defrule REGOLE::montagna-si
        (preferenze (tipo montagna)(risposta si))
        (attributo(nome periodo-viaggio)(valore ?s))

    =>
        (bind ?coeff 0.0)
        (if (eq ?s inverno) then 
            (bind ?coeff 0.1)
        )
        (if (eq ?s estate) then 
            (bind ?coeff -0.1)
        )

        (assert (attributo (nome tipo-turismo)(valore montano)(certezza (+ 0.6 ?coeff))))
    )
    
    (defrule REGOLE::montagna-no
        (preferenze (tipo montagna)(risposta no))
    =>
        (assert (attributo (nome tipo-turismo)(valore montano)(certezza -1.0)))
    )

;;--------------NATURALISTIC
    (defrule REGOLE::naturalistico
        (preferenze (tipo naturalistico)(risposta ?a&:(and (> ?a 0)(< ?a 6))))
    =>

        (assert (attributo (nome tipo-turismo)(valore naturalisticoo)(certezza (* 0.6 (/ ?a 5)))))
    )
;;--------------BALNEARE-LACUSTRE
    (defrule REGOLE::balneare
        (preferenze (tipo balneare-lacustre)(risposta balneare))
        (attributo(nome periodo-viaggio)(valore ?s))
    =>
        (bind ?coeff 0.0)
        (if (eq ?s inverno) then 
            (bind ?coeff -0.1)
        )
        (if (eq ?s estate) then 
            (bind ?coeff 0.1)
        )
        (assert (attributo (nome tipo-turismo)(valore balneare)(certezza (+ 0.6 ?coeff))))
        (assert (attributo (nome tourist-tipo)(valore lacustre)(certezza 0.4)))
    )
    (defrule REGOLE::lacustre
        (preferenze (tipo balneare-lacustre)(risposta lacustre))
    =>
        (assert (attributo (nome tipo-turismo)(valore balneare)(certezza 0.4)))
        (assert (attributo (nome tourist-tipo)(valore lacustre)(certezza 0.6)))
    )
;;--------------SPORT-TERMALE
    (defrule REGOLE::termale
        (preferenze (tipo sport-termale)(risposta relax))
    =>
        (assert (attributo (nome tipo-turismo)(valore sportivo) (certezza 0.2)))
        (assert (attributo (nome tipo-turismo)(valore termale) (certezza 0.4)))
  
    )
       (defrule REGOLE::sport
        (preferenze (tipo sport-termale)(risposta sport))
    =>
        (assert (attributo (nome tipo-turismo)(valore sportivo) (certezza 0.4)))
        (assert (attributo (nome tipo-turismo)(valore termale) (certezza 0.2)))
  
    )

;;--------------COSTO
    (defrule REGOLE::costo-economico
        (preferenze (tipo costo)(risposta economico))
    =>
        (assert (attributo (nome stelle-hotel)(valore 1)(certezza 0.4)))
        (assert (attributo (nome stelle-hotel)(valore 2)(certezza 0.2)))
        (assert (attributo (nome stelle-hotel)(valore 3)(certezza -0.2)))
        (assert (attributo (nome stelle-hotel)(valore 4)(certezza -0.4)))
    )
    (defrule REGOLE::costo-normale 
        (preferenze (tipo costo)(risposta normale))
    =>
        (assert (attributo (nome stelle-hotel)(valore 1)(certezza -0.2)))
        (assert (attributo (nome stelle-hotel)(valore 2)(certezza 0.4)))
        (assert (attributo (nome stelle-hotel)(valore 3)(certezza 0.4)))
        (assert (attributo (nome stelle-hotel)(valore 4)(certezza -0.2)))
    )
      (defrule REGOLE::costo-costoso
        (preferenze (tipo costo)(risposta costoso))
    =>
        (assert (attributo (nome stelle-hotel)(valore 1)(certezza -0.4)))
        (assert (attributo (nome stelle-hotel)(valore 2)(certezza -0.2)))
        (assert (attributo (nome stelle-hotel)(valore 3)(certezza 0.2)))
        (assert (attributo (nome stelle-hotel)(valore 4)(certezza 0.4)))
    )
    ;;--------------PEOPLE NUMB

    (defrule numero-persone
        (preferenze (tipo numero-persone)(risposta ?a))
    =>
        (assert (attributo (nome numero-persone)(valore ?a)(certezza 1.0)))
    )




;;------------------------------------ MODULE VALUTAZIONE 

(defmodule VALUTAZIONE (import COMMON ?ALL) (import HOTEL ?ALL)(import LOCATION ?ALL))


;;------ VALUTAZIONE HOTEL

(defrule VALUTAZIONE::valutazione-hotel-per-stelle
    (attributo (nome stelle-hotel)(valore ?s)(certezza ?cf))
    (hotel (nome ?h)(location ?l)(stelle ?s))
=>
    (assert (attributo (nome hotel-in ?l) (valore ?h)(certezza (* (/ ?cf 0.4) 0.7))))
)

(defrule VALUTAZIONE::valutazione-hotel-per-disponibilità
    (attributo (nome numero-persone)(valore ?p))
    (hotel (nome ?h) (location ?l) (posti-liberi ?e&:(> ?e ?p)) (capacità ?c))
=>
    (bind ?ncf (* 0.7 (/ ?e ?c)))
    (assert (attributo (nome hotel-in ?l)(valore ?h)(certezza ?ncf)))
)

(defrule VALUTAZIONE::valutazione-hotel-per-zero-disponibilità    
    (attributo (nome numero-persone) (valore ?p))
    (hotel (nome ?h) (location ?l) (posti-liberi ?e&:(< ?e ?p)))
=>
    (assert (attributo (nome hotel-in ?l) (valore ?h) (certezza -1.0)))
)



;;------ VALUTAZIONE LOCATION

 (defrule VALUTAZIONE::valutazione-location-per-tipo
        (attributo (nome tipo-turismo)(valore ?v)(certezza ?a))
        (location-turistica(nome-location ?l)(tipo-turismo ?v)(score ?cf))
        =>
        (assert(attributo(nome valutazione-tipo-turismo)(valore ?l)(certezza (/ (* ?a ?cf) (* ?a 5)))))
 )

 (defrule VALUTAZIONE::valutazione-location-per-regione
        (attributo(nome viaggio-regione)(valore ?l)(certezza ?a))
        (location (nome ?na)(regione ?l))
        =>
        (assert(attributo(nome valutazione-tipo-turismo)(valore ?na)(certezza ?a)))
 )


;;----------------------------- PATH

(defmodule PATH (import COMMON ?ALL) (import HOTEL ?ALL)(import LOCATION ?ALL)(export ?ALL))

(deftemplate PATH::miglior-hotel-della-location
    (slot nome-location)
    (slot miglior-hotel)
    (slot score(type FLOAT))

)

(defrule PATH::calcolo-miglior-hotel-della-location
        (location (nome ?r))
        (attributo (nome hotel-in ?r) (valore ?h) (certezza ?hcf))
        (not (attributo (nome hotel-in ?r) (valore ?h2&~?h) (certezza ?hcf2&:(> ?hcf2 ?hcf))))
        (hotel (nome ?h) (location ?r) (stelle ?s))
        =>

        (assert(miglior-hotel-della-location(nome-location ?r)(miglior-hotel ?h)(score ?hcf)))
)


;; Crea tutti i path composti da una sola città
(defrule PATH::costruzione-singleton-path-semplice
    (location (nome ?r)(regione ?reg))    
    =>
    (assert (path (locations ?r)(regioni ?reg) (num-citta 1) (distanza-totale 0.0)(score 0.0)))
)


;; Ritorna tutti i path con numero location < durata-viaggio in cui le distanze tra una citta e l'altra sia minore di max-km-GG e il totale dei km sia minore di durata-viaggio * max-km-GG < del itinerario
(defrule PATH::costruzione-path
    (path (locations $?rs ?lr)(regioni $?regioni) (num-citta ?len) (distanza-totale ?td)(score ?scr))    
    (attributo (nome durata-viaggio) (valore ?tl))
    (test (<= (+ ?len  1) ?tl)) ;vincolo numero giorni
    (loc-to-loc (location-src ?lr) (location-dst ?nr) (distanza ?d)) 
    (location (nome ?nr)(regione ?reg))
    (test (< ?d ?*MAX-KM-GG*)) ;;vincolo distanza giornaliera
    (test (eq (member$ ?nr (create$ ?rs ?lr)) FALSE))
    
    =>
    (if (< (+ ?td ?d) (* ?*MAX-KM-GG* ?tl)) then
        (assert (path (locations ?rs ?lr ?nr)(regioni ?regioni ?reg) (num-citta (+ ?len 1)) (distanza-totale (+ ?td ?d))(score ?scr)))
    )    
)


;; I path simili sono quelli aventi stesse città ma ordine diverso, stabiliamo quindi che un itinerario è un insieme di città in cui non è importante l'ordine
(defrule PATH::eliminazione-path-simili
    ?p1 <- (path (path-id ?id1)(locations $?rs))
    ?p2 <- (path (path-id ?id2&:(neq ?id2 ?id1))(locations $?rs1))
    (test (subsetp ?rs ?rs1))
    (test (subsetp ?rs1 ?rs))
    =>
    (retract ?p1)
)

(defrule PATH::pruning-numero-location-nel-path
    (attributo (nome numero-location) (valore ?tl))
    ?p <- (path (num-citta ?len))
    ;; Se la differenza tra la durata del itinerario e la durata del path è maggiore di 1
    (test (neq ?len ?tl))
    =>
    (retract ?p)
)




;;----------------------------------ITINERARIO

(defmodule COSTRUZIONE-ITINERARIO(import COMMON ?ALL) (import HOTEL ?ALL) (import ITINERARIO ?ALL)(import PATH ?ALL))


(deffunction COSTRUZIONE-ITINERARIO::calcolo-data-viaggio (?periodo)

    (bind ?mesiEstivi (create$ luglio agosto settembre maggio giugno))
    (bind ?mesiInvernali (create$ ottobre novembre dicembre gennaio febbraio))

    (seed (round (time)))
    (bind ?giorno (random 1 28))
    (bind ?meseScelto nessuno)

    
    (if (eq ?periodo indifferente) then
        (seed (round (time)))
        (bind ?i (random 1 2))
        (if (eq ?i 1) then
            (bind ?periodo estate)
        )

        (if (eq ?i 2) then
            (bind ?periodo inverno)
        )
    )

    (if (eq ?periodo estate) then
        (seed (round (time)))
        (bind ?meseScelto (nth$ (random 1 5) ?mesiEstivi))

    )

    (if (eq ?periodo inverno) then
        (seed (round (time)))
        (bind ?meseScelto (nth$ (random 1 5) ?mesiInvernali))
    )

    (bind ?return (str-cat ?giorno " - " ?meseScelto " 2020"))
    (return ?return)

)

(defrule COSTRUZIONE-ITINERARIO::media-cf-location
    (declare (salience 500))
    (attributo (nome valutazione-tipo-turismo))
=>
    (bind ?sum 0)
    (bind ?count 0)
    (do-for-all-facts ((?f attributo)) (eq ?f:nome (create$ tipo-turismo))
        (bind ?sum (+ ?sum ?f:certezza))
        (bind ?count (+ ?count 1))
    )
    (assert (media-cf-location (valore (/ ?sum ?count))))
)


;; un path viene bannato se le città che lo pongono sono sotto la media o non esistono hotel con cf maggiore di 0.2
(defrule COSTRUZIONE-ITINERARIO::path-pruning-per-media
    (declare (salience 400))
    (media-cf-location (valore ?a))
    (path (path-id ?id) (locations $?rl ?r $?rr))

    (or (attributo (nome valutazione-tipo-turismo) (valore ?r) (certezza ?cfr&:(< ?cfr ?a)))
        (not (attributo (nome hotel-in ?r) (certezza ?cfh&:(>= ?cfh 0.2)))))
=>
    (assert (path-bannati (path-id ?id)))
)


(defrule COSTRUZIONE-ITINERARIO::costruzione-itinerario
    (declare (salience 300))    
    (path (path-id ?id) (locations $?rs) (num-citta ?len)(score ?scr))
    (not (path-bannati (path-id ?id)))
    (attributo (nome durata-viaggio) (valore ?ds))
=>
    (assert (itinerario (itinerario-id ?id) (locations ?rs)(distanza-totale ?len)(durata ?len)))
)


;; Inserisce al campo giorni il minimo periodo di giorni in una citta, avremo un vettore in cui ci saranno tanti 1 quanti sono le città
(defrule COSTRUZIONE-ITINERARIO::inserimento-giorni-itinerario-basico
    (iterazione (i ?i))
    (test (eq ?i 0))
    ?t <- (itinerario (locations $?rl ?r $?rr)(giorni $?d))

    =>
    (bind ?count (member$ ?r (create$ $?rl ?r $?rr)))
    (modify ?t (giorni (replace$ ?d ?count ?count 1)))

)
;; Per ogni itinerario andiamo ad aggiungere il vettore dei miglior hotel per città e in base al numero di persone andiamo a calcolare i costi
(defrule COSTRUZIONE-ITINERARIO::modifica-itinerario-per-hotel-costi
    (declare (salience 0))
    ?t <- (itinerario (locations $?rl ?r $?rr) (hotels $?hs) (giorni $?ds) (costi $?cs)(hotel-score $?hscore))
    (miglior-hotel-della-location(nome-location ?r)(miglior-hotel ?h)(score ?hcf))
    (hotel (nome ?h)(location ?r)(stelle ?s))
    (attributo (nome numero-persone) (valore ?p))    
    =>
    (bind ?count (member$ ?r (create$ ?rl ?r ?rr))) ;; assegna a count la posizione di ?r rispetto la lista delle location del itinerario
    (bind ?daily-cost (+ ?*HOTEL-COSTO-BASE* (* ?s ?*HOTEL-COSTO-ADDIZIONALE*)))
    (bind ?cost-all-people (* (max 1 (div ?p 2)) ?daily-cost))
    (modify ?t (hotels (replace$ ?hs ?count ?count ?h))(costi (replace$ ?cs ?count ?count ?cost-all-people))(hotel-score (replace$ ?hscore ?count ?count ?hcf))) ;;il replace vuole 4 argomenti il primo è quello da sotituire, l'ultimo quello che rimpiazza il primo, quelli di mezzo sono interi che indicano l'indice del valore del multislot da sostituire 
)


(defrule COSTRUZIONE-ITINERARIO::scelta-periodo-viaggio
    (declare (salience 5))
    (iterazione (i ?i))
    (attributo(nome periodo-viaggio)(valore ?val))
    =>   
    (bind ?data (calcolo-data-viaggio ?val))
    (assert(attributo (nome periodo-singolo-viaggio)(valore ?data)))
)


(defmodule VALUTAZIONE-ITINERARIO (import PATH ?ALL)(import COMMON ?ALL) (import HOTEL ?ALL)(import ITINERARIO ?ALL)(import LOCATION ?ALL))


(defrule VALUTAZIONE-ITINERARIO::pruning-numero-location-nell-itinerario
    (declare (salience 10000))
    (attributo (nome numero-location) (valore ?tl))
    ?p <- (itinerario (locations $?rl))
    ;; Se la differenza tra la durata del itinerario e la durata del path è maggiore di 1
    (test (neq (length$ ?rl) ?tl))
    =>
    (retract ?p)
)

(defrule VALUTAZIONE-ITINERARIO::eliminazione-itinerari-simili
    (declare (salience 10000))
    ?p1 <- (itinerario (itinerario-id ?id1)(locations $?rs))
    ?p2 <- (itinerario (itinerario-id ?id2&:(neq ?id2 ?id1))(locations $?rs1))
    (test (eq $?rs $?rs1))  
    =>
    (retract ?p1)
)


;; Per ogni itinerario andiamo ad aggiungere il vettore contenente gli score delle citta che lo compongono
(defrule VALUTAZIONE-ITINERARIO::aggiunta-location-score
    (declare (salience 1000))
    ?t <- (itinerario (locations $?rl ?r $?rr)(location-score $?ls))
    (attributo (nome valutazione-tipo-turismo)(valore ?r)(certezza ?cf))
    =>
    (bind ?count (member$ ?r (create$ ?rl ?r ?rr))) ;; assegna a count la posizione di ?r rispetto la lista delle location del itinerario
    (modify ?t (location-score (replace$ ?ls ?count ?count ?cf))) ;;il replace vuole 4 argomenti il primo è quello da sotituire, l'ultimo quello che rimpiazza il primo, quelli di mezzo sono interi che indicano l'indice del valore del multislot da sostituire 
)

;;--- Funzione che ripartisce i giorni rimanenti (?remainDay) in base allo score della lista
(deffunction VALUTAZIONE-ITINERARIO::lista-giorni(?numberOfLoc ?remainDay $?itinerarioLocScore) 
    (bind ?supportList (create$ 0 0 0 0 0))
    (bind ?counter 1)
    (loop-for-count ?numberOfLoc
        (bind ?supportList (replace$ ?supportList ?counter ?counter 1))
        (bind ?counter (+ ?counter 1))
    )
    (bind ?numberDay ?remainDay)
    (loop-for-count ?numberDay
        (if (> ?remainDay 0) then
            (bind ?maxvalore (max (expand$ ?itinerarioLocScore)))
            (bind ?index (member$ ?maxvalore ?itinerarioLocScore))
            (bind ?ls2 ?maxvalore)
          

            (bind ?supportvalore (nth$ ?index ?supportList))
            (bind ?supportList (replace$ ?supportList ?index ?index (+ ?supportvalore 1)))
            (bind ?remainDay (- ?remainDay 1))
            (bind ?itinerarioLocScore (replace$ ?itinerarioLocScore ?index ?index (- ?maxvalore  0.2)))
        
        )
    
    )
    (return ?supportList)
)

;; Assegnazione lista giorni per città
(defrule VALUTAZIONE-ITINERARIO::modifica-giorni-itinerario-by-hotel
    (declare (salience 100))
    ?t <- (itinerario (locations $?rl ?r $?rr)(giorni $?d)(distanza-totale ?td)(hotel-score $?hotelScore)(durata ?dur))
    (attributo (nome durata-viaggio)(valore ?len))    
    (test (< ?dur ?len))
    =>
    (bind ?rd (- ?len ?td))
    (bind ?supportList (lista-giorni (length$ (create$ $?rl ?r $?rr)) ?rd ?hotelScore))
    (modify ?t (giorni ?supportList)(durata ?len))
)

;; Funzione che ritorta true se la somma di una lista di interi è uguale ad un numero
(deffunction VALUTAZIONE-ITINERARIO::sum-list-equal-integer (?integer $?lista)
    (bind ?result 0)
    (foreach ?number $?lista
        (bind ?result (+ ?result ?number))
    )

    (return (eq ?result ?integer))
)

;;Rule che printa i itinerario che hanno la somma dei giorni di un itinerario diversa dall'attributo durata-viaggio 
(defrule VALUTAZIONE-ITINERARIO::check-numero-giorni
    (declare (salience 20))
    ?t <- (itinerario (locations $?rl ?r $?rr)(giorni $?d)(distanza-totale ?td)(hotel-score $?hotelScore)(durata ?dur))
    =>
    (bind ?result (sum-list-equal-integer ?dur ?d))
    (if (eq ?result FALSE) then
        (printout t "---------ERRORE nella lista: ")
        (printout t (create$ $?rl ?r $?rr))
        (printout t "  durata-viaggio: ")
        (printout t ?dur)
        (printout t "  è diverso dalla durata del itinerario: ")
        (printout t ?d crlf)
    )   

)



(defrule VALUTAZIONE-ITINERARIO::valutazione-itinerario-by-locations
    (declare (salience 10))   
    (itinerario (itinerario-id ?id)(locations $?ll ?l $?lr)(giorni $?ds)(durata ?dr)(hotel-score $?hhs)(location-score $?lls)(distanza-totale ?nl&:(> ?nl 1)))
    (attributo (nome valutazione-tipo-turismo)(valore ?l)(certezza ?lcf))
    (attributo (nome durata-viaggio)(valore ?td))
=>
    (bind ?locLen (length$ (create$ $?ll ?l $?lr)))
    (bind ?count (member$ ?l (create$ ?ll ?l ?lr)))
    (bind ?d (nth$ ?count ?ds))
    (bind ?ncf ?lcf)
    (assert (attributo (nome il-viaggio)(valore ?id)(certezza (+ ?ncf (* ?nl 0.017)))  ))

)

(defrule VALUTAZIONE-ITINERARIO::valutazione-itinerario-by-hotels
    (declare (salience 15))   
    (itinerario (itinerario-id ?id)(locations $?ll ?l $?lr)(hotels $?hs)(giorni $?ds)(durata ?dr)(score-totale ?ts)(hotel-score $?hhs)(location-score $?lls)(distanza-totale ?nl&:(> ?nl 1)))
    (attributo (nome hotel-in ?l)(valore ?h)(certezza ?hcf)) ;; valutare se conviene usare l'attributo miglior-hotel-in
    (test (eq ?h (nth$ (member$ ?l (create$ ?ll ?l ?lr)) ?hs)))
    (attributo (nome durata-viaggio)(valore ?td))
=>
    (bind ?locLen (length$ (create$ $?ll ?l $?lr)))
    (bind ?count (member$ ?l (create$ ?ll ?l ?lr)))
    (bind ?d (nth$ ?count ?ds))
    (bind ?ncf ?hcf)
    (assert (attributo (nome il-viaggio) (valore ?id) (certezza (+ ?ncf (* ?nl 0.015)))))
)

(defrule VALUTAZIONE-ITINERARIO::valutazione-itinerario-con-specifica-regione
    (declare (salience 10))
    (itinerario (itinerario-id ?id)(locations $?ll ?l $?lr)(hotels $?hs)(giorni $?ds)(durata ?dr)(score-totale ?ts)(hotel-score $?hhs)(location-score $?lls)(distanza-totale ?nl&:(> ?nl 1)))
    (attributo (nome viaggio-regione)(valore ?r))
    (location (nome ?l)(regione ?r))
    =>
    (assert (attributo (nome il-viaggio) (valore ?id) (certezza 1.0)))    

)

;; ritorna true se il path contiene solo una regione
(deffunction VALUTAZIONE-ITINERARIO::path-piu-regioni ($?reg)
    (bind ?isTrue TRUE)
    (bind ?firstRegion (nth$ 1 ?reg))
    (foreach ?regione $?reg
        (bind ?isTrue (and ?isTrue (eq ?firstRegion ?regione)))
    )
    (return ?isTrue)

)

(defrule VALUTAZIONE-ITINERARIO::valutazione-itinerario-piu-regioni
    (declare (salience 10))
    (attributo (nome viaggio-piu-regioni-generico)(valore si))
    (itinerario (itinerario-id ?id)(locations $?loc))
    (test (> (length$ ?loc) 1))
    (path (path-id ?id)(regioni $?reg))
    =>
    (bind ?support (path-piu-regioni ?reg))
    (if (eq ?support TRUE) then
        (assert (attributo (nome il-viaggio) (valore ?id) (certezza 0.2)))  
    )

    (if (eq ?support FALSE) then
        (assert (attributo (nome il-viaggio) (valore ?id) (certezza 0.90)))  
    )

)

(defrule VALUTAZIONE-ITINERARIO::valutazione-itinerario-una-regione
    (declare (salience 10))
    (attributo (nome viaggio-piu-regioni-generico)(valore no))
    (itinerario (itinerario-id ?id)(locations $?loc))
    (test (> (length$ ?loc) 1))
    (path (path-id ?id)(regioni $?reg))
    =>
    (bind ?support (path-piu-regioni ?reg))
    (if (eq ?support TRUE) then
        (assert (attributo (nome il-viaggio) (valore ?id) (certezza 0.90)))  
    )

    (if (eq ?support FALSE) then
        (assert (attributo (nome il-viaggio) (valore ?id) (certezza 0.20)))  
    )

)

(defrule VALUTAZIONE-ITINERARIO::eliminazione-viaggi
    (declare (salience 0))
    (attributo (nome il-viaggio) (valore ?id) (certezza ?cf))
    (attributo (nome il-viaggio) (valore ?id) (certezza ?cfd))
    (test (neq ?cf ?cfd))
    =>
    (printout t  "Esistono piu viaggi: " ?id  crlf)
)



(deffunction VALUTAZIONE-ITINERARIO::sum-integer-list ($?lista)
    (bind ?result 0)
    (foreach ?number $?lista
        (bind ?result (+ ?result ?number))
    )
    return ?result
)

(defrule VALUTAZIONE-ITINERARIO::valutazione-itinerario-by-budget
    (declare (salience 10))
    (attributo (nome budget-viaggio)(valore ?r))
    (itinerario (itinerario-id ?id)(distanza-totale ?nl&:(> ?nl 1))(costi $?costi))
    =>
    (bind ?tc (sum-integer-list ?costi))
    (if (< ?tc ?r) then
        (assert (attributo (nome il-viaggio) (valore ?id) (certezza 0.90)))
    )
)

(defrule VALUTAZIONE-ITINERARIO::pruning-itinerario-by-budget
    (declare (salience 10))
    (attributo (nome budget-viaggio)(valore ?r))
    ?t <- (itinerario (itinerario-id ?id)(distanza-totale ?nl&:(> ?nl 1))(costi $?costi))
    =>
    (bind ?tc (sum-integer-list ?costi))
    (if (> ?tc ?r) then
        (retract ?t)
    )

)


;;---- Valutazione-itinerario
(defrule valutazione-itinerario-per-numero-location    
    (attributo (nome numero-location)(valore ?v))
    (itinerario (itinerario-id ?id)(locations $?loc))
    (test (eq (length$ ?loc) ?v))
    =>
    (assert (attributo (nome il-viaggio) (valore ?id) (certezza 0.90)))
)



(defmodule STAMPA-RISULTATI (import COMMON ?ALL) (import ITINERARIO ?ALL))

    (defrule numero-itinerario    
        (declare (salience 0))
        (iterazione (i ?i))
    =>    
        (bind ?count 0)
        (do-for-all-facts ((?f itinerario)) TRUE
            (bind ?count (+ ?count 1))
        )
        (printout t "---------------------------------------------------------- numero itinerari: " ?count crlf)
    )


(defrule intestazione-risultati
   (declare (salience 500))
   (iterazione (i ?i))
=>
   (printout t  crlf crlf)
   (printout t  "               888888b.                                                         888 "crlf            
                "               888   88b                                                        888 "  crlf           
                "               888  .88P                                                        888 "       crlf      
                "               8888888K.   .d88b.  88888b.  888  888  .d88b.  88888b.  888  888 888888  .d88b."   crlf 
                "               888   Y88b d8P  Y8b 888  88b 888  888 d8P  Y8b 888  88b 888  888 888    d88  88b " crlf 
                "               888    888 88888888 888  888 Y88  88P 88888888 888  888 888  888 888    888  888 " crlf 
                "               888   d88P Y8b.     888  888  Y8bd8P  Y8b.     888  888 Y88b 888 Y88b.  Y88..88P " crlf 
                "               8888888P     Y8888  888  888   Y88P     Y8888  888  888   Y88888   Y888   Y88P "  crlf crlf )    
   (printout t "******************************   ITINERARI SCELTI DA NOI (ITERAZIONE " (+ ?i 1) ")  *****************************"  crlf)
   (printout t  crlf)
   (assert (printed-itinerarios 0))
)

(defrule stampa-e-rimozione-miglior-itinerario
    ?fact1 <- (printed-itinerarios ?p)
    (test (< ?p 5))
    ?fact2 <- (attributo (nome il-viaggio)(valore ?tid)(certezza ?tcf))
    (not (attributo (nome il-viaggio)(valore ?tid2&~?tid)(certezza ?tcf2&:(> ?tcf2 ?tcf))))
    (test (> ?tcf ?*MIN-PRINT-CF*))
    ?fact3 <- (itinerario (itinerario-id ?tid)(locations $?lc)(hotels $?hs)(giorni $?ds)(costi $?cs)(durata ?dr))
    (attributo (nome periodo-singolo-viaggio)(valore ?data))
 
=>
    (retract ?fact1)
    (assert (printed-itinerarios (+ ?p 1)))
    (retract ?fact2)
    (bind ?total-cost (+ (expand$ ?cs) 0))
    (printout t  crlf)
    (printout t "Itinerario suggerio numero: " (+ ?p 1) " con certezza: " (/ (round (* ?tcf 1000)) 10) "%" crlf)
    (printout t "******************************************************************************************************" crlf)
    (printout t  crlf)
    (printout t "*  - Location da visitare: " ?lc crlf)
    (printout t "*  - Hotels: " (subseq$ ?hs 1 ?dr ) crlf)
    (printout t "*  - Partizionamente giorni: " ?ds crlf)
    (printout t "*  - Costi giornalieri: " (subseq$ ?cs 1 ?dr ) "  |  Costo Totale: " ?total-cost crlf) 
    (printout t "*  - Data viaggio: " ?data crlf)
    (printout t  crlf)
    (printout t "******************************************************************************************************" crlf)
    (printout t  crlf)
    (printout t  crlf)
    (retract ?fact3)


)

(defmodule REGOLE-FINALI (import COMMON ?ALL) (import ITINERARIO ?ALL))

(defrule REGOLE-FINALI::incrementa-iterazione
    (declare (salience 1000))
    ?fact <- (iterazione (i ?i))
=>
    (retract ?fact)
    (assert (iterazione (i (+ ?i 1))))
    ;; Toglie dal focus REGOLE-FINALI di modo da iniziare un altro ciclo
    (pop-focus)
)

 
