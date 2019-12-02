
;;--------------- COMMON

(defmodule COMMON (export ?ALL))

(defglobal

    ?*MAX-KM-GG* = 250
    ?*HOTEL-COSTO-BASE* = 50
    ?*HOTEL-COSTO-ADDIZIONALE* = 25    
    ?*MIN-PRINT-CF* = 0.20
)

(deftemplate attributo
    (multislot nome)
    (slot valore)
    (slot certezza (type FLOAT) (range -1.0 1.0) (default 0.0))

)

(deftemplate iterazione
    (slot i (type INTEGER))
)

(deffacts iterazione
    (iterazione (i 0))
)

;;--------------- COMBINE CERTAINTIES 
  
(defrule COMMON::combine-certainties-both-positive
    (declare (auto-focus TRUE))
    ?fact1 <- (attributo (nome $?d) (valore ?v) (certezza ?C1&:(>= ?C1 0.0)))
    ?fact2 <- (attributo (nome $?d) (valore ?v) (certezza ?C2&:(>= ?C2 0.0)))
    (test (neq ?fact1 ?fact2))
=>
    (retract ?fact1)
    (bind ?C3 (- (+ ?C1 ?C2) (* ?C1 ?C2)))
    (modify ?fact2 (certezza ?C3))
)
    
(defrule COMMON::combine-certainties-both-negative
    (declare (auto-focus TRUE))
    ?fact1 <- (attributo (nome $?d) (valore ?v) (certezza ?C1&:(<= ?C1 0.0)))
    ?fact2 <- (attributo (nome $?d) (valore ?v) (certezza ?C2&:(<= ?C2 0.0)))

    (test (neq ?fact1 ?fact2))
=>
    (retract ?fact1)
    (bind ?C3 (+ (+ ?C1 ?C2) (* ?C1 ?C2)))
    (modify ?fact2 (certezza ?C3))
)

(defrule COMMON::combine-certainties-negative-positive
    (declare (auto-focus TRUE))
    ?fact1 <- (attributo (nome $?d) (valore ?v) (certezza ?C1))
    ?fact2 <- (attributo (nome $?d) (valore ?v) (certezza ?C2))

    (test (neq ?fact1 ?fact2))
    (test (< (* ?C1 ?C2) 0.0))
=>
    (retract ?fact1)
    (bind ?C3 (/ (+ ?C1 ?C2) (- 1 (min (abs ?C1) (abs ?C2) ))))
    (modify ?fact2 (certezza ?C3))
)


;;----------------- DOMANDE

(defmodule DOMANDE (export ?ALL))

   
(deftemplate DOMANDE::preferenze
    (slot tipo)
    (slot risposta)
)

(deftemplate DOMANDE::domanda
    (slot importanza (type INTEGER)) ;; Un valore da 0-3 per indicare l'importanza della domanda la domanda 0 verrà fatta prima della domanda 3
    (slot descrizione (default ?NONE)) ;;ho viaggio-piu-regioni-genericocambiato attributo in descrizione per non confondersi con gli attributo asseriti
    (slot la-domanda (default ?NONE))
    (slot tipo (default normal))
    (multislot risposte-valide (default ?NONE))
    (slot skippable (default TRUE))
    (slot chiesto (default FALSE))
    (multislot precursori (default ?DERIVE))
)

(deffacts DOMANDE::lista-domande
     ;; Iterazione 0
    (domanda (descrizione periodo-viaggio)(importanza 0)(la-domanda " In quale periodo vorresti partire? [estate inverno indifferente] ")(risposte-valide estate inverno indifferente))
    (domanda (tipo range)(descrizione durata-viaggio)(importanza 0) (la-domanda "Quanti giorni vuoi che la vacanza duri? valore tra [1,7]") (risposte-valide 1 7))
    (domanda (tipo range)(descrizione numero-persone)(importanza 0)(la-domanda "Quante persone vogliono andare in vacanza? tra [2,10] ")(risposte-valide 2 10))
    (domanda (descrizione costo)(importanza 0)(la-domanda " Vuoi fare un viaggo economico o più costoso? [economico normale costoso] ")(risposte-valide economico normale costoso))

    ;; Iterazione 1
    (domanda (descrizione budget-viaggio-generico)(importanza 1) (la-domanda "Hai un budget massimo? [Si, No]") (risposte-valide si no))
    (domanda (tipo range)(descrizione budget-viaggio)(importanza 1) (la-domanda "Qual'è il tuo budget? valore tra [200,5000]") (risposte-valide 200 5000)(precursori budget-viaggio-generico is si))
    (domanda (descrizione viaggio-piu-regioni-generico)(importanza 1) (la-domanda "Vuoi visitare più regioni? [Si, No]") (risposte-valide Si No si no))
    (domanda (tipo range)(descrizione enogastronomico)(importanza 1)(la-domanda "Quanto è importante per te il buon cibo? tra [1,5] ")(risposte-valide 1 5))
    (domanda (tipo range)(descrizione religioso)(importanza 1)(la-domanda "Quanto è importante per te l'aspetto religioso di una località? tra [1,5]")(risposte-valide 1 5))
    (domanda (tipo range)(descrizione culturale)(importanza 1)(la-domanda "Quanto è importante per te l'aspetto culturale di una località? tra [1,5]")(risposte-valide 1 5))
    (domanda (descrizione montagna)(importanza 1)(la-domanda " Ti piacciono i luoghi montani? [si,no] " )(risposte-valide si no))
    (domanda (tipo range)(descrizione naturalistico)(importanza 1)(la-domanda " Quanto dai importanza all'aspetto naturalisticoo di una localita' ? tra [1,5] ")(risposte-valide 1 5))
    (domanda (descrizione balneare-lacustre)(importanza 1)(la-domanda " Preferisci un turismo balneare o di tipo lacustre ? [balneare lacustre]" )(risposte-valide balneare lacustre))
    (domanda (descrizione sport-termale)(importanza 1)(la-domanda " In vacanza vuoi rilassarti o mantenerti attivo facendo sport ? [relax sport] ")(risposte-valide relax sport))

    ;; Iterazione 2
    ;; Iterazione 3
    (domanda (descrizione numero-location-generico) (importanza 2) (la-domanda "Vuoi visitare meno location? [Si,No]") (risposte-valide Si No si no))
    (domanda (descrizione viaggio-regione-generico) (importanza 2) (la-domanda "Vuoi visitare qualche regionee in particolare? [Si,No]") (risposte-valide Si No si no))
    (domanda (descrizione viaggio-regione) (importanza 2) (la-domanda "Quale regionee vorresti visitare? ") (risposte-valide sicilia calabria puglia toscana liguria lombardia piemonte lazio)(precursori viaggio-regione-generico is si))
    (domanda (tipo range)(descrizione numero-location)(importanza 2)(la-domanda "Quante città vuoi visitare?")(risposte-valide 1 7)(precursori numero-location-generico is si))


)

;;MODULE LOCATION

(defmodule LOCATION (export ?ALL))

(deftemplate location
    (slot nome (default ?NONE))
    (slot regione (default ?NONE))
    (slot altitudine (type FLOAT))
    (slot longitudine (type FLOAT))
)

(deftemplate loc-to-loc
    (slot location-src (default ?NONE))
    (slot location-dst (default ?NONE))
    (slot distanza (type FLOAT))
)




(deffacts location-list
    (location (nome agrigento) (regione sicilia)(altitudine 37.31) (longitudine 12.58))
    (location (nome palermo) (regione sicilia)(altitudine 38.14) (longitudine 13.31))

    (location (nome reggio) (regione calabria)(altitudine 38.10) (longitudine 15.66))
    (location (nome salerno) (regione calabria)(altitudine 40.41) (longitudine 14.46))

    (location (nome lecce) (regione puglia)(altitudine 40.21) (longitudine 18.11))
    (location (nome pisa) (regione toscana)(altitudine 43.43) (longitudine 10.24))

    (location (nome milano) (regione lombardia)(altitudine 45.28) (longitudine 09.11)) 
    (location (nome bergamo) (regione lombardia)(altitudine 45.42) (longitudine 09.40))  
   
    (location (nome torino) (regione piemonte)(altitudine 45.04) (longitudine 07.42)) 
    (location (nome pinerolo) (regione piemonte)(altitudine 45.10) (longitudine 08.00))  
    
)


(defrule calcolo-distanza
    (location (nome ?n)(regione ?r)(altitudine ?a)(longitudine ?l))
    (location (nome ?n1&:(neq ?n1 ?n))(regione ?r1)(altitudine ?a1)(longitudine ?l1))
    =>

    ;; equaizione per calcolare km dati due punti  
    ;;R= 6372 R * arccos(sin(latA) * sin(latB) + cos(latA) * cos(latB) * cos(lonA-lonB))
    ;;(* R (acos(+ (* (sin latA)(cos latB)) (* (*(cos latA) (cos(latB))) (cos (- lonA LonB))))))
    ;; trasformazione decimale/radianti : 40° × π / 180°
    (bind ?a1_rad (* ?a1 (/ (pi) 180)))
    (bind ?a_rad (* ?a (/ (pi) 180)))
    (bind ?l1_rad (* ?l1 (/ (pi) 180)))
    (bind ?l_rad (* ?l (/ (pi) 180)))
    (bind ?km_distanza (* 6372.795 (acos (+ (* (sin ?a_rad)(sin ?a1_rad)) (* (* (cos ?a_rad) (cos ?a1_rad)) (cos (- ?l_rad ?l1_rad))) )) ))
    (assert(loc-to-loc(location-src ?n)(location-dst ?n1)(distanza ?km_distanza))) 

)

(deftemplate location-turistica
    (slot nome-location (default ?NONE))
    (slot tipo-turismo (default ?NONE))
    (slot score(type INTEGER) (range 1 5))
)

(deffacts location-turism-list
    (location-turistica (nome-location agrigento)(tipo-turismo balneare)(score  4))
    (location-turistica (nome-location agrigento)(tipo-turismo culturale)(score  2))
    (location-turistica (nome-location agrigento)(tipo-turismo enogastonomico)(score  3))
    (location-turistica (nome-location agrigento)(tipo-turismo religioso)(score  1))
    (location-turistica (nome-location agrigento)(tipo-turismo sportivo)(score  3))



    (location-turistica (nome-location palermo)(tipo-turismo balneare)(score  4))
    (location-turistica (nome-location palermo)(tipo-turismo religioso)(score  4))
    (location-turistica (nome-location palermo)(tipo-turismo sportivo)(score  4))
    (location-turistica (nome-location palermo)(tipo-turismo enogastonomico)(score  2))
    (location-turistica (nome-location palermo)(tipo-turismo culturale)(score  1))

    (location-turistica (nome-location reggio)(tipo-turismo montano)(score  3))
    (location-turistica (nome-location reggio)(tipo-turismo enogastonomico)(score  5))
    (location-turistica (nome-location reggio)(tipo-turismo lacustre)(score  5))
    (location-turistica (nome-location reggio)(tipo-turismo enogastonomico)(score  5))
    (location-turistica (nome-location reggio)(tipo-turismo lacustre)(score  5))

    (location-turistica (nome-location salerno)(tipo-turismo termale)(score  4))
    (location-turistica (nome-location salerno)(tipo-turismo naturale)(score  2))
    (location-turistica (nome-location salerno)(tipo-turismo montano)(score  3))
    (location-turistica (nome-location salerno)(tipo-turismo sportivo)(score  2))
    (location-turistica (nome-location salerno)(tipo-turismo lacustre)(score  3))

    (location-turistica (nome-location lecce)(tipo-turismo balneare)(score  5))
    (location-turistica (nome-location lecce)(tipo-turismo enogastonomico)(score  3))
    (location-turistica (nome-location lecce)(tipo-turismo lacustre)(score  2))
    (location-turistica (nome-location lecce)(tipo-turismo sportivo)(score  3))
    (location-turistica (nome-location lecce)(tipo-turismo montano)(score  2))
    
    (location-turistica (nome-location pisa)(tipo-turismo montano)(score  4))
    (location-turistica (nome-location pisa)(tipo-turismo naturale)(score  2))
    (location-turistica (nome-location pisa)(tipo-turismo termale)(score  4))
    (location-turistica (nome-location pisa)(tipo-turismo relioso)(score  2))
    (location-turistica (nome-location pisa)(tipo-turismo lacustre)(score  4))

    (location-turistica (nome-location milano)(tipo-turismo montano)(score  3))
    (location-turistica (nome-location milano)(tipo-turismo sportivo)(score  4))
    (location-turistica (nome-location milano)(tipo-turismo lacustre)(score  3))
    (location-turistica (nome-location milano)(tipo-turismo culturale)(score  4))
    (location-turistica (nome-location milano)(tipo-turismo enograstonomico)(score  3))

    (location-turistica (nome-location bergamo)(tipo-turismo montano)(score  4))
    (location-turistica (nome-location bergamo)(tipo-turismo enogastronomico)(score  3))
    (location-turistica (nome-location bergamo)(tipo-turismo sportivo)(score  4))
    (location-turistica (nome-location bergamo)(tipo-turismo religioso)(score  3))
    (location-turistica (nome-location bergamo)(tipo-turismo sportivo)(score  4))
    
    (location-turistica (nome-location torino)(tipo-turismo montano)(score  3))
    (location-turistica (nome-location torino)(tipo-turismo culturale)(score  3))
    (location-turistica (nome-location torino)(tipo-turismo sportivo)(score  3))
    (location-turistica (nome-location torino)(tipo-turismo religioso)(score  5))
    (location-turistica (nome-location torino)(tipo-turismo enogastronomico)(score  5))

    (location-turistica (nome-location pinerolo)(tipo-turismo montano)(score  4))
    (location-turistica (nome-location pinerolo)(tipo-turismo enogastonomico)(score  2))
    (location-turistica (nome-location pinerolo)(tipo-turismo termale)(score  2))
    (location-turistica (nome-location pinerolo)(tipo-turismo sportivo)(score  1))
    (location-turistica (nome-location pinerolo)(tipo-turismo religioso)(score  2))
)


;;MODULE HOTEL

(defmodule HOTEL (export ?ALL) (import LOCATION ?ALL))

(deftemplate HOTEL::hotel
    (slot nome (default ?NONE))
    (slot location (default ?NONE))
    (slot stelle (type INTEGER) (range 1 5))
    (slot posti-liberi (type INTEGER) (range 0 ?VARIABLE))
    (slot capacità (type INTEGER) (range 1 ?VARIABLE))
)

(deffacts lista-hotel
    
    (hotel (nome "hotel delle palme")(location palermo)(stelle 4)(posti-liberi 100)(capacità 300))
    (hotel (nome "hotel sirena")(location palermo)(stelle 2)(posti-liberi 50)(capacità 150))
    (hotel (nome "hotel empedocle")(location agrigento)(stelle 2)(posti-liberi 150)(capacità 300))
    (hotel (nome "hotel dei templi")(location agrigento)(stelle 4)(posti-liberi 90)(capacità 100))
    (hotel (nome "hotel piemonte")(location torino)(stelle 4)(posti-liberi 2)(capacità 40))
    (hotel (nome "hotel san carlo")(location torino)(stelle 3)(posti-liberi 10)(capacità 40))
    (hotel (nome "hotel del centro")(location milano)(stelle 4)(posti-liberi 150)(capacità 300))
    (hotel (nome "la vecchia taverna")(location milano)(stelle 2)(posti-liberi 15)(capacità 30))
    (hotel (nome "hotel reggio")(location reggio)(stelle 2)(posti-liberi 10)(capacità 30))
    (hotel (nome "hotel mirafiori")(location salerno)(stelle 4)(posti-liberi 15)(capacità 30))
    (hotel (nome "hotel marconi")(location bergamo)(stelle 2)(posti-liberi 0)(capacità 30))
    (hotel (nome "la spezia")(location lecce)(stelle 4)(posti-liberi 100)(capacità 300))
    (hotel (nome "hotel miramonti")(location lecce)(stelle 3)(posti-liberi 10)(capacità 30))
    (hotel (nome "hotel della torre")(location pisa)(stelle 4)(posti-liberi 3)(capacità 30))
    (hotel (nome "la lavandaia")(location milano)(stelle 3)(posti-liberi 10)(capacità 30))
    (hotel (nome "hotel regina")(location pinerolo)(stelle 4)(posti-liberi 30)(capacità 30))
    (hotel (nome "hotel carlo alberto")(location pinerolo)(stelle 2)(posti-liberi 10)(capacità 30))
    
)



(deftemplate path
    (slot path-id (default-dynamic (gensym*)))
    (multislot locations)
    (multislot regioni)
    (slot num-citta (type INTEGER))
    (slot distanza-totale (type FLOAT))
    (slot score (type FLOAT)(default 0.0))
    
)

;; MODULE ITINERARIO

(defmodule ITINERARIO (export ?ALL))

(deftemplate ITINERARIO::itinerario
    (slot itinerario-id (default-dynamic (gensym*)))
    (multislot locations)
    (multislot hotels (default * * * * * * *))
    (multislot costi (type INTEGER) (default 0 0 0 0 0 0 0))
    (multislot giorni (type INTEGER) (default 0 0 0 0 0 0 0))
    (multislot location-score (type FLOAT)(default 0.0 0.0 0.0 0.0 0.0 0.0 0.0))
    (multislot hotel-score (type FLOAT)(default 0.0 0.0 0.0 0.0 0.0 0.0 0.0))
    (slot durata (type INTEGER)(default 0))
    (slot distanza-totale (type INTEGER) (range 0 ?VARIABLE))    
    (slot score-totale (type FLOAT)(default 0.0))
)



(deftemplate ITINERARIO::media-cf-location
    (slot valore)
)

(deftemplate ITINERARIO::path-bannati
    (slot path-id)
)