;;--------------------------------------------------------------- COMMON

(defmodule COMMON (export ?ALL))

(defglobal
    ?*MAX-TOURISM-SCORE* = 5
    ?*MAX-KM-GG* = 150
    ?*HOTEL-COSTO-BASE* = 50
    ?*HOTEL-COSTO-ADDIZIONALE* = 25    
    ?*MIN-PRINT-CF* = 0.20
)

(deftemplate attributo
    (multislot nome)
    (slot valore)
    (slot certezza (type FLOAT) (range -1.0 1.0) (default 0.0))

)

(deffacts preference-hote-location-iniziale   
    (attributo (nome hotel-loc-preference)(valore hotel)(certezza 1.0)) 
)

(deftemplate iterazione
    (slot i (type INTEGER))
)

(deffacts iterazione
    (iterazione (i 0))
)


;;---------- COMBINE CERTAINTIES ------------
  

(defrule  gestione-certezze-positive
    (declare (auto-focus TRUE))
    ?fact1 <- (attributo (nome $?n) (valore ?v) (certezza ?C1))
    ?fact2 <- (attributo (nome $?n) (valore ?v) (certezza ?C2))
    (test (neq ?fact1 ?fact2))
=>
    
    (bind ?C3 (/ (+ ?C1 ?C2) 2))
    (retract ?fact1)
    (modify ?fact2 (certezza ?C3))
    
)



;; DOMANDE
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
    (domanda (descrizione periodo-viaggio)(importanza 0)(la-domanda " In quale periodo vorresti partire? [estate inverno indifferente] ")(risposte-valide estate inverno indifferente)(skippable FALSE))
    (domanda (tipo range)(descrizione durata-viaggio)(importanza 0) (la-domanda "Quanti giorni vuoi che la vacanza duri? valore tra [1,7]") (risposte-valide 1 7) (skippable FALSE))
    (domanda (tipo range)(descrizione numero-persone)(importanza 0)(la-domanda "Quante persone vogliono andare in vacanza? tra [2,10] ")(risposte-valide 2 10)(skippable FALSE))
    (domanda (descrizione costo)(importanza 0)(la-domanda " Vuoi fare un viaggo economico o più costoso? [economico normale costoso] ")(risposte-valide economico normale costoso)(skippable FALSE))

    ;; Iterazione 1
    (domanda (descrizione budget-viaggio-generico)(importanza 1) (la-domanda "Hai un budget massimo? [Si, No]") (risposte-valide si no) (skippable FALSE))
    (domanda (tipo range)(descrizione budget-viaggio)(importanza 1) (la-domanda "Qual'è il tuo budget? valore tra [200,5000]") (risposte-valide 200 5000) (skippable FALSE)(precursori budget-viaggio-generico is si))
    (domanda (descrizione viaggio-piu-regioni-generico)(importanza 1) (la-domanda "Vuoi visitare più regioni? [Si, No]") (risposte-valide Si No si no) (skippable FALSE))
    (domanda (tipo range)(descrizione mangiare)(importanza 1)(la-domanda "Quanto è importante per te il buon cibo? tra [1,5] ")(risposte-valide 1 5)(skippable FALSE))
    (domanda (tipo range)(descrizione religione)(importanza 1)(la-domanda "Quanto è importante per te l'aspetto religioso di una località? tra [1,5]")(risposte-valide 1 5)(skippable FALSE))
    (domanda (tipo range)(descrizione cultura)(importanza 1)(la-domanda "Quanto è importante per te l'aspetto culturale di una località? tra [1,5]")(risposte-valide 1 5)(skippable FALSE))
    (domanda (descrizione montagna)(importanza 1)(la-domanda " Ti piacciono i luoghi montani? [si,no] " )(risposte-valide si no)(skippable FALSE))
    (domanda (tipo range)(descrizione naturalistico)(importanza 1)(la-domanda " Quanto dai importanza all'aspetto naturalisticoo di una localita' ? tra [1,5] ")(risposte-valide 1 5))
    (domanda (descrizione balneare-lacustre)(importanza 1)(la-domanda " Preferisci un turismo balneare o di tipo lacustre ? [balneare lacustre]" )(risposte-valide balneare lacustre))
    (domanda (descrizione sport-termale)(importanza 1)(la-domanda " In vacanza vuoi rilassarti o mantenerti attivo facendo sport ? [relax sport] ")(risposte-valide relax sport))

    ;; Iterazione 2
    (domanda (descrizione numero-location-generico) (importanza 2) (la-domanda "Vuoi visitare meno location? [Si,No]") (risposte-valide Si No si no) (skippable FALSE))
    (domanda (descrizione viaggio-regione-generico) (importanza 2) (la-domanda "Vuoi visitare qualche regionee in particolare? [Si,No]") (risposte-valide Si No si no) (skippable FALSE))
    (domanda (descrizione viaggio-regione) (importanza 2) (la-domanda "Quale regionee vorresti visitare? ") (risposte-valide sicilia calabria puglia toscana liguria lombardia piemonte lazio) (skippable FALSE)(precursori viaggio-regione-generico is si))
    (domanda (tipo range)(descrizione numero-location)(importanza 2)(la-domanda "Quante città vuoi visitare?")(risposte-valide 1 7)(skippable FALSE)(precursori numero-location-generico is si))

    ;;
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

(deftemplate location-turistica
    (slot nome-location (default ?NONE))
    (slot tipo-turismo (default ?NONE))
    (slot score(type INTEGER) (range 1 5))
)


(deffacts location-list
    (location (nome agrigento) (regione sicilia)(altitudine 37.31) (longitudine 12.58))
    (location (nome palermo) (regione sicilia)(altitudine 38.14) (longitudine 13.31))
    (location (nome catania) (regione sicilia)(altitudine 37.51) (longitudine 15.08))
    (location (nome scilla) (regione calabria)(altitudine 38.14) (longitudine 13.31))
    (location (nome reggio) (regione calabria)(altitudine 38.10) (longitudine 15.66))
    (location (nome salerno) (regione calabria)(altitudine 40.41) (longitudine 14.46))
    (location (nome bari) (regione puglia)(altitudine 41.07) (longitudine 16.53))
    (location (nome lecce) (regione puglia)(altitudine 40.21) (longitudine 18.11))
    (location (nome brindisi) (regione puglia)(altitudine 40.39) (longitudine 17.56))
    (location (nome roma) (regione lazio)(altitudine 41.54) (longitudine 12.31))
    (location (nome latina) (regione lazio)(altitudine 41.28) (longitudine 12.51))
    (location (nome frosinone) (regione lazio)(altitudine 41.38) (longitudine 13.22))
    (location (nome pisa) (regione toscana)(altitudine 43.43) (longitudine 10.24))
    (location (nome siena) (regione toscana)(altitudine 43.19) (longitudine 11.18))
    (location (nome lucca) (regione toscana)(altitudine 43.51) (longitudine 10.31))
    (location (nome genova) (regione liguria)(altitudine 44.25) (longitudine 08.55))    
    (location (nome savona) (regione liguria)(altitudine 44.19) (longitudine 08.28))    
    (location (nome laspezia) (regione liguria)(altitudine 44.07) (longitudine 09.51))    
    (location (nome milano) (regione lombardia)(altitudine 45.28) (longitudine 09.11))    
    (location (nome pavia) (regione lombardia)(altitudine 45.11) (longitudine 09.11))    
    (location (nome bergamo) (regione lombardia)(altitudine 45.42) (longitudine 09.40))    
    (location (nome asti) (regione piemonte)(altitudine 44.53) (longitudine 08.11)) 
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
    ;;(bind ?euclidian_distanza (sqrt(+ (** (- ?l ?l1) 2) (** (- ?a ?a1) 2))))
    (assert(loc-to-loc(location-src ?n)(location-dst ?n1)(distanza ?km_distanza))) 

)

(defrule location-turism-list-random-creation
    (location (nome ?n) (regione ?r))
    =>
    ;;(seed (round (time)))
    (assert(location-turistica (nome-location ?n)(tipo-turismo balneare)(score  (random 1 5))))
    (assert(location-turistica (nome-location ?n)(tipo-turismo naturale)(score  (random 1 5))))
    (assert(location-turistica (nome-location ?n)(tipo-turismo culturale)(score  (random 1 5))))
    (assert(location-turistica (nome-location ?n)(tipo-turismo enogastronomico)(score  (random 1 5))))
)

;;MODULE HOTEL

(defmodule HOTEL (export ?ALL) (import LOCATION ?ALL))

(deftemplate HOTEL::hotel
    (slot nome (default ?NONE))
    (slot location (default ?NONE))
    (slot stelle (type INTEGER) (range 1 4))
    (slot posti-liberi (type INTEGER) (range 0 ?VARIABLE))
    (slot capacità (type INTEGER) (range 1 ?VARIABLE))
)


(defrule lista-hotel-creazione-randomn
    (location (nome ?n) (regione ?r))
    =>
    
    (assert(hotel (nome (str-cat "morandi-" ?n))(location ?n)(stelle (random 1 5))(posti-liberi (random 100 300))(capacità 300)))
    (assert(hotel (nome (str-cat "empedocle-" ?n))(location ?n)(stelle (random 1 5))(posti-liberi (random 100 300))(capacità 300)))
    (assert(hotel (nome (str-cat "leonardo-" ?n))(location ?n)(stelle (random 1 5))(posti-liberi (random 100 300))(capacità 300)))
    (assert(hotel (nome (str-cat "sciascia-" ?n))(location ?n)(stelle (random 1 5))(posti-liberi (random 100 300))(capacità 300)))

)

;;PATH  (assert (path (resorts ?r) (num-citta 1) (distanza-totale 0)))

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
    (multislot hotels (default ND ND ND ND ND ND ND))
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