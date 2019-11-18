(defmodule MAIN (export ?ALL))

(defrule MAIN::start
  (declare (salience 10000))
  =>
  (set-fact-duplication TRUE)
  (focus PRIMO SECONDO TERZO))


(defmodule PRIMO (export ?ALL))

(defrule prova1
    (declare (salience -100))
    =>
    (printout t "prova 1 salience -100")

)

(defrule prova2
    (declare (salience 0))
    =>
    (printout t "prova 1 salience 0")

)

(defrule prova3
    (declare (salience 100))
    =>
    (printout t "prova 1 salience 100")

)

(defmodule SECONDO (export ?ALL))

(defrule prova1
    (declare (salience -100))
    =>
    (printout t "prova 2 salience -100")

)

(defrule prova2
    (declare (salience 0))
    =>
    (printout t "prova 2 salience 0")

)

(defrule prova3
    (declare (salience 100))
    =>
    (printout t "prova 3 salience 100")

)

(defmodule TERZO (export ?ALL))

(defrule prova1
    (declare (salience -100))
    =>
    (printout t "prova 3 salience -100")

)

(defrule prova2
    (declare (salience 0))
    =>
    (printout t "prova 3 salience 0")

)

(defrule prova3
    (declare (salience 100))
    =>
    (printout t "prova 3 salience 100")

)

