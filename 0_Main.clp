(defmodule MAIN (export ?ALL))

(deftemplate status (slot step) (slot mode (allowed-values human computer)))

(defrule go-on-agent  (declare (salience 30))
  (maxduration ?d)
  (status (step ?s&:(< ?s ?d)))
  =>
  (focus AGENT)
)

; SI PASSA AL MODULO ENV DOPO CHE AGENTE HA DECISO AZIONE DA FARE
(defrule go-on-env  (declare (salience 30))
  (maxduration ?d)
  ?f1<-	(status (step ?s&:(< ?s  ?d)))
  =>
  (focus GAME)
)

(defrule next-step  (declare (salience 20))
  (maxduration ?d)
  ?f1<-	(status (step ?s&:(< ?s  ?d)))
  =>
  (bind ?s2 (+ ?s 1))
  (modify ?f1 (step ?s2))
)

(defrule game-over
  (maxduration ?d)
  (status (step ?s&:(>= ?s ?d)))
  =>
  ;(assert (exec (step ?s) (action solve)))
  (focus GAME)
)

(deffacts initial-facts
  (maxduration 10)
  (status (step 0) (mode computer))
  ;(status (step 0) (mode human))
  (agent-first)
)