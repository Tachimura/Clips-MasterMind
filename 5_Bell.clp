(defmodule BELL (import MAIN ?ALL) (import GAME ?ALL) (import AGENT ?ALL) (export ?ALL))

; Modo fancy: un array -> how?
(defglobal ?*colours* = (create$  blue green red yellow orange white black purple ))

;(defglobal ?*guess* = (create$ red red red red))

; Per essere fancy aggiungi value permesso e cardinalità
(deftemplate dice-eight (slot number) (slot thrown) (multislot choice))

(deffacts init-facts
    (dice-eight (number (+ (mod (random) 8) 1)) (thrown 1) (choice (create$ red red red red)))
)

;possibile ottimizzazione : riunire le due regole?

; Questa regola inizializza le componenti della guess
(defrule init-guess
    (status (step ?s) (mode computer))
    ?dice <-(dice-eight (number ?chance)(thrown ?t&:(<= ?t  4)) (choice $?c))
    =>
    (printout t "I throw the dice. It reads: " ?chance crlf " So it's colour: " (nth$ ?chance ?*colours*) " let me think... " $?c " It is throw number: " ?t  crlf)
    (modify ?dice (number (+ (mod (random) 8) 1)) (thrown (+ ?t 1)) (choice (replace$ $?c ?t ?t (nth$ ?chance ?*colours*) )))
)

; Questa regola asserisce la guess stupida
(defrule guess-B
    (status (step ?s) (mode computer) ) 
    ?dice<-(dice-eight (number ?chance)(thrown ?t&:(eq ?t  5))(choice $?c))
    =>
    (printout t "I am guessing:" $?c crlf crlf)
    (assert (guess (step ?s) (g  $?c)))
    (modify ?dice (thrown 1))
    (pop-focus) ;questo serve?
)