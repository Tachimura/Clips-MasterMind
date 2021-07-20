(defmodule GAME (import MAIN ?ALL) (export deftemplate guess answer))

(deftemplate secret-code
  (multislot code (allowed-values blue green red yellow orange white black purple) (cardinality 4 4))
)

(deftemplate guess
  (slot step (type INTEGER))
  (multislot g (allowed-values blue green red yellow orange white black purple) (cardinality 4 4))
)

(deftemplate answer
  (slot step (type INTEGER))
  (slot right-placed (type INTEGER))
  (slot miss-placed (type INTEGER))
)

(defrule check-mate (declare (salience 100))
  (status (step ?s))
  ?f <- (guess (step ?s) (g ?k1 ?k2 ?k3 ?k4))
  (secret-code (code ?k1 ?k2 ?k3 ?k4) )
  =>
  (printout t "You have discovered the secrete code!" crlf)
  (retract ?f)
  (halt) 
)

(defrule prepare-answer
  ;mode human
  (status (step ?s) (mode human))
  (guess (step ?s))
  =>
  (assert (answer (step ?s) (right-placed 0) (miss-placed 0)))
)      

(defrule check-miss-placed
  (status (step ?s) (mode human))
  (secret-code (code $?prima ?k $?dopo) )
  (guess (step ?s) (g $?prima2 ?k $?dopo2))
  (test (neq (length$ $?prima2) (length$ $?prima)))
  (test (neq (length$ $?dopo2) (length$ $?dopo)))
  =>
  (assert (missplaced gensym*))
)

(defrule count-missplaced
  (status (step ?s) (mode human))
  ?a <- (answer (step ?s) (miss-placed ?mp))
  ?m <- (missplaced ?x)
  =>
  (retract ?m)
  (bind ?new-mp (+ ?mp 1))
  (modify ?a (miss-placed ?new-mp))  
)

(defrule check-right-placed
  (status (step ?s) (mode human))
  (secret-code (code $?prima ?k $?dopo) )
  (guess (step ?s) (g $?prima2  ?k $?dopo2))
  (test (eq (length$ $?prima2) (length$ $?prima)))
  (test (eq (length$ $?dopo2) (length$ $?dopo)))   
  =>
  (assert (rightplaced gensym*))
)

(defrule count-rightplaced
  (status (step ?s) (mode human) )
  ?a <- (answer (step ?s) (right-placed ?rp) (miss-placed ?mp))
  ?r <- (rightplaced gensym*)
  =>
  (retract ?r)
  (bind ?new-rp (+ ?rp 1))
  (modify ?a (right-placed ?new-rp))
)

(defrule for-humans (declare (salience -10))
  (status (step ?s) (mode human))
  (answer (step ?s) (right-placed ?rp) (miss-placed ?mp)) 
  =>
  (printout t "Right placed " ?rp " missplaced " ?mp crlf)
)

; MIE REGOLE
(defrule prepare-answer
  (status (step ?s)(mode computer))
  (guess (step ?s))
  =>
  (assert (answer (step ?s) (right-placed -1) (miss-placed -1)))
)

(defrule check-blackwhite
  (status (step ?s) (mode computer))
  (secret-code (code $?p) )
  (guess (step ?s) (g $?guess))
  ?a <- (answer (step ?s) (right-placed -1) (miss-placed -1))
  =>
  (bind ?wp 0)
  (bind ?bp 0)
  (bind ?i 1)
  (bind $?t1 (create$ 0 0 0 0))
  (bind $?t2 (create$ 0 0 0 0))
  ; Calcolo le Black (Right-Placed)
  (while (< ?i 5)
    (if (eq (nth$ ?i $?p) (nth$ ?i $?guess)) then
      (bind ?bp (+ ?bp 1))
      (bind $?t1 (replace$ $?t1 ?i ?i 1))
      (bind $?t2 (replace$ $?t2 ?i ?i 1))
    )
    (bind ?i (+ ?i 1))
  )
  (bind ?i 1)
  ; Calcolo le White (Miss-Placed)
  (while(< ?i 5)
    (if(eq (nth$ ?i $?t1)  0) then 
      (bind ?k  1) 
      (bind ?flag TRUE)
      (while(and (< ?k 5) (eq ?flag TRUE) )
        (if (and (and (neq ?i ?k) (eq(nth$ ?k $?t2) 0)) (eq (nth$ ?i $?guess) (nth$ ?k $?p))) then
          (bind ?wp (+ ?wp 1))
            (bind $?t2 (replace$ $?t2 ?k ?k 2))
            (bind ?flag FALSE) 
        )
        (bind ?k (+ ?k 1))
      )
    )
    (bind ?i (+ ?i 1))
  )
  (modify ?a (right-placed ?bp) (miss-placed ?wp))
)