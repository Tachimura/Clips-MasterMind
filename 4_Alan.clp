(defmodule ALAN (import MAIN ?ALL) (import GAME ?ALL) (import AGENT ?ALL) (export ?ALL))

(deftemplate combination-s
    (slot step)
    (multislot code (allowed-values 1 2 3 4 5 6 7 8) (cardinality 4 4))
)

(deftemplate guess-alan
    (slot step)
    (multislot code (allowed-values 1 2 3 4 5 6 7 8) (cardinality 4 4))
)

(deftemplate counter 
    (slot name)
    (slot val)
)

(deffacts count
    (counter (name size-s) (val 0))
)

; Regola per eseguire l'inizializzazione della base di conoscenza di Alan
(defrule init-s
    (status (step 0) (mode computer))
    ?size_s <- (counter (name size-s) (val 0))
    =>
    ; Qui si inizializza S
    (bind ?val_sizes 1)
    (assert (combination-s (step 0) (code (create$ 1 1 1 1))))
    (bind ?n 1112)
    (while (neq $?n 1111) ; Per debug mettere (while (neq $?n 1311))
        ; Mi prendo le 4 componenti del numero
        (bind ?pos_1 (div ?n 1000))
        (bind ?pos_2 (div (- ?n (* ?pos_1 1000)) 100))
        (bind ?pos_3 (div (- ?n (+ (* ?pos_1 1000) (* ?pos_2 100))) 10))
        (bind ?pos_4 (mod ?n 10))
        
        ; Eseguo l'assert della combinazione
        ;(bind $?field (create$ ?pos_1 ?pos_2 ?pos_3 ?pos_4))
        ;(assert (combination-s (step 0) (code (replace$ (create$ 1 1 1 1) 1 4 $?field))))
        (assert (combination-s (step 0) (code (create$ ?pos_1 ?pos_2 ?pos_3 ?pos_4))))
        (bind ?val_sizes (+ ?val_sizes 1))
        ; Calcolo la prossima combinazione
        (bind ?pos_4 (+ ?pos_4 1))
	    (if( > ?pos_4 8) then
		    (bind ?pos_4 1)
		    (bind ?pos_3 (+ ?pos_3 1))
		    (if(> ?pos_3 8) then
			    (bind ?pos_3 1)
			    (bind ?pos_2 (+ ?pos_2 1))
			    (if(> ?pos_2 8) then
				    (bind ?pos_2 1)
                    (bind ?pos_1 (+ ?pos_1 1))
				    (if(> ?pos_1 8) then
					    (bind  ?pos_1 1)
                    )
			    )
		    )
	    )
        (bind ?n (+ (* ?pos_1 1000 ) (+ (* ?pos_2 100 ) (+ (* ?pos_3  10 ) ?pos_4))))
    )
    ; Aggiorno la dimensione di size_s
    (modify ?size_s (val ?val_sizes))

    (assert (counter (name old-size-s) (val ?val_sizes)))
    (assert (counter (name check-rule) (val 1)))
    (assert (guess-alan (step 0) (code (create$ 1 1 2 2))))
    (assert (guess (step 0) (g blue blue green green)))
    ;(printout t "Dimensione S: " ?val_sizes crlf)
    (printout t "I'm ready to guess! Step: 0" ", I guessed: (blue blue green green) (1 1 2 2)" crlf)
)

; Regola che riduce S, tiene solo le combinazioni che hanno le stesse white e black
; che ha la guess appena fatta rispetto alla secret password
(defrule reduce-s 
    (status (step ?new_s) (mode computer))
    (guess-alan (step ?s) (code $?p))
    (answer (step ?s) (right-placed ?black-pegs) (miss-placed ?white-pegs))
    ?combination <- (combination-s (step ?s) (code $?guess &:(neq $?guess $?p))) ; Tutte le combinazioni diverse dalla precedente guess
    ?c1<-(counter (name size-s) (val ?val1))
    ?c2<-(counter (name check-rule) (val ?val2))
    =>
    (modify ?c2 (val (+ ?val2 1)))
    (bind ?wp 0)
    (bind ?bp 0)
    (bind ?i 1)
    (bind $?t1 (create$ 0 0 0 0))
    (bind $?t2 (create$ 0 0 0 0))
    ; Calcolo le Black (Right-Placed)
    (while (< ?i 5)
        (if (eq (nth$ ?i $?p) (nth$ ?i $?guess))
            then 
            (bind ?bp (+ ?bp 1))
            (bind $?t1 (replace$ $?t1 ?i ?i 1))
            (bind $?t2 (replace$ $?t2 ?i ?i 1))
        )
        (bind ?i (+ ?i 1))
    )
    ; Calcolo le White (Miss-Placed)
    (bind ?i 1)
    (while(< ?i 5)
        (if(eq (nth$ ?i $?t1) 0) then 
            (bind ?k 1) 
            (bind ?flag TRUE)
            (while(and (< ?k 5) (eq ?flag TRUE))
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
    ; Se o le black o le white sono differenti, sicuramente questa non sarà la password segreta
    (if(or (neq ?black-pegs ?bp) (neq ?white-pegs ?wp))
        then  ;De morgan bestie!
            (retract ?combination)
            (modify ?c1 (val (- ?val1 1)) )
        else
            (modify ?combination (step ?new_s))
    )
)

; Funzione che dato un numero, ritorna i colori associati :=> (1111) -> (green green green green)
(deffunction convert_code($?number)
    (bind $?colours (create$  blue green red yellow orange white black purple ))
    return (create$ (nth$(nth$ 1 $?number) $?colours) (nth$(nth$ 2 $?number)$?colours) (nth$(nth$ 3 $?number)$?colours)(nth$(nth$ 4 $?number) $?colours))
)

; Regola che fa la guess
(defrule make-guess
    ?c1<- (counter (name size-s) (val ?val1))
    ?c2<- (counter (name old-size-s) (val ?val))
    ?c3<- (counter (name check-rule) (val ?val)) ; Parte solo quando abbiamo finito di ridurre S
    ?guess-alan <- (guess-alan (step ?s) (code $?pw)) ; Precedente guess fatta da Alan
    ?guess-alan-combination <- (combination-s (step ?s) (code $?pw)) ; Combinazione della precedente guess fatta da Alan
    (combination-s (step ?s1 &:(neq ?s1 ?s)) (code $?guess)) ; Prendo una combinazione (che non sia uguale a quella precedente)
    =>
    ; Aggiorno la guess-alan e rimuovo la vecchia la combinazione associata
    (retract ?guess-alan-combination)
    (modify ?guess-alan (step ?s1) (code $?guess))
    ; Aggiungo la nuova guess
    (assert (guess (step ?s1) (g (convert_code $?guess)) ))
    ; Aggiorno i contatori
    (modify ?c1 (val (- ?val1 1))) ; Ho rimosso sopra una combinazione
    (modify ?c2 (val (- ?val1 1)))
    (modify ?c3 (val 1)) ; Impostandolo a 1 non scatta più fino alla prossima riduzione di S
    (printout t "I'm ready to guess! Step: " ?s1 ", I guessed: " (convert_code $?guess) $?guess crlf)
)