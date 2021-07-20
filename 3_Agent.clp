;  ---------------------------------------------
;  --- Definizione del modulo e dei template ---
;  ---------------------------------------------
(defmodule AGENT (import MAIN ?ALL) (import GAME ?ALL) (export ?ALL))

(deftemplate agent-type (slot name (allowed-values Alan Bell)))

(defrule human-player
  (status (step ?s) (mode human))
  =>
  (printout t "Your guess at step " ?s crlf)
  (bind $?input (readline))
  (assert (guess (step ?s) (g  (explode$ $?input)) ))
  (pop-focus)
)
 
(defrule choose-agent 
  ; Diversa dalla regola human-player, deve essere utilizzata solo al passo 0 e inizializza il tipo di agente - computer
  (status (step 0) (mode computer))
  =>
  (printout t "Choose your agent (Alan/Bell):"crlf)
  (bind ?input (readline))
  (printout t "You chose " ?input crlf)
  (assert (agent-type (name ?input)))
)

;Funzione che rimpalla il focus sull'agent scelto
(defrule keep-agent
  (agent-type (name ?name))
  (status (step ?s) (mode computer))
  =>
  (printout t "I am " ?name " at guess number: " ?s crlf )
  (if(eq ?name "Alan") then (focus ALAN) else (if(eq ?name "Bell") then (focus BELL) )) 
)