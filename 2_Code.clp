; Funzione che mappa i numeri: (1..8) in simboli: (blue green red yellow orange white black purple)
(deffunction colormap (?number)
  (if( eq ?number 1) then (return blue) 
    else (if(eq ?number 2) then (return green)
    else (if(eq ?number 3) then (return red)
    else (if(eq ?number 4) then (return yellow)
    else (if(eq ?number 5) then (return orange)
    else (if(eq ?number 6) then (return white)
    else (if(eq ?number 7) then (return black)
    else (if(eq ?number 8) then (return purple)))))))))
)

; Funzione che ritorna un colore randomico
(deffunction codegen ()
  (bind ?code (colormap(+ (mod (random) 8) 1)))
  return ?code
)

; Funzione che genera la secret-code
; Valori: 1-blue, 2-green, 3-red, 4-yellow, 5-orange, 6-white, 7-black, 8-purple
(deffacts secret-cod1
  ; Generazione di un codice randomico
  (secret-code(code (codegen)(codegen)(codegen)(codegen)))
  ;(secret-code (code blue blue green green))
  ;(secret-code (code orange black red red))
  ;(secret-code (code green blue blue purple))
  ;(secret-code(code green red blue yellow))
  ;(secret-code(code blue green green green))

  ;(secret-code(code blue green green blue))
  ;(secret-code (code purple purple purple red))
  ;(secret-code(code green black blue blue))
  ;(secret-code (code green green green green))
)