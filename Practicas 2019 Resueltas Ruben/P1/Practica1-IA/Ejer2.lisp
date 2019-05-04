; Practica 1, ejercicio 2


;Crea una función que busque en una lista el primer elemento numérico y lo devuelva.
;Ej: > (primer-num ‘(a b c f 6 u 5 4))
;    > 6

(defun primer-num (lista)
    (cond 
        ((null lista) (print "La lista esta vacia"))        ; Caso base: la lista esta vacia
        ((numberp (first lista)) (first lista))             ; Comprobamos si un numero, si lo es lo imprimimos 
        (t (primer-num (cdr lista)))                        ; Recursividad
    )
)
