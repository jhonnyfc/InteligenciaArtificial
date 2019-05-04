; Practica 1, ejercicio 5

; Crea una función que dada una lista de números ordenada de forma creciente
; inserte un nuevo elemento en la posición que le corresponda.
;   Ej: > (insertar ‘9 ‘(3 6 33 77 88 100))
;       > ‘(3 6 9 33 77 88 100)


; Solucion 1
(defun insertar (elem lista)
    (cond 
        ((null lista) (list elem))
        ((> elem (car lista)) (cons (car lista) (insertar elem (cdr lista))))
        (t (cons elem lista))
    )
)
