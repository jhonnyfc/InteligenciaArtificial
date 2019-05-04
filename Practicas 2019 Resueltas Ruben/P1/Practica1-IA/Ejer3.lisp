; Practica 1, ejercicio 3


; Ejercicio 3: Crea una función que modifique todas las apariciones de un elemento de una
; lista por otro. Supondremos que la lista tiene un único nivel, es decir, no contiene
; sublistas. Utilizar recursividad.
; Ej: > (cambiar ‘a ‘b ‘(1 2 3 4 a 6 7 8))
;     > (1 2 3 4 b 6 7 8)

(defun cambiar (el1 el2 lista)
    (cond 
        ((null lista) (pritn "La lista esta vacia"))            ; Caso base
        ((equal el1 (first lista)) (cons el2 (cambiar el1 el2 (cdr lista))))    ; Si 
        t (cons (first lista) (cambiar el1 el2 (cdr lista))))
    )
)