; Practica 1, ejercicio 4


;Crea una función que devuelva una lista formada por todos los elementos de la
;lista de entrada mayor que un elemento dado. Utilizar recursividad. Realizar una
;segunda versión utilizando MAPCAR.
;Ej: > (lista-mayores ‘4 ‘(7 2 3 6 1))
;       > (7 6)
    
;Nota: con el mapcar el salida saldrá (7 NIL NIL 6 NIL)


; Solucion 1
(defun lista-mayores (elem lista &optional resul)
    (cond 
        ((null lista) resul)
        ((> (first lista) elem ) (lista-mayores elem (cdr lista)
            (append resul (list (first lista)))))
        (t (lista-mayores elem (cdr lista) resul))
    )
)



; Solucion 2
(defun lista-mayores (elem lista)
    (cond 
        ((null lista) nil)
        ((> (car lista) elem) (cons (car lista) (lista-mayores elem (cdr lista))))
        (t (lista-mayores elem (cdr lista)))
    )
)

; Solucion 3:
(setf elem 4)
(setf lista '(7 2 3 6 1))
(mapcar #'(lambda (x) (if (> x elem) x)) lista)







