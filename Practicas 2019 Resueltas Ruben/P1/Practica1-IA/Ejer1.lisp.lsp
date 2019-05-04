
; 1. Crea una función que rote el primer elemento de una lista colocándolo en la
; última posición. Los demás elementos deben mantenerse en el mismo orden.
; Ej: > (rotar-izda ‘ (a b c d))
;     > (b c d a)

(defun rotar-izda (lista)
    (append (cdr lista) (list(car lista)))
)
