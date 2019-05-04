;; Inteligencia Artificial - Practica 6: Algoritmo A* 
;; Autor: Ruben Cherif

;; http://disi.unal.edu.co/~lctorress/iartificial/

;; f(x) = g(x) + h(x)
;; g(x) = coste NodoInicial - NodoActual(x)
;; h(x) = heuristica

;; Problema del granjero

(defun crear-estado (granjero lobo cabra col)
	(list granjero lobo cabra col)
)

;; Funciones de acceso a los elementos que devuelve el literal izquierda o derecha:

(defun posicion-granjero (estado)
	(first estado)
)

(defun posicion-lobo (estado)
	(second estado)
)

(defun posicion-cabra (estado)
	(third estado)
)

(defun posicion-col (estado)
	(fourth estado)
)

;; Definir *estado-inicial* y *estado-final* :

(defparameter *estado-inicial*
	(crear-estado 'i 'i 'i 'i)
)

(defparameter *estado-final* 
	(crear-estado 'd 'd 'd 'd)
)

;; Definir la funcion es-estado-final

(defun es-estado-final (estado)
	(equal estado *estado-final*)
)


;; Funciones auxiliares
(defun opuesta (posicion)
	(if (eq posicion 'i) 'd 'i)	;;return 
)


;; Comprueba si el estado es seguro
(defun es-seguro (estado)
	
	if(and 
		(if (and  
			(eq (posicion-lobo estado) (posicion-cabra estado) ) 
			(eq (posicion-lobo estado) (posicion-granjero estado))
			)(return t)
		)
		
		(if (and  
			(eq (posicion-cabra estado) (posicion-col estado) ) 
			(eq (posicion-cabra estado) (posicion-granjero estado))
			)(return t)
		)
	)
)

;; Definiar lista de operadores

(defparameter *operadores*
	'(pasa-granjero-solo
	  pasan-granjero-y-lobo
	  pasan-granjero-y-cabra
	  pasan-granjero-y-col)
)


(defun pasan-granjero-solo (estado)
	(es-seguro (crear_estado 
			(opuesta (posicion-granjero estado))
			posicion-lobo 
			posicion-cabra 
			posicion-col
		)
	)
)

(defun pasan-granjero-y-lobo (estado)
	
	(if (eq (posicion-granjero estado) (posicion-lobo(estado)) )	
		(es-seguro (crear_estado 
				(opuesta (posicion-granjero estado))
				(opuesta (posicion-lobo estado))
				posicion-cabra 
				posicion-col
			)
		)
	)
)

(defun pasan-granjero-y-cabra (estado)
	
	(if (eq (posicion-granjero estado) (posicion-lobo(estado)) )	
		(es-seguro (crear_estado 
				(opuesta (posicion-granjero estado))
				posicion-lobo
				(opuesta (posicion-cabra estado)) 
				posicion-col
			)
		)
	)
)

(defun pasa-granjero-y-col (estado)
	
	(if (eq (posicion-granjero estado) (posicion-lobo(estado)) )	
		(es-seguro (crear_estado 
				(opuesta (posicion-granjero estado))
				posicion-lobo
				posicion-cabra 
				(opuesta (posicion-col estado))
			)
		)
	)
)

;;Definición de nodo de coste con heuristica

(defstruct (nodo-ch (:constructor crea-nodo-ch)
 				(:conc-name nil)
			)
 			estado
 			camino
 			coste-camino
 			coste-mas-heuristica
)

;; Funcion aplica

(defun aplica (operador estado)
	(funcall (symbol-function operador) estado)
)

;; Procedimiento de verificacion: A*
;; @param *operadores*
;; @param *estado-inicial*

(defun busqueda-a-estrella ()
	(let ((abiertos (list (crea-nodo-ch :estado *estado-inicial*:
							camino nil
							:coste-camino 0
 							:coste-mas-heuristica (heuristica *estado-inicial*))))
 		(cerrados nil)
 		(actual nil)
		(sucesores nil))
 		(loop (if (null abiertos) (return nil))
		 	(setf actual (first abiertos))				;; actual.push(abiertos[1])
		 	(setf abiertos (rest abiertos))				;; abiertos.pop (rest = cdr)
		 	(setf cerrados (cons actual cerrados))		;; cerrados.push(nodoActual)
		 	(cond 
			 	((es-estado-final (estado actual))(return actual))									;; FIN: Hemos llegado al objetivo
 				(t (setf sucesores(nuevos-o-mejores-sucesores actual abiertos cerrados))			;; Calculamos los sucesores del nodo actual
 					(setf abiertos(ordena-por-coste-mas-heuristica (append abiertos sucesores)))	;; Se anaden sucesores a abiertos y se ordena lista de abiertos segun f(x) = g(x) + h(x)
 				)
 			)
 		)
 	)
)

 ;;Funciones auxiliares

 ;;Función nuevos-o-mejores-sucesores
(defun nuevos-o-mejores-sucesores (nodo abiertos cerrados)
 	(elimina-peores (sucesores nodo) abiertos cerrados)
)


;;;Función sucesor de un nodo

(defun sucesor (nodo operador)
 	(let ((siguiente-estado (aplica operador (estado nodo))))
 		(when siguiente-estado
 			(crea-nodo-ch 
			 	:estado siguiente-estado
 				:camino (cons operador (camino nodo))
 				:coste-camino (+ (coste-de-aplicar-operador (estado nodo) operador)
 								(coste-camino nodo)
				)
 				:coste-mas-heuristica (+
 					(+ (coste-de-aplicar-operador (estado nodo) operador)
 					(coste-camino nodo))
 					(heuristica siguiente-estado)
				)
			)
 		)
 	)
 )

;;;Función sucesores de un nodo

(defun sucesores (nodo)
	(let ((resultado ()))
 		(dolist (operador *operadores*)
 			(let ((siguiente (sucesor nodo operador)))
 				(when siguiente (push siguiente resultado))
 			)
 		)
 		(reverse resultado)
 	)
)



;; Función esta-mejor: comprueba si existe algún nodo en la lista 
;; con el mismo estado que N y tal que el coste del camino es menor
;; o igual que el de N
;; @param nodo = nodo sucesores
;; @param lista-de-nodos
(defun esta-mejor (nodo lista-de-nodos)
	(let ((siguiente-estado (estado nodo)))
 		(dolist (n lista-de-nodos)
 			(if (and (equal siguiente-estado (estado n))(<= (coste-camino n) (coste-camino nodo)))
			 	(return t)
			)
		)
 	)
)

;; Función elimina-peores: 	
;; @param nodos = lista sucesores
;; @param abiertos = lista de nodos abiertos
;; @param cerrados = lista de nodos cerrrados
(defun elimina-peores (nodos abiertos cerrados)
 	(let ((lista-resultado ()))
 		(dolist (nodo nodos)
			(when (and (not(esta-mejor nodo abiertos)) (not(esta-mejor nodo cerrados)))
 				(push nodo lista-resultado)
			)
		)
 		(reverse lista-resultado)
	)
)

 ;;;Función ordena-por-coste-mas-heuristica

(defun ordena-por-coste-mas-heuristica (lista-de-nodos)
	(sort lista-de-nodos #'< :key #'coste-mas-heuristica)
)
