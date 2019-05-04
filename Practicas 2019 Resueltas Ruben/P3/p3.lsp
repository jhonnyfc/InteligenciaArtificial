;Practica3->OWA

;En la matriz quito los valores de la diagonal porque no los necesito ni interfiener en el problema
(setf x '((0.3149 0.1605 0.3640)(0.6851 0.0407 0.0624)(0.8395 0.9593 0.3874)(0.6360 0.9376 0.6126)))

;ejecutar asi:(owa x)
(defun OWA (matriz)
	(print "Elige una opcion:")
	(print "1.- Maximo.")
	(print "2.- Media aritmetica.")
	(print "3.- Owa al menos la mitad.")
	(print "4.- Owa la mayor cantidad posible.")
	(print "5.- Operador OWA la mayor parte de.")
	(setf orden (read))
	(cond	
		((= orden 1)())
			
		((= orden 2)
			(setf suma 0)
			(setf n 0)
			(setf media-filas nil)
			(dolist (y matriz media-filas)
				;en y esta el car(x)(sublista)
				;lisp te crea las variables por defecto
				;y setf asigna (Ej->media-filas=nil)
				(setf media-filas (cons (media-aritmetica y) media-filas))
			)
			(setf maximo_valor (maximo media-filas))
			(ordenar media-filas)
			
			(print media-filas)
			;(print maximo_valor)
			(setf pos (posicion-valor media-filas maximo_valor))
		)
		((= orden 3)
			(owau x 0 0.5)		
		)
		((= orden 4)
			(owau x 0.5 0.8)		
		)
		((= orden 5)
			(owau x 0.3 0.8)	
		)	
	)
)


(defun owau(matriz a b)

	(setf longitud (length (car x)))
	(setf lista_pesos nil)
	(do ((i 1(incf i)))((= i (+ longitud 1)))
		(setf lista_pesos (cons (- (pesos_menos a b (/ i longitud)) (pesos_menos a b (/ (- i 1) longitud))) lista_pesos ))
	)			
	(nreverse lista_pesos)
	;(print lista_pesos)
	(setf solucion nil)
	(setf listaAg nil)
	(dolist(fila matriz)
		(ordenar fila)
		;(print fila)
		(setf solucion (mapcar #'* lista_pesos fila)) ; Producto entre lista de pesos y fila
		(setf suma 0)
		(dolist (x solucion ())		; Sumamos componenetes del vector
			(incf suma x)
		)
		(setf listaAg (cons suma listaAg))
	)

	(nreverse listaAg)
	;(print listaAg)	
	(print "Resultado Final: " )
	(setf res (maximo listaAg))
	;(print res)
)

(defun pesos_menos (a b r)
	(setf w 0)
	(cond	
		((< r a)(setf w 0))
		((<= a r b)(setf w (/ (- r a) (- b a))))
		(t(setf w 1))
	)
	w
)
			

(defun ordenar(lista)
	(sort lista #'>)
)
			
		
	
(defun media-aritmetica(lista)
	(setf suma 0)
	(setf n 0)
	(dolist (x lista ())
		(incf suma x)
		(incf n)
	)
	;(print (/ suma n))
	(/ suma n)
)

(defun maximo (lista)
	(setf maximo 0)
	(dolist (x lista)
		(cond	
			((> x maximo)(setf maximo x))
		)
	)
	maximo
)

(defun posicion-valor(lista x)
	(setf pos 1)
	(dolist	(y lista)
		(if (= x y)(return pos)(incf pos))
	)
)


(defun recorrido(matriz)
	(dolist(x matriz)
		(print x)
		(print "---")
	)
)