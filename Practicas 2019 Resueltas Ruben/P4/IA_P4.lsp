;Problema de las jarras de agua

(setf x '(0 1 2 3 4))
(setf y '(0 1 2 3))

(defparameter *operadores*
	'(  llenar_jarra_4
		llenar_jarra_3
		vaciar_jarra_4
		vaciar_jarra_3
		volcar_jarra_4
		volcar_jarra_3)
)

(defun crear_estado (x y)
	(list x y)
)

(defparameter *estado_inicial* 
	(crear_estado 0 0)
)

(defparameter *estado_final* 
	(crear_estado 2 y)
)

(defun est_final (estado)
	(equal estado *esatdo_final*)
)

(defun llenar_jarra_4 (estado)
	(if (< (contenido_jarra_4 estado) 4) (crear_estado 4 contenido_jarra_3 estado))
)

(defun llenar_jarra_3 (estado)
	(if (< (contenido_jarra_3 estado) 3) (crear_estado contenido_jarra_4 estado 3))
)

(defun vaciar_jarra_4 (estado)
	(if (> (contenido_jarra_4 estado) 0)(crear_estado 0 (contenido_jarra_3 estado)))
)

(defun vaciar_jarra_3 (estado);mirar si esta bien
	(if (> (contenido_jarra_3 estado) 0)(crear_estado (contenido_jarra_4 estado) 0))
)

(defun volcar_jarra_4 (estado)
	(setf x (contenido_jarra_4 estado))
	(setf y (contenido_jarra_3 estado))
	(if (AND (> x 0)(< y 3)(<= (+ x y) 3))(crear_estado 0 (+ x y))(crear_estado (- x (- 3 y)) 4 ))
)

(defun volcar_jarra_3 (estado)
	(setf x (contenido_jarra_4 estado))
	(setf y (contenido_jarra_3 estado))
	(if (AND (< x 4)(> y 0)(<= (+ x y) 4))(crear_estado (+ x y) 0 )(crear_estado 3 (- y (- 4 x)) ))
)

(defun contenido_jarra_4 (estado)
	(first estado)
)

(defun contenido_jarra_3 (estado)
	(second estado)
)

(defun aplica (operador estado)
	(funcall (symbol-function operador) estado)
)
	
	
	
	
	
	