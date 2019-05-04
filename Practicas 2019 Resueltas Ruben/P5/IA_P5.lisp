;;;Definir las variables globales *misioneros*, *canibales* *capacidad*
;;;contienen en cada caso el nº concreto de misioneros, caníbales y personas que caben en la barca.

(defvar *misioneros* 3)

(defvar *canibales* 3)

(defvar *capacidad* 2)

;;; Representación de un estado con (M C P), donde M es el nº de misioneros en la orilla izda.,
;;; C es el nº de caníbales en la orilla izda.y P la posición de la barca, 1 si está a la izda. y 0 si está ala dcha.

(defun crea-estado (misioneros canibales posicion)
    (list misioneros canibales posicion)
)

;; Funciones de acceso a los elementos que devuelve el literal izquierda o derecha:

(defun orilla-de-la-barca (estado)
    (if (eq (third estado) 1) 'izquierda 'derecha)
)

(defun misioneros-en-izquierda (estado)
    (first estado)
)

(defun canibales-en-izquierda (estado)
    (second estado)
)

(defun misioneros-en-derecha (estado)
    (- *misioneros* (misioneros-en-izquierda estado))
)

(defun canibales-en-derecha (estado)
    (- *canibales* (canibales-en-izquierda estado))
)

;;; Definir *estado-inicial* y *estado-final*:

(defparameter *estado-inicial*
    (crea-estado *misioneros* *canibales* 1)
)

(defparameter *estado-final*
    (crea-estado 0 0 0)
)

;;; Definir la función es-estado-final:

(defun es-estado-final (estado)
    (equal estado *estado-final*)
)

;;; Funciones auxiliares:
;;; (orilla-opuesta-estado estado): devuelve la orilla opuesta a la que se encuentra la barca en estado.

(defun orilla-opuesta-estado (estado)
    (if (= (third estado) 1) 0 1)
)

(defun mover (misioneros canibales estado)
    (if (eq (third estado) 1)
        (crea-estado (- (misioneros-en-izquierda estado) misioneros)
                    (- (canibales-en-izquierda estado) canibales) 0)

        (crea-estado (+ (misioneros-en-izquierda estado) misioneros)
                    (+ (canibales-en-izquierda estado) canibales) 1)
    )
)
 
;;; Precondición: (es-posible m c estado): comprueba si la orilla en la que se encuentra la barca
;;; tiene al menos m misioneros y c canibales.

(defun es-posible (m c estado)
    (if (eq (orilla-de-la-barca estado) 'derecha)
        (and (<= m (misioneros-en-derecha estado))(<= c (canibales-en-derecha estado)))
        (and (<= m (misioneros-en-izquierda estado))(<= c (canibales-en-izquierda estado)))
    )
)

;;; Postcondición: (es-seguro estado): que comprueba si en la orilla que no se encuentra la barca
;;; no hay misioneros o hay más misioneros que caníbales:

(defun es-seguro (estado)
    (if (eq (orilla-de-la-barca estado) 'derecha)
        (not (< 0 (misioneros-en-izquierda estado)(canibales-en-izquierda estado)))
        (not (< 0 (misioneros-en-derecha estado)(canibales-en-derecha estado)))
    )
)

;;; Definir la lista de operadores teniendo en cuenta las siguientes restricciones:
;;; a) 0<= M <= *misioneros*, 0<= C <= *caníbales*,
 ;;; b) 1<= M+C <= *capacidad*=2

(defparameter *operadores*
    '(pasan-dos-misioneros
    pasan-dos-canibales
    pasan-un-misionero-y-un-canibal
    pasa-un-misionero-solo
    pasa-un-canibal-solo)
)

(defun pasan-dos-misioneros (estado)
    (when (es-posible 2 0 estado)
        (let ((estado-resultante (mover 2 0 estado)))
            (when (es-seguro estado-resultante) estado-resultante)
        )
    )
)

(defun pasan-dos-canibales (estado)
    (when (es-posible 0 2 estado)
        (let ((estado-resultante (mover 0 2 estado)))
            (when (es-seguro estado-resultante) estado-resultante)
        )
    )
)

(defun pasan-un-misionero-y-un-canibal (estado)
    (when (es-posible 1 1 estado)
        (let ((estado-resultante (mover 1 1 estado)))
            (when (es-seguro estado-resultante) estado-resultante)
        )
    )
)

(defun pasa-un-misionero-solo (estado)
    (when (es-posible 1 0 estado)
        (let ((estado-resultante (mover 1 0 estado)))
            (when (es-seguro estado-resultante) estado-resultante)
        )
    )
)

(defun pasa-un-canibal-solo (estado)
    (when (es-posible 0 1 estado)
        (let ((estado-resultante (mover 0 1 estado)))
            (when (es-seguro estado-resultante) estado-resultante)
        )
    )
)