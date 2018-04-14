;Formato de nodo para juego Mancala
;(nivel utilidad (11 12 13 14 15 16 P1 21 22 23 24 25 26 P2))

;Hola. Checando.
(print 'hola)

;Metodo para obtener la dificultad del juego seleccionada.
(setq d '())
(let ((in (open "dificultad.txt" :if-does-not-exist nil)))
	(when in
		(loop for line = (read-line in nil)
			while line do (push (intern line) d)
		)
		(close in)
	)
)
(cond
	( (equal (car d) 'F) (setq profundidad 3) )
	( (equal (car d) 'M) (setq profundidad 6) )
	( (equal (car d) 'D) (setq profundidad 9) )
	(t (setq profundidad 4))
)

(setq nivel 0)

(setq posibilidades NIL)

(defun alfa-beta (nodo)
	(setq valorAccion (maxi-val nodo -9999 9999))
	(setq accion (selec-accion posibilidades valorAccion))
	(return-from alfa-beta accion)
)

(defun maxi-val (estado alfa beta)
	(cond
		((eql T (verifica-corte (third estado) (first estado))) (print estado) (print 'utilidad) (print (utilidad estado)) (utilidad estado))
		(T
			(setq v -9999)
			(setq acciones (acciones-disp estado))
			(print 'acciones-disp-maxi)
			(print acciones)
			(loop
				(when (eql acciones NIL) (return v))
				(setq accion (pop acciones))
				(setq nodo (resultado accion (car estado)))
				;(print v)
				(setq v (max v (mini-val nodo alfa beta)))
				;(print v)
				(rplaca (nthcdr 1 nodo) v)
				(print 'nodo)
				(print nodo)
				(if (= (car nodo) 1) (push nodo posibilidades))
				(cond
					((>= v beta) v)
					(T (setq alfa (max alfa v)))
				)
			)

			(return-from maxi-val v)
		)
	)
)

(defun mini-val (estado alfa beta)
	(cond
		((eql T (verifica-corte (third estado) (first estado))) (utilidad estado))
		(T
			(setq v 9999)
			(setq acciones (acciones-disp (gira-estado estado)))
			(print 'acciones-disp-mini)
			(print acciones)
			(loop
				(when (eql acciones NIL) (return v))
				(setq accion (gira-tablero (pop acciones))) ;;
				(setq nodo (resultado accion (car estado)))
				(setq v (min v (maxi-val nodo alfa beta)))
				(rplaca (nthcdr 1 nodo) v)
				(if (= (car nodo) 1) (push nodo posibilidades))
				(cond
					((<= v alfa) v)
					(T (setq beta (min beta v)))
				)
			)
			(return-from mini-val v)
		)
	)
)

;Selecciona el mejor tablero a jugar.
(defun selec-accion (posibles-acciones utilidadMax)
(print 'ya-casi)
(print posibles-acciones)
	(cond
		((null posibles-acciones) (print 'error))
		((= (cadr (car posibles-acciones)) utilidadMax) (car (last (car posibles-acciones))))
		(t (selec-accion (cdr posibles-acciones)))
	)
)

(defun verifica-corte (tablero prof)
	(cond
		((> prof profundidad) (setq res T))
		((or (> (seventh tablero) 24) (> (car (last tablero)) 24)) (setq res T))
		((= (apply #'+ (nthcdr 8 (reverse tablero))) 0) (setq res T))
		((= (- (apply #'+ (nthcdr 7 tablero)) (car (last tablero))) 0) (setq res T))
		(t (setq res NIL))
	)
	(return-from verifica-corte res)
)

;Funcion para determinar la utilidad de cada estado.
(defun utilidad (nodo)
	(setq tablero (third nodo))
	(cond
		((eql (car d) 'F)
			(setq respuesta (- (seventh tablero) (car (reverse tablero)))) ;utilidad cuando facil
		)
		((eql (car d) 'M)
		  (setq respuesta (- (seventh tablero) (car (reverse tablero))))
		)
		((eql (car d) 'D)
      (setq respuesta (+ (- (seventh tablero) (car (reverse tablero))) (turno-extra nodo))) ;REVISAR turno-extra, probarlo.
		)
		((> (seventh tablero) 24) (setq respuesta (+ respuesta 100)))
		((> (car (last tablero)) 24) (setq respuesta (+ respuesta 100)))
	)
	(return-from utilidad respuesta)
)

;Funcion que nos dice que acciones estan disponibles.
;"Completo"
(defun acciones-disp (estado)
	(setq acciones-disponibles NIL)
	(setq tablero (third estado))
	(setq tablero-aux (copy-seq tablero))
	(setq i 1)
	(loop
		(when (> i 6) (return acciones-disponibles))
		(if (not (= (car tablero-aux) 0)) (push (genera-nodo i tablero) acciones-disponibles))
		(setq i (+ i 1))
		(setq tablero-aux (cdr tablero-aux))
	)
	(return-from acciones-disp acciones-disponibles)
)

;Funcion generadora de un tablero dado un tablero actual y una posicion de movimiento
;"Completo"
(defun genera-nodo (indice estado)
	(setq cant-piedras (car (nthcdr (- indice 1) estado)))
	(setq cant-piedras-aux cant-piedras)
	(setq aux (copy-seq estado))
	(setq iterador (+ 1 indice))
	(rplaca (nthcdr (- indice 1) aux) 0)
	(setq lugar 2)
	(loop
		(when (= cant-piedras 0) (return aux))
		(cond
			((not (= (mod lugar (- 15 indice)) 0)) (rplaca (nthcdr (- iterador 1) aux) (+ 1 (car (nthcdr (- iterador 1) aux)))) (setq cant-piedras (- cant-piedras 1)))
		)
		(setq iterador (+ 1 iterador))
		(if (> iterador 14) (setq iterador 1))
		(setq lugar (+ lugar 1))
	)
	(cond
		((< cant-piedras-aux 14)
			(setq bolas (+ indice cant-piedras-aux))
			(if (<= 14 bolas) (setq bolas (- bolas 13)))
			(cond
				((and (<= bolas 6) (= (car (nthcdr (- bolas 1) estado)) 0))
					(rplaca (nthcdr 6 aux) (+ (car (nthcdr 6 aux)) (car (nthcdr (- 14 bolas 1) estado)) 1))
					(rplaca (nthcdr (- 13 bolas) aux) 0)
					(rplaca (nthcdr (- bolas 1) aux) 0)
				)
			)
		)
	)
	(return-from genera-nodo aux)
)

;Metodo para saber cuantos turnos extra son posibles en un estado.
(defun turno-extra (estado)
	(setq i 0)
	(setq resp 0)
	(loop
		(when (> i 5) (return resp))
		(if (= (mod (- 7 i) 13) (mod (nth i estado) 13)) (setq resp (+ resp 1)))
		(setq i (+ i 1))
	)
	(return-from turno-extra resp)
)

;Funcion de generacion de nodos por cada movimiento.
;"Completo"
(defun resultado (accion nivel)
	(setq res (list (+ 1 nivel) 0 accion))
	(return-from resultado res)
)

;Funcion para girar el tablero y regresar un nodo cuando tire el oponente.
;"Completo"
(defun gira-estado (nodo)
	(setq tablero (third nodo))
	(return-from gira-estado (append (list (first nodo) (second nodo) (append (nthcdr 7 tablero) (reverse (nthcdr 7 (reverse tablero)))))))
)

;Funcion para girar el tablero cuando tire el oponente.
;"Completo"
(defun gira-tablero (tablero)
	(return-from gira-tablero (append (nthcdr 7 tablero) (reverse (nthcdr 7 (reverse tablero)))))
)
;NOTAS
;-Necesario que la funcion para evaluar estado terminar sea auxiliar para
;conocer cuantos niveles bajar, segun la dificultad.

;Pruebas
(trace maxi-val)
(trace mini-val)
(print (alfa-beta '(0 0 (4 4 4 4 4 4 0 4 4 4 4 4 4 0))))
