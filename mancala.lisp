;Formato de nodo para juego Mancala
;(idEstado nivel idPapa *utilidad (11 12 13 14 15 16 P1 21 22 23 24 25 26 P2))

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
)

(defun alfa-beta (estado)
	(setq valorAccion (maxi-val estado -9999 9999))
	(setq accion (selec-accion estado valorAccion))
	(return accion)
)

(defun maxi-val (estado alfa beta)
	(cond
		((eql T (verifica-corte (fifth estado) (second estado))) (return (utilidad estado)))
		(T
			(setq v -9999)
			(setq acciones (acciones-disp estado))
			(loop
				(when (eql acciones NIL) (return v))
				(setq accion (pop acciones))
				(setq v (max v (mini-val (resultado estado accion) alfa beta)))
				(cond
					((>= v beta) (return v))
					(T (setq alfa (max alfa v)))
				)
				(setq alfa (max alfa v))
			)
			(return v)
		)
	)
)

(defun mini-val (estado alfa beta)
	(cond
		((eql T (verifica-corte estado)) (return (utilidad estado)))
		(T
			(setq v 9999)
			(setq acciones (acciones-disp estado))
			(loop
				(when (eql acciones NIL) (return v))
				(setq accion (pop acciones))
				(setq v (min v (max-val (resultado estado accion) alfa beta)))
				(cond
					((<= v alfa) (return v))
					(T (setq alfa (max alfa v)))
				)
				(setq beta (max alfa v))
			)
			(return v)
		)
	)
)

(defun selec-accion)
;(defun evalua)
(defun verifica-corte (estado prof)
	(cond
		((> prof profundidad) (setq res T))
		((or (> (seventh estado) 24) (> (car (last estado)) 24) ) (setq res T))
		((= (apply #'+ (nthcdr 8 (reverse estado))) 0) (setq res T))
		((= (- (apply #'+ (nthcdr 7 estado)) (car (last estado))) 0) (setq res T))
		(t(setq res NIL))
	)
	(return-from verifica-corte res)
)

;Funcion para determinar la utilidad de cada estado.
(defun utilidad ()
	(cond
		((eql (car d) 'F)
			(setq respuesta (- (seventh estado) (car (reverse estado))));utilidad cuando facil
		)
		((eql (car d) 'M)
		  (setq respuesta (- (seventh estado) (car (reverse estado))))
		)
		((eql (car d) 'D)
      (setq respuesta (+ (- (seventh estado) (car (reverse estado))) (turno-extra estado)))
		)
	)
)

;Funcion que nos dice que acciones estan disponibles.
(defun acciones-disp (estado)
	(setq acciones-disponibles NIL)
	(setq casillas (fifth estado))
	(setq casillas-aux (copy-seq casillas))
	(setq i 1)
	(loop
		(when (> i 6) (return acciones-disponibles))
		(if (not (= (car casillas-aux) 0)) (push (genera-nodo i casillas) acciones-disponibles))
		(setq i (+ i 1))
		(setq casillas-aux (cdr casillas-aux))
	)
	(return-from acciones-disp acciones-disponibles)
)

;Funcion generadora de un tablero dado un tablero actual y una posicion de movimiento
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

;Metodo para saber cuantos turnos extra son posibles en un estado
(defun turno-extra (estado)
	(setq i 0)
	(setq resp 0)
	(loop
		(when (> i 5) (return resp))
		(if (= (mod (- 7 i) 13) (mod (nth i estado) 13)) (setq resp (+ resp 1)))
		(setq i (+ i 1))
	)
)

(defun resultado)

;NOTAS
;-Necesario que la funcion para evaluar estado terminar sea auxiliar para
;conocer cuantos niveles bajar, segun la dificultad.
