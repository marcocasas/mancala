;Formato de nodo para juego Mancala
;(idEstado nivel idPapa *utilidad (11 12 13 14 15 16 P1 21 22 23 24 25 26 P2))

(defun alfa-beta (estado)
	(setq valorAccion (maxi-val estado -9999 9999))
	(setq accion (selec-accion estado valorAccion))
	(return accion)
)

(defun maxi-val (estado alfa beta)
	(cond
		((eql T (verifica-terminal estado)) (return (utilidad estado)))
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
		((eql T (verifica-terminal estado)) (return (utilidad estado)))
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
(defun verifica-terminal)
(defun utilidad)

;Método que nos dice que acciones estan disponibles.
(defun acciones-disp (estado)
	(setq acciones-disponibles NIL)
	(setq casillas (fifth estado))
	(setq i 1)
	(loop 
		(when (null casillas) (return acciones-disponibles))
		(if (not (zerop (car casillas))) (push acciones-disponibles (genera-nodo i estado)))
		(setq casillas (cdr casillas) i (+ i 1))
	)
)

(defun genera-nodo (indice estado)
	(setq cant-piedras (car (nthcdr (- indice 1) estado)))
	(setq cant-piedras-aux cant-piedras)
	;(print 'cant-piedras-inicial)
	;(print cant-piedras)
	(setq aux (copy-seq estado))
	;(print 'aux-inicial)
	;(print aux)
	(setq iterador (+ 1 indice))
	;(print 'iterador-inicial)
	;(print iterador)
	(rplaca (nthcdr (- indice 1) aux) 0)
	;(print 'aux-tras-cero)
	;(print aux)
	(setq lugar 2)
	(loop
		(when (= cant-piedras 0) (return aux))
		(cond
			((not (= (mod lugar (- 15 indice)) 0)) (rplaca (nthcdr (- iterador 1) aux) (+ 1 (car (nthcdr (- iterador 1) aux)))) (setq cant-piedras (- cant-piedras 1)))
		)
		(setq iterador (+ 1 iterador))
		(if (> iterador 14) (setq iterador 1))
		;(print 'aux-modificado-suma)
		;(print aux)
		;(print 'cant-piedras-loop)
		;(print cant-piedras)
		(setq lugar (+ lugar 1))
		;(print 'lugar)
		;(print lugar)
	)
	(print aux)
	(cond
		((< cant-piedras-aux 14)
			(print cant-piedras-aux)
			(setq bolas (+ indice cant-piedras-aux))
			(print bolas)
			(if (>= bolas 14) (rplaca (nthcdr 6 aux) (+ 1 (car (nthcdr 6 aux)))))
			(if (<= 14 bolas) (setq bolas (- bolas 13)))
			(print bolas)
			(cond
				((and (<= bolas 6) (= (car (nthcdr (- bolas 1) estado)) 0))
					(print bolas)
					(print (car (nthcdr 6 aux)))
					(print (car (nthcdr (- 14 bolas 1) estado)))
					(rplaca (nthcdr 6 aux) (+ (car (nthcdr 6 aux)) (car (nthcdr (- 14 bolas 1) estado)) 1))
					(rplaca (nthcdr (- 13 bolas) aux) 0)
					(rplaca (nthcdr (- bolas 1) aux) 0)
				)
			)
		)
	)
	(print aux)
)

(print (genera-nodo 6 '(0 0 2 0 0 8 4 8 4 8 0 2 5 7)))

(defun resultado)

;NOTAS
;-Necesario que la funcion para evaluar estado terminar sea auxiliar para
;conocer cuantos niveles bajar, segun la dificultad.
;