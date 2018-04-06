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
		(if (NOT (ZEROP (car casillas))) (push acciones-disponibles i))
		(setq casillas (cdr casillas) i (+ i 1))
	)
)

;NOTAS
;-Necesario que la funcion para evaluar estado terminar sea auxiliar para
;conocer cuantos niveles bajar, segun la dificultad.
;