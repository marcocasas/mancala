(defun lee-txt()
	(let ((in (open "jugada.txt" :if-does-not-exist nil)))
		(when in
			(loop for line = (read-line in nil)
				while line do (setq jugada (read-from-string line))
			)
			(close in)
		)
	)
)

(lee-txt)

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

;Funcion para girar el tablero cuando tire el oponente.
;"Completo"
(defun gira-tablero (tablero)
	(return-from gira-tablero (append (nthcdr 7 tablero) (reverse (nthcdr 7 (reverse tablero)))))
)

(setq tirada (genera-nodo (car jugada) (cadr jugada)))

;(print jugada)
(print tirada)
;(with-open-file (str "tirada-hecha.txt"
;                     :direction :output
;                     :if-exists :supersede
;                     :if-does-not-exist :create)
;  (print tirada))

;(open "tirada-hecha.txt" :direction :output :if-exists :supersede)