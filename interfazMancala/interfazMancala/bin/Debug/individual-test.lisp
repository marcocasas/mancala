(defun genera-nodo (indice estado)
	(setq cant-piedras (car (nthcdr (- indice 1) estado)))
	(setq cant-piedras-aux cant-piedras)
	(setq aux (copy-seq estado))
	(setq aux2 (copy-seq aux))
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
			(setq canicas-en-casilla cant-piedras-aux)
			(if (<= 14 bolas) (setq bolas (- bolas 13)))
			(setq donde-acabo (+ indice cant-piedras-aux))
			(setq donde-acabo (mod donde-acabo 13))
			(cond
				((not (= (nth (- 13 donde-acabo) estado) 0))
					(cond
						((and (<= bolas 6) (= (car (nthcdr (- bolas 1) estado)) 0) (not (= (nth (- 13 bolas) estado) 0)))
							(rplaca (nthcdr 6 aux) (+ (car (nthcdr 6 aux)) (car (nthcdr (- 14 bolas 1) estado)) 1))
							(rplaca (nthcdr (- 13 bolas) aux) 0)
							(rplaca (nthcdr (- bolas 1) aux) 0)
						)
					)
				)
			)
		)
	)
	(return-from genera-nodo aux)
)

;(print (genera-nodo 4 '(4 4 4 2 4 0 10 0 5 6 7 8 9 11)))

(setq salida '(4 1 2 3 0 5 4 0 0 0 0 0 0 10 H))

(cond
	(
		(= (apply #'+ (nthcdr 9 (cdr (reverse salida)))) 0)
		(rplaca (nthcdr 13 salida) (apply #'+ (nthcdr 7 (reverse (cdr (reverse salida))))))
	)
	(
		(= (apply #'+ (nthcdr 7 (reverse (cddr (reverse salida))))) 0)
		(rplaca (nthcdr 6 salida) (apply #'+ (nthcdr 8 (reverse salida))))
	)
)

(print salida)