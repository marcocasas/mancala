(setq d '())

(let ((in (open "dificultad.txt" :if-does-not-exist nil)))
	(when in
		(loop for line = (read-line in nil)
			while line do (push (intern line) d)
		)
		(close in)
	)
)

(print d)
;Determina la profundidad de acuerdo a la dificultad elegida por el usuario.
(cond
	( (equal (car d) 'F) (setq profundidad 3) )
	( (equal (car d) 'M) (setq profundidad 6) )
	( (equal (car d) 'D) (setq profundidad 9) )
)

(print profundidad)
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

(print (verifica-corte '(0 0 0 0 0 1 24 0 0 0 0 0 1 24) 3))
