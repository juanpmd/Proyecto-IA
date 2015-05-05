;################################################
;##########--Juan Pablo Mejia Duque--############
;################################################
;###########--Taller Calificable 2--#############
;##########--Inteligencia Artificial--###########
;################################################
;----------------------------------------------------------------------
;Esta funcion guarda los hechos que hay hasta el momento en la kb
(defun hechos (kb)
	(cond ((null kb) nil)
		((equal (caar kb) nil)(append (cdar kb)(hechos (cdr kb))))
			(t (hechos (cdr kb)))
	)
)
- Entrada:
;(hechos '((((PAmerican x)(PWeapon y)(PSells x y z)(PHostile z))(PCriminal x))(()(POwns Nono M1))(()(PMissile M1))(((PMissile x)(POwns Nono x))(PSells West x Nono))(((PMissile x))(PWeapon x))(((PEnemy x America))(PHostile x))(()(PAmerican West))(()(PEnemy Nono America))))
- Salida:

;----------------------------------------------------------------------

;Esta funcion me retorna un kb sin hechos para empezar trabajando con ella
(defun sinhechos (kb)
	(cond ((null kb) nil)
		((equal (caar kb) nil)(sinhechos (cdr kb)))
			(t (append (list(car kb))(sinhechos (cdr kb))))
	)
)
;(sinhechos '((((PAmerican x)(PWeapon y)(PSells x y z)(PHostile z))(PCriminal x))(()(POwns Nono M1))(()(PMissile M1))(((PMissile x)(POwns Nono x))(PSells West x Nono))(((PMissile x))(PWeapon x))(((PEnemy x America))(PHostile x))(()(PAmerican West))(()(PEnemy Nono America))))
;----------------------------------------------------------------------

;Esta funcion verifica que en una expresion se puda llegar al consecuente remplazando al antecedente con los hechos que ya hay
(defun verificacion (kb hechos temporal)
)


;----------------------------------------------------------------------


;----------------------------------------------------------------------


;----------------------------------------------------------------------


;----------------------------------------------------------------------







