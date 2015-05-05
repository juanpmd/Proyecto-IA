
;################################################
;##########--Juan Pablo Mejia Duque--############
;################################################
;###########--Taller Calificable 2--#############
;##########--Inteligencia Artificial--###########
;################################################

;----------------------------------------------------------------------

;Esta funcion dice si ese puede llegar al hecho por alguna de las expresiones, si dice que si el programa puede continuar, 
;sino retornara directo nil
(defun estaexpresion (kb exp)
	(cond ((null kb) nil)
		((equal (car exp) (caar (cdar kb))) t)
			(t (estaexpresion (cdr kb) exp))
	)
)
;(estaexpresion '((()(PEnemy Nono America))(((PMissile x)(POwns Nono x))(PSells West x Nono))(((PEnemy x America))(PHostile x))) 
;'(PHostile Carros))
;----------------------------------------------------------------------

;Esta funcion busca si ya se comprobo la expresion en el kb
(defun encontro (kb exp)
	(cond ((null kb) nil)
		((equal exp (car (cdar kb))) t)
			(t (encontro (cdr kb) exp))
	)
)
;Esta prueba retorna nil
;(encontro '((()(PEnemy Nono America))(((PMissile x)(POwns Nono x))(PSells West x Nono))(((PEnemy x America))(PHostile x))) '(PHostile Carros))
;Esta prueba retorna T
;(encontro '((()(PEnemy Nono America))(((PMissile x)(POwns Nono x))(PSells West x Nono))(((PEnemy x America))(PHostile x))) '(PHostile x))


(defun size (lista)
	(cond ((null lista) 0) 
		(t (+ 1 (size (cdr lista))))
	)
)

;----------------------------------------------------------------------

;esta funcion cambia el elemento elem de la lista por remp y retorna la lista remplazada
(defun cambiolista (lista elem remp final)
	(cond ((null lista) final)
		((equal (car lista) elem)(cambiolista (cdr lista) elem remp (append final (list remp))))
			(t (cambiolista (cdr lista) elem remp (append final (list (car lista)))))
	)
)
;(remplazo2 '(PMissile x y) 'x 'Nono '())


;Esta funcion es lo mismo que cambio lista pero para una lista de listas
(defun remplazo (listkb elem remp) 
	(cond ((null listkb) nil)
		(t (append (list (cambiolista (car listkb) elem remp '())) (remplazo (cdr listkb) elem remp)))
	)
)
;(remplazo '((PAmerican x)(PWeapon y)(PSells x y z)(PHostile z)(PCriminal x)) 'x 'Carlos 
;(remplazo '((PMissile x)(POwns Nono x)) 'x 'M1)


;Esta funcion es para hacer el cambio total de toda una expresion
(defun cambiofinal (listkb elem remp)
	(cond ((null listkb) nil)
		(t (append (list (remplazo (car listkb) elem remp))(remplazo (cdr listkb) elem remp)))
	)
)
;(cambiofinal '(((PAmerican x)(PWeapon y)(PSells x y z)(PHostile z))(PCriminal x)) 'x 'Juan)
;(cambiofinal '(()(PMissile M1)) 'x 'Juan)

;----------------------------------------------------------------------

;Buscar un elemento en una lista
(defun buscarelem (lista elem) ;falta colocar que funcione con numeros
	(cond ((null lista) nil)
		((equal (car lista) elem) t)
			(t (buscarelem (cdr lista) elem))
	)
)
;(buscarelem '(a b c d e f g h i j k l m n o q r s u v w x y z) 'Nono)

;----------------------------------------------------------------------

;Estas dos funciones siguientes (trabajo y trabajoPalabra) retorna cada una la variable que se necesita
; y la otra retorna el valor que tomara esa variable
(defun trabajo (listkb predicado)
	(cond ((null listkb) nil)
		((AND (equal (buscarelem '(a b c d e f g h i j k l m n o q r s u v w x y z) (car listkb)) t)
			(equal (buscarelem '(a b c d e f g h i j k l m n o q r s u v w x y z) (car predicado)) nil))
				(append (list (car listkb)) (trabajo (cdr listkb) (cdr predicado))))
			(t (trabajo (cdr listkb) (cdr predicado)))
	)
)
;(trabajo '(Nono x y z) '(Nono M1 y Arroz))
;Esto me devuelve (x z)


(defun trabajoPalabra (listkb predicado)
	(cond ((null listkb) nil)
		((AND (equal (buscarelem '(a b c d e f g h i j k l m n o q r s u v w x y z) (car listkb)) t)
			(equal (buscarelem '(a b c d e f g h i j k l m n o q r s u v w x y z) (car predicado)) nil))
				(append (list (car predicado)) (trabajoPalabra (cdr listkb) (cdr predicado))))
			(t (trabajoPalabra (cdr listkb) (cdr predicado)))
	)
)
;(trabajoPalabra '(Nono x y z) '(Nono M1 y Arroz))
;Esto me devuelve (M1 Arroz)



;Con estas siguientes funciones reviso un hecho en una lista de listas (retornan las listas de las variables 
;a cambiar y de los datos por los que se cambiaran)
(defun busca (listkb predicado)
	(cond ((null listkb) nil)
		((equal (caar listkb) (car predicado))(trabajo (cdar listkb) (cdr predicado)))
			(t (busca (cdr listkb) predicado))
	)
)
;(busca '((PMissile x)(POwns Nono x)) '(POwns Nono M1))


(defun buscaPalabra (listkb predicado)
	(cond ((null listkb) nil)
		((equal (caar listkb) (car predicado))(trabajoPalabra (cdar listkb) (cdr predicado)))
			(t (buscaPalabra (cdr listkb) predicado))
	)
)
;(buscaPalabra '((PMissile x y z)(POwns Nono x)) '(POwns Nono M1))
;el resultado es (M1)

;----------------------------------------------------------------------

;Esta funcion cambia todas las variables de listkb por los valores
(defun total (listkb variables valores)
	(cond ((null variables) listkb)
		(t (total (cambiofinal listkb (car variables) (car valores)) (cdr variables) (cdr valores)))
	)
)
;(total '(((PAmerican x)(PWeapon y)(PSells x y z)(PHostile z))(PCriminal x)) '(x y z) '(M1 M2 M3))
;el resultado es (((PAMERICAN M1) (PWEAPON M2) (PSELLS M1 M2 M3) (PHOSTILE M3)) (PCRIMINAL M1))


(defun verificar (lista)
	(cond ((null lista) nil)
		((equal (car lista) nil) t)
			(t (verificar (cdr lista)))
	)
)
;(vericiar '(()(PAmerican West)))
;Esto devuelve T
;(vericiar '((PEnemy Nono)(PAmerican West)))
;Esto devuelve nil


;Frente a un kb y una expresion, el cambia todo el kb respecto a esa expresion
(defun cambios2 (kb exp temp)
	(cond ((null kb) temp)
		((equal (verificar (car kb)) t) (cambios2 (cdr kb) exp (append temp (list (car kb)))))
			;(t (cambios2 (cdr kb) exp (append temp (list (car kb)))))
			;(t (cambios2 (cdr kb) exp (append temp (list (cambiofinal (car kb) 'x 'Juan)))))
			(t (cambios2 (cdr kb) exp (append temp (list (total (car kb) (busca (caar kb) exp) (buscaPalabra (caar kb) exp))))))
	)
)
;(cambios2 '((((PAmerican x)(PWeapon y)(PSells x y z)(PHostile z))(PCriminal x))(()(POwns Nono M1))(()(PMissile M1))(((PMissile x)(POwns Nono x))(PSells West x Nono))(((PMissile x))(PWeapon x))(((PEnemy x America))(PHostile x))(()(PAmerican West))(()(PEnemy Nono America))) '(PEnemy Nono America) '())
;El resultado es
;((((PAMERICAN X) (PWEAPON Y) (PSELLS X Y Z) (PHOSTILE Z)) (PCRIMINAL X))(NIL (POWNS NONO M1))(NIL (PMISSILE M1))(((PMISSILE X) (POWNS NONO X)) (PSELLS WEST X NONO))(((PMISSILE X)) (PWEAPON X))(((PENEMY NONO AMERICA)) (PHOSTILE NONO))(NIL (PAMERICAN WEST))(NIL (PENEMY NONO AMERICA)))


(defun final (kb pos)
	(cond ((equal pos (size kb)) kb)
		(t (final (cambios2 kb (cadr (nth (+ pos 0) kb)) '()) (+ 1 pos)))
	)
)

;(final '((((PAmerican x)(PWeapon y)(PSells x y z)(PHostile z))(PCriminal x))(()(POwns Nono M1))(()(PMissile M1))(((PMissile x)(POwns Nono x))(PSells West x Nono))(((PMissile x))(PWeapon x))(((PEnemy x America))(PHostile x))(()(PAmerican West))(()(PEnemy Nono America))) '0)

;----------------------------------------------------------------------

;Esta es la funcion principal que revisa que si se pueda llegar a el objetivo y ademas que si se llego con el kb
(defun predicadosfinal (kb exp)
	(cond ((equal (estaexpresion kb exp) nil) nil)
		((equal (encontro (final kb '0) exp) t) t)
			(t nil)
	)
)
;Este resultado deberia retornar T
;(predicadosfinal '((((PAmerican x)(PWeapon y)(PSells x y z)(PHostile z))(PCriminal x))(()(POwns Nono M1))(()(PMissile M1))(((PMissile x)(POwns Nono x))(PSells West x Nono))(((PMissile x))(PWeapon x))(((PEnemy x America))(PHostile x))(()(PAmerican West))(()(PEnemy Nono America))) '(PCriminal West))


;Este ejemplo deberia retornar nil ya que PArmaggedon no puede remplazar la variable y por lo que nunca demostraria que y es Carlos
;(predicadosfinal '((()(PAmerican West))(()(PEnemy Nono America)))(()(POwns Nono M1))(()(PMissile M1))(((PAmerican West)(PWeapon M1)(PSells West M1 Nono)(PHostile Nono))(PCriminal West))(((PMissile M1)(POwns Nono M1))(PSells West M1 Nono))(((PMissile M1))(PWeapon M1))(((PEnemy Nono America))(PHostile Nono))(((POwns x M1)(PMissile x))(PArmaggedon x y))) '(PArmaggedon Nono Carlos))

;----------------------------------------------------------------------
;Esta funcion guarda los hechos que hay hasta el momento en la kb
(defun hechos (kb)
	(cond ((null kb) nil)
		((equal (caar kb) nil)(append (list (car kb))(hechos (cdr kb))))
			(t (hechos (cdr kb)))
	)
)
;(hechos '((((PAmerican x)(PWeapon y)(PSells x y z)(PHostile z))(PCriminal x))(()(POwns Nono M1))(()(PMissile M1))(((PMissile x)(POwns Nono x))(PSells West x Nono))(((PMissile x))(PWeapon x))(((PEnemy x America))(PHostile x))(()(PAmerican West))(()(PEnemy Nono America))))