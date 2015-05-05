;################################################
;##########--Juan Pablo Mejia Duque--############
;################################################
;###########--------Proyecto--------#############
;##########--Inteligencia Artificial--###########
;################################################
;-------------------------------------------------------------------------------------------
;__________________________________Razonamiento Hacia Adelante______________________________ 

;=================FUNCION BUSCARELEM==================
;SE USA EN FUNCION: variable
;Buscar un elemento en una lista
(defun buscarelem (lista elem) ;falta colocar que funcione con numeros
	(cond ((null lista) nil)
		((equal (car lista) elem) t)
			(t (buscarelem (cdr lista) elem))
	)
)
;Entrada:
;(buscarelem '(a b c d e f g h i j k l m n o q r s u v w x y z) 'Nono)
;Salida:
; NIL

;=================FUNCION CAMBIOLISTA==================
;SE USA EN FUNCION: remplazo
;esta funcion cambia el elemento elem de la lista por remp y retorna la lista remplazada
(defun cambiolista (lista elem remp final)
	(cond ((null lista) final)
		((equal (car lista) elem)(cambiolista (cdr lista) elem remp (append final (list remp))))
			(t (cambiolista (cdr lista) elem remp (append final (list (car lista)))))
	)
)
;Entrada:
;(cambiolista '(PMissile x y) 'x 'Nono '())
;Salida:
;(PMISSILE NONO Y)

;=================FUNCION REMPLAZO==================
;SE USA EN FUNCION: cambiofinal
;Esta funcion es lo mismo que cambio lista pero para una lista de listas
(defun remplazo (listkb elem remp) 
	(cond ((null listkb) nil)
		(t (append (list (cambiolista (car listkb) elem remp '())) (remplazo (cdr listkb) elem remp)))
	)
)
;Entrada:
;(remplazo '((PAmerican x)(PWeapon y)(PSells x y z)(PHostile z)(PCriminal x)) 'x 'West)
;Salida:
;((PAMERICAN WEST) (PWEAPON Y) (PSELLS WEST Y Z) (PHOSTILE Z) (PCRIMINAL WEST))


;=================FUNCION CAMBIOFINAL==================
;SE USA EN FUNCION: cambiovariables
;Esta funcion es para hacer el cambio total de toda una expresion
(defun cambiofinal (listkb elem remp)
	(cond ((null listkb) nil)
		(t (append (list (remplazo (car listkb) elem remp))(remplazo (cdr listkb) elem remp)))
	)
)
;Entrada:
;(cambiofinal '(((PAmerican x)(PWeapon y)(PSells x y z)(PHostile z))(PCriminal x)) 'x 'Juan)
;Salida:
;(((PAMERICAN JUAN) (PWEAPON Y) (PSELLS JUAN Y Z) (PHOSTILE Z)) (PCRIMINAL JUAN))











;=================FUNCION HECHOSKB==================
;SE USA EN FUNCION: forward
;Esta funcion guarda los hechos de la entrada kb
(defun hechoskb (kb)
	(cond ((null kb) nil)
		((equal (caar kb) nil)(append (cdar kb)(hechoskb (cdr kb))))
			(t (hechoskb (cdr kb)))
	)
)
;Entrada:
;(hechoskb '((((PAmerican x)(PWeapon y)(PSells x y z)(PHostile z))(PCriminal x))(()(POwns Nono M1))(()(PMissile M1))(((PMissile x)(POwns Nono x))(PSells West x Nono))(((PMissile x))(PWeapon x))(((PEnemy x America))(PHostile x))(()(PAmerican West))(()(PEnemy Nono America))))
;Salida:
;((POWNS NONO M1) (PMISSILE M1) (PAMERICAN WEST) (PENEMY NONO AMERICA))

;==============FUNCION EXPRESIONESKB================
;SE USA EN FUNCION: forward
;Esta funcion me retorna solo las expresiones de la kb
(defun expresioneskb (kb)
	(cond ((null kb) nil)
		((equal (caar kb) nil)(expresioneskb (cdr kb)))
			(t (append (list(car kb))(expresioneskb (cdr kb))))
	)
)
;Entrada:
;(expresioneskb '((((PAmerican x)(PWeapon y)(PSells x y z)(PHostile z))(PCriminal x))(()(POwns Nono M1))(()(PMissile M1))(((PMissile x)(POwns Nono x))(PSells West x Nono))(((PMissile x))(PWeapon x))(((PEnemy x America))(PHostile x))(()(PAmerican West))(()(PEnemy Nono America))))
;Salida:
;((((PAMERICAN X)(PWEAPON Y)(PSELLS X Y Z)(PHOSTILE Z))(PCRIMINAL X))(((PMISSILE X)(POWNS NONO X))(PSELLS WEST X NONO))(((PMISSILE X))(PWEAPON X))(((PENEMY X AMERICA))(PHOSTILE X)))

;=================FUNCION ANTESTA==================
;SE USA EN FUNCION: secumple
;Esta funcion mira si un antecedente se encuentra en los hechos (no importa las variables que tiene)
(defun antesta (antecedente hechos)
	(cond ((null hechos) nil)
		((equal (car antecedente) (caar hechos)) t)
			(t (antesta antecedente (cdr hechos)))
	)
)
;Entrada:
;(antesta '(PEnemy x America) '((POwns Nono M1)(PMissile M1)(PAmerican West)(PEnemy Nono America)))
;Salida:
; T

;=================FUNCION SECUMPLE==================
;SE USA EN FUNCION: Desenrollar
;Este funcion mira si los antecedentes de una expresion se pueden suplir con los hechos para asi poder sacar despues su consecuente como un hecho
(defun secumple (expresion hechos)
	(cond ((null hechos) nil)
		((equal (car expresion) nil) t)
			((equal (antesta (caar expresion) hechos) t)(secumple (append (list (cdar expresion))(cdr expresion)) hechos))
				(t nil)
	)
) 
;Entrada:
;(secumple '(((PMissile x)(POwns Nono x))(PSells West x Nono)) '((PMissile M1)(PAmerican West)(PEnemy Nono America)(POwns Nono M1)))
;Salida:
; T

;=================FUNCION CUMPLEUNAEXP==================
;SE USA EN FUNCION: Desenrollar
;Esta funcion lo que hace es ver si de todas las expresiones de entrada hay alguna que si se logre cumplir con los hechos que hay
(defun cumpleunaexp (expresiones hechos) 
	(cond ((null expresiones) nil)
		((equal (secumple (car expresiones) hechos) t) t)
			(t (cumpleunaexp (cdr expresiones) hechos))
	)
)
;Entrada:
;(cumpleunaexp '((((PAmerican x)(PWeapon y)(PSells x y z)(PHostile z))(PCriminal x))(((PMissile x)(POwns Nono x))(PSells West x Nono))(((PMissile x))(PWeapon x))(((PEnemy x America))(PHostile x))) '((PMissile M1)(PAmerican West)(PEnemy Nono America)(POwns Nono M1)))
;Salida:
; T

;=================FUNCION BUSCARHECHO==================
;SE USA EN FUNCION: nuevaexpresion
;Esta funcion lo que hace es encontrar cual es el hecho que puede utilizar para el cambio de variable para un antecedente
(defun buscarhecho (antecedente hechos)
	(cond ((null hechos) nil)
		((equal (caar hechos) (car antecedente))(car hechos))
			(t (buscarhecho antecedente (cdr hechos)))
	)
)
;Entrada:
;(buscarhecho '(POwns Nono x) '((PMissile M1)(PAmerican West)(PEnemy Nono America)(POwns Nono M1)))
;Salida:
;(POWNS NONO M1)

;=================FUNCION VARIABLE==================
;SE USA EN FUNCION: nuevaexpresion
;Esta funcion me devuelve las variables que voy a cambiar de un antecedente de acuerdo a un hecho
(defun variable (antecedente hecho)
	(cond ((null antecedente) nil)
		((AND (equal (buscarelem '(a b c d e f g h i j k l m n o q r s u v w x y z) (car antecedente)) t)
			(equal (buscarelem '(a b c d e f g h i j k l m n o q r s u v w x y z) (car hecho)) nil))
				(append (list (car antecedente)) (variable (cdr antecedente) (cdr hecho))))
			(t (variable (cdr antecedente) (cdr hecho)))
	)
)
;Entrada:
;(variable '(PSells x y z) '(PSells West M1 Nono))
;Salida:
; (X Y Z)

;=================FUNCION VALORVARIABLE==================
;SE USA EN FUNCION: nuevaexpresion
(defun valorvariable (antecedente hecho)
	(cond ((null antecedente) nil)
		((AND (equal (buscarelem '(a b c d e f g h i j k l m n o q r s u v w x y z) (car antecedente)) t)
			(equal (buscarelem '(a b c d e f g h i j k l m n o q r s u v w x y z) (car hecho)) nil))
				(append (list (car hecho)) (valorvariable (cdr antecedente) (cdr hecho))))
			(t (valorvariable (cdr antecedente) (cdr hecho)))
	)
)
;Entrada:
;(valorvariable '(PSells x y z) '(PSells West M1 Nono))
;Salida:
; (WEST M1 NONO)

;=================FUNCION CAMBIOVARIABLES==================
;SE USA EN FUNCION: nuevaexpresion
;Esta funcion cambia todas las variables de la expresion por los valores nuevos
(defun cambiovariables (expresion variables valores)
	(cond ((null variables) expresion)
		(t (cambiovariables (cambiofinal expresion (car variables) (car valores)) (cdr variables) (cdr valores)))
	)
)
;Entrada:
;(cambiovariables '(((PAmerican x)(PWeapon y)(PSells x y z)(PHostile z))(PCriminal x)) '(x) '(West))
;Salida:
;(((PAMERICAN WEST) (PWEAPON Y) (PSELLS WEST Y Z) (PHOSTILE Z)) (PCRIMINAL WEST))



;=================FUNCION NUEVAEXPRESION==================
;SE USA EN FUNCION: nuevohecho
;Esta funcion saca de una expresion, la nueva expesion con las variables cambiadas
(defun nuevaexpresion (expresion hechos)
	(cond ((null expresion) nil)
		;(t (cambiovariables expresion (variable (car expresion) (buscarhecho (car expresion) hechos)) (valorvariable (car expresion) (buscarhecho (car expresion) hechos))))
		(t (cambiovariables expresion (variable (caar expresion)(buscarhecho (caar expresion) hechos)) (valorvariable (caar expresion) (buscarhecho (caar expresion) hechos))))
	)
)

;(nuevaexpresion '(((PMissile x)(POwns Nono x))(PSells West x Nono)) '((PMissile M1)(PAmerican West)(PEnemy Nono America)(POwns Nono M1)))


;=================FUNCION NUEVOHECHO==================
;SE USA EN FUNCION: Desenrollar
;Esta funcion lo que hace es de una expresion que si se cumple, coge los hechos que hay y saca un nuevo hecho
(defun nuevohecho (expresion hechos)
	(cond ((null expresion) nil)
		((equal (car expresion) nil) (cdr expresion))
			(t (nuevohecho (append (list (cdar (nuevaexpresion expresion hechos)))(cdr (nuevaexpresion expresion hechos))) hechos))
			;Cambia en la ultima linea expresion por la expresion con las nuevas variables reemplazadas
			
	)
)
;Entrada:
;(nuevohecho '(((PMissile x)(POwns Nono x))(PSells West x Nono)) '((PMissile M1)(PAmerican West)(PEnemy Nono America)(POwns Nono M1)))
;Salida:
;((PSELLS WEST M1 NONO))

;===============FUNCION DESENROLLAR================
;SE USA EN FUNCION: forward
;Exp=expresiones, esta funcion lo que hace es desenrollar lo mas que puede las expresiones,
;es decir sacar la mayor cantidad de hechos que se pueden para despues buscar en esa lista
;de hechos el que se busca (Ej: (PCriminal West))
(defun desenrollar (expresiones hechos)
	(cond ((null expresiones) hechos)
		((equal (secumple (car expresiones) hechos) t)
			(desenrollar (cdr expresiones) (append hechos (nuevohecho (car expresiones) hechos))))
		;Revisar en la linea anterior que el append si me cree el sistema como lo busco
			((AND (equal (secumple (car expresiones) hechos) nil)(equal (cumpleunaexp expresiones hechos) t))
				(desenrollar (append (cdr expresiones)(list (car expresiones))) hechos))
			;Revisar en la linea anterior si el append necesita el list o no
		(t hechos)
	)
)
;Entrada:
;(desenrollar '((((PAmerican x)(PWeapon y)(PSells x y z)(PHostile z))(PCriminal x))(((PMissile x)(POwns Nono x))(PSells West x Nono))(((PMissile x))(PWeapon x))(((PEnemy x America))(PHostile x))) '((POwns Nono M1)(PMissile M1)(PAmerican West)(PEnemy Nono America)))
;Salida:
;((POWNS NONO M1) (PMISSILE M1) (PAMERICAN WEST) (PENEMY NONO AMERICA) (PSELLS WEST M1 NONO) (PWEAPON M1) (PHOSTILE NONO) (PCRIMINAL WEST))

;=================FUNCION ENCONTROHECHO==================
;SE USA EN FUNCION: forward
(defun encontrohecho (hechosfinales hecho)
	(cond ((null hechosfinales) nil)
		((equal (car hechosfinales) hecho) t)
			(t (encontrohecho (cdr hechosfinales) hecho))
	)
)
;Entrada:
;(encontrohecho '((POWNS NONO M1) (PMISSILE M1) (PAMERICAN WEST) (PENEMY NONO AMERICA) (PSELLS WEST M1 NONO) (PWEAPON M1) (PHOSTILE NONO) (PCRIMINAL WEST)) '(PCriminal West))
;Salida:
;((POWNS NONO M1) (PMISSILE M1) (PAMERICAN WEST) (PENEMY NONO AMERICA) (PSELLS WEST M1 NONO) (PWEAPON M1) (PHOSTILE NONO) (PCRIMINAL WEST))
	
;=================FUNCION FORWARD==================
;Esta es la funcion final del procedimiento hacia adelante
(defun forward (kb hecho)
	(cond ((null kb) nil)
		((equal (encontrohecho (desenrollar (expresioneskb kb) (hechoskb kb)) hecho) t) t)
		;Hay problemas en desenrollar
			(t nil)
	)
)
;Entrada:
;(forward '((((PAmerican x)(PWeapon y)(PSells x y z)(PHostile z))(PCriminal x))(()(POwns Nono M1))(()(PMissile M1))(((PMissile x)(POwns Nono x))(PSells West x Nono))(((PMissile x))(PWeapon x))(((PEnemy x America))(PHostile x))(()(PAmerican West))(()(PEnemy Nono America))) '(PCriminal West))
;Salida:
; T

;-------------------------------------------------------------------------------------------
;__________________________________Razonamiento Hacia Atras_________________________________ 

