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
;Esta funcion devuelve todos los hechos posibles resultantes del kb
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
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;__________________________________Razonamiento Hacia Atras_________________________________ 
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------


;=================FUNCION CAMBIOVARBACKWARD==================
;SE USA EN FUNCION: expmodificada
;Esta funcion cambia todas las variables del antecedente por los valores nuevos
(defun cambiovarbackward (expresion variables valores)
	(cond ((null variables) expresion)
		(t (cambiovariables (remplazo expresion (car variables) (car valores)) (cdr variables) (cdr valores)))
	)
)
;Entrada:
;(cambiovariables '(((PAmerican x)(PWeapon y)(PSells x y z)(PHostile z))) '(x) '(West))
;Salida:
;(((PAMERICAN WEST) (PWEAPON Y) (PSELLS WEST Y Z) (PHOSTILE Z)) (PCRIMINAL WEST))

;=================FUNCION ENCONTROHECHO==================
;SE USA EN FUNCION: basetrabajo
;Esta funcion de acuerdo a una expresion y a un hecho, devuelve la expresion pero con la modificacion de variables
(defun expmodificada (expresion hecho)
	(cond ((null expresion) nil)
		((equal (caadr expresion) (car hecho))(cambiovarbackward (car expresion) (variable (cadr expresion) hecho) (valorvariable (cadr expresion) hecho)))
			(t nil)
	)
)
;Entrada:
;(expmodificada '(((PAmerican x)(PWeapon y)(PSells x y z)(PHostile z))(PCriminal x)) '(PCriminal West))
;Salida:
;((PAMERICAN WEST) (PWEAPON Y) (PSELLS WEST Y Z) (PHOSTILE Z))

;=================FUNCION ENCONTROHECHO==================
;SE USA EN FUNCION: backward
;Esta funcion trae de acuerdo a un hecho que se busca, sus antecedentes con el cual se empieza a trabajar el backward
(defun basetrabajo (kb hecho)
	(cond ((null kb) nil)
		((equal (caadar kb) (car hecho)) (expmodificada (car kb) hecho))
			(t (basetrabajo (cdr kb) hecho))
	)
)

;Entrada:
;(basetrabajo '((((PAmerican x)(PWeapon y)(PSells x y z)(PHostile z))(PCriminal x))(()(POwns Nono M1))(()(PMissile M1))(((PMissile x)(POwns Nono x))(PSells West x Nono))(((PMissile x))(PWeapon x))(((PEnemy x America))(PHostile x))(()(PAmerican West))(()(PEnemy Nono America))) '(PCriminal West))
;Salida:
;((PAMERICAN WEST) (PWEAPON Y) (PSELLS WEST Y Z) (PHOSTILE Z))

;=================FUNCION HECHOSEXP==================
;SE USA EN FUNCION: estahecho
;Esta funcion dice si existe al menos 1 hecho que pueda comparar para ver si puede suplir la expresion
(defun enchechos (exp hechoskb)
	(cond ((null hechoskb) nil)
		((equal (caar hechoskb) (car exp)) t)
			(t (enchechos exp (cdr hechoskb)))
	)
)
;Entrada:
;(enchechos '(PMissile M1) '((POWNS NONO M1)(PMISSILE M1)(PAMERICAN WEST)(PENEMY NONO AMERICA)(PMissile M2)))
;Salida:
;T

;=================FUNCION HECHOSEXP==================
;SE USA EN FUNCION: estahecho
;Esta funcion se le da una expresion y devuelve una lista de hechos posibles que pueden suplirlo
(defun hechosexp (exp hechoskb res)
	(cond ((null hechoskb) res)
		((equal (caar hechoskb) (car exp))(hechosexp exp (cdr hechoskb) (append res (list (car hechoskb)))))
			(t (hechosexp exp (cdr hechoskb) res))
	)
)
;Entrada:
;(hechosexp '(PMissile M1) '((POWNS NONO M1)(PMISSILE M1)(PAMERICAN WEST)(PENEMY NONO AMERICA)(PMissile M2)) '())
;(hechosexp '(PMissile x) '((POWNS NONO M1)(PMISSILE M1)(PAMERICAN WEST)(PENEMY NONO AMERICA)(PMissile M2)) '())
;Salida:
;((PMISSILE M1) (PMISSILE M2))

;=================FUNCION SEVALE==================
;SE USA EN FUNCION: validahecho
;Esta funcion revisa los valores que tiene una expresion con los de un hecho, si se puede cumplir con las reglas del 
;backward entonces sevale devuelve T, sino devuelve NIL
(defun sevale (exp hecho)
	(cond ((null exp) t)
		((AND (equal (buscarelem '(a b c d e f g h i j k l m n o q r s u v w x y z) (car exp)) nil)
			(equal (car exp) (car hecho)))(sevale (cdr exp) (cdr hecho)))
		((AND (equal (buscarelem '(a b c d e f g h i j k l m n o q r s u v w x y z) (car exp)) t)
			(equal (buscarelem '(a b c d e f g h i j k l m n o q r s u v w x y z) (car hecho)) nil))(sevale (cdr exp) (cdr hecho)))
				(t nil)
	)
)
;Entrada:
;(sevale '(PSells West x Nono) '(Psells Platon M1 Nono))
;Salida:
;NIL

;=================FUNCION VALIDAHECHO==================
;SE USA EN FUNCION: estahecho
;Esta funcion dice si la expresion se puede validar con los hechos disponibles
(defun validahecho (exp hechos)
	(cond ((null hechos) nil)
		((equal (sevale (cdr exp) (cdar hechos)) t) t)
			(t (validahecho exp (cdr hechos)))
	)
)
;Entrada:
;(validahecho '(Pmissile x) '((PMISSILE M1) (PMISSILE M2)))
;Salida:
;T

;=================FUNCION ESTAHECHO==================
;SE USA EN FUNCION: backward
;Esta funcion nos devuelve T si hay un hecho que cumpla la expresion que se ingresa
(defun estahecho (expresion hechoskb)
	(cond ((null hechoskb) nil)
		((equal (enchechos expresion hechoskb) t)(validahecho expresion (hechosexp expresion hechoskb '())))
	)
)
;Entrada:
;(estahecho '(Pmissile x) '((POWNS NONO M1) (PMISSILE M1) (PAMERICAN WEST) (PENEMY NONO AMERICA) (PMissile M2)))
;Salida:
;T

;=================FUNCION HAYANT==================
;SE USA EN FUNCION: backward
;Esta funcion mira si hay una expresion en donde la expresion de entrada es el consecuente y exista un antecedente para el
(defun hayant (expresion expresioneskb)
	(cond ((null expresioneskb) nil)
		((equal (caadar expresioneskb) (car expresion)) t)
			(t (hayant expresion (cdr expresioneskb)))
	)
)
;Entrada:
;(hayant '(PWeapon y) '((((PAMERICAN X)(PWEAPON Y)(PSELLS X Y Z)(PHOSTILE Z))(PCRIMINAL X))(((PMISSILE X)(POWNS NONO X))(PSELLS WEST X NONO))(((PMISSILE X))(PWEAPON X))(((PENEMY X AMERICA))(PHOSTILE X))))
;Salida:
; T

;=================FUNCION VARIABLESIMPLE==================
;SE USA EN FUNCION: variablescom, sacarvariables
;Esta funcion saca las variables del consecuente deuna funcion o de un bloque del antecedente
(defun variablesimple (expresion resultado)
	(cond ((null expresion) resultado)
		((equal (buscarelem '(a b c d e f g h i j k l m n o q r s u v w x y z) (car expresion)) t)
			(variablesimple (cdr expresion) (append resultado (list (car expresion)))))
				(t (variablesimple (cdr expresion) resultado))
	)
)
;Entrada:
;(variablesimple '(POwns x y West) '())
;Salida:
; (X Y)

;=================FUNCION VARIABLESCOM==================
;SE USA EN FUNCION: sacarvariables
;Esta funcion saca las variables que hay en el antecedente de una expresion
(defun variablescom (antexp resultado)
	(cond ((null antexp) resultado)
		(t (variablescom (cdr antexp) (append (variablesimple (car antexp) '()) resultado)))
	)
)
;Entrada:
;(variablescom '((PMissile x)(POwns y x)) '())
;Salida:
;(Y X X)

;=================FUNCION SACARVARIABLES==================
;SE USA EN FUNCION: modificacion, hayextra
;consexp es el consecuente de la expresion, antexp es el antecedente de la expresion
;Cuando use esta funcion debo usar algo que separa el antecedente y el consecuente
(defun sacarvariables (antexp consexp)
	(cond ((null antexp) nil)
		(t (append (variablescom antexp '()) (variablesimple consexp '())))
	)
)
;Entrada:
;(sacarvariables '((PMissile x)(POwns y x)) '(PSells West x))
;Salida:
;(Y X X X)

;=================FUNCION ESTAVAR==================
;SE USA EN FUNCION: variablesfinales, hayextra, variablecambio
(defun estavar (variable lista)
	(cond ((null lista) nil)
		((equal variable (car lista)) t)
			(t (estavar variable (cdr lista)))
	)
)
;Entrada:
;(estavar 'x '(Y X X Z))
;Salida:
;T

;=================FUNCION VARIABLESFINALES==================
;SE USA EN FUNCION: hayextra, modificacion
(defun variablesfinales (variables resultado)
	(cond ((null variables) resultado)
		((equal (estavar (car variables) resultado) nil)(variablesfinales (cdr variables) (append (list (car variables)) resultado)))
			(t (variablesfinales (cdr variables) resultado))
	)
)
;Entrada:
;(variablesfinales '(Y X X W) '())
;Salida:
;(W X Y)

;=================FUNCION VARIABLECAMBIO==================
;SE USA EN FUNCION: hayextra, modificacion
;varexpresion son todas las variables que se encuentre en la expresion, varconsecuente son solo las variables que hay en el
;consecuente de esa expresion
(defun variablecambio (varconsecuente varexpresion resultado)
	(cond ((null varexpresion) resultado)
		((equal (estavar (car varexpresion) varconsecuente) t)(variablecambio varconsecuente (cdr varexpresion) resultado))
			(t (variablecambio varconsecuente (cdr varexpresion) (append resultado (list (car varexpresion)) )))
	)
)
;Entrada:
;(variablecambio '(W X) '(W X Y) '())
;Salida:
;(Y)

;=================FUNCION EXPDEEXP==================
;SE USA EN FUNCION: 
;El expresioneskb se usa la funcion de arriba (expresioneskb (kb))
;Esta funcion saca con un exp la expresion completa que lo suple
(defun expdeexp (exp expresioneskb)
	(cond ((null expresioneskb) nil)
		((equal (car exp) (caadar expresioneskb))(car expresioneskb))
			(t (expdeexp exp (cdr expresioneskb)))
	)
)
;Entrada:
;(expdeexp '(PHostile x) '((((PAMERICAN X)(PWEAPON Y)(PSELLS X Y Z)(PHOSTILE Z))(PCRIMINAL X))(((PMISSILE X)(POWNS NONO X))(PSELLS WEST X NONO))(((PMISSILE X))(PWEAPON X))(((PENEMY X AMERICA))(PHOSTILE X))))
;Salida:
;(((PENEMY X AMERICA)) (PHOSTILE X))

;=================FUNCION HAYEXTRA==================
;SE USA EN FUNCION: nuevasexp
;Esta funcion recolecta todas las variables manejadas en una expresion y en el consecuente que se esta usando(exp)
;si hay un problema de variables por inconsistencias, el resultado sera true y tendra que hacer reasignacion de variables
(defun hayextra (exp expdeexp)
	(cond ((null (variablesimple exp '())) nil)
			((estavar (car (variablesimple exp '())) (variablecambio (variablesimple (cadr expdeexp) '()) (variablesfinales (sacarvariables (car expdeexp) (cadr expdeexp)) '()) '())) t)
	)
)
;Entrada:
;(hayextra '(PSells West y) '(((PMissile x)(POwns x y z))(PSells x z)))
;Salida:
;T


;=================FUNCION MODIFICACION==================
;SE USA EN FUNCION: nuevasexp
;si la funcion hayextra da T, entonces hay que hacer una modificacion de la expresion
;vars es variablecambio y nuevas es una lista de todas las variables posibles - las variables de expdeexp - variables exp
;(a b c d e f g h i j k l m n o q r s u v w x y z) - varexpdeexp - varsexp 
(defun modificacion (exp expdeexp)
	(cond ((null exp) nil)
		;(t (cambiarvarextra expdeexp vars nuevas))
		;(t (cambiarvarextra expdeexp (variablecambio (variablesimple (cadr expdeexp) '()) (variablesfinales (sacarvariables (car expdeexp) (cadr expdeexp)) '()) '()) (variablecambio (sacarvariables (car expdeexp) (cadr expdeexp)) (variablecambio (variablesimple exp '()) '(a b c d e f g h i j k l m n o q r s u v w x y z) '()) '())))
		(t (cambiovariables expdeexp (variablecambio (variablesimple (cadr expdeexp) '()) (variablesfinales (sacarvariables (car expdeexp) (cadr expdeexp)) '()) '()) (variablecambio (sacarvariables (car expdeexp) (cadr expdeexp)) (variablecambio (variablesimple exp '()) '(a b c d e f g h i j k l m n o q r s u v w x y z) '()) '())))
	)
)
;Entrada:
;(modificacion '(PSells West y) '(((PMissile x)(POwns x y z))(PSells x z)))
;Salida:
;(((PMISSILE X) (POWNS X A Z)) (PSELLS X Z))

;=================FUNCION CAMBIOVARIABLES2==================
;SE USA EN FUNCION: nuevasexp
;Esta funcion cambia todas las variables de la expresion por los valores nuevos
(defun cambiovariables2 (expresion variables valores)
	(cond ((null variables) expresion)
		((equal (buscarelem '(a b c d e f g h i j k l m n o q r s u v w x y z) (car variables)) t)
			(cambiovariables2 (cambiofinal expresion (car variables) (car valores)) (cdr variables) (cdr valores)))
		(t (cambiovariables2 expresion (cdr variables) (cdr valores)))
		;(t (cambiovariables2 (cambiofinal expresion (car variables) (car valores)) (cdr variables) (cdr valores)))
	)
)
;Entrada:
;(cambiovariables2 '(((PAmerican x)(PWeapon y)(PSells x y z)(PHostile z))(PCriminal x)) '(x) '(West))
;Salida:
;(((PAMERICAN WEST) (PWEAPON Y) (PSELLS WEST Y Z) (PHOSTILE Z)) (PCRIMINAL WEST))

;=================FUNCION CAMBIOVARIABLES==================
;SE USA EN FUNCION: backward
;expdeexp es la expresion completa de el consecuente exp
;Esta funcion lo que hace es que de acuerdo a una expresion y a un bloque de expresion, se haran los cambios de variables
(defun nuevasexp (exp expdeexp)
	(cond ((null exp) nil)
		;((equal (hayextra exp expdeexp) t)(cambiovariables (modificacion exp expdeexp) (cdadr (modificacion exp expdeexp)) (cdr exp)))
		;	(t (cambiovariables expdeexp (cdadr expdeexp) (cdr exp)))
		;Estas documentadas devuelven toda la expresion y no solo los consecuentes
		((equal (hayextra exp expdeexp) t)(car (cambiovariables2 (modificacion exp expdeexp) (cdadr (modificacion exp expdeexp)) (cdr exp))))
			(t (car (cambiovariables2 expdeexp (cdadr expdeexp) (cdr exp))))
	)
)
;Entrada:
;(nuevasexp '(PSells West y) '(((PMissile x)(POwns x y z))(PSells x z)))
;Salida:
;((PMISSILE WEST) (POWNS WEST A Y))

;=================FUNCION EXPRESIONDEEXP==================
;SE USA EN FUNCION: backward
;Esta funcion saca de acuerdo a un consecuente su expresion completa del kb
(defun expresiondeexp (expresion kb)
	(cond ((null kb) nil)
		((equal (caadar kb) (car expresion)) (car kb))
			(t (expresiondeexp expresion (cdr kb)))
	)
)
;Entrada:
;(expresiondeexp '(PHostile x) '((((PAmerican x)(PWeapon y)(PSells x y z)(PHostile z))(PCriminal x))(()(POwns Nono M1))(()(PMissile M1))(((PMissile x)(POwns Nono x))(PSells West x Nono))(((PMissile x))(PWeapon x))(((PEnemy x America))(PHostile x))(()(PAmerican West))(()(PEnemy Nono America))))
;Salida:
;(((PENEMY X AMERICA)) (PHOSTILE X))

;=================FUNCION BACKWARD==================
;Esta es la funcion final de backward
(defun backward (expresion kb)
	(cond ((null expresion) t)
		;((equal (estahecho (car expresion) (hechoskb kb)) t)(backward (cdr expresion) kb))
			;((equal (hayant expresion (expresioneskb kb)) t)(backward (append (nuevasexp (car expresion) (expresioneskb kb)) (cdr expresion)) kb))
			;Revisar que ese append este funcionando
			((equal (estahecho (car expresion) (hechoskb kb)) t)(backward (cdr expresion) kb))
			((equal (hayant (car expresion) (expresioneskb kb)) t)(backward (append (nuevasexp (car expresion) (expresiondeexp (car expresion) kb)) (cdr expresion)) kb))
				(t nil)
	)
)
;Entrada:
;(backward '((PCriminal West)) '((((PAmerican x)(PWeapon y)(PSells x y z)(PHostile z))(PCriminal x))(()(POwns Nono M1))(()(PMissile M1))(((PMissile x)(POwns Nono x))(PSells West x Nono))(((PMissile x))(PWeapon x))(((PEnemy x America))(PHostile x))(()(PAmerican West))(()(PEnemy Nono America))))
;Salida:
;T















