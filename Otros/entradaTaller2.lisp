; entrada para el Taller 2
; G. Alvarez, marzo 18 de 2015

(setq kb '(
		   (((PAmerican x)(PWeapon y)(PSells x y z)(PHostile z))(PCriminal x))
           (()(POwns Nono M1))
           (()(PMissile M1))
           (((PMissile x)(POwns Nono x))(PSells West x Nono))
           (((PMissile x))(PWeapon x))
           (((PEnemy x America))(PHostile x))
           (()(PAmerican West))
           (()(PEnemy Nono America))
          )
)
(setq query '(PCriminal West))





		   (((PAmerican x)(PWeapon y)(PSells x y z)(PHostile z))(PCriminal x))
           (((PMissile x)(POwns Nono x))(PSells West x Nono))
           (((PMissile x))(PWeapon x))
           (((PEnemy x America))(PHostile x))


puedo llenar el primero, no entonces reorganizo

           (((PMissile x)(POwns Nono x))(PSells West x Nono))
           (((PMissile x))(PWeapon x))
           (((PEnemy x America))(PHostile x))
           (((PAmerican x)(PWeapon y)(PSells x y z)(PHostile z))(PCriminal x))

si puedo llenar esa enotnces pego PSells a los hechos y hago cdr

           (((PMissile x))(PWeapon x))
           (((PEnemy x America))(PHostile x))
           (((PAmerican x)(PWeapon y)(PSells x y z)(PHostile z))(PCriminal x))

si puedo llenar esa enotnces pego PSells a los hechos y hago cdr 

		   (((PEnemy x America))(PHostile x))
           (((PAmerican x)(PWeapon y)(PSells x y z)(PHostile z))(PCriminal x))


si puedo llenar esa enotnces pego PSells a los hechos y hago cdr 

           (((PAmerican x)(PWeapon y)(PSells x y z)(PHostile z))(PCriminal x))


si puedo llenar esa entonces pego PSells a los hechos y hago cdr, si no pudiera llenarlo reviso siempre si el cdr es diferente a nil porque si es nil
significa que este dato no se puede cumplir entonces no se llega al final
















//-------------------------------------------------------------------------------

'(((PAmerican x)(PWeapon y)(PSells x y z)(PHostile z))(PCriminal x))
'((PAmerican x)(PWeapon y)(PSells x y z)(PHostile z))

tengo (PAmerican West)

'(((PAmerican West)(PWeapon y)(PSells West y z)(PHostile z))(PCriminal West))
'((PWeapon y)(PSells x y z)(PHostile z))

no tengo PWeapon entonces no hago nada


'(((PMissile x)(POwns Nono x))(PSells West x Nono))
'((PMissile x)(POwns Nono x)

tengo PMissile M1

'(((PMissile M1)(POwns Nono M1))(PSells West M1 Nono))
'((POwns Nono x))

tengo POwns Nono M1

'(((PMissile M1)(POwns Nono M1))(PSells West M1 Nono))
'()

como la 2 lista es ya nil entonces guardo a hechos PSells West M1 Nono



//-------------------------------------------------------------------------------



(
 (NIL (POWNS NONO M1)) 
 (NIL (PMISSILE M1)) 
 (NIL (PAMERICAN WEST))
 (NIL (PENEMY NONO AMERICA))
 )



(
	(NIL (PCLARO WIN)) 
	(NIL (POWNS NONO M1)) 
	(NIL (PMISSILE M1))
 	(NIL (PAMERICAN WEST)) 
 	(NIL (PENEMY NONO AMERICA))
)



(
 ((POWNS NONO M1)) 
 ((PMISSILE M1)) 
 ((PAMERICAN WEST))
 ((PENEMY NONO AMERICA))
)


(
	(POWNS NONO M1) 
	(PMISSILE M1) 
	(PAMERICAN WEST) 
	(PENEMY NONO AMERICA)
)






(
	(((PAMERICAN X) (PWEAPON Y) (PSELLS X Y Z) (PHOSTILE Z))(PCRIMINAL X))
    (((PMISSILE X) (POWNS NONO X)) (PSELLS WEST X NONO))
    (((PMISSILE X)) (PWEAPON X))
    (((PENEMY X AMERICA)) (PHOSTILE X))
)


