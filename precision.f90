module precision
!     ******************************************************************
!                            MODULE PRECISION
!     DEFINI LA DOUBLE PRECISION
!     F.L., mai 2016
!     ******************************************************************
!     dp : entier  correspondant au kind d'un reel double precision 
!          (ex : 1.0D0 ), tel que defini par le compilateur.
!     Pour definir des constantes, preferer l'écriture 42.0_dp à 42.0D0.
!         Cela rend le code plus consitant et permet de changer la
!         precision de toutes les variables en modifiant uniquement dp.
!     Pour declarer une variable X en double precision, ecrire :
!           REAL(dp):: X
!       ou 
!           COMPLEX(dp):: X
!     Pour creer un complexe Z en double precision à partir de sa partie 
!     reelle X et de sa partie imaginaire Y, utiliser :
!          Z=CMPLX(X,Y,dp)
!     REMARQUE :
!     La fonction COMPLEXE(X,Y) ne fait plus partie des 
!     fonction intrinseques en fortran95/fortran2003, meme si quelques
!     compilateurs la procurent encore (ex : gfortran)  )
  implicit none
  integer,parameter:: dp=kind(1.0D0)
end module precision
