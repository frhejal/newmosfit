!     ******************************************************************
!                            MODULE PRECISION
!     DEFINITION DE LA DOUBLE PRECISION
!     F.L., mai 2016
!     ******************************************************************
!     dp : entier  correspondant au kind d'un reel double precision 
!          (ex : 1.0D0 ), tel que defini par le compilateur.
!     Pour definir une variable X en double precision, ecrire :
!           REAL(dp):: X
!       ou 
!           COMPLEX(dp):: X
!     Pour creer un complexe Z double precision à partir de sa partie 
!     reelle X et de sa partie imaginaire Y, utiliser :
!          Z=CMPLX(X,Y,dp)
!     (La fonction intrinseque Z=COMPLEXE(X,Y) ne fait plus partie des 
!     fonction intrinseques en fortran95, meme si quelques
!     compilateurs la procurent encore (ex : gfortran)  )
      MODULE PRECISION
		IMPLICIT NONE
		INTEGER,PARAMETER:: dp=KIND(1.0D0)
      END 
