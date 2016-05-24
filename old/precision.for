c     ******************************************************************
c                            MODULE PRECISION
c     DEFINITION DE LA DOUBLE PRECISION
c     F.L., mai 2016
c     ******************************************************************
c     dp : entier  correspondant au kind d'un reel double precision 
c          (ex : 1.0D0 ), tel que defini par le compilateur.
c     Pour definir une variable X en double precision, ecrire :
c           REAL(dp):: X
c       ou 
c           COMPLEX(dp):: X
c     Pour creer un complexe Z double precision à partir de sa partie 
c     reelle X et de sa partie imaginaire Y, utiliser :
c           Z=CMPLX(X,Y,dp)
c     (La fonction intrinseque Z=COMPLEXE(X,Y) ne fait plus partie des 
c     fonction intrinseques en fortran95, meme si quelques
c     compilateurs la procurent encore (ex : gfortran)  )
      MODULE PRECISION
        implicit none
        integer,parameter:: dp=kind(1.0D0)
      END 
      
      
      
      
