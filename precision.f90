!***********************************************************************
!                            MODULE PRECISION
!***********************************************************************
!>@brief Définition de la double precision et de quelques constantes mathématiques
!!@details  @b dp : entier  correspondant au kind d'un reel double precision 
!!          (ex : 1.0D0 ), tel que defini par le compilateur.
!!@n     Pour definir des constantes, preferer l'écriture 42.0_dp à 42.0D0.
!!         Cela rend le code plus consitant et permet de changer la
!!         precision de toutes les variables en modifiant uniquement dp.
!!@n     Pour declarer une variable X en double precision, ecrire :
!!@n@b            REAL(dp)::X
!!       ou 
!!@b           COMPLEX(dp)::X
!!@n     Pour creer un complexe Z en double precision à partir de sa partie 
!!       reelle X et de sa partie imaginaire Y, utiliser :
!!@n@b          Z=CMPLX(X,Y,dp)
!!@n\b     REMARQUE :
!!     La fonction COMPLEXE(X,Y) ne fait plus partie des 
!!     fonction intrinseques en fortran95/fortran2003, meme si quelques
!!     compilateurs la procurent encore (ex : gfortran)  )
module precision
  implicit none
  integer,parameter:: dp=kind(1.0D0) !< double precision
  real(dp),parameter:: PI=4.*atan(1.0_dp) !<  Rapport entre le perimètre d'un cercle et son diamètre
  real(dp),parameter:: RPD=PI/180.0_dp    !< Conversion de degrés en radians
  real(dp),parameter:: ROOT2 = sqrt(2.0_dp)!< racine de 2
  real(dp),parameter:: ROOT3 = sqrt(3.0_dp)!<racine de 3
end module precision
