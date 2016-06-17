!>@file
!***********************************************************************
!                        MODULE VARIABLESFIXES
!***********************************************************************
!>@brief Déclarations des variables fixées utilisées par le programme
!! principal et par les routines de calcul
!!@version juin 2016
module variablesFixes
  use precision
  !variables lues en option
  real(dp)::CN=0.078125_dp  !< Largeur du canal (mm/s)
  integer::NS=1 !< Nombre de sous-spectres theoriques utilisé pour l'ajustement d'un spectre experimental.
  integer::NMAX=0  !<Nombre maximum d'itérations dans l'ajustement en moindres carrés
  integer::NS1=0 !< Premier spectre de la distribution
  integer::NS2=0 !< Dernier spectre de la distribution
  integer::GRASS(10) !< Plages de sous-spectres à sommer (si IO(17)=1)
end module variablesFixes
