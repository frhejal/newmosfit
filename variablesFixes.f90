!>@file
!***********************************************************************
!                        MODULE VARIABLESFIXES
!***********************************************************************
!>@brief Déclarations des variables fixées utilisées par le programme
!! principal et par les routines de calcul
!!@version juin 2016
module variablesFixes
  use precision
  ! Variables lues en option
  real(dp),save::CN=0.078125_dp  !< Largeur du canal (mm/s)
  integer,save::NS=1 !< Nombre de sous-spectres théoriques utilisé pour l'ajustement d'un spectre expérimental.
  integer,save::NMAX=0  !<Nombre maximum d'itérations dans l'ajustement en moindres carrés
  integer,save::NS1=0 !< Premier spectre de la distribution
  integer,save::NS2=0 !< Dernier spectre de la distribution
  integer,save::GRASS(10) !< Plages de sous-spectres à sommer (si IO(17)=1)
  integer,save::PLAGEL(2) !<Plage de sous-spectres à lisser
end module variablesFixes
