module variablesFixes
!**********************************************************************
!       module variablesFixess
!       Décalarations des variables fixes utilisée par le programme
!       principal et par les routines de calcul (spectres, ajustement, 
!......................................................................
  use precision
  !variables lues en option
  real(dp)::CN=0.078125_dp  ! largeur du canal (mm/s)
  integer::NS=1 ! nombre de sous-spectres theoriques utilisé pour l'ajustement d'un spectre experimental.
  integer::NMAX=0   !
  integer::NS1=0,NS2=0! On effectue une distribution de spectres entre NS1 et NS2
  integer::GRASS(10) !plages de sous-spectres à sommer (si IO(17)=1)
end module variablesFixes
