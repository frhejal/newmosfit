module options
!**********************************************************************
!        module OPTIONS
!        Variables de controle des options
!     ..................................................................
  use precision
  implicit none
  integer::NIN=5, NOUT=6  ! label des fichiers d'entree et de sortie
  integer::IO(20) !liste des options
  integer::IZZ    !indique si il y a des canaux à ignorer
  integer::IZ(10) !plages de canaux a ignorer
  character::titre(256) !en-tete du fichier .coo (date, echantillon, temperature, etc...)
end module options
