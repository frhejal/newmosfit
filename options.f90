module options
!**********************************************************************
!        module OPTIONS
!        Variables de controle des options
!     ..................................................................
  use precision
  implicit none
  integer::IO(20) ! liste des options
  integer::IOPT   ! indique si des options sont spécifiées.
  integer::IZZ    ! indique si il y a des canaux à ignorer
  integer::IZ(10) ! plages de canaux a ignorer
  character(len=256)::titre !en-tete du fichier .coo (date, echantillon, temperature, etc...)

  contains
  subroutine options_raz
  ! initialisation des variables
    IZ=0
    IO=0
    IOPT=0
    IZZ=0
    titre='Sans Titre'
  end subroutine options_raz
end module options
