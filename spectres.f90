module spectres
!**********************************************************************
!        module SPECTRE
!        tableaux de données experimentales, theorique et de bruit.
!    ..................................................................
  use precision
  use options
  implicit none
  integer,parameter::N=256 ! nombre de mesures par spectre
  real(dp)::Q(N,42)
  real(dp)::Y(N)
  real(dp)::BF(N)
  real(dp)::P(N)  ! poids statistique des canaux, si poids(i)=0 le canal i est ignoré.  
  integer::NS ! nombre de sous-spectres theoriques utilisé pour l'ajsutement d'un spectre experimental.
  contains
  !---------------------------------------------------------------------
  subroutine spectres_preparer_bruit
    ! retire le bruit moyen brumoy du spectre de bruit de fond BF
    ! le bruit moyen est la moyenne des 10 premieres mesures de BF
    real(dp)::brumoy
    integer::j
    brumoy=0.1_dp*sum(BF(1:10))
    BF=BF-brumoy
  end subroutine spectres_preparer_bruit
  !---------------------------------------------------------------------
  subroutine spectres_poids(iz)
  ! met à zero le poids statistique des canaux à ignorer
    integer,intent(in)::iz(10)
    integer::i,k
    canaux : do i=2,N
      do k=1,9,2
        if((i>= IZ(k)) .AND. (i<=IZ(k+1))) then
          P(i)=0.0_dp
          cycle canaux
        endif
      enddo
    enddo canaux
  end subroutine spectres_poids
  !---------------------------------------------------------------------
  subroutine spectres_calculer
    
  end subroutine spectres_calculer
end module spectres
