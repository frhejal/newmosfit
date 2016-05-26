module param_hyperfins
!**********************************************************************
!        module PARAMHYPERFINS
!        definition des parametres hyperfins
!     ..................................................................
  use precision
  implicit none
    integer::NB(10)=0 ! indique le type d'ajustement de chaque parametre hyperfin
    ! parametre           DI GA H1 SQ CH ETA TETA GAMA BETA ALFA
    ! indice dans NB(J)    1  2  3  4  5  6    7    8    9    10
    ! exemple :   NB(3)=1  : H1 est ajusté.
    !             NB(6)=0  : ETA est bloqué
    ! Les parametres sont bloqués par defaut (NB=0), sauf TY qui est toujours ajustable
      real(dp)::DI=0.0_dp ! moment isomérique
      real(dp)::GA=0.1_dp ! demi-largeur (commune aux raies du spectre)
      real(dp)::H1=1.0D5  ! intensité totale (nombre de coups)
      real(dp)::SQ=0._dp  ! interaction quadrupolaire
      real(dp)::CH=0._dp  ! champ interne (kOe)
      real(dp)::ETA=0.    ! parametre d'asymétrie
      real(dp)::TETA=0.0_dp !  TETA et GAMA :    angles polaires du chp interne
      real(dp)::GAMA=0.0_dp !    dans les axes principaux du gradient (degres)
      real(dp)::BETA=0.0_dp !  ALFA et BETA : angles polaires de la direction 
      real(dp)::ALFA=0.0_dp ! du rayonnement (meme axes que pour ETA, THETA)
      real(dp)::TY=0.0_dp   ! Niveau moyen hors d'absorption
      integer::MONOC=0      ! 1 : monocristal, 0 : poudre
      ! parametres initiaux dans le cas d'une progression arithmetique du spectre
      integer::NB0(10)=0
      real(dp)::DI0=0.0_dp ! DI du premier sous-spectre de la progression 
      real(dp)::PDI=0.0_dp ! increment de DI
      real(dp)::CH0=0.0_dp ! cf DI0
      real(dp)::PCH=0.0_dp ! increment de CH
      real(dp)::SQ0=0.0_dp ! cf DI0
      real(dp)::PSQ=0.0_dp ! increment de SQ
      real(dp)::TETA0=0.0_dp !cf DI0
      real(dp)::PTETA=0.0_dp ! increment de TETA
  contains
  subroutine razhyp
      DI=0.0_dp
      GA=0.1_dp
      H1=1.D5
      SQ=0.0_dp
      CH=0.0_dp
      ETA=0.0_dp
      TETA=0.0_dp
      GAMA=0.0_dp
      BETA=0.0_dp
      ALFA=0.0_dp
      MONOC=0
      TY=0.0_dp
      NB=0.0_dp
      MONOC=0
  end subroutine razhyp
  subroutine razhyp0
      DI0=0.0_dp
      PDI=0.0_dp
      CH0=0.0_dp
      PCH=0.0_dp
      SQ0=0.0_dp
      PSQ=0.0_dp
      TETA0=0.0_dp
      PTETA=0.0_dp
  end subroutine razhyp0
  subroutine super(nt,ns1)
      integer,intent(in)::nt,ns1
      real(dp):: step
      step = real(nt-ns1,dp)
      DI=DI0+step*PDI
      SQ=SQ0+step*PSQ
      CH=CH0+step*PCH
      TETA=TETA0+step*PTETA
  endsubroutine super
end module param_hyperfins
