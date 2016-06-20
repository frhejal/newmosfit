!>@file
!***********************************************************************
!                         MODULE HABILLAGE
!***********************************************************************
!>@brief Habillage des raies par des lorentziennes/gaussiennes 
!@details          Calcul de la position des raies,
!!         Habillage des raies par des lorentziennes
!!          ou par des convolutions Gauss*Lorentz
!>@version juin 2016
module habillage

  use precision
  use options
  use variablesAjustables
  implicit none
  real(dp)::H(8,40) !< Hauteur des lorentziennes/gaussiennes
  real(dp)::G(8,40) !< Largeur des lorentziennes/gaussiennes
  real(dp)::X0(8,40)!< Emplacement des raies
  
  contains
!---------------------------------------------------------------------
  !>@brief selection de la fonction à appeller selon l'option IO(16) choisie
  subroutine habillage_raies(cn,diso,largeur,hauteur,n,nt,energies,intensites,spectre)
    integer,intent(in)::n !<Nombre de canaux par spectre
    integer,intent(in)::nt !<Numéro du sous-spectre
    real(dp),intent(in)::cn !< Largeur d'un canal
    real(dp),intent(in)::diso ! <Déplacement isomérique
    real(dp),intent(in)::largeur !< Largeur des raies
    real(dp),intent(in)::hauteur !< Hauteur des raies
    real(dp),intent(in)::energies(8)  !< "Energie" (i.e. vitesse effet doppler) theorique des 8 raies du NTieme spectre
    real(dp),intent(in)::intensites(8)!< Intensité théorique des 8 raies du NTième spectre
    real(dp),intent(out)::spectre(n) !< Spectre théorique resultant de l'habillage des raies
    if(IO(16)==0)then 
      call habillage_lorentz(cn,diso,largeur,hauteur,n,nt,energies,intensites,spectre)
    else
      call habillage_convol(cn,diso,largeur,hauteur,n,nt,energies,intensites,spectre)
    endif
  end subroutine habillage_raies
!---------------------------------------------------------------------
!> @brief Habillage par des lorentziennes
  subroutine habillage_lorentz(cn,diso,largeur,hauteur,n,nt,energies,intensites,spectre)
    integer,intent(in)::n !<Nombre de canaux par spectre
    integer,intent(in)::nt !<Numéro du sous-spectre
    real(dp),intent(in)::cn !< Largeur d'un canal
    real(dp),intent(in)::diso ! <Déplacement isomérique
    real(dp),intent(in)::largeur !< Largeur des raies
    real(dp),intent(in)::hauteur !< Hauteur des raies
    real(dp),intent(in)::energies(8)  !< "Energie" (i.e. vitesse effet doppler) theorique des 8 raies du NTieme spectre
    real(dp),intent(in)::intensites(8)!< Intensité théorique des 8 raies du NTième spectre
    real(dp),intent(out)::spectre(n) !< Spectre théorique resultant de l'habillage des raies
    integer::i,l
    real(dp)::b,d0
    spectre=0.0_dp
    d0= 0.5_dp*(real(N,dp) + 1.0_dp)   !milieu du spectre
    do l=1,8
      G(l,nt)=largeur/cn
      H(l,nt)=hauteur*intensites(l)/8.0_dp
      if(  ((IOGVT(nt)==3 ) .AND. (NGT(l,nt) /=0) )&
          &   .OR.      (IOGVT(nt)==1)             &
          &   .OR.      (IOGVT(nt)==2)             &
      & )then
        G(l,nt)=GVT(l,nt)/CN
        H(l,nt)=H(l,nt)*largeur/GVT(l,nt)
      endif
      x0(l,nt)=d0+(energies(l)+diso)/CN
      do i=1,n
        b=real(i,dp) - X0(l,nt)
        spectre(i)=spectre(i)+ H(l,nt)*G(l,nt)**2 / ( b*b +G(l,nt)**2)
      enddo
    enddo
  end subroutine habillage_lorentz
!---------------------------------------------------------------------
  !>@brief Habillage par une convolution Gauss*Lorentz
  subroutine habillage_convol(cn,diso,largeur,hauteur,n,nt,energies,intensites,spectre)
    integer,intent(in)::n !<Nombre de canaux par spectre
    integer,intent(in)::nt !<Numéro du sous-spectre
    real(dp),intent(in)::cn !< Largeur d'un canal
    real(dp),intent(in)::diso ! <Déplacement isomérique
    real(dp),intent(in)::largeur !< Largeur des raies
    real(dp),intent(in)::hauteur !< Hauteur des raies
    real(dp),intent(in)::energies(8)  !< "Energie" (i.e. vitesse effet doppler) theorique des 8 raies du NTieme spectre
    real(dp),intent(in)::intensites(8)!< Intensité théorique des 8 raies du NTième spectre
    real(dp),intent(out)::spectre(n) !< Spectre théorique resultant de l'habillage des raies
    integer::i,j,kp,l,m
    real(dp)::b,cm,d0,u,ugauss,xi,xj
    real(dp),allocatable::gauss(:),lorentz(:)
    spectre=0.0_dp
    kp=IO(16)
    m=n*kp  ! decoupage de chaque canal en kp sous-canaux.
    cm=CN/kp
    allocate(gauss(m),lorentz(m))
    gauss=0.0_dp
    lorentz=0.0_dp
    d0=0.5_dp*(real(m,dp)+1.0_dp)!milieu du spectre
    do l=1,8
      G(l,nt)=largeur/cm
      H(l,nt)=hauteur*intensites(l)/8.0_dp
      if(  ((IOGVT(nt)==3 ) .AND. (NGT(l,nt) /=0) )&
          &   .OR.      (IOGVT(nt)==1)             &
          &   .OR.      (IOGVT(nt)==2)             &
      & )then
        x0(l,nt)=d0+(energies(l)+diso)/cm
      endif
      !gaussienne
      do i=1,m
        xi=real(i,dp)
        b=xi-x0(l,nt)
        u=(b*b)/(2.0_dp*(G(l,nt)**2))
        u=min(u,40.0_dp) ! limitation de la hauteur max de la gaussienne
        ! u=min(u,140.0_dp) ancienne valeur yvan
        ugauss=exp(-u)
        gauss(i)=gauss(i)+H(l,nt)*ugauss
      enddo
    enddo
    !lorentzienne de la gaussienne
    do i=1,m
      xi=real(i,dp)
      do j=1,m
        xj=real(j,dp)
        b=xi-xj
        lorentz(i) = lorentz(i)+gauss(j)*(0.1_dp/cm)**2 /(b*b + ((0.1_dp/cm))**2)
      enddo
    enddo
    j=1
    do i=kp,m,kp
      spectre(j)=lorentz(i)
      j=j+1
      if(abs(spectre(j))<0.2_dp) spectre(j)=0.0_dp ! pourquoi cette valeur ?
    enddo
    deallocate(gauss,lorentz)
  end subroutine habillage_convol
end module habillage
