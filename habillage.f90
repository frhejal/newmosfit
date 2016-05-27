module habillage
!**********************************************************************
!            Calcul de la position des raies
!         Habillage des raies par des lorentziennes
!          ou par des convolutions Gauss*Lorentz
!......................................................................
  use precision
  use options
  use variablesAjustables
  implicit none
  real(dp)::H(8,40) ! hauteur des lorentziennes
  real(dp)::G(8,40) ! largeur des lorentziennes
  real(dp)::X0(8,40)! emplacement des lorentziennes 
  
  contains
!---------------------------------------------------------------------
  subroutine habillage_raies(diso,largeur,hauteur,n,nt,energies,intensites,spectre)
    integer,intent(in)::n,nt
    real(dp),intent(in)::diso,largeur,hauteur
    real(dp),intent(in)::energies(8)  ! "energie" (i.e. vitesse effet doppler) theorique des 8 raies du NTieme spectre
    real(dp),intent(in)::intensites(8)! intensité theorique des 8 raies du NTieme spectre
    real(dp),intent(out)::spectre(n)
    if(IO(16)==0)then 
      call habillage_lorentz(diso,largeur,hauteur,n,nt,energies,intensites,spectre)
    else
      call habillage_convol(diso,largeur,hauteur,n,nt,energies,intensites,spectre)
    endif
  end subroutine habillage_raies
!---------------------------------------------------------------------
  subroutine habillage_lorentz(diso,largeur,hauteur,n,nt,energies,intensites,spectre)
  ! Habillage par une Lorentzienne
    integer,intent(in)::n,nt
    real(dp),intent(in)::diso,largeur,hauteur
    real(dp),intent(in)::energies(8)
    real(dp),intent(in)::intensites(8)
    real(dp),intent(out)::spectre(n)
    integer::i,l
    real(dp)::d0,b
    spectre=0.0_dp
    d0= 0.5_dp*(real(N,dp) + 1.0_dp)   !milieu du spectre
    do l=1,8
      G(l,nt)=largeur/CN
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
  subroutine habillage_convol(diso,largeur,hauteur,n,nt,energies,intensites,spectre)
  ! Habillage par une convolution Gauss*Lorentz
    integer,intent(in)::n,nt
    real(dp),intent(in)::diso,largeur,hauteur
    real(dp),intent(in)::energies(8)
    real(dp),intent(in)::intensites(8)
    real(dp),intent(out)::spectre(n)    
  
  end subroutine habillage_convol
end module habillage
