!    This file is part of Mosfit2016.
!
!    Mosfit2016 is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    Mosfit2016 is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with this program.  If not, see <http://www.gnu.org/licenses/>.
!>@file
!***********************************************************************
!                         MODULE HABILLAGE
!***********************************************************************
!>@brief Habillage des raies par des lorentziennes/gaussiennes 
!@details          Calcul de la position des raies,
!!         habillage des raies par des lorentziennes
!!          ou par des convolutions Gauss*Lorentz.
!>@version juin 2016
module habillage

  use precision
  use options
  use variablesAjustables
  implicit none
  real(DP),save::H(8,40) !< Hauteur des lorentziennes/gaussiennes
  real(DP),save::G(8,40) !< Largeur des lorentziennes/gaussiennes
  real(DP),save::X0(8,40)!< Emplacement des raies (énergie)
  
  contains
!---------------------------------------------------------------------
  !>@brief sélection de la fonction à appeller selon l'option IO(16) choisie
  subroutine habillage_raies(cn,diso,largeur,hauteur,n,nt,energies,intensites,spectre)
    integer,intent(in)::n !< Nombre de canaux par spectre
    integer,intent(in)::nt !< Numéro du sous-spectre
    real(DP),intent(in)::cn !< Largeur d'un canal
    real(DP),intent(in)::diso !< Déplacement isomérique
    real(DP),intent(in)::largeur !< Largeur des raies
    real(DP),intent(in)::hauteur !< Hauteur des raies
    real(DP),intent(in)::energies(8)  !< "Energie" (i.e. vitesse effet doppler) théorique des 8 raies du NTième spectre
    real(DP),intent(in)::intensites(8)!< Intensité théorique des 8 raies du NTième spectre
    real(DP),intent(out)::spectre(n) !< Spectre théorique résultant de l'habillage des raies
    if(IO(16)==0)then 
      call habillage_lorentz(cn,diso,largeur,hauteur,n,nt,energies,intensites,spectre)
    else
      call habillage_convol(cn,diso,largeur,hauteur,n,nt,energies,intensites,spectre)
    endif
  end subroutine habillage_raies
!---------------------------------------------------------------------
!> @brief Habillage par des lorentziennes
  subroutine habillage_lorentz(cn,diso,largeur,hauteur,n,nt,energies,intensites,spectre)
    integer,intent(in)::n !< Nombre de canaux par spectre
    integer,intent(in)::nt !< Numéro du sous-spectre
    real(DP),intent(in)::cn !< Largeur d'un canal
    real(DP),intent(in)::diso !< Déplacement isomérique
    real(DP),intent(in)::largeur !< Largeur des raies
    real(DP),intent(in)::hauteur !< Hauteur des raies
    real(DP),intent(in)::energies(8)  !< "Energie" (i.e. vitesse effet doppler) théorique des 8 raies du NTième spectre
    real(DP),intent(in)::intensites(8)!< Intensité théorique des 8 raies du NTième spectre
    real(DP),intent(out)::spectre(n) !< Spectre théorique résultant de l'habillage des raies
    integer::i,l
    real(DP)::b,d0
    spectre=0.0_DP
    d0= 0.5_DP*(real(N,dp) + 1.0_DP)   ! Milieu du spectre
    do l=1,8
      G(l,nt)=largeur/cn ! Largeur par défaut ..
      H(l,nt)=hauteur*intensites(l)/8.0_DP
      ! Correction  de la largeur selon l'option IOGV
      if(  ((IOGVT(nt)==3 ) .AND. (NGT(l,nt) /=0) )& ! Si l'ajustement des raies est customisé, on se réfère à NGT (I.E, NG donné par l'utilisateur)
          &   .OR.      (IOGVT(nt)==1)             & ! Si on a 2 groupes (spectre quadrupolaire), toutes les raies ont déjà leur largeur décrite dans GVT (appel précédent à variablesAjustables_actualiser_largeur_raies)
          &   .OR.      (IOGVT(nt)==2)             & ! Idem si 3 groupes (spectre magnétique)
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
    integer,intent(in)::n !< Nombre de canaux par spectre
    integer,intent(in)::nt !< Numéro du sous-spectre
    real(DP),intent(in)::cn !< Largeur d'un canal
    real(DP),intent(in)::diso !< Déplacement isomérique
    real(DP),intent(in)::largeur !< Largeur des raies
    real(DP),intent(in)::hauteur !< Hauteur des raies
    real(DP),intent(in)::energies(8)  !< "Energie" (i.e. vitesse effet doppler) théorique des 8 raies du NTième spectre
    real(DP),intent(in)::intensites(8)!< Intensité théorique des 8 raies du NTième spectre
    real(DP),intent(out)::spectre(n) !< Spectre théorique résultant de l'habillage des raies
    integer::i,j,kp,l,m
    real(DP)::b,cm,d0,u,ugauss,xi,xj
    real(DP),allocatable::gauss(:),lorentz(:)
    spectre=0.0_DP
    kp=IO(16)
    m=n*kp  ! Découpage de chaque canal en kp sous-canaux.
    cm=CN/kp
    allocate(gauss(m),lorentz(m))
    gauss=0.0_DP
    lorentz=0.0_DP
    d0=0.5_DP*(real(m,dp)+1.0_DP)! Milieu du spectre
    do l=1,8
      G(l,nt)=largeur/cm
      H(l,nt)=hauteur*intensites(l)/8.0_DP
      if(  ((IOGVT(nt)==3 ) .AND. (NGT(l,nt) /=0) )&
          &   .OR.      (IOGVT(nt)==1)             &
          &   .OR.      (IOGVT(nt)==2)             &
      & )then
        x0(l,nt)=d0+(energies(l)+diso)/cm
      endif
      ! Gaussienne
      do i=1,m
        xi=real(i,dp)
        b=xi-x0(l,nt)
        u=(b*b)/(2.0_DP*(G(l,nt)**2))
        u=min(u,40.0_DP) ! Limitation de la hauteur max de la gaussienne
        ! u=min(u,140.0_DP) ancienne valeur yvan
        ugauss=exp(-u)
        gauss(i)=gauss(i)+H(l,nt)*ugauss
      enddo
    enddo
    ! Lorentzienne de la gaussienne
    do i=1,m
      xi=real(i,dp)
      do j=1,m
        xj=real(j,dp)
        b=xi-xj
        lorentz(i) = lorentz(i)+gauss(j)*(0.1_DP/cm)**2 /(b*b + ((0.1_DP/cm))**2)
      enddo
    enddo
    j=1
    do i=kp,m,kp
      spectre(j)=lorentz(i)
      j=j+1
      if(abs(spectre(j))<0.2_DP) spectre(j)=0.0_DP ! Pourquoi cette valeur ?
    enddo
    deallocate(gauss,lorentz)
  end subroutine habillage_convol
end module habillage
