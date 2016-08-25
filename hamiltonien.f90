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
!                         MODULE HAMILTONIEN
!***********************************************************************
!>@brief Définition du champ interne utilisé et résolution de l'hamiltonien.
!>@details Calcul des énergies et des fonctions d'onde des états fondamentaux et excités.
!>@version juin 2016
module hamiltonien
  use precision
  use options
  use algebre
  implicit none
  real(DP),save::HX    !< Champ hyperfin, composante selon x
  real(DP),save::HY    !< Champ hyperfin, composante selon y
  real(DP),save::HZ    !< Champ hyperfin, composante selon z
  complex(DP),save::hamilF(10) !<Etat fondamental :hamiltonien / valeurs propres (sur la diagonale, apres appel de CEGREN)
  complex(DP),save::hamilE(10) !<Etat excité hamiltonien / valeurs propres (sur la diagonale, apres appel de CEGREN)
  complex(DP),save::fctF(16)!< Fonction d'onde état fondamental
  complex(DP),save::fctE(16)!< Fonction d'onde état excité
  
  contains
  !---------------------------------------------------------------------
  !> @brief Définition du champ hyperfin
  !> @details Les valeurs de Hx, Hy, Hz sont calculées en fonction de l'amplitude CH du champ,
  !! des angles polaires (theta, gamma) dans les axes du gradient, ou de l'angle theta et du paramètre cycloidal Wm
  !! dans le cas d'un cycloide.
  subroutine hamiltonien_definition_champ_hyperfin(ch,theta,gama,wm)
    real(DP),intent(in)::ch !< Intensité du champ interne 
    real(DP),intent(in)::theta !< Angle polaire du champ interne par rapport à l'axe Z
    real(DP),intent(in)::gama !< Angle polaire du champ interne par rapport à l'axe X
    real(DP),intent(in)::wm !< Paramètre cycloidal Hperp/Hz (Hperp = dans le plan OXY)
    real(DP)::sint,cost,sing,cosg,rmh
    sint=sin(theta)  
    cost=cos(theta)  
    sing=sin(gama)
    cosg=cos(gama)
    select case(IO(15))
    case(0)
      HX=ch*sint*cosg
      HY=ch*sint*sing
      HZ=ch*cost
    case(1)
      rmh=(cost**2)+wm*(sint**2)
      HZ=ch*rmh*cost
      HX=ch*rmh*sint
      HY=0.
    case(2)
      stop "valeur de IO(15) inconnue"
    end select
  end subroutine hamiltonien_definition_champ_hyperfin
  !---------------------------------------------------------------------
  !>@brief Calcul des hamiltoniens des états fondametaux et excités, recherche de leurs valeurs propres.
  !>@details Les équations correspondantes proviennent de la thèse de F.Varret (1972),chap 4.
  subroutine hamiltonien_calculer_fonction_onde(ze,zf,sq,eta)
    real(DP),intent(in)::ze !< Rapport gyromagnétique de l'état excité
    real(DP),intent(in)::zf !< Rapport gyromagnétique de l'état fondamental
    real(DP),intent(in)::sq !< Intéraction quadrupolaire
    real(DP),intent(in)::eta !< Paramètre d'asymétrie
    real(DP)::Q
    ! Etats fondamentaux-------------------------------------------------
    hamilF=(0.0_DP,0.0_DP)
    hamilF(1)= cmplx( -0.5_DP*HZ*zf, 0.0_DP      ,dp) ! Etat <1/2|1/2>
    hamilF(2)= cmplx( -0.5_DP*HX*zf, 0.5_DP*HY*zf, dp)! Etat <1/2|-1/2>
    hamilF(3)= cmplx( 0.5_DP*HZ*zf , 0.0_DP      , dp)! Etat <-1/2|-1/2>
    ! Recherche énergies (valeurs propres) et fonctions d'onde (vecteurs propres)
    call algebre_eigenvalues(hamilF,fctF,2,0)
    ! Etats  excités-----------------------------------------------------
    hamilE=(0.0_DP,0.0_DP)
    Q=0.5_DP*sq/sqrt(1.0_DP+eta**2/3.0_DP)
    hamilE(1) = cmplx( -1.5_DP*HZ*ze+Q, 0.0_DP , dp)
    hamilE(2) = cmplx( -0.5_DP*Root3*HX*ze , 0.5_DP*root3*HY*ze, dp )
    hamilE(3) = cmplx( -0.5_DP*HZ*ze-Q, 0.0_DP,dp)
    hamilE(4) = cmplx( eta*root3*Q/3.0_DP, 0.0_DP, dp)
    hamilE(5) = cmplx( -HX*ze, HY*ze, dp)
    hamilE(6) = cmplx( 0.5_DP*HZ*ze-Q, 0.0_DP, dp)
    hamilE(8) = cmplx( eta*root3*Q/3.0_DP, 0.0_DP,dp)
    hamilE(9) = hamilE(2)
    hamilE(10)= cmplx(1.5_DP*HZ*ze+Q, 0.0_DP,dp)
    ! Recherche des énergies et fonctions d'onde par recherche des valeurs
    call algebre_eigenvalues(hamilE,fctE,4,0)
  endsubroutine hamiltonien_calculer_fonction_onde
  !---------------------------------------------------------------------
  !>@brief Calcul des énergies de transition
  !>@details Simples différences entre les niveaux d'énergie fondamentaux et excités
  subroutine hamiltonien_energies(energies)  
    real(DP),intent(out)::energies(8)
    integer::i,ii,j,jj,k
    k=0
    do i=1,2
      do j=1,4
        k=k+1
        ii=(i*i+i)/2
        jj=(j*j+j)/2
        energies(k)=real(hamilE(jj),dp)-real(hamilF(ii),dp)
      enddo
    enddo
  end subroutine hamiltonien_energies
  !---------------------------------------------------------------------
  !> @brief Calcul des intensités des raies
  !> @details On utilise les matrices tMm, tMp et tM0,  
  !> contenant des coefficients de Glebsch Gordon.
  !> @n  cf. thèse de F.Varret (1972),chap 4
  subroutine hamiltonien_intensites(alpha,beta,monoc,intensites)
    integer,intent(in)::monoc !< Option poudre (0) ou monocristal (1)
    real(DP),intent(in)::alpha !< Angle polaire dans la direction du rayonnement (par rapport à l'axe X)
    real(DP),intent(in)::beta  !< Angle polaire dans la direction du rayonnement (par rapport à l'axe Z)
    real(DP),intent(out)::intensites(8) ! Intensité des raies calculées
    integer::i,ii,ik,il,j,jj,jk,jl,k
    real(DP)::cosa,cosb,sina,sinb
    complex(DP)::cu,ap,am,a0
    complex(DP)::tMm(2,4),tM0(2,4),tMp(2,4) 
    cosa=cos(alpha)
    sina=sin(alpha)
    cosb=cos(beta)
    sinb=sin(beta)
    tMm=(0.0_DP)
    tMp=(0.0_DP)
    tM0=(0.0_DP)
    ! Calcul de tMm------------------------------------------------------
    tMm(1,1)=cmplx( 0.5_DP*cosa*(1.0_DP + cosb),&
                  & 0.5_DP*sina*(1.0_DP +cosb), dp )
    tMm(1,2)=cmplx( sinb/Root3, 0.0_DP, dp)
    tMm(1,3)=cmplx( cosa*(1.0_DP -cosb)/(2.0_DP*Root3),&
                  & -sina*(1.0_DP -cosb)/(2.0_DP*Root3), dp) 
    tMm(2,2)=cmplx( cosa*(1.0_DP+cosb)/(2.0_DP*Root3),&
                  & sina*(1.0_DP+cosb)/(2.0_DP*Root3), dp)
    tMm(2,3)=cmplx( sinb/Root3, 0.0_DP, dp)
    tMm(2,4)=cmplx( 0.5_DP*cosa*(1.0_DP-cosb),&
                  & -0.5_DP*sina*(1.0_DP-cosb), dp)
    ! Calcul de tMp------------------------------------------------------
    do j=1,4
      do i=1,2
        ii=3-i
        jj=5-j
        cu=cmplx( (-1.0_DP)**(i+j) ,0.0_DP,dp)
        tMp(i,j)=cu*conjg(tMm(ii,jj))
      enddo
    enddo
    if(monoc==0)then
    ! Calcul de TM0 si on est dans une poudre---------------------------
    ! Remarque : si alpha=0, beta=0, on retrouve l'expression de M0 donnée par la F.Varret (thèse, p48)
      tM0(1,1) = cmplx( -cosa*sinb/Root2, -sina*cosb/Root2, dp )
      tM0(1,2) = cmplx( cosb*root2/root3, 0.0_DP, dp)
      tM0(1,3) = cmplx( cosa*sinb/(root2*root3), -sina*sinb/(root2*root3),dp) 
      do j=1,4
        cu=cmplx((-1.0_DP)**j,0.0_DP,dp)
        jj=5-j
        tM0(2,jj)=cu*conjg(tM0(1,j))
      enddo
    endif
    ! Calcul des intensités---------------------------------------------
    !   I(f->e) = |<F|Mp|E>|**2  + |<F|M0|E>|**2 + |<F|Mm|E>|**2
    k=0
    intensites=0.0_DP
    do i=1,2
    ! Transition du niveau fondamental i...
      do j=1,4
      ! ...vers le niveau excité j
        k=k+1 ! Muméro de raie
        ik=2*(i-1)+1  ! début du  ieme vecteur propre dans fctF
        jk=4*(j-1)+1  ! début du jieme vecteur propre dans fctE
        ap=(0.0_DP,0.0_DP)
        a0=(0.0_DP,0.0_DP)
        am=(0.0_DP,0.0_DP)
        !Multiplication vecteur-matrice-vecteur :        
        do jj=1,4
          do ii=1,2
            il=ik-1+ii  ! ième terme du ième vecteur propre de fctF
            jl=jk-1+jj  ! jième terme du jième vecteur propre de fctE
            ap=ap+ conjg(fctF(il))*tMp(ii,jj)*fctE(jl)
            a0=a0+ conjg(fctF(il))*tM0(ii,jj)*fctE(jl)
            am=am+ conjg(fctF(il))*tMm(ii,jj)*fctE(jl)
          enddo
        enddo
        intensites(k)=intensites(k)+real( ap*conjg(ap)+a0*conjg(a0) + am*conjg(am) )
      enddo
    enddo
    intensites =intensites*8.0_DP/sum(intensites)
  end subroutine hamiltonien_intensites
end module hamiltonien

