module hamiltonien
!**********************************************************************
!       module hamiltonien
!       Définition du champ interne utilisé,
!
!       et des parametres ajustables (largeur de raie, hbruit)
!......................................................................
  use precision
  use options
  use algebre
  implicit none
  real(dp)::HX,HY,HZ    ! Champ hyperfin
  complex(dp)::hamilF(10),hamilE(10) !hamiltoniens / valeurs propres (sur la diagonale, apres appel de CEGREN)
  complex(dp)::fctF(16),fctE(16)   ! fonctions d'onde etat fondamental/excité
  
  contains
  
  subroutine hamiltonien_definition_champ_hyperfin(ch,theta,gama)
  ! Definition du champ hyperfin
  ! Toute modification du champ interne doit etre codée ici (exemple : dans le cas d'un champ cycloidal)
    real(dp),intent(in)::ch ! champ interne 
    real(dp),intent(in)::theta,gama !teta,gama angles polaires du champ interne dans les axes du gradient
    real(dp)::sint,cost,sing,cosg
    sint=sin(theta)  
    cost=cos(theta)  
    sing=sin(gama)
    cosg=cos(gama)
!~     select case(io(...))     ajouter une option pour changer champ interne
    HX=ch*sint*cosg
    HY=ch*sint*sing
    HZ=ch*cost
!~     end select
  end subroutine hamiltonien_definition_champ_hyperfin
  !---------------------------------------------------------------------
  subroutine hamiltonien_calculer_fonction_onde(ze,zf,sq,eta)
  ! calcul des hamiltoniens des etats fondametaux et excités, recherche de leurs valeurs propres
    real(dp),intent(in)::ze,zf ! rapports gyromagnétiques de l'element étudié
    real(dp),intent(in)::sq !interaction quadripolaire
    real(dp),intent(in)::eta !
    real(dp)::Q
    !Etats fondamentaux-------------------------------------------------
    hamilF=(0.0_dp,0.0_dp)
    hamilF(1)= cmplx( -0.5_dp*HZ*zf, 0.0_dp      ,dp) !Etat <1/2|1/2>
    hamilF(2)= cmplx( -0.5_dp*HX*zf, 0.5_dp*HY*zf, dp)!Etat <1/2|-1/2>
    hamilF(3)= cmplx( 0.5_dp*HZ*zf , 0.0_dp      , dp)!Etat <-1/2|-1/2>
    !recherche energies (valeurs propres) et fonctions d'onde (vecteurs propres)
    call algebre_eigenvalues(hamilF,fctF,2,0)
    !Etats  Excités-----------------------------------------------------
    hamilE=(0.0_dp,0.0_dp)
    Q=0.5_dp*sq/sqrt(1.0_dp+eta**2/3.0_dp)
    hamilE(1) = cmplx( -1.5_dp*HZ*ze+Q, 0.0_dp , dp)
    hamilE(2) = cmplx( -0.5_dp*Root3*HX*ze , 0.5_dp*root3*HY*ze, dp )
    hamilE(3) = cmplx( -0.5_dp*HZ*ze-Q, 0.0_dp,dp)
    hamilE(4) = cmplx( eta*root3*Q/3.0_dp, 0.0_dp, dp)
    hamilE(5) = cmplx( -HX*ze, HY*ze, dp)
    hamilE(6) = cmplx( 0.5_dp*HZ*ze-Q, 0.0_dp, dp)
    hamilE(8) = cmplx( eta*root3*Q/3.0_dp, 0.0_dp,dp)
    hamilE(9) = hamilE(2)
    hamilE(10)= cmplx(1.5_dp*HZ*ze+Q, 0.0_dp,dp)
    !recherche des energies et fonctions d'onde par recherche des valeurs
    call algebre_eigenvalues(hamilE,fctE,4,0)
  endsubroutine hamiltonien_calculer_fonction_onde
  !---------------------------------------------------------------------
  subroutine hamiltonien_energies(energies)  
  ! calcul des energies de transition
  ! simple différence d'energies
    real(dp),intent(out)::energies(8)
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
  subroutine hamiltonien_intensites(alpha,beta,monoc,intensites)
  ! calcul des intensités, utilisant les matrices de tMm tMp et tM0,  
  ! contenant des coefficients de Glebsch Gordon
    integer,intent(in)::monoc ! option poudre (0) ou monocristal (1)
    real(dp),intent(in)::alpha,beta !angles polaires de la direction du rayonnement
    real(dp),intent(out)::intensites(8)
    integer::i,ii,ik,il,j,jj,jk,jl,k
    real(dp)::cosa,cosb,sina,sinb
    complex(dp)::cu,ap,am,a0
    complex(dp)::tMm(2,4),tM0(2,4),tMp(2,4) 
    cosa=cos(alpha)
    sina=sin(alpha)
    cosb=cos(beta)
    sinb=sin(beta)
    tMm=(0.0_dp)
    tMp=(0.0_dp)
    tM0=(0.0_dp)
    !Calcul de tMm------------------------------------------------------
    tMm(1,1)=cmplx( 0.5_dp*cosa*(1.0_dp + cosb),&
                  & 0.5_dp*sina*(1.0_dp +cosb), dp )
    tMm(1,2)=cmplx( sinb/Root3, 0.0_dp, dp)
    tMm(1,3)=cmplx( cosa*(1.0_dp -cosb)/(2.0_dp*Root3),&
                  & -sina*(1.0_dp -cosb)/(2.0_dp*Root3), dp) 
    tMm(2,2)=cmplx( cosa*(1.0_dp+cosb)/(2.0_dp*Root3),&
                  & sina*(1.0_dp+cosb)/(2.0_dp*Root3), dp)
    tMm(2,3)=cmplx( sinb/Root3, 0.0_dp, dp)
    tMm(2,4)=cmplx( 0.5_dp*cosa*(1.0_dp-cosb),&
                  & -0.5_dp*sina*(1.0_dp-cosb), dp)
    !calcul de tMp------------------------------------------------------
    do j=1,4
      do i=1,2
        ii=3-i
        jj=5-j
        cu=cmplx( (-1.0_dp)**(i+j) ,0.0_dp,dp)
        tMp(i,j)=cu*conjg(tMm(ii,jj))
      enddo
    enddo
    if(monoc==0)then
    ! Calcul de TM0 si on est dans une poudre---------------------------
    ! remarque : si alpha=0, beta=0, on retrouve l'expression de M0 donnée par la F.Varret (thèse, p48)
      tM0(1,1) = cmplx( -cosa*sinb/Root2, -sina*cosb/Root2, dp )
      tM0(1,2) = cmplx( cosb*root2/root3, 0.0_dp, dp)
      tM0(1,3) = cmplx( cosa*sinb/(root2*root3), -sina*sinb/(root2*root3),dp) 
      do j=1,4
        cu=cmplx((-1.0_dp)**j,0.0_dp,dp)
        jj=5-j
        tM0(2,jj)=cu*conjg(tM0(1,j))
      enddo
    endif
    ! Calcul des intensités---------------------------------------------
    !   I(f->e) = |<F|Mp|E>|**2  + |<F|M0|E>|**2 + |<F|Mm|E>|**2
    k=0
    intensites=0.0_dp
    do i=1,2
    ! Transition du niveau fondamental i...
      do j=1,4
      ! ...vers le niveau excité j
        k=k+1 ! numero de raie
        ik=2*(i-1)+1  ! debut du  ieme vecteur propre dans fctF
        jk=4*(j-1)+1  ! debut du jieme vecteur propre dans fctE
        ap=(0.0_dp,0.0_dp)
        a0=(0.0_dp,0.0_dp)
        am=(0.0_dp,0.0_dp)
        !Multiplication vecteur-matrice-vecteur :        
        do jj=1,4
          do ii=1,2
            il=ik-1+ii  ! ieme terme du iieme vecteur propre de fctF
            jl=jk-1+jj  ! jieme terme du jieme vecteur propre de fctE
            ap=ap+ conjg(fctF(il))*tMp(ii,jj)*fctE(jl)
            a0=a0+ conjg(fctF(il))*tM0(ii,jj)*fctE(jl)
            am=am+ conjg(fctF(il))*tMm(ii,jj)*fctE(jl)
          enddo
        enddo
        intensites(k)=intensites(k)+real( ap*conjg(ap)+a0*conjg(a0) + am*conjg(am) )
      enddo
    enddo
    intensites =intensites*8.0_dp/sum(intensites)
  end subroutine hamiltonien_intensites
end module hamiltonien

