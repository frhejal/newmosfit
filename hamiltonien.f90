module hamiltonien
!**********************************************************************
!       module hamiltonien
!       Définition du champ interne utilisé,
!
!       et des parametres ajustables (largeur de raie, hbruit)
!......................................................................
  use precision
  use options
  implicit none
  complex(dp)::HX,HY,HZ    ! Champ hyperfin
  complex(dp)::hamilF(10),hamilE(10) !hamiltoniens / valeurs propres (sur la diagonale, apres appel de CEGREN)
  complex(dp)::fctF(16),fctE(16)   ! fonctions d'onde etat fondamental/excité
  contains
  
  subroutine hamiltonien_definition_champ_hyperfin(ch,sq,ch,teta,gama)
  ! Definition du champ hyperfin
  ! Toute modification du champ interne doit etre codée ici (exemple : dans le cas d'un champ cycloidal)
    real(dp),intent(in)::sq !interaction quadripolaire
    real(dp),intent(in)::ch ! champ interne 
    real(dp),intent(in)::teta !teta,gama angles polaires du champ interne dans les axes du gradient
    real(dp),intent(in)::gama !
    real(dp)::sint,cost,sing,cosg
    sint=sin(teta)  
    cost=cos(teta)  
    sing=sin(gama)
    cosg=cos(gama)
!~     select case(io(...))     ajouter une option pour changer champ interne
    HX=ch*sint*cosg
    HY=ch*sint*sing
    HZ=ch*cost
!~     end select
  end subroutine hamiltonien_definition_champ_hyperfin,
  !---------------------------------------------------------------------
  subroutine hamiltonien_calculer_fonction_onde(ze,zf,sq,eta,gama)
  ! calcul des hamiltoniens des etats fondametaux et excités, recherche de leurs valeurs propres
    integer,intent(in)::ze,zf ! rapports gyromagnétiques de l'element étudié
    real(dp),intent(in)::sq !interaction quadripolaire
    real(dp),intent(in)::eta
    real(dp)::rac,Q
    !Etat fondamental
    HamilF=(0.0_dp,0.0_dp)
    HamilF(1)= complex( -0.5_dp*HZ*zf, 0.0_dp      ,dp) !Etat <1/2|1/2>
    HamilF(2)= complex( -0.5_dp*HX*zf, 0.5_dp*HY*zf, dp)!Etat <1/2|-1/2>
    HamilF(3)= complex( 0.5_dp*HZ*zf , 0.0_dp      , dp)!Etat <-1/2|-1/2>
    !recherche energies (valeurs propres) et fonctions d'onde (vecteurs propres)
    call cegren(hamilF,fctF,2,0)
    !Etats  Excité
    HamilE=(0.0_dp,0.0_dp)
!~     à traduire :
      rac=1.0_dp+eta**2/3.0_dp                                                 
      Q=0.5*sq/SQRT(rac)                                               
      Hamil(1) = complex( -1.5_dp*HZ*ze+Q, 0.0_dp , dp)
!~       RE(1)=-1.5*HZ*ZE+Q                                               
!~       RE(2)=-0.5*R3*HX*ZE
!~       CE(2)=0.5*R3*HY*ZE                                               
!~       RE(3)=-0.5*HZ*ZE-Q                                               
!~       RE(4)=ETA*R3*Q/3.                                                
!~       RE(5)=-HX*ZE                                                     
!~       CE(5)=HY*ZE                                                      
!~       RE(6)=0.5*HZ*ZE-Q                                                
!~       RE(8)=RE(4)                                                      
!~       RE(9)=RE(2)                                                      
!~       CE(9)=CE(2)                                                      
!~       RE(10)=1.5*HZ*ZE+Q      
    call cegren(hamilE,fctE,4,0)
  endsubroutine hamiltonien_calculer_fonction_onde
  !---------------------------------------------------------------------
  subroutine hamiltonien_energie(energie)  
  ! calcul des energies de transition 
  ! simple différence d'energies
  
  end subroutine hamiltonien_energie
  !---------------------------------------------------------------------
  subroutine hamiltonien_intensite(alpha,beta,,intensite)
  ! ATTENTION ANGLES EN DEGRES
  end subroutine hamiltonien_intensite
end module hamiltonien

