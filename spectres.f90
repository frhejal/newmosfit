module spectres
!**********************************************************************
!        module SPECTRE
!        tableaux de données experimentales, theorique et de bruit.
!    ..................................................................
  use precision
  use options
  use algebre
  use hamiltonien
  use habillage
  use variablesAjustables
  implicit none
  integer,parameter::N=256 ! nombre de mesures par spectre
  real(dp)::Q(N,42)
  real(dp)::Y(N)
  real(dp)::BF(N)
  real(dp)::P(N)  ! poids statistique des canaux, si poids(i)=0 le canal i est ignoré.  
  real(dp)::GR(N)
  real(dp)::ENERGIES(8,40)
  real(dp)::INTENSITES(8,40)
!~   rel(dp)::
  integer::NS ! nombre de sous-spectres theoriques utilisé pour l'ajsutement d'un spectre experimental.
  
  contains
  !---------------------------------------------------------------------
  subroutine spectres_preparer_bruit
    ! retire le bruit moyen brumoy du spectre de bruit de fond BF
    ! le bruit moyen est la moyenne des 10 premieres mesures de BF
    real(dp)::brumoy
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
  subroutine spectre_theorique(nt)  !(parametres hyper fins ?)
  ! calcule le spectre theorique à partir des parametres hypers fins
    !Choix du rapport gyromagnétique------------------------------------
    integer,intent(in)::nt
    real(dp)::ze,zf
    real(dp)::sq,ch,eta,theta,gama,beta,alpha
    real(dp)::energ(8),intens(8)
    if(io(3)==0)then
      ! Fe57
      zf = 3.915_dp/330.0_dp 
      ze = -2.236_dp/330.0_dp
    else
      ! sn119
      zf = -0.08278_dp ! Unités : mm/s/kOe
      ze = 0.0180_dp
    endif
    !recuperation des parametres hyperfins du module variableAjustalbles
    sq=BT(4,nt)
    ch=BT(5,nt)
    eta=BT(6,nt)
    theta=BT(7,nt)*RPD  ! les angles sont donnes en degres
    gama=BT(8,nt)*RPD
    beta=BT(9,nt)*RPD
    alpha=BT(10,nt)*RPD 
! /!\ ! ajouter ici une boucle sur theta si on ajoute une option cycloide (cf routine DIST modifiée )
    !Calcul du champ hyperfin------------------------------------------- 
    call hamiltonien_definition_champ_hyperfin(ch,teta,gama)
    !Calcul d'energie et intensités-------------------------------------
    call hamiltonien_calculer_fonction_onde(ze,zf,sq,eta)
    !Calcul des energies par recherche des valeurs propres
    call hamiltonien_energies(energ)
    call hamiltonien_intensites(alpha, beta, MONOT(nt) , intens )
    ENERGIES(1:8,nt) = energ(1:8)
    INTENSITES(1:8,nt) = intens(1:8)
  end subroutine spectre_theorique
  !---------------------------------------------------------------------
  subroutine spectres_calculer
  ! 
    integer::nt
    do nt=1,NS
      call spectre_deriver(nt,0)
    enddo
  end subroutine spectres_calculer
  !---------------------------------------------------------------------
  subroutine spectre_deriver(phi,nt,m)
  ! calcule le spectre et le tableau des dérivées par rapport aux parametres variables
    integer,intent(in)::nt ! numero du sous-spectre
    integer,intent(in)::m
    real(dp),intent(in)::phi
    integer::i,l
    do i=1,N
      if(NBT(i,nt)/=0)then
        l=IAD(i,nt)
        BT(i,nt)=B(l)
        
      endif
    enddo
    
    call spectre_theorique(nt)
  end subroutine spectre_deriver
end module spectres
