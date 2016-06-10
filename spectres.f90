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
  use connex
  implicit none
  integer,parameter::N=256 ! nombre de mesures par spectre
  real(dp)::Q(N,42) ! tableau de travail pour les moindres carres.
  real(dp)::Y(N)  ! Spectre experimental
  real(dp)::BF(N) ! Spectre de bruit de fond
  real(dp)::POIDS(N)  ! Poids statistique des canaux, si poids(i)=0 le canal i est ignoré.  
  real(dp)::SOUS_SPECTRES(N,40) ! sous spectres calculés
  real(dp)::TOTAL_SOUS_SPECTRES(N,5) ! sommes des groupes de sous-spectres demandés par IO(17)
  real(dp)::ENERGIES(8,40)
  real(dp)::INTENSITES(8,40)
  !variables lues en option
  real(dp)::CN    ! largeur du canal (mm/s)
  integer::NS ! nombre de sous-spectres theoriques utilisé pour l'ajustement d'un spectre experimental.
  integer::NMAX   !
  integer::NS1,NS2! On effectue une distribution de spectres entre NS1 et NS2
  integer::GRASS(10) !plages de sous-spectres à sommer (si IO(17)=1)
  contains
  !=====================================================================
  subroutine spectres_preparer_bruit
    ! retire le bruit moyen brumoy du spectre de bruit de fond BF
    ! le bruit moyen est la moyenne des 10 premieres mesures de BF
    real(dp)::brumoy
    brumoy=0.1_dp*sum(BF(1:10))
    BF=BF-brumoy
  end subroutine spectres_preparer_bruit
  !=====================================================================
  subroutine spectres_poids(iz)
  ! met à zero le poids statistique des canaux à ignorer
    integer,intent(in)::iz(10)
    integer::i,kz
    canaux : do i=2,N
      do kz=1,9,2
        if((i>= IZ(kz)) .AND. (i<=IZ(kz+1))) then
          POIDS(i)=0.0_dp
          cycle canaux
        endif
      enddo
    enddo canaux
  end subroutine spectres_poids
  !=====================================================================
  subroutine spectres_theorique(nt)
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
  end subroutine spectres_theorique
  !=====================================================================
  subroutine spectres_theorique_total
    ! calcul du spectre total et de ses derivees par rapport aux parametres variables,
    ! par appel répété de spectres_derivee (qui appelle spectres_theorique et l'habille avec des gaussiennes)
    integer::nt
    integer::i,j
!~     Q=0.0_dp
    ! remplissage initial du tableau de travail Q-----------------------
    do i=1,N
      Q(i,K+2)=B(K) ! niveau moyen hors d'absorption
      Q(i,K)=1.0_dp
      do j=1,K-1
        Q(i,j)=0.0_dp
      enddo
    enddo
    ! calcul du spectre theorique---------------------------------------
    do nt=1,NS
!~       call variablesAjustables_calculer_ecart_type(phi,nt,n) 
      call variablesAjustables_actualiser_rangement(nt)
      if(IOGVT(nt)/=0) call variablesAjustables_actualiser_largeur_raies(nt)
      call spectres_derivee(nt)
    enddo
    ! spectre theorique a present contenu dans Q(i,K+2) 
    ! matrice des coefficients de correlation  a present contenue dans Q(1:N,1:K)
    !             Q(i,j) = d(spectre[i])/dB[j]  
    !Spectre de bruit---------------------------------------------------
    if(IO(4)==1)then  ! ajout du spectre de bruit non ajustable
      do i=1,N
        Q(i,K+2) = Q(i,K+2)+B(K-1)*BF(i)
        Q(i,K-1) = BF(i)
      enddo
    elseif(IO(4)/=0)then ! ajout du spectre de bruit ajustable
      do i=1,N
        Q(i,K+2)=Q(i,K+2)+ HBRUIT * BF(i)
      enddo
    endif
  end subroutine spectres_theorique_total
  !=====================================================================
  subroutine spectres_derivee(nt)
  ! calcule le spectre et le tableau des dérivées par rapport aux paramètres variables
    integer,intent(in)::nt ! numéro du sous-spectre
    integer::i,j,jj,l
    real(dp)::diff,di1,gb,pm
    real(dp)::spectre(N),spectre0(N)
    real(dp)::derivee(2,N)
!~     real(dp),intent(in)::phi
      DI=BT(1,NT)
      GA=BT(2,NT)
      H1=BT(3,NT)
      spectre=0.0_dp
      !calcul de la fonction -------------------------------------------
      call spectres_theorique(nt)
      call habillage_raies(CN,DI,GA,H1,N,nt,ENERGIES(:,nt),INTENSITES(:,nt),spectre)
      do i=1,N
        Q(i,K+2)=Q(i,K+2)-spectre(i)
      enddo
      spectre0=spectre
      !calcul des derivees par rapport aux largeurs variables-----------  
      ! les derivees sont estimees par un petit deplacement diff de chaque parametre
      do j=1,8
        if(NGT(j,nt) /=0) then
          l=IADG(j,nt)
          diff=CN*1.0D-3 !element infiniment petit = 1/1000e d'un canal
          GVT(j,nt)=B(l)+diff !
          IF(IOGVT(nt)/=0) call variablesAjustables_actualiser_largeur_raies(nt)
          call habillage_raies(CN,DI,GA,H1,N,nt,ENERGIES,INTENSITES,spectre)
          do i=1,N
            Q(i,l)=(spectre0(i)-spectre(i))/diff ! ecart sur le spectre engendre par l'ecart sur la largeur
          enddo
          GVT(j,nt)=B(l) !retour à la valeur initiale de  la largeur
          IF(IOGVT(nt)/=0) call variablesAjustables_actualiser_largeur_raies(nt)
        endif
      enddo
      ! calcul des derivees par rapport aux parametres hyperfins--------
      parametres : do j=1,10
        if( (NBT(j,nt) == 0) .OR. (NBT(j,nt)==3) ) cycle parametres
        l=IAD(j,nt)
        ! Approximation parabolique (moyenne sur deux acroissements opposes)
        do jj = 1,2
          pm=(-1.0_dp)**jj
          select case(j) ! selon le parametre hyperfin concerné, differentes méthodes de calcul de la dérivée
            case(1) !deplacement isomerique
              diff=pm*CN*1.0D-3
              di1=DI+diff
              call habillage_raies(CN,di1,GA,H1,N,nt,ENERGIES(:,nt),INTENSITES(:,nt),spectre)
              derivee(jj,:)=(spectre0-spectre)/diff
            case(2) ! largeur de raie
              diff=pm*CN*1.0D-3
              gb=GA+diff
              call habillage_raies(CN,DI,gb,H1,N,nt,ENERGIES(:,nt),INTENSITES(:,nt),spectre)
              derivee(jj,:)=(spectre0-spectre)/diff
            case(3)  ! hauteur de raie
                derivee(jj,:)=-spectre0/H1
            case(4:10) ! interaction quadrupolaire, champ interne, angles (tous sans unité)
              diff=pm*1.0D-2
              BT(j,nt)=BT(j,nt)+diff
              call spectres_theorique(nt)
              BT(j,nt)=BT(j,nt)-diff
              call habillage_raies(CN,DI,GA,H1,N,nt,ENERGIES(:,nt),INTENSITES(:,nt),spectre)
              derivee(jj,:)=(spectre0-spectre)/diff
          end select
        enddo
        Q(:,l)=Q(:,l)+0.5_dp*(derivee(1,:)+derivee(2,:))
      enddo parametres
  end subroutine spectres_derivee
  !=====================================================================
  subroutine spectres_total_sous_spectres(grass,compteur)
    integer,intent(in)::grass(10) !groupes de sous-spectres à sommer
    integer,intent(out)::compteur
    integer::i,nt
      TOTAL_SOUS_SPECTRES=0.0_dp
      compteur=0
      do i=1,10,2
        if(GRASS(i)/=0)then
          compteur=compteur+1
          do nt=GRASS(i),GRASS(i+1) ! Total des sous-spectres de nt=GRASS(i) à nt=GRASS(i+1)
            TOTAL_SOUS_SPECTRES(:,compteur)=TOTAL_SOUS_SPECTRES(:,compteur)+SOUS_SPECTRES(:,nt)
          enddo
          TOTAL_SOUS_SPECTRES=TY-TOTAL_SOUS_SPECTRES
        endif
      enddo
  end subroutine spectres_total_sous_spectres
  !=====================================================================
  subroutine spectre_raz
    CN=0.078125_dp
    NS=1
    NS1=0
    NS2=0
    NMAX=0
    POIDS=1.0_dp
  end subroutine spectre_raz
end module spectres
