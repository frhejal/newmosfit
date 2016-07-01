!>@file
!***********************************************************************
!                         MODULE SPECTRE
!***********************************************************************
!>@brief Gestion des données expérimentales, théorique et de bruit, 
!!  et calcul des spectres
!!
!!@version juin 2016
module spectres
  use precision
  use options
  use algebre
  use hamiltonien
  use habillage
  use variablesFixes
  use variablesAjustables
  use connex
  
  implicit none
  integer,parameter::N=256 !< Nombre de mesures par spectre
  real(dp),save::Q(N,42) !< Tableau de travail pour les moindres carrés.
  real(dp),save::Y(N)  !< Spectre expérimental
  real(dp),save::BF(N) !< Spectre de bruit de fond
  real(dp),save::POIDS(N)  !< Poids statistique des canaux, si poids(i)=0 le canal i est ignoré.  
  real(dp),save::SOUS_SPECTRES(N,40) !< Sous-spectres calculés
  real(dp),save::TOTAL_SOUS_SPECTRES(N,5) !< Sommes des groupes de sous-spectres demandés par IO(17)
  real(dp),save::ENERGIES(8,40)  !< Energie des raies de tout les sous-spectres
  real(dp),save::INTENSITES(8,40)  !< Intensité des raies de tout les sous-spectres 

  contains
  !=====================================================================
  !>@brief  Retire le bruit moyen brumoy du spectre de bruit de fond BF
  !>@details Le bruit moyen est la moyenne des 10 premières mesures de BF
  subroutine spectres_preparer_bruit
    real(dp)::brumoy!< Bruit moyen 
    brumoy=0.1_dp*sum(BF(1:10))
    BF=BF-brumoy
  end subroutine spectres_preparer_bruit
  !=====================================================================
  !@brief Met à zéro le poids statistique des canaux à ignorer
  subroutine spectres_poids(iz)
    integer,intent(in)::iz(10)!< Liste des canaux à ignorer
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
  !>@brief Calcule le spectre théorique à partir des paramètres hyperfins
  subroutine spectres_theorique(nt)
    integer,intent(in)::nt!< Numéro du sous-spectre en cours de traitement
    real(dp)::ze ! Rapport gyromagnétique de l'état fondamental
    real(dp)::zf ! Rapport gyromagnétique de l'état excité
    real(dp)::sq,ch,eta,theta,gama,beta,alpha !< paramètres hyperfins
    real(dp)::wm ! Paramètre cycloïdal
    real(dp)::energ(8),intens(8)
    ! Choix du rapport gyromagnétique-----------------------------------
    if(io(3)==0)then
      ! Fe57
      zf = 3.915_dp/330.0_dp 
      ze = -2.236_dp/330.0_dp
    else
      ! Sn119
      zf = -0.08278_dp ! Unités : mm/s/kOe
      ze = 0.0180_dp
    endif
    ! Récuperation des paramètres hyperfins du module variableAjustalbles
    sq=BT(4,nt)
    ch=BT(5,nt)
    eta=BT(6,nt)
    theta=BT(7,nt)*RPD  ! les angles sont donnés en degrés
    if(IO(15)==0)then
      gama=BT(8,nt)*RPD
      wm=0.0_dp
    else
      gama=0.0_dp
      wm=BT(8,nt)
    endif
    beta=BT(9,nt)*RPD
    alpha=BT(10,nt)*RPD 
    ! Calcul du champ hyperfin------------------------------------------
    call hamiltonien_definition_champ_hyperfin(ch,theta,gama,wm)
    ! Calcul des foncitons d'onde et des énéergies----------------------
    call hamiltonien_calculer_fonction_onde(ze,zf,sq,eta)
    ! Calcul des énergies par recherche des valeurs propres
    call hamiltonien_energies(energ)
    ! Calcul d'intensité
    call hamiltonien_intensites(alpha, beta, MONOT(nt) , intens )
    ENERGIES(1:8,nt) = energ(1:8)
    INTENSITES(1:8,nt) = intens(1:8)
  end subroutine spectres_theorique
  !=====================================================================
  !>@brief Calcul du spectre total théorique et de ses dérivées
  !>@details Le spectre est calculé à partir des paramètres hyperfins, via le calcul des
  !> valeurs propres des hamiltoniens de l'état excité et de l'état fondamental
  subroutine spectres_theorique_total
    real(dp)::spectre(N)
    integer::nt,ntheta
    integer::i,j
    ! Remplissage initial du tableau de travail Q-----------------------
    do i=1,N
      Q(i,K+2)=B(K) ! Niveau moyen hors d'absorption
      Q(i,K)=1.0_dp
      do j=1,K-1
        Q(i,j)=0.0_dp
      enddo
    enddo
    ! Calcul du spectre théorique---------------------------------------
    do nt=1,NS
      call variablesAjustables_actualiser_rangement(nt)
      if(IOGVT(nt)/=0) call variablesAjustables_actualiser_largeur_raies(nt)
      DI=BT(1,nt)
      GA=BT(2,nt)
      H1=BT(3,nt)
      select case(IO(15))
        case(0)! Cas classique-------------------------------------------
          call spectres_theorique(nt)
          call habillage_raies(CN,DI,GA,H1,N,nt,ENERGIES(:,nt),INTENSITES(:,nt),spectre)
        case(1)! Cycloide------------------------------------------------
          call spectre_cycloide_theorique_et_habillage(nt,spectre)
        case default! Gestion d'erreur----------------------------------
          stop "valeur de IO(15) inconnue"
      end select
      ! Calcul des derivées du spectre théorique------------------------
      call spectres_derivee(nt,spectre)
    enddo
    !-------------------------------------------------------------------
    ! - Le spectre théorique est à présent contenu dans Q(i,K+2) 
    ! - La matrice des coefficients de corrélation à présent contenue dans Q(1:N,1:K)
    !             Q(i,j) = d(spectre[i])/dB[j]  
    !-------------------------------------------------------------------
    !Spectre de bruit---------------------------------------------------
    if(IO(4)==1)then  ! Ajout du spectre de bruit non ajustable
      do i=1,N
        Q(i,K+2) = Q(i,K+2)+B(K-1)*BF(i)
        Q(i,K-1) = BF(i)
      enddo
    elseif(IO(4)/=0)then ! Ajout du spectre de bruit ajustable
      do i=1,N
        Q(i,K+2)=Q(i,K+2)+ HBRUIT * BF(i)
      enddo
    endif
  end subroutine spectres_theorique_total
  !=====================================================================
  !>@brief Calcul du spectre théorique dans le cas des cycloides
  !>@details Calcule un spectre moyen en effectuant un calcul de spectre théorique 
  !! pour une cinquantaine de valeurs de theta autour du cercle.
  subroutine spectre_cycloide_theorique_et_habillage(nt,spectre)
    integer,intent(in)::nt !< Numéro du sous-spectre en cours de traitement
    real(dp),intent(out)::spectre(N) !< Sous-spectre théorique calculé
    real(dp)::spectre_theta(N)
    integer::i,ntheta
    ntheta=50
    spectre=0.0_dp
    do i=1,nTheta ! Moyenne sur theta
      BT(7,nt) = real(i-1)*360.0/nTheta
      call spectres_theorique(nt)
      call habillage_raies(CN,DI,GA,H1,N,nt,ENERGIES(:,nt),INTENSITES(:,nt),spectre_theta)
      spectre = spectre + spectre_theta
    enddo
    spectre = spectre/nTheta
  end subroutine spectre_cycloide_theorique_et_habillage
  !=====================================================================
  !>@brief Calcule le tableau des dérivées du spectre par rapport aux paramètres variables
  !>@details Le calcul des dérivées se fait en recalculant le spectre théorique avec de 
  !! petites variations du parametre concerné
  subroutine spectres_derivee(nt,spectre)
    integer,intent(in)::nt !< Numéro du sous-spectre
    real(dp),intent(out)::spectre(N)!< Spectre précédemment calculé
    integer::i,j,jj,l
    real(dp)::diff,di1,gb,pm
    real(dp)::spectre0(N)
    real(dp)::derivee(2,N)
      do i=1,N
        Q(i,K+2)=Q(i,K+2)-spectre(i)
      enddo
      spectre0=spectre
      ! Calcul des derivees par rapport aux largeurs variables----------
      ! Les derivées sont estimées par un petit déplacement diff de chaque paramètre
      do j=1,8 ! Dérivées sur les largeurs indépendantes
        if(NGT(j,nt) /=0) then
          l=IADG(j,nt)
          diff=CN*1.0D-3 ! Elément "infiniment" petit = 1/1000e d'un canal
          GVT(j,nt)=B(l)+diff !
          IF(IOGVT(nt)/=0) call variablesAjustables_actualiser_largeur_raies(nt)
          call habillage_raies(CN,DI,GA,H1,N,nt,ENERGIES,INTENSITES,spectre)
          do i=1,N
            Q(i,l)=(spectre0(i)-spectre(i))/diff ! Ecart sur le spectre engendré par l'ecart sur la largeur
          enddo
          GVT(j,nt)=B(l) ! Retour à la valeur initiale de  la largeur
          IF(IOGVT(nt)/=0) call variablesAjustables_actualiser_largeur_raies(nt)
        endif
      enddo
      ! Calcul des dérivees par rapport aux paramétres hyperfins--------
      parametres : do j=1,10
        if( (NBT(j,nt) == 0) .OR. (NBT(j,nt)==3) ) cycle parametres
        l=IAD(j,nt)
        ! Approximation parabolique (moyenne sur deux acroissements opposés)
        do jj = 1,2
          pm=(-1.0_dp)**jj
          select case(j) ! Selon le parametre hyperfin concerné, différentes méthodes de calcul de la dérivée
            case(1) ! Déplacement isomérique
              diff=pm*CN*1.0D-3
              di1=DI+diff
              call habillage_raies(CN,di1,GA,H1,N,nt,ENERGIES(:,nt),INTENSITES(:,nt),spectre)
              derivee(jj,:)=(spectre0-spectre)/diff
            case(2) ! Largeur de raie GA, commune à plusieurs raies si IOGV=1 ( possiblement aussi si IOGV=3, selon choix de l'utilisateur)
              diff=pm*CN*1.0D-3
              gb=GA+diff
              call habillage_raies(CN,DI,gb,H1,N,nt,ENERGIES(:,nt),INTENSITES(:,nt),spectre)
              derivee(jj,:)=(spectre0-spectre)/diff
            case(3)  ! Hauteur de raie
                derivee(jj,:)=-spectre0/H1
            case(4:10) ! Interaction quadrupolaire, champ interne, angles (tous sans unité)
              diff=pm*1.0D-2
              BT(j,nt)=BT(j,nt)+diff
              if(IO(15)==0)then
                call spectres_theorique(nt)
                call habillage_raies(CN,DI,GA,H1,N,nt,ENERGIES(:,nt),INTENSITES(:,nt),spectre)
              else
                call spectre_cycloide_theorique_et_habillage(nt,spectre)
              endif
              BT(j,nt)=BT(j,nt)-diff
              derivee(jj,:)=(spectre0-spectre)/diff
          end select
        enddo
        Q(:,l)=Q(:,l)+0.5_dp*(derivee(1,:)+derivee(2,:))
      enddo parametres
  end subroutine spectres_derivee
  !=====================================================================
  !>@brief Effectue la somme de sous-spectres pour obtenir de nouveaux sous-spectres. 
  !>@details Les groupes de sous-spectres à sommer sont specifiés dans GRASS. 
  !! Jusqu'à 5 groupes de sous-spectres peuvent être sommés.
  !! Les nouveaux sous-spectres obtenus sont placés dans TOTAL_SOUS_SPECTRES.
  !!@n Utilisation de GRASS :  Grass(2*j-1) indique le premier spectre de la jième somme, 
  !! grass(2*j) indique le dernier spectre de la jième somme.
  !!@n Exemple :
  !!@n Pour GRASS = [ 1 3 5 6 7 7 0 0 0 0], on obtient 3 nouveaux sous-spectres.
  !! Le premier est la somme des sous-spectres 1 à 3, le second est la somme des sous-spectres 5 et 6,
  !! le troisième est le spectre 7 seul (de 7 à 7).
  subroutine spectres_total_sous_spectres(grass,compteur)
    integer,intent(in)::grass(10) !< Groupes de sous-spectres à sommer
    integer,intent(out)::compteur !< Nombre de sous-spectres rangés dans TOTAL_SOUS_SPECTRES
    integer::i,nt
      TOTAL_SOUS_SPECTRES=0.0_dp
      compteur=0
      do i=1,10,2
        if(GRASS(i)/=0)then
          compteur=compteur+1
          do nt=GRASS(i),GRASS(i+1) ! Total des sous-spectres de nt=GRASS(i) à nt=GRASS(i+1)
            TOTAL_SOUS_SPECTRES(:,compteur)=TOTAL_SOUS_SPECTRES(:,compteur)+SOUS_SPECTRES(:,nt)
          enddo
          TOTAL_SOUS_SPECTRES(:,compteur)=TY-TOTAL_SOUS_SPECTRES(:,compteur)
        endif
      enddo
  end subroutine spectres_total_sous_spectres
  !=====================================================================
  !> @brief Calcul des absorptions et des dispersions
  !> @details Les absortpions et des dispersions sont calculées par rapport au total des coups enregistrés,
  !! en prenant en compte le niveau sans absorption et le niveau de bruit moyen.
  !> @todo : verifier si il existe une difference entre B(k) et nivzero, et entre B(k+1) et HBRUIT.
  !! Inutile de multiplier les variables sinon. 
  subroutine spectres_absorption_dispersion(k,n,b,spectre_exp,spectre_fit,spectre_bruit,nivzero,hbruit,sExp,sFit,sBruit,daExp,daFit)
    integer,intent(in)::k !< Position de TY dans le vecteur des grandeurs ajustables B
    integer,intent(in)::n !< Nombre de canaux du spectre
    real(dp),intent(in)::b(40) !< Grandeurs ajustables
    real(dp),intent(in)::spectre_exp(n) !< Spectre expérimental
    real(dp),intent(in)::spectre_fit(n) !< Spectre calculé 
    real(dp),intent(in)::spectre_bruit(n) !< Spectre de btuit
    real(dp),intent(in)::nivzero !< Niveau zéro du spectre (nombres de coups hors d'absorption) (équivaut à b(K) ?)
    real(dp),intent(in)::hbruit !< Hauteur de bruit
    real(dp),intent(out)::sExp !< Absoprtion du spectre expérimental
    real(dp),intent(out)::sFit !< Absorption du spectre calculé
    real(dp),intent(out)::sBruit!< Absorption du spectre de bruit
    real(dp),intent(out)::daExp !< Dispersion du spectre expérimental
    real(dp),intent(out)::daFit !< Sispersion du spectre calculé
    real(dp)::difExp
    real(dp)::difFit
    integer::i
    ! Sommes exprimant l'absorption
    sExp=1.0_dp
    sFit=0.0_dp
    sBruit=1.0_dp
    do i=1,n
      sExp=sExp + b(k) - spectre_exp(i)
      sFit=sFit + b(k) - spectre_fit(i)
      if(hbruit/=0.0_dp) sBruit =sBruit-hbruit*spectre_bruit(i) 
    enddo
    ! Sommes exprimant la dispersion
    difFit=0.0_dp
    difExp=1.0_dp
    do i=1,n
      difFit = difFit +(spectre_exp(i)-spectre_fit(i))**2 
    enddo
    do i=3,12
      difExp=difExp+(nivzero-spectre_exp(i))**2
    enddo
    do i=n-9,n
      difExp=difExp+(nivzero-spectre_exp(i))**2
    enddo
    ! Ratio dispersion/absorption
    daFit= n*sqrt(difFit/n)/sFit
    daExp= n*sqrt(difExp/20.0_dp)/sExp
  end subroutine spectres_absorption_dispersion
  !=====================================================================
  !>@brief Calcul des contributions de chaque groupe de sous-spectres par rapport au spectre total.
  !> Les contributions sont calculées sous forme de surfaces (hauteur*largeur)
  subroutine spectres_contributions_distributions(nsmin,nsmax,s,sInt,champ)
    integer,intent(in)::nsmin!< Premier sous-spectre
    integer,intent(in)::nsmax!< Dernier sous-spectre
    real(dp),intent(out)::s(44) !< Contributions des groupes de sous-spectres (Surface des sous-spectres), avec 2 cases vides au début et à la fin (pour le lissage)
    real(dp),intent(out)::sInt(40)!< Cumul des contributions, Sint(nt) est la somme des contributions des nt premiers spectres
    real(dp),intent(out)::champ(44)!< Champ hyperfin des sous-spectres
    integer::nt
    real(dp)::st
    s=0.0_dp
    st=0.0_dp
    champ=0.0_dp
    do nt=nsmin,nsmax
      s(nt+2)=abs(BT(2,nt))*BT(3,nt)
      champ(nt+2)=BT(5,nt)
      st=st+s(nt+2)
    enddo
    champ(nsmin+1)=2.0_dp*champ(2+nsmin)-champ(nsmin+3)
    champ(nsmax+3)=2.0_dp*champ(2+nsmax)-champ(nsmax+1)
    ! Contribution de chaque sous-spectre (en pourcent)
    sInt=0.0_dp
    s=100.0_dp*s/st
    sInt(nsmin)=s(nsmin+2)
    do nt=nsmin+1,nsmax
      sInt(nt)=sInt(nt-1)+s(nt+2)
    enddo
  end subroutine spectres_contributions_distributions
  !=====================================================================
  !<@brief Lissage des distributions
  subroutine spectres_lissage_distribution(nsmin,nsmax,s,sl)
    integer,intent(in)::nsmin!< Premier spectre du lissage
    integer,intent(in)::nsmax!< Dernier spectre du lissage
    real(dp),intent(in)::s(44) !< Distributions non lissées (Surface des sous-spectres), avec 2 case vides au debut et à la fin (pour le lissage)
    real(dp),intent(out)::sl(42) !< Distributions lissées
    integer::i
    do i=nsmin,nsmax+2
      sl(i)=0.25_dp*(s(i)+2.0_dp*s(i+1)+s(i+2))
    enddo
  end subroutine spectres_lissage_distribution
  !=====================================================================
  !<@brief Calcul des moyennes des paramètres hyperfins sur la plage de sous-spectres spécifiée
  !@details On calcule des moyennes arithmétiques et algébriques, pondérées par la contribution des sous-spectres
  subroutine spectres_moyennes_param_hyperfins(nsmin,nsmax,bt,s,sTotal,btmoy)
    integer,intent(in)::nsmin !< Premier sous-spectre de la plage
    integer,intent(in)::nsmax !< Dernier sous-spectre de la plage
    real(dp),intent(in)::s(44)!< Contributions des sous-spectres
    real(dp),intent(in)::sTotal !< Cumul des contributions jusqu'au dernier spectre (donc Stotal =100.0, en toute logique)
    real(dp),intent(in)::bt(10,40)!< Paramètres hyperfins
    real(dp),intent(out)::btmoy(7,2) ! bt(:,1) : moyenne arithmétique, bt(:,2) : moyenne quadratique
    integer::i,nt
      btmoy=0.0_dp
      do i=1,7
        select case(i)
          case(1,4,5,7)
            do nt=nsmin,nsmax
              btmoy(i,1)=btmoy(i,1)+bt(i,nt)*s(nt+2)/sTotal
              btmoy(i,2)=btmoy(i,2)+(bt(i,nt)**2)*s(nt+2)/sTotal
            enddo
        end select
      enddo
  end subroutine spectres_moyennes_param_hyperfins
  !=====================================================================
  !>@brief Réinitialisation
  subroutine spectre_raz
    POIDS=1.0_dp
  end subroutine spectre_raz
end module spectres
