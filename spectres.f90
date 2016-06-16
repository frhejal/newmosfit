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
  use variablesFixes
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
    real(dp)::spectre(N)
    real(dp)::spectre_theta(N)
    integer::nt,ntheta
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
      call variablesAjustables_actualiser_rangement(nt)
      if(IOGVT(nt)/=0) call variablesAjustables_actualiser_largeur_raies(nt)
      DI=BT(1,nt)
      GA=BT(2,nt)
      H1=BT(3,nt)
      select case(IO(15))
        case(0)!Cas classique-------------------------------------------
          call spectres_theorique(nt)
          call habillage_raies(CN,DI,GA,H1,N,nt,ENERGIES(:,nt),INTENSITES(:,nt),spectre)
        case(1)!Cycloide------------------------------------------------
          ntheta=50
          do i=0,nTheta-1 !moyenne sur theta
            BT(7,nt) = real(i)*2.0_dp*PI/nTheta
            call spectres_theorique(nt)
            call habillage_raies(CN,DI,GA,H1,N,nt,ENERGIES(:,nt),INTENSITES(:,nt),spectre_theta)
            spectre = spectre + spectre_theta
          enddo
          spectre = spectre/nTheta
        case default! Erreur--------------------------------------------
          stop "valeur de IO(15) inconnue"
      end select
      ! calcul des derivees du spectre theorique------------------------
      call spectres_derivee(nt,spectre)
    enddo
    !-------------------------------------------------------------------
    ! spectre theorique a present contenu dans Q(i,K+2) 
    ! matrice des coefficients de correlation  a present contenue dans Q(1:N,1:K)
    !             Q(i,j) = d(spectre[i])/dB[j]  
    !-------------------------------------------------------------------
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
  subroutine spectres_derivee(nt,spectre)
  ! calcule le spectre et le tableau des dérivées par rapport aux paramètres variables
    integer,intent(in)::nt ! numéro du sous-spectre
    real(dp),intent(out)::spectre(N)
    integer::i,j,jj,l
    real(dp)::diff,di1,gb,pm
    real(dp)::spectre0(N)
    real(dp)::derivee(2,N)
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
    integer,intent(out)::compteur !nombre de sous-spectres
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
  subroutine spectres_absoption_dispersion(k,n,b,spectre_exp,spectre_fit,spectre_bruit,nivzero,hbruit,sExp,sFit,sBruit,daExp,daFit)
    integer,intent(in)::k
    integer,intent(in)::n
    real(dp),intent(in)::b(40)
    real(dp),intent(in)::spectre_exp(n)
    real(dp),intent(in)::spectre_fit(n)
    real(dp),intent(in)::spectre_bruit(n)
    real(dp),intent(in)::nivzero
    real(dp),intent(out)::sExp
    real(dp),intent(out)::sFit
    real(dp),intent(out)::sBruit
    real(dp),intent(out)::daExp
    real(dp),intent(out)::daFit
    real(dp),intent(in)::hbruit
    real(dp)::difExp
    real(dp)::difFit
    integer::i
    ! Surfaces exprimant l'absorption
    sExp=1.0_dp
    sFit=0.0_dp
    sBruit=1.0_dp
    do i=1,n
      sExp=sExp + b(k) - spectre_exp(i)
      sFit=sFit + b(k) - spectre_fit(i)
      if(hbruit/=0.0_dp) sBruit =sBruit-hbruit*spectre_bruit(i) 
    enddo
    ! Surfaces exprimant la dispersion
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
  end subroutine spectres_absoption_dispersion
  !=====================================================================
  subroutine spectres_contributions_distributions(ns,s,sInt)
  !calcul des contributions de chqeu sous-spectre
    integer,intent(in)::ns
    real(dp),intent(out)::s(44) ! distribution (Surface des sous-spectres), avec 2 case vides au debut et à la fin (pour le lissage)
    real(dp),intent(out)::sInt(40)
    integer::nt
    real(dp)::st
    s=0.0_dp
    st=0.0_dp
    do nt=1,ns
      s(nt+2)=abs(BT(2,nt))*BT(3,nt)
      st=st+s(nt+2)
    enddo
    ! Contribution de chaque sous-spectre (en pourcent)
    sInt=0.0_dp
    s=100.0_dp*s/st
    sInt(1)=s(3)
    do nt=2,ns
      sInt(nt)=sInt(nt-1)+s(nt+2)
    enddo
  end subroutine spectres_contributions_distributions
  !=====================================================================
  subroutine spectres_lissage_distribution(ns,s,sl)
  !Lissage des distributions
    integer,intent(in)::ns
    real(dp),intent(in)::s(44) ! distribution non lissée (Surface des sous-spectres), avec 2 case vides au debut et à la fin (pour le lissage)
    real(dp),intent(out)::sl(42) !distribution lissée
    integer::i
      do i=1,ns+2
        sl(i)=0.25_dp*(s(i)+2.0_dp*s(i+1)+s(i+2))
      enddo
  end subroutine spectres_lissage_distribution
  !=====================================================================
  subroutine spectres_moyennes_param_hyperfins(ns,ns2,bt,s,sTotal,nss,btmoy)
  ! Calcul des moyennes des parametres hyperfins sur ns (ou ns2) premiers spectres
    integer,intent(in)::ns
    integer,intent(in)::ns2
    real(dp),intent(in)::s(44)
    real(dp),intent(in)::sTotal
    real(dp),intent(in)::bt(10,40)
    integer,intent(out)::nss
    real(dp),intent(out)::btmoy(7,2) ! bt(:,1) : moyenne arithmetique, bt(:,2) : moyenne quadratique
    integer::i,nt
      nss=ns
      btmoy=0.0_dp
      if(ns2/=0)nss=ns2
      do i=1,7
        select case(i)
          case(1,4,5,7)
            do nt=1,nss
              btmoy(i,1)=btmoy(i,1)+bt(i,nt)*s(nt+2)/sTotal
              btmoy(i,2)=btmoy(i,2)+(bt(i,nt)**2)*s(nt+2)/sTotal
            enddo
        end select
      enddo
  end subroutine spectres_moyennes_param_hyperfins
  !=====================================================================
  subroutine spectre_raz
    POIDS=1.0_dp
  end subroutine spectre_raz
end module spectres
