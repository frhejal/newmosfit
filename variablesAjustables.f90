!>@file
!***********************************************************************
!                        MODULE VARIABLESAJUTABLES
!***********************************************************************
!>@brief Rangements et manipulation des parametres hyperfins (PHF)
!!et des variables ajustables (largeur de raie, hbruit)
!>@version juin 2016
!>@details Les parametres variables sont rangés dans le tableau B.
!>@n Pour chaque spectre, la lecture du tableau NB dans le fichier d'entree
!! indique quels parametres sont variables.
!! parametre          |DI|GA|H1|SQ|CH|ETA|TETA|GAMA|BETA|ALFA
!!--------------------|-:|-:|-:|-:|-:|--:|---:|---:|---:|---:|
!! position dans NB(J)| 1| 2| 3| 4| 5|  6|   7|   8|   9|  10|
!! Les PHF sont bloqués par defaut (NB(i)=0), sauf TY qui est toujours ajustable
module variablesAjustables
  use precision
  use connex
  use options
  implicit none
  integer:: K  !< nombre courant de parametres ajustables stockés dans B
  ! hauteur de bruit
  real(dp)::HBRUIT !< si HBRUIT /= 0, introduction d'un spectre de bruit de fond
  ! PHF pour un sous-spectre:
  real(dp)::DI    !< moment isomérique
  real(dp)::GA    !< demi-largeur (commune aux raies du spectre)
  real(dp)::H1    !< intensité totale (nombre de coups)
  real(dp)::SQ    !< interaction quadrupolaire
  real(dp)::CH    !< champ interne (kOe)
  real(dp)::ETA   !< parametre d'asymétrie
  real(dp)::THETA  !< TETA et GAMA :    angles polaires du chp interne
  real(dp)::GAMA  !<   dans les axes principaux du gradient (degres)
  real(dp)::BETA  !<  ALFA et BETA : angles polaires de la direction du
  real(dp)::ALPHA  !<         rayonnement (meme axes que pour ETA, THETA)
  real(dp)::TY    !< niveau moyen hors d'absorption
  integer::NB(10) !< Type d'ajustement des 10 paramètres hyperfins (pour le sous-spectre en cours de lecture)
                  !! exemple | signification 
                  !!--------:|--------------
                  !!NB(3)=0  | H1 est bloqué
                  !!NB(3)=1  | H1 est à ajuster
                  !!NB(3)=2  | H1 est à ajuster de maniere identique a H1 du sous-spectre precedent
                  !!NB(3)=3  | H1 est relié a d'autres PHF ajustables, par la routine CONNEX
  ! toutes les variables ajustables
  real(dp)::B(40) !< variables ajustables,
                  !! B(K) = TY, B(K-1) = bruit ajustable
  real(dp)::VQ(40,40)  !< matrice de variance/covariance des variables ajustables

  !indications supplementaires :
  integer::MONOC      !<   MONOC=1 : monocristal, 
                      !!   MONOC=0 : poudre
  integer::MONOT(40)  !< Indication cristal/non cristal de chaque sous-spectre, cf. MONOC
  integer::IOGV       !<   IOGV   |      type d'ajustement des raies
                      !! --------:|-------------------------------------
                      !!        0 | largeur unique pour toutes les raies (ajustable si NB(2)= 1 ou 2)
                      !!        1 | spectre quadrupolaire à raies de largeurs differentes (2 largeurs independantes)
                      !!        2 | spectre magnétique formé de 3 doublets symetriques d'entensité 3,2,1 (3 largeurs indépendantes)
                      !!        3 | cas général
  integer::IOGVT(40)  !< type d'ajustement des raies de chaque sous-spectre, cf IOGV
  ! PHF initiaux dans le cas d'une distribution arithmetique du spectre
  integer::NB0(10)!< Équivalent NB pour le premier sous-spectre de la distribution
  real(dp)::DI0   !< DI du premier sous-spectre de la distribution
  real(dp)::PDI   !< Increment de DI
  real(dp)::CH0   !< cf. DI0
  real(dp)::PCH   !< Increment de CH
  real(dp)::SQ0   !< cf. DI0
  real(dp)::PSQ   !< Increment de SQ
  real(dp)::THETA0 !< cf. DI0
  real(dp)::PTHETA !< Increment de TETA
  ! PHF pour tout les spectre (une fois la lecture des données terminées)
  real(dp)::BT(10,40) !< Parametres hyper fins de tous les spectres (cf. DI à ALFA)
  real(dp)::ETBT(10,40)!< Ecart type de BT
  integer(dp)::NBT(10,40) !< Type d'ajustement des PHF de tous les spectres (cf. NB)
  integer ::IAD(10,40) !< IAD(i,n) = emplacement du PHF ajustable BT(i,n) dans B
  ! Largeur des raies
  integer::NG(8)      !< NG(i)| Ajustement des largeurs 
                      !! ----:|-------------------------
                      !!  0   | pas d'ajustement de la ieme largeur , 
                      !!  1   | ajustement de la ieme largeur , valeur initiale GV(i)
  real(dp)::GV(8)     !< GV(i)=valeur initiale de la ieme largeur, si NG(i)=1
  real(dp)::GVT(8,40) !< valeur des largeurs variables, pour tout les sous-spectres
  real(dp)::ETGVT(8,40)!< Ecart type de GVT
  integer::NGT(8,40)  !< equivalent de NG, pour tout les sous-spectres 
  integer::IADG(8,40) !< IADG(i,n) = emplacement de la largeur variable GVT(i,n) dans B
  !gestion d'erreur
  character(len=*),parameter:: erreur_kmax='Nombre de parametres ajustables superieur à 40' !<gestion d'erreur
  contains
  !=====================================================================
  !>@brief Mise à zero de toutes les variables du module variableAjustable
  subroutine variablesAjustables_raz
      NB=0
      MONOC=0
      B=0.0_dp
      HBRUIT=0.0_dp
      K=1
      call variablesAjustables_raz_hyperfins
      call variablesAjustables_raz0
  end subroutine variablesAjustables_raz
  !=====================================================================
  !> @brief Remise à zero des dernier PHF lues dans le fichier de donnees
  subroutine variablesAjustables_raz_hyperfins
      DI=0.0_dp
      GA=0.1_dp
      H1=1.D5
      SQ=0.0_dp
      CH=0.0_dp
      ETA=0.0_dp
      THETA=0.0_dp
      GAMA=0.0_dp
      BETA=0.0_dp
      ALPHA=0.0_dp
      MONOC=0
      TY=0.0_dp
  end subroutine variablesAjustables_raz_hyperfins
  !=====================================================================
  !>@brief Remise à zero des paramètres hyperfins du premier spectre de la distribution
  subroutine variablesAjustables_raz0
      DI0=0.0_dp
      PDI=0.0_dp
      CH0=0.0_dp
      PCH=0.0_dp
      SQ0=0.0_dp
      PSQ=0.0_dp
      THETA0=0.0_dp
      PTHETA=0.0_dp
      NB0=0
  end subroutine variablesAjustables_raz0
  !=====================================================================
  !> @brief Calcul des paramètres hyperfins du NTième spectre selon une distribution arithmétique 
  subroutine variablesAjustables_super(nt,ns1)
      integer,intent(in)::nt !< numero du spectre
      integer,intent(in)::ns1!< numéro du premier spectre
      real(dp):: step
      step = real(nt-ns1,dp)
      DI=DI0+step*PDI
      SQ=SQ0+step*PSQ
      CH=CH0+step*PCH
      THETA=THETA0+step*PTHETA
      IOGV=0
  end subroutine variablesAjustables_super
  !=====================================================================
  !>@brief Définition des largeurs variables des raies du spectre d'apres IOGV. 
  !!@n@b Remarque: dans le cas d'une distribution arithmétique, IOGV=0
  subroutine variablesAjustables_definir_largeurs_raies
    NG=0
    GV=0
    if(IOGV/=0) GV(1:8) = GA  
    if((IOGV==1).OR.(IOGV==2)) NB(2)=0 !GA devient non ajustable  
    if(IOGV==1)then 
        NG(1)=1
        NG(3)=1
    elseif(IOGV==2)then
        NG(2)=1
        NG(3)=1
        NG(4)=1
    endif
  end subroutine variablesAjustables_definir_largeurs_raies
  !=====================================================================
  !>@brief Rangement des parametres hyperfins du NTième spectre dans les tableaux BT,NBT et B
  subroutine variablesAjustables_ranger(nt)
    integer,intent(in)::nt
    integer::i
    !rangement dans BT
    BT(1,nt)=DI
    BT(2,nt)=GA
    BT(3,nt)=H1
    BT(4,nt)=SQ
    BT(5,nt)=CH
    BT(6,nt)=ETA
    BT(7,nt)=THETA
    BT(8,nt)=GAMA
    BT(9,nt)=BETA
    BT(10,nt)=ALPHA
    MONOT(nt)=MONOC
    IOGVT(nt)=IOGV
    ! Rangement dans B (parametres ajustables)
    do i=1,10
      if(k>40) stop erreur_kmax
      NBT(i,nt)=NB(i)
      if(NB(i)==1)then    !si le PHF est à ajuster, on le range dans B
        IAD(i,nt)=K
        B(K)=BT(i,nt)
        K=K+1
      elseif(NB(i)==2)then ! cas où le PHF s'ajuste comme celui du spectre precedent
        IAD(i,nt) = IAD(i,nt-1)
      endif
    enddo
    ! Rangement des largeurs variables dans GVT,NGT et B
    do i=1,8
      if(k>40) stop erreur_kmax
      NGT(i,nt) = NG(i)
      GVT(i,nt)= GV(i)
      if(NG(i)==1)then
        IADG(i,nt)=K
        B(K)=GVT(i,nt)
        K=K+1
      endif
    enddo

  end subroutine variablesAjustables_ranger
  !=====================================================================
  !>@brief Actualisation du rangement des parametre ajustables du  NTième spectre
  subroutine variablesAjustables_actualiser_rangement(nt)
    integer,intent(in)::nt!< numéro du spectre concerné
    integer::i,l
    !mise dans BT et GVT des parametres---------------------------------
    do i=1,10
      if(NBT(i,nt)/=0)then
        l=IAD(i,nt)
        BT(i,nt)=B(l)
      endif
      if(IO(5)/=0 .AND. NBT(i,nt)==3) call connex_connexions(BT,IO)
    enddo
    do i=1,8
      GVT(i,nt)=BT(2,nt)
      if(NGT(i,nt)/=0)then 
        l=IADG(i,nt)
        GVT(i,nt)=B(l)
      endif
    enddo
    ! option beta=theta, alpha=gama ------------------------------------
    if(IO(9)==1)then
      BT(9,nt)=BT(7,nt)
      BT(10,nt)=BT(8,nt)
    endif
  end subroutine variablesAjustables_actualiser_rangement
  !=====================================================================
  !>@brief ajout du bruit dans le tableau des parametres ajustables
  subroutine variablesAjustables_ranger_bruit
      if(k>40) stop erreur_kmax
      B(K)=HBRUIT
      K=K+1
  end subroutine variablesAjustables_ranger_bruit
  !=====================================================================
  !>@brief Evaluation du taux maxi de comptage ("niveau zéro") à partir des 10 premiers et 10 derniers canaux,
  !! rangement du taux dans B.
  subroutine variablesAjustables_nivzer(spectre)
    real(dp),intent(in)::spectre(:)!< Une variable qui porte bien son nom.
    integer::i,ny
    ny=size(spectre)
    do i=2,11
      TY=TY+0.05_dp*spectre(i)
    enddo
    do i=ny-9,ny
          TY=TY+0.05_dp*spectre(i)
    enddo
    !rangement dans les parametres variables B
    if(k>40)  stop erreur_kmax
    B(K)=TY
  end subroutine variablesAjustables_nivzer
  !===================================================================== 
  !>@brief en fonction de IOGVT(NT),applique les relations demandées entre les 
  !! largeurs de raies (spectre quadrupolaire, spectre magnétique...)
  subroutine variablesAjustables_actualiser_largeur_raies(nt)
    integer,intent(in)::nt !< numéro du spectre en cours de traitement. 
    if(IOGVT(nt)==1)then 
      GVT(2,nt)=GVT(1,nt)
      GVT(5,nt)=GVT(1,nt)
      GVT(6,nt)=GVT(1,nt)
      GVT(4,nt)=GVT(3,nt)
      GVT(7,nt)=GVT(3,nt)
      GVT(8,nt)=GVT(3,nt)
    elseif(IOGVT(nt)==2) then
      GVT(7,nt)=GVT(2,nt)
      GVT(6,nt)=GVT(3,nt)
      GVT(5,nt)=GVT(4,nt)
    endif
  endsubroutine variablesAjustables_actualiser_largeur_raies
  !=====================================================================
  !<@brief ! Calcul des ecarts type des paramètres ajustables
  subroutine variablesAjustables_calculer_ecart_type(phi,nt,n)
    integer,intent(in)::nt!< numéro du spectre concerné
    integer,intent(in)::n !<nombre de canaux par spectre
    real(dp),intent(in)::phi !<Somme des Ycalc-Yexp
    integer::i,l
    do i=1,10
      if(NBT(i,nt)/=0)then 
        l=IAD(i,nt)
        ETBT(i,nt) = sqrt(VQ(l,l)*phi/(n-K))
      endif
    enddo
    do i=1,8
      if(NGT(i,nt)/=0)then
        l=IADG(i,nt)
        ETGVT(i,nt) = sqrt(VQ(l,l)*phi/(n-K))
      endif
    enddo
  end subroutine variablesAjustables_calculer_ecart_type
end module variablesAjustables
