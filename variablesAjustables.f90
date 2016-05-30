module variablesAjustables
!**********************************************************************
!       module hyperfins
!       Rangements et manipulation des parametres hyperfins (PHF)
!       et des parametres ajustables (largeur de raie, hbruit)
!......................................................................
  use precision
  use connex
  implicit none
  integer:: K  ! nombre courant de parametres ajustables stockés dans B
    ! hauteur de bruit
  real(dp)::HBRUIT ! si HBRUIT /= 0, introduction d'un spectre de bruit de fond
    ! PHF pour un sous-spectre:
  real(dp)::DI    ! moment isomérique
  real(dp)::GA    ! demi-largeur (commune aux raies du spectre)
  real(dp)::H1    ! intensité totale (nombre de coups)
  real(dp)::SQ    ! interaction quadrupolaire
  real(dp)::CH    ! champ interne (kOe)
  real(dp)::ETA    ! parametre d'asymétrie
  real(dp)::TETA  !  TETA et GAMA :    angles polaires du chp interne
  real(dp)::GAMA  !    dans les axes principaux du gradient (degres)
  real(dp)::BETA  !  ALFA et BETA : angles polaires de la direction du
  real(dp)::ALFA  !          rayonnement (meme axes que pour ETA, THETA)
  real(dp)::TY    ! niveau moyen hors d'absorption
  integer::NB(10) ! indique le type d'ajustement des 10 PHF (pour le sous-spectre en cours de lecture)
      ! parametre           DI GA H1 SQ CH ETA TETA GAMA BETA ALFA
      ! indice dans NB(J)    1  2  3  4  5  6    7    8    9    10
      ! exemple :   NB(3)=0  : H1 est bloqué
      !             NB(3)=1  : H1 est a ajuster
      !             NB(3)=2  : H1 est a ajuster de maniere identique a H1 du sous-spectre precedent
      !             NB(3)=3  : H1 est relié a d'autres PHF ajustables, par la routine CONNEX
      ! Les PHF sont bloqués par defaut (NB=0), sauf TY qui est toujours ajustable

  ! PHF initiaux dans le cas d'une progression arithmetique du spectre
  integer::NB0(10)
  real(dp)::DI0   ! DI du premier sous-spectre de la progression 
  real(dp)::PDI   ! increment de DI
  real(dp)::CH0   ! cf. DI0
  real(dp)::PCH   ! increment de CH
  real(dp)::SQ0   ! cf. DI0
  real(dp)::PSQ   ! increment de SQ
  real(dp)::TETA0 ! cf. DI0
  real(dp)::PTETA ! increment de TETA

  !PHF pour tout les spectre (une fois la lecture des données terminées)
  integer::MONOT(40) ! indication cristal/non cristal de chaque sous-spectre, cf. MONOC
  integer::IOGVT(40) ! type d'ajustement des raies de chaque sous-spectre, cf IOGV
  real(dp)::BT(10,40) ! PHF de tous les spectres (cf. DI à ALFA)
  real(dp)::NBT(10,40) ! type d'ajustement des PHF de tous les spectres (cf. NB)
  real(dp)::B(40) ! PHF ajustables 
  integer ::IAD(10,40) ! IAD(i,n) = emplacement du PHF ajustable BT(i,n) dans B
  !indications supplementaires :
  integer::MONOC  !   MONOC=1 : monocristal
                  !   MONOC=0 : poudre
  integer::IOGV   ! type d'ajustement des raies
                  !   IOGV=0 : largeur unique pour toutes les raies (ajustable si NB(2)= 1 ou 2)
                  !   IOGV=1 : spectre quadrupolaire à raies de largeurs differentes (2 largeurs independantes)
                  !   IOGV=2 : spectre magnetique formé de 3 doublets symetriques d'entensité 3,2,1 (3 largeur independantes)
                  !   IOGV=3 : cas general
  !largeur des raies
  integer::NG(8)      ! NG(i)=0 : pas ajustement de la ieme largeur , 
                      ! NG(i)=1 : ajustement de la ieme largeur , valeur initiale GV(i)
  real(dp)::GV(8)     ! GV(i)=valeur initiale de la ieme largeur, si NG(i)=1
  real(dp)::GVT(8,40) ! valeur des largeurs variables, pour tout les sous-spectres
  integer::NGT(8,40)  ! equivalent de NG, pour tout les sous-spectres 
  integer::IADG(8,40) ! IADG(i,n) = emplacement de la largeur variable GVT(i,n) dans B
  !gestion d'erreur
  character(len=*),parameter:: erreur_kmax='Nombre de parametres ajustables superieur à 40'
  contains
  !---------------------------------------------------------------------
  subroutine variablesAjustables_raz
    !remise à zero des dernier PHF lus dans le fichier de donnee
      DI=0.0_dp
      GA=0.1_dp
      H1=1.D5
      SQ=0.0_dp
      CH=0.0_dp
      ETA=0.0_dp
      TETA=0.0_dp
      GAMA=0.0_dp
      BETA=0.0_dp
      ALFA=0.0_dp
      MONOC=0
      TY=0.0_dp
      NB=0
      MONOC=0
  end subroutine variablesAjustables_raz
  !---------------------------------------------------------------------
  subroutine variablesAjustables_raz0
      DI0=0.0_dp
      PDI=0.0_dp
      CH0=0.0_dp
      PCH=0.0_dp
      SQ0=0.0_dp
      PSQ=0.0_dp
      TETA0=0.0_dp
      PTETA=0.0_dp
      NB0=0
  end subroutine variablesAjustables_raz0
  !---------------------------------------------------------------------
  subroutine variablesAjustables_super(nt,ns1)
  ! calcul des parametres hyperfins selon une distribution arithmetique 
      integer,intent(in)::nt,ns1
      real(dp):: step
      step = real(nt-ns1,dp)
      DI=DI0+step*PDI
      SQ=SQ0+step*PSQ
      CH=CH0+step*PCH
      TETA=TETA0+step*PTETA
      IOGV=0
  end subroutine variablesAjustables_super
    !---------------------------------------------------------------------
  subroutine variablesAjustables_definir_largeurs_raies
      !definition des largeurs variables des raies du spectre d'apres IOGV
      !Remarque: dans le cas d'une distribution arithmetique, IOGV=0
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
  !---------------------------------------------------------------------
  subroutine variablesAjustables_ranger(nt)
    !rangement des parametres hyperfins dans les tableaux BT,NBT et B
    integer,intent(in)::nt
    integer::i
    !rangement dans BT
    BT(1,nt)=DI
    BT(2,nt)=GA
    BT(3,nt)=H1
    BT(4,nt)=SQ
    BT(5,nt)=CH
    BT(6,nt)=ETA
    BT(7,nt)=TETA
    BT(8,nt)=GAMA
    BT(9,nt)=BETA
    BT(10,nt)=ALFA
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
    !rangement des largeurs variables dans GVT,NGT et B
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
  !---------------------------------------------------------------------
  subroutine variablesAjustables_ranger_bruit
      if(k>40) stop erreur_kmax
      B(K)=HBRUIT
      K=K+1
  end subroutine variablesAjustables_ranger_bruit
  !---------------------------------------------------------------------
  subroutine variablesAjustables_nivzer(Y)
  ! Evaluation du taux maxi de comptage à partir des 10 premiers et 10 derniers canaux
    real(dp),intent(in)::Y(:)
    integer::i,ny
    ny=size(Y)
    do i=2,11
      TY=TY+0.05_dp*Y(i)
    enddo
    do i=ny-9,ny
          TY=TY+0.05_dp*Y(i)
    enddo
    !rangement dans les parametres variables B
    if(k>40)  stop erreur_kmax
    B(K)=TY
  end subroutine variablesAjustables_nivzer
  !--------------------------------------------------------------------- 
  subroutine variablesAjustables_actualiser_largeur_raies(nt)
  ! en fonction de IOGVT(NT),applique les relations demandées entre les 
  ! largeurs de raies (spectre quadrupolaire, spectre magnétique...)
    integer,intent(in)::nt
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
  !---------------------------------------------------------------------
end module variablesAjustables
