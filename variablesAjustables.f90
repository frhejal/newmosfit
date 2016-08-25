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
!                        MODULE VARIABLESAJUTABLES
!***********************************************************************
!>@brief Rangement et manipulation des paramètres hyperfins (PHF)
!! et des autres variables ajustables (largeur de raie, hbruit)
!>@version juin 2016
!>@details Les paramètres variables sont rangés dans le tableau B.
!>@n Pour chaque spectre, la lecture du tableau NB dans le fichier d'entrée
!! indique quels paramètres sont variables.
!! paramètre          |DI|GA|H1|SQ|CH|ETA|TETA|GAMA|BETA|ALFA
!!--------------------|-:|-:|-:|-:|-:|--:|---:|---:|---:|---:|
!! position dans NB(J)| 1| 2| 3| 4| 5|  6|   7|   8|   9|  10|
!! Les PHF sont bloqués par défaut (NB(i)=0), sauf TY qui est toujours ajustable
module variablesAjustables
  use precision
  use connex
  use options
  implicit none
  integer,save:: K  !< Nombre courant de paramètres ajustables stockés dans B
  real(DP),save::HBRUIT !< Si HBRUIT /= 0, introduction d'un spectre de bruit de fond
  ! PHF pour un sous-spectre:
  real(DP),save::DI    !< Déplacement isomérique
  real(DP),save::GA    !< Demi-largeur (commune aux raies du spectre)
  real(DP),save::H1    !< Intensité totale (nombre de coups)
  real(DP),save::SQ    !< Interaction quadrupolaire
  real(DP),save::CH    !< Champ interne (kOe)
  real(DP),save::ETA   !< Paramètre d'asymétrie
  real(DP),save::THETA !< THETA et GAMA :  angles polaires du champ interne
  real(DP),save::GAMA  !<   dans les axes principaux du gradient (degrés)
  real(DP),save::BETA  !<  ALPHA et BETA : angles polaires de la direction du
  real(DP),save::ALPHA  !<         rayonnement (mêmes axes que pour ETA, THETA)
  real(DP),save::TY    !< Niveau moyen hors d'absorption
  integer,save::NB(10) !< Type d'ajustement des 10 paramètres hyperfins (pour le sous-spectre en cours de lecture)
                  !! exemple | signification 
                  !!--------:|--------------
                  !!NB(3)=0  | H1 est bloqué
                  !!NB(3)=1  | H1 est à ajuster
                  !!NB(3)=2  | H1 est à ajuster de manière identique a H1 du sous-spectre précédent
                  !!NB(3)=3  | H1 est relié a d'autres PHF ajustables, par la routine CONNEX
  ! Toutes les variables ajustables
  real(DP),save::B(40) !< Variables ajustables,
                  !! B(K) = TY, B(K-1) = bruit ajustable
  real(DP),save::VQ(40,40)  !< matrice de variance/covariance des variables ajustables

  ! Indications supplementaires :
  integer,save::MONOC      !<   MONOC=1 : monocristal, 
                      !!   MONOC=0 : poudre
  integer,save::MONOT(40)  !< Indication cristal/non cristal de chaque sous-spectre, cf. MONOC
  integer,save::IOGV       !<   IOGV   |      type d'ajustement des raies
                      !! --------:|-------------------------------------
                      !!        0 | largeur unique pour toutes les raies (ajustable si NB(2)= 1 ou 2)
                      !!        1 | spectre quadrupolaire à raies de largeurs différentes (2 largeurs indépendantes)
                      !!        2 | spectre magnétique formé de 3 doublets symétriques d'intensité 3,2,1 (3 largeurs indépendantes)
                      !!        3 | cas général
  integer,save::IOGVT(40)  !< Type d'ajustement des raies de chaque sous-spectre, cf IOGV
  ! PHF initiaux dans le cas d'une distribution arithmetique de spectre
  integer::NB0(10)!< Équivalent NB pour le premier sous-spectre de la distribution
  real(DP),save::DI0   !< DI du premier sous-spectre de la distribution
  real(DP),save::PDI   !< Incrément de DI
  real(DP),save::CH0   !< cf. DI0
  real(DP),save::PCH   !< Incrément de CH
  real(DP),save::SQ0   !< cf. DI0
  real(DP),save::PSQ   !< Incrément de SQ
  real(DP),save::THETA0 !< cf. DI0
  real(DP),save::PTHETA !< Incrément de TETA
  ! PHF pour tout les spectres (une fois la lecture des données terminées)
  real(DP),save::BT(10,40) !< Paramètres hyperfins de tous les spectres (cf. DI à ALFA)
  real(DP),save::ETBT(10,40)!< Ecart type de BT
  integer(dp),save::NBT(10,40) !< Type d'ajustement des PHF de tous les spectres (cf. NB)
  integer,save::IAD(10,40) !< IAD(i,n) = emplacement du PHF ajustable BT(i,n) dans B
  ! Largeur des raies
  integer,save::NG(8)      !< NG(i)| Ajustement des largeurs 
                      !! ----:|-------------------------
                      !!  0   | pas d'ajustement de la ième largeur , 
                      !!  1   | ajustement de la ième largeur , valeur initiale GV(i)
  real(DP),save::GV(8)     !< GV(i)=valeur initiale de la ième largeur, si NG(i)=1
  real(DP),save::GVT(8,40) !< Valeur des largeurs variables, pour tout les sous-spectres
  real(DP),save::ETGVT(8,40)!< Ecart type de GVT
  integer,save::NGT(8,40)  !< Equivalent de NG, pour tout les sous-spectres 
  integer,save::IADG(8,40) !< IADG(i,n) = emplacement de la largeur variable GVT(i,n) dans B
  ! Gestion d'erreur
  character(len=*),parameter:: erreur_kmax='Nombre de parametres ajustables superieur à 40' !<gestion d'erreur
  contains
  !=====================================================================
  !>@brief Mise à zero de toutes les variables du module variableAjustable
  subroutine variablesAjustables_raz
      NB=0
      MONOC=0
      B=0.0_DP
      HBRUIT=0.0_DP
      K=1
      call variablesAjustables_raz_hyperfins
      call variablesAjustables_raz0
  end subroutine variablesAjustables_raz
  !=====================================================================
  !> @brief Remise à zéro des dernier PHF lues dans le fichier de données
  subroutine variablesAjustables_raz_hyperfins
      DI=0.0_DP
      GA=0.1_DP
      H1=1.D5
      SQ=0.0_DP
      CH=0.0_DP
      ETA=0.0_DP
      THETA=0.0_DP
      GAMA=0.0_DP
      BETA=0.0_DP
      ALPHA=0.0_DP
      MONOC=0
      TY=0.0_DP
  end subroutine variablesAjustables_raz_hyperfins
  !=====================================================================
  !>@brief Remise à zéro des paramètres hyperfins du premier spectre de la distribution
  subroutine variablesAjustables_raz0
      DI0=0.0_DP
      PDI=0.0_DP
      CH0=0.0_DP
      PCH=0.0_DP
      SQ0=0.0_DP
      PSQ=0.0_DP
      THETA0=0.0_DP
      PTHETA=0.0_DP
      NB0=0
  end subroutine variablesAjustables_raz0
  !=====================================================================
  !> @brief Calcul des paramètres hyperfins du NTième spectre selon une distribution arithmétique 
  subroutine variablesAjustables_super(nt,ns1)
      integer,intent(in)::nt !< Numéro du spectre
      integer,intent(in)::ns1!< Numéro du premier spectre
      real(DP):: step
      step = real(nt-ns1,dp)
      DI=DI0+step*PDI
      SQ=SQ0+step*PSQ
      CH=CH0+step*PCH
      THETA=THETA0+step*PTHETA
      IOGV=0
  end subroutine variablesAjustables_super
  !=====================================================================
  !>@brief Définition des largeurs variables des raies du spectre d'apres IOGV. 
  !!@details Cette routine travaille de concert avec  variablesAjustables_actualiser_largeur_raies
  !! Une fois les différentes largeurs de raies calculées et affinées
  !!Remarque: dans le cas d'une distribution arithmétique, IOGV=0
  subroutine variablesAjustables_definir_largeurs_raies
    integer::i
    select case(IOGV)
      case(0)
        NG=0 ! Toutes les raies sont égale à GA
        GV=0.0_DP
      case(1)
        GV = GA
        NB(2)=0 !GA devient non ajustable  
        ! Deux largeurs de raies indépendantes.
        NG(1)=1 
        NG(3)=1
      case(2)
        GV = GA
        NB(2)=0 ! GA devient non ajustable  
        ! Trois largeurs de raies indépendantes.
        NG(2)=1
        NG(3)=1
        NG(4)=1
      case(3)! Largeur de raies définies par utilisateur
        do i=1,8
          !Les raies sont ajustables si NB(i)=1, défini par l'utilisateur
          if(NG(i)==0) GV(i)=0.0_DP 
        enddo
      case default
        write(6,*) "STOP IOGV=", IOGV
        stop "OPTION INVALIDE POUR IOGV"
    end select
        
  end subroutine variablesAjustables_definir_largeurs_raies
  !===================================================================== 
  !>@brief en fonction de IOGVT(NT),applique les relations demandées entre les 
  !! largeurs de raies (spectre quadrupolaire, spectre magnétique...)
  !!@details Les largeurs à ajuster ont été définie dans la routine 
  !! variablesAjustables_definir_largeurs_raies. 
  !!@n Il s'agit ici d'appliquer les largeurs de raies calculées à toutes les raies
  !! pour la construction du spectre.
  subroutine variablesAjustables_actualiser_largeur_raies(nt)
    integer,intent(in)::nt !< Numéro du spectre en cours de traitement. 
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
  !>@brief Rangement des paramètres hyperfins du NTième spectre dans les tableaux BT,NBT et B,
  !!Rangement des largeur de raie dans les tableaux NGT, GVT et B
  subroutine variablesAjustables_ranger(nt)
    integer,intent(in)::nt
    integer::i
    ! Rangement dans BT
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
    ! Rangement dans B (paramètres ajustables)
    do i=1,10
      if(k>40) stop erreur_kmax
      NBT(i,nt)=NB(i)
      if(NB(i)==1)then    ! Si le PHF est à ajuster, on le range dans B
        IAD(i,nt)=K
        B(K)=BT(i,nt)
        K=K+1
      elseif(NB(i)==2)then ! Cas où le PHF s'ajuste comme celui du spectre précedent
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
  !>@brief Actualisation du rangement des paramètre ajustables du  NTième spectre
  subroutine variablesAjustables_actualiser_rangement(nt)
    integer,intent(in)::nt!< Numéro du spectre concerné
    integer::i,l
    ! Mise dans BT et GVT des paramètres---------------------------------
    do i=1,10
      if(NBT(i,nt)/=0)then
        l=IAD(i,nt)
        BT(i,nt)=B(l)
      endif
      if(IO(5)/=0 .AND. NBT(i,nt)==3) call connex_connexions(BT,IO)
    enddo
    do i=1,8
      GVT(i,nt)=BT(2,nt)  ! Par défaut, largeur de raie actualisée à la valeur trouvée pour GA
      if(NGT(i,nt)/=0)then ! Selon la valeur de NGT, une autre largeur de raie peut être utilisée
        l=IADG(i,nt)
        GVT(i,nt)=B(l)
      endif
    enddo
    ! Option beta=theta, alpha=gama ------------------------------------
    if(IO(9)==1)then
      BT(9,nt)=BT(7,nt)
      BT(10,nt)=BT(8,nt)
    endif
  end subroutine variablesAjustables_actualiser_rangement
  !=====================================================================
  !>@brief Ajout du bruit dans le tableau des paramètres ajustables
  subroutine variablesAjustables_ranger_bruit
      if(k>40) stop erreur_kmax
      B(K)=HBRUIT
      K=K+1
  end subroutine variablesAjustables_ranger_bruit
  !=====================================================================
  !>@brief Evaluation du taux maxi de comptage ("niveau zéro") à partir des 10 premiers et 10 derniers canaux,
  !! rangement du taux dans B.
  subroutine variablesAjustables_nivzer(spectre)
    real(DP),intent(in)::spectre(:)!< Une variable qui porte bien son nom.
    integer::i,ny
    ny=size(spectre)
    do i=2,11
      TY=TY+0.05_DP*spectre(i)
    enddo
    do i=ny-9,ny
          TY=TY+0.05_DP*spectre(i)
    enddo
    ! Rangement dans les paramètres variables B
    if(k>40)  stop erreur_kmax
    B(K)=TY
  end subroutine variablesAjustables_nivzer
  !=====================================================================
  !<@brief ! Calcul des écarts type des paramètres ajustables
  subroutine variablesAjustables_calculer_ecart_type(phi,nt,n)
    integer,intent(in)::nt!< Numéro du spectre concerné
    integer,intent(in)::n !< Nombre de canaux par spectre
    real(DP),intent(in)::phi !< Somme des Ycalc-Yexp
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
