program mosfit
!***********************************************************************
!              __  __  ___  ____  _____ ___ _____ 
!             |  \/  |/ _ \/ ___||  ___|_ _|_   _|
!             | |\/| | | | \___ \| |_   | |  | |
!             | |  | | |_| |___) |  _|  | |  | |
!             |_|  |_|\___/|____/|_|   |___| |_|
!
!***********************************************************************
!         FITTAGE THEORIQUE DE SPECTRES MOSSBAUER  FER 57/SN 119
!                     VERSION  MAI  2016
! MODIFICATIONS*********************************************************
!
! INDIQUER DATE  NOM  MODIFICATION
!
!M1  OCT 83 J.T  SORTIE BENSON
!M2  JAN 84 JMG  CONVOLUTION
!M3  FEV 84 JMG  D/A
!M4  FEV 84 J.T  SORTIE BENSON PAR X0,G,H
!M5  MAI 84 J.T  PRECISION PARAMETRES
!M6  JAN 85 J.T  PERMUTATION LIGNES 939 ET 940 POUR SORTIE CORRECTE
!                DES LARGEURS AVEC IOGV
!M7  AOU 85 JMG  MODIF VALEURS STANDARDS
!M9  DEC 85 JMG  SORTIE BENSON SERIE DE SPECTRES
!    JAN 94  NR  SORTIE DES SPECTRES SOUS MATLAB 4.0 IBM RISC/6000
!    FEV 94  NR  TRACE DES SOUS-SPECTRES
!    MAR 95  YL  VALEUR MAX DE L'EXPONENTIELLE DANS CONVOL
!    1995-2016 ??  ???
!    MAI 2016 FL  REECRITURE EN FORTRAN 95
!********************OPTIONS********************************************
!    IO(1)=N  AJOUT N MILLIONS
!    IO(2)=1  TRACE SUR LARGEUR 12OCX
!    IO(3)=1  SN119
!    IO(4)=1  HBRUIT NON AJUSTABLE
!          2         AJUSTABLE
!    IO(5)=N  CONNEXIONS PARAMETRES
!    IO(6)=1  PERFORATION YEXP-YCALC
!    IO(7)=1  PERFORATION YCALC
!    IO(8)=1  X0,G,H,MATRICE DE VARIANCE COVARIANCE
!    IO(9)=1  BETA=TETA ; ALFA=GAMA
!    IO(10)=1  PAS DE SPECTRE EXPERIMENTAL
!           2  MEME SPECTRE EXP QUE CAS PRECEDENT
!    IO(11)=1  TRACE YEXP YCALC
!    IO(12)=1  SORTIE BENSON
!    IO(13)=1  DISTRIBUTION
!    IO(14)=1  DISTRIBUTION avec tracé du diagramme en cartouches
!    IO(16)=N  (N=NBRE DE SOUS CANAUX)  CONVOLUTION GAUSS*LORENZ
!    IO(17)=1  TRACE DES SOUS-SPECTRES
!    IO(20)=1  HORIZONTALISATION FOND CONTINU
!***********************************************************************
  use precision
  use options         ! variables pour choix des options
  use variablesAjustables ! variables des parametres hyperfins
  use ajustement      ! moindres carres
  use lecture         ! routines de lecture du fichier .coo
  use ecriture        ! routines d'ecriture du fichier résultat
  use algebre         ! routines d'algebre lineaire (inverses de matrice, resolution de systemes)
  use spectres         ! variables de stockage des spectre (experimental, theorique ou de bruit), gestion du bruit
  implicit none
!***********************************************************************
  !variables locales----------------------------------------------------
  integer::NMAX,NS1,NS2,NT
  real(dp)::GRASS(10)
  real(dp)::AA(1600)
  real(dp)::CRITERE ! critere de convergence
  real(dp)::KHI2   ! ecart statistique de l'ajustement en moindres carrés
  real(dp)::dump ! variable-poubelle
  real(dp)::cmin,cmax
  integer::i,j,ij
  character(len=*),parameter::fichier='test.out'
  !initialisations------------------------------------------------------
  call raz
  call options_raz
  call variablesAjustables_raz
!***********************************************************************
! Entree des options et des données, et copie dans le fichier de sortie
!***********************************************************************
  !Lecture des options--------------------------------------------------
  call lecture_titre
  call lecture_options(NMAX,NS,NS1,NS2,HBRUIT,GRASS)
  call ecriture_nommer_fichier_de_sortie(fichier)
  call ecriture_titre(0)
  call ecriture_options(NMAX,NS,NS1,NS2)
  !Lecture des parametres ajustables des sous-spectres------------------
  !(ou construction d'une distribution en progression arithmetique)
  do NT=1,NS
    MONOC=0
    IOGV=0
    if((NT>=NS1) .AND. (NT<=NS2))then
      ! Progression arithmetique demandee du sous-spectre NS1 au sous-spectre NS2
      if(NT==NS1) call lecture_param0(DI0,PDI,GA,H1,SQ0,PSQ,CH0,PCH,ETA,TETA0,PTETA,GAMA,BETA,ALFA,MONOC,NB)
      call variablesAjustables_super(NT,NS1)
    else
      if(IO(10)/=2) call variablesAjustables_raz
      call lecture_param(DI,GA,H1,SQ,CH,ETA,TETA,GAMA,BETA,ALFA,MONOC,NB,IOGV )
      call ecriture_param(DI,GA,H1,SQ,CH,ETA,TETA,GAMA,BETA,ALFA,MONOC,NB)
    endif
    call variablesAjustables_definir_largeurs_raies
    ! Mise en tableau des parametres hyperfins et des largeurs variables
    call variablesAjustables_ranger(NT) 
  enddo
  ! Lecture de bruit-----------------------------------------------------
  if(IO(4)/=0)then
    if(IO(4)/=1) call variablesAjustables_ranger_bruit
    call ecriture_bruit
    call lecture_titre
    call ecriture_titre(0)
    call lecture_spectre(BF,N)
    call spectres_preparer_bruit
  endif
  !Chargement du spectre experimental-----------------------------------
  if(IO(10)==0)call lecture_spectre(Y,N)  
  if(IO(10)==1)then
    ! pas de spectre experimental
    Y=0.0_dp          
  else
    ! ajout de IO(1) million(s) demandé en option
    Y = Y + real(IO(1),dp)*1000000_dp 
  endif
!Defnition du niveau zero-----------------------------------------------
  if(TY==0.0_dp) call variablesAjustables_nivzer(Y)
!modification des poids pour canaux ignorés-----------------------------
  call spectres_poids(IZ)
!***********************************************************************
! Ajustement par moindres-carres
!***********************************************************************
  if(NMAX==0) then
    ! Pas d'ajustement, simple calcul du spectre theorique à partir des parametres initiaux
    call ecriture_info_iteration(NMAX,NMAX,B)
    call spectres_theorique_total
  else
    ! Algorithme d'estimation moindres-carrés de Marquardt
    call ajustement_moindres_carres(Q,N,B,Y,K,POIDS,NMAX,CRITERE)
    !inversion de la matrice des variances-covariances------------------
    ij=0
    do j=1,K
      do i=1,K
        ij=ij+1
        AA(ij)=VQ(i,j)
      enddo
    enddo
    call minv(AA,K,dump)
    ij=0
    do j=1,K
      do i=1,K
        ij=ij+1
        VQ(i,j)=AA(ij)
      enddo
    enddo
    ! remise des bonnes valeurs dans les tableaux X0,H,G----------------
    do nt=1,NS
      call variablesAjustables_calculer_ecart_type(PH,nt,n) 
      call variablesAjustables_actualiser_rangement(nt)
      if(IOGVT(nt)/=0) call variablesAjustables_actualiser_largeur_raies(nt)
      DI=BT(1,nt)
      GA=BT(2,nt)
      H1=BT(3,nt)
      call habillage_raies(DI,GA,H1,N,nt,ENERGIES(:,nt),INTENSITES(:,nt),SPECTRE_THEO)
    enddo
  endif
!***********************************************************************
! Sorties
!***********************************************************************
  call ecriture_titre(1)
  !ecarts type
  call ecriture_ecart_type(NS,BT,ETBT,GVT,ETGVT,IOGVT)
  ! largeurs, hauteur et energie des raies
  if(io(8)==1) call ecriture_raies_covariance(NS,X0,G,H)
  ! Absorptions des differents sous-spectres
  call ecriture_rapports_absorption(NS,NS2,K,N,B,BT,Y,Q(:,K+2),BF,TY,HBRUIT)
  ! Calcul du khi**2
  KHI2=ajustement_ecart_stat(K,N,Y,Q(:,K+2),POIDS)
  call ecriture_ecart_stat(KHI2)
  call ecriture_spectres(N,Y,Q(:,k+2),cmin,cmax)
  call ecriture_fin
  
  contains 
    subroutine raz
    !remise a zero des variables
      CRITERE = 0.001_dp
      POIDS=1.0_dp
      B=0.0_dp
      HBRUIT=0.0_dp
      CN=0.078125_dp
      NS=1
      NS1=0
      NS2=0
      K=1
    end subroutine raz
end program mosfit
