program mosfit
!**********************************************************************!
!         FITTAGE THEORIQUE DE SPECTRES MOSSBAUER  FER 57
!                     VERSION  MAI  2016
!**********************************************************************!
!
! MODIFICATIONS********************************************************!
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
!  1995-2016 ??  ???
!    MAI 2016 FL  REECRITURE EN FORTRAN 95
!********************OPTIONS*******************************************!
!    IO(1)=N  AJOUT N MILLIONS
!     IO(2)=1  TRACE SUR LARGEUR 12OCX
!    IO(3)=1  SN119
!    IO(4)=1  HBRUIT NON AJUSTABLE
!          2         AJUSTABLE
!    IO(5)=N  CONNEXIONS PARAMETRES
!    IO(6)=1  PERFORATION YEXP-YCALC
!   IO(7)=1  PERFORATION YCALC
!    IO(8)=1  X0,G,H,MATRICE DE VARIANCE COVARIANCE
!    IO(9)=1  BETA=TETA ; ALFA=GAMA
!    IO(10)=1  PAS DE SPECTRE EXPERIMENTAL
!           2  MEME SPECTRE EXP QUE CAS PRECEDENT
!    IO(11)=1  TRACE YEXP YCALC
!    IO(12)=1  SORTIE BENSON
!    IO(13)=1  DISTRIBUTION
!    IO(14)=1  DISTRIBUTION
!    IO(16)=N  (N=NBRE DE SOUS CANAUX)  CONVOLUTION GAUSS*LORENZ
!    IO(17)=1  TRACE DES SOUS-SPECTRES
!    IO(20)=1  HORIZONTALISATION FOND CONTINU
!**********************************************************************!
  use precision
  use options         ! variables pour choix des options
  use variablesAjustables ! variables des parametres hyperfins
  use lecture         ! routines de lecture du fichier .coo
  use ecriture        ! routines d'ecriture du fichier résultat
  use algebre         ! routines d'algebre lineaire (inverses de matrice, resolution de systemes)
  use spectres         ! variables de stockage des spectre (experimental, theorique ou de bruit), gestion du bruit
  implicit none
!**********************************************************************!
  !variables qu'ils faudra probablement ranger dans d'autres modules
  integer::NMAX,NS1,NS2,NT
  real(dp)::CN
  real(dp)::GRASS(10)
  real(dp)::E ! critere de convergence
  !variables locales
  character(len=*),parameter::fichier='test.out'
!~   fichier_sortie='pouet'
  !---------------------------------------------------------------------
  !initialisations
  call raz
  call options_raz
  call variablesAjustables_raz
  !---------------------------------------------------------------------
  !Lecture des options 
  call lecture_titre
  call lecture_options(CN,NMAX,NS,NS1,NS2,HBRUIT,GRASS)
  call ecriture_nommer_fichier_de_sortie(fichier)
  call ecriture_options(CN,NMAX,NS,NS1,NS2,HBRUIT)
  !---------------------------------------------------------------------
  !Lecture des parametres ajustables des sous-spectres (ou construction en progression arithmetique)
  do NT=1,NS
    MONOC=0
    IOGV=0
    if((NT>=NS1) .AND. (NT<=NS2))then
      ! Progression arithmetique demandee, du spectre NS1 au spectre NS2
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
  !---------------------------------------------------------------------
  ! lecture de bruit
  if(IO(4)/=0)then
    if(IO(4)/=1) call variablesAjustables_ranger_bruit
    call ecriture_bruit
    call lecture_titre
    call ecriture_titre
    call lecture_spectre(BF,N)
    call spectres_preparer_bruit
  endif
  !Chargement du spectre experimental
  if(IO(10)==0) call lecture_spectre(Y,N)  
  Y = Y + real(IO(1),dp)*1000000_dp  ! ajout de IO(1) million(s) demandé en option
  if(IO(10)==1)  Y=0.0_dp      ! pas de spectre experimental
  !Defnition du niveau zero
  !modification des poids
  !ajout du niveau zero en parametre
  
  

!~   call ecriture_spectre(Y)
  contains 
    subroutine raz
    !remise a zero des variables
      E = 0.001_dp
  !~     P=1.0_dp
  !~     Q=0.0_dp
      B=0.0_dp
      HBRUIT=0.0_dp
      CN=0.078125_dp
  !~     N=256
      NS=1
      NS1=0
      NS2=0
      K=1
  !~     NPAS=0
  !~     NMAX=20
    end subroutine raz
end program mosfit
