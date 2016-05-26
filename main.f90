program mosfit
!********************************************************************
!         FITTAGE THEORIQUE DE SPECTRES MOSSBAUER  FER 57
!                     VERSION  FEV  86
!**********************************************************************!
!
! MODIFICATIONS********************************************************
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
!   MAI 2016 FL  REECRITURE EN FORTRAN 95
!********************OPTIONS*******************************************
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
!*********************************************************************
  use precision
  use options         ! variables pour choix des options
  use param_hyperfins ! variables des parametres hyperfins
  use lecture         ! routines de lecture du fichier .coo
  use ecriture        ! routines d'ecriture du fichier résultat
  use algebre         ! routines d'algebre lineraire (inverses de matrice, resolution de systemes)
  implicit none
  integer::NMAX,NS,NS1,NS2,IOPT,IOGV,NT
  !IOGV : type d'ajustement des largeurs de raie dans un sous-spectre
  integer::NG(8)
  real(dp)::CN,HBRUIT
  real(dp)::GRASS(10),GV(8)
  !variables locales
  character(len=*),parameter::fichier='test.out'
!~   fichier_sortie='pouet'
  !-----------------------------------------------------------------------
  !Lecture des options 
  call ecriture_nommer_fichier_de_sortie(fichier)
  call lecture_options(CN,NMAX,NS,NS1,NS2,IOPT,HBRUIT,GRASS)
  call ecriture_options(CN,NMAX,NS,NS1,NS2,IOPT,HBRUIT)
  !-----------------------------------------------------------------------
  !Lecture (ou construction en progression arithmetique) des sous-spectres
  do NT=1,NS
  !(re)initialisations
    MONOC=0
    IOGV=0
    NG=0
    NB=0
    ! lecture des parametres  hyperfins
    if((NT>=NS1) .AND. (NT<=NS2))then !progression arithmetique demandee du spectre NS1 au spectre NS2
      if(NT==NS1)then
        call lecture_param0(DI0,PDI,GA,H1,SQ0,PSQ,CH0,PCH,ETA,TETA0,PTETA,GAMA,BETA,ALFA,MONOC,NB)
      endif
      call super(NT,NS1) ! calcul des parametres des spectres NS1 à NS2
    else
      if(IO(2)/=2) call razhyp ! remize à zero des parametres hyperfins
      call lecture_param( DI,GA,H1,SQ,CH,ETA,TETA,GAMA,BETA,ALFA,MONOC,NB,IOGV )
      call ecriture_param(DI,GA,H1,SQ,CH,ETA,TETA,GAMA,BETA,ALFA,MONOC,NB)
      if(IOGV/=0) GV(1:8) = GA  
      if((IOGV==1) .OR. (IOGV==2)) NB(2)=0
    endif
    
    
    
    
  enddo
!~   call lecture_spectre( DI,GA,H1,SQ,CH,ETA,TETA,GAMA,BETA,ALFA)
!~   call ecriture_options(CN,NMAX,NS,NS1,NS2,IOPT,HBRUIT,'test.out')
end program mosfit
