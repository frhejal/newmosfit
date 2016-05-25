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
  use options   ! variables pour choix des options
  use lecture   ! routines de lecture du fichier .coo
  use ecriture  ! routines d'ecriture du fichier résultat
  use algebre   ! routines d'algebre lineraire (inverses de matrice, resolution de systemes)
  implicit none
  integer::NMAX,NS,NS1,NS2,IOPT,MONOC
  integer::NG(8),NB(10)
  real(dp)::CN,HBRUIT, DI,GA,H1,SQ,CH,ETA,TETA,GAMA,BETA,ALFA
  real(dp)::GRASS(10)
  !variables locales
  integer::ind
  
  character(len=*)::fichier_sortie='fit.out'
  !-----------------------------------------------------------------------
  !Lecture des options 
  call lecture_options(CN,NMAX,NS,NS1,NS2,IOPT,HBRUIT,GRASS)
  !-----------------------------------------------------------------------
  !Lecture des sous-spectres
  do NT=1,NS
    !(re)initialisations
    MONOC=0
    IOGV=0
    NG=0
    NB=0
    if(NT==NS1)then  ! lecture des parametres 
      read(5,* ) DI0,PDI,GA,H1,SQ0,PSQ,CH0,PCH,ETA0,TETA0,PTETA,GAMA0,BETA0,ALFA0,MONOC0
      read(5,*) (NB0(I), I=1,10)
    endif
    if((NT>=NS1)&&(NT<=NS2))then 
      call super
    else
!~     niveau du label 110.....
    endif
    
    
    
    
  enddo
!~   call lecture_spectre( DI,GA,H1,SQ,CH,ETA,TETA,GAMA,BETA,ALFA)
!~   call ecriture_options(CN,NMAX,NS,NS1,NS2,IOPT,HBRUIT,'test.out')
end program mosfit
