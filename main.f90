!>@file 
!!@brief Fichier contenant le programme principal
!***********************************************************************
!              __  __  ___  ____  _____ ___ _____ 
!             |  \/  |/ _ \/ ___||  ___|_ _|_   _|
!             | |\/| | | | \___ \| |_   | |  | |
!             | |  | | |_| |___) |  _|  | |  | |
!             |_|  |_|\___/|____/|_|   |___| |_|
!
!***********************************************************************
!>         Fittage theorique de spectres mossbauer  fer 57/sn 119,
!>@version                  juin  2016
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
!***********************************************************************
!>@param AA Matrice de Variance/covariance, stockée dans un vecteur
!!@param s(44)  Contributions des distributions (+4 cases vides)
!!@param sl(42)  Contributions lissées des distributions
!!@param sInt(40) Contributions intermediaires (=sommes cumulées) 
!!@param btmoy(7,2) moyenne des parametres hyperfins sur les spectres choisis
program mosfit
  use precision
  use options         ! variables pour choix des options
  use variablesFixes
  use variablesAjustables ! variables des parametres hyperfins
  use ajustement      ! moindres carres
  use lecture         ! routines de lecture du fichier .coo
  use ecriture        ! routines d'ecriture du fichier résultat
  use algebre         ! routines d'algebre lineaire (inverses de matrice, resolution de systemes)
  use spectres         ! variables de stockage des spectre (experimental, theorique ou de bruit), gestion du bruit
  implicit none
  real(dp)::AA(1600) 
  real(dp)::dump ! variable-poubelle
  real(dp)::cmin=0,cmax=0
  integer::nt,nts,nss
  real(dp)::daExp,daFit,sExp,sFit,sBruit
  real(dp)::diffSpectres(N)
  real(dp)::s(44)
  real(dp)::sl(42)
  real(dp)::sInt(40) 
  real(dp)::btmoy(7,2)
  character(len=*),parameter::fichierOut='test.out'
  character(len=*),parameter::fichierGnuplot='test.dat'
  character(len=*),parameter::fichierResultats='test.doc'
  !initialisations------------------------------------------------------
  call raz
!=======================================================================
! Entree des options et des données, copie dans le fichier de sortie
!=======================================================================
  !Lecture des options--------------------------------------------------
  call lecture_titre
  call lecture_options(CN,NMAX,NS,NS1,NS2,HBRUIT,GRASS)
  call ecriture_nommer_fichier_de_sortie(fichierOut)
  call ecriture_titre(0)
  call ecriture_options(CN,NMAX,NS,NS1,NS2)
  !Lecture des parametres ajustables des sous-spectres------------------
  !(ou construction d'une distribution en progression arithmetique)
  do NT=1,NS
    MONOC=0
    IOGV=0
    if((NT>=NS1) .AND. (NT<=NS2))then
      ! Progression arithmetique demandee du sous-spectre NS1 au sous-spectre NS2
      if(NT==NS1) call lecture_param0(DI0,PDI,GA,H1,SQ0,PSQ,CH0,PCH,ETA,&
                                      &TETA0,PTETA,GAMA,BETA,ALFA,MONOC,NB)
      call variablesAjustables_super(NT,NS1)
    else
      if(IO(10)/=2) call variablesAjustables_raz_hyperfins
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
    call ecriture_titre(1)
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
!=======================================================================
! Ajustement par moindres-carres
!=======================================================================
  if(NMAX==0) then
    ! Pas d'ajustement, simple calcul du spectre theorique à partir des parametres initiaux
    call ecriture_info_iteration(NMAX,NMAX,B)
    call spectres_theorique_total
  else
    ! Algorithme d'estimation moindres-carrés de Marquardt
    call ajustement_moindres_carres(Q,N,B,Y,K,POIDS,NMAX,CRITERE)
    !inversion de la matrice des variances-covariances------------------
    call algebre_matrice_vers_vecteur(VQ,AA,K,K)
    call algebre_inverser_matrice(AA,K,dump)
    call algebre_vecteur_vers_matrice(AA,VQ,K,K)
    ! remise des bonnes valeurs dans les tableaux X0,H,G----------------
    do nt=1,NS
      call variablesAjustables_calculer_ecart_type(PH,nt,N) 
      call variablesAjustables_actualiser_rangement(nt)
      if(IOGVT(nt)/=0) call variablesAjustables_actualiser_largeur_raies(nt)
      call spectres_theorique(nt)
      DI=BT(1,nt)
      GA=BT(2,nt)
      H1=BT(3,nt)
      call habillage_raies(CN,DI,GA,H1,N,nt,ENERGIES(:,nt),INTENSITES(:,nt),SOUS_SPECTRES(:,nt))
    enddo
  endif
!=======================================================================
! Sorties
!=======================================================================
  call ecriture_titre(1)
  call ecriture_titre(0)
  ! Ecarts type
  call ecriture_ecart_type(NS,BT,ETBT,GVT,ETGVT,IOGVT)
  ! Largeurs, hauteur et energie des raies
  if(io(8)==1) call ecriture_raies_covariance(NS,X0,G,H)
  ! Absorptions,moyenne et lissage des sous-spectres
  call spectres_absoption_dispersion(K,N,B,Y,Q(:,K+2),BF,TY,HBRUIT,sExp,sFit,sBruit,daExp,daFit)
  call spectres_contributions_distributions(NS,s,sInt)
  call ecriture_absorption_dispersion_contributions(NS,K,HBRUIT,daExp,daFit,sExp,sFit,sBruit,B,s,sInt)
  if(IO(13)/=0)then
    call spectres_lissage_distribution(NS,s,sl)
    call spectres_moyennes_param_hyperfins(ns,ns2,bt,s,sInt(NS),nss,btmoy)
    call ecriture_lissage(NS,s,sl)
    call ecriture_moyennes(nss,btmoy,' ')
  endif
  ! Calcul du khi**2
  KHI2=ajustement_ecart_stat(K,N,Y,Q(:,K+2),POIDS)
  call ecriture_ecart_stat(KHI2)
  call ecriture_tracer_spectres(N,Y,Q(:,k+2),cmin,cmax)
  if((IO(6)==1) .OR. (IO(11)==1)) then
    ! Ecriture des de la difference entre le spectre experimental et le spectre calculé
    diffSpectres=Y-Q(:,K+2)
    if(IO(6)==1) call ecriture_spectre_entier(diffSpectres)
    if(IO(11)==1) call ecriture_tracer_spectres(N,diffSpectres,diffSpectres,cmin,cmax)
  endif
  ! Ecriture du spectre calculé
  if(IO(7)==1) call ecriture_spectre_entier(Q(:,K+2))
  ! Résumé et ecriture des sous-spectres dans un fichier gnuplot
  if(IO(12)==1)then
    if(IO(17)==1)then
    ! Tracé des sous-spectres-------------------------------------------
      call spectres_total_sous_spectres(GRASS,nts)
    endif
    call ecriture_pour_gnuplot(N,nts,CN,Y,Q(:,K+2),TOTAL_SOUS_SPECTRES,fichierGnuplot)
    call ecriture_resultats_resume(NS,nss,s,sl,BT,btmoy,fichierResultats)
  endif
  call ecriture_fin
  contains
!***********************************************************************
  !>@brief Appel des diverse fonctions de réinitialisation des variables
  subroutine raz
    call options_raz
    call spectre_raz
    call ajustement_raz
    call variablesAjustables_raz
  end subroutine raz
end program mosfit
