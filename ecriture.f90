!>@file
!***********************************************************************
!        				MODULE ECRITURE
!***********************************************************************
!>@brief   Gestion des sorties du code
!! 
!!@version juin 2016
module ecriture
  use precision
  use options
  implicit none
  integer,save::NOUT=41  ! Label du fichier de sortie (sortie par défaut)
  character(len=255),save,private::fichierOut !< Fichier de sortie complet
  character(len=255),save,private::fichierDat !< Fichier de données des spectres (pour tracé éventuel)
  character(len=255),save,private::fichierDoc !< Fichier de resumé des résultats
  contains
  !=====================================================================
  !>@brief Création du fichier fit.out 
  !!@details Le fichier est effacé avant le début de l'ecriture.
  subroutine ecriture_nommer_fichier_de_sortie(nom)
    character(len=*)::nom
    integer::taille
    taille=len_trim(nom)
    if( taille>255) stop "ERREUR dans ecriture_nommer_fichier_de_sortie  : nom de fichier trop long"
    if(IO(18)==1)then
      fichierOut=nom
      fichierDoc=nom
      fichierDat=nom
      fichierOut(taille-3:taille)=".out"
      fichierDat(taille-3:taille)=".dat"
      fichierDoc(taille-3:taille)=".doc"
    else
      fichierOut="fit.out"
      fichierDoc="RESULTAT.doc"
      fichierDat="Spect.dat"
    endif
    write(6,*) fichierOut
    open(NOUT,file=trim(fichierOut), status='unknown', form='formatted')
    write(NOUT,'(A)') ' VERSION 2.1, Juin 2016 '
  end subroutine ecriture_nommer_fichier_de_sortie
  !=====================================================================
  !>@brief Ecriture des titres et sous-titres
  subroutine ecriture_titre(option)
    integer::option !< Choix du titre ou sous-titre à écrire
    select case(option)
      case(0)
        write(NOUT,'(A)') TITRE
      case(1)
        write(NOUT,'(A)')'1'
      case(2)
        write(NOUT,'(A,A,///)')'1','   COMPOSANTE  DONNEE  '
      case default
        stop "Valeur inconnue pour l'option dans ecriture_titre"
    end select
  end subroutine ecriture_titre
  !=====================================================================
  !>@brief Ecriture des options précédemment lues
  !!@details L'écriture se fait dans le fichier de sortie (unité NOUT précédemment ouverte)
  subroutine ecriture_options(cn,nmax,ns,ns1,ns2,plage)
    real(dp),intent(in)::cn
    integer,intent(in)::nmax,ns,ns1,ns2
    integer,intent(in)::plage(2)
    integer::i
    if(IZZ==1) write(NOUT,*)' CANAUX SUPPRIMES = ',(IZ(i),i=1,10)
    if(IOPT==1) write(NOUT,'(1X,A,20(11X,I1))') ' OPTIONS UTILISEES = ',(IO(i),i=1,20)
    write(NOUT,'(1X,A,3X,F15.8,A,I4)') 'VITESSE PAR CANAL=', cn, ' NBRE DE COMPOSANTES =',ns
    write(NOUT,*) 'NBRE MAX ITERATIO=', nmax
    if(IO(4)/=0) write(NOUT,*) 'IL Y A UN SPECTRE DE BRUIT'
    if(ns1/=0) write(NOUT,*) 'DISTRIBUTION ENTRE NS1=',ns1,' ET NS2 =',ns2
    if(IO(13)==1)write(NOUT,*) 'LISSAGE DE TOUS LES SPECTRES'
    if(IO(13)==2)write(NOUT,*) 'LISSAGE DE LA DISTRIBUTION'
    if(IO(13)==3)write(NOUT,*) 'LISSAGE DU SPECTRE ', plage(1) ,'AU SPECTRE ', plage(2)
    if(ns>40) write(NOUT,*) '  NOMBRE DE SOUS SPECTRES(NS) SUPERIEUR A 40 '
  end subroutine ecriture_options
  !=====================================================================
  !>@brief Ecriture des valeurs des paramètres hyperfins dans le fichier de sortie
  !!@details Le fichier n'est pas effacé, les données sont ajoutées à la fin.
  subroutine ecriture_param( di,ga,h1,sq,ch,eta,teta,gama,beta,alfa,monoc,nb,iogv,gv,ng)
    integer,intent(in)::monoc
    real(dp),intent(in)::di,ga,h1,sq,ch,eta,teta,gama,beta,alfa
    integer,intent(in)::nb(10)
    integer,intent(in)::iogv
    real(dp),intent(in)::gv(8)
    integer,intent(in)::ng(8)
    integer::i
    integer,save::cpt=0
    if(cpt==0)then
      if(IO(15)==0)then
          write(NOUT,'(A)')  '   DI          GA          H1        SQ        &
               &CH       ETA      TETA      GAMA      BETA      ALFA    MONOC '
      else
          write(NOUT,'(A)')  '   DI          GA          H1        SQ        &
               &CH       ETA      TETA       WM      BETA      ALFA    MONOC '
      endif
    endif
    write(NOUT, '(2(F8.3,2X),F11.2,7(2X,F8.3),3X,I4)' ) di,ga,h1,sq,ch,eta,teta,gama,beta,alfa,monoc
    write(NOUT,'(10(5X,I3,2X))')(nb(i), i=1,10)
    if(iogv==3)then
      write(NOUT,'(3X,"LARGEUR DE RAIES CHOIX UTILISATEUR")')
      write(NOUT,'(8F8.3)')(gv(i),i=1,8)
      write(NOUT,'(8(4X,I4))')(ng(i),i=1,8)
    endif
    cpt=cpt+1
  end subroutine ecriture_param
  !=====================================================================
  !>@brief Ecriture d'infos sur l'itération en cours dans l'algorithme des moindres carrés
  subroutine ecriture_info_iteration(npas,nmax,b)
    integer,intent(in)::npas !< Numéro de l'itération
    integer,intent(in)::nmax !< Nombre maximum d'itérations
    real(dp),intent(in)::b(:) !< Paramètres ajustés
    integer::i,k
    k=size(b)
    write(NOUT,'(1X,A,I6)') '  NUMERO DU PASSAGE ', npas
    write(NOUT,'(1X,A , 8(2X,E13.5))')' B CALC ', (b(I),i=1,k)
    if(npas==nmax) write(NOUT,'(1X, A )') ' COUPURE   PAR    NMAX '
  end subroutine ecriture_info_iteration
  !=====================================================================
  !>@brief Message d'erreur 
  subroutine ecriture_fonction_independante(i)
    integer,intent(in)::i
    write (NOUT,'(1X,A,I4)') ' LA FONCTION EST INDEPENDANTE DU PARAMETRE NO', i
  end subroutine ecriture_fonction_independante
  !=====================================================================
  !>@brief Ecriture des ecarts type de chaque sous-spectre
  subroutine ecriture_ecart_type(ns,bt,etbt,gvt,etgvt,iogvt)
    integer,intent(in)::ns
    real(dp),intent(in)::bt(10,40)
    real(dp),intent(in)::etbt(10,40)
    real(dp),intent(in)::gvt(8,40)
    real(dp),intent(in)::etgvt(8,40)
    integer,intent(in)::iogvt(40)
    integer::i,nt
    write(NOUT,'(//,50X,A,//)') 'CARACTERISTIQUES DES SPECTRES'
    if(IO(15)==0)then
      write(NOUT,'(1X,20X,"DI",9X,"GA",9X,"H1",9X," SQ",9X,"CH",9X," ETA",9X,"TETA",9X,"GAMA",9X,"BETA",9X,"ALFA",/)')
      write(NOUT,'(1X,20X,"MMS",8X,"MMS",8X,"COUPS",6X,"MMS",9X,"KG",25X,"DEG",23X,"DEG",8X,///)')
    else
      write(NOUT,'(1X,20X,"DI",9X,"GA",9X,"H1",9X," SQ",9X,"CH",9X," ETA",11X,"WM",9X,"BETA",9X,"ALFA",/)')
      write(NOUT,'(1X,20X,"MMS",8X,"MMS",8X,"COUPS",6X,"MMS",9X,"KG",11X,"SANS DIMENSION",15X,"DEG",8X,///)')
    endif
    do nt=1,ns
      if(IO(15)==0)then
        write(NOUT,'(//," SPECTRE ",I2,2X,10F12.3,/)') nt,(bt(i,nt),i=1,10)
        write(NOUT,'(" ECART TYPE  ",10E12.3,//)') (etbt(i,nt),i=1,10)
      else
        write(NOUT,'(//," SPECTRE ",I2,2X,6F12.3,F12.3,2F12.3,/)') nt,(bt(i,nt),i=1,6),(bt(i,nt),i=8,10)
        write(NOUT,'(" ECART TYPE  ",9E12.3,//)') (etbt(i,nt),i=1,6),(etbt(i,nt),i=8,10)
      endif
      if(iogvt(nt)/=0)then
        if(IO(15)==0)then
          write(NOUT,'(21X,"GVT(1)",8X,"GVT(2)",8X,"GVT(3)",8X,"GVT(4)",8X,"GVT(5) ",8X,"GVT(6)",8X,"GVT(7)",8X,"GVT(8)",/)'      ) 
          write(NOUT,'(13X,8(8x,F7.4),/)') (gvt(i,nt),i=1,8)
          write(NOUT,'(" ECART TYPE  ",8(2X,E12.3),//)') (etgvt(i,nt),i=1,8)
        else
          write(NOUT,'(21X,"GVT(1)",8X,"GVT(2)",8X,"GVT(3)",8X,"GVT(4)",8X,"GVT(5) ",8X,"GVT(7)",8X,"GVT(8)",/)'      ) 
          write(NOUT,'(13X,8(8x,F7.4),/)') (gvt(i,nt),i=1,6),gvt(8,nt)
          write(NOUT,'(" ECART TYPE  ",8(2X,E12.3),//)') (etgvt(i,nt),i=1,5),etgvt(8,nt)
        endif
      endif
    enddo
  end subroutine ecriture_ecart_type
  !=====================================================================
  !>@brief Ecriture des caractéristiques des raies (largeur, hauteur, energie)
  subroutine ecriture_raies_covariance(ns,x0,g,h)
    integer,intent(in)::ns
    real(dp),intent(in)::x0(8,40)
    real(dp),intent(in)::g(8,40)
    real(dp),intent(in)::h(8,40)
!~     real(dp),intent(in)::vq(40,40)
    integer::nt,l
    write(NOUT,'(/,42X,"X0",9X,"G",10X,"H",5X,"(CANAUX)",/)')
    do nt=1,ns
      do l=1,8
        write(NOUT,'(1X,39X,F6.2,5X,F6.2,5X,F7.0,5X,F10.0,/)') X0(l,nt), G(l,nt), H(l,nt)
      enddo
!~       write(NOUT,'(//,30X,"MATRICE DE VARIANCE COVARIANCE",//)')
!~       do i=1,K
!~         write(NOUT,'(8X,10(2X,E10.3),//)') (VQ(i,j),j=1,K)
!~       enddo
    enddo
  end subroutine ecriture_raies_covariance
  !=====================================================================
  !>@brief Calcul des rapports d'absoption/dispersion (surfaces) entre le spectre expérimental et le spectre fitté
  subroutine ecriture_lissage(nsmin,nsmax,s,sl,champ)
    integer,intent(in)::nsmin
    integer,intent(in)::nsmax
    real(dp),intent(in)::s(44)
    real(dp),intent(in)::sl(42)
    real(dp),intent(in)::champ(44)
    real(dp)::ordS,ordT
    integer::i,ic,colMax,colonneS,colonneT
    character::chaine(256)
    ! Lissage de la distribution
    if(IO(14)==0)then
      write(NOUT,'(10X,"LISSAGE",16X,"%",22X,"%LISSE",22X,"CH",22X,/)')
      do i=nsmin,nsmax+2
        write(NOUT,'(10X,3(20X,F6.2),/)') s(i+1),sl(i), champ(i+1)
      enddo
    else
      write(NOUT,'(X,"SPECTRE",5X,"%",4X,"%LISSE",4X,"CH",21X,"DIAGRAMME  EN  CARTOUCHES",//)')  
      write(NOUT,'(52X,"0",19X,"5",18X,"10%",17X,"15%",17X,"20%",17X,"25%",17X,"30%")')
      ! Tracé de la distribution en ASCII-art...
      do i=nsmin,nsmax+2
        ordS=20.0_dp+4.0_dp*sl(i)
        ordT=20.0_dp+4.0_dp*s(i+1)
        colonneS=1+int(ordS)
        colonneT=1+int(ordT)
        if(colonneS>256) colonneS=21
        if(colonneT>256) colonneT=21
        if(colonneS<-20) colonneS=21
        if(colonneT<-20) colonneT=21
        colMax = max(colonneS,colonneT)
        do ic=1,colMax
          chaine(ic)=' '
        enddo
        chaine(colonneT)='X'
        chaine(colonneS)='*'
        chaine(21)='!'
        if((i>nsmin) .AND. (i<nsmax+2))then
          write(NOUT,'(X,I4,3X,3(2X,F6.2),A1,256A1)')i-1,s(i+1), sl(i), champ(i+1), (chaine(ic),ic=1,colMax)
        else
          write(NOUT,'(8X,3(2X,F6.2),A1,256A1)')s(i+1), sl(i), champ(i+1), (chaine(ic),ic=1,colMax)
        endif
        write(NOUT,'(32X,A1,256A1)') (chaine(ic),ic=1,colMax)
      enddo
    endif
  end subroutine ecriture_lissage
  !=====================================================================
  !>@brief Ecriture des absorptions/dispersions( surfaces relatives des spectres)
  subroutine ecriture_absorption_dispersion_contributions(nsmin,nsmax,k,hbruit,daExp,daFit,sExp,sFit,sBruit,b,s,sInt)
    integer,intent(in)::nsmin!< Premier spectre du lissage
    integer,intent(in)::nsmax!< Dernier spectre du lissage
    integer,intent(in)::k
    real(dp),intent(in)::hbruit
    real(dp),intent(in)::daExp
    real(dp),intent(in)::daFit
    real(dp),intent(in)::sExp
    real(dp),intent(in)::sFit
    real(dp),intent(in)::sBruit
    real(dp),intent(in)::b(40)
    real(dp),intent(in)::s(44)
    real(dp),intent(in)::sInt(40)
    integer::nt
    if(IO(4)==1) write(NOUT,'(//,"  BRUIT DE FOND NON  HBRUIT= ",F10.3,///)') hbruit
    if(IO(4)==2) write(NOUT,'(//,"  BRUIT DE FOND  HBRUIT= ",F10.3,///)') b(k-1)
    ! Surfaces exprimant l'absorption
    write(NOUT,'(//,"  ABS TOT EXP ",F11.0,12X," ABS TOT CALC ",F11.0, " DONT BRUIT ",F11.0,/)') sExp,sFit,sBruit
    write(NOUT,'("  ABSFIT/ABSEXP= ",F10.5,10X," ABSBRUIT/ABSEXP= ",F10.5,/)' ) sFit/sExp, sBruit/sExp
    ! Surfaces exprimant la dispersion
    write(NOUT,'(/,"   SURFACE DISPERSEE/SURFACE ABSORPTION    EXP=",F11.5,1X,"FIT=",F11.5,/)') daExp, daFit
    ! Contribution de chaque sous-spectre (en pourcent)
    do nt=nsmin,nsmax
      write(NOUT,'(30X," SPECTRE ",I2,10X,F6.2," %","  TOTAL ",10X,F6.2,/)')nt, s(nt+2),sInt(nt)
    enddo
  end subroutine ecriture_absorption_dispersion_contributions
  !=====================================================================
  !>@brief Moyennes arithmétiques et quadratiques
  subroutine ecriture_moyennes(nsmin,nsmax,btmoy,dash)
    integer,intent(in)::nsmin!< premier des sous-spectres lissés
    integer,intent(in)::nsmax!< dernier des sous-spectres lissés
    real(dp),intent(in)::btmoy(7,2)
    character,intent(in)::dash(1)
    integer::i
    if(IO(13)==1)then
      write(NOUT,'(//,A,"CALCUL DE MOYENNES SUR LES ",I3," SPECTRES",//)')dash,nsmax-nsmin+1
    else if(IO(13)==2)then 
      write(NOUT,'(//,A,"CALCUL DE MOYENNES SUR LES ",I3," SPECTRES DE LA DISTRIBUTION",//)')dash,nsmax-nsmin+1
    else if(IO(13)==3)then 
      write(NOUT,'(//,A,"CALCUL DE MOYENNES DU SPECTRE ",I3," AU SPECTRE ",I3,//)')dash,nsmin, nsmax
    endif
!~     write(NOUT,'(A,20X,"DI",9X,"GA",9X,"H1",9X," SQ",9X,"CH",9X," ETA",9X,&
!~                 &"TETA",/)')dash
!~     write(NOUT,'(//,A,"MOYENNE",6X,7F12.3,//)')dash,(btmoy(i,1),i=1,7)
!~     write(NOUT,'(//,A,"QUADRATIQUE",2X,7F12.3,//)')dash,(btmoy(i,2),i=1,7)
    write(NOUT,'(A,20X,"DI",9X," SQ",9X,"CH",9X,"TETA",/)')dash
    write(NOUT,'(//,A,"MOYENNE",6X,4F12.3,//)')dash,btmoy(1,1),btmoy(4,1),btmoy(5,1),btmoy(7,1)
    write(NOUT,'(//,A,"QUADRATIQUE",2X,4F12.3,//)')dash,btmoy(1,2),btmoy(4,2),btmoy(5,2),btmoy(7,2)
  end subroutine ecriture_moyennes
  !=====================================================================
  !>@brief Ecriture de l'écart statistique "Khi"
  subroutine ecriture_ecart_stat(khi2)
    real(dp),intent(in)::khi2
    write(NOUT,'("1",//,50X,"KHI2 = ",E15.8)') khi2
  end subroutine ecriture_ecart_stat
  !=====================================================================
  !>@brief Tracé des deux spectres expérimentaux dans un fichier texte, Ascii-art style
  !... Parce que les interfaces graphiques c'est pour les faibles.
  subroutine ecriture_tracer_spectres(n,spectre_exp,spectre_fit,cMin,cMax)
    integer,intent(in)::n
    real(dp),intent(in)::spectre_exp(n) !< Spectre expérimental
    real(dp),intent(in)::spectre_fit(n)	!< Spectre calculé
    real(dp),intent(out)::cmin,cmax
    character::chaine(120)
    integer::i,ic,colExp,colFit,colMax
    real(dp)::echelle,ordExp,ordFit
    cMin=min(minval(spectre_exp),minval(spectre_fit))
    cMax=max(maxval(spectre_exp),maxval(spectre_fit))
    write(NOUT,'(1X,/," MAX = ",F10.0,"     MIN = ",F10.0)') cmax,cmin
    echelle=119.0
    if(IO(2)==1)echelle=107.0
    do i=n,1,-1
      ordExp=(cMax-spectre_exp(i))*echelle/(cMax-cMin)
      ordFit=(cMax-spectre_fit(i))*echelle/(cMax-cMin)
      colExp = 1+int(ordExp)
      colFit = 1+int(ordFit)
      colMax = max(colExp,colFit)
      do ic=1,colMax
        chaine(ic)=' '
      enddo
      chaine(colFit)='X'
      chaine(colExp)='*'
      write(NOUT,'(1X,I4,2X,A1,120A1)')i,(chaine(ic),ic=1,colMax)
    enddo
  end subroutine ecriture_tracer_spectres
  !=====================================================================
  !>@brief Ecriture des valeurs d'un spectre sous forme d'entiers, par groupes de 8 canaux.
  subroutine ecriture_spectre_entier(spectre)
    real(dp),intent(in)::spectre(:)
    integer::i,j,n
    n=size(spectre,1)
    do i=1,n,8
      write(NOUT,'(I4,8I8)') i,(int(spectre(j)),j=i,i+7)
    enddo
  end subroutine ecriture_spectre_entier
  !=====================================================================
  !>@brief Ecriture des spectres (expérimental et calculé), ainsi que des sous-spectres. 
  !>@details L'écriture se fait dans un fichier à part, facilement exploitable par un logiciel tiers (ex: Gnuplot)
  subroutine ecriture_pour_gnuplot(n,nts,cn,spectreExp,spectreFit,totalSousSpectres)
    integer,intent(in)::n
    integer,intent(in)::nts !< Nombre de plages de sous-spectres
    real(dp),intent(in)::cn !< Vitesse par canaux
    real(dp),intent(in)::spectreExp(n) !< Spectre expérimental
    real(dp),intent(in)::spectreFit(n) !< Spectre théorique (calculé)
    real(dp),intent(in)::totalSousSpectres(n,5)!< Sous-spectres
    real(dp)::tout(n,8) ! les 4 variables ci-dessus dans un seul tableau.
    integer::i,j,NoutSave
    NoutSave=NOUT
    NOUT=42
    ! Vitesse de la source
    do i=1,n/2
      tout(i,1)=(i-(n/2+1))*cn
      tout(n/2+i,1)=i*cn
    enddo
    ! Spectre théorique
    tout(:,2)=spectreFit
    ! Spectre expérimental
    tout(:,3)=spectreExp
    ! Sous-spectres théoriques
    if(IO(17)/=0)then
      do i=1,nts
        tout(:,i+3)=totalSousSpectres(:,i)
      enddo
    endif
    ! Ecriture dans le fichier 
    open(NOUT,file=trim(fichierDat), status='unknown', form='formatted')
    do i=1,n
      write(NOUT,'(2X,F6.2,7(1X,F12.2))') (tout(i,j),j=1,nts+3)
    enddo
    close(NOUT)
    NOUT=NoutSave
  end subroutine ecriture_pour_gnuplot
  !=====================================================================
  !>@brief Ecriture d'un resumé des résultats dans le fichier .doc
  subroutine ecriture_resultats_resume(nsmin,nsmax,s,sl,bt,btmoy)
    integer,intent(in)::nsmin !< Nombre de sous-spectres
    integer,intent(in)::nsmax !< Nombre de sous-spectres sommés
    real(dp),intent(in)::s(44)
    real(dp),intent(in)::sl(42)
    real(dp),intent(in)::bt(10,40)
    real(dp),intent(in)::btmoy(7,2)
    integer::i,nt,NoutSave
    NoutSave=NOUT
    NOUT=42
    open(NOUT,file=trim(fichierDoc), status='unknown', form='formatted')
    call ecriture_titre(0)
    write(NOUT,'(//,"#",50X,"CARACTERISTIQUES DES SPECTRES",//)') 
    if(IO(15)==0)then
      write(NOUT,'("#",1X,13X,"DI",7X,"GA",5X," SQ",8X,"CH",7X," ETA",8X,"TETA",7X,"GAMA",7X,"BETA",7X,"ALFA",9x,"TAUX",/)')
      write(NOUT,'("#",1X,13X,"MMS",5X,"MMS",5X,"MMS",8X,"KG",25X,"DEG",19X,"DEG",16X," % ",/)')
      do nt=nsmin,nsmax
        write(NOUT,'(/,"#"," SPECTRE ",I2,2X,F6.3,2X,F5.2,2X,F6.3,2X,F8.2,5(2X,F9.2),8X,F6.2)') &
                                                                              & nt,bt(1,nt),bt(2,nt),(bt(i,nt),i=4,10),s(nt+2)
      enddo
    else
      write(NOUT,'("#",1X,13X,"DI",7X,"GA",5X," SQ",8X,"CH",7X," ETA",8X,"TETA",7X,"WM",7X,"BETA",7X,"ALFA",9x,"TAUX",/)')
      write(NOUT,'("#",1X,13X,"MMS",5X,"MMS",5X,"MMS",8X,"KG",19X,"DEG",8X,"adim",10X,"DEG",16X," % ",/)')
      do nt=nsmin,nsmax
        write(NOUT,'(/,"#"," SPECTRE ",I2,2X,F6.3,2X,F5.2,2X,F6.3,2X,F8.2,5(2X,F9.2),8X,F6.2)') &
                                                                              & nt,bt(1,nt),bt(2,nt),(bt(i,nt),i=4,10),s(nt+2)
      enddo
    endif
    do i=nsmin,nsmax+2
      write(NOUT,*)i,sl(i)
    enddo
    if (IO(13)/=0) call ecriture_moyennes(nsmin,nsmax,btmoy,'#')
    close(NOUT)
    NOUT=NoutSave
  end subroutine ecriture_resultats_resume
  !=====================================================================
  subroutine ecriture_fin
    write(NOUT,'("1")')
    close(NOUT)
  end subroutine ecriture_fin
end module ecriture
