module ecriture
!**********************************************************************
!        module ECRITURE
!        Gestion des entrees du code : lecture du fichier
!     ..................................................................
  use precision
  use options
  implicit none
  integer::NOUT=6  ! label du fichier de sortie (sortie par defaut)
  character(len=255),private::fichier_sortie !nom du fichier de sortie
  
  contains
  !=====================================================================
  subroutine ecriture_nommer_fichier_de_sortie(nom)
    character(len=*)::nom
    if( len( trim( nom ) )>255) stop "ERREUR dans ecriture_nommer_fichier_de_sortie  : nom de fichier trop long"
    fichier_sortie = nom
    open(NOUT,file=trim(fichier_sortie), status='unknown', form='formatted')
    write(NOUT,'(A)') ' VERSION MAI 2016 '
  end subroutine ecriture_nommer_fichier_de_sortie
  !=====================================================================
  subroutine ecriture_titre(option)
    integer::option
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
  subroutine ecriture_options(cn,nmax,ns,ns1,ns2)
  ! Ecriture des options precedement lues 
  ! L'ecriture se fait dans le fichier_sortie
  ! Le fichier est effacé avant le debut de l'ecriture.
    real(dp),intent(in)::cn
    integer,intent(in)::nmax,ns,ns1,ns2
    integer::i
    if(IZZ==1) write(NOUT,*)' CANAUX SUPPRIMES = ',(IZ(i),i=1,10)
    if(IOPT==1) write(NOUT,'(1X,A,20(11X,I1))') ' OPTIONS UTILISEES = ',(IO(i),i=1,20)
    write(NOUT,'(1X,A,3X,F15.8,A,I4)') 'VITESSE PAR CANAL=', cn, ' NBRE DE COMPOSANTES =',ns
    write(NOUT,*) 'NBRE MAX ITERATIO=', nmax
    if(IO(4)/=0) write(NOUT,*) 'IL Y A UN SPECTRE DE BRUIT'
    if(ns1/=0) write(NOUT,*) 'DISTRIBUTION ENTRE NS1=',ns1,' ET NS2 =',ns2
    if(ns>40) write(6,*) '  NOMBRE DE SOUS SPECTRES(NS) SUPERIEUR A 40 '
  end subroutine ecriture_options
  !=====================================================================
  subroutine ecriture_param( di,ga,h1,sq,ch,eta,teta,gama,beta,alfa,monoc,nb)
  ! Ecriture des valeurs des parametres hyperfins dans fichier_sortie.
  ! Le fichier n'est pas effacé, les données sont ajoutées à la fin.
    integer,intent(in)::monoc
    real(dp),intent(in)::di,ga,h1,sq,ch,eta,teta,gama,beta,alfa
    integer,intent(in)::nb(10)
    integer::i
    integer,save::cpt=0
    if(cpt==0) write(NOUT,'(A)')  '   DI          GA          H1        SQ        &
               &CH       ETA      TETA      GAMA      BETA      ALFA    MONOC '
    write(NOUT, '(2(F8.3,2X),F11.2,7(2X,F8.3),3X,I4)' ) di,ga,h1,sq,ch,eta,teta,gama,beta,alfa,monoc
    write(NOUT,'(10(5X,I3,2X))')(nb(i), i=1,10)
    cpt=cpt+1
  end subroutine ecriture_param
  !=====================================================================
  subroutine ecriture_info_iteration(npas,nmax,b)
    integer,intent(in)::npas 
    integer,intent(in)::nmax
    real(dp),intent(in)::b(:)
    integer::i,k
    k=size(b)
    write(NOUT,'(1X,A,I6)') '  NUMERO DU PASSAGE ', npas
    write(NOUT,'(1X,A , 8(2X,E13.5))')' B CALC ', (b(I),i=1,k)
    if(npas==nmax) write(NOUT,'(1X, A )') ' COUPURE   PAR    NMAX '
  end subroutine ecriture_info_iteration
  !=====================================================================
  subroutine ecriture_fonction_independante(i)
    integer,intent(in)::i
    write (NOUT,'(1X,A,I4)') ' LA FONCTION EST INDEPENDANTE DU PARAMETRE NO', i
  end subroutine ecriture_fonction_independante
  !=====================================================================
  subroutine ecriture_ecart_type(ns,bt,etbt,gvt,etgvt,iogvt)
  ! ecriture des ecarts type de chaque sous-spectre
    integer,intent(in)::ns
    real(dp),intent(in)::bt(10,40)
    real(dp),intent(in)::etbt(10,40)
    real(dp),intent(in)::gvt(8,40)
    real(dp),intent(in)::etgvt(8,40)
    integer,intent(in)::iogvt(40)
    integer::i,nt
    write(NOUT,'(//,50X,A,//)') 'CARACTERISTIQUES DES SPECTRES'
    write(NOUT,'(1X,20X,"DI",9X,"GA",9X,"H1",9X," SQ",9X,"CH",9X," ETA",9X,"TETA",9X,"GAMA",9X,"BETA",9X,"ALFA",/)')
    write(NOUT,'(1X,20X,"MMS",8X,"MMS",8X,"COUPS",6X,"MMS",9X,"KG",25X,"DEG",23X,"DEG",8X,///)')
    do nt=1,ns
      write(NOUT,'(//," SPECTRE ",I2,2X,10F12.3,/)') nt,(bt(i,nt),i=1,10)
      write(NOUT,'(" ECART TYPE  ",10E12.3,//)') (etbt(i,nt),i=1,10)
      if(iogvt(nt)/=0)then
        write(NOUT,'(21X,"GVT(1)",8X,"GVT(2)",8X,"GVT(3)",8X,"GVT(4)",8X,"GVT(5) ",8X,"GVT(6)",8X,"GVT(7)",8X,"GVT(8)",/)'      ) 
        write(NOUT,'(13X,8(8x,F7.4),/)') (gvt(i,nt),i=1,8)
        write(NOUT,'(" ECART TYPE  ",8(2X,E12.3),//)') (etgvt(i,nt),i=1,8)
      endif
    enddo
  end subroutine ecriture_ecart_type
  !=====================================================================
  subroutine ecriture_raies_covariance(ns,x0,g,h)
  ! ecriture des caractrristiques des raies (largeur, hauteur, energie)
  ! et de la matrice de variance/corvariance
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
  subroutine ecriture_lissage(ns,s,sl)
  ! Calcul des rapports d'absoption/dispersion (surfaces) entre le spectre expérimental et le spectre fitté
    integer,intent(in)::ns
    real(dp),intent(in)::s(44)
    real(dp),intent(in)::sl(42)
    real(dp)::ordS,ordT
    integer::i,ic,colMax,colonneS,colonneT
    character::chaine(256)
    ! Lissage de la distribution
    if(IO(14)==0)then
      write(NOUT,'(30X,"LISSAGE",19X,"%",22X,"%LISSE",/)')
      do i=1,ns+2
        write(NOUT,'(30X,2(20X,F6.2),/)') s(i+1),sl(i)
      enddo
    else
      write(NOUT,'(9X,"LISSAGE",5X,"%",8X,"%LISSE",20X,"DIAGRAMME  EN  CARTOUCHES",//)')  
      write(NOUT,'(56X,"0",19X,"5",18X,"10%",18X,"15%",18X,"20%",18X,"25%",18X,"30%")')
      !tracé de la distribution en ASCII-art...
      do i=1,ns+2
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
        write(NOUT,'(8X,2(6X,F6.2),4X,A1,256A1)') s(i+1), sl(i), (chaine(ic),ic=1,colMax)
        write(NOUT,'(36X,A1,256A1)') (chaine(ic),ic=1,colMax)
      enddo
    endif
  end subroutine ecriture_lissage
  !=====================================================================
  subroutine ecriture_absorption_dispersion_contributions(ns,k,hbruit,daExp,daFit,sExp,sFit,sBruit,b,s,sInt)
  !ecriture des absoprtions/dispersions( surfaces relatives des spectres)
    integer,intent(in)::ns
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
    do nt=1,ns
      write(NOUT,'(30X," SPECTRE ",I2,10X,F6.2," %","  TOTAL ",10X,F6.2,/)')nt, s(nt+2),sInt(nt)
    enddo
  end subroutine ecriture_absorption_dispersion_contributions
  !=====================================================================
  subroutine ecriture_moyennes(nss,btmoy,dash)
    integer,intent(in)::nss
    real(dp),intent(in)::btmoy(7,2)
    character,intent(in)::dash(1)
    integer::i
      !Moyennes arithmétiques et quadratiques
      write(NOUT,'(//,A,"CALCUL SUR LES ",I3," PREMIERS SPECTRES",//)')dash,nss
      write(NOUT,'(A,20X,"DI",9X,"GA",9X,"H1",9X," SQ",9X,"CH",9X," ETA",9X,&
                                &"TETA",9X,"GAMA",9X,"BETA",9X,"ALFA",/)')dash
      write(NOUT,'(//,A,"MOYENNE",6X,7F12.3,//)')dash,(btmoy(i,1),i=1,7)
      write(NOUT,'(//,A,"QUADRATIQUE",2X,7F12.3,//)')dash,(btmoy(i,2),i=1,7)
  end subroutine ecriture_moyennes
  !=====================================================================
  subroutine ecriture_ecart_stat(khi2)
    real(dp),intent(in)::khi2
    write(NOUT,'("1",//,50X,"KHI2 = ",E15.8)') khi2
  end subroutine ecriture_ecart_stat
  !=====================================================================
  subroutine ecriture_tracer_spectres(n,spectre_exp,spectre_fit,cMin,cMax)
  ! tracé des deux spectres experimentaux dans un fichier texte, parceque les interfaces graphiques c'est pour les faibles.
    integer,intent(in)::n
    real(dp),intent(in)::spectre_exp(n)
    real(dp),intent(in)::spectre_fit(n)
    real(dp),intent(out)::cmin,cmax
    character::chaine(120)
    integer::i,ic,colExp,colFit,colMax
    real(dp)::echelle,ordExp,ordFit
    cMin=min(minval(spectre_exp),minval(spectre_fit))
    cMax=max(maxval(spectre_exp),maxval(spectre_fit))
    write(6,'(1X,/," MAX = ",F10.0,"     MIN = ",F10.0)') cmax,cmin
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
  subroutine ecriture_spectre_entier(spectre)
    !ecriture des valeurs d'un spectre sous forme d'entiers, par groupe de 8 canaux
    real(dp),intent(in)::spectre(:)
    integer::i,j,n
    n=size(spectre,1)
    do i=1,n,8
      write(NOUT,'(I4,8I8)') i,(int(spectre(j)),j=i,i+7)
    enddo
  end subroutine ecriture_spectre_entier
  !=====================================================================
  subroutine ecriture_pour_gnuplot(n,nts,cn,spectreExp,spectreFit,totalSousSpectres,nom)
  ! Ecriture des spectres experimentaux, calculé et des sous-spectres
    integer,intent(in)::n
    integer,intent(in)::nts
    real(dp),intent(in)::cn !vitesse par canaux
    real(dp),intent(in)::spectreExp(n) !spectre experimental
    real(dp),intent(in)::spectreFit(n) !spectre theorique
    real(dp),intent(in)::totalSousSpectres(n,5)! sous-spectres choisis
    real(dp)::tout(n,8) ! les 4 variables ci-dessus dans un seul tableau
    character(len=*)::nom
    integer::i,j,NoutSave
    NoutSave=NOUT
    NOUT=42
    ! vitesse de la source
    do i=1,n/2
      tout(i,1)=(i-(n/2+1))*cn
      tout(n/2+i,1)=i*cn
    enddo
    ! spectre theorique
    tout(:,2)=spectreFit
    ! spectre experimental
    tout(:,3)=spectreExp
    ! sous-spectres theoriques
    if(IO(17)/=0)then
      do i=1,nts
        tout(:,i+3)=totalSousSpectres(:,i)
      enddo
    endif
    ! ecriture dans le fichier 
    open(NOUT,file=trim(nom), status='unknown', form='formatted')
    do i=1,n
      write(NOUT,'(2X,F6.2,7(1X,F12.2))') (tout(i,j),j=1,nts+3)
    enddo
    close(NOUT)
    NOUT=NoutSave
  end subroutine ecriture_pour_gnuplot
  !=====================================================================
  subroutine ecriture_resultats_resume(ns,nss,s,sl,bt,btmoy,nom)
  ! ecriture resumee des resultats
    integer,intent(in)::ns
    integer,intent(in)::nss
    real(dp),intent(in)::s(44)
    real(dp),intent(in)::sl(42)
    real(dp),intent(in)::bt(10,40)
    real(dp),intent(in)::btmoy(7,2)
    character(len=*)::nom
    integer::i,nt,NoutSave
    NoutSave=NOUT
    NOUT=42
    open(NOUT,file=trim(nom), status='unknown', form='formatted')
    call ecriture_titre(0)
    write(NOUT,'(//,"#",50X,"CARACTERISTIQUES DES SPECTRES",//)') 
    write(NOUT,'("#",1X,13X,"DI",7X,"GA",5X," SQ",8X,"CH",7X," ETA",8X,"TETA",7X,"GAMA",7X,"BETA",7X,"ALFA",9x,"TAUX",/)')
    write(NOUT,'("#",1X,13X,"MMS",5X,"MMS",5X,"MMS",8X,"KG",25X,"DEG",19X,"DEG",16X," % ",/)')
    do nt=1,ns
      write(NOUT,'(/,"#"," SPECTRE ",I2,2X,F6.3,2X,F5.2,2X,F6.3,2X,F8.2,5(2X,F9.2),8X,F6.2)') &
                                                                            & nt,bt(1,nt),bt(2,nt),(bt(i,nt),i=4,10),s(nt+2)
    enddo
    do i=1,ns+2
      write(NOUT,*)i,sl(i)
    enddo
    if (IO(13)/=0)call ecriture_moyennes(nss,btmoy,'#')
    close(NOUT)
    NOUT=NoutSave
  end subroutine ecriture_resultats_resume
  !=====================================================================
  subroutine ecriture_fin
    write(NOUT,'("1")')
    close(NOUT)
  end subroutine ecriture_fin
end module ecriture
