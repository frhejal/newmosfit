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
!~   character(len=1),private::STAR='*'
!~   character(len=1),private::BLANC=' '
!~   character(len=1),private::AX='X'
!~   character(len=1),private::AT='!'
  
  contains
  !=====================================================================
  subroutine ecriture_nommer_fichier_de_sortie(nom)
    character(len=*)::nom
    if( len( trim( nom ) )>255) stop "ERREUR dans ecriture_nommer_fichier_de_sortie  : nom de fichier trop long"
    fichier_sortie = nom
    open(NOUT,file=trim(fichier_sortie), status='unknown', form='formatted')
    write(NOUT,'(A)') ' VERSION MAI 2016 '
!~     close(NOUT)
  end subroutine ecriture_nommer_fichier_de_sortie
  !=====================================================================
  subroutine ecriture_titre(option)
    integer::option
!~     open(NOUT,file=trim(fichier_sortie), status='unknown', form='formatted',access='append')
    if(option==1)write(NOUT,'(A)')'1'
    write(NOUT,'(A)') titre
!~     close(NOUT)
  end subroutine ecriture_titre
  !=====================================================================
  subroutine ecriture_options(cn,nmax,ns,ns1,ns2)
  ! Ecriture des options precedement lues 
  ! L'ecriture se fait dans le fichier_sortie
  ! Le fichier est effacé avant le debut de l'ecriture.
    integer,intent(in)::nmax,ns,ns1,ns2
    real(dp),intent(in)::cn
!~     character(len=*),intent(in)::fichier_sortie
    integer::i
!~     open(NOUT,file=trim(fichier_sortie), status='unknown', form='formatted',access='append')
    if(IZZ==1) print *, ' CANAUX SUPPRIMES = ',(IZ(i),i=1,10)
    if(IOPT==1) print *, ' OPTIONS UTILISEES = ',(IO(i),i=1,20)
    print *, 'VITESSE PAR CANAL=', cn, ' NBRE DE COMPOSANTES =',ns
    print *, 'NBRE MAX ITERATIO=', nmax
    if(IO(4)/=0) print *, 'IL Y A UN SPECTRE DE BRUIT'
    if(ns1/=0) print *, 'DISTRIBUTION ENTRE NS1=',ns1,' ET NS2 =',ns2
    if(ns>40) write(6,*) '  NOMBRE DE SOUS SPECTRES(NS) SUPERIEUR A 40 '
!~     close(NOUT)
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
!~     open(NOUT,file=trim(fichier_sortie), status='unknown', form='formatted',access='append')
    if(cpt==0) write(NOUT,'(A)')  '   DI          GA          H1        SQ        &
               &CH       ETA      TETA      GAMA      BETA      ALFA    MONOC '
    write(NOUT, '(2(F8.3,2X),F11.2,7(2X,F8.3),3X,I4)' ) di,ga,h1,sq,ch,eta,teta,gama,beta,alfa,monoc
    write(NOUT,'(10(5X,I3,2X))')(nb(i), i=1,10)
!~     close(NOUT)
    cpt=cpt+1
  end subroutine ecriture_param
  !=====================================================================
  subroutine ecriture_bruit
!~     open(NOUT,file=trim(fichier_sortie), status='unknown', form='formatted',access='append')
    write(NOUT,'(A,A,///)')'1','   COMPOSANTE  DONNEE  '
!~     close(NOUT)
  end subroutine ecriture_bruit
  !=====================================================================
  subroutine ecriture_spectre(spectre)
    real(dp),intent(in)::spectre(:)
    integer::i,j,n
    N=size(spectre)
!~     open(NOUT,file=trim(fichier_sortie), status='unknown', form='formatted',access='append')
    do i=1,n,8
      write(NOUT,'(I4,8I8)')  I, (int(spectre(j)),j=i,i+7)
    enddo
!~     close(NOUT)
  end subroutine ecriture_spectre
  !=====================================================================
  subroutine ecriture_info_iteration(npas,nmax,b)
    integer,intent(in)::npas 
    integer,intent(in)::nmax
    real(dp),intent(in)::b(:)
    integer::i,k
    k=size(b)
!~     open(NOUT,file=trim(fichier_sortie), status='unknown', form='formatted',access='append')
    write(NOUT,'(1X,A,I6)') '  NUMERO DU PASSAGE ', npas
    write(NOUT,'(1X,A , 8(2X,E13.5))')' B CALC ', (b(I),i=1,k)
    if(npas==nmax) write(NOUT,'(1X, A )') ' COUPURE   PAR    NMAX '
!~     close(NOUT)
  end subroutine ecriture_info_iteration
  !=====================================================================
  subroutine ecriture_fonction_independante(i)
    integer,intent(in)::i
!~     open(NOUT,file=trim(fichier_sortie), status='unknown', form='formatted',access='append')
    write (NOUT,'(1X,A,I4)') ' LA FONCTION EST INDEPENDANTE DU PARAMETRE NO', i
!~     close(NOUT)
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
        write(NOUT,'(13X,8(8x,F6.4),/)') (gvt(i,nt),i=1,8)
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
  subroutine ecriture_rapports_absorption(ns,ns2,k,n,b,bt,spectre_exp,spectre_fit,spectre_bruit,nivzero,hbruit)
  ! Calcul des rapports d'absoption/dispersion (surfaces) entre le spectre expérimental et le spectre fitté
    integer,intent(in)::ns
    integer,intent(in)::ns2
    integer,intent(in)::k
    integer,intent(in)::n
    real(dp),intent(in)::b(40)
    real(dp),intent(in)::bt(10,40)
    real(dp),intent(in)::spectre_exp(n)
    real(dp),intent(in)::spectre_fit(n)
    real(dp),intent(in)::spectre_bruit(n)
    real(dp),intent(in)::nivzero
    real(dp),intent(inout)::hbruit
    real(dp)::st,sExp,sFit,sBruit,sInt, difExp, difFit, daExp,daFit
    real(dp)::ordS,ordT
    real(dp)::s(40),sl(42),t(44)
    real(dp)::btm(7,2)
    integer::i,ic,nt,cmax,colonneS,colonneT,nss
    character::chaine(256)
    s=0.0_dp
    st=0.0_dp
    do nt=1,ns
      s(nt)=abs(BT(2,nt)*BT(3,nt))
      st=st+s(nt)
    enddo
    if(io(4)==1) write(NOUT,'(//,"  BRUIT DE FOND NON  HBRUIT= ",F10.3,///)') hbruit
    if(io(4)==2) then
      hbruit=b(k-1)
      write(NOUT,'(//,"  BRUIT DE FOND  HBRUIT= ",F10.3,///)') hbruit
    endif
    ! Surfaces exprimant l'absorption
    sExp=1.0_dp
    sFit=0.0_dp
    sBruit=1.0_dp
    do i=1,n
      sExp=sExp + b(k) - spectre_exp(i)
      sFit=sFit + b(k) - spectre_fit(i)
      if(hbruit/=0) sBruit =sBruit-hbruit*spectre_bruit(i)
    enddo
    write(NOUT,'(//,"  ABS TOT EXP ",F11.0,12X," ABS TOT CALC ",F11.0, " DONT BRUIT ",F11.0,/)') sExp,sFit,sBruit
    write(NOUT,'("  ABSFIT/ABSEXP= ",F10.5,10X," ABSBRUIT/ABSEXP= ",F10.5,/)' ) sFit/sExp, sBruit/sExp
    ! Surfaces exprimant la dispersion
    difFit=0.0_dp
    difExp=1.0_dp
    do i=1,n
      difFit = difFit +(spectre_exp(i)-spectre_fit(i))**2 
    enddo
    do i=3,12
      difExp=difExp+(nivzero-spectre_exp(i))**2
    enddo
    do i=n-9,n
      difExp=difExp+(nivzero-spectre_exp(i))**2
    enddo
    ! Ratio dispersion/absorption
    daFit= n*sqrt(difFit/n)/sFit
    daExp= n*sqrt(difExp/20.0_dp)/sExp
    write(NOUT,'(/,"   SURFACE DISPERSEE/SURFACE ABSORPTION    EXP=",F11.5,1X,"FIT=",F11.5,/)') daExp, daFit
    ! Contribution de chaque sous-spectre (en pourcent)
    sInt=0.0_dp
    s=100.0_dp*s/st
    t(3:ns+2)=s(:)
    do nt=1,ns
      sInt=sInt+s(nt)
      write(NOUT,'(30X," SPECTRE ",I2,10X,F6.2," %","  TOTAL ",10X,F6.2,/)')nt, s(nt),sInt
    enddo
    ! Lissage de la distribution
    if (IO(13)/=0)then
      t(1:2)=0.0_dp
      t(ns+3:ns+4)=0.0_dp
      do i=1,ns+2
        sl(i)=0.25_dp*(t(i)+2.0_dp*t(i+1)+t(i+2))
      enddo
      if(io(14)==0)then
        write(NOUT,'(30X,"LISSAGE",19X,"%",22X,"%LISSE",/)')
        do i=1,ns+2
          write(NOUT,'(30X,2(20X,F6.2),/)') t(i+1),sl(i)
        enddo
      else
        write(NOUT,'(9X,"LISSAGE",5X,"%",8X,"%LISSE",20X,"DIAGRAMME  EN  CARTOUCHES",//)')  
        write(NOUT,'(56X,"0",19X,"5",18X,"10%",18X,"15%",18X,"20%",18X,"25%",18X,"30%")')
        !tracé de la distribution en ASCII-art...
        do i=1,ns+2
          ordS=20.0_dp+4.0_dp*sl(i)
          ordT=20.0_dp+4.0_dp*t(i+1)
          colonneS=1+int(ordS)
          colonneT=1+int(ordT)
          if(colonneS>256) colonneS=21
          if(colonneT>256) colonneT=21
          if(colonneS<-20) colonneS=21
          if(colonneT<-20) colonneT=21
          cMax = max(colonneS,colonneT)
          do ic=1,cmax
            chaine(ic)=' '
          enddo
          chaine(colonneT)='X'
          chaine(colonneS)='*'
          chaine(21)='!'
          write(NOUT,'(8X,2(6X,F6.2),4X,A1,256A1)') t(i+1), sl(i), (chaine(ic),ic=1,cmax)
          write(NOUT,'(36X,A1,256A1)') (chaine(ic),ic=1,cmax)
        enddo
      endif
      nss=ns
      btm=0.0_dp
      if(ns2/=0)nss=ns2
      do i=1,7
        select case(i)
          case(1,4,5,7)
            do nt=1,nss
              btm(i,1)=btm(i,1)+bt(i,nt)*s(nt)/sInt
              btm(i,2)=btm(i,2)+(bt(i,nt)**2)*s(nt)/sInt
            enddo
        end select
      enddo
      write(NOUT,'(//," CALCUL SUR LES ",I3," PREMIERS SPECTRES",//)')nss
      write(NOUT,'(1X,20X,"DI",9X,"GA",9X,"H1",9X," SQ",9X,"CH",9X," ETA",9X,&
                                &"TETA",9X,"GAMA",9X,"BETA",9X,"ALFA",/)')
      write(NOUT,'(//," MOYENNE",6X,7F12.3,//)')(BTM(i,1),i=1,7)
      write(NOUT,'(//," QUADRATIQUE",2X,7F12.3,//)')(BTM(i,2),i=1,7)
    endif
  end subroutine ecriture_rapports_absorption
  !=====================================================================
  subroutine ecriture_fin
    close(NOUT)
  end subroutine ecriture_fin
end module ecriture
!~    20 FORMAT(//,50X,'CARACTERISTIQUES DES SPECTRES',//)                
!~  25   FORMAT(//,'  ABS TOT EXP ',F11.0,12X,' ABS TOT CALC ',F11.0,     
!~      1 ' DONT BRUIT ',F11.0,/)
!~  26   FORMAT('  ABSFIT/ABSEXP= ',F10.5,10X,' ABSBRUIT/ABSEXP= ',F10.5,/)
!~    28 FORMAT(30X,' SPECTRE ',I2,10X,F6.2,' %','  TOTAL ',10X,F6.2,/)   
!~    31 FORMAT (1X,20X,'MMS',8X,'MMS',8X,'COUPS',6X,'MMS',9X,'KG',25X,   
!~      1'DEG',23X,'DEG',8X,///)                                          
!~    30 FORMAT(//,' SPECTRE ',I2,2X,10F12.3,/)                           
!~    22 FORMAT(1X,39X,F6.2,5X,F6.2,5X,F7.0,5X,F10.0,/)                     
!~    27 FORMAT(/,'   SURFACE DISPERSEE/SURFACE ABSORPTION    EXP=',F11.5,1
!~      *X,'FIT=',F11.5,/)                                                
!~    21 FORMAT(/,42X,'X0',9X,'G',10X,'H',5X,'(CANAUX)',/)                
!~  9    FORMAT(//,'  BRUIT DE FOND NON  HBRUIT= ',F10.3,///)             
!~  10   FORMAT(//,'  BRUIT DE FOND  HBRUIT= ',F10.3,///)                 
!~    29 FORMAT (1X,20X,'DI',9X,'GA',9X,'H1',9X,' SQ',9X,'CH',9X,' ETA',9X,
