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
    do nt=1,NS
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
  subroutine ecriture_raies_covariance
    
  subroutine ecriture_raies_covariance
  !=====================================================================
  subroutine ecriture_fin
    close(NOUT)
  end subroutine ecriture_fin
end module ecriture
