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
  !-----------------------------------------------------------------------
  subroutine ecriture_nommer_fichier_de_sortie(nom)
    character(len=*)::nom
    if( len( trim( nom ) )>255) stop "ERREUR dans ecriture_nommer_fichier_de_sortie  : nom de fichier trop long"
    fichier_sortie = nom
    open(NOUT,file=trim(fichier_sortie), status='unknown', form='formatted')
    write(NOUT,*) ' VERSION MAI 2016 '
    close(NOUT)
  end subroutine ecriture_nommer_fichier_de_sortie
  !-----------------------------------------------------------------------
  subroutine ecriture_titre
    open(NOUT,file=trim(fichier_sortie), status='unknown', form='formatted',access='append')
    write(NOUT,*) titre
    close(NOUT)
  end subroutine ecriture_titre
  !-----------------------------------------------------------------------
  subroutine ecriture_options(cn,nmax,ns,ns1,ns2)
  ! Ecriture des options precedement lues 
  ! L'ecriture se fait dans le fichier_sortie
  ! Le fichier est effacé avant le debut de l'ecriture.
    integer,intent(in)::nmax,ns,ns1,ns2
    real(dp),intent(in)::cn
!~     character(len=*),intent(in)::fichier_sortie
    integer::i
    open(NOUT,file=trim(fichier_sortie), status='unknown', form='formatted',access='append')
    if(IZZ==1) print *, ' CANAUX SUPPRIMES = ',(IZ(i),i=1,10)
    if(IOPT==1) print *, ' OPTIONS UTILISEES = ',(IO(i),i=1,20)
    print *, 'VITESSE PAR CANAL=', cn, ' NBRE DE COMPOSANTES =',ns
    print *, 'NBRE MAX ITERATIO=', nmax
    if(IO(4)/=0) print *, 'IL Y A UN SPECTRE DE BRUIT'
    if(ns1/=0) print *, 'DISTRIBUTION ENTRE NS1=',ns1,' ET NS2 =',ns2
    if(ns>40) write(6,*) '  NOMBRE DE SOUS SPECTRES(NS) SUPERIEUR A 40 '
    close(NOUT)
  end subroutine ecriture_options
  !---------------------------------------------------------------------
  subroutine ecriture_param( di,ga,h1,sq,ch,eta,teta,gama,beta,alfa,monoc,nb)
  ! Ecriture des valeurs des parametres hyperfins dans fichier_sortie.
  ! Le fichier n'est pas effacé, les données sont ajoutées à la fin.
    integer,intent(in)::monoc
    real(dp),intent(in)::di,ga,h1,sq,ch,eta,teta,gama,beta,alfa
    integer,intent(in)::nb(10)
    integer::i
    integer,save::cpt=0
    open(NOUT,file=trim(fichier_sortie), status='unknown', form='formatted',access='append')
    if(cpt==0) write(NOUT,*)  '   DI          GA          H1        SQ        &
               &CH       ETA      TETA      GAMA      BETA      ALFA    MONOC '
    write(NOUT, '(2(F8.3,2X),F11.2,7(2X,F8.3),3X,I4)' ) di,ga,h1,sq,ch,eta,teta,gama,beta,alfa,monoc
    write(NOUT,'(10(5X,I3,2X))')(nb(i), i=1,10)
    close(NOUT)
    cpt=cpt+1
  end subroutine ecriture_param
  !---------------------------------------------------------------------
  subroutine ecriture_bruit
    open(NOUT,file=trim(fichier_sortie), status='unknown', form='formatted',access='append')
 1  FORMAT('1','   COMPOSANTE  DONNEE  ',///)
    write(NOUT,1)
    close(NOUT)
  end subroutine ecriture_bruit
  !---------------------------------------------------------------------
  subroutine ecriture_spectre(spectre)
    real(dp),intent(in)::spectre(:)
    integer::i,j,n
    N=size(spectre)
    open(NOUT,file=trim(fichier_sortie), status='unknown', form='formatted',access='append')
    do i=1,n,8
      write(NOUT,'(I4,8I8)')  I, (int(spectre(j)),j=i,i+7)
    enddo
    close(NOUT)
  end subroutine ecriture_spectre
  
  
end module ecriture
