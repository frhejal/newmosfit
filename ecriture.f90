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
  end subroutine ecriture_nommer_fichier_de_sortie
  !-----------------------------------------------------------------------
  subroutine ecriture_options(cn,nmax,ns,ns1,ns2,iopt,hbruit)
  ! Ecriture des options precedement lues 
  ! L'ecriture se fait dans le fichier_sortie
  ! Le fichier est efface avant le debut de l'ecriture.
    integer,intent(in)::nmax,ns,ns1,ns2,iopt
    real(dp),intent(in)::cn,hbruit
!~     character(len=*),intent(in)::fichier_sortie
    integer::i
    open(NOUT,file=trim(fichier_sortie), status='unknown', form='formatted')
    write(NOUT,*) 'VERSION MAI 2016'
    write(NOUT,*) titre
    if(IZZ==1) print *, ' CANAUX SUPPRIMES = ',(IZ(i),i=1,10)
    if(iopt==1) print *, ' OPTIONS UTILISEES = ',(IO(i),i=1,20)
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
    cpt=cpt+1
  end subroutine ecriture_param
  
  
end module ecriture
