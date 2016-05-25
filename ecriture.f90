module ecriture
!**********************************************************************
!        module ECRITURE
!        Gestion des entrees du code : lecture du fichier
!     ..................................................................
  use precision
  use options
  implicit none
  contains
  !-----------------------------------------------------------------------
  subroutine ecriture_options(CN,NMAX,NS,NS1,NS2,IOPT,HBRUIT, fichier_sortie)
  ! Ecriture des options precedement lues 
  ! L'ecriture se fait dans le fichier fichier_sortie
  ! Le fichier est efface avant le debut de l'ecriture.
  !
      integer,intent(in)::NMAX,NS,NS1,NS2,IOPT
      real(dp),intent(in)::CN,HBRUIT
      character(len=*),intent(in)::fichier_sortie
      integer::i
      open(NOUT,file=fichier_sortie, status='unknown', form='formatted')
      write(NOUT,*) 'VERSION MAI 2016'
      write(NOUT,*) titre
      if(izz==1) print *, ' CANAUX SUPPRIMES = ',(iz(i),i=1,10)
      if(IOPT==1) print *, ' OPTIONS UTILISEES = ',(io(i),i=1,20)
      print *, 'VITESSE PAR CANAL=', CN, ' NBRE DE COMPOSANTES =',NS
      print *, 'NBRE MAX ITERATIO=', NMAX
      if(IO(4)/=0) print *, 'IL Y A UN SPECTRE DE BRUIT'
      if(NS1/=0) print *, 'DISTRIBUTION ENTRE NS1=',ns1,' ET NS2 =',ns2
      if(NS>40) write(6,*) '  NOMBRE DE SOUS SPECTRES(NS) SUPERIEUR A 40 '
      write(NOUT,*)  '   DI          GA          H1        SQ &
                 CH       ETA      TETA      GAMA      BETA      ALFA    MONOC '
      close(NOUT)
  end subroutine ecriture_options
  
end module ecriture
