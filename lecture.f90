module lecture
!**********************************************************************
!        module LECTURE
!        Gestion des entrees du code :
!        lecture du fichier donné en entree par defaut
!     ..................................................................
  use precision
  use options
  implicit none
  contains
!-----------------------------------------------------------------------
    subroutine lecture_options(CN,NMAX,NS,NS1,NS2,IOPT,HBRUIT,GRASS)
    ! Lecture des options et des valeurs initiales des parametres variables 
    ! La lecture se fait dans l'entree standard 
    ! (ex : fichier donné en entrée lors du lancement du programme)
      integer,intent(out)::NMAX,NS,NS1,NS2,IOPT
      real(dp),intent(out)::CN,HBRUIT
      real(dp),intent(out)::GRASS(10)
      integer::i
      read(5,'(256A)') titre
      read(5,*) CN,NMAX,NS,NS1,NS2,IZZ,IOPT,HBRUIT
      if(IZZ==1) read(5,*) (IZ(i),i=1,10)
      if(IOPT==1) read(5,*) (IO(i),i=1,20)
      if(IO(17)/=0) read(5,*) (GRASS(i), i=1,10)
    end subroutine lecture_options
!-----------------------------------------------------------------------
    subroutine lecture_spectre(DI,GA,H1,SQ,CH,ETA,TETA,GAMA,BETA,ALFA)
      real(dp),intent(out)::DI,GA,H1,SQ,CH,ETA,TETA,GAMA,BETA,ALFA
      read(5,*) DI,GA,H1,SQ,CH,ETA,TETA,GAMA,BETA,ALFA,MONOC
    end subroutine lecture_spectre
end module lecture
