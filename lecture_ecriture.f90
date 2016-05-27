module lecture
!**********************************************************************
!        module LECTURE
!        Gestion des entrees du code : lecture du fichier
!     ..................................................................
use precision
use options
  implicit none

!~   real(dp)::
  character(len=*)::titre
  contains
!-----------------------------------------------------------------------
    subroutine lecture_options(CN,NMAX,NS,NS1,NS2,IZZ,IOPT,HBRUIT,)
    ! Lecture des options et des valeurs initiales des parametres variables 
    ! La lecture se fait dans l'entree standard 
    ! (ex : fichier donné en entrée lors du lancement du programme)
      integer,intent(out)::NMAX,NS,NS1,NS2,IZZ,IOPT
      real(dp),intent(out)::CN,HBRUIT
      read(5,16A4) titre
    end subroutine lecture_options
!-----------------------------------------------------------------------

end module lecture

module ecriture
  use precision
  use lecture
  contains
  !-----------------------------------------------------------------------
  subroutine ecriture_option(CN,NMAX,NS,NS1,NS2,IZZ,IOPT,HBRUIT, fichier_sortie)
  ! Ecriture des options precedement lues 
  ! L'ecriture se fait dans le fichier fichier_sortie
  ! Le fichier est efface avant le debut de l'ecriture.
  !
      integer,intent(in)::NMAX,NS,NS1,NS2,IZZ,IOPT
      real(dp),intent(in)::CN,HBRUIT
      character(len=*)::fichier_sortie
      write(6,*) 'VERSION MAI 2016'
      write(6,*) titre
!~       if(izz==1) print *, ' CANAUX SUPPRIMES = ',(iz(i),i=1,10)
!~       if(IOPT.EQ.1) print *, ' OPTIONS UTILISEES = ',(io(i),i=1,20)
!~       print *, 'VITESSE PAR CANAL=', CN, ' NBRE DE COMPOSANTES =',NS
!~       print *, 'NBRE MAX ITERATIO=', NMAX
!~       IF(IO(4).NE.0) PRINT *, 'IL Y A UN SPECTRE DE BRUIT'
      
  end subroutine ecriture_option
  
end module ecriture
