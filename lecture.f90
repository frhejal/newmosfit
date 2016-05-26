module lecture
!**********************************************************************
!        module LECTURE
!        Gestion des entrees du code :
!        La lecture se fait dans l'entree standard 
!        (ex : fichier donné en entrée lors du lancement du programme)
!     ..................................................................
  use precision
  use options
  implicit none
  integer::NIN=5  ! label du fichier d'entree (fichier par defaut)
  contains
!-----------------------------------------------------------------------
    subroutine lecture_options(cn,nmax,ns,ns1,ns2,iopt,hbruit,grass)
    ! Lecture des options et des valeurs initiales des parametres variables 
      integer,intent(out)::nmax,ns,ns1,ns2,iopt
      real(dp),intent(out)::cn,hbruit
      real(dp),intent(out)::grass(10)
      integer::i
      read(NIN,'(256A)') titre
      read(NIN,*) cn,nmax,ns,ns1,ns2,izz,iopt,hbruit
      if(IZZ==1) read(5,*) (IZ(i),i=1,10)
      if(IOPT==1) read(5,*) (IO(i),i=1,20)
      if(IO(17)/=0) read(5,*) (grass(i), i=1,10)
    end subroutine lecture_options
!-----------------------------------------------------------------------
    subroutine lecture_param(di,ga,h1,sq,ch,eta,teta,gama,beta,alfa,monoc,nb,iogv)
    ! Lecture des parametres hyperfins et de leurs options d'ajustement (ajustable vs fixe)
      integer,intent(out)::monoc,iogv
      real(dp),intent(out)::di,ga,h1,sq,ch,eta,teta,gama,beta,alfa
      integer,intent(out)::nb(10)
      integer::i
      read(5,*) di,ga,h1,sq,ch,eta,teta,gama,beta,alfa,monoc
      read(5,*) (nb(i), i=1,10), iogv
    end subroutine lecture_param
!-----------------------------------------------------------------------    
    subroutine lecture_param0( di,pdi,ga,h1,sq,psq,ch,pch,eta,teta,pteta,gama,beta,alfa,monoc,nb)
    ! Lecture des parametres hyperfins à partir desquels construire une progression arithmetique 
      integer,intent(out):: monoc
      real(dp),intent(out)::di,pdi,ga,h1,sq,psq,ch,pch,eta,teta,pteta,gama,beta,alfa
      integer,intent(out)::nb(10)
      integer::i
      read(5,*)  di,pdi,ga,h1,sq,psq,ch,pch,eta,teta,pteta,gama,beta,alfa,monoc
      read(5,*) (nb(i), i=1,10)
    end subroutine lecture_param0
end module lecture
