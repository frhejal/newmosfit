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
  integer::NIN=5  !Entree standard
  contains
!-----------------------------------------------------------------------
    subroutine lecture_titre
      read(NIN,'(256A)') titre      
    end subroutine lecture_titre
!-----------------------------------------------------------------------
    subroutine lecture_options(cn,nmax,ns,ns1,ns2,hbruit,grass)
    ! Lecture des options et des valeurs initiales des parametres hyperfins ajustables
      integer,intent(out)::nmax,ns,ns1,ns2
      real(dp),intent(out)::cn
      real(dp),intent(out)::hbruit
      integer,intent(out)::grass(10)
      integer::i
      read(NIN,*) cn,nmax,ns,ns1,ns2,IZZ,IOPT,hbruit
      if(IZZ==1) read(5,*) (IZ(i),i=1,10)
      if(IOPT==1) read(5,*) (IO(i),i=1,20)
      if(IO(17)/=0) read(5,*) (grass(i), i=1,10)
    end subroutine lecture_options
!-----------------------------------------------------------------------
    subroutine lecture_param(di,ga,h1,sq,ch,eta,teta,gama,beta,alpha,monoc,nb,iogv)
    ! Lecture des parametres hyperfins et de leurs options d'ajustement (ajustable vs fixe)
      integer,intent(out)::monoc,iogv
      real(dp),intent(out)::di,ga,h1,sq,ch,eta,teta,gama,beta,alpha
      integer,intent(out)::nb(10)
      integer::i
      read(5,*) di,ga,h1,sq,ch,eta,teta,gama,beta,alpha,monoc
      read(5,*) (nb(i), i=1,10), iogv
    end subroutine lecture_param
!-----------------------------------------------------------------------    
    subroutine lecture_param0( di,pdi,ga,h1,sq,psq,ch,pch,eta,teta,pteta,gama,beta,alpha,monoc,nb)
    ! Lecture des parametres hyperfins à partir desquels construire une progression arithmetique 
      integer,intent(out):: monoc
      real(dp),intent(out)::di,pdi,ga,h1,sq,psq,ch,pch,eta,teta,pteta,gama,beta,alpha
      integer,intent(out)::nb(10)
      integer::i
      read(NIN,*)  di,pdi,ga,h1,sq,psq,ch,pch,eta,teta,pteta,gama,beta,alpha,monoc
      read(NIN,*) (nb(i), i=1,10)
    end subroutine lecture_param0
!-----------------------------------------------------------------------    
    subroutine lecture_spectre(spectre,N)
      integer,intent(in)::N
      real(dp),intent(out)::spectre(N)
      integer:: temp(N)
      integer::i,j,dump
      do i=1,n,8
        read(NIN,'(I5,8I8)') dump , (temp(j),j=i,i+7)
      enddo
      spectre= real(temp,dp)
    end subroutine lecture_spectre
end module lecture
