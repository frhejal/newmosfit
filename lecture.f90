!>@file
!**********************************************************************
!                             MODULE LECTURE
!**********************************************************************
!>@brief  Gestion des entrees du code :
!>@details  La lecture se fait dans l'entree standard 
!! (ex : fichier donné en entrée lors du lancement du programme)
!!@version juin 2016
module lecture
  use precision
  use options
  implicit none
  integer::NIN=5 !<Entree standard
  contains
!=======================================================================
  subroutine lecture_titre
    read(NIN,'(256A)') titre      
  end subroutine lecture_titre
!=======================================================================
  !> @brief Lecture des options et des valeurs initiales des parametres hyperfins ajustables
  subroutine lecture_options(cn,nmax,ns,ns1,ns2,hbruit,grass)
    integer,intent(out)::nmax !<Nombre maximal d'itérations dans l'algorithme des moindres carrés 
    integer,intent(out)::ns !<nombre de sous-spectres 
    integer,intent(out)::ns1 !< numéro du premier sous spectre de la distribution
    integer,intent(out)::ns2 !< numéro du dernier sous-spectre de la distribution
    real(dp),intent(out)::cn !< largeur d'un canal (mm/s)
    real(dp),intent(out)::hbruit !< hauteur du spectre de bruit
    integer,intent(out)::grass(10) !< Plages de sous-spectres à sommer
    integer::i
    read(NIN,*) cn,nmax,ns,ns1,ns2,IZZ,IOPT,hbruit
    if(IZZ==1) read(5,*) (IZ(i),i=1,10)
    if(IOPT==1) read(5,*) (IO(i),i=1,20)
    if(IO(17)/=0) read(5,*) (grass(i), i=1,10)
  end subroutine lecture_options
!=======================================================================
    !> @brief Lecture des paramètres hyperfins et de leurs options d'ajustement (ajustable vs fixe)
    subroutine lecture_param(di,ga,h1,sq,ch,eta,teta,gama,beta,alpha,monoc,nb,iogv)
      integer,intent(out)::monoc
      integer,intent(out)::iogv
      real(dp),intent(out)::di,ga,h1,sq,ch,eta,teta,gama,beta,alpha
      integer,intent(out)::nb(10)
      integer::i
      read(5,*) di,ga,h1,sq,ch,eta,teta,gama,beta,alpha,monoc
      read(5,*) (nb(i), i=1,10), iogv
    end subroutine lecture_param
!=======================================================================
    !> @brief Lecture des parametres hyperfins à partir desquels construire une progression arithmetique 
    subroutine lecture_param0( di,pdi,ga,h1,sq,psq,ch,pch,eta,teta,pteta,gama,beta,alpha,monoc,nb)
      integer,intent(out):: monoc
      real(dp),intent(out)::di,pdi,ga,h1,sq,psq,ch,pch,eta,teta,pteta,gama,beta,alpha
      integer,intent(out)::nb(10)
      integer::i
      read(NIN,*)  di,pdi,ga,h1,sq,psq,ch,pch,eta,teta,pteta,gama,beta,alpha,monoc
      read(NIN,*) (nb(i), i=1,10)
    end subroutine lecture_param0
!=======================================================================
    !> @brief Lecture des valeurs du spectre
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
