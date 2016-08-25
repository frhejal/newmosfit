!    This file is part of Mosfit2016.
!
!    Mosfit2016 is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    Mosfit2016 is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with this program.  If not, see <http://www.gnu.org/licenses/>.
!>@file
!**********************************************************************
!                             MODULE LECTURE
!**********************************************************************
!>@brief  Gestion des entrées du code.
!!@version juin 2016
module lecture
  use precision
  use options
  implicit none
  integer,save::NIN=5
  character(len=255),save::fichierCoo
  contains
!=======================================================================
  !>@brief Ouverture du fichier donné en argument
  !>@details Les fichiers de sortie sont nommés en fonction du nom du fichier d'entrée
  subroutine lecture_ouvrir_fichier_entree
    integer::taille
    call get_command_argument(1,fichierCoo)
    taille=len_trim(fichiercoo)
    if(taille/=0)then
      open(NIN,file=fichierCoo,status="unknown",form="formatted", access="sequential")
    endif
    write(6,*) fichierCoo
  end subroutine lecture_ouvrir_fichier_entree
!=======================================================================
  !>@brief Lecture du titre du fichier d'entrée. 256 caractères maximum,
  !! les caractères supplémentaires sont ignorés.
  subroutine lecture_titre
    read(NIN,'(256A)') titre      
  end subroutine lecture_titre
!=======================================================================
  !> @brief Lecture des options et des valeurs initiales des parametres hyperfins ajustables
  subroutine lecture_options(cn,nmax,ns,ns1,ns2,hbruit,grass,plage)
    integer,intent(out)::nmax !< Nombre maximal d'itérations dans l'algorithme des moindres carrés
    integer,intent(out)::ns !< Nombre de sous-spectres
    integer,intent(out)::ns1 !< Numéro du premier sous spectre de la distribution
    integer,intent(out)::ns2 !< Numéro du dernier sous-spectre de la distribution
    real(DP),intent(out)::cn !< Largeur d'un canal (mm/s)
    real(DP),intent(out)::hbruit !< Hauteur du spectre de bruit
    integer,intent(out)::grass(10) !< Plages de sous-spectres à sommer
    integer,intent(out)::plage(2) !< Plages de sous-spectres à lisser
    integer::i
    read(NIN,*) cn,nmax,ns,ns1,ns2,IZZ,IOPT,hbruit
    if(IZZ==1) read(5,*) (IZ(i),i=1,10)
    if(IOPT==1) read(5,*) (IO(i),i=1,20)
    IF(IO(13)==3) read(5,*) plage
    if(IO(17)==1) read(5,*) (grass(i), i=1,10)
  end subroutine lecture_options
!=======================================================================
    !> @brief Lecture des paramètres hyperfins et de leurs options d'ajustement (ajustable vs fixe),
    !! et éventuellement des largeurs de raies (si IOGV lue vaut 3)
    subroutine lecture_param(di,ga,h1,sq,ch,eta,teta,gama,beta,alpha,monoc,nb,iogv,gv,ng)
      integer,intent(out)::monoc!< Monocristal ou pas monocristal
      integer,intent(out)::iogv !< Type d'ajustement des raies
      real(DP),intent(out)::di,ga,h1,sq,ch,eta,teta,gama,beta,alpha !< Paramètres hyperfins
      integer,intent(out)::nb(10) !< Type d'ajustement des paramètres hyperfins
      real(DP),intent(out)::gv(8) !< Largeur des raies (si IOGV==3)
      integer,intent(out)::ng(8)  !< Ajustement des raies (si IOGV==3)
      integer::i
      read(NIN,*) di,ga,h1,sq,ch,eta,teta,gama,beta,alpha,monoc
      read(NIN,*) (nb(i), i=1,10), iogv
      if(iogv==3)then
        read(NIN,*) (gv(i),i=1,8)
        read(NIN,*) (ng(i),i=1,8)
      endif
    end subroutine lecture_param
!=======================================================================
    !> @brief Lecture des paramètres hyperfins à partir desquels construire une progression arithmétique
    subroutine lecture_param0( di,pdi,ga,h1,sq,psq,ch,pch,eta,teta,pteta,gama,beta,alpha,monoc,nb)
      integer,intent(out):: monoc
      real(DP),intent(out)::di,pdi,ga,h1,sq,psq,ch,pch,eta,teta,pteta,gama,beta,alpha
      integer,intent(out)::nb(10)
      integer::i
      read(NIN,*)  di,pdi,ga,h1,sq,psq,ch,pch,eta,teta,pteta,gama,beta,alpha,monoc
      read(NIN,*) (nb(i), i=1,10)
    end subroutine lecture_param0
!=======================================================================
    !> @brief Lecture des valeurs du spectre
    subroutine lecture_spectre(spectre,N)
      integer,intent(in)::N
      real(DP),intent(out)::spectre(N)
      integer:: temp(N)
      integer::i,j,dump
      do i=1,n,8
        read(NIN,'(I5,8I8)') dump , (temp(j),j=i,i+7)
      enddo
      spectre= real(temp,dp)
    end subroutine lecture_spectre
end module lecture
