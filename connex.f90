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
!***********************************************************************
!                              MODULE CONNEX
!***********************************************************************
!>@brief Ancienne routine CONNEX, laissée à la disposition de l'utilisateur.
!>@details Permet a l'utilisateur d'imposer des relations entre les différentes composantes 
!  du spectre  (intensités, parametres hyperfins...)
!>
!> @n @n Rappel des indices de BT(i,nt) :
!>
!>         nt = numéro du sous-spectre
!>         i  = identifiant du paramètres hyperfin
!>
!>  parametre   | DI | GA | H1 | SQ | CH | ETA|TETA|GAMA|BETA|ALFA|
!>--------------|----|----|----|----|----|----|----|----|----|----|
!>  valeur de i |  1 |  2 | 3  | 4  | 5  |  6 | 7  | 8  | 9  | 10 |
!>@version juin 2016
module connex
  use precision
  IMPLICIT NONE  !  DECLAREZ VOS VARIABLES !
  contains
  subroutine connex_connexions(bt,io)
    real(DP),intent(inout)::bt(10,40) !< Tableau des paramètres ajustables pour chaque sous-spectre
    integer,intent(in)::io(20)!< Liste d'options
    !=====================================================================
    !   DEBUT DE ZONE A MODIFIER  
    !=====================================================================
    integer::i
    select case(io(5))
      case(0) 
        print *, 'Un appel inutile à CONNEX a été fait'
      case(1)
    !  J.P  OXYDATION DU VERT                                           
        bt(3,2)=0.63_DP*bt(2,1)*bt(3,1)/bt(2,2)
      case(2)
    !  JACQUES
        bt(3,2)=bt(2,1)*bt(3,1)/2.0_DP/bt(2,2)
      case(3)
    !  FERRITES BEATRICE                                                
        do i=2,20
          bt(1,i)=bt(1,1)+0.11_DP
        enddo 
      case(4)
    !  JEAN-MARC
        bt(3,3)=bt(2,1)*bt(3,1)/3.0_DP/bt(2,3)
      case(5)
    !  GAETAN
        bt(3,3)=bt(2,1)*bt(3,1)/3.0_DP/bt(2,3)
        bt(3,4)=bt(2,6)*bt(3,6)/3.0_DP/bt(2,4)
      case default
        stop 'OPTION IO(5) : VALEUR INCONNUE DANS CONNEX'
    end select
    !=====================================================================
    !     FIN DE ZONE A MODIFIER
    !=====================================================================
  end subroutine connex_connexions
end module connex
