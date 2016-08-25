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
!                        MODULE VARIABLESFIXES
!***********************************************************************
!>@brief Déclarations des variables fixées utilisées par le programme
!! principal et par les routines de calcul
!!@version juin 2016
module variablesFixes
  use precision
  ! Variables lues en option
  real(DP),save::CN=0.078125_DP  !< Largeur du canal (mm/s)
  integer,save::NS=1 !< Nombre de sous-spectres théoriques utilisé pour l'ajustement d'un spectre expérimental.
  integer,save::NMAX=0  !< Nombre maximum d'itérations dans l'ajustement en moindres carrés
  integer,save::NS1=0 !< Premier spectre de la distribution
  integer,save::NS2=0 !< Dernier spectre de la distribution
  integer,save::GRASS(10) !< Plages de sous-spectres à sommer (si IO(17)=1)
  integer,save::PLAGEL(2) !< Plage de sous-spectres à lisser
end module variablesFixes
