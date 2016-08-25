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
!                            MODULE PRECISION
!***********************************************************************
!>@brief Définition de la double précision et de quelques constantes mathématiques
!!@details  @b dp : entier  correspondant au kind d'un reel double precision 
!!          (ex : 1.0D0 ), tel que defini par le compilateur.
!!@n     Pour définir des constantes, préférer l'écriture 42.0_DP à 42.0D0.
!!         Cela rend le code plus consistant et permet de changer la
!!         précision de toutes les variables en modifiant uniquement dp.
!!@n     Pour déclarer une variable X en double precision, ecrire :
!!@n@b            REAL(DP)::X
!!       ou 
!!@b           COMPLEX(DP)::X
!!@n     Pour créer un complexe Z en double précision à partir de sa partie 
!!       réelle X et de sa partie imaginaire Y, utiliser :
!!@n@b          Z=CMPLX(X,Y,dp)
!!@n\b     REMARQUE :
!!     La fonction COMPLEXE(X,Y) ne fait plus partie des 
!!     fonction intrinsèques en fortran95/fortran2003, même si quelques
!!     compilateurs la procurent encore (ex : gfortran)  )
module precision
  implicit none
  integer,parameter:: real(DP)=kind(1.0D0) !< Double précision
  real(DP),parameter:: PI=4.0_dp*atan(1.0_dp) !<  Rapport entre le périmètre d'un cercle et son diamètre
  real(DP),parameter:: RPD=PI/180.0_dp    !< Conversion de degrés en radians
  real(DP),parameter:: ROOT2 = sqrt(2.0_dp)!< Racine de 2
  real(DP),parameter:: ROOT3 = sqrt(3.0_dp)!< Racine de 3
end module precision
