module connex
!**********************************************************************
!        Ancienne routine CONNEX.
!        laissé à la disposition de l'utilisateur.
!        Contient des relations entre paramètres connectés. 
! Rappel des indices de BT(i,nt) :
!         nt = numero du sous-spectre
!         i  = identifiant du parametres hyperfin
!   parametre  | DI GA H1 SQ CH ETA TETA GAMA BETA ALFA
!  valeur de i |  1  2  3  4  5  6    7    8    9   10
!......................................................................
use precision
IMPLICIT NONE  !!!!!!!!!!!  DECLAREZ VOS VARIABLES !
contains
  subroutine connex_connex(BT,IO)
    real(dp),intent(inout)::BT(10,40) 
    integer,intent(in)::IO(20)
!=====================================================================
!   DEBUT DE ZONE A MODIFIER  
!=====================================================================
    integer::i
    select case(IO(5))
      case(1)
!  J.P  OXYDATION DU VERT                                           
        BT(3,2)=0.63_dp*BT(2,1)*BT(3,1)/BT(2,2)
      case(2)
!  JACQUES
        BT(3,2)=BT(2,1)*BT(3,1)/2./BT(2,2)
      case(3)
!  FERRITES BEATRICE                                                
        do i=2,20
          BT(1,i)=BT(1,1)+0.11_dp
        enddo 
      case(4)
!  JEAN-MARC
        BT(3,3)=BT(2,1)*BT(3,1)/3.0_dp/BT(2,3)
      case(5)
! GAETAN
        BT(3,3)=BT(2,1)*BT(3,1)/3.0_dp/BT(2,3)
        BT(3,4)=BT(2,6)*BT(3,6)/3.0_dp/BT(2,4)
      case default
        stop 'IDENTIFIANT INCONNU DANS CONNEX'
    end select
!=====================================================================
!     FIN DE ZONE A MODIFIER
!=====================================================================
  end subroutine connex_connex
end module connex
