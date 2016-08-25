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
!                        MODULE AJUSTEMENT
!**********************************************************************
!>@brief Contient les routines liées à l'ajustement par la méthode des
!! moindres carrés.
!>@details Recherche itérative des valeurs des paramètres variables
!!        permettant d'approcher le spectre expérimental. 
!!        La méthode utilisée est l'algorithme de Marquardt.
!!@n ref.[1]: D.W. MARQUARDT- J. Soc. Indust. Appl. Math 11, 431 (1963)
!>@version Juin 2016
module ajustement
  use precision
  use options
  use variablesAjustables
  use spectres
  use ecriture
  use algebre
  implicit none
  real(DP),save::PH  !< Phi dans ref.[1]
  real(DP),save::CRITERE !< Critère de convergence
  real(DP),save::KHI2   !< Ecart statistique de l'ajustement en moindres carrés
  contains
  !>@brief Calcul de la convergence.
  !>@details Vérifie si le critère de convergence est atteint.
  !! Le critère de convergence porte sur la différence relative entre les paramètres ajustés de la dernière itération 
  !! et  ceux de l'itération précedente.
  function convergence(eps)
    real(DP),intent(in)::eps !< Critère de convergence
    real(DP),save::oldB(40)
    real(DP)::petit=1.0D-5 ! Pour éviter la division par zéro
    logical::convergence
    if(  maxval(abs(B(1:K)-oldB(1:K))/(abs(B(1:K))+petit)) > eps)  then
      convergence= .FALSE.
    else
      convergence= .TRUE.
    endif
    oldB=B
  end function convergence
  !=====================================================================
  !>@brief Cacul du Khi**2
  function ajustement_ecart_stat(k,n,spectre_exp,spectre_fit,p)
    integer,intent(in)::k
    integer,intent(in)::n
    real(DP),intent(in)::spectre_exp(n)
    real(DP),intent(in)::spectre_fit(n)
    real(DP),intent(in)::p(n) !Poids des canaux
    real(DP)::khi2
    real(DP)::ajustement_ecart_stat
    integer::i
    khi2=0.0_DP
    do i=1,n
      if(spectre_exp(i)/=0.0_DP) khi2=khi2+p(i)*(spectre_fit(i) -spectre_exp(i))**2 /spectre_exp(i)
    enddo
    khi2=khi2/(n-k)
    ajustement_ecart_stat=khi2
  end function ajustement_ecart_stat
  !=====================================================================
  !>@brief Recherche de moindres carrés utilisant l'agorithme de Marquardt
  !>@détails Basée sur la routine en F77 MAMAGT (codes Relfej,Mosfit)
  !! Référence :
  !! [1] D.W. MARQUARDT- J. Soc. Indust. Appl. Math 11, 431 (1963)
  subroutine ajustement_moindres_carres(q,n,b,y,k,p,nmax,critere)
      integer,intent(in)::n !< Ordre du système (taille du spectre)
      integer,intent(in):: k !< Nombre de paramètres ajustables
      integer,intent(in):: nmax !< Nombre maximum d'itérations
      real(DP),intent(in)::critere !<Critère de convergence
      real(DP),intent(inout)::q(n,42) !< Tableau de travail. Apres calcul du spectre théorique , contient
                                      !! le spectre théorique + le tableau des dérivées 
                                      !!   (matrice de coefficients de correlation)
      real(DP),intent(inout)::b(k) !< Vecteur des paramètres ajustables
      real(DP),intent(in)::y(n) !< Spectre expérimental
      real(DP),intent(in)::p(n) !< Poids statistique des canaux
      logical::fin,coupureNmax 
      integer::i,j,l,m,ierr
      integer::iter ! Nombre d'itération de l'algorithme de Marquardt effectué
      integer::npas ! Nombre de calculs du spectre effectués (nombre d'appel de spectres_theorique_total)
      integer::dump(100)! Vecteur de travail pour alsb
      real(DP)::saveB(40)
      real(DP)::nu,lambda,phi
      npas=0
      fin=.FALSE.
      saveB=0.0_DP
      Q=0.0_DP
      VQ(1:K,1:K)=0.0_DP
      nu=5.0_DP
      lambda=0.01_DP
      iter=0
      !Début algorithme moindres carrés
      marquardt: do while(.NOT. fin)
        ! Vérification du critère de sortie de la boucle---------------- 
        call ecriture_info_iteration(npas,nmax,B)
        coupureNmax = (npas>=nmax) .AND. (npas/=0)  ! Au moins un pas doit être fait, même dans le cas sparticulier où nmax= 0.
        fin = coupureNmax .OR. convergence(CRITERE)
        npas=npas+1
        ! Premier calcul du spectre théorique---------------------------
        call spectres_theorique_total
        if(fin) exit marquardt  ! sortie sans calculer le reste (nmax ou critere atteint)
        !Calcul de phi--------------------------------------------------
        PH=0.0_DP
        do i=1,n
          PH= PH + p(i)*(y(i)-q(i,K+2))**2  ! Equation (3) de ref.[1]
        enddo
        iter=iter+1
        if(iter >1) then ! Si on a passé la première itération (initialisation)
          if( PH < phi )then
            phi = PH
            lambda=lambda/nu 
          else 
            lambda=lambda*nu  ! On revient à lambda précédent ou on fait lambda*nu
            iter=iter-1 ! Annule l'incrémentation de int (i.e. on reste dans l'itération actuelle)
            B(1:K)=saveB(1:K) ! On recharge les paramètres précédents 
            call ecriture_info_iteration(npas,nmax,B)
            fin = convergence(critere) .OR. (npas>=nmax)
            npas=npas+1
            call spectres_theorique_total
            !Sortie en cas de convergence (ou de Nmax itérations atteintes)
            if(fin)exit marquardt
          endif
        else
          phi = PH
        endif
        ! Vecteur g (équations (9) et (21) de ref.[1])
        do i=1,K
          q(i,K+1)=0.0_DP
          do m=1,N
            q(i,K+1)=q(i,K+1)+(y(m)-q(m,K+2))*q(m,i)*p(m) 
          enddo
        enddo
        ! Matrice A de ref.[1]
        do i=1,K
          do j=1,K
            q(j,K+2)=0.0_DP
            if(j>=i)then 
              do l=1,n
                q(j,K+2)=q(j,K+2)+q(l,i)*q(l,j)*p(l) 
              enddo
            else
              q(j,K+2)=q(i,j)
            endif
          enddo
          do l=1,K
            q(l,i)=q(l,K+2)
          enddo
        enddo
        do i=1,K
          do j=1,K
            vq(i,j)=q(i,j)
          enddo
        enddo
        ! Vecteur g*  (équation (28) de ref.[1]) 
        do i=1,K
          if (q(i,i)==0.0_DP)then  ! La fonction est indépendante du paramètre i
            call ecriture_fonction_independante(i)
            do m=1,K
              q(m,K+2)=y(m)
            enddo
            exit marquardt
          endif
          q(i,K+2)=SQRT(q(i,i))
          q(i,K+1)=q(i,K+1)/q(i,K+2)
        enddo
        ! Création de la matrice (A* + lambda I)  (équation (31) de ref.[1])
        do i=1,K
          do j=1,K
            if(i==j)then
              q(I,J)=1.0_DP+lambda
            elseif(i<j)then
              q(i,j)=q(i,j)/(q(i,K+2)*q(j,K+2))
            else
              q(i,j)=q(j,i)
            endif
          enddo
        enddo
        ! Résolution de l'équation (31) de ref.[1]
        call algebre_resoudre_systeme(q,n,K,1,dump,ierr)
        if(ierr/=0) stop 'erreur de resolution de systeme lineraire dans ASLB'
        do i=1,K
          q(i,K+1)=q(i,K+1)/q(i,K+2)
          saveB(i)=b(i)   ! Sauvegarde de l'ancien vecteur de paramètres
          b(i)=b(i)+q(i,K+1)  !  b[r+1] = b[r] +delta[r]
        enddo
    enddo marquardt
  end subroutine ajustement_moindres_carres
  !=====================================================================
  !>@brief Initialisation de Phi et du critère de convergence
  subroutine ajustement_raz
    PH=0.0_DP
    CRITERE = 0.001_DP
  end subroutine ajustement_raz
end module ajustement
