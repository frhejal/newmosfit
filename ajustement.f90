module ajustement
!**********************************************************************
!        MODULE moindres-carrés
!        Recherche itérative des valeur des parametres variables
!        permettant d'approcher le spectre experimental
!        Methode dutilisée : algorithme de Marquardt
!        D.W. MARQUARDT- J. Soc. Indust. Appl. Math 11, 431 (1963)
!.....................................................................
  use precision
  use options
  use variablesAjustables
  use spectres
  use ecriture
  implicit none
  real(dp)::CRITERE=1.0D-3
  real(dp)::PH=0.0_dp
  contains 
  function convergence(oldB)
  ! la fonction qui calcule le KHI**2
    real(dp),intent(in)::oldB(40)
    real(dp)::petit=1.0D-12 ! pour eviter la division par zero
    logical::convergence
      if( maxval(abs(B-oldB)/(abs(B)+petit)) > CRITERE)  then
        convergence= .FALSE.
      else
        convergence= .TRUE.
      endif
  end function convergence
  !=====================================================================
  subroutine ajustement_moindres_carres(q,n,b,Y,k,p,nmax)
   ! Recherche de moindres carrés utilisant l'agorithme de Marquardt
   ! Basée sur la routine en F77 MAMAGT (codes Relfej,Mosfit)
   ! Reference :
   ! [1] D.W. MARQUARDT- J. Soc. Indust. Appl. Math 11, 431 (1963)
!~       DIMENSION Y(1),B(1),Q(ID,1),G(100),G1(100),P(1)
!~       COMMON/VIR/PH,VQ(100,100),ETBT(10,100),ETGVT(8,100)
!~       COMMON/UNIT/IUNITE,IUNITS,IUNITS2,IUNITS3,IUNITS4,IUNITS5
      integer,intent(in)::n ! ordre du systeme (taille du spectre)
      integer,intent(in):: k ! nombre de parametres ajustables
      integer,intent(in):: nmax ! nombre maximum d'iterations
      real(dp),intent(inout)::q(n,42) !tableau de travail. Apres calcul du spectre theorique , contient
                                      ! le spectre theorique +  tableau des derivees 
                                      !   (matrice de coefficients de correlation)
      real(dp),intent(inout)::b(k) ! vecteur des parametres ajustables
      real(dp),intent(in)::Y(n) ! spectre experimental
      real(dp),intent(in)::p(n) ! poids statistique des canaux
      logical::fin,coupureNmax 
      integer ::i,int,npas
      real(dp)::oldB(40)
      real(dp)::nu,lambda
      npas=0 
      fin=.FALSE.
      oldB=0.0_dp
      VQ(1:K,1:K)=0.0_dp
      nu=5.0
      lambda=0.01
      int=0
      marquardt: do while(.NOT. fin)
        ! verification du critere de sortie de la boucle---------------- 
        call ecriture_info_iteration(npas,nmax,B)
        coupureNmax = (npas>=nmax) .AND. (npas/=0)  ! au moins un pas doit etre fait, meme dans le cas sparticulier où nmax= 0.
        fin = coupureNmax .OR. convergence(oldB)
        npas=npas+1
        oldB(1:K)=B(1:K)
        ! premier calcul du spectre theorique---------------------------
        call spectres_theorique_total(PH)
        if(fin) exit marquardt  ! sortie sans calculer le reste (nmax ou critere atteint)
        !calcul de phi--------------------------------------------------
          PH=0.0_dp
          do i=1,n
            PH= PH + p(I)*(y(I)-q(I,K+2))**2  ! equation (3) de [1]
          enddo
!~   174 INT = INT+1
!~       if(int >1) then ! si on a passe les deux premieres iterations
!~         if( PH < phi )then
!~         ! Choix du multiplicateur de Lagrande (cf lambda dans [1]) :
!~         !   - si on a laissé int=int+1 se faire à l'iteration precedente, on est en train de tester :
!~         !       A:   phi(lamda/nu) < phi_actuel
!~         !   - si on a int inchangé depuis l'itération précedente, on est en train de tester 
!~         !       B:  phi( lambda ) < phi_actuel) 
!~           phi = PH
!~           lambda=lambda/nu 
!~         else 
!~           INT=INT-1 ! annule l'incrementation de int
!~           lambda=lambda*nu  ! on revient à lambda (test A)
!~                             ! ou on fait lambda*nyu (test B)
!~           B(1:K)=G(1:K) !on recharge les parametres precedents (i.e. on reste dans l'iteration actuelle)
!~             fin = convergence(oldB)
            oldB(1:K)=B(1:K)
            call spectres_theorique_total(PH)
!~           CALL CALC(E,*101) ! on recalcule le spectre precedent 
!~         endif
!~       else
!~         phi = PH
!~       endif
!~       !
!~    88 DO 6 I=1,K
!~         Q(I,K+1)=0.
!~         DO 6 M=1,N
!~           6 Q(I,K+1)=Q(I,K+1)+(Y(M)-Q(M,K+2))*Q(M,I)*P(M)
!~         enddo
!~       enddo
!~       DO 4 I=1,K
!~         DO 51 J=1,K
!~           Q(J,K+2)=0.
!~           if(j>=i)then
!~             DO 54 L=1,N
!~               Q(J,K+2)=Q(J,K+2)+Q(L,I)*Q(L,J)*P(L)
!~             enddo
!~           else
!~             Q(J,K+2)=Q(I,J)
!~           endif
!~         DO 55 L=1,K
!~           Q(L,I)=Q(L,K+2)
!~         enddo
!~       enddo
!~       DO I=1,K
!~         DO J=1,K
!~           VQ(I,J)=Q(I,J)
!~         enddo
!~       enddo
!~       DO I=1,K
!~         IF (Q(I,I)==0)then
!~           WRITE (IUNITS,740) I
!~           FORMAT (1X,45H LA FONCTION EST INDEPENDANTE DU PARAMETRE NO,I4)
!~           DO M=1,K
!~             Q(M,K+2)=Y(M)
!~           enddo
!~         endif
!~         Q(I,K+2)=SQRT(Q(I,I))
!~         Q(I,K+1)=Q(I,K+1)/Q(I,K+2)
!~       enddo
!~       DO 24 I=1,K
!~         DO 24 J=1,K
!~           IF(I-J)11,16,10 then 
!~           if(i=j)
!~             Q(I,J)=1.+lambda
!~           elseif(i<j)then
!~             Q(I,J)=Q(I,J)/(Q(I,K+2)*Q(J,K+2))
!~           else
!~             Q(I,J)=Q(J,I)
!~           endif
!~         enddo
!~       enddo
!~       CALL ALSB(Q,ID,K,1,G,IERR)
!~       DO 25 I=1,K
!~         Q(I,K+1)=Q(I,K+1)/Q(I,K+2)
!~         G(I)=B(I)   !sauvegarde de l'ancien vecteur de parametres
!~         B(I)=B(I)+Q(I,K+1)  !  b[r+1] = b[r] +delta[r]
!~       enddo
!~       GOTO 222
    enddo marquardt
!~   101 RETURN
  end subroutine ajustement_moindres_carres
end module ajustement
