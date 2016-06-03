module ajustement
!**********************************************************************
!        MODULE moindres-carrés
!        Gestion des entree/sortie
!.....................................................................
  use precision
  use option
  implicit none
  contains 
  subroutine ajustement_verifier_convergence(npas)
    integer,intent(in)::npas ! numero du pas
  
  ! l'equivalent de MAMAGT ?
  ! la fonction qui calcule le KHI**2
   SUBROUTINE MAMAGT(Q,ID,B,Y,N,K,E,CALC,P)
   ! Recherche de moindres carrés utilisant l'agorithme de Marquardt
   ! Reference :
   ! [1] D.W. MARQUARDT- J. Soc. Indust. Appl. Math 11, 431 (1963)
!~       DIMENSION Y(1),B(1),Q(ID,1),G(100),G1(100),P(1)
!~       COMMON/VIR/PH,VQ(100,100),ETBT(10,100),ETGVT(8,100)
!~       COMMON/UNIT/IUNITE,IUNITS,IUNITS2,IUNITS3,IUNITS4,IUNITS5
      integer ::i 
      VQ(1:K,1:K)=0.0_dp
      nu=5.0
      lambda=0.01
      INT=0
  222    CALL CALC(E,*101) ! premier calcul de spectre
                           ! si convergence atteinte, goto 101
      !calcul de phi
      ph=0.0_dp
      do I=1,N
        ph= ph + P(I)*(Y(I)-Q(I,K+2))**2  ! equation (3) de [1]
      enddo
  174 INT = INT+1
      if(int >1) then ! si on a passe les deux premieres iterations
        if( ph < phi )then
        ! Choix du multiplicateur de Lagrande (cf lambda dans [1]) :
        !   - si on a laisse int=int+1 se faire à l'iteration precedente, on est en train de tester :
        !       A:   phi(lamda/nu) < phi_actuel
        !   - si on a int inchangé depuis l'itération précedente, on est en train de tester 
        !       B:  phi( lambda ) < phi_actuel) 
          phi = ph 
          lambda=lambda/nu 
        else 
          INT=INT-1 ! annule l'incrementation de int
          lambda=lambda*nu  ! on revient à lambda (test A)
                            ! ou on fait lambda*nyu (test B)
          B(1:K)=G(1:K) !on recharge les parametres precedents (i.e. on reste dans l'iteration actuelle)
          CALL CALC(E,*101) ! on recalcule le spectre precedent 
        endif
      else
        phi = ph
      endif
      !
   88 DO 6 I=1,K
        Q(I,K+1)=0.
        DO 6 M=1,N
          6 Q(I,K+1)=Q(I,K+1)+(Y(M)-Q(M,K+2))*Q(M,I)*P(M)
        enddo
      enddo
      DO 4 I=1,K
        DO 51 J=1,K
          Q(J,K+2)=0.
          if(j>=i)then
            DO 54 L=1,N
              Q(J,K+2)=Q(J,K+2)+Q(L,I)*Q(L,J)*P(L)
            enddo
          else
            Q(J,K+2)=Q(I,J)
          endif
        DO 55 L=1,K
          Q(L,I)=Q(L,K+2)
        enddo
      enddo
      DO I=1,K
        DO J=1,K
          VQ(I,J)=Q(I,J)
        enddo
      enddo
      DO I=1,K
        IF (Q(I,I)==0)then
          WRITE (IUNITS,740) I
          FORMAT (1X,45H LA FONCTION EST INDEPENDANTE DU PARAMETRE NO,I4)
          DO M=1,K
            Q(M,K+2)=Y(M)
          enddo
        endif
        Q(I,K+2)=SQRT(Q(I,I))
        Q(I,K+1)=Q(I,K+1)/Q(I,K+2)
      enddo
      DO 24 I=1,K
        DO 24 J=1,K
          IF(I-J)11,16,10 then 
          if(i=j)
            Q(I,J)=1.+lambda
          elseif(i<j)then
            Q(I,J)=Q(I,J)/(Q(I,K+2)*Q(J,K+2))
          else
            Q(I,J)=Q(J,I)
          endif
        enddo
      enddo
      CALL ALSB(Q,ID,K,1,G,IERR)
      DO 25 I=1,K
        Q(I,K+1)=Q(I,K+1)/Q(I,K+2)
        G(I)=B(I)   !sauvegarde de l'ancien vecteur de parametres
        B(I)=B(I)+Q(I,K+1)  !  b[r+1] = b[r] +delta[r]
      enddo
      GOTO 222
      
  101 RETURN
      END
end module ajustement
