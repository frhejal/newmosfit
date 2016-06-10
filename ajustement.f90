module ajustement
!**********************************************************************
!        MODULE moindres-carrés
!        Recherche itérative des valeur des parametres variables
!        permettant d'approcher le spectre experimental
!        Methode dutilisée : algorithme de Marquardt
!        ref.[1]: D.W. MARQUARDT- J. Soc. Indust. Appl. Math 11, 431 (1963)
!.....................................................................
  use precision
  use options
  use variablesAjustables
  use spectres
  use ecriture
  use algebre
  implicit none
  real(dp)::PH  ! Phi dans [1]
  real(dp)::CRITERE ! critere de convergence
  real(dp)::KHI2   ! ecart statistique de l'ajustement en moindres carrés
  contains
  function convergence(critere)
  ! la fonction qui calcule le KHI**2
    real(dp),intent(in)::critere
    real(dp),save::oldB(40)
!~     real(dp)::oldB(40)
!~     real(dp)::petit=1.0D-12 ! pour eviter la division par zero
    real(dp)::petit=1.0D-5 ! pour eviter la division par zero
    logical::convergence
    if(  maxval(abs(B(1:K)-oldB(1:K))/(abs(B(1:K))+petit)) > CRITERE)  then
      convergence= .FALSE.
    else
      convergence= .TRUE.
    endif
    oldB=B
  end function convergence
  !=====================================================================
  function ajustement_ecart_stat(k,n,spectre_exp,spectre_fit,p)
  ! Calcul du KHI2 
    integer,intent(in)::k
    integer,intent(in)::n
    real(dp),intent(in)::spectre_exp(n)
    real(dp),intent(in)::spectre_fit(n)
    real(dp),intent(in)::p(n) !poids 
    real(dp)::khi2
    real(dp)::ajustement_ecart_stat
    integer::i
    khi2=0.0_dp
    do i=1,n
      if(spectre_exp(i)/=0.0_dp) khi2=khi2+p(i)*(spectre_fit(i) -spectre_exp(i))**2 /spectre_exp(i)
    enddo
    khi2=khi2/(n-k)
    ajustement_ecart_stat=khi2
  end function ajustement_ecart_stat
  !=====================================================================
  subroutine ajustement_moindres_carres(q,n,b,y,k,p,nmax,critere)
   ! Recherche de moindres carrés utilisant l'agorithme de Marquardt
   ! Basée sur la routine en F77 MAMAGT (codes Relfej,Mosfit)
   ! Reference :
   ! [1] D.W. MARQUARDT- J. Soc. Indust. Appl. Math 11, 431 (1963)
      integer,intent(in)::n ! ordre du systeme (taille du spectre)
      integer,intent(in):: k ! nombre de parametres ajustables
      integer,intent(in):: nmax ! nombre maximum d'iterations
      real(dp),intent(in)::critere
      real(dp),intent(inout)::q(n,42) !tableau de travail. Apres calcul du spectre theorique , contient
                                      ! le spectre theorique +  tableau des derivees 
                                      !   (matrice de coefficients de correlation)
      real(dp),intent(inout)::b(k) ! vecteur des parametres ajustables
      real(dp),intent(in)::y(n) ! spectre experimental
      real(dp),intent(in)::p(n) ! poids statistique des canaux
      logical::fin,coupureNmax 
      integer::i,j,l,m,ierr
      integer::iter ! nombre d'itération de l'algorithme de Marquardt effectué
      integer::npas ! nombre de calculs du spectre effectués (nombre d'appel de spectres_theorique_total)
      integer::dump(100)! vecteur de travail pour alsb
      real(dp)::saveB(40)
      real(dp)::nu,lambda,phi
      npas=0
      fin=.FALSE.
      saveB=0.0_dp
      Q=0.0_dp
      VQ(1:K,1:K)=0.0_dp
      nu=5.0_dp
      lambda=0.01_dp
      iter=0
      marquardt: do while(.NOT. fin)
        ! verification du critere de sortie de la boucle---------------- 
        call ecriture_info_iteration(npas,nmax,B)
        coupureNmax = (npas>=nmax) .AND. (npas/=0)  ! au moins un pas doit etre fait, meme dans le cas sparticulier où nmax= 0.
        fin = coupureNmax .OR. convergence(critere)
        npas=npas+1
        ! premier calcul du spectre theorique---------------------------
        call spectres_theorique_total
        if(fin) exit marquardt  ! sortie sans calculer le reste (nmax ou critere atteint)
        !calcul de phi--------------------------------------------------
        PH=0.0_dp
        do i=1,n
          PH= PH + p(i)*(y(i)-q(i,K+2))**2  ! equation (3) de ref.[1]
        enddo
        iter=iter+1
        if(iter >1) then ! si on a passe les deux premieres iterations 
          if( PH < phi )then
            phi = PH
            lambda=lambda/nu 
          else 
              lambda=lambda*nu  ! on revient à lambda precedent ou on fait lambda*nu
              iter=iter-1 ! annule l'incrementation de int
              B(1:K)=saveB(1:K) !on recharge les parametres precedents (i.e. on reste dans l'iteration actuelle)
              call ecriture_info_iteration(npas,nmax,B)
              fin = convergence(critere) .OR. (npas>=nmax)
              npas=npas+1
              call spectres_theorique_total
              if(fin)exit marquardt
          endif
        else
          phi = PH
        endif
        do i=1,K
          Q(i,K+1)=0.0_dp
          do m=1,N
            q(i,K+1)=q(i,K+1)+(y(m)-q(m,K+2))*q(m,i)*p(m)
          enddo
        enddo
        do i=1,K
          do j=1,K
            Q(j,K+2)=0.0_dp
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
        do i=1,K
          if (Q(i,i)==0.0_dp)then 
            ! la fonction est independante du parametre i
            call ecriture_fonction_independante(i)
            do m=1,K
              q(m,K+2)=y(m)
            enddo
            exit marquardt
          endif
          q(i,K+2)=SQRT(q(i,i))
          q(i,K+1)=q(i,K+1)/q(i,K+2)  ! vecteur g* (equation (28) de ref.[1]) 
        enddo
        !creation de la matrice (A* + lambda I)  (equation (32) de ref.[1])
        do i=1,K
          do j=1,K
            if(i==j)then
              q(I,J)=1.0_dp+lambda
            elseif(i<j)then
              q(i,j)=q(i,j)/(q(i,K+2)*q(j,K+2))
            else
              q(i,j)=q(j,i)
            endif
          enddo
        enddo
        !resolution de l'equation (32) de ref.[1]
        call algebre_resoudre_systeme(q,n,K,1,dump,ierr)
        if(ierr/=0) stop 'erreur de resolution de systeme lineraire dans ASLB'
        do i=1,K
          q(i,K+1)=q(i,K+1)/q(i,K+2)
          saveB(i)=b(i)   !sauvegarde de l'ancien vecteur de parametres
          b(i)=b(i)+q(i,K+1)  !  b[r+1] = b[r] +delta[r]
        enddo
    enddo marquardt
  end subroutine ajustement_moindres_carres
  !=====================================================================
  subroutine ajustement_raz
    PH=0.0_dp
    CRITERE = 0.001_dp
  end subroutine ajustement_raz
end module ajustement
