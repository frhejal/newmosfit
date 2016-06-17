!>@file
!***********************************************************************
!        				        MODULE ALGEBRE
!***********************************************************************
!>@brief   Routines de calcul algebrique
!! 
!!@version juin 2016
!**********************************************************************
module algebre
  use precision
  implicit none
  contains
!>@brief Inversion de matrice
!>@details La matrice A doit etre une matrice générale, rangée dans un vecteur.
!!@n Methode: La methode standard de Gauss-Jordan est utilisée. Le determinant est égalemement calculé.
!! Un determinant nul indique que la matrice est singulière.
!!
!!Traduction en Fortran95 de la sous-routine F77 "MINV"
  subroutine algebre_inverser_matrice(A,N,D)
    real(dp),intent(inout):: A(N*N) !<Matrice a inverser,detruite et remplacée par son inverse
    integer,intent(in)    :: N !< Ordre de la matrce A
    real(dp),intent(out)  :: D !< Determinant resultant de l'inversion
    integer               :: I,IJ,IZ,IK,J,JI,JK,JP,JQ,JR,K,KI,KJ,KK,NK
    integer               :: L(N),M(N)
    real(dp)              :: BIGA, HOLD
!=======================================================================
! Recherche de l'élément le plus grand
    D=1.0_dp
    NK=-N
    do K=1,N                 ! Recherche du Kieme pivot
      NK=NK+N
      L(K)=K
      M(K)=K
      KK=NK+K    !KK : position finale du pivot (colonne K, ligne K)
      BIGA=A(KK)
      do J=K,N
        IZ=N*(J-1)  ! IZ debut de la colonne J dans A
        do I=K,N    ! parcours la Jieme colonne de K a N
          IJ=IZ+I   ! IJ emplacement de la case (i,j) dans A
          if(  ABS(A(IJ))  >  ABS(BIGA) ) then
            BIGA=A(IJ)
            L(K)=I              !Le Kieme Pivot a été trouvé à la ligne L(K), colonne M(K)
            M(K)=J
          endif
        enddo
      enddo
  ! Echange des lignes
      J=L(K)
      if((J-K)>0) then
        KI=K-N
        do I=1,N
          KI=KI+N
          HOLD=-A(KI)
          JI=KI-K+J
          A(KI)=A(JI)
          A(JI) =HOLD
        enddo
      endif
  ! Echange des colonnes
      I=M(K)
      if((I-K)>0)then
        JP=N*(I-1)
        do J=1,N
          JK=NK+J
          JI=JP+J
          HOLD=-A(JK)
          A(JK)=A(JI)
          A(JI) =HOLD
        enddo
      endif
  ! Division de la colommn par l'opposé du pivot (Pivot contenu dans BIGA)
      if(ABS(BIGA).LE.1.E-5)then
        D=0.0
        return
      endif
      do I=1,N
        if( (I-K) /=0)then
          IK=NK+I
          A(IK)=A(IK)/(-BIGA)
        endif
      enddo
  ! Reduction de la matrice
      do I=1,N
        IK=NK+I
        HOLD=A(IK)
        IJ=I-N
        do J=1,N
          IJ=IJ+N
          if(  (I-K /=0 ).AND. (J-K /=0) )then
            KJ=IJ-I+K
            A(IJ)=HOLD*A(KJ)+A(IJ)
          endif
        enddo
      enddo
  ! Division de la ligne par le pivot
      KJ=K-N
      do J=1,N
        KJ=KJ+N
        if( (J-K)  /=0 ) A(KJ)=A(KJ)/BIGA
      enddo
  ! Produit de pivot
      D=D*BIGA 
  ! Remplacement du pivot par l'inverse
      A(KK)=1.0/BIGA
    enddo
  ! Deplacement finale des lignes et des colonnes
    K=N-1
    do while(K>0)
      I=L(K)
      if((I-K) >0)then
        JQ=N*(K-1)
        JR=N*(I-1)
        do J=1,N
          JK=JQ+J
          HOLD=A(JK)
          JI=JR+J
          A(JK)=-A(JI)
          A(JI) =HOLD
        enddo
      endif
      J=M(K)
      if((J-K)>0)then
        KI=K-N
        do I=1,N
          KI=KI+N
          HOLD=A(KI)
          JI=KI-K+J
          A(KI)=-A(JI)
          A(JI) =HOLD
        enddo
      endif
      K=(K-1)
    enddo
    return
  end subroutine algebre_inverser_matrice
!=======================================================================
!> @brief Résolution de systèmes linéaires à éléments réèls.
!>@details Méthode du pivot de Gauss.
!! Traduction en Fortran95 de la sous-routine F77 "ALSB"
!!
  subroutine algebre_resoudre_systeme(A,ID,NA,M,K,IER)
    integer,intent(in)::ID !< 1ere dimension du bloc A
    integer,intent(in)::NA  !<ordre du systeme
    integer,intent(in)::M !< nombre de seconds membres
    integer,intent(out)::IER !0 si matrice non singuliere, 1 si singulière
    real(dp),intent(inout),dimension(ID,*)::A!< Matrice et second membres (remplacés par les solutions)
    integer,intent(inout),dimension(100)::K !< K permet de garder la trace des deplacements de ligne effectués
    !variables locales :
    integer::I,I1,I2,I3,IN,it,J,J2,J3,JMAX,KC,MP,N,NAB,NDEB,NM
    real(dp)::AMAX,AUX,ERA,P,S,T
    IER=0
    N=NA
    do I=1,N
      K(I)=I
    enddo
    NDEB=N+1
    NM=N+M
    do i=1,N
! Recherche du pivot maximum
      AMAX=ABS(A(I,I))
      JMAX=I
      I1=I+1
      if(I <= N)then
!   Selectionner dans la ligne I l'indice de colonne JMAX du terme
!   de plus grande valeur absolue A parmi les termes
!   situés a droite de la diagonale.
!   Si , en cours de triangularisation , un terme diagonal est nul
!   ainsi que tous les termes a sa droite et dans sa ligne ,
!   la matrice A est singuliere.
        do J=I1,N
          if (AMAX <= ABS(A(I,J))  ) then
            AMAX=ABS(A(I,J))
            JMAX=J
          endif
        enddo
      endif
! Matrice singulière (pivot nul)
      if(AMAX==0) then
        IER=1
        write(6,*) ' MATRICE SINGULIERE'
        return
      endif
! Transport de la colonne
      if(JMAX>I)then
        do I2=1,N
          AUX=A(I2,I)
          A(I2,I)=A(I2,JMAX)
          A(I2,JMAX)=AUX
        enddo
      endif
!Test sur la singularité de la matrice
      if(I>1)then
        S=0.0
        T=0.0
        IN=I-1
        do IT=1,IN
          P=A(IT,I)*A(I,IT)
          S=S+P
          T=T+ABS(P)
        enddo
        ERA=1.E-6*(T+ABS(A(I,I)-S))
        if(AMAX <= ERA)then
          IER=2
          WRITE (6,*) '  MATRICE QUASI SINGULIERE'
          return
        endif
      endif
!Division par le pivot
      do J2=I1,NM
        A(I,J2)=A(I,J2)/A(I,I)
      enddo
!  Substitution des lignes
!  Diviser les termes de la ligne i situés a droite de la
!   diagonale ainsi que la i-eme composante des seconds membres
!   par le terme de la diagonale
      if(I<N)then
        do I3=I1,N
          do J3=I1,NM
            A(I3,J3)=A(I3,J3)-A(I3,I)*A(I,J3)
          enddo
        enddo
      endif
!     SORTIE INDICES
      if(JMAX>I)then
        NAB=K(JMAX)
        K(JMAX)=K(I)
        K(I)=NAB
      endif
    enddo
    if(N == 1) return ! Pour éviter un plantage de l'agorithme pour I=J-1=0.
                      ! (Comme si on allait résoudre un systeme de rang 1 ...)
! Calcul des solutions
! Résolution du système triangulaire
! Vecteur solution remplace vecteur second membre
      do KC=NDEB,NM
        do J=N, 2, -1
           do I=J-1, 1, -1
              A(I,KC)=A(I,KC)-A(J,KC)*A(I,J)
           enddo
        enddo
      enddo
! classement des solutions
      do I=1,N
        do while(K(I)>I)
          J=K(I)
          K(I)=K(J)
          K(J)=J
          do MP=NDEB,NM
            AUX=A(J,MP)
            A(J,MP)=A(I,MP)
            A(I,MP)=AUX
          enddo
        enddo
      enddo
    end subroutine algebre_resoudre_systeme
  !=====================================================================
!>@brief Calcul des valeurs propres et vecteurs propres d'une matrice complexe triangulaire a(n,n).
!>@details En sortie, les valeurs propres sont sur la diagonale de a.
!! Si mv = 0, les vecteurs prorpes sont calculés et placés dans les colonnes de r. 
!! La matrice est de rang n, elle est stockée colonne par colonne dans un vecteur
!! Traduction en Fortran95 de la sous-routine F77 "CEGREN"
!Exemple: pour n=4, le vecteur  V = [ 1 2 3 4 5 6 7 8 9 10]  
!   exemple: pour n=4, 
!   le vecteur 
!    V = [ 1 2 3 4 5 6 7 8 9 10]  
!   code la matrice 4*4 :
!    A = | 1  2  4  7 |
!        | 0  3  5  8 |
!        | 0  0  6  9 |
!        | 0  0  0 10 |
! 
! les GOTO font reference à l'ancienne version F66 de mosfit, 
! au cas où des courageux voudraient decrypter l'algorithme.
  subroutine algebre_eigenvalues(a,r,n,mv)
    integer,intent(in)::n,mv
    complex(dp),intent(inout)::a(10)
    complex(dp),intent(out)::r(16)
    integer::i,ii,il,ilq,ilr,im,imq,imr,ind,ip,iq
    integer::j,jq,k,l,ll,lm,lq,m,mm,mq,mvk,nn,nnl
    real(dp)::anorm,anrmx,sinus,thr,uw,x,y,yp,yu,yz,z,zu
    complex(dp)::co,cosinus,cp,tmp
    ip=0
    mvk=0
    if(mv/=1)then 
      ! Diagonale de R=1, le reste=0
      r=(0.0_dp,0.0_dp)
      do i=1,n
        ii= (i-1)*n + i
        r(ii)=(1.0_dp , 0.0_dp)
      enddo
    endif
    ! Normalisation du plus grand terme de la diagonale
    do i=1,n
      k=((i+1)*i)/2 !position sur la diagonale
      z=max(abs(a(k)),z)
    enddo
    ! Normalisation du plus grand terme extradiagonal
    zu=abs(a(2))
    do i=1,n-1
      do j=i+1,n
        k=i+(j*j-j)/2 
        zu=max(abs(a(k)),zu)
      enddo
    enddo
    if(zu/=0.0_dp)then
      y=1.D30
      nn=((n-1)*n)/2 ! nbre element non diagonaux
      nnl=nn+n       ! nbre elements dans a
      yz=real(nn,dp)
      yu=1D37/sqrt(yz)
      uw=1.0D-37/sqrt(yz)
      if(z>=1.0D-44)then
        z=z/y
        if(zu>=uw) zu=zu/yu
      else
        if(zu<uw)then
          mvk=mvk+1
          do k=1,nnl
            a(k)=a(k)*1.0D50
          enddo
          z=z*1.0D50
          zu=zu*1.0D50
          z=z/y
          zu=zu/yu
          z=max(z,zu)
        else
          z=zu/yu
        endif
      endif
      mvk=mvk+1
      do k=1,nnl
        a(k)=a(k)/z
      enddo
      anorm=0.0_dp
      do i=1,n
        do j=i+1,n
          k=i+(j*j-j)/2 
          anorm=anorm+abs(a(k))*abs(a(k))
        enddo
      enddo
      anorm=ROOT2*sqrt(anorm)
!~       anorm=1.414*sqrt(anorm)
      anrmx=anorm*1.0D-7/real(n,dp)
      ind=0
      thr=anorm
    outer: do while(thr>anrmx)
        thr=thr/real(n,dp)
        inner: do while(ind==0)
          L=1
          LQ=0
          do while(l/=n)
            m=l+1
            lq=lq+l-1
            do while(m/=n+1)
              mq=(m*m-m)/2
              lm=l+mq
              x=abs(a(lm))
              if(abs(a(lm))<thr)then
                ip=ip+1
                if(ip>=nn) ind=0
              else !goto 65
                ind=1
                ll=l+lq
                mm=m+mq
                x=0.5_dp*real(A(ll)-a(mm),dp)
                y=sqrt(x*x + abs(a(lm))*abs(a(lm)))
                if(x<0)then     !goto 70
                  co=a(lm)/(x-y)
                elseif(x>0)then !goto 76
                  co=a(lm)/(x+y)
                else            !goto 75
                  cp=a(lm)/abs(a(lm))
                endif           !goto 81
                yp=1.0_dp + abs(co)*abs(co)
                sinus=1.0_dp/sqrt(yp)
                cosinus=co*sinus
                if(abs(cosinus)==0.0_dp)then
                  ip=ip+1
                else !goto 83
                  ip=0
                  ilq=n*(l-1)
                  imq=n*(m-1)
                  do i=1,n
                    iq=(i*i-i)/2
                    if(i<l)then !goto 80
                      im=i+mq
                      il=i+lq
                      tmp=sinus*a(il) + conjg(cosinus)*a(im)
                      a(im)=-cosinus*a(il) + sinus*a(im)
                      a(il)=tmp
                    elseif(i>l)then   !goto 96
                      if(i<m)then     !goto 85
                        im=i+mq
                        il=l+iq
                        tmp=sinus*a(il) + cosinus*conjg(a(im))
                        a(im)=-cosinus*conjg(a(il)) + sinus*a(im)
                        a(il)=tmp  
                      elseif(i>m)then !goto 86
                        im=m+iq
                        il=l+iq
                        tmp=sinus*a(il) + cosinus*a(im)
                        a(im)=-conjg(cosinus)*a(il) + sinus*a(im)
                        a(il)=tmp
                      endif
                    endif !label 115
                    if(mv/=1)then !goto 120
                      ilr=ilq+i
                      imr=imq+i
                      tmp=r(ilr)*sinus+r(imr)*conjg(cosinus)
                      r(imr)=-r(ilr)*cosinus + r(imr)*sinus
                      r(ilr)=tmp
                    endif
                  enddo !goto 125
                  y=(real(a(ll),dp) +  real(a(mm),dp)*(yp-1.0_dp) + 2.0_dp*real(conjg(co)*a(lm)))/yp
                  x=(real(a(mm),dp) +  real(a(ll),dp)*(yp-1.0_dp) - 2.0_dp*real(conjg(co)*a(lm)))/yp
                  a(lm)=(a(lm)-co*co*conjg(a(lm)) +co*(a(mm)-a(ll)))/yp
                  a(ll)=cmplx(y,0.0_dp,dp)
                  a(mm)=cmplx(x,0.0_dp,dp)
                endif
              endif
              !label 130
              m=m+1
            enddo
            !label 140
            l=l+1
          enddo
          !label 150
          ! inversion de la condition d'arret de la boucle 
          ! (voilà ce qui se passe quand on traduit les gotos de f66)
          if(ind==1)then 
            ind = 0
          else
            exit inner  
          endif
        enddo inner
        ! label 160
        if(sinus==1.0_dp) exit outer ! Quelles chances que ça arrive ?
      enddo outer
    endif
    ! label 165
    iq=-n
    ! rangement des valeurs propres dans l'ordre croissant
    !les vp sont sur la diagonale de a.
    do i=1,n
      iq=iq+n
      ll=(i*i+i)/2
      jq=n*(i-2)
      do j=i,n
        jq=jq+n
        mm=(j*j+j)/2
        if(real(a(ll),dp) < real(a(mm),dp))then
          tmp=a(ll)
          a(ll)=a(mm)
          a(mm)=tmp
          if(mv/=1)then
            do k=1,n
              ilr=iq+k
              imr=jq+k
              tmp=r(ilr)
              r(ilr)=r(imr)
              r(imr)=tmp
            enddo
          endif
        endif
      enddo
    enddo !goto 185
    if(mvk/=0)then
      if(mvk/=1) a=a*1.0D-50
      a=a*z
    endif
  end subroutine algebre_eigenvalues
  !=====================================================================
  !>@brief Recopie les valeurs d'une matrice dans un vecteur, colonne par colonne.
  subroutine algebre_matrice_vers_vecteur(mat,vec,m,n)
    real(dp),intent(in)::mat(:,:)
    real(dp),intent(out)::vec(:)
    integer,intent(in)::m !< nombre de lignes de la matrice
    integer,intent(in)::n !< nombre de colonnes de la matrice
    integer::i,j,ij
    if(m>size(mat,1).OR.n>size(mat,2)) stop "Au moins une dimension de la matrice est plus petite que specifié"
    if(m*n> size(vec,1)) stop "La matrice est trop grande pour etre contenue dans ce vecteur"
    ij=0
    do j=1,n
      do i=1,m
        ij=ij+1
        vec(ij)=mat(i,j)
      enddo
    enddo
  end subroutine algebre_matrice_vers_vecteur
  !=====================================================================
  !>@brief  Recopie les valeurs d'un vecteur dans une matrice, colonne par colonne.
  subroutine algebre_vecteur_vers_matrice(vec,mat,m,n)
    real(dp),intent(in)::vec(:)
    real(dp),intent(out)::mat(:,:)
    integer,intent(in)::m !< nombre de lignes de la matrice
    integer,intent(in)::n !< nombre de colonnes de la matrice
    integer::i,j,ij
    if(m>size(mat,1).OR.n>size(mat,2)) stop "Au moins une dimension de la matrice est plus petite que specifié"
    if(m*n> size(vec,1)) stop "Le vecteur be correspond pas aux dimensions données pour la matrice"
    ij=0
    do j=1,n
      do i=1,m
        ij=ij+1
        mat(i,j)=vec(ij)
      enddo
    enddo
  end subroutine algebre_vecteur_vers_matrice
end module algebre
