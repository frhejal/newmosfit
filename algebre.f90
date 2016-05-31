!**********************************************************************
!        MODULE ALGEBRE
!        Contient les sousroutines MINV et ALSB
!           MINV : Inverse une matrice N x N rangée dans un vecteur A
!           ALSB : resoud un systeme lineaire
!     ..................................................................
module algebre
  use precision
  implicit none
  contains
!---------------------------------------------------------------------
!        SUBROUTINE MINV
!        DESCRIPTION OF PARAMETERS
!           A - INPUT MATRIX, DESTROYED IN COMPUTATION AND REPLACED BY
!               RESULTANT INVERSE.
!           N - ORDER OF MATRIX A
!           D - RESULTANT DETERMINANT
!           L - WORK VECTOR OF LENGTH N
!           M - WORK VECTOR OF LENGTH N
!
!        REMARKS
!           MATRIX A MUST BE A GENERAL MATRIX
!        METHOD
!           THE STANDARD GAUSS-JORDAN METHOD IS USED. THE DETERMINANT
!           IS ALSO CALCULATED. A DETERMINANT OF ZERO INDICATES THAT
!           THE MATRIX IS SINGULAR.
!        Translated from F77 by F.L, may 2016
  subroutine minv(A,N,D)
    integer,intent(in)    :: N
    real(dp),intent(inout):: A(N*N)
    real(dp),intent(out)  :: D
    integer               :: I,IJ,IZ,IK,J,JI,JK,JP,JQ,JR,K,KI,KJ,KK,NK
    integer               :: L(N),M(N)
    real(dp)              :: BIGA, HOLD
!        ...............................................................
!        SEARCH FOR LARGEST ELEMENT
!
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
  !        INTERCHANGE ROWS
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
  !        INTERCHANGE COLUMNS
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
  !        DIVIDE COLUMN BY MINUS PIVOT (VALUE OF PIVOT ELEMENT IS
  !        CONTAINED IN BIGA)
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
  !        REDUCE MATRIX
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
  !        DIVIDE ROW BY PIVOT
      KJ=K-N
      do J=1,N
        KJ=KJ+N
        if( (J-K)  /=0 ) A(KJ)=A(KJ)/BIGA
      enddo
  !     PRODUCT OF PIVOTS
  !     D=D*BIGA        ! Aucune idee de l'utilite de ce truc
  !
  !        REPLACE PIVOT BY RECIPROCAL
  !
      A(KK)=1.0/BIGA
    enddo
  !
  !        FINAL ROW AND COLUMN INTERCHANGE
  !
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
  end subroutine minv
!---------------------------------------------------------------------
!        SUBROUTINE ALSB
!   RESOLUTION DE SYSTEMES LINEAIRES A ELEMENTS REELS
!   METHODE DU PIVOT DE GAUSS.
!   A  MATRICE ET 2NDS MEMBRES (REMPLACES PAR LES SOLUTIONS)
!   ID 1ERE DIMENSION DU BLOC A
!   NA ORDRE DU SYSTEME
!   M  NOMBRE DE 2NDS MEMBRES
!   K  BLOC DE TRAVAIL
!    IER 0 SI MATRICE NON SINGULIERE. 1 SI MATRICE SINGULIERE
!      INTEGER NM NDEB
!        Translated from F66/F77 by F.L, may 2016
  subroutine alsb(A,ID,NA,M,K,IER)
    integer,intent(in)    ::ID,NA,M
    integer,intent(out)   ::IER
    real(dp),intent(inout),dimension(ID,*)::A
    integer,intent(inout),dimension(100)::K ! K permet de garder la trace des deplacements de ligne effectués
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
!     RECHERCHE DU PIVOT MAXIMUM
      AMAX=ABS(A(I,I))
      JMAX=I
      I1=I+1
      if(I <= N)then
        do J=I1,N
          if (AMAX <= ABS(A(I,J))  ) then
            AMAX=ABS(A(I,J))
            JMAX=J
          endif
        enddo
      endif
!     MATRICE SINGULIERE (PIVOT NUL)
      if(AMAX==0) then
        IER=1
        write(6,*) ' MATRICE SINGULIERE'
        return
      endif
!     TRANSPORT DE LA COLONNE
      if(JMAX>I)then
        do I2=1,N
          AUX=A(I2,I)
          A(I2,I)=A(I2,JMAX)
          A(I2,JMAX)=AUX
        enddo
      endif
!     TEST SUR LA SINGULARITE DELA MATRICE
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
!     DIVISION PAR LE PIVOT
      do J2=I1,NM
        A(I,J2)=A(I,J2)/A(I,I)
      enddo
!     SUBSTITUTION DES LIGNES
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
    if(N == 1) return ! Pour eviter un plantage de l'agorithme pour I=J-1=0.
                      ! (Comme si on allait résoudre un systeme de rang 1 ...)
!   CALCUL DES SOLUTIONS
      do KC=NDEB,NM
        do J=N, 2, -1
           do I=J-1, 1, -1
              A(I,KC)=A(I,KC)-A(J,KC)*A(I,J)
           enddo
        enddo
      enddo
!     CLASSEMENT DES SOLUTIONS
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
    end subroutine alsb
  !---------------------------------------------------------------------
  subroutine cegren(a,r,n,mv)
  ! calcul des valeurs propres et vecteurs propres d'une matrice complexe triangulaire
  ! la matrice est stockée colonne par colonne dans un vecteur
  !   exemple:
  !   le vecteur 
  !    V = [ 1 2 3 4 5 6 7 8 9 10]  
  !   code la matrice
  !    A = | 1  2  4  7 |
  !        | 0  3  5  8 |
  !        | 0  0  6  9 |
  !        | 0  0  0 10 |
    integer,intent(in)::n,mv
    complex(dp),intent(inout)::a(10)
    complex(dp),intent(out)::r(16)
    integer::i,ii,il,ilq,ilr,im,imq,imr,ind,ip,iq
    integer::j,k,l,ll,lm,lq,m,mm,mq,mvk,nn,nnl
    real(dp)::anorm,anrmx,sinus,thr,uw,x,y,yp,yu,yz,z,zu
    complex(dp)::co,cosinus,cp,tmp
    ip=0
    mvk=0
    if(mv/=1)then 
      ! diagonale de R=1, le reste=0
      r=(0.0_dp,0.0_dp)
      do i=1,n
        ii= (i-1)*n + i
        r(ii)=(1.0_dp , 0.0_dp)
      enddo
    endif
    !normalisation du plus grand terme de la diagonale
    do i=1,n
      k=((i+1)*i)/2 !position sur la diagonale
      z=max(abs(a(k)),z)
    enddo
    !normalisation du plus grand terme extradiagonal
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
      anorm=sqrt(2.0_dp)*sqrt(anorm)
      anrmx=anorm*1.0D-7/real(n,dp)
      ind=1
      thr=anorm
    outer: do while(thr>anrmx)
      thr=thr/real(n,dp)
      do while(ind==1)
        ind=0
        L=1
        LQ=0
        do while(l/=n)
          m=l+1
          lq=lq+l-1
          do while(m<=n)
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
              if(abs(cosinus)==0)then
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
                  endif !goto 115
                  if(mv/=0)then !goto 120
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
            !goto 130
            m=m+1
          enddo
          !label 140
          l=l+1
        enddo
        !label 150
      enddo
      ! label 160
      if(sinus==1.0_dp) exit outer ! quelle proba que ça arrive ?
      enddo outer
    endif
    ! label 165
  end subroutine cegren
  
end module algebre
!**********************************************************************
!
