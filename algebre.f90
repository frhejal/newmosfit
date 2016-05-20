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
!     ..................................................................
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
    DO K=1,N                 ! Recherche du Kieme pivot                                    
      NK=NK+N                                                          
      L(K)=K                                                           
      M(K)=K                                                           
      KK=NK+K    !KK : position finale du pivot (colonne K, ligne K)                                          
      BIGA=A(KK)                                                       
      DO J=K,N                                                      
        IZ=N*(J-1)  ! IZ debut de la colonne J dans A                                                     
        DO I=K,N    ! parcours la Jieme colonne de K a N
          IJ=IZ+I   ! IJ emplacement de la case (i,j) dans A                                                       
          IF(  ABS(A(IJ))  >  ABS(BIGA) ) THEN 
            BIGA=A(IJ)                                                       
            L(K)=I              !Le Kieme Pivot a été trouvé à la ligne L(K), colonne M(K)
            M(K)=J  
          ENDIF
        ENDDO
      ENDDO
  !        INTERCHANGE ROWS                                              
      J=L(K)                                                           
      IF((J-K)>0) THEN
        KI=K-N                                                           
        DO I=1,N                                                      
          KI=KI+N                                                          
          HOLD=-A(KI)                                                      
          JI=KI-K+J                                                        
          A(KI)=A(JI)                                                      
          A(JI) =HOLD 
        ENDDO
      ENDIF
  !        INTERCHANGE COLUMNS                                           
      I=M(K)
      IF((I-K)>0)THEN
        JP=N*(I-1)                                                       
        DO J=1,N                                                      
          JK=NK+J                                                          
          JI=JP+J                                                          
          HOLD=-A(JK)                                                      
          A(JK)=A(JI)                                                      
          A(JI) =HOLD                                                      
        ENDDO
      ENDIF
  !        DIVIDE COLUMN BY MINUS PIVOT (VALUE OF PIVOT ELEMENT IS       
  !        CONTAINED IN BIGA)                                            
      IF(ABS(BIGA).LE.1.E-5)THEN
        D=0.0                                                            
        RETURN                                                           
      ENDIF
      DO I=1,N                                                      
        IF( (I-K) /=0)THEN
          IK=NK+I
          A(IK)=A(IK)/(-BIGA)
        ENDIF
      ENDDO
  !        REDUCE MATRIX                                                 
      DO I=1,N                                                      
        IK=NK+I                                                          
        HOLD=A(IK)                                                       
        IJ=I-N                                                           
        DO J=1,N                                                      
          IJ=IJ+N                                                          
          IF(  (I-K /=0 ).AND. (J-K /=0) )THEN
            KJ=IJ-I+K                                                        
            A(IJ)=HOLD*A(KJ)+A(IJ)
          ENDIF                                            
        ENDDO                              
      ENDDO
  !        DIVIDE ROW BY PIVOT                                           
      KJ=K-N                                                           
      DO J=1,N                                                      
        KJ=KJ+N                                                          
        IF( (J-K)  /=0 ) A(KJ)=A(KJ)/BIGA
      ENDDO                                               
  !     PRODUCT OF PIVOTS                                             
  !     D=D*BIGA        ! Aucune idee de l'utilite de ce truc                                                 
  !                                                                      
  !        REPLACE PIVOT BY RECIPROCAL                                   
  !                                                                      
      A(KK)=1.0/BIGA                                                   
    ENDDO                                                       
  !                                                                      
  !        FINAL ROW AND COLUMN INTERCHANGE                              
  !      
    K=N-1                                                              
    DO WHILE(K>0)
      I=L(K)                                                           
      IF((I-K) >0)THEN
        JQ=N*(K-1)                                                       
        JR=N*(I-1)                                                       
        DO J=1,N                                                     
          JK=JQ+J                                                          
          HOLD=A(JK)                                                       
          JI=JR+J          
          A(JK)=-A(JI)                                                     
          A(JI) =HOLD   
        ENDDO
      ENDIF
      J=M(K)                                                           
      IF((J-K)>0)THEN
        KI=K-N                                                           
        DO I=1,N                                                     
          KI=KI+N                                                          
          HOLD=A(KI)                                                       
          JI=KI-K+J                                                        
          A(KI)=-A(JI)                                                     
          A(JI) =HOLD                                                      
        ENDDO
      ENDIF
      K=(K-1)                                                          
    ENDDO
    RETURN                                                           
  end subroutine minv
!..................................................................
!        SUBROUTINE ALSB
!   RESOLUTION DE SYSTEMES LINEAIRES A ELEMENTS REELS                  
!   A  MATRICE ET 2NDS MEMBRES (REMPLACES PAR LES SOLUTIONS)           
!   ID 1ERE DIMENSION DU BLOC A                                        
!   NA ORDRE DU SYSTEME                                                
!   M  NOMBRE DE 2NDS MEMBRES                                          
!   K  BLOC DE TRAVAIL                                                 
!    IER 0 SI MATRICE NON SINGULIERE. 1 SI MATRICE SINGULIERE
!      INTEGER NM NDEB      
!        Translated from F77 by F.L, may 2016          
  subroutine alsb(A,ID,NA,M,K,IER)
    integer,intent(in)    ::ID,NA,M,K
    integer,intent(out)   ::IER
    real(dp),intent(inout)::A(ID,:)
    
!~  SUBROUTINE ALSB(A,ID,NA,M,K,IER)                                       
!~       DIMENSION A(ID,1),K(1)                                           
!~       COMMON/UNIT/NOUT,NOUT1
!~       IER=0                                                            
!~       N=NA                                                             
!~       DO 1  I=1,N                                                      
!~     1 K(I)=I                                                           
!~       NDEB=N+1                                                         
!~       NM=N+M                                                           
!~       DO 10 I=1,N                                                      
!~ C     RECHERCHE DU PIVOT MAXIMUM                                       
!~       AMAX=ABS(A(I,I))                                                 
!~       JMAX=I                                                           
!~       I1=I+1                                                           
!~       IF(I-N)2000,2001,2001                                            
!~  2000 DO 11 J=I1,N                                                     
!~       IF(AMAX-ABS(A(I,J)))12,12,11                                     
!~    12 AMAX=ABS(A(I,J))                                                 
!~       JMAX=J                                                           
!~    11 CONTINUE                                                         
!~ C     MATRICE SINGULIERE                                               
!~  2001 IF(AMAX)13,13,300                                                
!~    13 IER=1                                                            
!~       WRITE (NOUT,199)                                                     
!~ 199   FORMAT(' MATRICE SINGULIERE')                                    
!~       RETURN                                                           
!~ C     TRANSPORT DE LA COLONNE                                          
!~   300 IF(JMAX-I)301,301,14                                             
!~    14 DO 15 I2=1,N                                                     
!~       AUX=A(I2,I)                                                      
!~       A(I2,I)=A(I2,JMAX)                                               
!~       A(I2,JMAX)=AUX                                                   
!~    15 CONTINUE                                                         
!~   301 IF(I-1) 60,60,61                                                 
!~ C     TEST SUR LA SINGULARITE DELA MATRICE                             
!~    61 S=0.0                                                            
!~       T=0.0                                                            
!~       IN=I-1                                                           
!~       DO 62 IT=1,IN                                                    
!~       P=A(IT,I)*A(I,IT)                                                
!~       S=S+P                                                            
!~    62 T=T+ABS(P)                                                       
!~       ERA=1.E-6*(T+ABS(A(I,I)-S))                                      
!~       IF(AMAX-ERA) 63,63,60                                            
!~    63 IER=2                                                            
!~       WRITE (NOUT,198)                                                     
!~ 198   FORMAT('  MATRICE QUASI SINGULIERE')                             
!~       RETURN                                                           
!~ C     DIVISION PAR LE PIVOT                                            
!~    60 DO 16 J2=I1,NM                                                   
!~       A(I,J2)=A(I,J2)/A(I,I)                                           
!~    16 CONTINUE                                                         
!~ C     SUBSTITUTION DES LIGNES                                          
!~       IF(I-N)2002,2003,2003                                            
!~ 2002  DO 20 I3=I1,N                                                    
!~       DO 19 J3=I1,NM                                                   
!~       A(I3,J3)=A(I3,J3)-A(I3,I)*A(I,J3)                                
!~    19 CONTINUE                                                         
!~    20 CONTINUE                                                         
!~ C     SORTIE INDICES                                                   
!~  2003 IF(JMAX-I)10,10,311                                              
!~   311 NAB=K(JMAX)                                                      
!~       K(JMAX)=K(I)                                                     
!~       K(I)=NAB                                                         
!~    10 CONTINUE                                                         
!~       IF(N.EQ.1) RETURN                                                
!~ C     CALCUL DES SOLUTIONS                                             
!~       DO 70 KC=NDEB,NM                                                 
!~       J=N                                                              
!~    52 I=J-1                                                            
!~    50 A(I,KC)=A(I,KC)-A(J,KC)*A(I,J)                                   
!~       I=I-1                                                            
!~       IF(I)51,51,50                                                    
!~    51 J=J-1                                                            
!~       IF(J-1)70,70,52                                                  
!~    70 CONTINUE                                                         
!~ C     CLASSEMENT DES SOLUTIONS                                         
!~       DO 120 I=1,N                                                     
!~   100 J=K(I)                                                           
!~       IF(J-I)120,120,130                                               
!~   130 K(I)=K(J)                                                        
!~       K(J)=J                                                           
!~       DO 110 MP=NDEB,NM                                                
!~       AUX=A(J,MP)                                                      
!~       A(J,MP)=A(I,MP)                                                  
!~       A(I,MP)=AUX                                                      
!~   110 CONTINUE                                                         
!~       GO TO 100                                                        
!~   120 CONTINUE                                                         
!~   500 RETURN                                                           
!~       END                                                    
      end subroutine alsb
end module     
!**********************************************************************
!                                                                      
