!**********************************************************************
!        MODULE INVERSION, CONTAINS SUBROUTINE MINV(A,N,D)                                        
!                                                                      
!           INVERT A MATRIX                                            
!                                                                      
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
!     ..................................................................
MODULE ALGEBRE
  
  CONTAINS 
  SUBROUTINE MINV(A,N,D)                                           
  DIMENSION A(1600)                                                 
  DIMENSION L(40),M(40)                                            
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
          L(K)=I              !Le Kieme Pivot a été trouvé à la ligne L(K), colonne M(k)
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
!     D=D*BIGA                                                         
!                                                                      
!        REPLACE PIVOT BY RECIPROCAL                                   
!                                                                      
    A(KK)=1.0/BIGA                                                   
  ENDDO                                                       
!                                                                      
!        FINAL ROW AND COLUMN INTERCHANGE                              
!                                                                      
  K=N-1                                                              
  WHILE( K>=0)
    I=L(K)                                                           
    IF(I-K >0)THEN
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
      END SUBROUTINE
      END MODULE                                                             
!                                                                      
!**********************************************************************
!                                                                      
