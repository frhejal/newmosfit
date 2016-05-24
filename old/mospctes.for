c********************************************************************
C         FITTAGE THEORIQUE DE SPECTRES MOSSBAUER  FER 57              
C                     VERSION  FEV  86                                 
C**********************************************************************
C                                                                      
C MODIFICATIONS********************************************************
C                                                                      
C INDIQUER DATE  NOM  MODIFICATION                                     
C                                                                      
CM1  OCT 83 J.T  SORTIE BENSON                                         
CM2  JAN 84 JMG  CONVOLUTION                                           
CM3  FEV 84 JMG  D/A
CM4  FEV 84 J.T  SORTIE BENSON PAR X0,G,H                              
CM5  MAI 84 J.T  PRECISION PARAMETRES                                  
CM6  JAN 85 J.T  PERMUTATION LIGNES 939 ET 940 POUR SORTIE CORRECTE    
C                DES LARGEURS AVEC IOGV                                
CM7  AOU 85 JMG  MODIF VALEURS STANDARDS                               
CM9  DEC 85 JMG  SORTIE BENSON SERIE DE SPECTRES
C    JAN 94  NR  SORTIE DES SPECTRES SOUS MATLAB 4.0 IBM RISC/6000                       
C    FEV 94  NR  TRACE DES SOUS-SPECTRES 
C    MAR 95  YL  valeur max de l'exponentielle dans convol                                                                   
C   
C********************OPTIONS*******************************************
C    IO(1)=N  AJOUT N MILLIONS
C    IO(2)=1  TRACE SUR LARGEUR 12OCX                                  
C    IO(3)=1  SN119                                                    
C    IO(4)=1  HBRUIT NON AJUSTABLE                                     
C          2         AJUSTABLE                                         
C    IO(5)=N  CONNEXIONS PARAMETRES                                    
C    IO(6)=1  PERFORATION YEXP-YCALC                                   
C    IO(7)=1  PERFORATION YCALC                                        
C    IO(8)=1  X0,G,H,MATRICE DE VARIANCE COVARIANCE
C    IO(9)=1  BETA=TETA ; ALFA=GAMA
C    IO(10)=1  PAS DE SPECTRE EXPERIMENTAL
C           2  MEME SPECTRE EXP QUE CAS PRECEDENT
C    IO(11)=1  TRACE YEXP YCALC
C    IO(12)=1  SORTIE BENSON
C    IO(13)=1  DISTRIBUTION
C    IO(14)=1  DISTRIBUTION
C    IO(16)=N  (N=NBRE DE SOUS CANAUX)  CONVOLUTION GAUSS*LORENZ
C    IO(20)=1  HORIZONTALISATION FOND CONTINU
C    IO(17)=1  TRACE DES SOUS-SPECTRES  
C**********************************************************************
C
C                  P R O G R A M M E    M O S F I T
C
C**********************************************************************
c~       PROGRAM MOSFIT
      USE PRECISION
      IMPLICIT real(dp) (a-h,o-z)
      IMPLICIT integer (i-n)
      EXTERNAL CALC
      DIMENSION PRO(16),PRO1(16),NG(8),GV(8),ALIG1(18),ALIG2(18)
      DIMENSION AA(1600),S(40),SL(42)
      integer GRASS(10)
      CHARACTER*13 FICH
      COMMON/UN/Q(256,42),Y(256),B(40),BF(256),HBRUIT,N,K,NS
      COMMON/COMP/NPAS,NMAX,NS1,NS2
      COMMON/DEUX/P(256),TY,IZ(10)
      COMMON/OPT/IO(20)
      COMMON/LECT/IYE(512),IY(256),IY1(256)
      COMMON/UNIT/NOUT,NOUT1
      COMMON/TH/BT(10,40),GVT(8,40),NBT(10,40),NGT(8,40),MONOT(40),
     1 IOGVT(40)
      COMMON/GRA/AINT(8),ENE(8),GR(256),CN
      COMMON/ADD/IAD(10,40),IADG(8,40)
      COMMON/RAIES/H(8,40),G(8,40),X0(8,40)
      COMMON/PARHYP/DI,GA,H1,SQ,CH,TETA,ETA,GAMA,BETA,ALFA,MONOC,E
      COMMON/PARSUP/DI0,PDI,SQ0,PSQ,CH0,PCH,TETA0,PTETA,ETA0,GAMA0,
     1 BETA0,ALFA0,MONOC0,NB0(10),NB(10)
      COMMON/VIR/PH1,VQ(40,40),ETBT(10,40),ETGVT(8,40)
      COMMON/SOSP/YSS(256,5), ICOURB
      NOUT=6
      NOUT1=13
c      open(9,file='fort.9',status='unknown',form='formatted')
      OPEN(nout,file='fit.out',status='unknown',form='formatted')
      open(15,file='RESULTAT.doc',status='unknown',form='formatted')
   29 FORMAT (16A4)
   37 FORMAT ('1')
   38 FORMAT ('1','   COMPOSANTE  DONNEE  ',///)
      WRITE (NOUT,1000)
1000  FORMAT (' VERSION FEV 83 ')
      READ(5,29) PRO
      WRITE(9,29) PRO
      WRITE (NOUT,29) PRO
      WRITE(15,29) PRO
C   REMISES A ZERO
      CALL RAZ
      READ(5,*) CN,NMAX,NS,NS1,NS2,IZZ,IOPT,HBRUIT
      IF(IZZ.EQ.1) THEN
      READ(5,*) (IZ(I),I=1,10)
      PRINT *, ' CANAUX SUPPRIMES = ',(iz(i),i=1,10)
      ELSE
      END IF
      IF(IOPT.EQ.1) THEN
      READ(5,*) (IO(I),I=1,20)
      PRINT *, ' OPTIONS UTILISEES = ',(io(i),i=1,20)
      ELSE
      END IF
      PRINT *, 'VITESSE PAR CANAL=', CN, ' NBRE DE COMPOSANTES =',NS
      PRINT *, 'NBRE MAX ITERATIO=', NMAX
      IF(IO(4).NE.0) PRINT *, 'IL Y A UN SPECTRE DE BRUIT'
      IF(IO(17).NE.0) THEN
      READ(5,*) (GRASS(I),I=1,10)
      ELSE
      ENDIF
      IF(NS1.NE.0) PRINT *,'DISTRIBUTION ENTRE NS1=',ns1,' ET NS2 =',ns2
      IF(NS.LE.40) GO TO 3
      WRITE (NOUT,11)
 11   FORMAT(1X,'  NOMBRE DE SOUS SPECTRES(NS) SUPERIEUR A 40 ')
 3    WRITE(NOUT,61)
 61   FORMAT('   DI          GA          H1        SQ        CH 
     *      ETA      TETA      GAMA      BETA      ALFA    MONOC ')
      DO 102 NT=1,NS
      MONOC=0
      IOGV=0
      DO 112 I=1,8
  112 NG(I)=0
      DO 98 I=1,10
   98 NB(I)=0
      IF(NT.NE.NS1) GO TO 5
      READ(5,*) DI0,PDI,GA,H1,SQ0,PSQ,CH0,PCH,ETA0,TETA0,PTETA,GAMA0
     * ,BETA0,ALFA0,MONOC0
      READ(5,*) (NB0(I),I=1,10)
    5 IND=(NT-NS1)*(NT-NS2)
      IF(IND.LE.0) CALL SUPER(NT,*110)
      IF(IO(10).NE.2) CALL RAZHYP
      READ(5,*) DI,GA,H1,SQ,CH,ETA,TETA,GAMA,BETA,ALFA,MONOC
      WRITE(NOUT,60) DI,GA,H1,SQ,CH,ETA,TETA,GAMA,BETA,ALFA,MONOC
60    FORMAT(2(F8.3,2X),F11.2,7(2X,F8.3),3X,I4)
      READ(5,*) (NB(I),I=1,10),IOGV
      WRITE(NOUT,62)(NB(I), I=1,10)
62    FORMAT(10(5X,I3,2X))     
      IF(IOGV.EQ.0) GO TO 40
      DO 111 I=1,8
  111 GV(I)=GA
 40   CONTINUE
      IF((IOGV.EQ.1).OR.(IOGV.EQ.2))NB(2)=0
C     MISE EN TABLEAU DES PARAMETRES HYPERFINS
  110 BT(1,NT)=DI
      BT(2,NT)=GA
      BT(3,NT)=H1
      BT(4,NT)=SQ
      BT(5,NT)=CH
      BT(6,NT)=ETA
      BT(7,NT)=TETA
      BT(8,NT)=GAMA
      BT(9,NT)=BETA
      BT(10,NT)=ALFA
      MONOT(NT)=MONOC
      IOGVT(NT)=IOGV
      DO 113  I=1,10
      NBT(I,NT)=NB(I)
      IF(NB(I).NE.1) GO TO 114
      IAD(I,NT)=K
      B(K)=BT(I,NT)
      K=K+1
 114  IF(NB(I).NE.2) GO TO 113
      NTM1=NT-1
      IAD(I,NT)=IAD(I,NTM1)
 113  CONTINUE
C     MISE EN PLACE DES OPTIONS CONCERNANT LES LARGEURS VARIABLES
      IF(IOGV.EQ.0) GO TO 300
      GO TO (301,302,300),IOGV
 301  NG(1)=1
      NG(3)=1
      GO TO 300
 302  NG(2)=1
      NG(3)=1
      NG(4)=1
 300  CONTINUE
C     MISE EN TABLEAU DES LARGEURS VARIABLES
      DO 117 I=1,8
      NGT(I,NT)=NG(I)
      GVT(I,NT)=GV(I)
      IF(NG(I).EQ.0) GO TO 117
      IADG(I,NT)=K
      B(K)=GVT(I,NT)
      K=K+1
 117  CONTINUE
 102  CONTINUE
C     CONTRIBUTION DE PROFIL DONNE PAR CARTES
      IF(IO(4).EQ.0) GO TO 2
      IF(IO(4).EQ.1) GO TO 119
      B(K)=HBRUIT
      K=K+1
 119  WRITE (NOUT,38)
      READ(5,29) PRO1
      PRINT *, PRO1
      CALL LECTU(2)
      BRUMOY=0.
      DO 116 J=1,10
 116  BRUMOY=BRUMOY+BF(J)/10.
      DO 118 J=1,N
 118  BF(J)=BF(J)-BRUMOY
 2    IF(K.LE.40) GO TO 4
      WRITE (NOUT,14)
 14   FORMAT(1X,'  NOMBRE DE PARAMETRES AJUSTABLES SUPERIEUR A 30  ')
 4    CONTINUE
c~         write(6,*)"NPAS=", NPAS
      IF(IO(10).EQ.0) CALL LECTU(1)
      DO 100 KI=1,N
      Y(KI)=Y(KI)+FLOAT(IO(1))*1.0E+6
  100 CONTINUE
 105  IF(IO(10).NE.1) GO TO 106
      DO 99 I=1,N
 99   Y(I)=0.
  106 CONTINUE 
      IF(TY.EQ.0.) CALL NIVZER
      IF(IZ(1).NE.0) CALL POIDS
      B(K)=TY
      IF(NMAX.EQ.0)  CALL CALC(E,*122)
      IF(NMAX.NE.0) CALL MAMAGT (Q,256,B,Y,N,K,E,CALC,P)
      III=0
      DO 420 J=1,K
      DO 420 I=1,K
      III=III+1
      AA(III)=VQ(I,J)
 420  CONTINUE
      CALL MINV(AA,K,D)
      III=0
      DO 421 J=1,K
      DO 421 I=1,K
      III=III+1
      VQ(I,J)=AA(III)
 421  CONTINUE
C     REMISE DES BONNES VALEURS DANS LES TABLEAUX X0,H,G
      DO 101 NT=1,NS
      CALL DERIV(NT,1)
      CALL RTH(NT)
      DI=BT(1,NT)
      GA=BT(2,NT)
      H1=BT(3,NT) 
 101  CALL GRAPH(DI,GA,H1,N,NT)
 122  WRITE (NOUT,37)
      WRITE (NOUT,29) PRO
      CALL SORTIE(NSP2, SL,S)
      CALL  CTRAD
      CALL  TRACE(CMAX,CMIN)
      IF((IO(6).EQ.0).AND.(IO(11).EQ.0))GO TO 103
      DO 104 I=1,N
 104  IY(I)=INT(Y(I)-Q(I,K+2))
      IF(IO(6).EQ.1) CALL PERF
      IF(IO(11).NE.1)GO TO 103
      DO 107 I=1,N
      Q(I,K+2)=IY(I)
 107  Y(I)=IY(I)
      write(nout,*) cmax,cmin
      CALL TRACE(CMAX,CMIN)
 103  IF(IO(7).EQ.0)  GO TO 240
      DO 120  I=1,N
  120 IY(I)=INT(Q(I,K+2))
      CALL PERF
 240  IF(IO(12).EQ.0) GO TO 108
C     modif nini
      IF(IO(17).NE.0) THEN
      DO 246 ICOURB=1,5
      DO 246 NI=1,256
      YSS(NI,ICOURB)=0
 246  CONTINUE
      ICOURB=0
      DO 241 I=1,10,2
      I1=GRASS(I)
c~       write(6,*)GRASS(I)
      I2=GRASS(I+1)
      IF(I1.EQ.0) GO TO 244
      ICOURB=ICOURB + 1
      DO 242 J=I1,I2
      CALL DERIV(J,1)
      CALL RTH(J)
      DI=BT(1,J)
      GA=BT(2,J)
      H1=BT(3,J)
      CALL GRAPH(DI,GA,H1,N,J)
      DO 243 NI=1,256
      YSS(NI,ICOURB)=YSS(NI,ICOURB) + GR(NI)
 243  CONTINUE
 242  CONTINUE
      DO 245 NI=1,256
      YSS(NI,ICOURB)=TY-YSS(NI,ICOURB)
 245  CONTINUE
 241  CONTINUE
 244  ENDIF
C fin modif
      CALL LASER(CMAX,CMIN,CN,IO(17))
      CALL RESULTATS 
 108  WRITE (NOUT,37)
      STOP      
      END
c~       END PROGRAM 
C
C**********************************************************************
C
      SUBROUTINE LECTU(LE)
      USE PRECISION
      IMPLICIT real(dp) (a-h,o-z)
      IMPLICIT integer (i-n)
      COMMON/OPT/IO(20)
      COMMON/LECT/IYE(512),IY(256),IY1(256)
      COMMON/UN/Q(256,42),Y(256),B(40),BF(256),HBRUIT,N,K,NS
      COMMON/UNIT/NOUT,NOUT1
   11 FORMAT(I5,8I8)
      DO 100 I=1,N,8
      KMAX=I+7
      READ(5,11) NU,(IYE(KI),KI=I,KMAX)
c~       write(6,*) PAS
c~       write(6,*) NPAS
c~       write(6,*) "IYE :", IYE(I) , "calcul", FLOAT(IYE(I))-PAS*FLOAT(I)
c~       IYE(I)=FLOAT(IYE(I))-PAS*FLOAT(I)   ! PAS n'existe pas... 

 100  CONTINUE    
       write(6,*) "IYE(1)=", IYE(1)     
      write(6,*) "IYE(3)=", IYE(3)
      DO 102 I=1,N
      IF(LE.EQ.2) BF(I)=IYE(I)
      IF(LE.EQ.1) Y(I)=IYE(I)
  102 CONTINUE
99    RETURN
      END
C
C**********************************************************************
C                                                                      
      SUBROUTINE PERF
            USE PRECISION
      IMPLICIT real(dp) (a-h,o-z)
      IMPLICIT integer (i-n)
      COMMON/UN/Q(256,42),Y(256),B(40),BF(256),HBRUIT,N,K,NS           
      COMMON/LECT/IYE(512),IY(256),IY1(256)   
      COMMON/UNIT/NOUT,NOUT1
   11 FORMAT(I4,8I8)                                                   
      DO 100 I=1,N,8
      KMAX=I+7
      WRITE(NOUT,11) I,(IY(KI),KI=I,KMAX)                                                     
  100 CONTINUE                                                         
      RETURN                                                           
      END                                                              
C                                                                      
C**********************************************************************
C
      SUBROUTINE RAZ    
            USE PRECISION
      IMPLICIT real(dp) (a-h,o-z)
      IMPLICIT integer (i-n)                                               
      COMMON/UN/Q(256,42),Y(256),B(40),BF(256),HBRUIT,N,K,NS           
      COMMON/GRA/AINT(8),ENE(8),GR(256),CN                             
      COMMON/COMP/NPAS,NMAX,NS1,NS2                                    
      COMMON/PARSUP/DI0,PDI,SQ0,PSQ,CH0,PCH,TETA0,PTETA,ETA0,GAMA0,    
     1 BETA0,ALFA0,MONOC0,NB0(10),NB(10)                               
      COMMON/OPT/IO(20)                                                
      COMMON/DEUX/P(256),TY,IZ(10)                                     
      COMMON/PARHYP/DI,GA,H1,SQ,CH,TETA,ETA,GAMA,BETA,ALFA,MONOC,E     
      COMMON/LECT/IYE(512),IY(256),IY1(256)   
      COMMON/UNIT/NOUT,NOUT1
C     PARAMETRES INITIALISES  UNE FOIS PAR SPECTRE
      E=0.001                                                          
      DO 1 I=1,256                                                     
      P(I)=1.                                                          
      DO3 J=1,32                                                       
    3 Q(I,J)=0.0                                                       
    1 CONTINUE                                                         
      DO 9 I=1,30                                                      
 9    B(I)=0.                                                          
      DO 5 I=1,10                                                      
      IZ(I)=0                                                          
      NB0(I)=0
    5 CONTINUE                                                         
      DO 6 I=1,20                                                      
 6    IO(I)=0                                                          
      HBRUIT=0.                                                        
      CN=0.078125                                                      
      N=256                                                            
      NS=1                                                             
      NS1=0                                                            
      NS2=0                                                            
      K=1                                                              
      NPAS=0
      NMAX=20                                                          
      DI0=0.                                                           
      PDI=0.                                                           
      CH0=0.                                                           
      PCH=0.                                                           
      SQ0=0.                                                           
      PSQ=0.                                                           
      TETA0=0.                                                         
      PTETA=0.                                                         
      ETA0=0.                                                          
      GAMA0=0.
      BETA0=0.                                                         
      ALFA0=0.                                                         
      MONOC0=0                                                         
C     *************************************                            
C     PARAMETRES INITIALISES AVANT CHAQUE NAMT ( SAUF SI IO(10)=2 )
      ENTRY RAZHYP                                                     
      DI=0.                                                            
      GA=0.1                                                           
      H1=1.E5                                                          
      SQ=0.                                                            
      CH=0.                                                            
      ETA=0.                                                           
      TETA=0.                                                          
      GAMA=0.                                                          
      ALFA=0.                                                          
      BETA=0.
      MONOC=0                                                          
      TY=0.                                                            
      RETURN                                                           
      END                                                              
C                                                                      
C**********************************************************************
C                                                                      
      SUBROUTINE POIDS                                                 
      USE PRECISION
      IMPLICIT real(dp) (a-h,o-z)
      IMPLICIT integer (i-n)
      COMMON/DEUX/P(256),TY,IZ(10)                                     
      DO 1 I=2,256                                                     
      DO 1 KZ=1,9,2
      KZ1=KZ+1                                                         
      IF((I.GE.IZ(KZ)).AND.(I.LE.IZ(KZ1))) P(I)=0.                     
    1 CONTINUE                                                         
      RETURN                                                           
      END                                                              
C                                                                      
C                                                                      
      SUBROUTINE SUPER(NT,*) 
      USE PRECISION
      IMPLICIT real(dp) (a-h,o-z)
      IMPLICIT integer (i-n)
      COMMON/COMP/NPAS,NMAX,NS1,NS2                                    
      COMMON/PARHYP/DI,GA,H1,SQ,CH,TETA,ETA,GAMA,BETA,ALFA,MONOC,E     
      COMMON/PARSUP/DI0,PDI,SQ0,PSQ,CH0,PCH,TETA0,PTETA,ETA0,GAMA0,
     1 BETA0,ALFA0,MONOC0,NB0(10),NB(10)                               
      INC=NT-NS1                                                       
      F=FLOAT(INC)                                                     
      DI=DI0+PDI*F                                                     
      SQ=SQ0+PSQ*F                                                     
      CH=CH0+PCH*F                                                     
      TETA=TETA0+PTETA*F                                               
      ETA=ETA0                                                         
      GAMA=GAMA0                                                       
      BETA=BETA0                                                       
      ALFA=ALFA0
      MONOC=MONOC0                                                     
      DO 1 I=1,10                                                      
    1 NB(I)=NB0(I)                                                     
      RETURN1                                                          
      END                                                              
C                                                                      
C**********************************************************************
C                                                                      
      SUBROUTINE NIVZER                                                
C     EVALUATION DU TAUX MAXI DE COMPTAGE                              
      USE PRECISION
      IMPLICIT real(dp) (a-h,o-z)
      IMPLICIT integer (i-n)
      COMMON/UN/Q(256,42),Y(256),B(40),BF(256),HBRUIT,N,K,NS
      COMMON/DEUX/P(256),TY,IZ(10)                                     
      DO 102 I=2,11                                                    
102   TY=TY+0.05*Y(I)                                                  
      DO 103 I=N-9,N                                                   
103   TY=TY+0.05*Y(I)                                                  
      RETURN                                                           
      END                                                              
C                                                                      
C**********************************************************************
C                                                                      
      SUBROUTINE CALC(E,*)
      USE PRECISION
      IMPLICIT real(dp) (a-h,o-z)
      IMPLICIT integer (i-n)
      DIMENSION BI(40)                                                 
      COMMON/UN/Q(256,42),Y(256),B(40),BF(256),HBRUIT,N,K,NS           
      COMMON/OPT/IO(20)                                                
      COMMON/COMP/NPAS,NMAX,NS1,NS2                                    
      COMMON/UNIT/NOUT,NOUT1
13    FORMAT (1X,'  NUMERO DU PASSAGE ',I6)                            
   14 FORMAT (1X,' COUPURE   PAR    NMAX ')                            
   22 FORMAT (1X,' B CALC ', 8(2X,E13.5))                              
      T=1.E-5                                                          
      ITEST=0                                                          
      KM1=K-1                                                          
      WRITE (NOUT,13)NPAS
      WRITE  (6,22) (B(I),I=1,K)                                       
      IF(NPAS.EQ.NMAX) WRITE (NOUT,14)                                     
      IF(NPAS.GE.NMAX) GO TO 110                                       
      IF(NPAS.LE.1)GO TO 9                                             
      DO 111 I=1,K                                                     
      IF(ABS(B(I)-BI(I))/(T+ABS(B(I)))-E)  111,9,9                     
  111 CONTINUE                                                         
110   ITEST=1                                                          
    9 DO 112 I=1,K                                                     
  112 BI(I)=B(I)                                                       
      DO  1005  I=1,N
      Q(I,K+2)=B(K)                                                    
      Q(I,K)=1.0                                                       
      DO 1005 J=1,KM1                                                  
      Q(I,J)=0.                                                        
 1005 CONTINUE                                                         
      DO  1002  NT=1,NS                                                
 1002 CALL DERIV(NT,0)                                                 
      IF(IO(4).EQ.0) GO TO 1000                                        
      IF(IO(4).EQ.1) GO TO 1                                           
      DO 1001 I=1,N                                                    
      Q(I,K+2)=Q(I,K+2)+B(K-1)*BF(I)
 1001 Q(I,K-1)=+BF(I)                                                  
      GO TO 1000                                                       
 1    DO 2 I=1,N                                                       
 2    Q(I,K+2)=Q(I,K+2)+HBRUIT*BF(I)                                   
 1000 NPAS=NPAS+1                                                      
      IF(ITEST.EQ.1) RETURN1                                           
      RETURN                                                           
      END                                                              
C                                                                      
C**********************************************************************
C
      SUBROUTINE TRACE (C1,C2)                                                 
C     TRACE LES COURBES EXP ET THEOR AVEC PRECIS 1/100                 
C... Y  EXPERIMENTAL     *                                             
C... ..      Y1    CALCULE  X 
      USE PRECISION
      IMPLICIT real(dp) (a-h,o-z)
      IMPLICIT integer (i-n)                                         
      DIMENSION Y1(256)                                                
      CHARACTER*1 BL(120)                                              
      COMMON/UN/Q(256,42),Y(256),B(40),BF(256),HBRUIT,N,K,NS           
      COMMON/OPT/IO(20)                                                
      COMMON/UNIT/NOUT,NOUT1
      CHARACTER*1 AX/'X'/,BLANC/' '/,STAR/'*'/                         
      INAN=IO(2)                                                       
      C1=Y(1)
      C2=C1         
  
c~       write(6,*) C1,C2
c~       write(6,*) maxval(Y), minval(Y)
c~       write(6,*) maxval(Y1), minval(Y1)                                        
      DO 1 I=1,N                                                       
      Y1(I)=Q(I,K+2)                                                   
      IF(Y(I).GE.C1) C1=Y(I)                                           
      IF(Y(I).LE.C2)  C2=Y(I)                                          
      IF(Y1(I).LE.C2)C2=Y1(I)                                          
      IF(Y1(I).GE.C1)C1=Y1(I)  
    1 CONTINUE     
c~           C2 =  2917045    
c~           WRITE(6,*) "ATTENTION, MIN TRAFIQUE POUR DEBUGGAGE !!!"                                                    
      WRITE (NOUT,11) C1,C2                                                
   11 FORMAT(1X,/,' MAX = ',F10.0,'     MIN = ',F10.0)                 
      DO 8 IM=1,N
      I=N+1-IM                                                         
      ORD=(C1-Y(I))*119./(C1-C2)                                       
      ORD1=(C1-Y1(I))*119./(C1-C2)                                     
      IF(INAN.EQ.1.)ORD=(C1-Y(I))*107./(C1-C2)                         
      IF(INAN.EQ.1.)ORD1=(C1-Y1(I))*107./(C1-C2)  
      IOR=1+INT(ORD)                             
c~       write(6,*) "ORD, IFIX(ORD): ", Y(I), ORD, IFIX(ORD)                     
c~       write(6,*) "ORD, NINT(ORD): ", Y(I), ORD, NINT(ORD)                     
c~       write(6,*) "ORD, INT(ORD): ",Y(I),  ORD, INT(ORD)                     
c~       write(6,*) "Y1 ",Y1(I)
      IOR1=1+INT(ORD1)                                                
      KMAX=IOR                                                         
      IF(IOR1.GT.IOR) KMAX=IOR1                                        
      DO 9 KI=1,KMAX                                                   
 9    BL(KI)=BLANC
      BL(IOR1)=AX                                                      
      BL(IOR)=STAR                                                     
      WRITE (NOUT,10)I,(BL(KI),KI=1,KMAX)                                  
 10   FORMAT(1X,I4,2X,A1,120A1)                                        
8     CONTINUE                    
c~       write(6,*) I
c~       write(6,*) "Y(3)=", Y(3)                                        
c~       write(6,*) "Y(1)=", Y(1)                                        
      RETURN                                                           
      END                                                              
C                                                                      
C**********************************************************************
C                                                                      
      SUBROUTINE  CTRAD
C     CALCUL DES ECARTS STATISTIQUES    
      USE PRECISION
      IMPLICIT real(dp) (a-h,o-z)
      IMPLICIT integer (i-n)                               
      COMMON/UN/Q(256,42),Y(256),B(40),BF(256),HBRUIT,N,K,NS           
      COMMON/DEUX/P(256),TY,IZ(10)                                     
      COMMON/UNIT/NOUT,NOUT1
   13 FORMAT(1H1,//,50X,'QUI2 = ',E13.8)                               
      QUI2=0.0                                                         
      DO 1 I=1,N                                                       
      IF(Y(I).EQ.0.) GO TO 1                                           
      QUI2=QUI2+P(I)*(Q(I,K+2)-Y(I))**2/Y(I)                           
 1    CONTINUE                                                         
      QUI2=QUI2/(N-K)                                                  
      WRITE (NOUT,13) QUI2
      RETURN                                                           
      END                                                              
C                                                                      
C**********************************************************************
C                                                                      
      SUBROUTINE RTH(NT)  
C   CALCUL DANS LES AXES DU GRADIENT                                   
C   ( CAS DE LA POUDRE : MONOC=0 )                                     
C   ( CAS DU MONOCRISTAL MONOC=1 )
      USE PRECISION

      IMPLICIT real(dp) (a-h,o-z)
      IMPLICIT integer (i-n)
      COMPLEX(dp) VE(16),VF(16),F(10),E(10),VFC(4)                         
      COMPLEX(dp) TMP(4,2),TM0(4,2),TMM(4,2)
      COMPLEX(dp) AP,A0,AM,CU,CINT                                         
c~       COMPLEX CMPLX                                                    
c~       COMPLEX CONJG                                                    
      DIMENSION RF(3),CF(3),RE(10),CE(10)                              
      DIMENSION R(4,2),C(4,2)                                          
      COMMON/TH/BT(10,40),GVT(8,40),NBT(10,40),NGT(8,40),MONOT(40),    
     1 IOGVT(40)                                                       
      COMMON/OPT/IO(20)                                                
      COMMON/GRA/AINT(8),ENE(8),GR(256),CN                             
      DATA VE,VF,E,F/52*(0.,0.)/                                       
      RPD=3.1416/180.
      R2=SQRT(2.)                                                      
      R3=SQRT(3.)                                                      
      ZF=3.915/330.                                                    
      ZE=-2.236/330.                                                   
      IF(IO(3).NE.1) GO TO 9                                           
      ZF=-0.08278                                                      
      ZE=0.0180                                                        
 9    SQ=BT(4,NT)                                                      
      CH=BT(5,NT)                                                      
      ETA=BT(6,NT)                                                     
      TET=BT(7,NT)*RPD
      GAM=BT(8,NT)*RPD                                                 
      BET=BT(9,NT)*RPD                                                 
      ALF=BT(10,NT)*RPD                                                
      ST=SIN(TET)                                                      
      CT=COS(TET)                                                      
      SG=SIN(GAM)                                                      
      CG=COS(GAM)                                                      
      HX=CH*ST*CG                                                      
      HY=CH*ST*SG                                                      
      HZ=CH*CT                                                         
C    ETAT FONDAMENTAL ( I=1/2 )
      DO 10 I=1,3                                                      
      RF(I)=0.                                                         
      CF(I)=0.                                                         
   10 CONTINUE                                                         
      RF(1)=-0.5*HZ*ZF                                                 
      RF(3)=-RF(1)                                                     
      RF(2)=-0.5*HX*ZF                                                 
      CF(2)=0.5*HY*ZF                                                  
      DO 11 I=1,3                                                      
   11 F(I)=CMPLX(RF(I),CF(I),dp)                                          
c~    11 F(I)=CMPLX(RF(I),CF(I))                                          
      CALL CEGREN(F,VF,2,0)
      DO 12 I=1,4                                                      
   12 VFC(I)=CONJG(VF(I))                                              
C   ETAT EXCITE (I=3/2 )                                               
      DO 20 I=1,10                                                     
      RE(I)=0.                                                         
      CE(I)=0.                                                         
   20 CONTINUE                                                         
      RAC=1.+ETA**2/3.                                                 
      Q=0.5*SQ/SQRT(RAC)                                               
      RE(1)=-1.5*HZ*ZE+Q                                               
      RE(2)=-0.5*R3*HX*ZE
      CE(2)=0.5*R3*HY*ZE                                               
      RE(3)=-0.5*HZ*ZE-Q                                               
      RE(4)=ETA*R3*Q/3.                                                
      RE(5)=-HX*ZE                                                     
      CE(5)=HY*ZE                                                      
      RE(6)=0.5*HZ*ZE-Q                                                
      RE(8)=RE(4)                                                      
      RE(9)=RE(2)                                                      
      CE(9)=CE(2)                                                      
      RE(10)=1.5*HZ*ZE+Q                                               
      DO 21 I=1,10
   21 E(I)=CMPLX(RE(I),CE(I),dp)                                          
c~    21 E(I)=CMPLX(RE(I),CE(I))                                          
      CALL CEGREN(E,VE,4,0)                                            
C   CONSTRUCTION DES MATRICES DES OPERATEURS : TMP, TM0, TMM           
      CB=COS(BET)                                                      
      SB=SIN(BET)                                                      
      CA=COS(ALF)                                                      
      SA=SIN(ALF)                                                      
C  CALCUL DE TMM                                                       
      DO 33 I=1,2                                                      
      DO 33 J=1,4                                                      
      R(J,I)=0.
      C(J,I)=0.                                                        
   33 CONTINUE                                                         
      R(1,1)=CA*(1.+CB)/2.                                             
      C(1,1)=SA*(1.+CB)/2.                                             
      R(2,1)=SB/R3                                                     
      R(3,1)=CA*(1.-CB)/(2.*R3)                                        
      C(3,1)=-SA*(1.-CB)/(2.*R3)                                       
      R(2,2)=CA*(1.+CB)/(2.*R3)                                        
      C(2,2)=SA*(1.+CB)/(2.*R3)                                        
      R(3,2)=SB/R3                                                     
      R(4,2)=CA*(1.-CB)/2.
      C(4,2)=-SA*(1.-CB)/2.                                            
      DO 30 I=1,2                                                      
      DO 30 J=1,4                                                      
   30 TMM(J,I)=CMPLX(R(J,I),C(J,I),dp)                                    
c~    30 TMM(J,I)=CMPLX(R(J,I),C(J,I))                                    
C    CALCUL DE TMP                                                     
      DO 31 I=1,2                                                      
      DO 31 J=1,4                                                      
      I1=3-I                                                           
      J1=5-J                                                           
      U=(-1.)**(I+J)                                                   
      CU=CMPLX(U,0.0_dp,dp)
c~       CU=CMPLX(U,0.)
      TMP(J,I)=CU*CONJG(TMM(J1,I1))                                    
   31 CONTINUE                                                         
C  CALCUL DE TM0                                                       
      R(1,1)=-CA*SB/R2                                                 
      C(1,1)=-SA*SB/R2                                                 
      R(2,1)=CB*R2/R3                                                  
      C(2,1)=0.                                                        
      R(3,1)=CA*SB/(R2*R3)                                             
      C(3,1)=-SA*SB/(R2*R3)                                            
      R(4,1)=0.                                                        
      C(4,1)=0.
      DO 32 J=1,4                                                      
      TM0(J,1)=CMPLX(R(J,1),C(J,1),dp)                                    
c~       TM0(J,1)=CMPLX(R(J,1),C(J,1))                                    
      U=(-1.)**J                                                       
      CU=CMPLX(U,0.0_dp,dp)                                                   
c~       CU=CMPLX(U,0.)                                                   
      J1=5-J                                                           
      TM0(J1,2)=CU*CONJG(TM0(J,1))                                     
   32 CONTINUE                                                         
C  CALCUL DES ENERGIES ET DES INTENSITES                               
      K=0                                                              
      TINT=0.                                                          
      DO 40 I=1,2
C   TRANSITION DU NIVEAU FONDAMENTAL  I                                
      DO 40 J=1,4                                                      
C  VERS LE NIVEAU EXCITE  J                                            
      K=K+1                                                            
      I1=(I*I+I)/2                                                     
      J1=(J*J+J)/2                                                     
C  ENERGIE DE LA TRANSITION                                            
      ENE(K)=(REAL(E(J1))-REAL(F(I1)))                                   
C  CALCUL DE L' INTENSITE                                              
      AP=(0.,0.)                                                       
      A0=(0.,0.)
      AM=(0.,0.)                                                       
      IL=2*(I-1)+1                                                     
      JL=4*(J-1)+1                                                     
      IK=2*I                                                           
      JK=4*J                                                           
      DO 41 II=1,2                                                     
      DO 41 JJ=1,4                                                     
      LI=IL-1+II                                                       
      LJ=JL-1+JJ                                                       
C  TRANSITION TMP                                                      
      AP=AP+VFC(LI)*TMP(JJ,II)*VE(LJ)
C      TRANSITION TM0                                                  
      A0=A0+VFC(LI)*TM0(JJ,II)*VE(LJ)                                  
C    TRANSITION TMM                                                    
      AM=AM+VFC(LI)*TMM(JJ,II)*VE(LJ)                                  
   41 CONTINUE                                                         
       IF(MONOT(NT).EQ.1) A0=(0.,0.)                                   
      CINT=AP*CONJG(AP)+A0*CONJG(A0)+AM*CONJG(AM)                      
      AINT(K)=REAL(CINT)                                               
      TINT=TINT+AINT(K)                                                
 40   CONTINUE                                                         
C  NORMALISATION DES INTENSITES
      DO 42 I=1,8                                                      
   42 AINT(I)=AINT(I)*8./TINT                                          
      RETURN                                                           
      END                                                              
C                                                                      
C**********************************************************************
C                                                                      
      SUBROUTINE SORTIE (NSP2, SL,S)   
            USE PRECISION
      IMPLICIT real(dp) (a-h,o-z)
      IMPLICIT integer (i-n)                                            
      CHARACTER*1 BL(95)                                               
      CHARACTER*1 STAR/'*'/,BLANC/' '/,AX/'X'/,AT/'!'/                 
      DIMENSION S(40),SL(42),T(34),BTM(7),BTM2(7)
      COMMON/UN/Q(256,42),Y(256),B(40),BF(256),HBRUIT,N,K,NS           
      COMMON/DEUX/P(256),TY,IZ(10)
      COMMON/COMP/NPAS,NMAX,NS1,NS2                                    
      COMMON/TH/BT(10,40),GVT(8,40),NBT(10,40),NGT(8,40),MONOT(40),    
     1 IOGVT(40)                                                       
      COMMON/RAIES/H(8,40),G(8,40),X0(8,40)                            
      COMMON/OPT/IO(20)                                                
      COMMON/VIR/PH,VQ(40,40),ETBT(10,40),ETGVT(8,40)
      COMMON/UNIT/NOUT,NOUT1
   20 FORMAT(//,50X,'CARACTERISTIQUES DES SPECTRES',//)                
 25   FORMAT(//,'  ABS TOT EXP ',F11.0,12X,' ABS TOT CALC ',F11.0,     
     1 ' DONT BRUIT ',F11.0,/)
 26   FORMAT('  ABSFIT/ABSEXP= ',F10.5,10X,' ABSBRUIT/ABSEXP= ',F10.5,/)
   28 FORMAT(30X,' SPECTRE ',I2,10X,F6.2,' %','  TOTAL ',10X,F6.2,/)   
   31 FORMAT (1X,20X,'MMS',8X,'MMS',8X,'COUPS',6X,'MMS',9X,'KG',25X,   
     1'DEG',23X,'DEG',8X,///)                                          
   30 FORMAT(//,' SPECTRE ',I2,2X,10F12.3,/)                           
   22 FORMAT (1X,39X,F6.2,5X,F6.2,5X,F7.0,5X,F10.0,/)                  
   27 FORMAT(/,'   SURFACE DISPERSEE/SURFACE ABSORPTION    EXP=',F11.5,1
     *X,'FIT=',F11.5,/)                                                
   21 FORMAT(/,42X,'X0',9X,'G',10X,'H',5X,'(CANAUX)',/)                
 9    FORMAT(//,'  BRUIT DE FOND NON  HBRUIT= ',F10.3,///)             
 10   FORMAT(//,'  BRUIT DE FOND  HBRUIT= ',F10.3,///)                 
   29 FORMAT (1X,20X,'DI',9X,'GA',9X,'H1',9X,' SQ',9X,'CH',9X,' ETA',9X,
     1'TETA',9X,'GAMA',9X,'BETA',9X,'ALFA',/)                          
      WRITE (NOUT,20)
      ST=0.                                                            
      WRITE (NOUT,29)                                                      
      WRITE (NOUT,31)                                                      
      DO 201  NT=1,NS                                                  
      WRITE (NOUT,30)NT,(BT(I,NT),I=1,10)                                  
      WRITE (NOUT,50)(ETBT(I,NT),I=1,10)                                   
 50   FORMAT(' ECART TYPE  ',10E12.3,//)                               
      IF(IOGVT(NT).EQ.0) GO TO 201                                     
      WRITE (NOUT,32)                                                      
 32   FORMAT(21X,'GVT(1)',8X,'GVT(2)',8X,'GVT(3)',8X,'GVT(4)',8X,'GVT(5)
     1 ',8X,'GVT(6)',8X,'GVT(7)',8X,'GVT(8)',/)                        
      WRITE (NOUT,33)(GVT(I,NT),I=1,8)
 33   FORMAT(13X,8(8X,F6.4),/)                                         
      WRITE (NOUT,51)(ETGVT(I,NT),I=1,8)                                   
 51   FORMAT(' ECART TYPE  ',8(2X,E12.3),//)                           
  201 CONTINUE                                                         
      IF(IO(8).EQ.1) WRITE (NOUT,21)                                       
      DO 203 NT=1,NS                                                   
      IF(IO(8).NE.1) GO TO 206                                         
      DO 202 L=1,8                                                     
 202  WRITE (NOUT,22) X0(L,NT),G(L,NT),H(L,NT)                             
C     WRITE (NOUT,300)                                                     
C300  FORMAT(//,30X,'MATRICE DE VARIANCE COVARIANCE',//)
C     DO 301 I=1,K                                                     
C301  WRITE (NOUT,302)(VQ(I,J),J=1,K)                                      
C302  FORMAT(8X,10(2X,E10.3),//)                                       
  206 S(NT)=ABS(BT(2,NT))*BT(3,NT)                                     
      ST=ST+S(NT)                                                      
  203 CONTINUE                                                         
      IF(IO(4).EQ.2) HBRUIT=B(K-1)                                     
      IF(IO(4).EQ.1) WRITE (NOUT,9) HBRUIT                                 
      IF(IO(4).EQ.2) WRITE (NOUT,10) HBRUIT                                
      SEXP=1.                                                          
      DEXP=1.                                                          
      SFIT=0.                                                          
      SBRU=1.                                                          
      DO 204  I=1,N                                                    
      SEXP=SEXP+B(K)-Y(I)                                              
      SFIT=SFIT+B(K)-Q(I,K+2)                                          
      IF(HBRUIT.NE.0.) SBRU=SBRU-HBRUIT*BF(I)                          
  204 CONTINUE                                                         
      WRITE (NOUT,25) SEXP,SFIT,SBRU                                       
      FSE=SFIT/SEXP                                                    
      BSE=SBRU/SEXP                                                    
      WRITE (NOUT,26) FSE,BSE                                              
      DO 207 I=1,N                                                     
      DFIT=DFIT+(Y(I)-Q(I,K+2))**2                                     
207   CONTINUE                                                         
      DO 220 I=3,12                                                    
      DEXP=DEXP+(TY-Y(I))**2                                           
220   CONTINUE                                                         
      DO 221 I=N-9,N                                                   
      DEXP=DEXP+(TY-Y(I))**2                                           
221   CONTINUE                                                         
      DAEXP=N*SQRT(DEXP/20)/SEXP                                       
      DAFIT=N*SQRT(DFIT/N)/SFIT                                        
      WRITE (NOUT,27) DAEXP,DAFIT                                          
      SINT=0.                                                          
      DO 205 NT=1,NS                                                   
      S(NT)= 100.*S(NT)/ST                                             
      T(NT+2)=S(NT)                                                    
      SINT=SINT+S(NT)                                                  
  205 WRITE (NOUT,28) NT,S(NT),SINT                                        
      IF(IO(13).EQ.0) GO TO 209                                        
C    LISSAGE DE LA DISTRIBUTION                                        
      T(1)=0.                                                          
      T(2)=0.                                                          
      T(NS+3)=0.                                                       
      T(NS+4)=0.                                                       
      NSP2=NS+2                                                        
      IF(IO(14).NE.0) GO TO 214                                        
      WRITE (NOUT,39)                                                      
   39 FORMAT(30X,'LISSAGE',19X,'%',22X,'%LISSE',/)                     
      DO 208 I=1,NSP2                                                  
      SL(I)=(T(I)+2.*T(I+1)+T(I+2))/4.                                 
  208 WRITE (NOUT,40) T(I+1),SL(I)                                         
   40 FORMAT(30X,2(20X,F6.2),/)                                        
      GO TO 212                                                        
214   WRITE (NOUT,44)                                                      
44    FORMAT(9X,'LISSAGE',5X,'%',8X,'%LISSE',20X,'DIAGRAMME  EN  CARTOUC
     *HES',//)                                                         
      WRITE (NOUT,47)                                                      
47    FORMAT(56X,'0',19X,'5',18X,'10%')                                
      DO 213 I=1,NSP2                                                  
      SL(I)=(T(I)+2.*T(I+1)+T(I+2))/4.                                 
213   CONTINUE                                                         
      DO 216 I=1,NSP2                                                  
      ORD=20+4.*SL(I)                                                  
      ORD1=20+4.*T(I+1)                                                
      IOR=1+INT(ORD)                                                  
      IOR1=1+INT(ORD1)                                                
      IF(IOR.GT.72) IOR=21                                             
      IF(IOR1.GT.72) IOR1=21                                           
      IF(IOR.LT.-20) IOR=21                                            
      IF(IOR1.LT.-20) IOR1=21                                          
      KMAX=IOR                                                         
      IF(IOR1.GT.IOR) KMAX=IOR1                                        
      DO 215 KI=1,KMAX                                                 
215   BL(KI)=BLANC                                                     
      BL(IOR1)=AX                                                      
      BL(IOR)=STAR                                                     
      BL(21)=AT                                                        
      WRITE (NOUT,45) T(I+1),SL(I),(BL(KI),KI=1,KMAX)                      
45    FORMAT(8X,2(6X,F6.2),4X,A1,95A1)                                 
      WRITE (NOUT,46) (BL(KI),KI=1,KMAX)                                   
46    FORMAT(36X,A1,95A1)                                              
216   CONTINUE                                                         
C     CALCUL DE VALEURS MOYENNES                                       
212   NSS=NS                                                           
      IF(NS2.NE.0) NSS=NS2                                             
      DO 210 J=1,7                                                     
      BTM(J)=0.                                                        
      BTM2(J)=0.                                                       
      GO TO(211,210,210,211,211,210,211),J
211   DO 217 NT=1,NSS
      BTM(J)=BTM(J)+BT(J,NT)*S(NT)/SINT
      BTM2(J)=BTM2(J)+(BT(J,NT)**2)*S(NT)/SINT
217   CONTINUE
  210 CONTINUE
      WRITE (NOUT,43) NSS                                                  
43    FORMAT(//,' CALCUL SUR LES ',I3,' PREMIERS SPECTRES',//)         
      WRITE (NOUT,29)                                                      
      WRITE (NOUT,41)BTM                                                   
      WRITE (NOUT,42)BTM2                                                  
   41 FORMAT(//,' MOYENNE',6X,7F12.3,//)                               
   42 FORMAT(//,' QUADRATIQUE',2X,7F12.3,//)                           
  209 CONTINUE                                                         
      RETURN                                                           
      END                                                              
C                                                                      
C**********************************************************************
C                                                                      
      SUBROUTINE GRAPH(DI,GA,H1,N,NT)     
            USE PRECISION
      IMPLICIT real(dp) (a-h,o-z)
      IMPLICIT integer (i-n)                             
      COMMON/TH/BT(10,40),GVT(8,40),NBT(10,40),NGT(8,40),MONOT(40),    
     1 IOGVT(40)                                                       
      COMMON/GRA/AINT(8),ENE(8),GR(256),CN                             
      COMMON/RAIES/H(8,40),G(8,40),X0(8,40)                            
      COMMON/OPT/IO(20)                                                
      COMMON/UNIT/NOUT,NOUT1
      IF(IO(16).NE.0) CALL CONVOL(DI,GA,H1,N,NT)                       
      IF(IO(16).NE.0) GOTO 53                                          
      DO 50 I=1,N                                                      
   50 GR(I)=0.                                                         
      D0=(FLOAT(N)+1.0)/2.0                                            
      DO 52 L=1,8                                                      
      G(L,NT)=GA/CN                                                    
      H(L,NT)=H1*AINT(L)/8.                                            
      IOG=IOGVT(NT)+1                                                  
      GO TO(2,3,3,4),IOG                                               
 4    IF(NGT(L,NT).EQ.0) GO TO 2                                       
 3    G(L,NT)=GVT(L,NT)/CN                                             
      H(L,NT)=H(L,NT)*GA/GVT(L,NT)                                     
 2    CONTINUE                                                         
      X0(L,NT)=D0+(ENE(L)+DI)/CN                                       
      DO 52 I=1,N                                                      
      X=FLOAT(I)                                                       
      B=X-X0(L,NT)                                                     
      GR(I)=GR(I)+H(L,NT)*G(L,NT)**2/(B**2+G(L,NT)**2)                 
   52 CONTINUE                                                         
 53   RETURN                                                           
      END                                                              
C                                                                      
C**********************************************************************
C                                                                      
      SUBROUTINE CONVOL(DI,GA,H1,N,NT) 
            USE PRECISION
      IMPLICIT real(dp) (a-h,o-z)
      IMPLICIT integer (i-n)                                
      DIMENSION GG(2560),GGL(2560)                                     
      COMMON/TH/BT(10,40),GVT(8,40),NBT(10,40),NGT(8,40),MONOT(40),    
     1 IOGVT(40)                                                       
      COMMON/GRA/AINT(8),ENE(8),GR(256),CN                             
      COMMON/RAIES/H(8,40),G(8,40),X0(8,40)                            
      COMMON/OPT/IO(20)                                                
      COMMON/UNIT/NOUT,NOUT1
      DO 40 I=1,N                                                      
40    GR(I)=0.                                                         
      KP=IO(16)                                                        
      M=N*KP                                                           
      CM=CN/KP                                                         
      DO 50 I=1,M                                                      
      GGL(I)=0.                                                        
50    GG(I)=0.                                                         
      D0=(FLOAT(M)+1.0)/2.0                                            
      DO 51 L=1,8                                                      
      G(L,NT)=GA/CM                                                    
      H(L,NT)=H1*AINT(L)/8.                                            
      IOG=IOGVT(NT)+1                                                  
      GO TO (2,3,3,4),IOG                                              
4     IF(NGT(L,NT).EQ.0) GO TO 2                                       
3     G(L,NT)=GVT(L,NT)/CM                                             
      H(L,NT)=H(L,NT)*GA/GVT(L,NT)                                     
2     CONTINUE                                                         
      X0(L,NT)=D0+(ENE(L)+DI)/CM                                       
      DO 57 I=1,M                                                      
      X=FLOAT(I)                                                       
      B=X-X0(L,NT)                                                     
      U=(B**2)/(2.*(G(L,NT)**2))                                       
c     write(5,*) 'U=',U
      IF(U.GT.80.) U=80.                                             
c      IF(U.GT.140.) U=140.     ancienne valeur yvan
      UGAU=EXP(-U)                                                      
      GG(I)=GG(I)+H(L,NT)*UGAU                                          
57    CONTINUE                                                         
51    CONTINUE                                                         
      DO 61 I=1,M                                                      
      XI=FLOAT(I)                                                      
      DO 61 J=1,M                                                      
      XJ=FLOAT(J)                                                      
      BL=XI-XJ                                                         
      GGL(I)=GGL(I)+GG(J)*(.1/CM)**2/(BL**2+((.1/CM))**2)              
61    CONTINUE                                                         
      J=0                                                              
      DO 62 I=KP,M,KP                                                  
      J=J+1                                                            
      GR(J)=GGL(I)                                                     
      IF(ABS(GR(J)).LT.0.2) GR(J)=0.                                   
62    CONTINUE                                                         
      RETURN                                                           
      END                                                              
C                                                                      
C**********************************************************************
C                                                                      
      SUBROUTINE DERIV(NT,M)             
            USE PRECISION
      IMPLICIT real(dp) (a-h,o-z)
      IMPLICIT integer (i-n)                              
      DIMENSION GR0(256),DER(2,256)                                    
      COMMON/UN/Q(256,42),Y(256),B(40),BF(256),HBRUIT,N,K,NS           
      COMMON/GRA/AINT(8),ENE(8),GR(256),CN                             
      COMMON/TH/BT(10,40),GVT(8,40),NBT(10,40),NGT(8,40),MONOT(40),    
     1 IOGVT(40)                                                       
      COMMON/ADD/IAD(10,40),IADG(8,40)                                 
      COMMON/OPT/IO(20)                                                
      COMMON/VIR/PH,VQ(40,40),ETBT(10,40),ETGVT(8,40)
C MISE DANS BT ET GVT DES PARAMETRES                                   
      DO 30 I=1,10                                                     
      IF(NBT(I,NT).EQ.0) GOTO30                                        
      L=IAD(I,NT)                                                      
      BT(I,NT)=B(L)                                                    
      ETBT(I,NT)=SQRT(VQ(L,L)*PH/(N-K))                                
      IF(IO(5).EQ.0) GO TO 30                                          
      IF(NBT(I,NT).EQ.3) CALL CONNEX                                   
   30 CONTINUE                                                         
      IF(IO(9).EQ.1) BT(9,NT)=BT(7,NT)                                 
      IF(IO(9).EQ.1) BT(10,NT)=BT(8,NT)                                
      DO 31 I=1,8                                                      
      GVT(I,NT)=BT(2,NT)                                               
      IF(NGT(I,NT).EQ.0) GO TO 31                                      
      L=IADG(I,NT)                                                     
      GVT(I,NT)=B(L)                                                   
      ETGVT(I,NT)=SQRT(VQ(L,L)*PH/(N-K))                               
 31   CONTINUE                                                         
      IF(IOGVT(NT).NE.0) CALL GVAR(NT)                                 
      IF(M.EQ.1) RETURN                                                
      DI=BT(1,NT)                                                      
      GA=BT(2,NT)                                                      
      H1=BT(3,NT)                                                      
CALCUL DE LA FONCTION                                                  
      CALL RTH(NT)                                                     
      CALL GRAPH(DI,GA,H1,N,NT)                                        
      DO 40 I=1,N                                                      
      Q(I,K+2)=Q(I,K+2)-GR(I)                                          
      GR0(I)=GR(I)                                                     
   40 CONTINUE                                                         
C CALCUL DES DERIVEES PAR RAPPORT AUX LARGEURS VARIABLES               
      DO 69 J=1,8                                                      
      IF(NGT(J,NT).EQ.0) GO TO 69                                      
      L=IADG(J,NT)                                                     
      DIF=CN*1.E-3                                                     
      GVT(J,NT)=B(L)+DIF                                               
      IF(IOGVT(NT).NE.0) CALL GVAR(NT)                                 
      CALL GRAPH(DI,GA,H1,N,NT)                                        
      DO 68 I=1,N                                                      
 68   Q(I,L)=(GR0(I)-GR(I))/DIF                                        
      GVT(J,NT)=B(L)                                                   
      IF(IOGVT(NT).NE.0) CALL GVAR(NT)                                 
 69   CONTINUE                                                         
CALCUL DES DERIVEES PAR RAPPORT AUX PARAMETRES HYPERFINS.              
      DO50J=1,10                                                       
      IF(NBT(J,NT).EQ.0)GO TO 50                                       
      IF(NBT(J,NT).EQ.3) GO TO 50                                      
      L=IAD(J,NT)                                                      
C APPROXIMATION PARABOLIQUE(MOYENNE SUR DEUX ACCROISSEMENTS OPPOSES)   
      DO 1000 JJ=1,2                                                   
      PM=(-1.)**JJ                                                     
      GO TO(1,2,3,6,6,6,6,6,6,6),J                                     
    1 DIF=PM*CN*1.E-3                                                  
      DI1=DI+DIF                                                       
      CALL GRAPH(DI1,GA,H1,N,NT)                                       
      GOTO60                                                           
    2 DIF=PM*CN*1.E-3                                                  
      GB=GA+DIF                                                        
      CALL GRAPH(DI,GB,H1,N,NT)                                        
      GOTO60                                                           
    3 DO 33 I=1,N                                                      
   33 DER(JJ,I)=-GR0(I)/H1                                             
      GOTO 1000                                                         
    6 DIF=PM*1.E-2                                                     
      BT(J,NT)=BT(J,NT)+DIF                                            
      CALL RTH(NT)                                                     
      BT(J,NT)=BT(J,NT)-DIF                                            
    7 CALL GRAPH(DI,GA,H1,N,NT)                                        
   60 DO 41 I=1,N                                                      
   41 DER(JJ,I)=(GR0(I)-GR(I))/DIF                                     
 1000 CONTINUE                                                         
      DO 45 I=1,N                                                      
 45   Q(I,L)=Q(I,L)+((DER(1,I)+DER(2,I))/2.)                           
   50 CONTINUE                                                         
      RETURN                                                           
      END                                                              
C                                                                      
C**********************************************************************
C                                                                      
      SUBROUTINE GVAR(NT)          
            USE PRECISION
      IMPLICIT real(dp) (a-h,o-z)
      IMPLICIT integer (i-n)                                    
      COMMON/TH/BT(10,40),GVT(8,40),NBT(10,40),NGT(8,40),MONOT(40),    
     1 IOGVT(40)                                                       
      IOG=IOGVT(NT)                                                    
      GO TO (1,2,3),IOG                                                
 1    GVT(2,NT)=GVT(1,NT)                                              
      GVT(5,NT)=GVT(1,NT)                                              
      GVT(6,NT)=GVT(1,NT)                                              
      GVT(4,NT)=GVT(3,NT)                                              
      GVT(7,NT)=GVT(3,NT)                                              
      GVT(8,NT)=GVT(3,NT)                                              
      RETURN                                                           
 2    GVT(7,NT)=GVT(2,NT)                                              
      GVT(6,NT)=GVT(3,NT)                                              
      GVT(5,NT)=GVT(4,NT)                                              
 3    RETURN                                                           
      END                                                              
C                                                                      
C**********************************************************************
C                                                                      
      SUBROUTINE MAMAGT(Q,ID,B,Y,N,K,E,CALC,P)
            USE PRECISION
      IMPLICIT real(dp) (a-h,o-z)
      IMPLICIT integer (i-n)
c~       integer G         
      integer GG(100)
      DIMENSION Y(1),B(1),Q(ID,1),G(100),G1(100),P(1)                  
      COMMON/VIR/PH,VQ(40,40),ETBT(10,40),ETGVT(8,40)
      COMMON/UNIT/NOUT,NOUT1
      DO 420 I=1,K                                                     
      DO 420 J=1,K    
      G=0.0                                                 
 420  VQ(I,J)=0.                                                       
      U=5.0                                                            
 1000 S=0.01                                                           
      INT=0                                                            
  222 CALL CALC(E,*101)                                                
      PH=0.                                                            
      DO 7 I=1,N                                                       
    7 PH=PH+P(I)*(Y(I)-Q(I,K+2))**2                                    
      VARY=PH                                                          
  174 INT =INT+1                                                       
      IF (INT-1)190,190,99                                             
  190 PHI =PH                                                          
   88 DO 6 I=1,K                                                       
      Q(I,K+1)=0.                                                      
      DO 6 M=1,N                                                       
    6 Q(I,K+1)=Q(I,K+1)+(Y(M)-Q(M,K+2))*Q(M,I)*P(M)                    
      DO 4 I=1,K                                                       
      DO 51 J=1,K                                                      
      Q(J,K+2)=0.                                                      
      IF(J-I)52,53,53                                                  
   53 DO 54 L=1,N                                                      
   54 Q(J,K+2)=Q(J,K+2)+Q(L,I)*Q(L,J)*P(L)                             
      GO TO 51                                                         
   52 Q(J,K+2)=Q(I,J)                                                  
   51 CONTINUE                                                         
      DO 55 L=1,K                                                      
   55 Q(L,I)=Q(L,K+2)                                                  
    4 CONTINUE                                                         
      DO 401 I=1,K                                                     
      DO 401 J=1,K                                                     
 401  VQ(I,J)=Q(I,J)                                                   
      DO 5 I=1,K                                                       
      IF (Q(I,I)) 738,739,738                                          
  739 WRITE (NOUT,740) I                                                  
  740 FORMAT (1X,45H LA FONCTION EST INDEPENDANTE DU PARAMETRE NO,I4)  
      DO 1 M=1,K                                                       
 1    Q(M,K+2)=Y(M)                                                    
      RETURN                                                           
  738 Q(I,K+2)=SQRT(Q(I,I))                                            
    5 Q(I,K+1)=Q(I,K+1)/Q(I,K+2)                                       
      DO 24 I=1,K                                                      
      DO 24 J=1,K                                                      
      IF(I-J)11,16,10                                                  
   16 Q(I,J)=1.+S                                                      
      GO TO 24                                                         
   11 Q(I,J)=Q(I,J)/(Q(I,K+2)*Q(J,K+2))                                
      GO TO 24                                                         
   10 Q(I,J)=Q(J,I)                                                    
   24 CONTINUE              
c~       write(6,*) "**********************"  , G
      CALL ALSB(Q,ID,K,1,GG,IERR)              
c~       write(6,*) "**********************"  , G                               
c~       write(6,*) "-------------------------------------------------"
      DO 25I=1,K                                                        
      Q(I,K+1)=Q(I,K+1)/Q(I,K+2)                                       
      G(I)=B(I)                                                        
   25 B(I)=B(I)+Q(I,K+1)                                               
      GOTO 222                                                          
   99 IF (PH-PHI) 71,72,72                                             
   71 S=S/U                                                            
      PHI =PH                                                          
      GO TO 88                                                         
   72 S=S*U                                                            
      INT=INT-1                                                        
      DO 418 I=1,K                                                       
  418 B(I)=G(I)                                                        
      CALL CALC(E,*101)                                                
      GO TO 88                                                         
  101 RETURN                                                           
      END                                                              
C                                                                      
C**********************************************************************
C                                                                      
      SUBROUTINE ALSB(A,ID,NA,M,K,IER)                                 
C   RESOLUTION DE SYSTEMES LINEAIRES A ELEMENTS REELS                  
C   A  MATRICE ET 2NDS MEMBRES (REMPLACES PAR LES SOLUTIONS)           
C   ID 1ERE DIMENSION DU BLOC A                                        
C   NA ORDRE DU SYSTEME                                                
C   M  NOMBRE DE 2NDS MEMBRES                                          
C   K  BLOC DE TRAVAIL                                                 
C    IER 0 SI MATRICE NON SINGULIERE. 1 SI MATRICE SINGULIERE
C      INTEGER NM NDEB   
      USE PRECISION
      IMPLICIT real(dp) (a-h,o-z)
      IMPLICIT integer (i-n)        
      DIMENSION A(ID,1),K(100)                                           
      COMMON/UNIT/NOUT,NOUT1
c~       write(6,*) "ID=",ID, ", NA=", NA, ", M=",M, ", K=", K 
      IER=0                                                            
      N=NA                                                             
      DO 1  I=1,N                                                      
    1 K(I)=I                                                           
      NDEB=N+1                                                         
      NM=N+M                                                           
      DO 10 I=1,N                                                      
C     RECHERCHE DU PIVOT MAXIMUM                                       
      AMAX=ABS(A(I,I))                                                 
      JMAX=I                                                           
      I1=I+1                                                           
      IF(I-N)2000,2001,2001                                            
 2000 DO 11 J=I1,N                                                     
      IF(AMAX-ABS(A(I,J)))12,12,11                                     
   12 AMAX=ABS(A(I,J))                                                 
      JMAX=J                                                           
   11 CONTINUE                                                         
C     MATRICE SINGULIERE                                               
 2001 IF(AMAX)13,13,300                                                
   13 IER=1                                                            
      WRITE (NOUT,199)                                                     
199   FORMAT(' MATRICE SINGULIERE')                                    
      RETURN                                                           
C     TRANSPORT DE LA COLONNE                                          
  300 IF(JMAX-I)301,301,14                                             
   14 DO 15 I2=1,N                                                     
      AUX=A(I2,I)                                                      
      A(I2,I)=A(I2,JMAX)                                               
      A(I2,JMAX)=AUX                                                   
   15 CONTINUE                                                         
  301 IF(I-1) 60,60,61                                                 
C     TEST SUR LA SINGULARITE DELA MATRICE                             
   61 S=0.0                                                            
      T=0.0                                                            
      IN=I-1                                                           
      DO 62 IT=1,IN                                                    
      P=A(IT,I)*A(I,IT)                                                
      S=S+P                                                            
   62 T=T+ABS(P)                                                       
      ERA=1.E-6*(T+ABS(A(I,I)-S))                                      
      IF(AMAX-ERA) 63,63,60                                            
   63 IER=2                                                            
      WRITE (NOUT,198)                                                     
198   FORMAT('  MATRICE QUASI SINGULIERE')                             
      RETURN                                                           
C     DIVISION PAR LE PIVOT                                            
   60 DO 16 J2=I1,NM                                                   
      A(I,J2)=A(I,J2)/A(I,I)                                           
   16 CONTINUE                                                         
C     SUBSTITUTION DES LIGNES                                          
      IF(I-N)2002,2003,2003                                            
2002  DO 20 I3=I1,N                                                    
      DO 19 J3=I1,NM                                                   
      A(I3,J3)=A(I3,J3)-A(I3,I)*A(I,J3)                                
   19 CONTINUE                                                         
   20 CONTINUE                                                         
C     SORTIE INDICES                                                   
 2003 IF(JMAX-I)10,10,311                                              
  311 NAB=K(JMAX)                                                      
      K(JMAX)=K(I)                                                     
      K(I)=NAB                                                         
   10 CONTINUE                                                         
      IF(N.EQ.1) RETURN                                                
C     CALCUL DES SOLUTIONS                                             
      DO 70 KC=NDEB,NM                                                 
      J=N                                                              
   52 I=J-1                                                            
   50 A(I,KC)=A(I,KC)-A(J,KC)*A(I,J)                                   
      I=I-1                                                            
      IF(I)51,51,50                                                    
   51 J=J-1                                                            
      IF(J-1)70,70,52                                                  
   70 CONTINUE                                                         
C     CLASSEMENT DES SOLUTIONS                                         
      DO 120 I=1,N                                                     
  100 J=K(I)                                                           
      IF(J-I)120,120,130                                               
  130 K(I)=K(J)                                                        
      K(J)=J                                                           
      DO 110 MP=NDEB,NM                                                
      AUX=A(J,MP)                                                      
      A(J,MP)=A(I,MP)                                                  
      A(I,MP)=AUX                                                      
  110 CONTINUE                                                         
      GO TO 100                                                        
  120 CONTINUE    
c~       write(6,*)"======" , K
  500 RETURN                                                           
      END                                                              
C                                                                      
C**********************************************************************
C                                                                      
C        SUBROUTINE MINV(A,N,D)                                        
C                                                                      
C           INVERT A MATRIX                                            
C                                                                      
C        DESCRIPTION OF PARAMETERS                                     
C           A - INPUT MATRIX, DESTROYED IN COMPUTATION AND REPLACED BY 
C               RESULTANT INVERSE.                                     
C           N - ORDER OF MATRIX A                                      
C           D - RESULTANT DETERMINANT                                  
C           L - WORK VECTOR OF LENGTH N                                
C           M - WORK VECTOR OF LENGTH N                                
C                                                                      
C        REMARKS                                                       
C           MATRIX A MUST BE A GENERAL MATRIX                          
C        METHOD                                                        
C           THE STANDARD GAUSS-JORDAN METHOD IS USED. THE DETERMINANT  
C           IS ALSO CALCULATED. A DETERMINANT OF ZERO INDICATES THAT   
C           THE MATRIX IS SINGULAR.                                    
C     ..................................................................
      SUBROUTINE MINV(A,N,D)      
            USE PRECISION
      IMPLICIT real(dp) (a-h,o-z)
      IMPLICIT integer (i-n)                                     
      DIMENSION A(1600)                                                 
      DIMENSION L(40),M(40)                                            
C        ...............................................................
C        SEARCH FOR LARGEST ELEMENT                                    
C                                                                      
      D=1.0                                                            
      NK=-N                                                            
      DO 80 K=1,N                                                      
      NK=NK+N                                                          
      L(K)=K                                                           
      M(K)=K                                                           
      KK=NK+K                                                          
      BIGA=A(KK)                                                       
      DO 20 J=K,N                                                      
      IZ=N*(J-1)                                                       
      DO 20 I=K,N                                                      
      IJ=IZ+I                                                          
   10 IF( ABS(BIGA)- ABS(A(IJ))) 15,20,20                              
   15 BIGA=A(IJ)                                                       
      L(K)=I                                                           
      M(K)=J                                                           
   20 CONTINUE                                                         
C        INTERCHANGE ROWS                                              
      J=L(K)                                                           
      IF(J-K) 35,35,25                                                 
   25 KI=K-N                                                           
      DO 30 I=1,N                                                      
      KI=KI+N                                                          
      HOLD=-A(KI)                                                      
      JI=KI-K+J                                                        
      A(KI)=A(JI)                                                      
   30 A(JI) =HOLD                                                      
C        INTERCHANGE COLUMNS                                           
   35 I=M(K)                                                           
      IF(I-K) 45,45,38                                                 
   38 JP=N*(I-1)                                                       
      DO 40 J=1,N                                                      
      JK=NK+J                                                          
      JI=JP+J                                                          
      HOLD=-A(JK)                                                      
      A(JK)=A(JI)                                                      
   40 A(JI) =HOLD                                                      
C        DIVIDE COLUMN BY MINUS PIVOT (VALUE OF PIVOT ELEMENT IS       
C        CONTAINED IN BIGA)                                            
   45 IF(ABS(BIGA).GT.1.E-5)GO TO 48                                   
      D=0.0                                                            
      RETURN                                                           
   48 DO 55 I=1,N                                                      
      IF(I-K) 50,55,50                                                 
   50 IK=NK+I                                                          
      A(IK)=A(IK)/(-BIGA)                                              
   55 CONTINUE                                                         
C        REDUCE MATRIX                                                 
      DO 65 I=1,N                                                      
      IK=NK+I                                                          
      HOLD=A(IK)                                                       
      IJ=I-N                                                           
      DO 65 J=1,N                                                      
      IJ=IJ+N                                                          
      IF(I-K) 60,65,60                                                 
   60 IF(J-K) 62,65,62                                                 
   62 KJ=IJ-I+K                                                        
      A(IJ)=HOLD*A(KJ)+A(IJ)                                           
   65 CONTINUE                                                         
C        DIVIDE ROW BY PIVOT                                           
      KJ=K-N                                                           
      DO 75 J=1,N                                                      
      KJ=KJ+N                                                          
      IF(J-K) 70,75,70                                                 
   70 A(KJ)=A(KJ)/BIGA                                                 
   75 CONTINUE                                                         
C        PRODUCT OF PIVOTS                                             
C     D=D*BIGA                                                         
C                                                                      
C        REPLACE PIVOT BY RECIPROCAL                                   
C                                                                      
      A(KK)=1.0/BIGA                                                   
   80 CONTINUE                                                         
C                                                                      
C        FINAL ROW AND COLUMN INTERCHANGE                              
C                                                                      
      K=N                                                              
  100 K=(K-1)                                                          
      IF(K) 150,150,105                                                
  105 I=L(K)                                                           
      IF(I-K) 120,120,108                                              
  108 JQ=N*(K-1)                                                       
      JR=N*(I-1)                                                       
      DO 110 J=1,N                                                     
      JK=JQ+J                                                          
      HOLD=A(JK)                                                       
      JI=JR+J                                                          
      A(JK)=-A(JI)                                                     
  110 A(JI) =HOLD                                                      
  120 J=M(K)                                                           
      IF(J-K) 100,100,125                                              
  125 KI=K-N                                                           
      DO 130 I=1,N                                                     
      KI=KI+N                                                          
      HOLD=A(KI)                                                       
      JI=KI-K+J                                                        
      A(KI)=-A(JI)                                                     
  130 A(JI) =HOLD                                                      
      GO TO 100                                                        
  150 RETURN                                                           
      END                                                              
C                                                                      
C**********************************************************************
C                                                                      
      SUBROUTINE CONNEX   
            USE PRECISION
      IMPLICIT real(dp) (a-h,o-z)
      IMPLICIT integer (i-n)                                             
C     LAISSE A LA DISPOSITION DE L' UTILISATEUR                        
      COMMON/TH/BT(10,40),GVT(8,40),NBT(10,40),NGT(8,40),MONOT(40),    
     1 IOGVT(40)                                                       
      COMMON/OPT/IO(20)                                                
      COMMON/UNIT/NOUT,NOUT1
      ICON=IO(5)                                                       
      GO TO (1,2,3,4,5),ICON                                             
C**   J.P  OXYDATION DU VERT                                           
1     BT(3,2)=0.63*BT(2,1)*BT(3,1)/BT(2,2)                             
      RETURN                                                           
C**   JACQUES                                                          
2     BT(3,2)=BT(2,1)*BT(3,1)/2./BT(2,2)                               
      RETURN                                                           
C**   FERRITES BEATRICE                                                
3     DO 10 K=2,20                                                     
10    BT(1,K)=BT(1,1)+0.11                                             
      RETURN                                                           
C**   JEAN-MARC                                                        
4     BT(3,3)=BT(2,1)*BT(3,1)/3./BT(2,3)                               
      RETURN
C**   GAETAN
5     BT(3,3)=BT(2,1)*BT(3,1)/3./BT(2,3)
      BT(3,4)=BT(2,6)*BT(3,6)/3./BT(2,4)
      RETURN
      END                                                              
C
C******************* ajout pour compatibilite avec les reels doubles **
C
c~        REAL*8 FUNCTION DXABS(Z)
c~        COMPLEX*16 Z
c~        REAL*8 RT
c~        RT=DSQRT(REAL(Z)*REAL(Z)+AIMAG(Z)*AIMAG(Z))
c~        DXABS=RT
c~        RETURN
c~        END


C                                                                      
C**********************************************************************
C                                                                      
      SUBROUTINE CEGREN(A,R,N,MV)
      USE PRECISION    
      IMPLICIT real(dp) (a-h,o-z)
      IMPLICIT integer (i-n)
c~       real*8 DXABS
    
      COMPLEX(dp) A,R,XT, COS,CO                                           
C      COMPLEX CMPLX                                                    
C      COMPLEX CONJG                                                    
      DIMENSION A(10),R(16)                                            
      IP=0                                                             
      IF(MV-1) 10,25,10                                                
   10 IQ=-N                                                            
      DO 20 J=1,N                                                      
      IQ=IQ+N                                                          
      DO 20 I=1,N                                                      
      IJ=IQ+I                                                          
      R(IJ)=(0.0,0.0)                                                  
      IF(I-J) 20,15,20                                                 
   15 R(IJ)=(1.0,0.0)                                                  
   20 CONTINUE                                                         
C            NORMALISATION DU PLUS GRAND TERME EXTRADIAGONAL           
   25 Z=ABS(A(1))                                                     
      MVK=0                                                            
      DO 200 I=1,N                                                     
      K=(I*I+I)/2                                                      
      V=ABS(A(K))                                                     
      IF(Z.GE.V) GO TO 200                                             
      Z=V                                                              
  200 CONTINUE                                                         
      ZU=ABS(A(2))                                                    
      DO 220 I=1,N                                                     
      DO 220 J=I,N                                                     
      IF(I-J) 215,220,215                                              
  215 K=I+(J*J-J)/2                                                    
      V=ABS(A(K))                                                     
      IF(ZU.GE.V)GO TO 220                                             
      ZU=V                                                             
  220 CONTINUE                                                         
      IF(ZU.EQ.0.)GO TO 165                                            
      Y=1.D30     
      NN=((N-1)*N)/2                                                   
      NNL=NN+N                                                         
      YZ= FLOAT(NN)                                                    
      YU=1.D37/ SQRT(YZ)                                               
      UW=1.D-37/SQRT(YZ)                                               
      IF(Z-1.D-44)133,134,134                                          
  133 IF(ZU-UW)141,142,142                                             
  142 Z=ZU/YU                                                          
      GO TO 400                                                        
  141 MVK=MVK+1                                                        
      DO 147 K=1,NNL                                                   
  147    A(K)=A(K)*1.D50   
      Z=Z*1.D50                                                        
      ZU=ZU*1.D50                                                      

      GO TO 137                                                        
  134 IF(ZU-UW)401,137,137                                             
  401 Z=Z/Y                                                            
      GO TO 400                                                        
  137 Z=Z/Y                                                            
      ZU=ZU/YU                                                         
      IF(Z-ZU)132,400,400                                              
  132 Z=ZU                                                             
  400 MVK=MVK+1                                                        
      DO 205 K=1,NNL                                                   
      A(K)=A(K)/Z                                                      
  205 CONTINUE                                                         
  210 ANORM=0.0                                                        
      DO 35 I=1,N                                                      
      DO 35 J=I,N                                                      
      IF(I-J) 30,35,30                                                 
   30 IA=I+(J*J-J)/2                                                   
      ANORM=ANORM+ABS(A(IA))*ABS(A(IA))                              
   35 CONTINUE                                                         
      IF(ANORM) 165,165,40                                             
   40 ANORM=1.414*SQRT(ANORM) 
      ANRMX=ANORM*1.0D-7/FLOAT(N)                                      
      IND=0                                                            
      THR=ANORM                                                        
   45 THR=THR/FLOAT(N)                                                 
   50 L=1                                                              
      LQ=0                                                             
   55 M=L+1                                                            
      LQ=LQ+L-1                                                        
   60 MQ=(M*M-M)/2                                                     
      LM=L+MQ                                                          
      X=ABS(A(LM))                                                    
   62 IF((ABS(A(LM))-THR)) 129,65,65                                    
  129 IP=IP+1                                                          
      IF(IP.LT.NN)GO TO 130                                            
      IND=0                                                            
      GO TO 130                                                        
   65 IND=1                                                            
      LL=L+LQ                                                          
      MM=M+MQ                                                          
      X=0.5*REAL(A(LL)-A(MM))                                          
      YYYY= X*X+REAL(A(LM))*REAL(A(LM))+
     1AIMAG(A(LM))*AIMAG(A(LM))
      Y=SQRT(YYYY)    
      IF(X)70,75,76                                                    
  70  CO=A(LM)/(X-Y)                                                   
      GO TO 81                                                         
  76  CO=A(LM)/(X+Y)                                                   
      GO TO 81                                                         
  75  CO=A(LM)/ABS(A(LM))                                             
  81  YP=1.+REAL(CO)*REAL(CO)+AIMAG(CO)*AIMAG(CO)                      
      SIN=1./SQRT(YP)                                                  
      COS=CO*SIN                                                       
      IF(ABS(COS).NE.0.0)GO TO83                                      
      IP=IP+1                                                          
      GO TO 130                                                        
  83  IP=0                                                             
      ILQ=N*(L-1)                                                      
      IMQ=N*(M-1)                                                      
      DO 125 I=1,N                                                     
      IQ=(I*I-I)/2                                                     
      IF(I-L)80,115,96                                                 
  80  IM=I+MQ                                                          
      IL=I+LQ                                                          
      XT=SIN*A(IL)+CONJG(COS)*A(IM)                                    
      A(IM)=-COS*A(IL)+SIN*A(IM)                                       
      A(IL)=XT                                                         
      GO TO 115                                                        
  96  IF(I-M)85,115,86                                                 
  85  IM=I+MQ                                                          
      IL=L+IQ                                                          
      XT=SIN*A(IL)+COS*CONJG(A(IM))                                    
      A(IM)=-COS*CONJG(A(IL))+SIN*A(IM)                                
      A(IL)=XT                                                         
      GO TO 115                                                        
  86  IM=M+IQ                                                          
      IL=L+IQ                                                          
      XT=SIN*A(IL)+COS*A(IM)                                           
      A(IM)=-CONJG(COS)*A(IL)+SIN*A(IM)                                
      A(IL)=XT                                                         
  115 IF(MV-1) 120,125,120                                             
  120 ILR=ILQ+I                                                        
      IMR=IMQ+I                                                        
      XT=R(ILR)*SIN+R(IMR)*CONJG(COS)                                  
      R(IMR)=-R(ILR)*COS+R(IMR)*SIN                                    
      R(ILR)=XT                                                        
  125 CONTINUE                                                         
      Y=(REAL(A(LL))+REAL(A(MM))*(YP-1.)+2.*
     &REAL(CONJG(CO)*A(LM))) /YP 
      X=(REAL(A(MM))+REAL(A(LL))*(YP-1.)-2.*
     &REAL(CONJG(CO)*A(LM)))/YP  
      A(LM)=(A(LM)-CO*CO*CONJG(A(LM))+CO*(A(MM)-A(LL)))/YP             
      A(LL)=CMPLX(Y,0.0_dp, dp)                                                
      A(MM)=CMPLX(X,0.0_dp, dp)                                                
c~       A(LL)=CMPLX(Y,0.)                                                
c~       A(MM)=CMPLX(X,0.)                                                
  130 IF(M-N) 135,140,135                                              
  135 M=M+1                                                            
      GO TO 60                                                         
  140 IF(L-(N-1)) 145,150,145                                          
  145 L=L+1                                                            
      GO TO 55                                                         
  150 IF(IND-1) 160,155,160                                            
  155 IND=0                                                            
      GO TO 50                                                         
  160 IF(SIN.EQ.1.)GO TO 165                                           
      IF(THR-ANRMX) 165,165,45                                         
  165 IQ=-N                                                            
      DO 185 I=1,N                                                     
      IQ=IQ+N                                                          
      LL=(I*I+I)/2                                                     
      JQ=N*(I-2)                                                       
      DO 185 J=I,N                                                     
      JQ=JQ+N                                                          
      MM=(J*J+J)/2                                                     
      IF(REAL(A(LL))-REAL(A(MM)))170,185,185                           
  170 XT=A(LL)                                                         
      A(LL)=A(MM)                                                      
      A(MM)=XT                                                         
      IF(MV-1) 175,185,175                                             
  175 DO 180 K=1,N                                                     
      ILR=IQ+K                                                         
      IMR=JQ+K                                                         
      XT=R(ILR)                                                        
      R(ILR)=R(IMR)                                                    
  180 R(IMR)=XT                                                        
  185 CONTINUE                                                         
      IF(MVK.EQ.0) GO TO 450                                           
      IF(MVK.EQ.1) GO TO 460                                           
      DO 360 K=1,NNL                                                   
  360 A(K)=A(K)*1.D-50   
  460 DO 350 K=1,NNL                                                   
  350 A(K)=A(K)*Z                                                      
  450 RETURN                                                           
      END                                                              
c
c***************************************************************
c
      SUBROUTINE LASER(CMAX,CMIN,CN,I17)
C
C Programme permettant de tracer NC courbes
C
      USE PRECISION
      IMPLICIT real(dp) (a-h,o-z)
      IMPLICIT integer (i-n)
      COMMON/UN/Q(256,42),Y(256),B(40),BF(256),HBRUIT,N,K,NS
      COMMON/SOSP/YSS(256,5),ICOURB
      DIMENSION TTY(256,8)
      INTEGER st,sex 
c       spectre experimental     
      DO 7 I=1,256    
      TTY(I,2)=Q(I,K+2)                                                   
    7 CONTINUE
c       spectre theorique                                                         
      DO 6 J=1,256
      TTY(J,3)=Y(J)
   6  CONTINUE
      DO 71 I=1,128
      TTY(I,1)=(I-129)*CN
      TTY(128+I,1)=I*CN 
   71 CONTINUE
c       sous- spectres theorique
      IF (I17.NE.0) THEN
      DO 8 I = 1,ICOURB
      DO 9 J = 1,256
      TTY(J,I+3)=YSS(J,I)
    9 CONTINUE
    8 CONTINUE
      ENDIF
      open(14,file='Spect.dat',status='unknown',form='formatted')
c 666     format(2X,f6.2,2(1x,I8),5(1X,f12.2),/)        
  666 format(2X,f6.2,7(1x,f12.2))     
      DO 10 i=1,256
      st=INT(TTY(i,2))
      sex=INT(TTY(i,3))
      write(14,666) (TTY(i,j),j=1,8)
c       write(14,666) TTY(i,1),st,sex,(TTY(i,j),j=4,8)
  10  continue
      end
c    *****************************************************      
      SUBROUTINE RESULTATS
c    *****************************************************
      USE PRECISION
      IMPLICIT real(dp) (a-h,o-z)
      IMPLICIT integer (i-n)
      DIMENSION S(40),SL(42),T(34),BTM(7),BTM2(7)
      COMMON/UN/Q(256,42),Y(256),B(40),BF(256),HBRUIT,N,K,NS
      COMMON/COMP/NPAS,NMAX,NS1,NS2
      COMMON/TH/BT(10,40),GVT(8,40),NBT(10,40),NGT(8,40),MONOT(40),
     1 IOGVT(40)
      COMMON/OPT/IO(20)
      COMMON/VIR/PH,VQ(40,40),ETBT(10,40),ETGVT(8,40)
c
c     ************************************************
c     FORMAT D'ECRITURE
c
   20 FORMAT(//,'#',50X,'CARACTERISTIQUES DES SPECTRES',//)
   31 FORMAT ('#',1X,20X,'MMS',8X,'MMS',6X,'MMS',9X,'KG',25X,
     1'DEG',23X,'DEG',23X,' %  ',/)
   22 FORMAT ('#',1X,39X,F6.2,5X,F6.2,5X,F7.0,5X,F10.0,/)
   29 FORMAT ('#',1X,20X,'DI',7X,'GA',7X,' SQ',7X,'CH',7X,' ETA',7X,
     1'TETA',7X,'GAMA',7X,'BETA',7X,'ALFA',7x,'TAUX', /)
c    ***********************************************
c    TABLEAU DES VALEURS
c
      WRITE (15,20)
      ST=0.
      WRITE (15,29)
      WRITE (15,31)
c     *********************************************
c     CALCUL DE LA DISTRIBUTION
c
      DO 203 NT=1,NS
      S(NT)=ABS(BT(2,NT))*BT(3,NT)
      ST=ST+S(NT)
  203 CONTINUE
      SINT=0.
      DO 205 NT=1,NS
      S(NT)= 100.*S(NT)/ST
      T(NT+2)=S(NT)
      SINT=SINT+S(NT)
      WRITE (15,30)NT,BT(1,NT),BT(2,NT),(BT(I,NT),I=4,10),S(NT)
   30 FORMAT(/,'#',' SPECTRE ',I2,2X,F6.3,F5.2,F6.3, F8.2 ,5F9.2,F6.2)
205   continue
      IF(IO(13).EQ.0) GO TO 209
c
C    LISSAGE DE LA DISTRIBUTION
      T(1)=0.
      T(2)=0.
      T(NS+3)=0.
      T(NS+4)=0.
      NSP2=NS+2
      DO 213 I=1,NSP2
      SL(I)=(T(I)+2.*T(I+1)+T(I+2))/4.
      write(15,*) I,SL(I)  
213   CONTINUE
c     *********************************
C     CALCUL DE VALEURS MOYENNES
c
212   NSS=NS
      IF(NS2.NE.0) NSS=NS2
      DO 210 J=1,7
      BTM(J)=0.
      BTM2(J)=0.
      GO TO(211,210,210,211,211,210,211),J
211   DO 217 NT=1,NSS
      BTM(J)=BTM(J)+BT(J,NT)*S(NT)/SINT
      BTM2(J)=BTM2(J)+(BT(J,NT)**2)*S(NT)/SINT
217   CONTINUE
  210 CONTINUE
      WRITE (15,43) NSS
43    FORMAT(//,'#',' CALCUL SUR LES ',I3,' PREMIERS SPECTRES',//)
      WRITE (15,45)
45    FORMAT ('#',1X,20X,'DI',7X,'GA',7X,' SQ',7X,'CH',7X,' ETA',7X,
     1'TETA',7X,'GAMA',7X,'BETA',7X,'ALFA', /)
      WRITE (15,41)BTM
   41 FORMAT(/,'#',' MOYENNE',6X,7F12.3,/)
  209 CONTINUE
      RETURN
      END
      


