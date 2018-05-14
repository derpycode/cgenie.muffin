C
      SUBROUTINE HESSEN(A,IDIM1,M,IDIM2,B)
      IMPLICIT NONE
      INTEGER IDIM1,IDIM2,M,LC,N,N1,N2,NI,J,K
      REAL A,B,DIV
      DIMENSION A(IDIM1,IDIM2)
      DIMENSION B(M)
      DOUBLE PRECISION SUM
      IF (M-2 < 0) GOTO 30
      IF (M-2 == 0) GOTO 30
      IF (M-2 > 0) GOTO 32
   32 DO 40 LC=3,M
      N=M-LC+3
      N1=N-1
      N2=N-2
      NI=N1
      DIV=ABS(A(N,N-1))
      DO 2 J=1,N2
      IF(ABS(A(N,J))-DIV < 0) GOTO 2
      IF(ABS(A(N,J))-DIV == 0) GOTO 2
      IF(ABS(A(N,J))-DIV > 0) GOTO 1
    1 NI=J
      DIV=ABS(A(N,J))
    2 CONTINUE
      IF(DIV < 0) GOTO 3
      IF(DIV == 0) GOTO 40
      IF(DIV > 0) GOTO 3
    3 IF(NI-N1 < 0) GOTO 4
      IF(NI-N1 == 0) GOTO 7
      IF(NI-N1 > 0) GOTO 4
    4 DO 5 J=1,N
      DIV=A(J,NI)
      A(J,NI)=A(J,N1)
    5 A(J,N1)=DIV
      DO 6 J=1,M
      DIV=A(NI,J)
      A(NI,J)=A(N1,J)
    6 A(N1,J)=DIV
    7 DO 26 K=1,N1
   26 B(K)=A(N,K)/A(N,N-1)
      DO 45 J=1,M
      SUM=0.0
      IF(J-N1 < 0) GOTO 46
      IF(J-N1 == 0) GOTO 43
      IF(J-N1 > 0) GOTO 43
   46 IF(B(J) < 0) GOTO 41
      IF(B(J) == 0) GOTO 43
      IF(B(J) > 0) GOTO 41
   41 A(N,J)=0.0
      DO 42 K=1,N1
      A(K,J)=A(K,J)-A(K,N1)*B(J)
   42 SUM=SUM+A(K,J)*B(K)
      GO TO 45
   43 DO 44 K=1,N1
   44 SUM=SUM+A(K,J)*B(K)
   45 A(N1,J)=REAL(SUM)
   40 CONTINUE
   30 RETURN
      END
