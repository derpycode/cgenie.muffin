      SUBROUTINE QRT(A,N,R,SIG,D,IDIM1,IDIM2)
      IMPLICIT NONE
      INTEGER IDIM1,IDIM2,N1,N,IA,IP,J,J1,IQ,I,L
      REAL    A,D,SIG,R,G,XK,AL,PSI,C,E
      DIMENSION PSI(3),G(3),A(IDIM1,IDIM2)
      N1=N-1
      IA=N-2
      IP=IA
      IF(N-3 < 0) GOTO 101
      IF(N-3 == 0) GOTO 10
      IF(N-3 > 0) GOTO 60
   60 DO 12 J=3,N1
      J1=N-J
      IF(ABS(A(J1+1,J1))-D < 0) GOTO 10
      IF(ABS(A(J1+1,J1))-D == 0) GOTO 10
      IF(ABS(A(J1+1,J1))-D > 0) GOTO 11
   11 IF(ABS(A(J1+1,J1)*A(J1+2,J1+1)*(ABS(A(J1+1,J1+1)+A(J1+2,J1+2)
     1-SIG)+ABS(A(J1+3,J1+2)))/(A(J1+1,J1+1)*(A(J1+1,J1+1)-SIG)+A(J1+1,
     2J1+2)*A(J1+2,J1+1)+R))-D < 0) GOTO 10
      IF(ABS(A(J1+1,J1)*A(J1+2,J1+1)*(ABS(A(J1+1,J1+1)+A(J1+2,J1+2)
     1-SIG)+ABS(A(J1+3,J1+2)))/(A(J1+1,J1+1)*(A(J1+1,J1+1)-SIG)+A(J1+1,
     2J1+2)*A(J1+2,J1+1)+R))-D == 0) GOTO 10
      IF(ABS(A(J1+1,J1)*A(J1+2,J1+1)*(ABS(A(J1+1,J1+1)+A(J1+2,J1+2)
     1-SIG)+ABS(A(J1+3,J1+2)))/(A(J1+1,J1+1)*(A(J1+1,J1+1)-SIG)+A(J1+1,
     2J1+2)*A(J1+2,J1+1)+R))-D > 0) GOTO 12
   12 IP=J1
   10 DO 14 J=1,IP
      J1=IP-J+1
      IF(ABS(A(J1+1,J1))-D < 0) GOTO 13
      IF(ABS(A(J1+1,J1))-D == 0) GOTO 13
      IF(ABS(A(J1+1,J1))-D > 0) GOTO 14
   14 IQ=J1
   13 DO 100 I=IP,N1
      IF(I-IP < 0) GOTO 16
      IF(I-IP == 0) GOTO 15
      IF(I-IP > 0) GOTO 16
   15 G(1)=A(IP,IP)*(A(IP,IP)-SIG)+A(IP,IP+1)*A(IP+1,IP)+R
      G(2)=A(IP+1,IP)*(A(IP,IP)+A(IP+1,IP+1)-SIG)
      G(3)=A(IP+1,IP)*A(IP+2,IP+1)
      A(IP+2,IP)=0.0
      GO TO 19
   16 G(1)=A(I,I-1)
      G(2)=A(I+1,I-1)
      IF(I-IA < 0) GOTO 17
      IF(I-IA == 0) GOTO 17
      IF(I-IA > 0) GOTO 18
   17 G(3)=A(I+2,I-1)
      GO TO 19
   18 G(3)=0.0
   19 XK=SIGN(SQRT(G(1)**2+G(2)**2+G(3)**2),G(1))
      IF(XK < 0) GOTO 23
      IF(XK == 0) GOTO 24
      IF(XK > 0) GOTO 23
   23 AL=G(1)/XK+1.0
      PSI(1)=G(2)/(G(1)+XK)
      PSI(2)=G(3)/(G(1)+XK)
      GO TO 25
   24 AL=2.0
      PSI(1)=0.0
      PSI(2)=0.0
   25 IF(I-IQ < 0) GOTO 26
      IF(I-IQ == 0) GOTO 27
      IF(I-IQ > 0) GOTO 26
   26 IF(I-IP < 0) GOTO 29
      IF(I-IP == 0) GOTO 28
      IF(I-IP > 0) GOTO 29
   28 A(I,I-1)=-A(I,I-1)
      GO TO 27
   29 A(I,I-1)=-XK
   27 DO 30 J=I,N
      IF(I-IA < 0) GOTO 31
      IF(I-IA == 0) GOTO 31
      IF(I-IA > 0) GOTO 32
   31 C=PSI(2)*A(I+2,J)
      GO TO 33
   32 C=0.0
   33 E=AL*(A(I,J)+PSI(1)*A(I+1,J)+C)
      A(I,J)=A(I,J)-E
      A(I+1,J)=A(I+1,J)-PSI(1)*E
      IF(I-IA < 0) GOTO 34
      IF(I-IA == 0) GOTO 34
      IF(I-IA > 0) GOTO 30
   34 A(I+2,J)=A(I+2,J)-PSI(2)*E
   30 CONTINUE
      IF(I-IA < 0) GOTO 35
      IF(I-IA == 0) GOTO 35
      IF(I-IA > 0) GOTO 36
   35 L=I+2
      GO TO 37
   36 L=N
   37 DO 40 J=IQ,L
      IF(I-IA < 0) GOTO 38
      IF(I-IA == 0) GOTO 38
      IF(I-IA > 0) GOTO 39
   38 C=PSI(2)*A(J,I+2)
      GO TO 41
   39 C=0.0
   41 E=AL*(A(J,I)+PSI(1)*A(J,I+1)+C)
      A(J,I)=A(J,I)-E
      A(J,I+1)=A(J,I+1)-PSI(1)*E
      SELECT CASE (I-IA)
      CASE (:-1)
         GOTO 42
      CASE (0)
         GOTO 42
      CASE (1:)
         GOTO 40
      CASE DEFAULT
         STOP
      END SELECT
   42 A(J,I+2)=A(J,I+2)-PSI(2)*E
   40 CONTINUE
      IF(I-N+3 < 0) GOTO 43
      IF(I-N+3 == 0) GOTO 43
      IF(I-N+3 > 0) GOTO 100
   43 E=AL*PSI(2)*A(I+3,I+2)
      A(I+3,I)=-E
      A(I+3,I+1)=-PSI(1)*E
      A(I+3,I+2)=A(I+3,I+2)-PSI(2)*E
  100 CONTINUE
  101 RETURN
      END
