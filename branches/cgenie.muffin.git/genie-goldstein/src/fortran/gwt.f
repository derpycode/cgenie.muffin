      subroutine gwtcnr(alato,jg)
      implicit none
      real alato(*),sit,weight,pi
      integer jg,j
c
      pi=2.*asin(1.)
      do j=1,jg*2
         if (j.le.jg) then
            call gwtlt1(sit,weight,j,jg)
         else
            call gwtlt1(sit,weight,jg*2+1-j,jg)
            sit=-sit
         end if
         alato(j)=asin(sit)*180.0/pi
      end do
      end
c
      subroutine gwtcnr8(alato,jg)
      implicit none
      real alato(*),sit,weight,pi
      integer jg,j
c
      pi=2.*asin(1.)
      do j=1,jg*2
         if (j.le.jg) then
            call gwtlt18(sit,weight,j,jg)
         else
            call gwtlt18(sit,weight,jg*2+1-j,jg)
            sit=-sit
         end if
         alato(j)=asin(sit)*180.0/pi
      end do
      end
c
      subroutine gwtbox(alatbo,jg)
      implicit none
      integer j,jg
      real    pi,sit,weight
      real    alatbo(*)
      real zslat
c
      pi=2.*asin(1.)
      alatbo(1)=90.0
      zslat=1.0
      do j=1,jg*2
         if (j.le.jg) then
            call gwtlt1(sit,weight,j,jg)
         else
            call gwtlt1(sit,weight,jg*2+1-j,jg)
            sit=-sit
         end if
         zslat=zslat-weight
         if (zslat.lt.-1.0) zslat=-1.0
         alatbo(j+1)=real(asin(zslat)*180.0/pi)
      end do
      end
      SUBROUTINE GWTLT(SIT,WEIGHT,J,JG)
      IMPLICIT NONE
      INTEGER NNN,JG,K,J,N
      REAL PI,AJ
      PARAMETER(PI=3.14159265359)
      REAL SIT,WEIGHT
      REAL HH,TH
      REAL ACC,SA,SB,SC,D1,D2,D4,BN,GG,X,AMM,AMN
      REAL EM,ANN,RE,A,DD,DLT,DTH
      ACC=1.0E-23
      SA=SQRT(.5)
      SB=SQRT(1.5)
      SC=SQRT(1.0/3.0)
      D1=1.0
      D2=2.0
      D4=4.0
      NNN=JG+JG
      BN=NNN
      GG=D2*BN+D1
      HH=8.*BN*BN
      K=0
      AJ=J
      TH=PI*(2.*AJ-.5)/GG
      DTH=TH+(COS(TH)/SIN(TH))/HH
      X=COS(DTH)
54    CONTINUE
      AMM=SA
      AMN=X*SB
      EM=SC
      ANN=D1
      DO 51 N=2,NNN
      ANN=ANN+D1
      RE=SQRT(D4*ANN*ANN-D1)/ANN
      A=RE*(X*AMN-EM*AMM)
      AMM=AMN
      AMN=A
      EM=D1/RE
51    CONTINUE
      DD=GG*EM*AMM-X*ANN*A
      K=K+1
      DLT=(D1-X*X)*A/DD
      IF (ABS(DLT).LT.ACC) GOTO 52
      IF (K.GT.50) GOTO 53
      X=X-DLT
      GOTO 54
53    CONTINUE
      WRITE (2,105)
105   FORMAT(15H NO CONVERGENCE)
52    CONTINUE
      WEIGHT=REAL(GG*(D1-X*X)/(DD*DD))
      SIT=REAL(X)
      RETURN
      END
      SUBROUTINE GWTLT1(SIT,WEIGHT,J,JG)
      IMPLICIT NONE
      REAL PI,AJ,SIT,WEIGHT
      PARAMETER(PI=3.14159265359)
      REAL ACC,SA,SB,SC,D1,D2,D4,BN,GG,X,AMM,AMN
      REAL EM,ANN,RE,A,DD,DLT,DTH
      REAL HH,TH
      INTEGER NNN,JG,K,J,N
      ACC=1.0E-16
      SA=SQRT(0.5)
      SB=SQRT(1.5)
      SC=SQRT(1.0/3.0)
      D1=1.0
      D2=2.0
      D4=4.0
      NNN=JG+JG
      BN=NNN
      GG=D2*BN+D1
      HH=8.*BN*BN
      K=0
      AJ=J
      TH=PI*(2.*AJ-.5)/GG
      DTH=TH+(COS(TH)/SIN(TH))/HH
      X=COS(DTH)
54    CONTINUE
      AMM=SA
      AMN=X*SB
      EM=SC
      ANN=D1
      DO 51 N=2,NNN
      ANN=ANN+D1
      RE=SQRT(D4*ANN*ANN-D1)/ANN
      A=RE*(X*AMN-EM*AMM)
      AMM=AMN
      AMN=A
      EM=D1/RE
51    CONTINUE
      DD=GG*EM*AMM-X*ANN*A
      K=K+1
      DLT=(D1-X*X)*A/DD
      IF (ABS(DLT).LT.ACC) GOTO 52
      IF (K.GT.50) GOTO 53
      X=X-DLT
      GOTO 54
53    CONTINUE
      WRITE (2,105)
105   FORMAT(15H NO CONVERGENCE)
52    CONTINUE
      WEIGHT=REAL(GG*(D1-X*X)/(DD*DD))
      SIT=REAL(X)
      RETURN
      END
C
      SUBROUTINE GWTLT18(SIT,WEIGHT,J,JG)
      IMPLICIT NONE
      REAL PI,AJ,TH,WEIGHT,SIT,HH
      PARAMETER(PI=3.14159265359)
      REAL ACC,SA,SB,SC,D1,D2,D4,BN,GG,X,AMM,AMN,
     1 EM,ANN,RE,A,DD,DLT,DTH
      INTEGER NNN,JG,K,J,N
      ACC=1.0E-16
      SA=SQRT(0.5)
      SB=SQRT(1.5)
      SC=SQRT(1.0/3.0)
      D1=1.0
      D2=2.0
      D4=4.0
      NNN=JG+JG
      BN=NNN
      GG=D2*BN+D1
      HH=8.*BN*BN
      K=0
      AJ=J
      TH=PI*(2.*AJ-.5)/GG
      DTH=TH+(COS(TH)/SIN(TH))/HH
      X=COS(DTH)
54    CONTINUE
      AMM=SA
      AMN=X*SB
      EM=SC
      ANN=D1
      DO 51 N=2,NNN
      ANN=ANN+D1
      RE=SQRT(D4*ANN*ANN-D1)/ANN
      A=RE*(X*AMN-EM*AMM)
      AMM=AMN
      AMN=A
      EM=D1/RE
51    CONTINUE
      DD=GG*EM*AMM-X*ANN*A
      K=K+1
      DLT=(D1-X*X)*A/DD
      IF (ABS(DLT).LT.ACC) GOTO 52
      IF (K.GT.50) GOTO 53
      X=X-DLT
      GOTO 54
53    CONTINUE
      WRITE (2,105)
105   FORMAT(15H NO CONVERGENCE)
52    CONTINUE
      WEIGHT=GG*(D1-X*X)/(DD*DD)
      SIT=X
      RETURN
      END
