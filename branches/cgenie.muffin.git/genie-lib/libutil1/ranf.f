C Quick portable random number generator from Numerical Recipes
C
      REAL FUNCTION RANF(IDUM)
      IMPLICIT NONE
C
C Returns a uniform random deviate between 0.0 and 1.0.
C Set IDUM to any negative value to initialise or reinitialise the
C sequence.
C
      INTEGER M,IA,IC,IFF,IDUM,J,IR,IY
      REAL    RM
      PARAMETER (M=714025, IA=1366,IC=150889,RM=1./M)
      DIMENSION IR(97)
      DATA IFF /0/
C
      IF (IDUM.LT.0.OR.IFF.EQ.0) THEN
        IFF=1
        IDUM=MOD(IC-IDUM,M)
        DO 11 J=1,97
          IDUM=MOD(IA*IDUM+IC,M)
          IR(J)=IDUM
 11     CONTINUE
        IDUM=MOD(IA*IDUM+IC,M)
        IY=IDUM
      ENDIF
      J=1+(97*IY)/M
      IF (J.GT.97.OR.J.LT.1) STOP
      IY=IR(J)
      RANF=IY*RM
      IDUM=MOD(IA*IDUM+IC,M)
      IR(J)=IDUM
      RETURN
      END


     
        
