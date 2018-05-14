      SUBROUTINE AWI (IDL,IL,ALON,JL,ALAT,A,MASK,
     1                IDLO,ILO,ALONO,JLO,ALATO,AO,MASKO,IER)

      IMPLICIT NONE
C
C          This subroutine does an area weighted average from one grid,
C          on a spherical earth, to another.  Logical masks may be assigned
C          for each grid, and only those grid boxes which are masked true
C          on both grids will be used.  A value of AMM   will be assigned
C          to all nodes of the new grid which are initially false or have
C          no data from the old grid.  The new mask will also be changed to
C          false where no data is available.
C
C          Restrictions:  Longitude must be the first dimension and it
C                         be monotonically increasing (West to East).
C
C                         Latitude must be the second dimension and it
C                         must be monotonic.
C
C                         Values for longitude and latitude must be in
C                         degrees.
C
C                         Arrays that wrap around must repeat longitudes
C                         with a 360 degree increment.  It will be assumed
C                         that values in the wrapped input and mask arrays
C                         will also repeat (wrapped values in these arrays
C                         will not be used).
C
C        INPUT
C
C   integer   IDL    First dimension of input A and MASK.
C   integer   IL     Number of grid boxes in longitude for A and MASK.
C   real      ALON   Longitude (deg) limits of grid boxes for A and MASK.
C   integer   JL     Number of grid boxes in latitude for A and MASK.
C   real      ALAT   Latitude (deg) limits of grid boxes for A and MASK.
C   real      A      Array of input data.
C   logical   MASK   Mask for input data (.FALSE. to mask out data).
C
C        OUTPUT
C
C   integer   IDLO   First dimension of output AO and MASKO.
C   integer   ILO    Number of grid boxes in longitude for AO and MASKO.
C   real      ALONO  Longitude (deg) limits of grid boxes for AO and MASKO.
C   integer   JLO    Number of grid boxes in latitude for AO and MASKO.
C   real      ALATO  Latitude (deg) limits of grid boxes for AO and MASKO.
C   real      AO     Array of output data.
C   logical   MASKO  Mask for output data (.FALSE. to mask out data).
C   integer   IER    Error indication:
C                    (values may be summed for multiple errors)
C                    0  no errors
C                    1  input longitude dimension and/or length <=0.
C                    2  output dimension and/or length <=0.
C                    4  input latititude dimension <=0.
C                    8  output latitude dimension <=0.
C                   16  wrap-around on input longitude grid doesn't
C                       repeat (+360).
C                   32  wrap-around on output longitude grid doesn't
C                       repeat (+360).
C                   64  longitude of input is not monotonic increasing.
C                  128  longitude of output is not monotonic increasing.
C                  256  latitude of input is not monotonic.
C                  512  latitude of output is not monotonic.
C                 1024  input longitude wraps but doesn't repeat identically.
C                 2048  output longitude wraps but doesn't repeat identically.
C                   -1  output mask is changed.
C                   -2  output mask contains all false values.
C
      INTEGER IL,JL,ILO,JLO,IDL,IDLO,IER,I,J,IIL,IILO
      INTEGER J1,J2,JJ,I1,I2,K,II,III,IIP
      REAL    API,AMM,ALON,ALONO,SGN,ALAT,ALATO,ALMX
      REAL    ALMN,AL,DLN,ALMXO,ALMNO,DLNO,AMNLTO
      REAL    AMXLTO,AMNLT,AMXLT,AO,AMNLNO,AMXLNO
      REAL    AMNLN,AMXLN,WT,AVG,SLATMX,SLATMN,WLAT
      REAL    SLON,SLONP,SLONMX,SLONMN,DELON,A
      DIMENSION ALON(IL+1),ALAT(JL+1),ALONO(ILO+1),ALATO(JLO+1)
      LOGICAL MASK(IDL,JL),MASKO(IDLO,JLO)
      DIMENSION A(IDL,JL),AO(IDLO,JLO)
C
      DATA API /3.1415926536/
C                       Check dimensions and lengths.
C  PJV MODIFICATION 11 2 92
C
      AMM=50.
C
      IER=0
      IF (IDL.LT.IL.OR.IL.LE.0) IER=1
      IF (IDLO.LT.ILO.OR.ILO.LE.0) IER=IER+2
      IF (JL.LE.0) IER=IER+4
      IF (JLO.LE.0) IER=IER+8
C
      IF (IER.GT.0) RETURN
C                        Check monotonic increasing input longitudes.
      DO 1 I=2,IL
      IF (ALON(I).LE.ALON(I-1)) THEN
        IER=IER+64
        GO TO 2
      ENDIF
  1   CONTINUE
  2   CONTINUE
C                       Check monotonic increasing output longitudes.
      DO 3 I=2,ILO
      IF (ALONO(I).LE.ALONO(I-1)) THEN
        IER=IER+128
        GO TO 4
      ENDIF
  3   CONTINUE
  4   CONTINUE
C                     Check monotonicity of input latitudes.
      SGN=(ALAT(2)-ALAT(1))
      DO 5 J=2,JL
      IF (SGN.LT.0.0) THEN
        IF (ALAT(J)-ALAT(J-1).GE.0) THEN
          IER=IER+256
          GO TO 6
        ENDIF
      ELSE IF (SGN.GT.0.0) THEN
        IF (ALAT(J)-ALAT(J-1).LE.0.0) THEN
          IER=IER+256
          GO TO 6
        ENDIF
      ELSE
        IER=IER+256
        GO TO 6
      ENDIF
  5   CONTINUE
  6   CONTINUE
C                    Check monotonicity of output latitudes.
      SGN=(ALATO(2)-ALATO(1))
      DO 7 J=2,JLO
      IF (SGN.LT.0.0) THEN
        IF (ALATO(J)-ALATO(J-1).GE.0.0) THEN
          IER=IER+512
          GO TO 8
        ENDIF
      ELSE IF (SGN.GT.0.0) THEN
        IF (ALATO(J)-ALATO(J-1).LE.0.0) THEN
          IER=IER+512
          GO TO 8
        ENDIF
      ELSE
        IER=IER+512
        GO TO 8
      ENDIF
  7   CONTINUE
  8   CONTINUE
C                       Find wrap around of input grid, if it exists.
      IIL=IL
      ALMX=ALON(1)
      ALMN=ALON(1)
      DO 10 I=2,IL+1
      ALMX=MAX(ALMX,ALON(I))
      ALMN=MIN(ALMN,ALON(I))
      AL=ABS(ALON(I)-ALON(1))-360.0
      IF (ABS(AL).LE.1.E-4) THEN
        IIL=I-1
        GO TO 11
      ELSE IF (AL.GT.0.0) THEN
        IER=IER+1024
        GO TO 12
      ENDIF
  10  CONTINUE
  11  CONTINUE
      DLN=0.0
      IF (ALMN.LT.0.0) THEN
        DLN=INT(-ALMN/360.0+.001)*360.0
      ELSE IF (ALMN.GT.360.0) THEN
        DLN=-INT(ALMN/360.0+.001)*360.0
      ENDIF
  12  CONTINUE
C                       Find wrap around of output grid, if it exists.
      IILO=ILO
      ALMXO=ALONO(1)
      ALMNO=ALONO(1)
      DO 13 I=2,ILO+1
      ALMXO=MAX(ALMXO,ALONO(I))
      ALMNO=MIN(ALMNO,ALONO(I))
      AL=ABS(ALONO(I)-ALONO(1))-360.0
      IF (ABS(AL).LE.1.E-4) THEN
        IILO=I-1
        GO TO 14
      ELSE IF (AL.GT.0.0) THEN
        IER=IER+2048
        GO TO 15
      ENDIF
  13  CONTINUE
  14  CONTINUE
      DLNO=0.0
      IF (ALMNO.LT.0.0) THEN
        DLNO=INT(-ALMNO/360.0+.001)*360.0
      ELSE IF (ALMNO.GT.360.0) THEN
        DLNO=-INT(ALMNO/360.0+.001)*360.0
      ENDIF
  15  CONTINUE
C                     Test for errors.  Return if any.
      IF (IER.NE.0) RETURN
C                     The output grid needs to begin with or after the
C                     input grid.
      IF (ALMNO+DLNO.LT.ALMN+DLN) DLNO=DLNO+360.0
C
      DO 200 J=1,JLO
C              Find index limits In latitude to cover the new grid.
      J1=JL+1
      J2=0
      AMNLTO=MIN(ALATO(J),ALATO(J+1))
      AMXLTO=MAX(ALATO(J),ALATO(J+1))
C
C                     Search for index limits in J.
C
      DO 17 JJ=1,JL
      AMNLT=MIN(ALAT(JJ),ALAT(JJ+1))
      AMXLT=MAX(ALAT(JJ),ALAT(JJ+1))
C                     FIND JJ LIMITS
      IF (AMXLT.GT.AMNLTO.AND.AMNLT.LT.AMXLTO) THEN
        J1=MIN(JJ,J1)
        J2=MAX(JJ,J2)
      ENDIF
C
  17  CONTINUE
C                     If input grid doesn't at least partially cover the
C                     output grid box, no values will be assigned.  Mask out
C                     all values for the latitude.
C
      IF (J2.LT.J1) THEN
        DO 20 I=1,IILO
        AO(I,J)=amm
        IF (MASKO(I,J)) IER=-1
        MASKO(I,J)=.FALSE.
  20    CONTINUE
        GO TO 200
      ENDIF
C  TEMPORARY *******************************************************
C      AMNLT=MIN(MIN(ALAT(J1),ALAT(J1+1)),MIN(ALAT(J2),ALAT(J2+1)))
C      AMXLT=MAX(MAX(ALAT(J1),ALAT(J1+1)),MAX(ALAT(J2),ALAT(J2+1)))
C
C      WRITE (NOUT,90) J1,J2,AMNLT,AMXLT
C  90  FORMAT ('J',2I6,2F9.3)
C
      DO 100 I=1,IILO
C              No need to compute if it is masked out.
      IF (.NOT.MASKO(I,J)) GO TO 100
C              Find index limits in longitude to cover the new grid.
      I1=3*IL+1
      I2=0
      AMNLNO=MIN(ALONO(I),ALONO(I+1))+DLNO
      AMXLNO=MAX(ALONO(I),ALONO(I+1))+DLNO
C
C                     Search for index limits in I.
C                     Because of wrap around it is necessary to
C                     look through the data twice.
C                     The output grid longitudes have been adjusted
C                     (using DLNO) such that the first longitude in
C                     the output grid is greater than the first
C                     longitude on the input grid.
C
      DO 35 K=0,1
      DO 30 II=1,IIL
      AMNLN=MIN(ALON(II),ALON(II+1))+DLN+K*360.0
      AMXLN=MAX(ALON(II),ALON(II+1))+DLN+K*360.0
C                     FIND II LIMITS
      IF (AMXLN.GT.AMNLNO.AND.AMNLN.LT.AMXLNO) THEN
        I1=MIN(II+K*IL,I1)
        I2=MAX(II+K*IL,I2)
      ENDIF
C
  30  CONTINUE
  35  CONTINUE
C                     If input grid doesn't partially cover the output
C                     grid box, no values will be assigned.  Mask out
C                     the grid box.
      IF (I2.LT.I1) THEN
        AO(I,J)=amm
        IF (MASKO(I,J)) IER=-1
        MASKO(I,J)=.FALSE.
        GO TO 100
      ENDIF
C  TEMPORARY
C      WRITE (NOUT,91) I1,I2,AMNLNO,AMXLNO
C  91  FORMAT ('I',2I6,2F9.3)
C
      WT=0.0
      AVG=0.0
      DO 50 JJ=J1,J2
      SLATMX=MAX(ALAT(JJ),ALAT(JJ+1))
      SLATMN=MIN(ALAT(JJ),ALAT(JJ+1))
      WLAT=MAX(SIN(MIN(AMXLTO,SLATMX)*API/180.)-
     1         SIN(MAX(AMNLTO,SLATMN)*API/180.),0.0)
      IF (WLAT.NE.0.0) THEN
        DO 40 III=I1,I2
        SLON=DLN
        SLONP=DLN
        IF (III.GT.IIL) THEN
          SLON=SLON+360.
          SLONP=SLONP+360.
        ENDIF
        II=MOD(III-1,IIL)+1
        IIP=II+1
        IF (MASK(II,JJ)) THEN
          SLON=SLON+ALON(II)
          SLONP=SLONP+ALON(IIP)
          SLONMX=MAX(SLON,SLONP)
          SLONMN=MIN(SLON,SLONP)
          DELON=MAX(MIN(AMXLNO,SLONMX)-
     1            MAX(AMNLNO,SLONMN),0.0)
C          WRITE (NOUT,92) MAX(SLATMN,AMNLTO),MIN(SLATMX,AMXLTO),
C     1                 MAX(SLONMN,AMNLNO),MIN(SLONMX,AMXLNO),DELON
C  92      FORMAT (' LAT ',2F9.3,' LON ',2F9.3,' DLON ',F9.3)
C          WRITE (NOUT,93) SLONMN,SLONMX,AMNLNO,AMXLNO
C  93      FORMAT (' IN ',2F9.3,' OUT ',2F9.3)
          WT=WT+WLAT*DELON
          AVG=AVG+A(II,JJ)*WLAT*DELON
        ENDIF
  40    CONTINUE
      ENDIF
  50  CONTINUE
      IF (WT.GT.0.0) THEN
        AO(I,J)=AVG/WT
      ELSE
        AO(I,J)=amm
        IF (MASKO(I,J)) IER=-1
        MASKO(I,J)=.FALSE.
      ENDIF
 100  CONTINUE
 200  CONTINUE
C                     Finish filling the output array from wrap-around.
      IF (IILO.LT.ILO) THEN
        DO 300 J=1,JLO
        DO 300 I=IILO+1,ILO
        AO(I,J)=AO(I-IILO,J)
        MASKO(I,J)=MASKO(I-IILO,J)
 300    CONTINUE
      ENDIF
C                     Check if output MASKO is all false.
      DO 400 J=1,JLO
      DO 400 I=1,ILO
      IF (MASKO(I,J)) GO TO 500
 400  CONTINUE
      IER=-2
 500  CONTINUE
      RETURN
      END
