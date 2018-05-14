      INTEGER FUNCTION LNSIG1(TEXT)
      IMPLICIT NONE
      INTEGER ILEN,II,IEND,IBEG,III
      CHARACTER TEXT*(*)
C
C     This function returns the length of the non-blank character
C     string TEXT. It also removes any leading blanks
C
C     First find out the length of the non-blank string
C
      ILEN=LEN(TEXT)
      DO II=ILEN,1,-1
         IF (TEXT(II:II).NE.' ') GO TO 10
      END DO
 10   CONTINUE
      if (ii.gt.0) then
         if (ichar(text(ii:ii)).eq.0) ii=ii-1
      end if
c
      IEND=II
      IF (IEND.NE.0) THEN
         DO II=1,IEND
            IF (TEXT(II:II).NE.' ') GO TO 20
         END DO
 20      CONTINUE
         IBEG=II
         TEXT(1:IEND-IBEG+1)=TEXT(IBEG:IEND)
         IF (IEND-IBEG+1.NE.ILEN) THEN
            DO III=IEND-IBEG+2,ILEN
               TEXT(III:III)=' '
            END DO
         END IF
         LNSIG1=IEND-IBEG+1
      ELSE
         LNSIG1=1
      END IF
C
      RETURN
      END
