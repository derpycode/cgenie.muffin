c
      SUBROUTINE CBLANK(TEXT)
      IMPLICIT NONE
      CHARACTER*(*) TEXT
      INTEGER ILEN,I
      ILEN=LEN(TEXT)
      DO I=1,ILEN
         TEXT(I:I)=' '
      ENDDO
      END
