      SUBROUTINE CALNDR(DOY,MDUM,amfrac)
C     
C--------------------------------------------------------------------
C     Given the julian day, routine returns month and day as
C     character*7 variable. Leap years are ignored.
C     
c     DJL
c     This is copied form the original calndr.f in genie-igcm3,
c     which is now calndr_igcm.f.
c     This is because this routine is used by several modules
c     15/4/2004 this is fixedatmos,fixedocean,fixedseaice,
c     slabocean,slabseaice
c
C---------------------------------------------------------------------
C     
      IMPLICIT NONE
C     
C     Input day number
C     
      REAL DOY,DAY,GAP,AMFRAC
      INTEGER MDUM

      INTEGER MM
      INTEGER NDAY(13)

C     DATA NDAY/0,31,59,90,120,151,181,212,243,273,304,334,365/
      DATA NDAY/0,30,60,90,120,150,180,210,240,270,300,330,360/
C     
C     Perform operation.
C     ------------------
C     
      DAY=DOY
      DO MM=12,1,-1
         IF (DAY.GE.(REAL(NDAY(MM+1)+NDAY(MM))/2.0)) THEN
            MDUM=MM
            GAP=30.0            ! const month length
            AMFRAC=(DAY-0.5*REAL(NDAY(MM+1)+NDAY(MM)))/GAP
            GOTO 200
         ENDIF
      END DO
C
C     this means day is between 1-15
C
      MDUM=12
      GAP=30.0
      AMFRAC=(DAY+15.)/GAP

 200  CONTINUE

      RETURN
      END
