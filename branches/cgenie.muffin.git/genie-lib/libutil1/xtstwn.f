      subroutine xtstwn(lwin1)
C
      COMMON/WINDOW/LWIN
      LOGICAL LWIN
c
c     This subroutine tells qplot6 if it is using the windowing system
c     .FALSE. if no windowing,  .TRUE. if windowing
c
      logical lwin1
c
      lwin1=lwin
C
      return
      end
