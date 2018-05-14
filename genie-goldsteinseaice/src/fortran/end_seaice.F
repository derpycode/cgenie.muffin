* ======================================================================
* end_seaice.F
* ======================================================================
*
* AY (20/01/04) : restarting conversion of c-GOLDSTEIN into
*                 GOLDSTEIN + EMBM + sea-ice for genie.f
* 
*                 this code takes pieces from mains.F

      subroutine end_seaice

#include "seaice.cmn"

c ======================================================================
c Declarations
c ======================================================================

c AY (03/12/03)
c Local variables

c ======================================================================
c End sea-ice model
c ======================================================================

      print*,'======================================================='
      print*,' >>> Initialising sea-ice module shutdown ...'

c AY (20/01/04) : not much gets done in here, but I'm leaving it in
c	          for now

      if (debug_end) call diagend_seaice

      print*,' <<< Shutdown complete'
      print*,'======================================================='

* ======================================================================
* end of end_seaice.F
* ======================================================================

      return
      end
