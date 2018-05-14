* ======================================================================
* end_embm.F
* ======================================================================
*
* AY (04/12/03) : restarting conversion of c-GOLDSTEIN into
*                 GOLDSTEIN + EMBM + sea-ice for genie.f
* 
*                 this code takes pieces from mains.F

      subroutine end_embm

#include "embm.cmn"

c ======================================================================
c Declarations
c ======================================================================

c AY (03/12/03)
c Local variables

c ======================================================================
c End EMBM model
c ======================================================================

      print*,'======================================================='
      print*,' >>> Initialising EMBM module shutdown ...'

      if (debug_end) call diagend_embm

c AY (04/12/03)
c #ifdef dland
c     call diagend_land(lout)
c #endif

c AY (09/01/04) : don't think I need these anymore
c 100 format(e14.7)
c 110 format(11e14.6)

      print*,' <<< Shutdown complete'
      print*,'======================================================='

* ======================================================================
* end of end_embm.F
* ======================================================================
      
      return
      end
