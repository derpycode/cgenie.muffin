c AY (12/07/05) : removed surplus input arguments to function

      subroutine goldstein_delayfluxes(
     :     lrestart_genie,
     :     evap_ocn,
     :     latent_ocn,
     :     sensible_ocn
     :                                )

#include "ocean.cmn"

      integer ifirst
      data ifirst/1/

      real evap_ocn(maxi,maxj)
      real latent_ocn(maxi,maxj)
      real sensible_ocn(maxi,maxj)
      logical lrestart_genie

         if (ifirst.eq.1) then
           if (.not.lrestart_genie) then
             evap_save2(:,:)=0.0
             late_save2(:,:)=0.0
             sens_save2(:,:)=0.0
             ifirst=0
           endif
         endif

         evap_save1(:,:)=evap_ocn(:,:)
         evap_ocn(:,:)=evap_save2(:,:)
         evap_save2(:,:)=evap_save1(:,:)
         late_save1(:,:)=latent_ocn(:,:)
         latent_ocn(:,:)=late_save2(:,:)
         late_save2(:,:)=late_save1(:,:)
         sens_save1(:,:)=sensible_ocn(:,:)
         sensible_ocn(:,:)=sens_save2(:,:)
         sens_save2(:,:)=sens_save1(:,:)

      return 
      end
