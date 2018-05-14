* ======================================================================
* gold_ocnsic_avg.f
* ======================================================================
*
* AY (23/09/04) : calculates and outputs average fields of temperature 
*                 and albedo for simulations using the surf_ocn_sic.F 
*                 routine (see text below for more details)

      subroutine gold_ocnsic_avg(
     :     frac_sic,
     :     temp_ocn, temp_sic,
     :     albd_ocn, albd_sic,
     :     temp_avg, albd_avg)

#include "ocean.cmn"

c ======================================================================
c Inputs/Outputs
c ======================================================================
      
c Inputs
c     frac_sic   sea-ice area fraction
c     temp_ocn   ocean surface temperature
c     temp_sic   sea-ice surface temperature
c     albd_ocn   ocean albedo
c     albd_sic   sea-ice albedo
c Outputs
c     temp_avg   Average weighted temperature
c     albd_avg   Average albedo

c ======================================================================
c Declarations
c ======================================================================

c Input variables
      real
     :     frac_sic(imax,jmax), 
     :     temp_ocn(imax,jmax), temp_sic(imax,jmax),
     :     albd_ocn(imax,jmax), albd_sic(imax,jmax)

c Output variables
      real
     :     temp_avg(imax,jmax), albd_avg(imax,jmax)

c Local variables
      integer i, j
c
      real    tmp1, tmp2, tmp3

c ======================================================================
c Clear output fields
c ======================================================================

c For reference : this means that the output fields are zero over land

      do j=1,jmax
         do i=1,imax
            temp_avg(i,j) = 0.
            albd_avg(i,j) = 0.
         enddo
      enddo

c ======================================================================
c Calculate outputs
c ======================================================================

c The IGCM3 requires ocean cells to present average temperature and
c albedo in order for it to calculate net SW and LW fluxes.  This
c routine takes the temperatures and albedos of ocean and sea-ice in
c each GOLDSTEIN cell and calculates the average of these.  In the
c case of albedo this is very straightforward, but in the case of
c temperature the calculation is more convoluted (and potentially
c inconsistent with the IGCM3 - Dan and I have discussed this, but
c haven't strictly resolved it).

      do j=1,jmax
         do i=1,imax
            if (k1(i,j).le.kmax) then

c-----------------------------------------------------------------------
c              set up average albedo
c-----------------------------------------------------------------------

               albd_avg(i,j) = frac_sic(i,j)*albd_sic(i,j) + 
     :              (1-frac_sic(i,j))*albd_ocn(i,j)

c-----------------------------------------------------------------------
c              set up weighted average temperature
c-----------------------------------------------------------------------

               tmp1 = (temp_ocn(i,j)+zeroc)**4
               tmp2 = (temp_sic(i,j)+zeroc)**4
               tmp3 = ((1 - frac_sic(i,j))*tmp1) + 
     :              (frac_sic(i,j) * tmp2)
               temp_avg(i,j) = 
     :            real( (tmp3**0.25) - zeroc)
            endif
         enddo
      enddo

c ======================================================================
c end of gold_ocnsic_avg.f
c ======================================================================

      end
