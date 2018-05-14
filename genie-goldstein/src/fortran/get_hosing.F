* 
* hosing.f sets additional freshwater forcing
* including code originally in surflux.F (RMA, 3/5/06)
*
      subroutine get_hosing(istep) 

      use genie_util, ONLY: check_unit, check_iostat

#include "ocean.cmn"

      integer i,j,istep,ios

c set up freshwater fluxes according to parameters set up in genie_example.job:
c${go_hosing:=0.00}                                        initial/constant value of hosing flux (Sv)
c${go_hosing_trend:=0.00}                                  rate at which hosing flux is increased/decreased (Sv/kyr)
c${go_nyears_hosing:=0}                                    time period of hosing (yr)

crma update extra freshwater forcing
      hosing = hosing + hosing_trend*tsc*dt(kmax)

      if(mod(istep,itstp).lt.1) then
      call check_unit(44,__LINE__,__FILE__)
      open(44,file=outdir_name(1:lenout)//lout//'.'//'hose',
     1        status='old',position='append',iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      write(44,*,iostat=ios) istep,hosing
      call check_iostat(ios,__LINE__,__FILE__)
      close(44,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      endif

      do i=1,imax
         do j=1,jmax

ccc update fw_hosing Sv
c over gridboxes within convective regions (50-70N) in N.Atlantic
c and scaling as other freshwater fluxes:
            if(istep.le.nsteps_hosing) then
            fw_hosing(i,j) = m2mm*hosing*rhosing(i,j)
            else
            fw_hosing(i,j) = 0.0
            endif

ccc FW flux anomaly (from CMIP/PMIP model)
            fw_anom(i,j) = fw_anom(i,j)
     1                   + fw_anom_rate(i,j)*tsc*dt(kmax)

         enddo
      enddo

      end
