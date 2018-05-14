c
c diag4.f frequent sea-ice diagnostics
c
c AY (30/04/04) : upgraded to output hemispheric averages
c
c AY (23/09/04) : upgraded to handle sea-ice albedo

      subroutine diag4(sum1,sum2,sum3,sum4,isum1,isum2,isum3)

#include "seaice.cmn"

      real    sum1(2), sum2(2), sum3(2), sum4(2)
      integer isum1(2), isum2(2), isum3(2)

      integer i,j

c Reset arrays
      do i=1,2
         sum1(i) = 0.
         sum2(i) = 0.
         sum3(i) = 0.
         sum4(i) = 0.
         isum1(i) = 0
         isum2(i) = 0
         isum3(i) = 0
      enddo

c     Calculate averages, minima, maxima, etc.
      do j=1,jmax
         do i=1,imax
c
c           Northern/Southern hemisphere totals
            if(j.gt.(jmax/2))then
               sum1(1) = sum1(1) 
     1                 + (varice(1,i,j)*varice(2,i,j))*asurf(j)
               sum2(1) = sum2(1) + varice(2,i,j)*asurf(j)
            else
               sum1(2) = sum1(2) 
     1                 + (varice(1,i,j)*varice(2,i,j))*asurf(j)
               sum2(2) = sum2(2) + varice(2,i,j)*asurf(j)
            endif
c           Maximum sea-ice height
            if(varice(1,i,j).gt.sum3(1)) then
               sum3(1) = varice(1,i,j)
               isum1(1) = i
               isum1(2) = j
            endif
c           Minimum sea-ice temperature
            if(tice(i,j).lt.sum3(2)) then
               sum3(2) = tice(i,j)
               isum2(1) = i
               isum2(2) = j
            endif
c           Maximum sea-ice albedo
            if(albice(i,j).gt.sum4(1)) then
               sum4(1) = albice(i,j)
               isum3(1) = i
               isum3(2) = j
            endif
c Note : sum4(2) not used at present
c
         enddo
      enddo

c Multiply by the area of a grid cell to get total volume/area
crma      do i=1,2
crma         sum1(i) = sum1(i)*asurf
crma         sum2(i) = sum2(i)*asurf
crma      enddo

      end
