c
c diag3.f frequent atmos diagnostics
c
c AY (30/04/04) : upgraded to output hemispheric averages
c
      subroutine diag3(sum1,sum2,isum1,isum2)

#include "embm.cmn"

      real    sum1(4), sum2(4)
      real    area1, area2
      integer isum1(4), isum2(4)

      integer i,j

c     Reset arrays
      do i=1,4
         sum1(i) = 0.
         sum2(i) = 0.
         isum1(i) = 0
         isum2(i) = 0
      enddo

c     Initial conditions for minimum values
      sum1(4) = 999.9
      sum2(4) = 999.9

c     Calculate averages, minima, maxima, etc.
      area1 = 0.
      area2 = 0.
      do j=1,jmax
         do i=1,imax
c
c           Northern/Southern hemisphere averages
            if(j.gt.(jmax/2))then
               sum1(1) = sum1(1) + tq(1,i,j)*ds(j)
               sum2(1) = sum2(1) + tq(2,i,j)*ds(j)
               area1 = area1 + ds(j)
            else
               sum1(2) = sum1(2) + tq(1,i,j)*ds(j)
               sum2(2) = sum2(2) + tq(2,i,j)*ds(j)
               area2 = area2 + ds(j)
            endif
c           Maximum temperature
            if(tq(1,i,j).gt.sum1(3)) then
               sum1(3) = tq(1,i,j)
               isum1(1) = i
               isum1(2) = j
            endif
c           Minimum temperature
            if(tq(1,i,j).lt.sum1(4)) then
               sum1(4) = tq(1,i,j)
               isum1(3) = i
               isum1(4) = j
            endif
c           Maximum specific humidity
            if(tq(2,i,j).gt.sum2(3)) then
               sum2(3) = tq(2,i,j)
               isum2(1) = i
               isum2(2) = j
            endif
c           Minimum specific humidity
            if(tq(2,i,j).lt.sum2(4)) then
               sum2(4) = tq(2,i,j)
               isum2(3) = i
               isum2(4) = j
            endif
c
         enddo
      enddo

      sum1(1) = sum1(1)/area1
      sum2(1) = sum2(1)/area1
      sum1(2) = sum1(2)/area2
      sum2(2) = sum2(2)/area2

c Convert humidity to g / kg
      do i=1,4
         sum2(i) = sum2(i) * 1000.
      enddo

      end
