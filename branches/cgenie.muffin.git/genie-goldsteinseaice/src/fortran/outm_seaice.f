c subroutine outm.f writes out data for goldstein last change  6/6/95
c expanded to write out atmos and sea ice data (Bob 10/5/02)
c
c AY (20/01/04) : contracted to excise GOLDSTEIN and EMBM data
c
c AY (23/09/04) : upgraded to handle sea-ice albedo

      subroutine outm_seaice(unit)

#include "seaice.cmn"

      integer i, j, l, unit
      integer icell
      real    tmp_val(4)

c Sea ice

c      do 220 j=1,jmax
c         do 220 i=1,imax
c            do 220 l=1,2
c               write(unit,* )varice(l,i,j)
c  220 continue
        write(unit,fmt='(e24.15)')varice
c Sea ice for exact continuation need

c      do j=1,jmax
c         do i=1,imax
c            write(unit,* )tice(i,j)
c         enddo
c      enddo
        write(unit,fmt='(e24.15)')tice

c AY (23/09/04)
c For exact continuation also need albedo (for surf_ocn_sic)

c      do j=1,jmax
c         do i=1,imax
c            write(unit,* )albice(i,j)
c         enddo
c      enddo
        write(unit,fmt='(e24.15)')albice

c     AY (08/03/04) : write out averages for restart checks
      if (debug_loop) 
     & write(*,320) 'Avg height','Avg area', 'Avg T', 'Avg albedo'
c     
c     Clear temporary variables
      do l=1,4
         tmp_val(l) = 0
      enddo
      icell = 0
c     
c     Sum layer state variables and flow field
      do j=1,jmax
         do i=1,imax
c AY (23/09/04) : if statement modified to only consider cells with ice
c           if(k1(i,j).le.kmax) then
            if(k1(i,j).le.kmax.and.varice(2,i,j).gt.0.) then
               icell = icell + 1
               do l=1,2
                  tmp_val(l) = tmp_val(l) + varice(l,i,j)
               enddo
               tmp_val(3) = tmp_val(3) + tice(i,j)
               tmp_val(4) = tmp_val(4) + albice(i,j)
            endif
         enddo
      enddo
c     
c     Print average values out
      if (icell.gt.0) then
         if (debug_loop) write(*,310) tmp_val(1)/icell,tmp_val(2)/icell,
     :        tmp_val(3)/icell,tmp_val(4)/icell
      else
         if (debug_loop) write(*,310) 0.,0.,0.,0.
      endif
      
c 10   format(10f10.4)
 310  format(4f13.9)
 320  format(4a13)
      
      end
