c subroutine inm.f reads in data for goldstein last change  1/5/95
c expanded to read in atmos and sea ice data (Bob 10/5/02)
c
c AY (20/01/04) : altered for genie-seaice
c		  references to GOLDSTEIN and EMBM removed
c
c AY (23/09/04) : upgraded to handle sea-ice albedo

      subroutine inm_seaice(unit)

#include "seaice.cmn"

      integer i, j, l, unit
      integer icell
      real    tmp_val(4)

c read statement for sea ice
      read (unit,*)(((varice(l,i,j),l=1,2),i=1,imax),j=1,jmax)
c
c extra read statement for sea-ice temperature for exact continuation
      read (unit,*)((tice(i,j),i=1,imax),j=1,jmax)
c
c AY (23/09/04) : following added for surf_ocn_sic.F routine
c extra read statement for sea-ice albedo for exact continuation
      read (unit,*)((albice(i,j),i=1,imax),j=1,jmax)

c 10  continue

c     AY (08/03/04) : write out averages for restart checks
      if (debug_init) 
     :    write(*,320) 'Avg height','Avg area', 'Avg T', 'Avg albedo'
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
         if (debug_init) write(*,310) tmp_val(1)/icell,tmp_val(2)/icell,
     :        tmp_val(3)/icell,tmp_val(4)/icell
      else
         if (debug_init) write(*,310) 0.0,0.0,0.0,0.0
      endif
      
 310  format(4f13.9)
 320  format(4a13)
    
      end
