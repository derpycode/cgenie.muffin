c subroutine inm.f reads in data for goldstein last change  1/5/95
c expanded to read in atmos and sea ice data (Bob 10/5/02)
c
c AY (04/12/03) : altered for genie-goldstein
c		  references to GOLDSTEIN removed
c                 references to sea-ice retained for now
c

      subroutine inm_embm(unit)

#include "embm.cmn"

      integer i, j, l, unit
      real    tmp_val(2)
      real    area

c extra read statement for embm atmos
      read (unit,*)(((tq(l,i,j),l=1,2),i=1,imax),j=1,jmax)

c     AY (08/03/04) : write out averages for restart checks
      if (debug_init) write(*,220) 'Avg T','Avg Q'
c     
c     Clear temporary variables
      do l=1,2
         tmp_val(l) = 0
      enddo
c
c     Sum layer state variables and flow field
      area = 0.
      do j=1,jmax
         do i=1,imax
            area = area + ds(j)
            do l=1,2
               tmp_val(l) = tmp_val(l) + tq(l,i,j)*ds(j)
            enddo
         enddo
      enddo
c
c     Print average values out
crma      write(*,210) tmp_val(1)/(imax*jmax),
crma     +     (tmp_val(2)/(imax*jmax))*1000.
      if (debug_init) write(*,210) tmp_val(1)/area,
     +     (tmp_val(2)/area)*1000.

 210  format(2f13.9)
 220  format(2a13)

      end
