c subroutine inm.f reads in data for goldstein last change  1/5/95
c expanded to read in atmos and sea ice data (Bob 10/5/02)
c
c AY (01/12/03) : altered for genie-goldstein
c		  references to EMBM and sea-ice removed
c

      subroutine inm(unit)

#include "ocean.cmn"

      integer i, j, k, l, unit
      integer icell
      real    tmp_val(4)

      read (unit,*)((((ts(l,i,j,k),l=1,2),(u1(l,i,j,k),l=1,2),
     1             k=1,kmax),i=1,imax),j=1,jmax)

c     read (unit,*,end=10) t0
c     t = t0
c     print*,'t = ',t
c 10  continue

c     AY (08/03/04) : write out layer averages for restart checks
      if (debug_init) 
     +     write(*,120) 'Layer','Avg T','Avg S','Avg U','Avg V',
     +     'Cells'
      do k=1,kmax
c     
c        Clear temporary variables
         do l=1,4
            tmp_val(l) = 0
         enddo
         icell = 0
c
c        Sum layer state variables and flow field
         do j=1,jmax
            do i=1,imax
               if(k.ge.k1(i,j)) icell = icell + 1
               do l=1,2
                  if(k.ge.k1(i,j))then
                     tmp_val(l) = tmp_val(l) + ts(l,i,j,k)
                  endif
                  tmp_val(l+2) = tmp_val(l+2) + u1(l,i,j,k)
c
c                 Set up u array from read-in u1 array
                  u(l,i,j,k) = u1(l,i,j,k)
               enddo
            enddo
         enddo
c
c        Print average values out
         if (debug_init) 
     +        write(*,110) k,tmp_val(1)/icell,(tmp_val(2)/icell)+saln0,
     +        tmp_val(3)/(imax*jmax),tmp_val(4)/(imax*jmax),icell
      enddo

 110  format(i5,2f13.9,2e17.9,i4)
 120  format(a5,2a13,2a17,a6)

      end
