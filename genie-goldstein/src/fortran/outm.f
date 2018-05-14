c subroutine outm.f writes out data for goldstein last change  6/6/95
c expanded to write out atmos and sea ice data (Bob 10/5/02)
c
c AY (02/12/03) : contracted to excise EMBM and sea-ice data
c
      subroutine outm(unit)

#include "ocean.cmn"

      integer i, j, k, l, unit
      integer icell
      real    tmp_val(4)

C
C JULES - fix the crap write statements I told them about in 2005 
C
      real out_file(4,kmax,imax,jmax) 
C      real u_file(2,kmax,imax,jmax) 
C
      do 20 j=1,jmax
         do 20 i=1,imax
            do 20 k=1,kmax
               do 30 l=1,2  
                  if(k.ge.k1(i,j))then
c                     write(unit,* )ts(l,i,j,k)
                     out_file(l,k,i,j)=ts(l,i,j,k)
                  else
c                     write(unit,* )0.0      
                     out_file(l,k,i,j)=0.0
                  endif
   30          continue
               do l=1,2
c                 write(unit,* )u(l,i,j,k)
                  out_file(l+2,k,i,j)=u(l,i,j,k)
               enddo
   20 continue
        write(unit,fmt='(e24.15)') out_file
        
c        write(102,fmt='(e13.7)') u_file

C jules end
C
c     write(unit,*)t

c AY (08/03/04) : write out layer averages for restart checks
      if (debug_loop) 
     & write(*,120) 'Layer','Avg T','Avg S','Avg U','Avg V',
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
                  tmp_val(l+2) = tmp_val(l+2) + u(l,i,j,k)
               enddo
            enddo
         enddo
c
c     AR (09/01/26): prevent divide-by-zero
         if (icell == 0) icell = 1
c
c        Print average values out
         if (debug_loop) 
     & write(*,110) k,tmp_val(1)/icell,(tmp_val(2)/icell)+saln0,
     +        tmp_val(3)/(imax*jmax),tmp_val(4)/(imax*jmax),icell
      enddo

 110  format(i5,2f13.9,2e17.9,i4)
 120  format(a5,2a13,2a17,a6)

      end
