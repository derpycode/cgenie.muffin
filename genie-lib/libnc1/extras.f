c     Removed fswinf as it was never called....
c
      subroutine check_err(iret)
      implicit none
      include 'netcdf.inc'
      integer iret
c      
      if (iret.ne.0) then
         print*,' error in netcdf ',iret
         PRINT *, NF_STRERROR(iret)
         stop 1
      end if
c
      return
      end
c
      integer function loc_dim(text,dimname,nall)
      implicit none 
      integer nall
      character dimname(nall)*200,text*(*)
      integer ilen,lnsig,ifail,i,ilen1
c
      ilen=lnsig(text)
c
      ifail=-1
      do i=1,nall
         ilen1=lnsig(dimname(i))
         if (text(1:ilen).eq.dimname(i)(1:ilen1)) then
            ifail=i
            go to 100
         end if
      end do
c
 100  continue
      if (ifail.le.0) then
         print*,' Failure to locate dimension name '
         print*,text(1:ilen)
         do i=1,nall
            ilen1=lnsig(dimname(i))
            if (ilen1.gt.0) print*,nall,i,dimname(i)(1:ilen1)
         end do
         stop
      end if 
      loc_dim=ifail
c
      return
      end






