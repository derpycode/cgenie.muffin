c
      subroutine gwtbox(alatbo,jg)
      implicit none
      integer j,jg
      real    pi,sit,weight
      real    alatbo(*)
      real*8 zslat
c
      pi=2.*asin(1.)
      alatbo(1)=90.0
      zslat=1.0
      do j=1,jg*2
         if (j.le.jg) then
            call gwtlt1(sit,weight,j,jg)
         else
            call gwtlt1(sit,weight,jg*2+1-j,jg)
            sit=-sit
         end if
         zslat=zslat-weight
         if (zslat.lt.-1.0) zslat=-1.0
         alatbo(j+1)=real(asin(zslat)*180.0/pi)
      end do
      end
