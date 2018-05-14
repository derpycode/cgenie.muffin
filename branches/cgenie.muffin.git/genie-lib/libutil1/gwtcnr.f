      subroutine gwtcnr(alato,jg)
      implicit none
      real alato(*),sit,weight,pi
      integer jg,j
c
      pi=2.*asin(1.)
      do j=1,jg*2
         if (j.le.jg) then
            call gwtlt1(sit,weight,j,jg)
         else
            call gwtlt1(sit,weight,jg*2+1-j,jg)
            sit=-sit
         end if
         alato(j)=asin(sit)*180.0/pi
      end do
      end
c
      subroutine gwtcnr8(alato,jg)
      implicit none
      real*8 alato(*),sit,weight,pi
      integer jg,j
c
      pi=2.*asin(1.)
      do j=1,jg*2
         if (j.le.jg) then
            call gwtlt18(sit,weight,j,jg)
         else
            call gwtlt18(sit,weight,jg*2+1-j,jg)
            sit=-sit
         end if
         alato(j)=asin(sit)*180.0/pi
      end do
      end
