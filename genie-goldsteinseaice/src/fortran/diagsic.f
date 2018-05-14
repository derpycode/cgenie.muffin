c
c diagsic.f modified from AY's sea ice diag code nre 27/2/4
c
c AY (23/09/04) : upgraded to output sea-ice albedo

      subroutine diagsic(istep)

#include "seaice.cmn"

      real hamax(2),hamin(2),ticemax,ticemin,haaav(2),ticeaav,
     :     albicemax,albicemin,albiceaav,area

      integer i,j,l,icepts,istep

      do l=1,2
         hamax(l) = varice(l,1,1)
         hamin(l) = hamax(l)
         haaav(l) = 0.0
      enddo
      ticemax = tice(1,1)
      ticemin = ticemax
      ticeaav = 0.0
      albicemax = albice(1,1)
      albicemin = albicemax
      albiceaav = 0.0
      icepts = 0
      area = 0.0
      
      do j=1,jmax
         do i=1,imax
            do l=1,2
               hamax(l) = max(hamax(l),varice(l,i,j))
               hamin(l) = min(hamin(l),varice(l,i,j))
            enddo
            ticemax = max(ticemax,tice(i,j))
            ticemin = min(ticemin,tice(i,j))
            albicemax = max(albicemax,albice(i,j))
            albicemin = min(albicemin,albice(i,j))
            if(varice(2,i,j).gt.0.0)then
               haaav(1) = haaav(1) + varice(1,i,j)*ds(j)
               haaav(2) = haaav(2) + varice(2,i,j)*ds(j)
               ticeaav = ticeaav + tice(i,j)
               albiceaav = albiceaav + albice(i,j)
               icepts = icepts + 1
               area = area + ds(j)
            endif
         enddo
      enddo
      if(icepts.gt.0)then
crma         haaav(1) = haaav(1)/icepts
crma         haaav(2) = haaav(2)/icepts
crma         ticeaav  = ticeaav /icepts
crma         albiceaav = albiceaav / icepts
         haaav(1) = haaav(1)/area
         haaav(2) = haaav(2)/area
         ticeaav  = ticeaav /area
         albiceaav = albiceaav / area

      endif
      print*,'Sea-ice properties at time ',istep
      print*,'Ice height : max = ',hamax(1),'; min = ',hamin(1)
     +     ,'; avg = ',haaav(1)
      print*,'Ice cover  : max = ',hamax(2),'; min = ',hamin(2)
     +     ,'; avg = ',haaav(2)
      print*,'Ice temp.  : max = ',ticemax,'; min = ',ticemin
     +     ,'; avg = ',ticeaav
      print*,'Ice albd.  : max = ',albicemax,'; min = ',albicemin
     +     ,'; avg = ',albiceaav

      end
