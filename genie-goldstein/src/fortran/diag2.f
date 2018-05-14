c
c diag2.f frequent diagnostics for program goldstein variable depth
c mo indexes the oceans 1=Pacific, 2=Atlantic, 3=Indian, 4=Southern
c ndep indexes the depth 0=deep, 1=upper
c ipf=end of Pacific, iaf=end of Atlantic, jsf=north end of Southern
c lmax.gt.2.allowed 22/5/2
c some awkward points included in Atlantic 25/11/2, still not fully
c logical categorisation
c updated for generalised grid (RMA, 10/5/05)
c
      subroutine diag2(sum,avn,avs)

#include "ocean.cmn"

      real sum(8*maxl), avn,  avs, vol(8)
c      real tv

      integer i,j,k,l,mo,ndep

      logical first

      save vol, first

      data first/.true./
      data vol/8*0.0/

      do i=1,8*lmax
         sum(i) = 0
      enddo      
c      tv = 0
      avn = 0
      avs = 0
      do 10 k=1,kmax
         do 20 j=1,jmax
            do 30 i=1,imax
               if(k.ge.k1(i,j))then
                  ndep = (2*k-1)/kmax
                  if(j.le.jsf)then
                     mo = 4
                  elseif(i.ge.ips(j).and.i.le.ipf(j))then
                     mo = 1
                  elseif((i.ge.ias(j).and.i.le.iaf(j))
     &               .or.(iaf(j).le.ias(j).and.(i.le.iaf(j).or.
     &                                          ias(j).le.i)))then
                     mo = 2
                  else
                     mo = 3
                  endif
                  do 60 l=1,lmax
                     sum(mo + 4*ndep + 8*(l-1)) =
     1                  sum(mo + 4*ndep + 8*(l-1)) + 
     1                  ts(l,i,j,k)*dphi*ds(j)*dz(k)
c                    print*,i,j,k,l,mo + 4*ndep + 8*(l-1)
c    2                   ,sum(mo + 4*ndep + 8*(l-1))
   60             continue
                  if(first)vol(mo + 4*ndep) = vol(mo + 4*ndep) 
     1                                      + dphi*ds(j)*dz(k)
c                 print*,i,j,k,mo + 4*ndep,vol(mo + 4*ndep)
                  if(k.lt.kmax)
     1               avn = avn + abs(rho(i,j,k) - rho(i,j,k+1))/dza(k)

                  avs = avs + u(1,i,j,k)**2 + u(2,i,j,k)**2

               endif
   30       continue
   20    continue
   10 continue

c crash barrier added 111099
      if(avs.lt.1e20)then
        continue
      else
        print*,'big avs , stopping'
        stop
      endif

      do i=1,8     
         do l=1,lmax
            if (vol(i) .ne. 0.0) then
               sum(i + 8*(l-1)) = sum(i + 8*(l-1))/vol(i)
            end if
         enddo
      enddo

      avs = sqrt(avs/ntot)

      avn = avn/(intot)

      first = .false.

      end
