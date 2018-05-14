cmsw
cmsw screen_diags.f diagnostics for ENTS
cmsw prints to the screen. Based very closely on
cmsw c-goldstein's diaga.f
cmsw
      subroutine screen_diags

#include "genie_ents.cmn"
c      include '../genie-cgoldstein/var.cmn'
#include "var_ents.cmn"

      real amin,amax
      real tot_h2o,tot_c,tot_v,tot_s,tot_a

      integer iamin,iamax,jamin,jamax

      print*

      call aminmaxl(imax,jmax,ents_kmax,ents_k1(:,:),tqld(1,:,:),
     1                 amin,amax,iamin,iamax,jamin,jamax)
      print*,'min land T ',amin,' at ',iamin,jamin
      print*,'max land T ',amax,' at ',iamax,jamax

      call aminmaxl(imax,jmax,ents_kmax,ents_k1(:,:),tqld(2,:,:),
     1                 amin,amax,iamin,iamax,jamin,jamax)
      print*,'min land q ',amin,' at ',iamin,jamin
      print*,'max land q ',amax,' at ',iamax,jamax

      call slnd_h2o_invent(tot_h2o)
      print*,'Total water on land is ',tot_h2o,'(*10^12 m^3)'

      call slnd_c_invent(tot_c,tot_v,tot_s,tot_a)
      print*,'Total carbon is ',tot_c,'(GtC)'
      print*,tot_v,'(GtC) veg'
      print*,tot_s,'(GtC) soil'
      print*,tot_a,'(GtC) atm'
      
      end

      subroutine aminmaxl(imax,jmax,ents_kmax,k1,a,amin,amax,
     1                    iamin,iamax,jamin,jamax)

      implicit none

      integer i,j,imax,jmax,ents_kmax,iamin,iamax,jamin,jamax
      integer k1(imax,jmax)
      real amin,amax,a(imax,jmax)

      amin = a(1,1)
      amax = a(1,1)
      iamin = 1
      iamax = 1
      jamin = 1
      jamax = 1

      do j=1,jmax
         do i=1,imax
            if(k1(i,j).gt.ents_kmax)then
               if(a(i,j).lt.amin)then
                  amin = a(i,j) 
                  iamin = i
                  jamin = j
               endif
               if(a(i,j).gt.amax)then
                  amax = a(i,j) 
                  iamax = i
                  jamax = j
               endif
            endif
         enddo
      enddo

      end

      subroutine slnd_h2o_invent(sumh2o)

#include "genie_ents.cmn"
c      include '../genie-cgoldstein/var.cmn'
#include "var_ents.cmn"

      real sumh2o
      integer i,j

      sumh2o=0.

      do j=1,jmax
         do i=1,imax
            if(ents_k1(i,j).gt.ents_kmax)then
               sumh2o=sumh2o+tqld(2,i,j)
            endif
         enddo
      enddo
          
      sumh2o=sumh2o*asurfrea*1.e-12  

      end

      subroutine slnd_c_invent(sumc,sumv,sums,suma)

#include "genie_ents.cmn"
c      include '../genie-cgoldstein/var.cmn'
#include "var_ents.cmn"

      real sumc,sumv,sums,suma
      integer i,j

      sumv=0.
      sums=0.
      suma=0.

      do j=1,jmax
         do i=1,imax
            if(ents_k1(i,j).gt.ents_kmax)then
               suma=pco2ld*k_a*mu*mtp*rasurf
               sumv=sumv+Cveg(i,j)
               sums=sums+Csoil(i,j)
            endif
         enddo
      enddo

      sumv=sumv*asurfrea*1.e-12
      sums=sums*asurfrea*1.e-12 
      suma=suma*asurfrea*1.e-12
      sumc=sumv+sums+suma

      end

