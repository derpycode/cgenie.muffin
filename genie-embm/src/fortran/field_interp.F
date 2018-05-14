cmsw Subroutine to interpolate between monthly mean
cmsw fields. The interpolation method is calculated as
cmsw in Killworth (1995)
cmsw Ref: P. D. Killworth (1995). Time interpolation of
cmsw      forcing fields in ocean models. J. Phys. Ocn.
cmsw      26, 136-143.
cmsw This method preserves the monthly mean of the fields
cmsw whereas linear interpolation does not.
cmsw MSW 8/2/5
cmsw
      subroutine field_interp(uatml1,usurfl1,tncep1,pncep1,rhncep1,
     1                        atm_alb1)

c      include '../genie-cgoldstein/var.cmn'
c      include '../genie-simpleland/var_ents.cmn'
#include "embm.cmn"

      real invmat(nmth,nmth) 

      real uatml1(2,imax,jmax,nmth+1)
      real usurfl1(imax,jmax,nmth+1)
      real tncep1(imax,jmax,nmth+1)
      real pncep1(imax,jmax,nmth+1)
      real rhncep1(imax,jmax,nmth+1)
      real atm_alb1(imax,jmax,nmth+1)

      real puatml(2,imax,jmax,nmth) 
      real pusurfl(imax,jmax,nmth)
      real ptncep(imax,jmax,nmth)
      real ppncep(imax,jmax,nmth)
      real prhncep(imax,jmax,nmth)
      real patm_alb(imax,jmax,nmth)

      real midpoint(0:nmth+1)

      real xint,x1int,x2int,y1int(8),y2int(8),gradint(8)

      integer i,j,m,istep,l

cmsw Read in inverse of linear interpolation matrix. Calculation
cmsw of this matrix and its inverse is assuming equal month
cmsw length

      open(1,file=indir_name(1:lenin)//'inv_linterp_matrix.dat')
      do j=1,nmth
         do i=1,nmth
            read(1,*)invmat(i,j)
         enddo
      enddo
      close(1)

cmsw Calculate pseudo data

      do j=1,jmax
         do i=1,imax
            puatml(1,i,j,:)=matmul(invmat,uatml1(1,i,j,1:12))
            puatml(2,i,j,:)=matmul(invmat,uatml1(2,i,j,1:12))
            pusurfl(i,j,:)=matmul(invmat,usurfl1(i,j,1:12))
            ptncep(i,j,:)=matmul(invmat,tncep1(i,j,1:12))
            ppncep(i,j,:)=matmul(invmat,pncep1(i,j,1:12))
            prhncep(i,j,:)=matmul(invmat,rhncep1(i,j,1:12))
            patm_alb(i,j,:)=matmul(invmat,atm_alb1(i,j,1:12))
         enddo
      enddo

cmsw Linearly interpolate based on the pseudo data
cmsw First find exact istep for midpoint of each month
      do m=0,nmth+1
         midpoint(m)=0.5*((2.*m)-1)*real(nyear)/real(nmth)
      enddo

      do j=1,jmax
         do i=1,imax
            m=0
            do istep=1,nyear
               if(real(istep).ge.midpoint(m))then
                  m=m+1
               endif
cmsw x terms (i.e. time)
               xint=istep
               x1int=midpoint(m-1)
               x2int=midpoint(m)
cmsw y terms (i.e. field values)
               if(m.eq.1)then
                  y1int(1)=puatml(1,i,j,nmth)
                  y1int(2)=puatml(2,i,j,nmth)
                  y1int(3)=pusurfl(i,j,nmth)
                  y1int(4)=ptncep(i,j,nmth)
                  y1int(5)=ppncep(i,j,nmth)
                  y1int(6)=prhncep(i,j,nmth)
                  y1int(7)=patm_alb(i,j,nmth)

                  y2int(1)=puatml(1,i,j,m)
                  y2int(2)=puatml(2,i,j,m)
                  y2int(3)=pusurfl(i,j,m)
                  y2int(4)=ptncep(i,j,m)
                  y2int(5)=ppncep(i,j,m)
                  y2int(6)=prhncep(i,j,m)
                  y2int(7)=patm_alb(i,j,m)
               else if(m.gt.nmth)then
                  y1int(1)=puatml(1,i,j,m-1)
                  y1int(2)=puatml(2,i,j,m-1)
                  y1int(3)=pusurfl(i,j,m-1)
                  y1int(4)=ptncep(i,j,m-1)
                  y1int(5)=ppncep(i,j,m-1)
                  y1int(6)=prhncep(i,j,m-1)
                  y1int(7)=patm_alb(i,j,m-1)

                  y2int(1)=puatml(1,i,j,1)
                  y2int(2)=puatml(2,i,j,1)
                  y2int(3)=pusurfl(i,j,1)
                  y2int(4)=ptncep(i,j,1)
                  y2int(5)=ppncep(i,j,1)
                  y2int(6)=prhncep(i,j,1)
                  y2int(7)=patm_alb(i,j,1)
               else
                  y1int(1)=puatml(1,i,j,m-1)
                  y1int(2)=puatml(2,i,j,m-1)
                  y1int(3)=pusurfl(i,j,m-1)
                  y1int(4)=ptncep(i,j,m-1)
                  y1int(5)=ppncep(i,j,m-1)
                  y1int(6)=prhncep(i,j,m-1)
                  y1int(7)=patm_alb(i,j,m-1)

                  y2int(1)=puatml(1,i,j,m)
                  y2int(2)=puatml(2,i,j,m)
                  y2int(3)=pusurfl(i,j,m)
                  y2int(4)=ptncep(i,j,m)
                  y2int(5)=ppncep(i,j,m)
                  y2int(6)=prhncep(i,j,m)
                  y2int(7)=patm_alb(i,j,m)
               endif
               do l=1,8
                  gradint(l)=(y2int(l)-y1int(l))/(x2int-x1int)
               enddo
               uatml(1,i,j,istep)=(gradint(1)*(xint-x1int))
     1                             +y1int(1)
               uatml(2,i,j,istep)=(gradint(2)*(xint-x1int))
     1                             +y1int(2)
               usurfl(i,j,istep)=(gradint(3)*(xint-x1int))
     1                             +y1int(3)
               tncep(i,j,istep)=(gradint(4)*(xint-x1int))
     1                             +y1int(4)
               pncep(i,j,istep)=(gradint(5)*(xint-x1int))
     1                             +y1int(5)
               rhncep(i,j,istep)=(gradint(6)*(xint-x1int))
     1                             +y1int(6)
               atm_alb(i,j,istep)=(gradint(7)*(xint-x1int))
     1                             +y1int(7)

cmsw If just want annual average forcing then make all elements in array the same in
cmsw the time direction
               if(ents_seasonswitch.eq.0)then
                  uatml(1,i,j,istep)=uatml1(1,i,j,nmth+1)
                  uatml(2,i,j,istep)=uatml1(2,i,j,nmth+1)
                  usurfl(i,j,istep)=usurfl1(i,j,nmth+1)
                  tncep(i,j,istep)=tncep1(i,j,nmth+1)
                  pncep(i,j,istep)=pncep1(i,j,nmth+1)
                  rhncep(i,j,istep)=rhncep1(i,j,nmth+1)
                  atm_alb(i,j,istep)=atm_alb1(i,j,nmth+1)
               endif

            enddo
         enddo
      enddo

      end
