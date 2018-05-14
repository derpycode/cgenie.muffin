* ======================================================================
* end_goldstein.F
* ======================================================================
*
* AY (01/12/03) : restarting conversion of c-GOLDSTEIN into
*                 GOLDSTEIN + EMBM + sea-ice for genie.f
* 
*                 this code takes pieces from mains.F
*
* AY (28/09/04) : finally renamed this routine to avoid problems
*                 should we ever have another ocean module

      subroutine end_goldstein

      use genie_util, ONLY: check_unit, check_iostat

#include "ocean.cmn"

c ======================================================================
c Declarations
c ======================================================================

c AY (03/12/03)
c Local variables
      integer i, j, k, l, ios

      real avn, avs, sums(8*maxl)

c Stream function variables
      real opsi(0:maxj,0:maxk)
      real opsia(0:maxj,0:maxk), omina, omaxa
      real opsip(0:maxj,0:maxk), ominp, omaxp
      real zpsi(0:maxi,0:maxk), zu(maxi,maxk)

      real hft(3), hfp(3), hfa(3), phfmax, tv2, tv3

      real rhoout(kmax,imax,jmax)
      integer iposa(2)

c ======================================================================
c End GOLDSTEIN model
c ======================================================================

      print*,'======================================================='
      print*,' >>> Initialising GOLDSTEIN module shutdown ...'

      if (ctrl_diagend) then
         if (debug_end) call diagend
      endif

c write out convective frequency array. Divide by 2*nsteps if call co twice

c AY (03/12/03)
c     open(3,file='../results/'//lout//'.cost')
      call check_unit(3,__LINE__,__FILE__)
      open(3,file=outdir_name(1:lenout)//lout//'.cost',iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
crma      if(nsteps.gt.0)write(3,'(e15.8)')((cost(i,j)/nsteps
      if(nsteps.gt.0) then
         if (debug_end) 
     & write(3,'(e15.8)',iostat=ios)((cost(i,j),i=1,imax),j=1,jmax)
         call check_iostat(ios,__LINE__,__FILE__)
      end if
      close(3,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)

c write out barotropic streamfunction

c AY (03/12/03)
c     open(3,file='../results/'//lout//'.psi')
      open(3,file=outdir_name(1:lenout)//lout//'.psi',iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      do 60 j=0,jmax
         do 60 i=0,imax
            if (debug_end) write(3,*,iostat=ios)psi(i,j)
            call check_iostat(ios,__LINE__,__FILE__)
  60  continue
      close(3,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)

c AY (08/12/03)
c The following routines are normally only called in goldstein.F, but
c their outputs are written to file here.  Rather than have them passed
c from goldstein.F to end_goldstein.F, am re-calculating them here.
      if (debug_end) call diag2(sums,avn,avs)

      if (debug_end) 
     & call diagopsi(ominp,omaxp,omina,omaxa,opsi,opsia,opsip,iposa)

c AY (03/12/03)
c     open(10,file='../results/'//lout//'.opsi')
      call check_unit(10,__LINE__,__FILE__)
      open(10,file=outdir_name(1:lenout)//lout//'.opsi',iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      if (debug_end) write(10,100,iostat=ios)((opsi(j,k),j=0,jmax),k=0,kmax)
      call check_iostat(ios,__LINE__,__FILE__)
      close(10,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)

c AY (03/12/03)
c     open(10,file='../results/'//lout//'.opsip')
      open(10,file=outdir_name(1:lenout)//lout//'.opsip',iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      if (debug_end) write(10,100,iostat=ios)((opsip(j,k),j=0,jmax),k=0,kmax)
      call check_iostat(ios,__LINE__,__FILE__)
      close(10,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)

c AY (03/12/03)
c     open(10,file='../results/'//lout//'.opsia')
      open(10,file=outdir_name(1:lenout)//lout//'.opsia',iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      if (debug_end) write(10,100,iostat=ios)((opsia(j,k),j=0,jmax),k=0,kmax)
      call check_iostat(ios,__LINE__,__FILE__)
      close(10,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)

c zonal overturning streamfunction
c
      do i=0,imax
         do k=0,kmax
            zpsi(i,k) = 0
         enddo
      enddo

      do i=1,imax-1
         do k=1,kmax-1
            zu(i,k) = 0
            do j=1,jmax
crma               zu(i,k) = zu(i,k) + u(1,i,j,k)/c(j)*ds
               zu(i,k) = zu(i,k) + u(1,i,j,k)/c(j)*ds(j)
            enddo
            zpsi(i,k) = zpsi(i,k-1) - dz(k)*zu(i,k)
         enddo
      enddo

c AY (03/12/03)
c     open(10,file='../results/'//lout//'.zpsi')
      open(10,file=outdir_name(1:lenout)//lout//'.zpsi',iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      if (debug_end) write(10,100,iostat=ios)((zpsi(i,k),i=0,imax),k=0,kmax)
      call check_iostat(ios,__LINE__,__FILE__)
      close(10,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)

  100 format(e14.7)
  110 format(11e14.6)

c write poleward heat flux in Atlantic and Pacific and total

      pi=4*atan(1.0)
c AY (03/12/03)
c     open(15,file='../results/'//lout//'.fofy')
      call check_unit(15,__LINE__,__FILE__)
      open(15,file=outdir_name(1:lenout)//lout//'.fofy',iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      if (debug_end) write(15,'(10(a11,3x))',iostat=ios)
     $              ' latitude  '
     $             ,' tot_tot   ',' Pac_tot   '
     $             ,' Atl_tot   ',' tot_adv   '
     $             ,' Pac_adv   ',' Atl_adv   '
     $             ,' tot_dif   ',' Pac_dif   '
     $             ,' Atl_dif   '
      call check_iostat(ios,__LINE__,__FILE__)
      phfmax = 0
      do j=1,jmax-1
         do l=1,3
            hft(l) = 0
            hfp(l) = 0
            hfa(l) = 0
         enddo
         do i=1,imax
            if(k1(i,j).le.kmax.and.k1(i,j+1).le.kmax)then
               tv2 = 0
               tv3 = 0
               do k=k1(i,j),kmax
                  tv2 = tv2 + 0.5*cv(j)*u(2,i,j,k)*(ts(1,i,j+1,k) +
     1                       ts(1,i,j,k))*dz(k)*dphi
                  tv3 = tv3 - cv(j)*cv(j)*(ts(1,i,j+1,k) -
crma     1                       ts(1,i,j,k))/ds*diff(1)*dz(k)*dphi
     1                       ts(1,i,j,k))*rds(j)*diff(1)*dz(k)*dphi
               enddo
               hft(1) = hft(1) + tv2 + tv3
               hft(2) = hft(2) + tv2
               hft(3) = hft(3) + tv3
               if(i.ge.ips(j).and.i.le.ipf(j))then
                  hfp(1) = hfp(1) + tv2 + tv3
                  hfp(2) = hfp(2) + tv2
                  hfp(3) = hfp(3) + tv3
               elseif(i.ge.ias(j).and.i.le.iaf(j))then
                  hfa(1) = hfa(1) + tv2 + tv3
                  hfa(2) = hfa(2) + tv2
                  hfa(3) = hfa(3) + tv3
               endif
            endif
         enddo
         if (debug_end) 
     & write(15,110,iostat=ios)180.0/pi*asin(s(j)),(hft(l),hfp(l),
     &        hfa(l),l=1,3)
         call check_iostat(ios,__LINE__,__FILE__)
         if(abs(hft(3)).gt.phfmax)phfmax = abs(hft(3))
      enddo

      if (debug_end) write(6,*)'max poleward heat flux ',phfmax
               
      close(15,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)

c write out potential density

c AY (03/12/03)
c     open(11,file='../results/'//lout//'.rho')

      rhoout=0.

      call check_unit(11,__LINE__,__FILE__)
      open(11,file=outdir_name(1:lenout)//lout//'.rho',iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)

      do j=1,jmax
         do i=1,imax
c           do k=1,kmax-1
            do k=1   ,kmax
               if(k.ge.k1(i,j))then
c                  write(11,*,iostat=ios)rho(i,j,k)
c                  call check_iostat(ios,__LINE__,__FILE__)
                  rhoout(k,i,j)=rho(i,j,k)
               else
c                  write(11,*,iostat=ios)0.0
c                  call check_iostat(ios,__LINE__,__FILE__)
                  rhoout(k,i,j)=0.0
               endif
            enddo
         enddo
      enddo

      if (debug_end) write(11,fmt='(e21.13)')rhoout
      close(11,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)

      tv2 = dsc*usc*rsc*1e-6
      if (debug_end) 
     & write(6,'(a)',iostat=ios)'overturning extrema in Sv'
      call check_iostat(ios,__LINE__,__FILE__)
c AY (08/12/03) : variable rms not calculated in end_goldstein.F, so excised
c                 (still appears as an output in the goout file)
c     write(6,'(a)')'ominp,omaxp,omina,omaxa,avn,rms'
c     write(6,'(6e15.5)')ominp*tv2,omaxp*tv2,omina*tv2,omaxa*tv2,avn,rms
      if (debug_end) 
     & write(6,'(a)',iostat=ios)'ominp,omaxp,omina,omaxa,avn'
      call check_iostat(ios,__LINE__,__FILE__)
      if (debug_end) 
     & write(6,'(5e15.5)',iostat=ios)ominp*tv2,omaxp*tv2,omina*tv2,
     &     omaxa*tv2,avn
      call check_iostat(ios,__LINE__,__FILE__)

      print*,' <<< Shutdown complete'
      print*,'======================================================='

* ======================================================================
* end of end_goldstein.F
* ======================================================================
      
      return
      end
