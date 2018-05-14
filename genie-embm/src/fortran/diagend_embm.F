c
c diagend.f end-of-run diagnostics for c-goldstein created 2/9/2 nre
c
c tsdata and tqdata contain observational estimates for ts and tq
c err is the mismatch between model and data weighted by errw 
c diagnostic deep temperatures for exact comparison with Jia (2003) 16/6/3
c
c AY (05/12/03) : altered for genie-goldstein
c		  references to GOLDSTEIN and sea-ice excised
c                 contains many diagnostics - these need to go somewhere!

      subroutine diagend_embm

      use genie_util, ONLY: check_unit, check_iostat

#include "embm.cmn"
c     include 'extra.cmn'

c AY (09/01/04) : excised
c     character lout*7

      real err
      real err3, err4, err_embm
c      real sum, tv2, syr
c AY (05/12/03)
c     real tsdata(maxl,maxi,maxj,maxk), tqdata(2,maxi,maxj)
c     real errwts(maxl,maxk), errwtq(2)
c     real tsav(2), tqav(2), tsvar(2), tqvar(2)
      real tqdata(2,maxi,maxj)
      real errwtq(2)
      real tqav(2), tqvar(2)
c AY (05/12/03)
c for Jia avg
c     real tv3,tv4,tv5
c     real opsi(0:maxj,0:maxk), ou(maxj,maxk)
c     real opsia(0:maxj,0:maxk), omina, omaxa
c     real opsip(0:maxj,0:maxk), ominp, omaxp

c AY (03/12/03) : this looks like an error, I'll correct it
c     parameter(syr = 365*86400)
c AY (03/05/04) : syr not used anymore (also, yearlen now available)
c     parameter(syr = 365.25*86400)

      integer i, j, l, ios

c     data tsav, tqav, tsvar, tqvar/8*0.0/
      data tqav, tqvar/4*0.0/

c axes
      real lon(maxi),lat(maxj)

c If 'tsinterp' is '.true.': i) discontinue writing out of model-data
c field, ii) replace error score with the score calculated using the
c 'err_gold(...)' function further below
      if (.not.tqinterp) then
c read interpolated Levitus and NCEP data

c AY (05/12/03)
c     open(30,file='tempann.silo')
c     read(30,*)(((tsdata(1,i,j,k),k=1,kmax),i=1,imax),j=1,jmax)
c     close(30)
c     open(31,file='saliann.silo')
c     read(31,*)(((tsdata(2,i,j,k),k=1,kmax),i=1,imax),j=1,jmax)
c     close(31)
c     open(32,file='ta_ncep.silo')
      call check_unit(32,__LINE__,__FILE__)
      open(32,file=indir_name(1:lenin)//tdatafile(1:lentdata),
     &     iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      read(32,*,iostat=ios)((tqdata(1,i,j),i=1,imax),j=1,jmax)
      call check_iostat(ios,__LINE__,__FILE__)
      close(32,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)

c     open(33,file='qa_ncep.silo')
      call check_unit(33,__LINE__,__FILE__)
      open(33,file=indir_name(1:lenin)//qdatafile(1:lenqdata),
     &     iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      read(33,*,iostat=ios)((tqdata(2,i,j),i=1,imax),j=1,jmax)
      call check_iostat(ios,__LINE__,__FILE__)
      close(33,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)

      do j=1,jmax
         do i=1,imax
c AY (05/12/03)
c           do k=1,kmax
c              tsdata(2,i,j,k) = tsdata(2,i,j,k) - saln0
c           enddo
            tqdata(2,i,j) = tqdata(2,i,j)*1e-3
         enddo
      enddo

c calculate weights based on variance of data NB not real spatial but
c computational spatial

c AY (05/12/03)
c     do k=1,kmax
c        do j=1,jmax
c           do i=1,imax
c              if(k.ge.k1(i,j))then
c                 do l=1,2
c                    tsav(l) = tsav(l) + tsdata(l,i,j,k)
c                    tsvar(l) = tsvar(l)
c    &                        + tsdata(l,i,j,k)*tsdata(l,i,j,k)
c                 enddo
c              endif
c           enddo
c        enddo
c     enddo
c
c     do l=1,2
c        tsav(l) = tsav(l)/ntot
c        tsvar(l) = tsvar(l)/ntot - tsav(l)*tsav(l)
c     enddo

      do j=1,jmax
         do i=1,imax
            do l=1,2
               tqav(l) = tqav(l) + tqdata(l,i,j)
               tqvar(l) = tqvar(l) + tqdata(l,i,j)*tqdata(l,i,j)
            enddo
         enddo
      enddo

      do l=1,2
         tqav(l) = tqav(l)/(imax*jmax)
         tqvar(l) = tqvar(l)/(imax*jmax) - tqav(l)*tqav(l)
      enddo

c     print*,' data averages and variances in comp. space'
c     print*,(tsav(l),tsvar(l),tqav(l),tqvar(l),l=1,2)

c specify weights

c AY (05/12/03)
c     do k=1,kmax
c        errwts(1,k) = 1.0/tsvar(1)
c        errwts(2,k) = 1.0/tsvar(2)
c     enddo
      errwtq(1) = 1.0/tqvar(1)
      errwtq(2) = 1.0/tqvar(2)

c calculate error compared to observations (!)

c     open(25,file='tmp.err')
      call check_unit(25,__LINE__,__FILE__)
      open(25,file=outdir_name(1:lenout)//'tmp.err',iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)

      err = 0.
      do j=1,jmax
         do i=1,imax
c AY (05/12/03)
c           do k=1,kmax
c              if(k.ge.k1(i,j))then
c                 do l=1,2
c                    err = err + errwts(l,k)
c    &                   *(ts(l,i,j,k) - tsdata(l,i,j,k))**2
c                    write(25,10)ts(l,i,j,k) - tsdata(l,i,j,k)
c                 enddo
c              else
c                 write(25,10)0.0,0.0
c              endif
c           enddo
            do l=1,2
               err = err + errwtq(l)*(tq(l,i,j) - tqdata(l,i,j))**2
            enddo
            if (debug_init) 
     & write(25,10)(tq(l,i,j) - tqdata(l,i,j),l=1,2)
         enddo
      enddo
   10 format(e15.5)
      close(25,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)

c AY (05/12/03) : next line includes GOLDSTEIN for calculation
c     err = sqrt(err/((ntot + imax*jmax)*2))
      err = sqrt(err/(imax*jmax*2))
      if (debug_init) print*,
     & 'EMBM : weighted r.m.s. model-data error ',err
      else
         if (debug_init) print*,
     & "Writing out of model-data error fields (file"//
     $        " 'tmp.err') is inactive when observational dataset"//
     $        " is interpolated at runtime (i.e., 'tqinterp' is"//
     &        " '.true.')."
      endif

c AP (03/08/06) : Call external error function
c                 Should return identical result
      do i=1,imax
         lon(i)=180.0*(phi0+(i-0.5)*dphi)/pi
      enddo
      do j=1,jmax
         lat(j)=180.0*asin(s(j))/pi
      enddo
      err3 = err_embm(tq(1,1:imax,1:jmax), 1, imax, jmax, indir_name,
     $     lenin, tdatafile, lentdata, tdata_scaling, tdata_offset,
     $     tqinterp, tdata_varname, tdata_missing, lon, lat)
      if (qdata_rhum) then
         err4 = err_embm(rq_pa(1:imax,1:jmax), 2, imax, jmax, indir_name,
     $        lenin, qdatafile, lenqdata, qdata_scaling, qdata_offset,
     $        tqinterp, qdata_varname, qdata_missing, lon, lat)
      else
         err4 = err_embm(tq(2,1:imax,1:jmax), 2, imax, jmax, indir_name,
     $        lenin, qdatafile, lenqdata, qdata_scaling, qdata_offset,
     $        tqinterp, qdata_varname, qdata_missing, lon, lat)
      endif
      if (debug_init) print*,
     & 'err_embm composite = ',sqrt( ((err3**2*imax*jmax) +
     &                                 (err4**2*imax*jmax))
     &                               / ( 2*imax*jmax ) )


c AY (16/12/03) : the following surface flux and sea-ice diagnostics
c                 could probably be output by end_surflux.F or some
c                 similar routine

c write out pptn
c
c     open(25,file='../results/'//lout//'.pptn')
c     sum = 0
c     do j=1,jmax
c        do i=1,imax
c           write(25,100) pptn(i,j)*syr 
c           sum = sum + pptn(i,j)
c        enddo
c     enddo
c     write(6,*)'average pptn m/yr',sum*syr/imax/jmax
c     close(25)

c write out evap
c
c     open(26,file='../results/'//lout//'.evap')
c     sum = 0
c     do j=1,jmax
c        do i=1,imax
c           tv2 = (evap(i,j)*(1-varice1(2,i,j))
c    1          + evapsic(i,j)*varice1(2,i,j))
c           write(26,100)tv2*syr
c           sum = sum + tv2
c        enddo
c     enddo
c     write(6,*)'average evap m/yr',sum*syr/(imax*jmax)
c     close(26)

c write out runoff
c
c     open(27,file='../results/'//lout//'.runoff')
c     sum = 0
c     do j=1,jmax
c        do i=1,imax
c           write(27,100) runoff(i,j)*syr
c           sum = sum +runoff(i,j)
c        enddo
c     enddo
c     write(6,*)'average runoff m/yr',sum*syr/imax/jmax
c     close(27)

c Artic ice diag
c
c     open(28,file='../results/'//lout//'.arcice')
c     do i=1,imax
c        write(28,'(3e14.6)')varice(1,i,jmax),varice(2,i,jmax)
c    1    ,tice(i,jmax)
c     enddo
c     close(28)

c write out net freshwater flux into ocean (P-E+R+freeze/melt)
c
c     open(29,file='../results/'//lout//'.fwfxneto')
c     sum = 0
c     do j=1,jmax
c        do i=1,imax
c           write(29,100)fwfxneto(i,j)*syr
c           sum = sum + fwfxneto(i,j)
c        enddo
c     enddo
c     sum = sum*syr/(imax*jmax)
c     write(6,*)'global average net fwflux into ocean',sum
c     close(29)

c write out net heat flux into ocean
c
c     open(29,file='../results/'//lout//'.fx0neto')
c     sum = 0
c     do j=1,jmax
c        do i=1,imax
c           write(29,100)fx0neto(i,j)
c           sum = sum + fx0neto(i,j)
c        enddo
c     enddo
c     sum = sum/(imax*jmax)
c     write(6,*)'global average net heat flux into ocean',sum
c     close(29)

c write out net surface heat flux into atmos
c
c     open(29,file='../results/'//lout//'.fx0a')
c     sum = 0
c     do j=1,jmax
c        do i=1,imax
c           write(29,100) fx0a(i,j)
c           sum = sum + fx0a(i,j)
c        enddo
c     enddo
c     sum = sum/(imax*jmax)
c     write(6,*)'average net heat flux in atmos',sum
c     close(29)

c nre 6/10/3 write precipitated atm. humidity
c
c     open(29,file='../results/'//lout//'.qdry')
c     do j=1,jmax
c        do i=1,imax
c           tv2 = const1*exp(const4*tq(1,i,j)
c    1                      /(tq(1,i,j)+const5))
c           write(29,100) min(tq(2,i,j),rmax*tv2)
c        enddo
c     enddo
c     close(29)

c final CO_2
c
c     write(6,*)'final CO_2 at i*j=1',co2(1,1)

c AY (16/12/03) : this can still be calculated
c northward atm. heat flux

      call diagfna

c calc temp for comparison with Jia (2003)

c AY (05/12/03)
c     call diagopsi(ominp,omaxp,omina,omaxa,opsi,opsia,opsip)
c
c nearest point to 24deg North if i=36
c     j=26
c
c     tv4 = 0.
c     tv5 = 0.
c     do k=1,kmax
c first calculate average temp at this depth and lat.
c        tv2 = 0.
c        tv3 = 0.
c        do i=ias(j),iaf(j)
c           if(k1(i,j).le.k.and.k1(i,j+1).le.k)then
c              tv2 = tv2 + (ts(1,i,j+1,k) +
c    1                       ts(1,i,j,k))*dphi
c              tv3 = tv3 + dphi
c           endif
c        enddo
c        if(tv3.gt.1e-9) tv3 = 0.5*tv2/tv3
c        print*,k,'av temp',tv3
c        do i=ias(j),iaf(j)
c           if(k1(i,j).le.k.and.k1(i,j+1).le.k)then
c              if(k.le.4)then
c                 tv4 = tv4 + cv(j)*u(2,i,j,k)*tv3*dz(k)*dphi
c              else
c                 tv5 = tv5 + cv(j)*u(2,i,j,k)*tv3*dz(k)*dphi
c              endif
c           endif
c        enddo
c     enddo
c     tv4 = - tv4/opsia(j,4)
c     tv5 = tv5/opsia(j,4)
c     print*,'volm transport weighted temperatures j=26 and opsia'
c     print*,tv4,tv5,opsia(j,4)

c 100 format(e14.7)

      end
*
* diagfna.f quick modification of tstepa.f to allow calculation and
* plotting of northwards atm. heat flux 22/3/3
* subroutine tstepa.f for program goldstein, introduced 8/2/02
* transports tair, qair meridionally and vertically
* updates tair, qair in lower atmosphere
* 
* flux version fully explicit one step second order variable depth
*
      subroutine diagfna

      use genie_util, ONLY: check_unit, check_iostat

#include "embm.cmn"
c     include 'extra.cmn'
      
      real fn(2), diffextra, fntot
c      real tv

      integer i, j, l, ios

c AY (09/01/04) : excised - in common block now
c     character*7 lout

c     open(43,file='../results/'//lout//'.fofya')
      call check_unit(43,__LINE__,__FILE__)
      open(43,file=outdir_name(1:lenout)//lout//'.fofya',iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
c     *NB where is the close for this?*

* 2nd order explicit step

      do 100 j=1,jmax
         fntot = 0.

         do 110 i=1,imax
            l=1   
            if(j.ne.jmax)then

               fn(l) = cv(j)*betam(l)*uatm(2,i,j)*(tq1(l,i,j+1)
     1                       + tq1(l,i,j))*0.5
crma               diffextra = (2-l)*diffmod0*max(0.0,min(1.0,
crma     1                  (pptn(i,j)-ppmin)/(ppmax-ppmin)))
               diffextra = 0.
               fn(l) = fn(l) - cv(j)*cv(j)*(diffa(l,2,j)
     1                     + diffextra)
     1                     *(tq1(l,i,j+1) - tq1(l,i,j))*rds(j)
            else
               fn(l) = 0.
            endif

c nre dimless height of atm set to 1, implies factor of h in fluxsc

c++++++++++++++++++++++++++++++++++
c diagnostic for n. heat flux
               fntot = fntot + fn(1)
c++++++++++++++++++++++++++++++++++
  110    continue
         if(j.lt.jmax)write(43,'(e15.5)')
     &              dphi*fntot*usc*rhoair*cpa*hatmbl(1)*rsc

  100 continue

      end
