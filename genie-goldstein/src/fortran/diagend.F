c
c diagend.f end-of-run diagnostics for c-goldstein created 2/9/2 nre
c
c tsdata and tqdata contain observational estimates for ts and tq
c err is the mismatch between model and data weighted by errw 
c diagnostic deep temperatures for exact comparison with Jia (2003) 16/6/3
c
c AY (03/12/03) : altered for genie-goldstein
c		  references to EMBM and sea-ice removed
c
c AP (09/08/06) : error calculation modified to cope with null values
c                 in the observational data
c

c     subroutine diagend(lout)
      subroutine diagend

      use genie_util, ONLY: check_unit, check_iostat

#include "ocean.cmn"

c     character lout*7

      real tv2, err
      real err1, err2, err_gold
      real nullvalue
c      real sum, syr
c AY (03/12/03)
c     real tsdata(maxl,maxi,maxj,maxk), tqdata(2,maxi,maxj)
c     real errwts(maxl,maxk), errwtq(2)
c     real tsav(2), tqav(2), tsvar(2), tqvar(2)
      real tsdata(maxl,maxi,maxj,maxk)
      real errwts(maxl,maxk)
      real tsav(2), tsvar(2), ntotal(2)
c for Jia avg
      real tv3,tv4,tv5
      real opsi(0:maxj,0:maxk)
c      real ou(maxj,maxk)
      real opsia(0:maxj,0:maxk), omina, omaxa
      real opsip(0:maxj,0:maxk), ominp, omaxp
      integer iposa(2)

      integer i, j, k, l, ios
      integer ntot1, ntot2

c AY (03/12/03)
c     data tsav, tqav, tsvar, tqvar/8*0.0/
      data tsav, tsvar, ntotal/6*0.0/
      parameter(nullvalue = -1.e10)

c axes
      real lon(maxi),lat(maxj),depth(maxk)

c AY (03/12/03) : this looks like an error, I'll correct it
c     parameter(syr = 365*86400)
c AY (08/04/04) : because of yearlen, can't use a parameter statement
c      syr = yearlen*86400

c If 'tsinterp' is '.true.': i) discontinue writing out of model-data
c field, ii) replace error score with the score calculated using the
c 'err_gold(...)' function further below (which should be the same to
c computational precision)
      if (.not.tsinterp) then
c read interpolated Levitus and NCEP data

      call check_unit(30,__LINE__,__FILE__)
      open(30,file=indir_name(1:lenin)//tdatafile(1:lentdata),
     &     iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      read(30,*,iostat=ios)(((tsdata(1,i,j,k),k=1,kmax),i=1,imax),j=1,
     &     jmax)
      call check_iostat(ios,__LINE__,__FILE__)
      close(30,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)

      call check_unit(31,__LINE__,__FILE__)
      open(31,file=indir_name(1:lenin)//sdatafile(1:lensdata),
     &     iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      read(31,*,iostat=ios)(((tsdata(2,i,j,k),k=1,kmax),i=1,imax),j=1,
     &     jmax)
      call check_iostat(ios,__LINE__,__FILE__)
      close(31,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
c AY (03/12/03)
c     open(32,file='ta_ncep.silo')
c     read(32,*)((tqdata(1,i,j),i=1,imax),j=1,jmax)
c     close(32)
c     open(33,file='qa_ncep.silo')
c     read(33,*)((tqdata(2,i,j),i=1,imax),j=1,jmax)
c     close(33)
      do j=1,jmax
         do i=1,imax
            do k=1,kmax
               tsdata(2,i,j,k) = tsdata(2,i,j,k) - saln0
            enddo
c AY (03/12/03)
c           tqdata(2,i,j) = tqdata(2,i,j)*1e-3
         enddo
      enddo

c calculate weights based on variance of data NB not real spatial but
c computational spatial

      do k=1,kmax
         do j=1,jmax
            do i=1,imax
               if(k.ge.k1(i,j))then
                  do l=1,2
                     if(tsdata(l,i,j,k).gt.nullvalue)then
                        tsav(l) = tsav(l) + tsdata(l,i,j,k)
                        tsvar(l) = tsvar(l)
     &                           + tsdata(l,i,j,k)*tsdata(l,i,j,k)
                        ntotal(l) = ntotal(l) + 1
                     endif
                  enddo
               endif
            enddo
         enddo
      enddo

      do l=1,2
         tsav(l) = tsav(l)/ntotal(l)
         tsvar(l) = tsvar(l)/ntotal(l) - tsav(l)*tsav(l)
      enddo

c AY (03/12/03)
c     do j=1,jmax
c        do i=1,imax
c           do l=1,2
c              tqav(l) = tqav(l) + tqdata(l,i,j)
c              tqvar(l) = tqvar(l) + tqdata(l,i,j)*tqdata(l,i,j)
c           enddo
c        enddo
c     enddo
c
c     do l=1,2
c        tqav(l) = tqav(l)/(imax*jmax)
c        tqvar(l) = tqvar(l)/(imax*jmax) - tqav(l)*tqav(l)
c     enddo

c     print*,' data averages and variances in comp. space'
c     print*,(tsav(l),tsvar(l),tqav(l),tqvar(l),l=1,2)

c specify weights

      do k=1,kmax
         errwts(1,k) = 1.0/tsvar(1)
         errwts(2,k) = 1.0/tsvar(2)
      enddo
c AY (03/12/03)
c     errwtq(1) = 1.0/tqvar(1)
c     errwtq(2) = 1.0/tqvar(2)

c calculate error compared to observations (!)

c AY (03/12/03)
c     open(25,file='../results/'//lout//'.err')
c     open(25,file='tmp.err')
      call check_unit(25,__LINE__,__FILE__)
      open(25,file=outdir_name(1:lenout)//'tmp.err',iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      err = 0.
      do j=1,jmax
         do i=1,imax
            do k=1,kmax
               if(k.ge.k1(i,j))then
                  do l=1,2
                     if(tsdata(l,i,j,k).gt.nullvalue)then
                        err = err + errwts(l,k)
     &                      *(ts(l,i,j,k) - tsdata(l,i,j,k))**2
                        write(25,10,iostat=ios)ts(l,i,j,k) - 
     &                       tsdata(l,i,j,k)
                        call check_iostat(ios,__LINE__,__FILE__)
                     endif
                  enddo
               else
                  write(25,10,iostat=ios)0.0,0.0
                  call check_iostat(ios,__LINE__,__FILE__)
               endif
            enddo
c AY (03/12/03)
c           do l=1,2
c              err = err + errwtq(l)*(tq(l,i,j) - tqdata(l,i,j))**2
c           enddo
c           write(25,10)(tq(l,i,j) - tqdata(l,i,j),l=1,2)
         enddo
      enddo
   10 format(e15.5)
      close(25,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
c AY (03/12/03) : next line includes 2 EMBM fields in calculation
c     err = sqrt(err/((ntot + imax*jmax)*2))
      err = sqrt(err/(ntotal(1)+ntotal(2)))
      print*,'GOLD : weighted r.m.s. model-data error ',err
      else
         print*,"Writing out of model-data error fields (file"//
     $        " 'tmp.err') is inactive when observational dataset"//
     $        " is interpolated at runtime (i.e., 'tsinterp' is"//
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
      do k=1,kmax
         depth(k)=abs(zro(kmax+1-k)*dsc)
      enddo
      ntot1 = 0
      err1 = err_gold(ts(1,1:imax,1:jmax,1:kmax), 1, k1, ntot1, imax,
     $     jmax, kmax, indir_name, lenin, tdatafile, lentdata,
     $     tdata_scaling, tdata_offset, tsinterp, tdata_varname,
     $     tdata_missing, lon, lat, depth)
      ntot2 = 0
      err2 = err_gold(ts(2,1:imax,1:jmax,1:kmax), 2, k1, ntot2, imax,
     $     jmax, kmax, indir_name, lenin, sdatafile, lensdata,
     $     sdata_scaling, sdata_offset+saln0, tsinterp, sdata_varname,
     $     sdata_missing, lon, lat, depth)
      print*,'err_gold composite = ',sqrt( ((err1**2*ntot1) +
     &                                      (err2**2*ntot2))
     &                                    / ( ntot1 + ntot2 ) )
      if (tsinterp) then
         err = sqrt( ((err1**2*ntot1) +
     &        (err2**2*ntot2))
     &        / ( ntot1 + ntot2 ) )      
         print*,'GOLD : weighted r.m.s. model-data error ',err
      endif

c AY (03/12/03) : Series of surface fluxes follow.  Commented out here
c                 as they will probably be written out somewhere else
c                 in the future.

c write out pptn

c AY (03/12/03)
c     open(25,file='../results/'//lout//'.pptn')
c     open(25,file=outdir_name(1:lenout)//lout//'.pptn')
c     sum = 0
c     do j=1,jmax
c        do i=1,imax
c           write(25,100) pptn(i,j)*syr 
c           sum = sum + pptn(i,j)
c        enddo
c     enddo
c     write(6,*)'GOLD : average pptn m/yr',sum*syr/imax/jmax
c     close(25)

c write out evap

c AY (03/12/03)
c     open(26,file='../results/'//lout//'.evap')
c     open(26,file=outdir_name(1:lenout)//lout//'.evap')
c     sum = 0
c     do j=1,jmax
c        do i=1,imax
c           tv2 = (evap(i,j)*(1-varice1(2,i,j))
c    1          + evapsic(i,j)*varice1(2,i,j))
c           write(26,100)tv2*syr
c           sum = sum + tv2
c        enddo
c     enddo
c     write(6,*)'GOLD : average evap m/yr',sum*syr/(imax*jmax)
c     close(26)

c write out runoff

c AY (03/12/03)
c     open(27,file='../results/'//lout//'.runoff')
c     open(27,file=outdir_name(1:lenout)//lout//'.runoff')
c     sum = 0
c     do j=1,jmax
c        do i=1,imax
c           write(27,100) runoff(i,j)*syr
c           sum = sum +runoff(i,j)
c        enddo
c     enddo
c     write(6,*)'GOLD : average runoff m/yr',sum*syr/imax/jmax
c     close(27)

c Artic ice diag

c AY (03/12/03)
c     open(28,file='../results/'//lout//'.arcice')
c     open(28,file=outdir_name(1:lenout)//lout//'.arcice')
c     do i=1,imax
c        write(28,'(3e14.6)')varice(1,i,jmax),varice(2,i,jmax)
c    1    ,tice(i,jmax)
c     enddo
c     close(28)

c write out net freshwater flux into ocean (P-E+R+freeze/melt)

c AY (03/12/03)
c     open(29,file='../results/'//lout//'.fwfxneto')
c     open(29,file=outdir_name(1:lenout)//lout//'.fwfxneto')
c     sum = 0
c     do j=1,jmax
c        do i=1,imax
c           write(29,100)fwfxneto(i,j)*syr
c           sum = sum + fwfxneto(i,j)
c        enddo
c     enddo
c     sum = sum*syr/(imax*jmax)
c     write(6,*)'GOLD : global average net fwflux into ocean',sum
c     close(29)

c write out net heat flux into ocean

c AY (03/12/03)
c     open(29,file='../results/'//lout//'.fx0neto')
c     open(29,file=outdir_name(1:lenout)//lout//'.fx0neto')
c     sum = 0
c     do j=1,jmax
c        do i=1,imax
c           write(29,100)fx0neto(i,j)
c           sum = sum + fx0neto(i,j)
c        enddo
c     enddo
c     sum = sum/(imax*jmax)
c     write(6,*)'GOLD : global average net heat flux into ocean',sum
c     close(29)

c write out net surface heat flux into atmos

c AY (03/12/03)
c     open(29,file='../results/'//lout//'.fx0a')
c     open(29,file=outdir_name(1:lenout)//lout//'.fx0a')
c     sum = 0
c     do j=1,jmax
c        do i=1,imax
c           write(29,100) fx0a(i,j)
c           sum = sum + fx0a(i,j)
c        enddo
c     enddo
c     sum = sum/(imax*jmax)
c     write(6,*)'GOLD : average net heat flux in atmos',sum
c     close(29)

c nre 6/10/3 write precipitated atm. humidity

c AY (03/12/03)
c     open(29,file='../results/'//lout//'.qdry')
c     open(29,file=outdir_name(1:lenout)//lout//'.qdry')
c     do j=1,jmax
c        do i=1,imax
c           tv2 = const1*exp(const4*tq(1,i,j)
c    1                      /(tq(1,i,j)+const5))
c           write(29,100) min(tq(2,i,j),rmax*tv2)
c        enddo
c     enddo
c     close(29)

c final CO_2

c AY (03/12/03)
c     write(6,*)'final CO_2 at i*j=1',co2(1,1)

c northward atm. heat flux

c AY (03/12/03) - EMBM
c     call diagfna(lout)

c calc temp for comparison with Jia (2003)

      call diagopsi(ominp,omaxp,omina,omaxa,opsi,opsia,opsip,iposa)

c nearest point to 24deg North if i=36
      j=26

      tv4 = 0.
      tv5 = 0.
      do k=1,kmax
c first calculate average temp at this depth and lat.
         tv2 = 0.
         tv3 = 0.
         do i=ias(j),iaf(j)
            if(k1(i,j).le.k.and.k1(i,j+1).le.k)then
               tv2 = tv2 + (ts(1,i,j+1,k) +
     1                       ts(1,i,j,k))*dphi
               tv3 = tv3 + dphi
            endif
         enddo
         if(tv3.gt.1e-9) tv3 = 0.5*tv2/tv3
c        print*,k,'av temp',tv3
         do i=ias(j),iaf(j)
            if(k1(i,j).le.k.and.k1(i,j+1).le.k)then
               if(k.le.4)then
                  tv4 = tv4 + cv(j)*u(2,i,j,k)*tv3*dz(k)*dphi
               else
                  tv5 = tv5 + cv(j)*u(2,i,j,k)*tv3*dz(k)*dphi
               endif
            endif
         enddo
      enddo
crma      tv4 = - tv4/opsia(j,4)
crma      tv5 = tv5/opsia(j,4)
      print*,'GOLD : volm transport weighted temperatures j=26
     1     and opsia'
      print*,tv4,tv5,opsia(j,4)

      end
