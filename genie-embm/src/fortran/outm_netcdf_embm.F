* AY (05/10/04) : writes out netCDF restart files for the EMBM
*                 based upon code written by Dan Lunt for GOLDSTEIN
c
c RM (16/05/05) : edited for variable sin(lat) resolution (from NRE, 6/12/04)
c

      subroutine outm_netcdf_embm(istep)

c     This module writes netcdf restarts for embm

#include "embm.cmn"
      include 'netcdf.inc'
      include 'netcdf_grid.cmn'

      integer istep

      real temp_write(maxi,maxj)
      real shum_write(maxi,maxj)

      real lons1(maxi)
      real lats1(maxj)

c      integer landmask(maxi,maxj)

      integer i,j

      integer ntempid
      integer nshumid

      integer nlon1id,nlongit1id,nlat1id,nlatit1id
      integer nrecsid,ioffsetid

      integer dim1pass(2)

      character fname*200

c     For date and restarts...
      integer iday
      integer iyearid
      integer imonthid
      integer idayid   
      character yearstring*10
      character monthstring*2
      character daystring*2
      character datestring*7

c     For netcdf...
      integer status
      integer ncid

      real timestep

c AY (06/10/04) : extra variables for printing out average model properties
      integer l
c      integer icell
      real    tmp_val(2)
      real    area

c     output file format is yyyy_mm_dd
c     30 day months are assumed
      if (mod(yearlen,30.0).ne.0) then
         print*, 'ERROR: Goldstein NetCDF restarts (outm_netdf):'
         print*, '   mod(yearlen,30.0) must be zero'
         stop
      end if

      timestep=yearlen/real(nyear*ndta)

      iday=nint(day_rest)

      if(mod(istep,(iwstp*ndta)).eq.0)then

c     WRITE A RESTART.....

c AY (04/10/04) : uses grid variables calculated in initialise_seaice.F
      do i=1,maxi
         lons1(i)=nclon1(i)
      end do

      do j=1,maxj
         lats1(j)=nclat1(j)
      enddo

c AY (05/10/04) : landmask not necessary here
c     landmask(:,:)=0
c     do i=1,maxi
c        do j=1,maxj
c           if (k1(i,j).gt.maxk) landmask(i,j)=1
c        enddo
c     enddo

      temp_write(:,:)=real(tq(1,1:maxi,1:maxj))
      shum_write(:,:)=real(tq(2,1:maxi,1:maxj))

      write(datestring,'(i7.7)') istep
      write(yearstring,'(i10)') iyear_rest
      write(monthstring,'(i2.2)') imonth_rest
      write(daystring,'(i2.2)') iday

!-------------------------------------------------------
!     create a netcdf file
!-------------------------------------------------------
      fname=trim(dirnetout)//
     :        '/embm_restart_'//
     :        trim(adjustl(yearstring))//'_'//monthstring//'_'//
     :        daystring//'.nc'
      print*,' Opening netcdf restart file for write: ',trim(fname)
      status=nf_create(trim(fname), nf_clobber, ncid)
      call check_err(status)
      status=nf_def_dim(ncid, 'nrecs',1,nrecsid)
      call check_err(status)
      status=nf_def_dim(ncid, 'longitude',maxi,nlon1id)
      call check_err(status)
      status=nf_def_dim(ncid, 'latitude',maxj,nlat1id)
      call check_err(status)

      status=nf_def_var(ncid,'longitude',nf_real,1,nlon1id,nlongit1id)
      call check_err(status)
      status=nf_def_var(ncid,'latitude',nf_real,1,nlat1id,nlatit1id)
      call check_err(status)
      dim1pass(1)=nlon1id
      dim1pass(2)=nlat1id
      status=nf_def_var(ncid,'ioffset',nf_int,1,nrecsid,ioffsetid)
      call check_err(status)
      status=nf_def_var(ncid,'iyear',nf_int,1,nrecsid,iyearid)
      call check_err(status)
      status=nf_def_var(ncid,'imonth',nf_int,1,nrecsid,imonthid)
      call check_err(status)
      status=nf_def_var(ncid,'iday',nf_int,1,nrecsid,idayid)
      call check_err(status)

      status=nf_def_var(ncid,'air_temp',nf_double,2,dim1pass,
     &                     ntempid)
      call check_err(status)
      status=nf_def_var(ncid,'humidity',nf_double,2,dim1pass,
     &                     nshumid)
      call check_err(status)
      status=nf_enddef(ncid)
      call check_err(status)

      status=nf_put_var_int(ncid,iyearid,iyear_rest)
      call check_err(status)
      status=nf_put_var_int(ncid,imonthid,imonth_rest)
      call check_err(status)
      status=nf_put_var_int(ncid,idayid,iday)
      call check_err(status)
      status=nf_put_var_int(ncid,ioffsetid,ioffset_rest)
      call check_err(status)

        status=nf_put_var_double(ncid,nlongit1id,lons1)
        call check_err(status)
        status=nf_put_var_double(ncid,nlatit1id,lats1)
        call check_err(status)
        status=nf_put_var_double(ncid,ntempid,temp_write)
        call check_err(status)
        status=nf_put_var_double(ncid,nshumid,shum_write)
        call check_err(status)

      status=nf_close(ncid)
      call check_err(status)

c AY (06/10/04) : copied from outm_embm.f

c     AY (08/03/04) : write out averages for restart checks
      write(*,220) 'Avg T','Avg Q'
c     
c     Clear temporary variables
      do l=1,2
         tmp_val(l) = 0
      enddo
c
c     Sum layer state variables and flow field
      area = 0.
      do j=1,jmax
         do i=1,imax
            area = area + ds(j)
            do l=1,2
               tmp_val(l) = tmp_val(l) + tq(l,i,j)*ds(j)
            enddo
         enddo
      enddo
c
c     Print average values out
crma      write(*,210) tmp_val(1)/(imax*jmax),
crma     +     (tmp_val(2)/(imax*jmax))*1000.
      write(*,210) tmp_val(1)/area,
     +     (tmp_val(2)/area)*1000.

 210  format(2f13.9)
 220  format(2a13)

      endif

      day_rest=day_rest+timestep
c     This bit so that we don't get too far out in our count....
c     Anchor to a day if we start drifting.
c     Means timestep can never be less than 1/1000 of a day!!!!
      if (abs(iday-day_rest).le.1e-3) then
        print*,'CORRECTING TIME-LAG! in outm_netcdf in genie-goldstein',
     :                 iday,day_rest
        day_rest=iday
      endif
      if (day_rest.ge.31) then
        day_rest=day_rest-30
        imonth_rest=imonth_rest+1
        if (imonth_rest.eq.13) then
          imonth_rest=1
          iyear_rest=iyear_rest+1
        endif
      endif
      iday=nint(day_rest)

      return
      end
