      subroutine outm_netcdf(istep)

c     This module writes netcdf restarts for goldstein

#include "ocean.cmn"
      include 'netcdf.inc'
      include 'netcdf_grid.cmn'

      integer istep

      real temp_write(maxi,maxj,maxk)
      real salinity_write(maxi,maxj,maxk)
      real uvel_write(maxi,maxj,maxk)
      real vvel_write(maxi,maxj,maxk)
      real evap_write(maxi,maxj)
      real late_write(maxi,maxj)
      real sens_write(maxi,maxj)


      real lons1(maxi)
      real lats1(maxj)
      real depths1(maxk)   

      integer landmask(maxi,maxj,maxk)

      integer i,j,k

      integer ntempid
      integer nsalinityid
      integer nuvelid
      integer nvvelid
      integer nevapid
      integer nlateid
      integer nsensid

      integer nlon1id,nlongit1id,ndep1id,nlat1id,nlatit1id,ndepth1id
      integer nrecsid,ioffsetid

      integer dim1pass(3)
      integer dimpass(2)

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
      integer l,icell
      real    tmp_val(4)

c     output file format is yyyy_mm_dd
c     30 day months are assumed
      if (mod(yearlen,30.0).ne.0) then
         print*, 'ERROR: Goldstein NetCDF restarts (outm_netdf):'
         print*, '   mod(yearlen,30.0) must be zero'
         stop
      end if

      timestep=yearlen/real(nyear)

      iday=nint(day_rest)

      if(mod(istep,iwstp).eq.0)then

c     WRITE A RESTART.....

c     This bit modified from initialise_ocean.F
      do i=1,maxi
         lons1(i)=nclon1(i)
      end do

      do j=1,maxj
         lats1(j)=nclat1(j)
      enddo

      do k=1,maxk
         depths1(k)=ncdepth(maxk-k+1)
      enddo

      landmask(:,:,:)=0
      do i=1,maxi
        do j=1,maxj
          do k=1,maxk
            if (k.ge.k1(i,j)) landmask(i,j,k)=1
          enddo
        enddo
      enddo

      temp_write(:,:,:)=real(ts(1,1:maxi,1:maxj,1:maxk)*landmask(:,:,:))
      salinity_write(:,:,:)=real(ts(2,1:maxi,1:maxj,1:maxk)*
     :                             landmask(:,:,:))
      uvel_write(:,:,:)=real(u(1,1:maxi,1:maxj,1:maxk))
      vvel_write(:,:,:)=real(u(2,1:maxi,1:maxj,1:maxk))
      evap_write(:,:)=evap_save2(:,:)
      late_write(:,:)=late_save2(:,:)
      sens_write(:,:)=sens_save2(:,:)

      write(datestring,'(i7.7)') istep
      write(yearstring,'(i10)') iyear_rest
      write(monthstring,'(i2.2)') imonth_rest
      write(daystring,'(i2.2)') iday

!-------------------------------------------------------
!     create a netcdf file
!-------------------------------------------------------
      fname=trim(dirnetout)//
     :        '/goldstein_restart_'//
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
      status=nf_def_dim(ncid, 'depth',maxk,ndep1id)
      call check_err(status)

      status=nf_def_var(ncid,'longitude',nf_real,1,nlon1id,nlongit1id)
      call check_err(status)
      status=nf_def_var(ncid,'latitude',nf_real,1,nlat1id,nlatit1id)
      call check_err(status)
      status=nf_def_var(ncid,'depth',nf_real,1,ndep1id,ndepth1id)
      call check_err(status)
      dim1pass(1)=nlon1id
      dim1pass(2)=nlat1id
      dim1pass(3)=ndep1id
      dimpass(1)=nlon1id
      dimpass(2)=nlat1id
      status=nf_def_var(ncid,'ioffset',nf_int,1,nrecsid,ioffsetid)
      call check_err(status)
      status=nf_def_var(ncid,'iyear',nf_int,1,nrecsid,iyearid)
      call check_err(status)
      status=nf_def_var(ncid,'imonth',nf_int,1,nrecsid,imonthid)
      call check_err(status)
      status=nf_def_var(ncid,'iday',nf_int,1,nrecsid,idayid)
      call check_err(status)
      status=nf_def_var(ncid,'temp',nf_double,3,dim1pass,
     &                     ntempid)
      call check_err(status)
      status=nf_def_var(ncid,'salinity',nf_double,3,dim1pass,
     &                     nsalinityid)
      call check_err(status)
      status=nf_def_var(ncid,'uvel',nf_double,3,dim1pass,
     &                     nuvelid)
      call check_err(status)
      status=nf_def_var(ncid,'vvel',nf_double,3,dim1pass,
     &                     nvvelid)
      call check_err(status)
      status=nf_def_var(ncid,'evap',nf_double,2,dimpass,
     &                     nevapid)
      call check_err(status)
      status=nf_def_var(ncid,'late',nf_double,2,dimpass,
     &                     nlateid)
      call check_err(status)
      status=nf_def_var(ncid,'sens',nf_double,2,dimpass,
     &                     nsensid)
      call check_err(status)

c     ccccccccccccccccccc
c     These changes are so that certain netcdf viewers can read the netcdf fields OK.
c     DJL. 
      status=nf_put_att_text(ncid,nlongit1id,
     :                             'units',12, 
     :                             'degrees_east')
      status=nf_put_att_text(ncid,nlongit1id,
     :                             'long_name',9, 
     :                             'longitude')

      status=nf_put_att_text(ncid,nlatit1id,
     :                             'units',13, 
     :                             'degrees_north')
      status=nf_put_att_text(ncid,nlatit1id,
     :                             'long_name',8, 
     :                             'latitude')
c     ccccccccccccccccccc

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
        status=nf_put_var_double(ncid,ndepth1id,depths1)
        call check_err(status)
        status=nf_put_var_double(ncid,ntempid,temp_write)
        call check_err(status)
        status=nf_put_var_double(ncid,nsalinityid,salinity_write)
        call check_err(status)
        status=nf_put_var_double(ncid,nuvelid,uvel_write)
        call check_err(status)
        status=nf_put_var_double(ncid,nvvelid,vvel_write)
        call check_err(status)
        status=nf_put_var_double(ncid,nevapid,evap_write)
        call check_err(status)
        status=nf_put_var_double(ncid,nlateid,late_write)
        call check_err(status)
        status=nf_put_var_double(ncid,nsensid,sens_write)
        call check_err(status)

      status=nf_close(ncid)
      call check_err(status)

c AY (06/10/04) : copied from outm.f

c AY (08/03/04) : write out layer averages for restart checks
      write(*,120) 'Layer','Avg T','Avg S','Avg U','Avg V',
     +     'Cells'
      do k=1,kmax
c
c        Clear temporary variables
         do l=1,4
            tmp_val(l) = 0
         enddo
         icell = 0
c
c        Sum layer state variables and flow field
         do j=1,jmax
            do i=1,imax
               if(k.ge.k1(i,j)) icell = icell + 1
               do l=1,2
                  if(k.ge.k1(i,j))then
                     tmp_val(l) = tmp_val(l) + ts(l,i,j,k)
                  endif
                  tmp_val(l+2) = tmp_val(l+2) + u(l,i,j,k)
               enddo
            enddo
         enddo
c
c        Print average values out
         write(*,110) k,tmp_val(1)/icell,(tmp_val(2)/icell)+saln0,
     +        tmp_val(3)/(imax*jmax),tmp_val(4)/(imax*jmax),icell
      enddo

 110  format(i5,2f13.9,2e17.9,i4)
 120  format(a5,2a13,2a17,a6)

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
