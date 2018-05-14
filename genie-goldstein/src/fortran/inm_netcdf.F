      subroutine inm_netcdf(lrestart_genie)

c     This module reads netcdf restarts for goldstein      

#include "ocean.cmn"
      include 'netcdf.inc'

      real temp_read(maxi,maxj,maxk)
      real salinity_read(maxi,maxj,maxk)
      real uvel_read(maxi,maxj,maxk)
      real vvel_read(maxi,maxj,maxk)
      real evap_read(maxi,maxj)
      real late_read(maxi,maxi)
      real sens_read(maxi,maxj)

c     For date and restarts...
      integer iday

c     For netcdf...
      integer ifail
c      integer status
      integer ncid
      logical lexist
      character*200 fnamein
      
      real timestep

c AY (06/10/04) : extra variables for printing out average model properties
      integer i,j,k,l,icell
      real    tmp_val(4)
      
      logical lrestart_genie

      timestep=24.0*60.0*60.0*yearlen/real(nyear)

      fnamein=trim(filenetin)

         ifail=0
            inquire(file=trim(fnamein),
     :           exist=lexist)
            if (.not.lexist) then
               print*,' Missing file ',
     :           trim(fnamein)
               ifail=1
            endif
         if (ifail.ne.0) then
           print*,' Correct error and try again '
           stop 1
         endif

         print*,'GOLDSTEIN: Opening restart file for read: ',
     &      trim(filenetin)

         call open_file_nc(
     &      trim(fnamein),ncid)
         call get1di_data_nc(ncid,'ioffset',1,ioffset_rest,ifail)
         call get1di_data_nc(ncid,'iyear',1,iyear_rest,ifail)
         call get1di_data_nc(ncid,'imonth',1,imonth_rest,ifail)
         call get1di_data_nc(ncid,'iday',1,iday,ifail)
         call get3d_data_nc(ncid,'temp',maxi,maxj,maxk,
     &          temp_read,ifail)
         call get3d_data_nc(ncid,'salinity',maxi,maxj,maxk,
     &          salinity_read,ifail)
         call get3d_data_nc(ncid,'uvel',maxi,maxj,maxk,
     &          uvel_read,ifail)
         call get3d_data_nc(ncid,'vvel',maxi,maxj,maxk,
     &          vvel_read,ifail)
         call get2d_data_nc(ncid,'evap',maxi,maxj,
     &          evap_read,ifail)
         call get2d_data_nc(ncid,'late',maxi,maxj,
     &          late_read,ifail)
         call get2d_data_nc(ncid,'sens',maxi,maxj,
     &          sens_read,ifail)
         call close_file_nc(
     &      trim(filenetin),ncid)

         day_rest=iday
         ioffset_rest=mod(ioffset_rest,nint(yearlen))

      day_rest=day_rest+timestep/(24*60*60.)
c     This bit so that we don't get too far out in our count....
c     Anchor to a day if we start drifting.
c     Means timestep can never be less than 1/1000 of a day!!!!
      if (abs(iday-day_rest).le.1e-3) then
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
      if (debug_init) print*,'day in goldstein restart is now',day_rest
      if (debug_init) print*,'iday in goldstein restart is now',iday

      ts(1,1:maxi,1:maxj,1:maxk)=temp_read(:,:,:)
      ts(2,1:maxi,1:maxj,1:maxk)=salinity_read(:,:,:)
      u1(1,1:maxi,1:maxj,1:maxk)=uvel_read(:,:,:)
      u1(2,1:maxi,1:maxj,1:maxk)=vvel_read(:,:,:)
      u(:,:,:,:)=u1(:,:,:,:)

c     The i and j below were :,: but this fails at
c       compilation time in the 64x32 case for a 
c       reason I can't work out.  DJL 27/10/2005
      if (lrestart_genie) then
        do j=1,jmax
        do i=1,imax
          evap_save2(i,j)=evap_read(i,j)
          late_save2(i,j)=late_read(i,j)
          sens_save2(i,j)=sens_read(i,j)
        enddo
        enddo
      else
        do j=1,jmax
        do i=1,imax
          evap_save2(i,j)=0.0
          late_save2(i,j)=0.0
          sens_save2(i,j)=0.0
        enddo
        enddo
      endif

c AY (06/10/04) : copied from inm.f

c     AY (08/03/04) : write out layer averages for restart checks
      if (debug_init) write(*,120) 'Layer','Avg T','Avg S','Avg U','Avg V',
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
                  tmp_val(l+2) = tmp_val(l+2) + u1(l,i,j,k)
c
c                 Set up u array from read-in u1 array
                  u(l,i,j,k) = u1(l,i,j,k)
               enddo
            enddo
         enddo
c
c        Print average values out
         if (debug_init) 
     +        write(*,110) k,tmp_val(1)/icell,(tmp_val(2)/icell)+saln0,
     +        tmp_val(3)/(imax*jmax),tmp_val(4)/(imax*jmax),icell
      enddo

 110  format(i5,2f13.9,2e17.9,i4)
 120  format(a5,2a13,2a17,a6)

      return
      end
