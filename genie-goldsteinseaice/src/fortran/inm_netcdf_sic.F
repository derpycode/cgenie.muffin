* AY (04/10/04) : reads in netCDF restart files for GOLDSTEIN sea-ice
*                 based upon code written by Dan Lunt for GOLDSTEIN

      subroutine inm_netcdf_sic

c     This module reads netcdf restarts for goldstein sea-ice      

#include "seaice.cmn"
      include 'netcdf.inc'

      real hght_read(maxi,maxj)
      real frac_read(maxi,maxj)
      real temp_read(maxi,maxj)
      real albd_read(maxi,maxj)

c     For date and restarts...
      integer iday

c     For netcdf...
      integer ifail
      integer ncid
      logical lexist
      character*200 fnamein

      real timestep

c AY (06/10/04) : extra variables for printing out average model properties
      integer i,j,l,icell
      real    tmp_val(4)

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

         print*,' goldstein sea-ice: Opening restart file for read: ',
     &      trim(filenetin)

         call open_file_nc(
     &      trim(fnamein),ncid)
         call get1di_data_nc(ncid,'ioffset',1,ioffset_rest,ifail)
         call get1di_data_nc(ncid,'iyear',1,iyear_rest,ifail)
         call get1di_data_nc(ncid,'imonth',1,imonth_rest,ifail)
         call get1di_data_nc(ncid,'iday',1,iday,ifail)
         call get2d_data_nc(ncid,'sic_height',maxi,maxj,
     &          hght_read,ifail)
         call get2d_data_nc(ncid,'sic_cover',maxi,maxj,
     &          frac_read,ifail)
         call get2d_data_nc(ncid,'sic_temp',maxi,maxj,
     &          temp_read,ifail)
         call get2d_data_nc(ncid,'sic_albedo',maxi,maxj,
     &          albd_read,ifail)
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
      if (debug_init) 
     &   print*,'day in goldstein sea-ice restart is now',day_rest
      if (debug_init) 
     &   print*,'iday in goldstein sea-ice restart is now',iday

      varice(1,1:maxi,1:maxj)=hght_read(:,:)
      varice(2,1:maxi,1:maxj)=frac_read(:,:)
      tice(1:maxi,1:maxj)=temp_read(:,:)
      albice(1:maxi,1:maxj)=albd_read(:,:)

c AY (06/10/04) : copied from inm_seaice.f

c     AY (08/03/04) : write out averages for restart checks
      if (debug_init) 
     &    write(*,320) 'Avg height','Avg area', 'Avg T', 'Avg albedo'
c     
c     Clear temporary variables
      do l=1,4
         tmp_val(l) = 0
      enddo
      icell = 0
c     
c     Sum layer state variables and flow field
      do j=1,jmax
         do i=1,imax
c AY (23/09/04) : if statement modified to only consider cells with ice
c           if(k1(i,j).le.kmax) then
            if(k1(i,j).le.kmax.and.varice(2,i,j).gt.0.) then
               icell = icell + 1
               do l=1,2
                  tmp_val(l) = tmp_val(l) + varice(l,i,j)
               enddo
               tmp_val(3) = tmp_val(3) + tice(i,j)
               tmp_val(4) = tmp_val(4) + albice(i,j)
            endif
         enddo
      enddo
c
c     Print average values out
      if (icell.gt.0) then
         if (debug_init) write(*,310) tmp_val(1)/icell,tmp_val(2)/icell,
     :        tmp_val(3)/icell,tmp_val(4)/icell
      else
         if (debug_init) write(*,310) 0.0,0.0,0.0,0.0
      endif
      
 310  format(4f13.9)
 320  format(4a13)
    
      return
      end
