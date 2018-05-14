* AY (05/10/04) : reads in netCDF restart files for the EMBM
*                 based upon code written by Dan Lunt for GOLDSTEIN

      subroutine inm_netcdf_embm

c     This module reads netcdf restarts for goldstein sea-ice      

#include "embm.cmn"
      include 'netcdf.inc'

      real temp_read(maxi,maxj)
      real shum_read(maxi,maxj)

c     For date and restarts...
      integer iday

c     For netcdf...
      integer ifail
      integer ncid
      logical lexist
      character*200 fnamein
      
      real timestep

c AY (06/10/04) : extra variables for printing out average model properties
      integer i,j,l
      real    tmp_val(2)
      real    area

c      timestep=24.0*60.0*60.0*yearlen/real(nyear)
      timestep=24.0*60.0*60.0*yearlen/real(nyear*ndta)

c AY (06/10/04) : append input filename to input directory name
c      fnamein=trim(indir_name(1:lenin))//trim(filenetin)
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

         print*,' embm: Opening restart file for read: ',
     &      trim(filenetin)

         call open_file_nc(
     &      trim(fnamein),ncid)
         call get1di_data_nc(ncid,'ioffset',1,ioffset_rest,ifail)
         call get1di_data_nc(ncid,'iyear',1,iyear_rest,ifail)
         call get1di_data_nc(ncid,'imonth',1,imonth_rest,ifail)
         call get1di_data_nc(ncid,'iday',1,iday,ifail)
         call get2d_data_nc(ncid,'air_temp',maxi,maxj,
     &          temp_read,ifail)
         call get2d_data_nc(ncid,'humidity',maxi,maxj,
     &          shum_read,ifail)
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
      print*,'day in embm restart is now',day_rest
      print*,'iday in embm restart is now',iday

      tq(1,1:maxi,1:maxj)=temp_read(:,:)
      tq(2,1:maxi,1:maxj)=shum_read(:,:)

c AY (06/10/04) : copied from inm_embm.f

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

      return
      end
