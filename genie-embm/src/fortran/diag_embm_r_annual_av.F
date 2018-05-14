
c this subroutine is based on 'rmsnorm_embm_q.F' (which reused
c fragments from 'genie-main/src/fortran/genie_ea_go_gs.f90' in adapted
c form). Analogous to the computation of the RMS error score from
c prevously produced model output by the subroutines in
c 'rmsnorm_embm_q.F', this subroutine computes and returns
c various diagnostics from such output

c returned diagnostics:
c
c  - mean_r:             mean r (individual points)
c  - mean_r_area:        mean r (area weighted)
c  - var_r:              variance of r (individual points) 
c  - var_r_area:         variance of r (area weighted)
c  - mean_robs:          mean robs (individual points)
c  - mean_robs_area:     mean robs (area weighted)
c  - var_robs:           variance of robs (individual points) 
c  - var_robs_area:      variance of robs (area weighted)
c  - rmsnorm_r:          RMS model-data difference normalised by number of individual points and variance of data-based field (individual points)
c  - rmsnorm_r_area:     RMS model-data difference normalised by number of individual points and variance of data-based field (area weighted)
c  - n:                  number of grid cells
c
      subroutine diag_embm_r_annual_av(yearstr,mean_r
     $     ,mean_r_area,var_r,var_r_area,mean_robs
     $     ,mean_robs_area ,var_robs,var_robs_area
     $     ,rmsnorm_r,rmsnorm_r_area,n)
      
#include "embm.cmn"
      include 'netcdf.inc'

      character*13 yearstr

c Model data files
      integer model_lendatafile
      character*200 model_datafile

c NetCDF variables
      integer ncid, status
      character*256 filename

c String length function
      integer lnsig1

      real modeldata(maxi,maxj,1)

      real obsdata(maxi,maxj)

c diagnostics
      real rmsnorm_r,rmsnorm_r_area
      real mean_r,mean_r_area ,var_r,var_r_area
      real mean_robs,mean_robs_area ,var_robs,var_robs_area
      real area
      real areaobs
      real weight,weight_area
      integer n,nobs

      integer i,j

c     axes
      real lon(maxi),lat(maxj)

c ------------------------------------------------------------ c
c INITIALIZE VARIABLES
c ------------------------------------------------------------ c
      areaobs = 0.0
c ------------------------------------------------------------ c

      do i=1,imax
         lon(i)=180.0*(phi0+(i-0.5)*dphi)/pi
      enddo
      do j=1,jmax
         lat(j)=180.0*asin(s(j))/pi
      enddo

c     Retrieve previously written annual average fields from the EMBM
c     NetCDF output for specified output year
      model_datafile='embm_'//lout//'_av_'//yearstr//'.nc'
      model_lendatafile=lnsig1(model_datafile)
      filename=trim(outdir_name(1:lenout))
     $     //trim(model_datafile(1:model_lendatafile))
      print*,'EMBM model data file: ',trim(filename)
      status=nf_open(trim(filename), 0, ncid)
      if (status .ne. NF_NOERR) call check_err(status)
      call get3d_data_nc(ncid, 'dry_air_relative_humidity', imax, jmax,
     $     1,modeldata, status)
      if (status .ne. NF_NOERR) call check_err(status)
      status=nf_close(ncid)
      if (status .ne. NF_NOERR) call check_err(status)
c     Transform the data from the NetCDF file back to the model
c     representation

      call read_embm_target_field(2, imax, jmax, indir_name, lenin,
     $     qdatafile, lenqdata, qdata_scaling, qdata_offset, tqinterp
     $     ,qdata_varname, qdata_missing, lon, lat, obsdata)
      
      n = 0
      area = 0.0
      mean_r = 0.0
      mean_r_area = 0.0
      var_r = 0.0
      var_r_area = 0.0
      nobs = 0
      mean_robs = 0.0
      mean_robs_area = 0.0
      var_robs = 0.0
      var_robs_area = 0.0
      do j=1,jmax
         do  i=1,imax
            n = n + 1
            area = area + dphi*ds(j)
            mean_r = mean_r + modeldata(i,j,1)
            mean_r_area = mean_r_area + modeldata(i,j,1)*dphi
     $           *ds(j)
            var_r = var_r + modeldata(i,j,1)**2.0
            var_r_area = var_r_area + modeldata(i,j,1)**2.0
     $           *dphi*ds(j)
            if(obsdata(i,j).gt.-9e19) then
               nobs = nobs+1
               areaobs = areaobs + dphi*ds(j)
               mean_robs = mean_robs + obsdata(i,j)
               mean_robs_area = mean_robs_area + obsdata(i,j)*dphi*ds(j)
               var_robs = var_robs + obsdata(i,j)**2.0
               var_robs_area = var_robs_area + obsdata(i,j)**2.0*dphi
     $              *ds(j)
            endif
         enddo
      enddo
      mean_r = mean_r/n
      mean_r_area = mean_r_area/area
      var_r = var_r/n - mean_r*mean_r
      var_r_area = var_r_area/area - mean_r_area
     $     *mean_r_area
      if (qdata_rhum) then
         mean_robs = mean_robs/nobs
         mean_robs_area = mean_robs_area/areaobs
         var_robs = var_robs/nobs - mean_robs*mean_robs
         var_robs_area = var_robs_area/areaobs  -
     $     mean_robs_area*mean_robs_area
      else
         mean_robs = -999.9
         mean_robs_area = -999.9
         var_robs = -999.9
         var_robs_area = -999.9
      endif
      weight = 1.0/var_robs
      weight_area = 1.0/var_robs_area
      nobs = 0
      areaobs = 0.0
      do j=1,jmax
         do  i=1,imax
            if (obsdata(i,j).gt.-9e19) then
               nobs = nobs+1
               areaobs = areaobs+dphi*ds(j)
               rmsnorm_r = rmsnorm_r + weight*(modeldata(i,j,1)
     $              -obsdata(i,j))**2
               rmsnorm_r_area = rmsnorm_r_area + weight_area*
     $              (modeldata(i,j,1)-obsdata(i,j))**2*dphi*ds(j) 
            endif
         enddo
      enddo
      if (qdata_rhum) then
         rmsnorm_r = sqrt(rmsnorm_r/nobs)
         rmsnorm_r_area = sqrt(rmsnorm_r_area/areaobs)
      else
         rmsnorm_r = -999.9
         rmsnorm_r_area = -999.9
      endif

      end
