
c this subroutine is based on 'rmsnorm_embm_q.F' (which reused
c fragments from 'genie-main/src/fortran/genie_ea_go_gs.f90' in adapted
c form). Analogous to the computation of the RMS error score from
c prevously produced model output by the subroutines in
c 'rmsnorm_embm_q.F', this subroutine computes and returns
c various diagnostics from such output

c returned diagnostics:
c
c  - mean_q:             mean q (individual points)
c  - mean_q_area:        mean q (area weighted)
c  - var_q:              variance of q (individual points) 
c  - var_q_area:         variance of q (area weighted)
c  - mean_qobs:          mean qobs (individual points)
c  - mean_qobs_area:     mean qobs (area weighted)
c  - var_qobs:           variance of qobs (individual points) 
c  - var_qobs_area:      variance of qobs (area weighted)
c  - rmsnorm_q:          RMS model-data difference normalised by number of individual points and variance of data-based field (individual points)
c  - rmsnorm_q_area:     RMS model-data difference normalised by number of individual points and variance of data-based field (area weighted)
c  - n:                  number of grid cells
c
      subroutine diag_embm_q_annual_av(yearstr,mean_q
     $     ,mean_q_area,var_q,var_q_area,mean_qobs
     $     ,mean_qobs_area ,var_qobs,var_qobs_area
     $     ,rmsnorm_q,rmsnorm_q_area,n)
      
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

c Data scaling for EMBM data
c This is a hard coded scaling factor in the NetCDF-writing routine
c that need to be reverted in order to match the equivalent definitions
c used in err_embm.F
      real humscale
      parameter(humscale=1.0e3)

      real obsdata(maxi,maxj)

c diagnostics
      real rmsnorm_q,rmsnorm_q_area
      real mean_q,mean_q_area ,var_q,var_q_area
      real mean_qobs,mean_qobs_area ,var_qobs,var_qobs_area
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
      call get3d_data_nc(ncid, 'dry_air_humidity', imax, jmax, 1,
     $     modeldata, status)
      if (status .ne. NF_NOERR) call check_err(status)
      status=nf_close(ncid)
      if (status .ne. NF_NOERR) call check_err(status)
c     Transform the data from the NetCDF file back to the model
c     representation
      modeldata=modeldata/humscale

      call read_embm_target_field(2, imax, jmax, indir_name, lenin,
     $     qdatafile, lenqdata, qdata_scaling, qdata_offset, tqinterp
     $     ,qdata_varname, qdata_missing, lon, lat, obsdata)

      n = 0
      area = 0.0
      mean_q = 0.0
      mean_q_area = 0.0
      var_q = 0.0
      var_q_area = 0.0
      nobs = 0
      mean_qobs = 0.0
      mean_qobs_area = 0.0
      var_qobs = 0.0
      var_qobs_area = 0.0
      do j=1,jmax
         do  i=1,imax
            n = n + 1
            area = area + dphi*ds(j)
            mean_q = mean_q + modeldata(i,j,1)
            mean_q_area = mean_q_area + modeldata(i,j,1)*dphi
     $           *ds(j)
            var_q = var_q + modeldata(i,j,1)**2.0
            var_q_area = var_q_area + modeldata(i,j,1)**2.0
     $           *dphi*ds(j)
            if(obsdata(i,j).gt.-9e19) then
               nobs = nobs+1
               areaobs = areaobs + dphi*ds(j)
               mean_qobs = mean_qobs + obsdata(i,j)
               mean_qobs_area = mean_qobs_area + obsdata(i,j)*dphi*ds(j)
               var_qobs = var_qobs + obsdata(i,j)**2.0
               var_qobs_area = var_qobs_area + obsdata(i,j)**2.0*dphi
     $              *ds(j)
            endif
         enddo
      enddo
      mean_q = mean_q/n
      mean_q_area = mean_q_area/area
      var_q = var_q/n - mean_q*mean_q
      var_q_area = var_q_area/area - mean_q_area
     $     *mean_q_area
      if (qdata_rhum) then
         mean_qobs = 0.0
         mean_qobs_area = 0.0
         var_qobs = 0.0
         var_qobs_area = 0.0
      else
         mean_qobs = mean_qobs/nobs
         mean_qobs_area = mean_qobs_area/areaobs
         var_qobs = var_qobs/nobs - mean_qobs*mean_qobs
         var_qobs_area = var_qobs_area/areaobs  -
     $        mean_qobs_area*mean_qobs_area
      endif
      weight = 1.0/var_qobs
      weight_area = 1.0/var_qobs_area
      nobs = 0
      areaobs = 0.0
      do j=1,jmax
         do  i=1,imax
            if (obsdata(i,j).gt.-9e19) then
               nobs = nobs+1
               areaobs = areaobs+dphi*ds(j)
               rmsnorm_q = rmsnorm_q + weight*(modeldata(i,j,1)
     $              -obsdata(i,j))**2
               rmsnorm_q_area = rmsnorm_q_area + weight_area*
     $              (modeldata(i,j,1)-obsdata(i,j))**2*dphi*ds(j) 
            endif
         enddo
      enddo
      if (qdata_rhum) then
         rmsnorm_q = 0.0
         rmsnorm_q_area = 0.0
      else
         rmsnorm_q = sqrt(rmsnorm_q/nobs)
         rmsnorm_q_area = sqrt(rmsnorm_q_area/areaobs)
      endif
      
      end
