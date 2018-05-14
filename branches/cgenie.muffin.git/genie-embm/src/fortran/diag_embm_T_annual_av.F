
c this subroutine is based on 'rmsnorm_embm_T.F' (which reused
c fragments from 'genie-main/src/fortran/genie_ea_go_gs.f90' in adapted
c form). Analogous to the computation of the RMS error score from
c prevously produced model output by the subroutines in
c 'rmsnorm_embm_T.F', this subroutine computes and returns
c various diagnostics from such output

c returned diagnostics:
c
c  - mean_T:             mean T (individual points)
c  - mean_T_area:        mean T (area weighted)
c  - var_T:              variance of T (individual points) 
c  - var_T_area:         variance of T (area weighted)
c  - mean_Tobs:          mean Tobs (individual points)
c  - mean_Tobs_area:     mean Tobs (area weighted)
c  - var_Tobs:           variance of Tobs (individual points) 
c  - var_Tobs_area:      variance of Tobs (area weighted)
c  - rmsnorm_T:          RMS model-data difference normalised by number of individual points and variance of data-based field (individual points)
c  - rmsnorm_T_area:     RMS model-data difference normalised by number of individual points and variance of data-based field (area weighted)
c  - n:                  number of grid cells
c  - mean_T_SH_area:     mean T in southern hemisphere (area weighted)
c  - mean_T_NH_area:     mean T in northern hemisphere (area weighted)
c
      subroutine diag_embm_T_annual_av(yearstr,mean_T
     $     ,mean_T_area,var_T,var_T_area,mean_Tobs
     $     ,mean_Tobs_area ,var_Tobs,var_Tobs_area
     $     ,rmsnorm_T,rmsnorm_T_area,n,mean_T_SH_area,mean_T_NH_area)
      
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
      real rmsnorm_T,rmsnorm_T_area
      real mean_T,mean_T_area ,var_T,var_T_area
      real mean_Tobs,mean_Tobs_area ,var_Tobs,var_Tobs_area
      real area
      real areaobs
      real weight,weight_area
      integer n,nobs
      real mean_T_SH_area,mean_T_NH_area
      real area_SH,w

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
      call get3d_data_nc(ncid, 'air_temp', imax, jmax, 1, modeldata,
     $     status)
      if (status .ne. NF_NOERR) call check_err(status)
      status=nf_close(ncid)
      if (status .ne. NF_NOERR) call check_err(status)

      call read_embm_target_field(1, imax, jmax, indir_name, lenin,
     $     tdatafile, lentdata, tdata_scaling, tdata_offset, tqinterp
     $     ,tdata_varname, tdata_missing, lon, lat, obsdata)

      n = 0
      area = 0.0
      mean_T = 0.0
      mean_T_area = 0.0
      var_T = 0.0
      var_T_area = 0.0
      nobs = 0
      mean_Tobs = 0.0
      mean_Tobs_area = 0.0
      var_Tobs = 0.0
      var_Tobs_area = 0.0
      mean_T_SH_area = 0.0
      mean_T_NH_area = 0.0
      area_SH = 0.0
      do j=1,jmax
         do  i=1,imax
            n = n + 1
            area = area + dphi*ds(j)
            mean_T = mean_T + modeldata(i,j,1)
            mean_T_area = mean_T_area + modeldata(i,j,1)*dphi
     $           *ds(j)
            var_T = var_T + modeldata(i,j,1)**2.0
            var_T_area = var_T_area + modeldata(i,j,1)**2.0
     $           *dphi*ds(j)
            if(obsdata(i,j).gt.-9e19) then
               nobs = nobs+1
               areaobs = areaobs + dphi*ds(j)
               mean_Tobs = mean_Tobs + obsdata(i,j)
               mean_Tobs_area = mean_Tobs_area + obsdata(i,j)*dphi*ds(j)
               var_Tobs = var_Tobs + obsdata(i,j)**2.0
               var_Tobs_area = var_Tobs_area + obsdata(i,j)**2.0*dphi
     $              *ds(j)
            endif
            if ((sv(j-1)*sv(j)).le.0.0) then
               w = -1.0*sv(j-1)*rds(j)
            elseif (s(j).lt.0) then
               w = 1.0
            else
               w = 0.0
            endif
            mean_T_SH_area = mean_T_SH_area + w*modeldata(i,j,1)*dphi
     $           *ds(j)
            area_SH = area_SH + w*dphi*ds(j)
            mean_T_NH_area = (1.0-w)*mean_T_NH_area + modeldata(i,j,1)
     $           *dphi*ds(j)
         enddo
      enddo
      mean_T = mean_T/n
      mean_T_area = mean_T_area/area
      var_T = var_T/n - mean_T*mean_T
      var_T_area = var_T_area/area - mean_T_area
     $     *mean_T_area
      mean_Tobs = mean_Tobs/nobs
      mean_Tobs_area = mean_Tobs_area/areaobs
      var_Tobs = var_Tobs/nobs - mean_Tobs*mean_Tobs
      var_Tobs_area = var_Tobs_area/areaobs  -
     $     mean_Tobs_area*mean_Tobs_area
      mean_T_SH_area = mean_T_SH_area/area_SH
      mean_T_NH_area = mean_T_NH_area/(area-area_SH)
      weight = 1.0/var_Tobs
      weight_area = 1.0/var_Tobs_area
      nobs = 0
      areaobs = 0.0
      do j=1,jmax
         do  i=1,imax
            if (obsdata(i,j).gt.-9e19) then
               nobs = nobs+1
               areaobs = areaobs+dphi*ds(j)
               rmsnorm_T = rmsnorm_T + weight*(modeldata(i,j,1)
     $              -obsdata(i,j))**2
               rmsnorm_T_area = rmsnorm_T_area + weight_area*
     $              (modeldata(i,j,1)-obsdata(i,j))**2*dphi*ds(j) 
            endif
         enddo
      enddo
      rmsnorm_T = sqrt(rmsnorm_T/nobs)
      rmsnorm_T_area = sqrt(rmsnorm_T_area/areaobs)
      
      end
