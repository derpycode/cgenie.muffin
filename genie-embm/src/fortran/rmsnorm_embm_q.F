

c subroutine to produce a normalised (by the variance of the dataset
c used) rms difference between model humidity data and a humidity
c dataset according to the source code of
c 'genie-main/src/fortran/genie_ea_go_gs.f90'. The actual computation of
c the normalised RMS is done by the function 'err_embm(...)'. Code
c fragments from 'genie-main/src/fortran/genie_ea_go_gs.f90' have been
c reused in adapted form here.

      subroutine rmsnorm_embm_q(yearstr,rmsnorm,nobs)
      
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
      integer            :: lnsig1

      real modeldata1(maxi,maxj,1), modeldata2(maxi,maxj,1)

c Data scaling for EMBM data
c This is a hard coded scaling factors in the NetCDF-writing routine
c that need to be reverted in order to match the equivalent definitions
c used in err_embm.F
      real humscale
      parameter(humscale=1.0e3)

      real rmsnorm, err_embm

      integer nobs

      integer i,j

c     axes
      real lon(maxi),lat(maxj)
      
      do i=1,imax
         lon(i)=180.0*(phi0+(i-0.5)*dphi)/pi
      enddo
      do j=1,jmax
         lat(j)=180.0*asin(s(j))/pi
      enddo

      model_datafile='embm_'//lout//'_av_'//yearstr//'.nc'
      model_lendatafile=lnsig1(model_datafile)
      filename=trim(outdir_name(1:lenout))
     $     //trim(model_datafile(1:model_lendatafile))
      print*,'EMBM model data file: ',trim(filename)
      status=nf_open(trim(filename), 0, ncid)
      if (status .ne. NF_NOERR) call check_err(status)
      call get3d_data_nc(ncid, 'dry_air_humidity', imax, jmax, 1,
     $     modeldata1, status)
      if (status .ne. NF_NOERR) call check_err(status)
      call get3d_data_nc(ncid, 'dry_air_relative_humidity', imax, jmax,
     $     1, modeldata2, status)
      if (status .ne. NF_NOERR) call check_err(status)
      status=nf_close(ncid)
      if (status .ne. NF_NOERR) call check_err(status)
c     Transform the data from the NetCDF file back to the model
c     representation
      modeldata1=modeldata1/humscale

      if (qdata_rhum) then
         rmsnorm = err_embm(modeldata2(:,:,1), 2, imax, jmax, indir_name,
     $        lenin, qdatafile, lenqdata, qdata_scaling, qdata_offset,
     $        tqinterp, qdata_varname, qdata_missing, lon, lat)
      else
         rmsnorm = err_embm(modeldata1(:,:,1), 2, imax, jmax, indir_name,
     $        lenin, qdatafile, lenqdata, qdata_scaling, qdata_offset,
     $        tqinterp, qdata_varname, qdata_missing, lon, lat)
      endif
      nobs = imax*jmax
      
      end
