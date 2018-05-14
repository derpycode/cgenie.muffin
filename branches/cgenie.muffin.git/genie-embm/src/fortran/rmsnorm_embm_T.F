
c subroutine to produce a normalised (by the variance of the dataset
c used) rms difference between model temperature data and a temperature
c dataset according to the source code of
c 'genie-main/src/fortran/genie_ea_go_gs.f90'. The actual computation of
c the normalised RMS is done by the function 'err_embm(...)'. Code
c fragments from 'genie-main/src/fortran/genie_ea_go_gs.f90' have been
c reused in adapted form here.

      subroutine rmsnorm_embm_T(yearstr,rmsnorm,nobs)

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

      real modeldata(maxi,maxj,1)

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

      rmsnorm = err_embm(modeldata(:,:,1), 1, imax, jmax, indir_name,
     $     lenin,tdatafile, lentdata, tdata_scaling, tdata_offset,
     $     tqinterp,tdata_varname, tdata_missing, lon, lat)
      nobs = imax*jmax
      
      end
