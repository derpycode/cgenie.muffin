
c subroutine to produce a normalised (by the variance of the dataset
c used) rms difference between model salinity data and a salinity
c dataset according to the source code of
c 'genie-main/src/fortran/genie_ea_go_gs.f90'. The actual computation of
c the normalised RMS is done by the function 'err_gold(...)'. Code
c fragments from 'genie-main/src/fortran/genie_ea_go_gs.f90' have been
c reused in adapted form here.

      subroutine rmsnorm_goldstein_S(yearstr,rmsnorm,nobs)
      
#include "ocean.cmn"
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

      real modeldata1(maxi,maxj,maxk,1), modeldata2(maxi,maxj,maxk)

      real rmsnorm, err_gold

      integer nobs

      integer i,j,k

c     axes
      real lon(maxi),lat(maxj),depth(maxk)
      
      do i=1,imax
         lon(i)=180.0*(phi0+(i-0.5)*dphi)/pi
      enddo
      do j=1,jmax
         lat(j)=180.0*asin(s(j))/pi
      enddo
      do k=1,kmax
         depth(k)=abs(zro(kmax+1-k)*dsc)
      enddo

c     Retrieve previously written annual average fields from the
c     GOLDSTEIN NetCDF output for specified output year
      model_datafile='gold_'//lout//'_av_'//yearstr//'.nc'
      model_lendatafile=lnsig1(model_datafile)
      filename=trim(outdir_name(1:lenout))
     $     //trim(model_datafile(1:model_lendatafile))
      print*,'GOLD model data file: ',filename
      status=nf_open(trim(filename), 0, ncid)
      IF (status .ne. NF_NOERR) call check_err(status)
      call get4d_data_nc(ncid, 'salinity', imax, jmax, kmax, 1,
     $     modeldata1,status)
      IF (status .ne. NF_NOERR) call check_err(status)
      status=nf_close(ncid)
      IF (status .ne. NF_NOERR) call check_err(status)
c     Transform the data from the NetCDF file back to the model
c     representation
      do k=1,kmax
         modeldata2(:,:,k)=modeldata1(:,:,kmax-k+1,1)
      end do
      modeldata2=modeldata2-saln0

      nobs = 0
      rmsnorm = err_gold(modeldata2, 2, k1, nobs, imax, jmax, kmax,
     $     indir_name, lenin, sdatafile, lensdata, sdata_scaling,
     $     sdata_offset+saln0, tsinterp, sdata_varname, sdata_missing,
     $     lon, lat, depth)
      
      end

