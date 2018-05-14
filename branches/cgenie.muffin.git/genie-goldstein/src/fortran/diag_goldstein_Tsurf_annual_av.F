
c this subroutine is based on 'rmsnorm_goldstein_T.F' (which reused
c fragments from 'genie-main/src/fortran/genie_ea_go_gs.f90' in adapted
c form). Analogous to the computation of the RMS error score from
c prevously produced model output by the subroutines in
c 'rmsnorm_goldstein_T.F', this subroutine computes and returns
c various diagnostics from such output

c returned diagnostics:
c
c  - mean_Tsurf:             mean Tsurf (individual points)
c  - mean_Tsurf_area:        mean Tsurf (area weighted)
c  - var_Tsurf:              variance of Tsurf (individual points) 
c  - var_Tsurf_area:         variance of Tsurf (area weighted)
c  - mean_Tsurfobs:          mean Tsurfobs (individual points)
c  - mean_Tsurfobs_area:     mean Tsurfobs (area weighted)
c  - var_Tsurfobs:           variance of Tsurfobs (individual points) 
c  - var_Tsurfobs_area:      variance of Tsurfobs (area weighted)
c  - rmsnorm_Tsurf:          RMS model-data difference normalised by number of individual points and variance of data-based field (individual points)
c  - rmsnorm_Tsurf_area:     RMS model-data difference normalised by number of individual points and variance of data-based field (area weighted)
c  - n:                      number of grid cells
c
      subroutine diag_goldstein_Tsurf_annual_av(yearstr,mean_Tsurf
     $     ,mean_Tsurf_area,var_Tsurf,var_Tsurf_area,mean_Tsurfobs
     $     ,mean_Tsurfobs_area,var_Tsurfobs,var_Tsurfobs_area
     $     ,rmsnorm_Tsurf,rmsnorm_Tsurf_area,n)
      
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
      integer lnsig1

      real modeldata1(maxi,maxj,maxk,1), modeldata2(maxi,maxj)

      real obsdata(maxi,maxj,maxk)

c diagnostics
      real rmsnorm_Tsurf,rmsnorm_Tsurf_area
      real mean_Tsurf,mean_Tsurf_area ,var_Tsurf,var_Tsurf_area
      real mean_Tsurfobs,mean_Tsurfobs_area ,var_Tsurfobs,var_Tsurfobs_area
      real area
      real areaobs
      real weight,weight_area
      integer n,nobs

      integer i,j,k

c     axes
      real lon(maxi),lat(maxj),depth(maxk)

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
      call get4d_data_nc(ncid, 'temp', imax, jmax, kmax, 1,
     $     modeldata1,status)
      IF (status .ne. NF_NOERR) call check_err(status)
      status=nf_close(ncid)
      IF (status .ne. NF_NOERR) call check_err(status)
c     Transform the data from the NetCDF file back to the model
c     representation
      do k=1,kmax
         modeldata2(:,:)=modeldata1(:,:,1,1)
      end do

      call read_gold_target_field(1, k1, imax, jmax, kmax, indir_name,
     $     lenin,tdatafile, lentdata, tdata_scaling, tdata_offset,
     $     tsinterp,tdata_varname, tdata_missing, lon, lat, depth,
     $     obsdata)

      n = 0
      area = 0.0
      mean_Tsurf = 0.0
      mean_Tsurf_area = 0.0
      var_Tsurf = 0.0
      var_Tsurf_area = 0.0
      nobs = 0
      mean_Tsurfobs = 0.0
      mean_Tsurfobs_area = 0.0
      var_Tsurfobs = 0.0
      var_Tsurfobs_area = 0.0
      do j=1,jmax
         do  i=1,imax
            if(k1(i,j).le.kmax)then
               n = n + 1
               area = area + dphi*ds(j)
               mean_Tsurf = mean_Tsurf + modeldata2(i,j)
               mean_Tsurf_area = mean_Tsurf_area + modeldata2(i,j)*dphi
     $              *ds(j)
               var_Tsurf = var_Tsurf + modeldata2(i,j)**2.0
               var_Tsurf_area = var_Tsurf_area + modeldata2(i,j)**2.0
     $              *dphi*ds(j)
            endif
            if(obsdata(i,j,kmax).gt.-9e19) then
               nobs = nobs+1
               areaobs = areaobs + dphi*ds(j)
               mean_Tsurfobs = mean_Tsurfobs + obsdata(i,j,kmax)
               mean_Tsurfobs_area = mean_Tsurfobs_area + obsdata(i,j
     $              ,kmax)*dphi*ds(j)
               var_Tsurfobs = var_Tsurfobs + obsdata(i,j,kmax)**2.0
               var_Tsurfobs_area = var_Tsurfobs_area + obsdata(i,j,kmax)
     $              **2.0*dphi*ds(j)
            endif
         enddo
      enddo
      mean_Tsurf = mean_Tsurf/n
      mean_Tsurf_area = mean_Tsurf_area/area
      var_Tsurf = var_Tsurf/n - mean_Tsurf*mean_Tsurf
      var_Tsurf_area = var_Tsurf_area/area - mean_Tsurf_area
     $     *mean_Tsurf_area
      mean_Tsurfobs = mean_Tsurfobs/nobs
      mean_Tsurfobs_area = mean_Tsurfobs_area/areaobs
      var_Tsurfobs = var_Tsurfobs/nobs - mean_Tsurfobs*mean_Tsurfobs
      var_Tsurfobs_area = var_Tsurfobs_area/areaobs  -
     $     mean_Tsurfobs_area*mean_Tsurfobs_area
      weight = 1.0/var_Tsurfobs
      weight_area = 1.0/var_Tsurfobs_area
      nobs = 0
      areaobs = 0.0
      do j=1,jmax
         do  i=1,imax
            if ((k1(i,j).le.kmax).and.(obsdata(i,j,kmax).gt.-9e19)) then
               nobs = nobs+1
               areaobs = areaobs+dphi*ds(j)
               rmsnorm_Tsurf = rmsnorm_Tsurf + weight*(modeldata2(i,j)
     $              -obsdata(i,j,kmax))**2
               rmsnorm_Tsurf_area = rmsnorm_Tsurf_area + weight_area*
     $              (modeldata2(i,j)-obsdata(i,j,kmax))**2*dphi*ds(j) 
            endif
         enddo
      enddo
      rmsnorm_Tsurf = sqrt(rmsnorm_Tsurf/nobs)
      rmsnorm_Tsurf_area = sqrt(rmsnorm_Tsurf_area/areaobs)
      
      end
