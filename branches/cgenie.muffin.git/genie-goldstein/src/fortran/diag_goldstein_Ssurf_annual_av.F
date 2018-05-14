
c this subroutine is based on 'rmsnorm_goldstein_S.F' (which reused
c fragments from 'genie-main/src/fortran/genie_ea_go_gs.f90' in adapted
c form). Analogous to the computation of the RMS error score from
c prevously produced model output by the subroutines in
c 'rmsnorm_goldstein_S.F', this subroutine computes and returns
c various diagnostics from such output

c returned diagnostics:
c
c  - mean_Ssurf:             mean Ssurf (individual points)
c  - mean_Ssurf_area:        mean Ssurf (area weighted)
c  - var_Ssurf:              variance of Ssurf (individual points) 
c  - var_Ssurf_area:         variance of Ssurf (area weighted)
c  - mean_Ssurfobs:          mean Ssurfobs (individual points)
c  - mean_Ssurfobs_area:     mean Ssurfobs (area weighted)
c  - var_Ssurfobs:           variance of Ssurfobs (individual points) 
c  - var_Ssurfobs_area:      variance of Ssurfobs (area weighted)
c  - rmsnorm_Ssurf:          RMS model-data difference normalised by number of individual points and variance of data-based field (individual points)
c  - rmsnorm_Ssurf_area:     RMS model-data difference normalised by number of individual points and variance of data-based field (area weighted)
c  - n:                      number of grid cells
c
      subroutine diag_goldstein_Ssurf_annual_av(yearstr,mean_Ssurf
     $     ,mean_Ssurf_area,var_Ssurf,var_Ssurf_area,mean_Ssurfobs
     $     ,mean_Ssurfobs_area ,var_Ssurfobs,var_Ssurfobs_area
     $     ,rmsnorm_Ssurf,rmsnorm_Ssurf_area,n)
      
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
      real rmsnorm_Ssurf,rmsnorm_Ssurf_area
      real mean_Ssurf,mean_Ssurf_area ,var_Ssurf,var_Ssurf_area
      real mean_Ssurfobs,mean_Ssurfobs_area ,var_Ssurfobs,var_Ssurfobs_area
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
      call get4d_data_nc(ncid, 'salinity', imax, jmax, kmax, 1,
     $     modeldata1,status)
      IF (status .ne. NF_NOERR) call check_err(status)
      status=nf_close(ncid)
      IF (status .ne. NF_NOERR) call check_err(status)
c     Transform the data from the NetCDF file back to the model
c     representation
      do k=1,kmax
         modeldata2(:,:)=modeldata1(:,:,1,1)
      end do

      call read_gold_target_field(2, k1, imax, jmax, kmax, indir_name,
     $     lenin,sdatafile, lensdata, sdata_scaling, sdata_offset,
     $     tsinterp,sdata_varname, sdata_missing, lon, lat, depth,
     $     obsdata)

      n = 0
      area = 0.0
      mean_Ssurf = 0.0
      mean_Ssurf_area = 0.0
      var_Ssurf = 0.0
      var_Ssurf_area = 0.0
      nobs = 0
      mean_Ssurfobs = 0.0
      mean_Ssurfobs_area = 0.0
      var_Ssurfobs = 0.0
      var_Ssurfobs_area = 0.0
      do j=1,jmax
         do  i=1,imax
            if(k1(i,j).le.kmax)then
               n = n + 1
               area = area + dphi*ds(j)
               mean_Ssurf = mean_Ssurf + modeldata2(i,j)
               mean_Ssurf_area = mean_Ssurf_area + modeldata2(i,j)*dphi
     $              *ds(j)
               var_Ssurf = var_Ssurf + modeldata2(i,j)**2.0
               var_Ssurf_area = var_Ssurf_area + modeldata2(i,j)**2.0
     $              *dphi*ds(j)
            endif
            if(obsdata(i,j,kmax).gt.-9e19) then
               nobs = nobs+1
               areaobs = areaobs + dphi*ds(j)
               mean_Ssurfobs = mean_Ssurfobs + obsdata(i,j,kmax)
               mean_Ssurfobs_area = mean_Ssurfobs_area + obsdata(i,j
     $              ,kmax)*dphi*ds(j)
               var_Ssurfobs = var_Ssurfobs + obsdata(i,j,kmax)**2.0
               var_Ssurfobs_area = var_Ssurfobs_area + obsdata(i,j,kmax)
     $              **2.0*dphi*ds(j)
            endif
         enddo
      enddo
      mean_Ssurf = mean_Ssurf/n
      mean_Ssurf_area = mean_Ssurf_area/area
      var_Ssurf = var_Ssurf/n - mean_Ssurf*mean_Ssurf
      var_Ssurf_area = var_Ssurf_area/area - mean_Ssurf_area
     $     *mean_Ssurf_area
      mean_Ssurfobs = mean_Ssurfobs/nobs
      mean_Ssurfobs_area = mean_Ssurfobs_area/areaobs
      var_Ssurfobs = var_Ssurfobs/nobs - mean_Ssurfobs*mean_Ssurfobs
      var_Ssurfobs_area = var_Ssurfobs_area/areaobs  -
     $     mean_Ssurfobs_area*mean_Ssurfobs_area
      weight = 1.0/var_Ssurfobs
      weight_area = 1.0/var_Ssurfobs_area
      nobs = 0
      areaobs = 0.0
      do j=1,jmax
         do  i=1,imax
            if ((k1(i,j).le.kmax).and.(obsdata(i,j,kmax).gt.-9e19)) then
               nobs = nobs+1
               areaobs = areaobs+dphi*ds(j)
               rmsnorm_Ssurf = rmsnorm_Ssurf + weight*(modeldata2(i,j)
     $              -obsdata(i,j,kmax))**2
               rmsnorm_Ssurf_area = rmsnorm_Ssurf_area + weight_area*
     $              (modeldata2(i,j)-obsdata(i,j,kmax))**2*dphi*ds(j) 
            endif
         enddo
      enddo
      rmsnorm_Ssurf = sqrt(rmsnorm_Ssurf/nobs)
      rmsnorm_Ssurf_area = sqrt(rmsnorm_Ssurf_area/areaobs)
      
      end
