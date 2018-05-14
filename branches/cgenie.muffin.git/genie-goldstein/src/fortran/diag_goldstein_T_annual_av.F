
c this subroutine is based on 'rmsnorm_goldstein_T.F' (which reused
c fragments from 'genie-main/src/fortran/genie_ea_go_gs.f90' in adapted
c form). Analogous to the computation of the RMS error score from
c prevously produced model output by the subroutines in
c 'rmsnorm_goldstein_T.F', this subroutine computes and returns
c various diagnostics from such output

c returned diagnostics:
c
c  - mean_T:             mean T (individual points)
c  - mean_T_vol:         mean T (volume weighted)
c  - var_T:              variance of T (individual points) 
c  - var_T_vol:          variance of T (volume weighted)
c  - mean_Tobs:          mean Tobs (individual points)
c  - mean_Tobs_vol:      mean Tobs (volume weighted)
c  - var_Tobs:           variance of Tobs (individual points) 
c  - var_Tobs_vol:       variance of Tobs (volume weighted)
c  - rmsnorm_T:          RMS model-data difference normalised by number of individual points and variance of data-based field (individual points)
c  - rmsnorm_T_vol:      RMS model-data difference normalised by number of individual points and variance of data-based field (volume weighted)
c  - n:                  number of grid cells
c
      subroutine diag_goldstein_T_annual_av(yearstr,mean_T,mean_T_vol
     $     ,var_T,var_T_vol,mean_Tobs,mean_Tobs_vol,var_Tobs
     $     ,var_Tobs_vol,rmsnorm_T,rmsnorm_T_vol,n)
      
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

      real obsdata(maxi,maxj,maxk)

c diagnostics
      real rmsnorm_T,rmsnorm_T_vol
      real mean_T,mean_T_vol ,var_T,var_T_vol
      real mean_Tobs,mean_Tobs_vol ,var_Tobs,var_Tobs_vol
      real vol
      real volobs
      real weight,weight_vol
      integer n,nobs

      integer i,j,k

c     axes
      real lon(maxi),lat(maxj),depth(maxk)

c ------------------------------------------------------------ c
c INITIALIZE VARIABLES
c ------------------------------------------------------------ c
      volobs = 0.0
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
         modeldata2(:,:,k)=modeldata1(:,:,kmax-k+1,1)
      end do

      call read_gold_target_field(1, k1, imax, jmax, kmax, indir_name,
     $     lenin,tdatafile, lentdata, tdata_scaling, tdata_offset,
     $     tsinterp,tdata_varname, tdata_missing, lon, lat, depth,
     $     obsdata)

      n = 0
      vol = 0.0
      mean_T = 0.0
      mean_T_vol = 0.0
      var_T = 0.0
      var_T_vol = 0.0
      nobs = 0
      mean_Tobs = 0.0
      mean_Tobs_vol = 0.0
      var_Tobs = 0.0
      var_Tobs_vol = 0.0
      do k=1,kmax
         do j=1,jmax
            do  i=1,imax
               if(k.ge.k1(i,j))then
                  n = n + 1
                  vol = vol + dphi*ds(j)*dz(k)
                  mean_T = mean_T + modeldata2(i,j,k)
                  mean_T_vol = mean_T_vol + modeldata2(i,j,k)*dphi*ds(j)
     $                 *dz(k)
                  var_T = var_T + modeldata2(i,j,k)**2.0
                  var_T_vol = var_T_vol + modeldata2(i,j,k)**2.0*dphi
     $                 *ds(j)*dz(k)
               endif
               if(obsdata(i,j,k).gt.-9e19) then
                  nobs = nobs+1
                  volobs = volobs + dphi*ds(j)*dz(k)
                  mean_Tobs = mean_Tobs + obsdata(i,j,k)
                  mean_Tobs_vol = mean_Tobs_vol + obsdata(i,j,k)*dphi
     $                 *ds(j)*dz(k)
                  var_Tobs = var_Tobs + obsdata(i,j,k)**2.0
                  var_Tobs_vol = var_Tobs_vol + obsdata(i,j,k)**2.0*dphi
     $                 *ds(j)*dz(k)
               endif
            enddo
         enddo
      enddo
      mean_T = mean_T/n
      mean_T_vol = mean_T_vol/vol
      var_T = var_T/n - mean_T*mean_T
      var_T_vol = var_T_vol/vol - mean_T_vol*mean_T_vol
      mean_Tobs = mean_Tobs/nobs
      mean_Tobs_vol = mean_Tobs_vol/volobs
      var_Tobs = var_Tobs/nobs - mean_Tobs*mean_Tobs
      var_Tobs_vol = var_Tobs_vol/volobs  - mean_Tobs_vol*mean_Tobs_vol
      rmsnorm_T = 0.0
      rmsnorm_T_vol = 0.0
      weight = 1.0/var_Tobs
      weight_vol = 1.0/var_Tobs_vol
      nobs = 0
      volobs = 0.0
      do k=1,kmax
         do j=1,jmax
            do  i=1,imax
               if ((k.ge.k1(i,j)).and.(obsdata(i,j,k).gt.-9e19)) then
                  nobs = nobs+1
                  volobs = volobs+dphi*ds(j)*dz(k)
                  rmsnorm_T = rmsnorm_T + weight*(modeldata2(i,j,k) -
     $                 obsdata(i,j,k))**2
                  rmsnorm_T_vol = rmsnorm_T_vol + weight_vol*
     $                 (modeldata2(i,j,k)-obsdata(i,j,k))**2*dphi*ds(j)
     $                 *dz(k)
               endif
            enddo
         enddo
      enddo
      rmsnorm_T = sqrt(rmsnorm_T/nobs)
      rmsnorm_T_vol = sqrt(rmsnorm_T_vol/volobs)

      end
