
c this subroutine is based on 'rmsnorm_goldstein_S.F' (which reused
c fragments from 'genie-main/src/fortran/genie_ea_go_gs.f90' in adapted
c form). Analogous to the computation of the RMS error score from
c prevously produced model output by the subroutines in
c 'rmsnorm_goldstein_S.F', this subroutine computes and returns various
c diagnostics from such output

c returned diagnostics:
c
c  - mean_S:           mean S (individual points)
c  - mean_S_vol:       mean S (volume weighted)
c  - var_S:            variance of S (individual points) 
c  - var_S_vol:        variance of S (volume weighted)
c  - mean_Sobs:        mean Sobs (individual points)
c  - mean_Sobs_vol:    mean Sobs (volume weighted)
c  - var_Sobs:         variance of Sobs (individual points) 
c  - var_Sobs_vol:     variance of Sobs (volume weighted)
c  - rmsnorm_S:        RMS model-data difference normalised by number of individual points and variance of data-based field (individual points)
c  - rmsnorm_S_vol:    RMS model-data difference normalised by number of individual points and variance of data-based field (volume weighted)
c  - n:                number of grid cells
c
      subroutine diag_goldstein_S_annual_av(yearstr,mean_S,mean_S_vol
     $     ,var_S,var_S_vol,mean_Sobs,mean_Sobs_vol ,var_Sobs
     $     ,var_Sobs_vol,rmsnorm_S,rmsnorm_S_vol,n)
      
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
      real rmsnorm_S,rmsnorm_S_vol
      real mean_S,mean_S_vol ,var_S,var_S_vol
      real mean_Sobs,mean_Sobs_vol ,var_Sobs,var_Sobs_vol
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

      call read_gold_target_field(2, k1, imax, jmax, kmax, indir_name,
     $     lenin,sdatafile, lentdata, sdata_scaling, sdata_offset,
     $     tsinterp,sdata_varname, sdata_missing, lon, lat, depth,
     $     obsdata)

      n = 0
      vol = 0.0
      mean_S = 0.0
      mean_S_vol = 0.0
      var_S = 0.0
      var_S_vol = 0.0
      nobs = 0
      mean_Sobs = 0.0
      mean_Sobs_vol = 0.0
      var_Sobs = 0.0
      var_Sobs_vol = 0.0
      do k=1,kmax
         do j=1,jmax
            do  i=1,imax
               if(k.ge.k1(i,j))then
                  n = n + 1
                  vol = vol + dphi*ds(j)*dz(k)
                  mean_S = mean_S + modeldata2(i,j,k)
                  mean_S_vol = mean_S_vol + modeldata2(i,j,k)*dphi*ds(j)
     $                 *dz(k)
                  var_S = var_S + modeldata2(i,j,k)**2.0
                  var_S_vol = var_S_vol + modeldata2(i,j,k)**2.0*dphi
     $                 *ds(j)*dz(k)
               endif
               if(obsdata(i,j,k).gt.-9e19) then
                  nobs = nobs+1
                  volobs = volobs + dphi*ds(j)*dz(k)
                  mean_Sobs = mean_Sobs + obsdata(i,j,k)
                  mean_Sobs_vol = mean_Sobs_vol + obsdata(i,j,k)*dphi
     $                 *ds(j)*dz(k)
                  var_Sobs = var_Sobs + obsdata(i,j,k)**2.0
                  var_Sobs_vol = var_Sobs_vol + obsdata(i,j,k)**2.0*dphi
     $                 *ds(j)*dz(k)
               endif
            enddo
         enddo
      enddo
      mean_S = mean_S/n
      mean_S_vol = mean_S_vol/vol
      var_S = var_S/n - mean_S*mean_S
      var_S_vol = var_S_vol/vol - mean_S_vol*mean_S_vol
      mean_Sobs = mean_Sobs/nobs
      mean_Sobs_vol = mean_Sobs_vol/volobs
      var_Sobs = var_Sobs/n - mean_Sobs*mean_Sobs
      var_Sobs_vol = var_Sobs_vol/vol - mean_Sobs_vol*mean_Sobs_vol
      nobs = 0
      rmsnorm_S = 0.0
      rmsnorm_S_vol = 0.0
      weight = 1.0/var_Sobs
      weight_vol = 1.0/var_Sobs_vol
      nobs = 0
      volobs = 0.0
      do k=1,kmax
         do j=1,jmax
            do  i=1,imax
               if ((k.ge.k1(i,j)).and.(obsdata(i,j,k).gt.-9e19)) then
                  nobs = nobs+1
                  volobs = volobs+dphi*ds(j)*dz(k)
                  rmsnorm_S = rmsnorm_S + weight*(modeldata2(i,j,k) -
     $                 obsdata(i,j,k))**2
                  rmsnorm_S_vol = rmsnorm_S_vol + weight_vol*
     $                 (modeldata2(i,j,k)-obsdata(i,j,k))**2*dphi*ds(j)
     $                 *dz(k)
               endif
            enddo
         enddo
      enddo
      rmsnorm_S = sqrt(rmsnorm_S/nobs)
      rmsnorm_S_vol = sqrt(rmsnorm_S_vol/volobs)

      end
