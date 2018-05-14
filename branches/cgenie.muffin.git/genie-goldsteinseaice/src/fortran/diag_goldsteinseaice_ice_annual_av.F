
c this subroutine is based on 'genie-embm/src/fortram/rmsnorm_embm_q.F'
c (which reused fragments from
c 'genie-main/src/fortran/genie_ea_go_gs.f90' in adapted
c form). Analogous to the computation of the RMS error score from
c prevously produced model output by the subroutines in
c 'rmsnorm_embm_q.F', this subroutine computes and returns various
c diagnostics from such output

c returned diagnostics:
c
c  - ice_vol             ice volume
c  - ice_area            ice area
c  - ice_vol_SH          SH ice volume
c  - ice_area_SH         SH ice area
c  - ice_vol_NH          NH ice volume
c  - ice_area_NH         NH ice area
c
      subroutine diag_goldsteinseaice_ice_annual_av(yearstr,ice_vol
     $     ,ice_area,ice_vol_SH,ice_area_SH,ice_vol_NH,ice_area_NH)
      
#include "seaice.cmn"
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

      real modeldata1(maxi,maxj,1), modeldata2(maxi,maxj,1)

c diagnostics
      real ice_vol ,ice_area,ice_vol_SH,ice_area_SH,ice_vol_NH
     $     ,ice_area_NH

      real w

      integer i,j

c     Retrieve previously written annual average fields from the EMBM
c     NetCDF output for specified output year
      model_datafile='gsic_'//lout//'_av_'//yearstr//'.nc'
      model_lendatafile=lnsig1(model_datafile)
      filename=trim(outdir_name(1:lenout))
     $     //trim(model_datafile(1:model_lendatafile))
      print*,'GOLDSTEIN-seaice model data file: ',trim(filename)
      status=nf_open(trim(filename), 0, ncid)
      if (status .ne. NF_NOERR) call check_err(status)
      call get3d_data_nc(ncid, 'sic_height', imax, jmax,
     $     1,modeldata1, status)
      if (status .ne. NF_NOERR) call check_err(status)
      call get3d_data_nc(ncid, 'sic_cover', imax, jmax,
     $     1,modeldata2, status)
      if (status .ne. NF_NOERR) call check_err(status)
      status=nf_close(ncid)
      if (status .ne. NF_NOERR) call check_err(status)
      
      ice_vol = 0.0
      ice_area = 0.0
      ice_vol_SH = 0.0
      ice_area_SH = 0.0
      ice_vol_NH = 0.0
      ice_area_NH = 0.0
      do j=1,jmax
         do  i=1,imax
            if (k1(i,j).le.kmax) then
               ice_vol = ice_vol + modeldata1(i,j,1)*dphi*ds(j)
               ice_area = ice_area + modeldata2(i,j,1)*dphi*ds(j)
               if ((sv(j-1)*sv(j)).le.0.0) then
                  w = -1.0*sv(j-1)*rds(j)
               elseif (s(j).lt.0) then
                  w = 1.0
               else
                  w = 0.0
               endif
               ice_vol_SH = ice_vol_SH + w*modeldata1(i,j,1)*dphi*ds(j)
               ice_area_SH = ice_area_SH + w*modeldata2(i,j,1)*dphi
     $              *ds(j)
               ice_vol_NH = ice_vol_NH + (1.0-w)*modeldata1(i,j,1)*dphi
     $              *ds(j)
               ice_area_NH = ice_area_NH + (1.0-w)*modeldata2(i,j,1)
     $              *dphi*ds(j)
            endif
         enddo
      enddo
      ice_vol = ice_vol*rsc*rsc
      ice_area = ice_area*rsc*rsc
      ice_vol_SH = ice_vol_SH*rsc*rsc
      ice_area_SH = ice_area_SH*rsc*rsc
      ice_vol_NH = ice_vol_NH*rsc*rsc
      ice_area_NH = ice_area_NH*rsc*rsc
      end
