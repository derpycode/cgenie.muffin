
c this subroutine is based on 'rmsnorm_goldstein_{T,S}.F' (which reused
c fragments from 'genie-main/src/fortran/genie_ea_go_gs.f90' in adapted
c form). Analogous to the computation of the RMS error score from
c prevously produced model output by the subroutines in
c 'rmsnorm_goldstein_{T,S}.F', this subroutine computes and returns
c various diagnostics from such output

c returned diagnostics:
c
c  - opsia_max: maximum of the overturning streamfunction in the
c  Atlantic below 'opsi_depth_limit' (Sv)
c
c  - opsia_max_lat, opsia_max_depth: latitude and depth of maximum of
c  overturning streamfunction in the Atlantic
c
c  - opsia_min: minimum of the overturning streamfunction in the
c  northern Atlantic below 'opsi_depth_limit' (Sv)
c
c  - opsia_eq_max: maximum of the overturning streamfunction in the
c  Atlantic at the Equator below 'opsi_depth_limit' (Sv)
c
c  - opsia_eq_min: minimum of the overturning streamfunction in the
c  Atlantic at the Equator below 'opsi_depth_limit' (Sv)
c
c  - opsia_eq_zero: depth where the streamfunction is zero in the
c  Atlantic at the Equator below 'opsi_depth_limit' (m)
c
c  - opsip_max: maximum of the overturning streamfunction in the Pacific
c  below 'opsi_depth_limit' (Sv)
c
c  - opsip_min: minimum of the overturning streamfunction in the Pacific
c  below 'opsi_depth_limit' (Sv)
c
c  - opsi_depth_limit: threshold at and below which overturning
c  streamfunction extrema are diagnosed
c
c  - tf_drake: Drake Passage through flow (Sv)
c
      subroutine diag_goldstein_circulation_annual_av(yearstr,opsia_max
     $     ,opsia_max_lat,opsia_max_depth,opsia_min,opsia_eq_max
     $     ,opsia_eq_min,opsia_eq_zero_depth,opsip_max,opsip_min
     $     ,opsi_depth_limit,tf_drake)
      
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

      real modeldata1(maxi,maxj,maxk,1)
      real modeldata2(maxi,maxj+1,maxk,1)

c diagnostics from internal diagnostics subroutines
      real ominp,omaxp,omina,omaxa
      real opsi(0:maxj,0:maxk),opsip(0:maxj,0:maxk),opsia(0:maxj,0:maxk)
      integer iposa(2)
c variables for additional diagnostics
      real opsia_eq(0:maxk)
c diagnostics for output
      real opsia_max,opsia_max_lat,opsia_max_depth,opsia_min
     $     ,opsia_eq_max,opsia_eq_min,opsia_eq_zero_depth,opsip_max
     $     ,opsip_min,opsi_depth_limit,tf_drake

      integer loc_drake(2)
      real dist_drake
      logical int_drake

      integer i,j,k

c     Retrieve previously written annual average fields from the
c     GOLDSTEIN NetCDF output for specified output year
      model_datafile='gold_'//lout//'_av_'//yearstr//'.nc'
      model_lendatafile=lnsig1(model_datafile)
      filename=trim(outdir_name(1:lenout))
     $     //trim(model_datafile(1:model_lendatafile))
      print*,'GOLD model data file: ',filename
      status=nf_open(trim(filename), 0, ncid)
      IF (status .ne. NF_NOERR) call check_err(status)
      call get4d_data_nc(ncid, 'uvel', imax, jmax, kmax, 1,
     $     modeldata1,status)
      IF (status .ne. NF_NOERR) call check_err(status)
      call get4d_data_nc(ncid, 'vvel', imax, jmax+1, kmax, 1,
     $     modeldata2,status)
      IF (status .ne. NF_NOERR) call check_err(status)
      status=nf_close(ncid)
      IF (status .ne. NF_NOERR) call check_err(status)
c     Transform the data from the NetCDF file back to the model
c     representation. Since we're working "offline" it should be safe to
c     overwrite the model's internal horizonal velocity fields in order
c     to make use of the model's internal diagnostics routines
      do k=1,kmax
         do i=1,imax
            do j=1,jmax
               if (modeldata1(i,j,kmax-k+1,1).gt.-999.9) then
                  u(1,i,j,k)=modeldata1(i,j,kmax-k+1,1)/usc
               else
                  u(1,i,j,k)=0.0
               endif
            enddo
            u(2,0,j,k) = u(2,imax,j,k)
            do j=0,jmax
               if (modeldata2(i,j,kmax-k+1,1).gt.-999.9) then
                  u(2,i,j,k)=modeldata2(i,j,kmax-k+1,1)/usc
               else
                  u(2,i,j,k)=0.0
               endif
            enddo
            u(2,0,j,k) = u(2,imax,j,k)
         enddo
      end do
      call diagopsi(ominp,omaxp,omina,omaxa,opsi,opsia,opsip,iposa)

c dimensionalise (using units of Sv)
      opsia_max = omaxa*opsisc
      opsia_max_lat = 180.0*asin(sv(iposa(1)))/pi
      opsia_max_depth = -1.0*zw(iposa(2))*dsc
      opsia_min = omina*opsisc
      opsip_max = omaxp*opsisc
      opsip_min = ominp*opsisc
      opsi_depth_limit = -1.0*zw(overdep)*dsc

c additionally try to find max., min., and first downward crossing of
c zero from positive to negative values of the Atlantic's overturning
c streamfunction at the equator
      
      if (modulo(jmax,2).eq.0) then
         opsia_eq(:) = opsia(jmax/2,:)
      else
         opsia_eq(:) = 0.5*(opsia((jmax-1)/2,:)+opsia(((jmax-1)/2+1),:))
      endif
      opsia_eq_min = 1.0
      opsia_eq_max = -1.0
      do k=1,overdep
         if (opsia_eq(k).lt.opsia_eq_min) opsia_eq_min=opsia_eq(k)
         if (opsia_eq(k).gt.opsia_eq_max) opsia_eq_max=opsia_eq(k)
      enddo
      opsia_eq_min = opsia_eq_min*opsisc
      opsia_eq_max = opsia_eq_max*opsisc
      opsia_eq_zero_depth = -1.0*zw(overdep)*dsc
      k = overdep
      opsia_eq_zero_depth = -99999.9
      do while (((opsia_eq(k+1).le.0.0)
     $     .or.(opsia_eq(k).gt.0.0)).and.(k.gt.0))
         k = k-1
      enddo
      if ((opsia_eq(k+1)).gt.0.0) then
         opsia_eq_zero_depth = -1.0*(zw(k)+dz(k+1)*opsia_eq(k)
     $        /(opsia_eq(k)-opsia_eq(k+1)))*dsc
      endif

c find horizonatl grid location nearest to 60S, 67.5W
      loc_drake(1) = 0
      dist_drake = 999.
      do i=1,imax
         if (modulo(abs(phi0+(i-0.5)*dphi-(pi*(-67.5)/180.0))
     $        ,2.0*pi).lt.dist_drake) then
            loc_drake(1) = i
            dist_drake = modulo(abs(phi0+(i-0.5)*dphi-(pi*(-67.5)
     $           /180.0)),2.0*pi)
         endif
      enddo
      loc_drake(2) = 0
      dist_drake = 999.
      do j=1,jmax
         if (abs(s(j)-sin(pi*(-60.0)/180.0)).lt.dist_drake)
     $        then
            loc_drake(2) = j
            dist_drake = abs(s(j)-sin(pi*(-60.0)/180.0))
         endif
      enddo
      tf_drake = 0.0
      int_drake = .true.
      do j=loc_drake(2),jmax
         if (int_drake.and.k1(loc_drake(1),j).le.kmax) then
            do k=k1(loc_drake(1),j),kmax
               tf_drake = tf_drake + u(1,loc_drake(1),j,k)*dz(k)
     $              *(asin(sv(k))-asin(sv(k-1)))
            enddo
         else
            int_drake = .false.
         endif
      enddo
      int_drake = .true.
      do j=loc_drake(2),1,-1
         if (int_drake.and.k1(loc_drake(1),j).le.kmax) then
            do k=k1(loc_drake(1),j),kmax
               tf_drake = tf_drake + u(1,loc_drake(1),j,k)*dz(k)
     $              *(asin(sv(k))-asin(sv(k-1)))
            enddo
         else
            int_drake = .false.
         endif
      enddo
      tf_drake = tf_drake*usc*dsc*rsc*1e-6

      end
