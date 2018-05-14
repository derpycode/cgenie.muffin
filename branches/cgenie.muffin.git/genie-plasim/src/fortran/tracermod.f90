!
!     **********************************************************
!     *  Parameters and subroutines for tracer transport       *
!     **********************************************************

      module tracermod

!     **********************************************************
!     * This module contains the parameters, arrays and        *
!     * subroutines that are needed for transporting tracers   *
!     * using the Flux-Form Semi-Lagrangian (FFSL) algorithm   *
!     * developed by S.-J. Lin (now at GFDL).                  *
!     **********************************************************
!     * The original transport code (in F77) was written by    *
!     * S.-J. Lin. Adaptation for the Planet Simulator was     *
!     * done by Hui Wan (MPI-M).
!     **********************************************************

      use pumamod, only: NLAT,NLEV
      implicit none

      logical,parameter :: ffsl_debug  = .false.
      logical,parameter :: ffsl_zcross = .true.
      logical,parameter :: ffsl_deform = .false.

      logical,parameter :: ffsl_fill = .false.
      logical,parameter :: ffsl_mfct = .false.

      integer,parameter :: ffsl_iord = 2
      integer,parameter :: ffsl_jord = 2
      integer,parameter :: ffsl_kord = 3

      integer,parameter :: ffsl_cnst = 1   ! 1 = constant preserving
                                           ! 2 = mass conserving

      integer,parameter :: ffsl_j1  = 2  ! 1st lat. outside polar cap
      integer,parameter :: ffsl_j2  = NLAT + 1 - ffsl_j1 
                                         ! last lat. outside polar cap

      integer           :: iml
      integer           :: js0,jn0

      real :: dap(NLEV), dbk(NLEV)

      real :: gaulat (NLAT)     ! gaussian latitudes
      real ::   dlat (NLAT)     ! delta phi
      real ::  colad (NLAT)     ! ~ cos(phi)
      real :: rcolad (NLAT)     ! 1/colad
      real ::  colae (NLAT+1)   ! cos(lat(j-1/2))
      real ::  rcap              
      real ::  dtoa             ! delta_t/plarad
      real ::  dtdx  (NLAT)     ! delta_t/delta_x

      end module tracermod

!     ======================
!     SUBROUTINE TRACER_INI0
!     ======================

      subroutine tracer_ini0

      use pumamod, only: mypid,NROOT,NLON,NLAT,sid,dsigma,PI,dtrace
      use tracermod

      implicit none

      real :: zlate (NLAT+1)    ! N-S boundaries between lat. bands
      real :: zsilae(NLAT+1)    ! sin(zlate)

!     *****************************************
!     * inform the user about his/her choices *
!     *****************************************

      if (mypid == NROOT) then
          write(*,*) '************************************'
          write(*,*) '* FFSL transport core version 6.m  *'
          write(*,*) '************************************'
          write(*,*) '* iord =', ffsl_iord
          write(*,*) '* jord =', ffsl_jord
          write(*,*) '* kord =', ffsl_kord
          if (ffsl_mfct) write(*,*) ' * mfct option is on'
      endif

      if(NLEV.lt.6) then
         if (mypid == NROOT) then
            write(*,*) 'ERROR: in FFSL initialization: NLEV must be >=6'
            write(*,*) 'Or set NSELA=0 in puma_namelist'
            stop
         endif
      endif

!     ************************************************************
!     *   Arrays used for calculating vertical layer thickness   *
!     ************************************************************

!     The original code assumed hybrid pressure-sigma coordinate, 
!     while PUMA uses the pure sigma coordinate with ptop=0.

      dap(:) =  0.
      dbk(:) =  dsigma(:)

!     ************************************************************
!     * initialize arrays related to the meridional differencing *
!     ************************************************************

!     gaussian latitudes from south to north

      gaulat(:) = asin(-sid)

!     cell boundaries (edges) in the N-S direction, and the cosine

      zlate(1)      = -0.5*PI
      colae(1)      =  0.
      zsilae(1)     = -1.

      zlate(NLAT+1) =  0.5*PI
      colae(NLAT+1) =  0.
      zsilae(NLAT+1)=  1.


      zlate(2:NLAT) =  0.5*( gaulat(1:NLAT-1) + gaulat(2:NLAT) )
      colae(2:NLAT) =  cos(zlate(2:NLAT))

!     meridional grid spacing (i.e., cell width in the N-S direction)

      dlat(1:NLAT) = zlate(2:NLAT+1) - zlate(1:NLAT)

!     area factor of the meridional direction which can be interpreted 
!     as the cosine of the gaussian latitude in the discrete case  

      zsilae(2:NLAT) = sin(zlate(2:NLAT))
       colad(1:NLAT) = (zsilae(2:NLAT+1) - zsilae(1:NLAT)) /dlat(1:NLAT)

!     scaled area factor of the polar caps

      colad(1)    = NLON*colad(1)
      colad(NLAT) = colad(1)

      rcap        = 1./colad(1)/dlat(1)

!     inverse of the area factor

      rcolad(1:NLAT) = 1./colad(1:NLAT)

!     initialize tracers 3 and 4 for idealized testing

!     dlambda = 2.*PI/NLON
!     do j=1,NLAT
!     do i=1,NLON
!        dtrace(i,j,:,3) = 1. + sin((i-1)*dlambda)*cos(gaulat(j))
!        dtrace(i,j,:,4) = 1. - sin((i-1)*dlambda)*cos(gaulat(j))
!     enddo
!     enddo

      return
      end subroutine tracer_ini0

!     ======================
!     SUBROUTINE TRACER_INI
!     ======================

      subroutine tracer_ini

      use pumamod, only: mypid,NROOT,NLON,NLAT,PI,TWOPI,deltsec,plarad,umax
      use tracermod

      implicit none

      real :: zumax, zcr1, zmaxdt, zlat0, ztc

!     ************************************************************
!     * Check whether the time step is too long.
!     ************************************************************

!     Assuming the maximum meridional wind is 0.5*ffsl_Umax,
!     check if the CFL condition is violated in the N-S direction.

      zumax = max(umax,50.)

      if (mypid == NROOT .and. ffsl_debug) then
!
!         we actually need vmax for this check
!         zmaxdt =    PI*plarad/NLAT/abs(zvmax)
!
          zmaxdt = 2.*PI*plarad/NLAT/abs(zumax)
          write(*,'(a)') '* FFSL transport: '
          write(*,'(a,f8.2,a,f6.0,a)') '* Largest time step for max(V)=', &
                                       zumax/2.,' m/s is ',zmaxdt,' sec'
          write(*,'(a,f8.2,a)') '* Current time step is',deltsec,' sec'
          if(zmaxdt .lt. deltsec) & 
          write(*,*) ' Warning!!! time step maybe too long!'
      endif

!     Courant number at the equator

      zcr1 = abs(zumax)*deltsec*NLON/(TWOPI*plarad)

      if (zcr1>=0.95) then
         js0 = NLAT/2       ! the 1st latitude south of the Equator
         jn0 = NLAT/2 +1    ! the 1st latitude south of the Equator
         iml = NLON-2
         ztc = 0.
      else
         ztc = acos(zcr1)
         js0 = 1
         zlat0 = gaulat(js0)
         do while (zlat0.le.-ztc)
            js0   = js0+1
            zlat0 = gaulat(js0)
         end do
         js0 = max(js0, ffsl_j1+1)
         iml = min(6*js0/(ffsl_j1-1)+2, 4*NLON/5)
         jn0 = NLAT-js0+1
      endif

      if (mypid == NROOT .and. ffsl_debug) then
         write(*,'(a,i3.3,a,i3.3)') '* js0 = ',js0, ', jn0 = ',jn0 
         write(*,*)
      endif

!     ************************************************************
!     *            Frequently used parameters                    *
!     ************************************************************

      dtdx(:) = deltsec*NLON/(TWOPI*plarad*colad(:))
      dtoa    = deltsec/plarad

      return
      end subroutine tracer_ini

!     ======================
!     SUBROUTINE TRACER_MAIN
!     ======================

      subroutine tracer_main

      use pumamod, only: du,dv,dp,du0,dv0,dp0,dtrace, &
                         NLON,NLAT,NLEV,NTRACE,       &
                         mypid,NROOT
      use tracermod
      implicit none

      real :: zu   (NLON,NLAT,NLEV)
      real :: zv   (NLON,NLAT,NLEV)
      real :: zps0 (NLON,NLAT)
      real :: zps1 (NLON,NLAT)

!     --- 

      call prepare_uvps( zu,zv,zps0,zps1,      & ! output
                         du0,dv0,dp0,du,dv,dp)   ! input

      if (mypid == NROOT .and. ffsl_debug) then
         write(*,'(a,f11.2)') '* max u   =',maxval(abs(zu))
         write(*,'(a,f11.2)') '* max v   =',maxval(abs(zv))
         write(*,'(a,f11.2)') '* max ps0 =',maxval(zps0)
         write(*,'(a,f11.2)') '* max ps1 =',maxval(zps1)
       ! write(*,*)
       ! write(*,*) 'ps0 NP'
       ! write(*,*)  dp0(1:NLON)
       ! write(*,*) 'zps0 NP'
       ! write(*,*) zps0(1:NLON,NLAT)
       ! write(*,*) 'ps0 SP'
       ! write(*,*)  dp0(NLON*(NLAT-1)+1:)
       ! write(*,*) 'zps0 SP'
       ! write(*,*) zps0(1:NLON,1)
      end if
    
      if (mypid == NROOT) then

         call tpcore( dtrace,                             &
                      zps0,zps1,zu,zv,                    &
                      dtoa,dtdx,                          &
                      ffsl_iord,ffsl_jord,ffsl_kord,      & 
                      NTRACE,NLON,NLAT,NLEV,dap,dbk,      &
                      iml,ffsl_j1,ffsl_j2,js0,jn0,        &
                      colae,colad,rcolad,dlat,rcap,       &
                      ffsl_cnst,ffsl_deform,ffsl_zcross,  &
                      ffsl_fill,ffsl_mfct,ffsl_debug)

      end if

      return
      end subroutine tracer_main

!     =======================
!     SUBROUTINE PREPARE_UVPS
!     =======================

      subroutine prepare_uvps( pou,pov,pops0,pops1,       & ! output
                               pu0,pv0,pps0,pu1,pv1,pps1  ) ! input
                               

      use pumamod, only: NHOR,NUGP,NLON,NLAT,NLEV
      implicit none
!     implicit real (a-h,o-z) 

!     input arrays

      real,intent(in) :: pu0 (NHOR,NLEV)  ! u-wind at time t
      real,intent(in) :: pv0 (NHOR,NLEV)  ! v-wind at time t
      real,intent(in) :: pps0(NHOR)       ! surf. pressure at time t
      real,intent(in) :: pu1 (NHOR,NLEV)  ! u-wind at time t+dt
      real,intent(in) :: pv1 (NHOR,NLEV)  ! v-wind at time t+dt
      real,intent(in) :: pps1(NHOR)       ! surf. pressure at time t+dt

!     output arrays

      real,intent(out) :: pou  (NLON,NLAT,NLEV) ! estimated u-wind at t+0.5dt
      real,intent(out) :: pov  (NLON,NLAT,NLEV) ! estimated v-wind at t+0.5dt
      real,intent(out) :: pops0(NLON,NLAT)      ! surf. pressure at time t
      real,intent(out) :: pops1(NLON,NLAT)      ! surf. pressure at time t+dt 

!     local variables

      real :: zdcp (NHOR,NLEV)  ! decomposed field
      real :: zgbl (NUGP,NLEV)  ! global field

      integer :: jlev, jlat, is, ie

!     ---
!
!     First process the zonal wind:
!      1. do temporal average 
!      2. collect the global data
!      3. reshape the array, and invert the N-S direction 

      zdcp(:,:) = 0.5*( pu0(:,:) + pu1(:,:) )

      call mpgagp( zgbl,zdcp,NLEV )

      do jlev = 1,NLEV
      do jlat = 1,NLAT
         is = (jlat-1)*NLON + 1
         ie =  jlat   *NLON
         pou(1:NLON,NLAT+1-jlat,jlev) = zgbl(is:ie,jlev)
      end do
      end do

!    The same procedure for the meridional wind

      zdcp(:,:) = 0.5*( pv0(:,:) + pv1(:,:) )

      call mpgagp( zgbl,zdcp,NLEV )

      do jlev = 1,NLEV
      do jlat = 1,NLAT
         is = (jlat-1)*NLON + 1
         ie =  jlat   *NLON
         pov(1:NLON,NLAT+1-jlat,jlev) = zgbl(is:ie,jlev)
      end do
      end do

!    For surface pressure, just reshape and invert: 
!    Time t

      call mpgagp( zgbl(:,1),pps0,1 )

      do jlat = 1,NLAT
         is = (jlat-1)*NLON + 1
         ie =  jlat   *NLON
         pops0(1:NLON,NLAT+1-jlat) = zgbl(is:ie,1)
      end do

!    Time t+dt

      call mpgagp( zgbl(:,1),pps1,1 )

      do jlat = 1,NLAT
         is = (jlat-1)*NLON + 1
         ie =  jlat   *NLON
         pops1(1:NLON,NLAT+1-jlat) = zgbl(is:ie,1)
      end do

      return 
      end subroutine prepare_uvps

