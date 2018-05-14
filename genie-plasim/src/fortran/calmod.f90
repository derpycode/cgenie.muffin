!     =============
!     MODULE CALMOD
!     =============

      module calmod
      implicit none
      integer :: mondays(0:12) = (/0,31,28,31,30,31,30,31,31,30,31,30,31/)
      integer :: mona365(0:12) = (/0,31,59,90,120,151,181,212,243,273,304,334,365/)
      integer :: monaccu(0:12)
      integer :: ny400d = 400 * 365 + 97
      integer :: ny100d = 100 * 365 + 24
      integer :: ny004d =   4 * 365 +  1
      integer :: ny001d =       365

!     These values are copied from pumamod in subroutine calini

      integer :: n_days_per_month =  30
      integer :: n_days_per_year  = 360
      integer :: n_start_step     =   0
      integer :: ntspd            =   0
      real    :: solar_day        = 86400.0 ! [sec]
      
      end module calmod


!     =================
!     SUBROUTINE CALINI
!     =================

      subroutine calini(k_days_per_month,k_days_per_year,k_start_step,ktspd,psolday)
      use calmod
      implicit none
      integer k_days_per_month,k_days_per_year,k_start_step,ktspd
      real psolday

      n_days_per_month = k_days_per_month
      n_days_per_year  = k_days_per_year
      n_start_step     = k_start_step
      ntspd            = ktspd
      solar_day        = psolday

!      if (n_days_per_month == 0) then
!         write (*,*) 'calini: n_days_per_month = 0 (variable) '
!      else
!         write (*,*) 'calini: n_days_per_month = ',n_days_per_month
!      endif
!      write (*,*) 'calini: n_days_per_year  = ',n_days_per_year
!      write (*,*) 'calini: n_start_step i   = ',n_start_step
!      write (*,*) 'calini: ntspd            = ',ntspd
      return
      end subroutine calini

!     ====================
!     SUBROUTINE YDAY2MMDD
!     ====================

      subroutine yday2mmdd(kyday,kmon,kday)
      use calmod
      implicit none
      integer kyday,kmon,kday
      if (n_days_per_year == 365) then
         kmon = 1
         do while (kyday > mona365(kmon))
            kmon = kmon + 1
         enddo
         kday = kyday - monaccu(kmon-1)
      else
         kmon = (kyday-1) / n_days_per_month
         kday = kyday - n_days_per_month * kmon
      endif
      return
      end
      
!     =================
!     FUNCTION NWEEKDAY
!     =================

      integer function nweekday(kday)
      implicit none
      integer kday
      nweekday = mod(kday+5,7)
      return
      end


!     ===================
!     FUNCTION NDAYOFYEAR
!     ===================

      integer function ndayofyear(kstep)
      use calmod
      implicit none
      integer kstep,iyea,imon,iday,ihou,imin

      if (n_days_per_year == 365) then
         call step2cal(kstep,ntspd,iyea,imon,iday,ihou,imin)
         ndayofyear = iday + monaccu(imon-1)
      else
         call step2cal30(kstep,ntspd,iyea,imon,iday,ihou,imin)
         ndayofyear = iday + n_days_per_month * (imon-1)
      endif

      return
      end
      

!     ===================
!     SUBROUTINE STEP2CAL
!     ===================

      subroutine step2cal(kstep,ktspd,kyea,kmon,kday,khou,kmin)
      use calmod
      implicit none
      integer, intent(IN ) :: kstep ! time step since simulation start
      integer, intent(IN ) :: ktspd ! time steps per day
      integer, intent(OUT) :: kyea  ! current year   of simulation
      integer, intent(OUT) :: kmon  ! current month  of simulation
      integer, intent(OUT) :: kday  ! current day    of simulation
      integer, intent(OUT) :: khou  ! current hour   of simulation
      integer, intent(OUT) :: kmin  ! current minute of simulation

      integer :: idall
      integer :: istp
      integer :: iy400,id400
      integer :: iy100,id100
      integer :: iy004,id004
      integer :: iy001,id001
      integer :: jmon

      logical :: leap

      idall = kstep   / ktspd

      iy400 = idall   / ny400d          ! segment [of 400 years]
      id400 = mod(idall,ny400d)

      if (id400 <= ny100d) then         ! century year is leap year
         iy100 =         0              ! century in segment [0]
         id100 =     id400
         iy004 =     id100 / ny004d     ! tetrade in century [0..24]
         id004 = mod(id100 , ny004d)
         leap  = (id004 <= ny001d)
         if (leap) then
            iy001 =     0               ! year in tetrade [0]
            id001 = id004
         else
            iy001 =    (id004-1)/ny001d ! year in tetrade [1,2,3]
            id001 = mod(id004-1, ny001d)
         endif
      else                              ! century year is not leap year
         iy100 =    (id400-1)/ny100d    ! century in segment [1,2,3]
         id100 = mod(id400-1, ny100d)
         if (id100 < ny004d-1) then
            iy004 = 0                   ! tetrade in century [0]
            id004 = id100
            leap  = .false.
            iy001 =     id004/ny001d    ! year in tetrade [1,2,3]
            id001 = mod(id004,ny001d)
         else
            iy004 = (id100+1)/ny004d    ! tetrade in century [1..24]
            id004 = mod(id100+1,ny004d)
            leap  = (id004 <= ny001d)
            if (leap) then
               iy001 =     0            ! year in tetrade [0]
               id001 = id004
            else
               iy001 =    (id004-1)/ny001d
               id001 = mod(id004-1, ny001d)
            endif
         endif
      endif

      kyea  = iy400 * 400 + iy100 * 100 + iy004 * 4 + iy001

      monaccu(0) = mondays(0)
      monaccu(1) = mondays(1)
      monaccu(2) = mondays(1) + mondays(2)
      if (leap) monaccu(2) = monaccu(2) + 1
      do jmon = 3 , 12
         monaccu(jmon) = monaccu(jmon-1) + mondays(jmon)
      enddo
      kmon = 1
      id001 = id001 + 1          
      do while (id001 > monaccu(kmon))
         kmon = kmon + 1
      enddo
      kday = id001 - monaccu(kmon-1)

      istp = mod(kstep,ktspd)
      kmin = (istp * 1440) / ktspd
      khou = kmin / 60
      kmin = mod(kmin,60)

      return
      end subroutine step2cal

!     ===================
!     SUBROUTINE CAL2STEP
!     ===================

      subroutine cal2step(kstep,ktspd,kyea,kmon,kday,khou,kmin)
      use calmod
      implicit none
      integer, intent(OUT) :: kstep ! time step since simulation start
      integer, intent(IN ) :: ktspd ! time steps per day
      integer, intent(IN ) :: kyea  ! current year   of simulation
      integer, intent(IN ) :: kmon  ! current month  of simulation
      integer, intent(IN ) :: kday  ! current day    of simulation
      integer, intent(IN ) :: khou  ! current hour   of simulation
      integer, intent(IN ) :: kmin  ! current minute of simulation

      integer :: idall
      integer :: ilp
      integer :: iy400,id400
      integer :: iy100,id100
      integer :: iy004,id004
      integer :: jmon

      logical :: leap

      iy400 = kyea   /  400    ! segment [400]
      id400 = mod(kyea ,400)   ! year in segment [0..399]
      iy100 = id400   / 100    ! century [0,1,2,3]
      id100 = mod(id400,100)   ! year in century [0..99]
      iy004 = id100   /   4    ! tetrade [0..24]
      id004 = mod(id100,  4)   ! year in tetrade [0,1,2,3]

      leap  = (id004 == 0 .and. (id100 /= 0 .or. id400 == 0))

      ilp = -1
      if (id004 > 0) ilp = ilp + 1
      if (iy100 > 0 .and. id100 == 0 ) ilp = ilp + 1

      monaccu(0) = mondays(0)
      monaccu(1) = mondays(1)
      monaccu(2) = mondays(1) + mondays(2)
      if (leap) monaccu(2) = monaccu(2) + 1
      do jmon = 3 , 12
         monaccu(jmon) = monaccu(jmon-1) + mondays(jmon)
      enddo

      idall = iy400 * ny400d + iy100 * ny100d + iy004 * ny004d &
            + id004 * ny001d + monaccu(kmon-1)+ kday + ilp
      kstep = ktspd * idall + (ktspd * (khou * 60 + kmin)) / 1440

      return
      end subroutine cal2step

!     =====================
!     SUBROUTINE STEP2CAL30
!     =====================

      subroutine step2cal30(kstep,ktspd,kyea,kmon,kday,khou,kmin)
      use calmod
      implicit none
      integer, intent(IN ) :: kstep ! time step since simulation start
      integer, intent(IN ) :: ktspd ! time steps per day
      integer, intent(OUT) :: kyea  ! current year   of simulation
      integer, intent(OUT) :: kmon  ! current month  of simulation
      integer, intent(OUT) :: kday  ! current day    of simulation
      integer, intent(OUT) :: khou  ! current hour   of simulation
      integer, intent(OUT) :: kmin  ! current minute of simulation

      integer :: idall
      integer :: istp

      idall = kstep / ktspd
      kyea  = idall / n_days_per_year
      idall = mod(idall,n_days_per_year)
      kmon  = idall / n_days_per_month + 1
      kday  = mod(idall,n_days_per_month) + 1
      istp  = mod(kstep,ktspd)
      kmin  = (istp * solar_day) / (ktspd * 60)
      khou  = kmin / 60
      kmin  = mod(kmin,60)

      return
      end subroutine step2cal30

!     =====================
!     SUBROUTINE CAL2STEP30
!     =====================

      subroutine cal2step30(kstep,ktspd,kyea,kmon,kday,khou,kmin)
      use calmod
      implicit none
      integer, intent(OUT) :: kstep ! time step since simulation start
      integer, intent(IN ) :: ktspd ! time steps per day
      integer, intent(IN ) :: kyea  ! current year   of simulation
      integer, intent(IN ) :: kmon  ! current month  of simulation
      integer, intent(IN ) :: kday  ! current day    of simulation
      integer, intent(IN ) :: khou  ! current hour   of simulation
      integer, intent(IN ) :: kmin  ! current minute of simulation

      kstep = ktspd * (kyea * n_days_per_year + (kmon-1) * n_days_per_month + kday - 1) &
            +(ktspd * (khou *  60 + kmin) * 60) / solar_day

      return
      end subroutine cal2step30

!     =================
!     SUBROUTINE NTOMIN
!     =================

       subroutine ntomin(kstep,imin,ihou,iday,imon,iyea)
       use calmod
       implicit none
       integer kstep,iyea,imon,iday,ihou,imin
       
       if (n_days_per_year == 365) then
          call step2cal(kstep,ntspd,iyea,imon,iday,ihou,imin)
       else
          call step2cal30(kstep,ntspd,iyea,imon,iday,ihou,imin)
       endif
       return
       end

!     =================
!     SUBROUTINE NTODAT
!     =================

      subroutine ntodat(istep,datch)
      implicit none
      integer istep,imin,ihou,iday,imon,iyea
      character(len=18) datch
      character(len=3) mona(12)
      data mona /'Jan','Feb','Mar','Apr','May','Jun',                   &
     &           'Jul','Aug','Sep','Oct','Nov','Dec'/
      call ntomin(istep,imin,ihou,iday,imon,iyea)
      write (datch,20030) iday,mona(imon),iyea,ihou,imin
20030 format(i2.2,'-',a3,'-',i4.4,2x,i2,':',i2.2)
      end


!     =================
!     SUBROUTINE DTODAT
!     =================

      subroutine dtodat(id,datch)
      implicit none
      integer :: id(6)
      character(len=18) datch
      character(len=3) mona(12)
      data mona /'Jan','Feb','Mar','Apr','May','Jun',                   &
     &           'Jul','Aug','Sep','Oct','Nov','Dec'/
      write (datch,20030) id(3),mona(id(2)),id(1),id(4),id(5)
20030 format(i2,'-',a3,'-',i4.4,2x,i2,':',i2.2)
      end


