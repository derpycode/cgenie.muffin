      module restartmod
      implicit none
      integer, parameter :: nresdim  = 200     ! Max number of records
      integer, parameter :: nreaunit =  33     ! FORTRAN unit for reading
      integer, parameter :: nwriunit =  34     ! FORTRAN unit for writing
      integer            :: nexcheck =   1     ! Extended checks
      integer            :: nresnum  =   0     ! Actual number of records
      integer            :: nlastrec =   0     ! Last read record
      character (len=16) :: yresnam(nresdim)   ! Array of record names
      logical            :: ldebrm   = .false. ! Print debug info
      end module restartmod

!     ======================
!     SUBROUTINE RESTART_INI
!     ======================

      subroutine restart_ini(lrestart,yrfile)
      use restartmod
      implicit none
      integer iostat,j

      logical :: lrestart
      character (len=*)  :: yrfile
      character (len=16) :: yn ! variable name

      inquire(file=yrfile,exist=lrestart)
      if (lrestart) then
         open(nreaunit,file=yrfile,form='unformatted')
         do
            read (nreaunit,IOSTAT=iostat) yn
            if (iostat /= 0) exit
            nresnum = nresnum + 1
            yresnam(nresnum) = yn
            read (nreaunit,IOSTAT=iostat)
            if (iostat /= 0) exit
            if (nresnum >= nresdim) then
               print *,'Too many variables in restart file'
               print *,'Increase dimension NRESDIM in module restartmod'
               print *,'*** Error Stop ***'
               stop
            endif
         enddo
   
         write (*,'(a,i4,3a/)') 'Found ',nresnum, &
               ' variables in file <',yrfile,'>'
         do j = 1 , nresnum
            write(*,'(i4," : ",a)') j,yresnam(j)
         enddo
         nlastrec = nresnum
      endif ! (lrestart)

!     file must be left open for further access

      return
      end subroutine restart_ini     


!     ==========================
!     SUBROUTINE RESTART_PREPARE
!     ==========================

      subroutine restart_prepare(ywfile)
      use restartmod
      implicit none

      character (len=*) :: ywfile

      open(nwriunit,file=ywfile,form='unformatted',status='replace')

      return
      end subroutine restart_prepare


!     =======================
!     SUBROUTINE RESTART_STOP
!     =======================

      subroutine restart_stop
      use restartmod
      implicit none

      close (nreaunit)
      close (nwriunit)

      return
      end subroutine restart_stop


!     ==============================
!     SUBROUTINE GET_RESTART_INTEGER
!     ==============================

      subroutine get_restart_integer(yn,kv)
      use restartmod
      implicit none
      integer j

      character (len=*) :: yn
      integer :: kv

      do j = 1 , nresnum
         if (trim(yn) == trim(yresnam(j))) then
            call reseek(yn,j)
            read (nreaunit) kv
            nlastrec = nlastrec + 1
            return
         endif
      enddo
      if (nexcheck == 1) then
         print *,'*** Error in get_restart_integer ***'
         print *,'Requested integer {',yn,'} was not found'
         stop
      endif
      return
      end subroutine get_restart_integer


!     ============================
!     SUBROUTINE GET_RESTART_ARRAY
!     ============================

      subroutine get_restart_array(yn,pa,k1,k2,k3)
      use restartmod
      implicit none
      integer k1,k2,k3,j

      character (len=*) :: yn
      real :: pa(k2,k3)

      do j = 1 , nresnum
         if (trim(yn) == trim(yresnam(j))) then
            call reseek(yn,j)
            read (nreaunit) pa(1:k1,:)
            nlastrec = nlastrec + 1
            return
         endif
      enddo
      if (nexcheck == 1) then
         print *,'*** Error in get_restart_array ***'
         print *,'Requested array {',yn,'} was not found'
         stop
      endif
      return
      end subroutine get_restart_array


!     ==============================
!     SUBROUTINE PUT_RESTART_INTEGER
!     ==============================

      subroutine put_restart_integer(yn,kv)
      use restartmod
      implicit none

      character (len=*)  :: yn
      character (len=16) :: yy
      integer :: kv

      yy = yn
      write (nwriunit) yy
      write (nwriunit) kv
      return
      end subroutine put_restart_integer


!     ============================
!     SUBROUTINE PUT_RESTART_ARRAY
!     ============================

      subroutine put_restart_array(yn,pa,k1,k2,k3)
      use restartmod
      implicit none

      character (len=*)  :: yn
      character (len=16) :: yy
      integer :: k1,k2,k3
      real :: pa(k2,k3)

      yy = yn
      write (nwriunit) yy
      write (nwriunit) pa(1:k1,1:k3)
      return
      end subroutine put_restart_array


!     =================
!     SUBROUTINE RESEEK
!     =================

      subroutine reseek(yn,k)
      use restartmod
      implicit none
      integer iostat,k

      character (len=*)  :: yn
      character (len=16) :: yy

      if (ldebrm) write (*,*) 'Pos:',nlastrec,'   Want:',k
      if (k <= nlastrec) then
         if (ldebrm) write (*,*) 'Rewinding'
         rewind nreaunit
         nlastrec = 0
      endif

      do
         read (nreaunit,iostat=iostat) yy
         if (iostat /= 0) exit
         if (trim(yn) == trim(yy)) return ! success
         read (nreaunit,iostat=iostat)    ! skip data
         if (iostat /= 0) exit
         nlastrec = nlastrec + 1
      enddo
      write (*,*) 'Variable <',trim(yn),'> not in restart file'
      return
      end


!     =========================
!     SUBROUTINE CHECK_EQUALITY
!     =========================

      subroutine check_equality(yn,pa,pb,k1,k2)
      character (len=*) :: yn
      integer k1,k2,j1,j2
      real :: pa(k1,k2)
      real :: pb(k1,k2)

      do j2 = 1 , k2
      do j1 = 1 , k1
         if (pa(j1,j2) /= pb(j1,j2)) then
            print *,'No Equality on ',yn,'(',j1,',',j2,')',pa(j1,j2),pb(j1,j2)
            return
         endif
      enddo
      enddo
      print *,'Array {',yn,'} is OK'
      return
      end

