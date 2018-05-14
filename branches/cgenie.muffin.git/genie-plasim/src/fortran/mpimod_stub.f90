!     =======================================
!     mpimod_dummy.f90
!     ----------------
!     This module replaces <mpimod.f90> for 
!     single CPU runs
!
!     The module is shared by PUMA and PlaSim
!     =======================================

      subroutine mpbci(k) ! broadcast 1 integer
      implicit none
      integer k
      k=k
      return
      end

      subroutine mpbcin(k,n) ! broadcast n integer
      implicit none
      integer n
      integer :: k(n)
      n=n
      k(1)=k(1)
      return
      end

      subroutine mpbcr(p) ! broadcast 1 real
      implicit none
      real p
      p=p
      return
      end

      subroutine mpbcrn(p,n) ! broadcast n real
      implicit none
      integer n
      real :: p(n)
      p=p
      return
      end

      subroutine mpscin(k,n) ! scatter n integer
      implicit none
      integer n
      integer :: k(n)
      k=k
      return
      end

      subroutine mpscrn(p,n) ! scatter n real
      implicit none
      integer n
      real :: p(n)
      p=p
      return
      end

      subroutine mpscdn(p,n) ! scatter n double precision
      implicit none
      integer n
      real (kind=8) :: p(n)
      p=p
      return
      end

      subroutine mpscsp(pf,pp,klev) ! scatter spectral fields
      use pumamod
      implicit none
      integer klev
      real pf(NESP,klev)
      real pp(NSPP,klev)
      pp(1:NSPP,1:klev) = pf(1:NSPP,1:klev)
      return
      end

      subroutine mpscgp(pf,pp,klev) ! scatter gridpoint fields
      use pumamod
      implicit none
      integer klev
      real pf(NLON*NLAT,klev)
      real pp(NHOR,klev)
      pp(1:NHOR,1:klev) = pf(1:NHOR,1:klev)
      return
      end

      subroutine mpgasp(pf,pp,klev) ! gather spectral fields
      use pumamod
      implicit none
      integer klev
      real pf(NESP,klev)
      real pp(NSPP,klev)
      pf(1:NSPP,1:klev) = pp(1:NSPP,1:klev)
      return
      end

      subroutine mpgagp(pf,pp,klev) ! gather gridpoint fields
      use pumamod
      implicit none
      integer klev
      real pf(NHOR,klev)
      real pp(NHOR,klev)
      pf = pp
      return
      end

      subroutine mpgacs(pcs) ! gather cross sections
      use pumamod
      implicit none
      real pcs
      dimension pcs(NLAT,NLEV)
      pcs=pcs
      return
      end

      subroutine mpgallsp(pf,pp,klev) ! gather spectral to all
      use pumamod
      implicit none
      integer klev
      real pf(NESP,klev)
      real pp(NSPP,klev)
      pf(1:NSPP,1:klev) = pp(1:NSPP,1:klev)
      return
      end

      subroutine mpsum(psp,klev) ! sum spectral fields
      implicit none
      integer klev
      real :: psp(klev)
      psp=psp
      return
      end

      subroutine mpsumsc(psf,psp,klev) ! sum & scatter spectral
      use pumamod
      implicit none
      integer klev
      real psf(NESP,klev)
      real psp(NSPP,klev)
      psp(1:NSPP,1:klev) = psf(1:NSPP,1:klev)
      return
      end

      subroutine mpsumr(pr,kdim) ! sum kdim reals
      implicit none
      integer kdim
      real :: pr(kdim)
      pr=pr
      return
      end subroutine mpsumr

      subroutine mpsumbcr(pr,kdim) ! sum & broadcast kdim reals
      implicit none
      integer kdim
      real :: pr(kdim)
      pr=pr
      return
      end

!PBH dummy routine for scalars
      subroutine mpsumbcr1(pr) ! sum & broadcast kdim reals
      implicit none
      real pr
      pr=pr
      return
      end

!     ==================
!     SUBROUTINE MPABORT
!     ==================

      subroutine mpabort(ym)
      implicit none
      integer ilmess,ilabor,ilen,j,ioff

      character (len=* ) :: ym
      character (len=64) :: ystar = ' '
      character (len=64) :: yline = ' '
      character (len=64) :: yabor = 'Program aborted'
      character (len=64) :: ymess = ' '
      character (len=64) :: yhead = ' '

      ilmess = len_trim(ym)
      ilabor = len_trim(yabor)
      ilen   = 60

      do j = 1 , ilen+4
         ystar(j:j) = '*'
         yline(j:j) = '-'
      enddo

      ioff = 2
      if (ilmess < ilen-1) ioff = ioff + (ilen - ilmess) / 2
      ymess(1+ioff:ilmess+ioff) = trim(ym)

      ioff = 2
      if (ilabor < ilen-1) ioff = ioff + (ilen - ilabor) / 2
      yhead(1+ioff:ilabor+ioff) = trim(yabor)

      yline(1:1) = '*'
      ymess(1:1) = '*'
      yhead(1:1) = '*'
      yline(2:2) = ' '
      ymess(2:2) = ' '
      yhead(2:2) = ' '
      j = ilen + 4
      yline(j:j) = '*'
      ymess(j:j) = '*'
      yhead(j:j) = '*'
      j = ilen + 3
      yline(j:j) = ' '
      ymess(j:j) = ' '
      yhead(j:j) = ' '
   
      open (44,file='Abort_Message')
      write(44,'(A)') trim(ystar)
      write(44,'(A)') trim(yhead)
      write(44,'(A)') trim(yline)
      write(44,'(A)') trim(ymess)
      write(44,'(A)') trim(ystar)
      close(44)
      write( *,'(/,A)') trim(ystar)
      write( *,'(A)') trim(yhead)
      write( *,'(A)') trim(yline)
      write( *,'(A)') trim(ymess)
      write( *,'(A,/)') trim(ystar)

      stop
      end


      subroutine mpstart ! initialization
      use pumamod
      implicit none
      if (NPRO > 1) then
        print*,'error : scalar version compiled with NPRO > 1!'
        stop
      endif
      return
      end

      subroutine mpstop
      implicit none
      return
      end

      subroutine mpreadsp(ktape,p,kdim,klev)
      implicit none
      integer ktape,kdim,klev
      real p(kdim,klev)
      read (ktape) p
      return
      end

      subroutine mpreadgp(ktape,p,kdim,klev)
      implicit none
      integer ktape,kdim,klev
      real p(kdim,klev)
      read (ktape) p
      return
      end

      subroutine mpwritesp(ktape,p,kdim,klev)
      implicit none
      integer ktape,kdim,klev
      real p(kdim,klev)
      write (ktape) p
      return
      end

      subroutine mpwritegp(ktape,p,kdim,klev)
      implicit none
      integer ktape,kdim,klev
      real p(kdim,klev)
      write (ktape) p
      return
      end

      subroutine mpwritegph(ktape,p,kdim,klev,ihead)
      implicit none
      integer ktape,kdim,klev
      real p(kdim,klev)
!
      real(kind=4) zp(kdim,klev)
!
      integer ihead(8)
      write(ktape) ihead
      zp(:,:)=p(:,:)
      write(ktape) zp

      return
      end


      subroutine mpi_info(nprocess,pid)    ! get nproc and pid
      implicit none
      integer nprocess, pid
      nprocess = 1
      pid = 0
      return
      end subroutine mpi_info


      subroutine mpgetsp(yn,p,kdim,klev)
      implicit none
      character (len=*) :: yn
      integer kdim,klev
      real :: p(kdim,klev)
      call get_restart_array(yn,p,kdim,kdim,klev)
      return
      end subroutine mpgetsp


      subroutine mpgetgp(yn,p,kdim,klev)
      implicit none
      character (len=*) :: yn
      integer kdim,klev
      real :: p(kdim,klev)
      call get_restart_array(yn,p,kdim,kdim,klev)
      return
      end subroutine mpgetgp


      subroutine mpputsp(yn,p,kdim,klev)
      implicit none
      character (len=*) :: yn
      integer kdim,klev
      real :: p(kdim,klev)
      call put_restart_array(yn,p,kdim,kdim,klev)
      return
      end subroutine mpputsp


      subroutine mpputgp(yn,p,kdim,klev)
      implicit none
      character (len=*) :: yn
      integer kdim,klev
      real :: p(kdim,klev)
      call put_restart_array(yn,p,kdim,kdim,klev)
      return
      end subroutine mpputgp


      subroutine mpsurfgp(yn,p,kdim,klev)
      implicit none
      character (len=*) :: yn
      integer kdim,klev,iread
      real :: p(kdim,klev)
      call get_surf_array(yn,p,kdim,klev,iread)
      return
      end subroutine mpsurfgp


      subroutine mpmaxval(p,kdim,klev,pmax)
      implicit none
      integer kdim,klev
      real pmax
      real :: p(kdim,klev)
      pmax = maxval(p(:,:))
      return
      end subroutine mpmaxval


      subroutine mpsumval(p,kdim,klev,psum)
      implicit none
      integer klev,kdim
      real psum
      real :: p(kdim,klev)
      psum = sum(p(:,:))
      return
      end subroutine mpsumval

