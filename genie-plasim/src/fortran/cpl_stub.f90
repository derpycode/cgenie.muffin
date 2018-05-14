!     ==================
!     subroutine CLSGINI
!     ==================
!
      subroutine clsgini(kdatim,ktspd,kaomod,pslm,kxa,kya)
      implicit none
!
      real :: pslm(kxa,kya)
      integer :: kdatim(6)
      integer :: ktspd
      integer :: kaomod
      integer :: kxa,kya
!just to get rid of unused variable warnings...
      pslm=pslm
      kdatim=kdatim
      ktspd=ktspd
      kaomod=kaomod
      kxa=kxa
      kya=kya
!
      return
      end subroutine clsgini
!
!     ===================
!     subroutine CLSGSTEP
!     ===================
!
      subroutine clsgstep(kdatim,kstep,psst,ptaux,ptauy,pfresh,proff,&
     &                   pice,pheat,pfldo,kxa,kya)
      implicit none
! PBH note p variables should be arrays... previously (*)
      real :: psst(kxa,kya)      ! atm sst (input)
      real :: ptaux(kxa,kya)     ! atm u-stress (input)
      real :: ptauy(kxa,kya)     ! atm v-stress (input)
      real :: pfresh(kxa,kya)    ! atm fresh water flux (input)
      real :: proff(kxa,kya)     ! runoff (input)
      real :: pice(kxa,kya)      ! atm ice thickness (incl. snow) (input)
      real :: pheat(kxa,kya)     ! atm heat flux (input; not used yet)
      real :: pfldo(kxa,kya)     ! atm deep ocan heat flux (output)
      integer :: kdatim(6) ! date and time
      integer :: kstep     ! current atm time step
      integer :: kxa,kya
!just to get rid of unused variable warnings...
      psst=psst
      ptaux=ptaux
      ptauy=ptauy
      pfresh=pfresh
      proff=proff
      pice=pice
      pheat=pheat
      pfldo=pfldo
      kdatim=kdatim
      kstep=kstep
!
      return
      end subroutine clsgstep
!
!     ===================
!     subroutine CLSGSTOP
!     ===================
!
      subroutine clsgstop
      implicit none
!
      return
      end subroutine clsgstop
