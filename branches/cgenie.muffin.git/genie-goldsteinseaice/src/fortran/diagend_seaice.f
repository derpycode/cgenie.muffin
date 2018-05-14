c
c diagend.f end-of-run diagnostics for c-goldstein created 2/9/2 nre
c
c tsdata and tqdata contain observational estimates for ts and tq
c err is the mismatch between model and data weighted by errw 
c diagnostic deep temperatures for exact comparison with Jia (2003) 16/6/3
c
c AY (20/01/04) : altered for genie-seaice
c		  references to GOLDSTEIN and EMBM excised
c                 contains many diagnostics - these need to go somewhere!
c
c AY (23/09/04) : upgraded to output sea-ice albedo

      subroutine diagend_seaice

      use genie_util, ONLY: check_unit,check_iostat

#include "seaice.cmn"

c AY (03/12/03) : this looks like an error, I'll correct it
c      real syr
c     parameter(syr = 365*86400)
c     parameter(syr = 365.25*86400)

      integer i, ios

c AY (20/01/04) : the only part of this routine relevant to sea-ice
c                 is the following ...

c Artic ice diag

      print*,'Writing Arctic sea-ice diagnostic file'
c AY (20/01/04)
c     open(28,file='../results/'//lout//'.arcice')
      call check_unit(28,__LINE__,__FILE__)
      open(28,file=outdir_name(1:lenout)//lout//'.arcice',iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      do i=1,imax
         write(28,'(4e14.6)',iostat=ios)varice(1,i,jmax),
     1    varice(2,i,jmax),tice(i,jmax),albice(i,jmax)
         call check_iostat(ios,__LINE__,__FILE__)
      enddo
      close(28,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)

c  100 format(e14.7)

      end
