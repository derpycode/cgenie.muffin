cmsw
cmsw Reads in restarts
cmsw
ccc      subroutine in_ents(unit)
         subroutine in_ents(fname)

c      include '../genie-cgoldstein/var.cmn'
c      include '../genie-simpleland/var_ents.cmn'
#include "genie_ents.cmn"
      include 'var_ents.cmn'
 
      integer i,j,unit
  
        character*(*) fname

cccccccccccccccc FOR NETCDF
        real, allocatable :: mphoto(:,:),mrespveg(:,:),mleaf(:,:),
     : mrespsoil(:,:),mCveg(:,:),mCsoil(:,:),mfv(:,:),mtqld1(:,:),
     : mtqld2(:,:),msnow(:,:)
c
        real mpco2ld
c
c	interface
c
c  	 subroutine netcdf_in_ents(a,p,r,b,c,d,e,f,g,h,k,m,n,o)
c	  character*(*) :: a
c	  real b(:,:), c(:,:), d(:,:), e(:,:), f(:,:), g(:,:),
c     :	  h(:,:), k(:,:), m(:,:), n(:,:)
c	  real o
c	  integer p,r
c	 end subroutine netcdf_in_ents
c
c	end interface	
c
ccccccccccccccccccccccccccccccccccccccccccccccccc  
ccc	REPLACING INPUT RESTART ASCII WITH NETCDF
ccc
ccc      read(unit,*)((photo(i,j),i=1,imax),j=1,jmax)
ccc      read(unit,*)((respveg(i,j),i=1,imax),j=1,jmax)
ccc      read(unit,*)((leaf(i,j),i=1,imax),j=1,jmax)
ccc      read(unit,*)((respsoil(i,j),i=1,imax),j=1,jmax)
ccc      read(unit,*)((Cveg(i,j),i=1,imax),j=1,jmax)
ccc      read(unit,*)((Csoil(i,j),i=1,imax),j=1,jmax)
ccc      read(unit,*)((fv(i,j),i=1,imax),j=1,jmax)
ccc      read(unit,*)((tqld(1,i,j),i=1,imax),j=1,jmax)
ccc      read(unit,*)((tqld(2,i,j),i=1,imax),j=1,jmax)
ccc      read(unit,*)((snow(i,j),i=1,imax),j=1,jmax)
ccc      read(unit,*)pco2ld
ccccccccccccccccccccccccccccccccccccccccccccccc
ccc
ccc	data coming in separate variables

        allocate(mphoto(imax,jmax))
        allocate(mrespveg(imax,jmax))
        allocate(mleaf(imax,jmax))
        allocate(mrespsoil(imax,jmax))
        allocate(mCveg(imax,jmax))
        allocate(mCsoil(imax,jmax))
        allocate(mfv(imax,jmax))
        allocate(mtqld1(imax,jmax))
        allocate(mtqld2(imax,jmax))
        allocate(msnow(imax,jmax))

c	call netcdf_in_ents(fname,imax,jmax,mphoto,mrespveg,mleaf,mrespsoil,
c    :	mCveg,mCsoil,mfv,mtqld1,mtqld2,msnow,mpco2ld)

        photo=mphoto
        respveg=mrespveg
        leaf=mleaf
        respsoil=mrespsoil
        Cveg=mCveg
        Csoil=mCsoil
        fv=mfv
        tqld(1,:,:)=mtqld1
        tqld(2,:,:)=mtqld2
        snow=msnow
        pco2ld=mpco2ld
ccccccccccccccccccccccccccccccccccccccccccccccc

cmsw Initialise water bucket capacity

      do i=1,imax
        do j=1,jmax
          if(ents_k1(i,j).gt.ents_kmax)then
             bcap(i,j)=min(k8,k9+(k10*Csoil(i,j)))
cmsw initial roughness length
             z0(i,j)=max(0.001,kz0*Cveg(i,j))
          endif
        enddo
      enddo

      end
