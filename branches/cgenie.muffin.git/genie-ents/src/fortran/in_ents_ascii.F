cmsw
cmsw Reads in restarts from ascii file
cmsw
      subroutine in_ents_ascii(unit,
     :     land_snow_lnd                     !< land snow cover
     :     )
#include "genie_ents.cmn"
#include "var_ents.cmn"
 
      integer i,j,unit

c land snow cover
      real,dimension(maxi,maxj),intent(inout)::land_snow_lnd
  
      read(unit,*)((photo(i,j),i=1,imax),j=1,jmax)
      read(unit,*)((respveg(i,j),i=1,imax),j=1,jmax)
      read(unit,*)((leaf(i,j),i=1,imax),j=1,jmax)
      read(unit,*)((respsoil(i,j),i=1,imax),j=1,jmax)
      read(unit,*)((Cveg(i,j),i=1,imax),j=1,jmax)
      read(unit,*)((Csoil(i,j),i=1,imax),j=1,jmax)
      read(unit,*)((fv(i,j),i=1,imax),j=1,jmax)
      read(unit,*)((tqld(1,i,j),i=1,imax),j=1,jmax)
      read(unit,*)((tqld(2,i,j),i=1,imax),j=1,jmax)
      read(unit,*)((land_snow_lnd(i,j),i=1,imax),j=1,jmax)
      read(unit,*)pco2ld

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
