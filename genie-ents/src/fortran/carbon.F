cmsw
cmsw Calculates fluxes then updates carbon reservoirs
cmsw
      subroutine carbon (
     :     torog_atm,                        !< surface air temperature adjusted for orography
     :     dum_co2_out,
     :     landice_slicemask_lic,            !< land ice sheet mask
     :     sfxatm_lnd                        !< land-atmosphere carbon exchange fluxes
     :     )

#include "genie_ents.cmn"
#include "var_ents.cmn"

#ifdef fixedveg
      pco2ld=co2(1,1)*rmtp
#else
      real tair(imax,jmax)
      real tlnd(imax,jmax)
      real qlnd(imax,jmax)

      real k1v,k1s,k1a
      real,dimension(maxi,maxj),intent(in)::torog_atm
      real,dimension(maxi,maxj)::dum_co2_out
      real,dimension(maxi,maxj),intent(in)::landice_slicemask_lic

c land-atmosphere carbon exchange fluxes
      real,intent(inout),dimension(maxi,maxj)::sfxatm_lnd

      integer i,j

cmsw Work out CO2 conc according to atchem (ppmv)

c The following statement assumes spatially uniform pCO2 in the atmosphere
c TODO: compute global average pCO2
      pco2ld=dum_co2_out(1,1)*rmtp
      
cmsw Start land_pts loop

      do i=1,imax
         do j=1,jmax

cmsw Only calculate for non-ice land points

            if(abs(landice_slicemask_lic(i,j)-1.0).lt.1e-19) then

cmsw Set up temperature and water arrays in Kelvin

            tair(i,j)=torog_atm(i,j)+tzero

            tlnd(i,j)=tqld(1,i,j)+tzero
            qlnd(i,j)=tqld(2,i,j)

cmsw Calculate photosynthesis (kgC/m2/yr)
            
            call photosynthesis(Cveg(i,j),tair(i,j),
     &                          pco2ld,qlnd(i,j),bcap(i,j),
     &                          fv(i,j),photo(i,j))

cmsw Calculate plant respiration (kgC/m2/yr)

            call veg_resp(Cveg(i,j),tair(i,j),respveg(i,j))


cmsw Calculate leaf turnover (kgC/m2/yr)

            call leaf_litter(Cveg(i,j),photo(i,j),
     &                       respveg(i,j),
     &                       epsv(i,j),leaf(i,j))

cmsw Calculate soil respiration (kgC/m2/yr) using land temp.

            call soil_resp(Csoil(i,j),tlnd(i,j),respsoil(i,j))

cmsw vegetation carbon ODE

            k1v=dtland*(photo(i,j)-respveg(i,j)-leaf(i,j))

cmsw soil carbon ODE

            k1s=dtland*(leaf(i,j)-respsoil(i,j))

cmsw atmospheric carbon ODE

            k1a=dtland*(-photo(i,j)+respveg(i,j)+respsoil(i,j))

cmsw Euler method update (kg/m2)

            Cveg(i,j)=Cveg(i,j)+k1v
            Csoil(i,j)=Csoil(i,j)+k1s

            if (atchem_update) then
cmsw Calculate CO2 fluxes for atchem coupler (mol/m2/s)
               sfxatm_lnd(i,j) = (-photo(i,j)+respveg(i,j)+
     &              respsoil(i,j))*rmu*rsyr
            endif

cmsw If not a land point don't calculate
            endif

         enddo
      enddo
       
#endif
      end

cmsw***************************************************************************
cmsw Carbon flux subroutines
cmsw***************************************************************************

cmsw Photosynthesis (kgC/m2/yr)

      subroutine photosynthesis(dum_cveg,dum_tair,
     &                          dum_pco2,dum_qlnd,dum_bcap,
     &                          dum_fv,dum_photo)

#include "genie_ents.cmn"
c      include '../genie-cgoldstein/var.cmn'
#include "var_ents.cmn"

      real,intent(in):: dum_cveg,dum_tair
      real,intent(in):: dum_pco2,dum_qlnd,dum_bcap
      real,intent(out):: dum_fv,dum_photo

      real fta,fws,fco2

      if(dum_pco2.ge.k13)then
         fco2=(dum_pco2-k13)/(dum_pco2-k13+k14)
      else
         fco2=0.
      endif

      fws=max(0.,min(1.,((4*dum_qlnd/dum_bcap)-2)))

      dum_fv=max(1.e-5,1.-exp(-k17*dum_cveg))

      fta=((2.**(0.1*(dum_tair-topt))) /
     &    ( (1.+exp(0.3*(dum_tair-k11)))*
     &    (1.+exp(-0.3*(dum_tair-k12))) ))
     & +  ((2.**(0.1*(dum_tair-topt))) /
     &    ( (1.+exp(0.6*(dum_tair-k11a)))*
     &    (1.+exp(-0.3*(dum_tair-k12))) )) 

      dum_photo=k18*rk19*
     &          fco2*fws*fta*dum_fv

      end

cmsw***************************************************************************

cmsw vegetation respiration (kgC/m2/yr)

      subroutine veg_resp(dum_cveg,dum_tair,dum_respveg)

#include "genie_ents.cmn"
c      include '../genie-cgoldstein/var.cmn'
#include "var_ents.cmn"

      real,intent(in):: dum_cveg,dum_tair
      real,intent(out):: dum_respveg

      real ftrv

      ftrv=rk25*exp(-k20/(k21*dum_tair))

      dum_respveg=k24*ftrv*dum_cveg

      end

cmsw***************************************************************************

cmsw Leaf litter (kgC/m2/yr)

      subroutine leaf_litter(dum_cveg,dum_photo,dum_respveg,
     &                       dum_epsv,dum_leaf)

#include "genie_ents.cmn"
c      include '../genie-cgoldstein/var.cmn'
#include "var_ents.cmn"

      real,intent(in):: dum_cveg,dum_photo,dum_respveg
      real,intent(out):: dum_epsv,dum_leaf

      dum_epsv=1./(1.+exp(k16-dum_cveg))

      dum_leaf=(k26*dum_cveg)+(dum_epsv*(dum_photo-dum_respveg))

      end

cmsw****************************************************************************

cmsw Soil respiration (kgC/m2/yr)

      subroutine soil_resp(dum_csoil,dum_tlnd,dum_respsoil)

#include "genie_ents.cmn"
c      include '../genie-cgoldstein/var.cmn'
#include "var_ents.cmn"

      real,intent(in):: dum_csoil,dum_tlnd
      real,intent(out):: dum_respsoil

      real ftrs

      if(dum_tlnd.ge.tzero)then
         ftrs=exp(-k31/(dum_tlnd-k32))
      else
         ftrs=k0*q10**(0.1*(dum_tlnd-tzero))
      endif

      dum_respsoil=k29*rk30*ftrs*dum_csoil

      end
