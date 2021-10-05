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

c SKT Add isotopes
      real loc_delta_photo
      real loc_alpha_13C,loc_alpha_14C
      real loc_r13C,loc_r14C
      real loc_R13C_atm,loc_R14C_atm
      real loc_r13C_Cbio,loc_r14C_Cbio
      real k1v,k1s,k1a
      real k1v_13C,k1s_13C,k1a_13C
      real k1v_14C,k1s_14C,k1a_14C
      real,dimension(maxi,maxj),intent(in)::torog_atm
      real,dimension(3:5,maxi,maxj)::dum_co2_out
      real,dimension(maxi,maxj),intent(in)::landice_slicemask_lic

c land-atmosphere carbon exchange fluxes
      real,intent(inout),dimension(3:5,maxi,maxj)::sfxatm_lnd

      integer i,j
 
c Debugging
c     real,dimension(maxi,maxj)::test

cmsw Work out CO2 conc according to atchem (ppmv)

c The following statement assumes spatially uniform pCO2 in the atmosphere
c TODO: compute global average pCO2
c SKT Add isotope values
      pco2ld=dum_co2_out(3,1,1)*rmtp
      pco2_13C=dum_co2_out(4,1,1)*rmtp
      pco2_14C=dum_co2_out(5,1,1)*rmtp
      loc_r13C=pco2_13C/pco2ld
      loc_r14C=pco2_14C/pco2ld

c SKT Constant photosynthetic carbon isotope discrimination from Farquhar (1989)
c SKT This is from epsilon from CO2 to Cbio
      loc_delta_photo=20.1
c SKT CO2-dependent fractionation from Schubert and Jahren 2012
c      loc_delta_photo=(28.26*0.35*(pco2ld+15))/(28.26+0.35*(pco2ld+15))
c Calculate alpha, ratio R, and fractional abundance, r
c 13C
      loc_alpha_13C = 1+loc_delta_photo/1000.0
      loc_R13C_atm=loc_r13C/(1-loc_r13C)
      loc_r13C_Cbio=((1/loc_alpha_13C)*loc_R13C_atm)/
     &         (1.0 + (1/loc_alpha_13C)*loc_R13C_atm)

c 14C
      loc_alpha_14C = 1+2.0*loc_delta_photo/1000.0
      loc_R14C_atm=loc_r14C/(1-loc_r14C)
      loc_r14C_Cbio=((1/loc_alpha_14C)*loc_R14C_atm)/
     &         (1.0 + (1/loc_alpha_14C)*loc_R14C_atm)
     
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
 
            photo_13C(i,j)=loc_r13C_Cbio*photo(i,j)
            photo_14C(i,j)=loc_r14C_Cbio*photo(i,j)          

cmsw Calculate plant respiration (kgC/m2/yr)

            call veg_resp(Cveg(i,j),tair(i,j),respveg(i,j))
            respveg_13C(i,j)=loc_r13C_Cbio*respveg(i,j)
            respveg_14C(i,j)=loc_r14C_Cbio*respveg(i,j)

cmsw Calculate leaf turnover (kgC/m2/yr)

            call leaf_litter(Cveg(i,j),photo(i,j),
     &                       respveg(i,j),
     &                       epsv(i,j),leaf(i,j))
            leaf_13C(i,j)=loc_r13C_Cbio*leaf(i,j)
            leaf_14C(i,j)=loc_r14C_Cbio*leaf(i,j)


cmsw Calculate soil respiration (kgC/m2/yr) using land temp.

            call soil_resp(Csoil(i,j),tlnd(i,j),respsoil(i,j)) 
            respsoil_13C(i,j)=loc_r13C_Cbio*respsoil(i,j)
            respsoil_14C(i,j)=loc_r14C_Cbio*respsoil(i,j)


cmsw vegetation carbon ODE

            k1v=dtland*(photo(i,j)-respveg(i,j)-leaf(i,j))
            k1v_13C=dtland*(photo_13C(i,j)-respveg_13C(i,j)-
     &          leaf_13C(i,j))
            k1v_14C=dtland*(photo_14C(i,j)-respveg_14C(i,j)-
     &          leaf_14C(i,j))


cmsw soil carbon ODE

            k1s=dtland*(leaf(i,j)-respsoil(i,j))
            k1s_13C=dtland*(leaf_13C(i,j)-respsoil_13C(i,j))
            k1s_14C=dtland*(leaf_14C(i,j)-respsoil_14C(i,j))


cmsw atmospheric carbon ODE

            k1a=dtland*(-photo(i,j)+respveg(i,j)+respsoil(i,j))
            k1a_13C=dtland*(-photo_13C(i,j)+respveg_13C(i,j)
     &         +respsoil_13C(i,j))
            k1a_14C=dtland*(-photo_14C(i,j)+respveg_14C(i,j)
     &         +respsoil_14C(i,j))


c SKT Find initial isotope inventories
           Cveg_13C(i,j)=loc_r13C_Cbio*Cveg(i,j)
           Csoil_13C(i,j)=loc_r13C_Cbio*Csoil(i,j)
           Cveg_14C(i,j)=loc_r14C_Cbio*Cveg(i,j)
           Csoil_14C(i,j)=loc_r14C_Cbio*Csoil(i,j)

cmsw Euler method update (kg/m2)

            Cveg(i,j)=Cveg(i,j)+k1v
            Csoil(i,j)=Csoil(i,j)+k1s

c SKT update isotopes
            
            Cveg_13C(i,j)=Cveg_13C(i,j)+k1v_13C
            Csoil_13C(i,j)=Csoil_13C(i,j)+k1s_13C
            Cveg_14C(i,j)=Cveg_14C(i,j)+k1v_14C
            Csoil_14C(i,j)=Csoil_14C(i,j)+k1s_14C
          

            if (atchem_update) then
cmsw Calculate CO2 fluxes for atchem coupler (mol/m2/s)
               sfxatm_lnd(3,i,j) = (-photo(i,j)+respveg(i,j)+
     &              respsoil(i,j))*rmu*rsyr
               sfxatm_lnd(4,i,j) = (-photo_13C(i,j)+
     &              respveg_13C(i,j)+
     &              respsoil_13C(i,j))*rmu*rsyr
c               sfxatm_lnd(5,i,j) = (-photo_14C(i,j)+
c     &              respveg_14C(i,j)+
c     &              respsoil_14C(i,j))*rmu*rsyr



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
