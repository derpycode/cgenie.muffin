c     msw
c     msw Calculates fluxes then updates carbon reservoirs
c     msw
      subroutine carbon (
     :     torog_atm,             !< surface air temperature adjusted for orography
     :     sfcatm_lnd,            !< land-atmosphere carbon concentrations
     :     landice_slicemask_lic, !< land ice sheet mask
     :     sfxatm_lnd             !< land-atmosphere carbon exchange fluxes
     :     )
	  
#include "genie_ents.cmn"
#include "var_ents.cmn"     

#ifdef fixedveg
      pco2ld=co2(1,1)*rmtp
#else
      real tair(imax,jmax)
      real tlnd(imax,jmax)
      real qlnd(imax,jmax)

c     SKT Add isotopes
      real loc_delta_photo
      real const_lambda_14C
      parameter (const_lambda_14C = 1./8267.0)
      real loc_fracdecay
      real loc_alpha_13C,loc_alpha_14C
      real loc_r13C_photo,loc_r14C_photo
      real loc_pCO2,loc_pCO2_13C,loc_pCO2_14C
      real loc_r13C_pCO2,loc_r14C_pCO2
      real loc_r13C_Cveg,loc_r14C_Cveg
      real loc_r13C_Csoil,loc_r14C_Csoil
      real loc_R13C,loc_R14C
      real k1v,k1s,k1a
      real k1v_13C,k1s_13C,k1a_13C
      real k1v_14C,k1s_14C,k1a_14C
      real,dimension(maxi,maxj),intent(in)::torog_atm
      real,dimension(3:5,maxi,maxj)::sfcatm_lnd
      real,dimension(maxi,maxj),intent(in)::landice_slicemask_lic

c     land-atmosphere carbon exchange fluxes
      real,intent(inout),dimension(3:5,maxi,maxj)::sfxatm_lnd

      integer i,j

c     msw Start land_pts loop

      do i=1,imax
         do j=1,jmax
	  
c     msw Only calculate for non-ice land points

            if(abs(landice_slicemask_lic(i,j)-1.0).lt.1e-19) then

c     msw Set up temperature and water arrays in Kelvin

               tair(i,j)=torog_atm(i,j)+tzero
               tlnd(i,j)=tqld(1,i,j)+tzero
               qlnd(i,j)=tqld(2,i,j)

c     set up local carbon inventories and isotopic ratios
c     NOTE: convert atmopsheric pCO2 units from atm to uatm (~ ppmv) [rmtp]
c     NOTE: just in case the atmosphere is ever not 'well mixed', use grid-point specific values ...
c           if ENTS is called more often than the mixing routine in ATCHEM, the atmosphere becomes heterogeneous
               loc_pCO2       = rmtp*sfcatm_lnd(3,i,j)
               loc_pCO2_13C   = rmtp*sfcatm_lnd(4,i,j)
               loc_pCO2_14C   = rmtp*sfcatm_lnd(5,i,j)
               loc_r13C_pCO2  = loc_pCO2_13C/loc_pCO2
               loc_r14C_pCO2  = loc_pCO2_14C/loc_pCO2
               loc_r13C_Cveg  = Cveg_13C(i,j)/Cveg(i,j)
               loc_r14C_Cveg  = Cveg_14C(i,j)/Cveg(i,j)
               loc_r13C_Csoil = Csoil_13C(i,j)/Csoil(i,j)
               loc_r14C_Csoil = Csoil_14C(i,j)/Csoil(i,j)
               loc_fracdecay = EXP(-dtland*const_lambda_14C)

c     set isotopic fractionation associated with photosynthesis. epsilon from CO2 to Cbio
c     SKT Constant photosynthetic carbon isotope discrimination from Farquhar (1989)
c     SKT CO2-dependent fractionation from Schubert and Jahren 2012

               select case (trim(par_opt_13Cfrac_veg))
               case ('schubert2012')
                   loc_delta_photo=-(28.26*0.35*(loc_pCO2+15))/
     &                   (28.26+0.35*(loc_pCO2+15))
               case ('fixed')
                   loc_delta_photo=-20.1
               case default
                   loc_delta_photo=-(28.26*0.35*(loc_pCO2+15))/
     &                   (28.26+0.35*(loc_pCO2+15))
               end SELECT
	 
c     Calculate alpha, ratio R, and fractional abundance, r of 13C and 14C
c     NOTE: using BIOGEM notation (and sign convention) for converting R to r

               loc_alpha_13C = 1+loc_delta_photo/1000.0
               loc_R13C=loc_r13C_pCO2/(1-loc_r13C_pCO2)
               loc_r13C_photo=(loc_alpha_13C*loc_R13C)/
     &              (1.0 + loc_alpha_13C*loc_R13C)

               loc_alpha_14C = 1+2.0*loc_delta_photo/1000.0
               loc_R14C=loc_r14C_pCO2/(1-loc_r14C_pCO2)
               loc_r14C_photo=(loc_alpha_14C*loc_R14C)/
     &              (1.0 + loc_alpha_14C*loc_R14C)

c     msw Calculate photosynthesis (kgC/m2/yr)
c     NOTE: fractionation associated with photosynthesis           
               call photosynthesis(Cveg(i,j),tair(i,j),
     &              loc_pCO2,qlnd(i,j),bcap(i,j),
     &              fxsw(i,j),fv(i,j),photo(i,j))
               photo_13C(i,j)=loc_r13C_photo*photo(i,j)
               photo_14C(i,j)=loc_r14C_photo*photo(i,j)          

c     msw Calculate plant respiration (kgC/m2/yr)
c     NOTE: assumes no fractionation associated with respiration
               
               call veg_resp(Cveg(i,j),tair(i,j),respveg(i,j))
               respveg_13C(i,j)=loc_r13C_Cveg*respveg(i,j)
               respveg_14C(i,j)=loc_r14C_Cveg*respveg(i,j)

c     msw Calculate leaf turnover (kgC/m2/yr)
c     NOTE: assumes no fractionation associated with leaf fall
               
               call leaf_litter(Cveg(i,j),photo(i,j),
     &              respveg(i,j),
     &              epsv(i,j),leaf(i,j))
               leaf_13C(i,j)=loc_r13C_Cveg*leaf(i,j)
               leaf_14C(i,j)=loc_r14C_Cveg*leaf(i,j)

c     msw Calculate soil respiration (kgC/m2/yr) using land temp.
c     NOTE: the isotopic loss term depends on the isotopic composition of Csoil

               call soil_resp(Csoil(i,j),tlnd(i,j),respsoil(i,j)) 
               respsoil_13C(i,j)=loc_r13C_Csoil*respsoil(i,j)
               respsoil_14C(i,j)=loc_r14C_Csoil*respsoil(i,j)
               
c     msw vegetation carbon ODE

               k1v=dtland*(photo(i,j)-respveg(i,j)-leaf(i,j))
               k1v_13C=dtland*(photo_13C(i,j)-respveg_13C(i,j)-
     &              leaf_13C(i,j))
               k1v_14C=dtland*(photo_14C(i,j)-respveg_14C(i,j)-
     &              leaf_14C(i,j))

c     msw soil carbon ODE

               k1s=dtland*(leaf(i,j)-respsoil(i,j))
               k1s_13C=dtland*(leaf_13C(i,j)-respsoil_13C(i,j))
               k1s_14C=dtland*(leaf_14C(i,j)-respsoil_14C(i,j))

c     msw atmospheric carbon ODE

               k1a=dtland*(-photo(i,j)+respveg(i,j)+respsoil(i,j))
               k1a_13C=dtland*(-photo_13C(i,j)+respveg_13C(i,j)
     &              +respsoil_13C(i,j))
               k1a_14C=dtland*(-photo_14C(i,j)+respveg_14C(i,j)
     &              +respsoil_14C(i,j))

c     msw Euler method update (kg/m2)

               Cveg(i,j)=Cveg(i,j)+k1v
               Csoil(i,j)=Csoil(i,j)+k1s

c     SKT update isotopes
c     PV radioactive decay of stocks (not fluxes) to account for turnover time  
               
               Cveg_13C(i,j)=Cveg_13C(i,j)+k1v_13C
               Csoil_13C(i,j)=Csoil_13C(i,j)+k1s_13C
               Cveg_14C(i,j)=(Cveg_14C(i,j)+k1v_14C)*loc_fracdecay
               Csoil_14C(i,j)=(Csoil_14C(i,j)+k1s_14C)*loc_fracdecay

c     msw Calculate CO2 fluxes for atchem coupler (mol/m2/s)
               
               if (atchem_update) then
                  sfxatm_lnd(3,i,j) = (-photo(i,j)+respveg(i,j)+
     &                 respsoil(i,j))*rmu*rsyr
                  sfxatm_lnd(4,i,j) = (-photo_13C(i,j)+
     &                 respveg_13C(i,j)+
     &                 respsoil_13C(i,j))*rmu*rsyr
                  sfxatm_lnd(5,i,j) = (-photo_14C(i,j)+
     &                 respveg_14C(i,j)+
     &                 respsoil_14C(i,j))*rmu*rsyr

               endif

c     msw If not a land point don't calculate
               
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
     &                          dum_fxsw,dum_fv,dum_photo)

#include "genie_ents.cmn"
#include "var_ents.cmn"

      real,intent(in):: dum_cveg,dum_tair
      real,intent(in):: dum_pco2,dum_qlnd,dum_bcap
      real,intent(in):: dum_fxsw
      real,intent(out):: dum_fv,dum_photo

      real fta,fws,fco2,par
      real loc_k1

c CO2 limitation term
      if(dum_pco2.ge.k13)then
         fco2=(dum_pco2-k13)/(dum_pco2-k13+k14)
      else
         fco2=0.
      endif
	  
c water stress limitation term
      fws=max(0.,min(1.,((4*dum_qlnd/dum_bcap)-2)))
	  
c temperature limitation term
      select case (trim(par_opt_photo))
	  
        case ('williamson2006')
c         double temperature peak
          fta=((2.**(0.1*(dum_tair-topt))) /
     &    ( (1.+exp(0.3*(dum_tair-k11a)))*
     &    (1.+exp(-0.3*(dum_tair-k12))) ))
     & +  ((2.**(0.1*(dum_tair-topt))) /
     &    ( (1.+exp(0.6*(dum_tair-k11b)))*
     &    (1.+exp(-0.3*(dum_tair-k12))) ))
	 
        case ('paleo')
c         single temperature peak
          fta=((2.**(0.1*(dum_tair-topt))) /
     &    ( (1.+exp(0.3*(dum_tair-k11a)))*
     &    (1.+exp(-0.3*(dum_tair-k12))) ))
	 
        case default
c         double temperature peak
          fta=((2.**(0.1*(dum_tair-topt))) /
     &    ( (1.+exp(0.3*(dum_tair-k11a)))*
     &    (1.+exp(-0.3*(dum_tair-k12))) ))
     & +  ((2.**(0.1*(dum_tair-topt))) /
     &    ( (1.+exp(0.6*(dum_tair-k11b)))*
     &    (1.+exp(-0.3*(dum_tair-k12))) ))
	 
      end SELECT

	  
c vegetation fraction (equivalent to effect of canopy light extinction on LAI)
      dum_fv=max(1.e-5,1.-exp(-k17*dum_cveg))

c calculate photosynthesis [kgC/m2/yr]	  
c PV NOTE: 'williamson2006' uses double temperature peak without light limitation (needs k18)
c PV NOTE: 'paleo' uses single temperature peak with light limitation term (needs eps_max)
      select case (trim(par_opt_photo))
        case ('williamson2006')
            dum_photo=k18*rk19*fco2*fws*fta*dum_fv
        case ('paleo')
            par=0.5*dum_fxsw ! PV photosynthetically active radiation 50% (par) [W/m2]=[J/s/m2]
            par = par/(1e6)*(1./rsyr) ! PV convert [J/s/m2] -> [MJ/yr/m2] to match units to eps_max
            dum_photo=par*eps_max*rk19*fco2*fws*fta*dum_fv
       case default
            dum_photo=k18*rk19*fco2*fws*fta*dum_fv
      end SELECT

      end

cmsw***************************************************************************

cmsw vegetation respiration (kgC/m2/yr)

      subroutine veg_resp(dum_cveg,dum_tair,dum_respveg)

#include "genie_ents.cmn"
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