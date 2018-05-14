c subroutine outm.f writes out data for goldstein last change  6/6/95
c expanded to write out atmos and sea ice data (Bob 10/5/02)
c
c AY (05/02/04) : modified to output surface fluxes
c                 note the organisation of output file (one field after
c                 the other, no clever interlacing)
c 
c AY (12/07/05) : removed surplus input argument to function

      subroutine outm_surf(unit,co2_in,
     :     albedo_ocn,usurf_ocn,latent_ocn,sensible_ocn,
     :     netsolar_ocn,netlong_ocn,
     :     evap_ocn,pptn_ocn,runoff_ocn,
     :     latent_atm,sensible_atm,
     :     netsolar_atm,netlong_atm,
     :     evap_atm,pptn_atm,
     :     dhght_sic,darea_sic)

#include "embm.cmn"

      integer unit

      real co2_in(imax,jmax), usurf_ocn(imax,jmax)

      real
     :     albedo_ocn(imax,jmax),
     :     latent_ocn(imax,jmax),sensible_ocn(imax,jmax),
     :     netsolar_ocn(imax,jmax),netlong_ocn(imax,jmax),
     :     evap_ocn(imax,jmax),pptn_ocn(imax,jmax),
     :     runoff_ocn(imax,jmax),
     :     latent_atm(imax,jmax), sensible_atm(imax,jmax),
     :     netsolar_atm(imax,jmax),netlong_atm(imax,jmax),
     :     evap_atm(imax,jmax),pptn_atm(imax,jmax),
     :     dhght_sic(imax,jmax),darea_sic(imax,jmax)

c Ocean : latent heat flux
c      do 100 j=1,jmax
c         do 100 i=1,imax
c            write(unit,* )latent_ocn(i,j)
c 100     continue
            write(unit,10 )latent_ocn

c Ocean : sensible heat flux
c      do 110 j=1,jmax
c         do 110 i=1,imax
c            write(unit,* )sensible_ocn(i,j)
c 110     continue
            write(unit,10 )sensible_ocn

c Ocean : net solar heat flux
c      do 120 j=1,jmax
c         do 120 i=1,imax
c            write(unit,* )netsolar_ocn(i,j)
c 120     continue
            write(unit,10 )netsolar_ocn

c Ocean : net long-wave heat flux
c      do 130 j=1,jmax
c         do 130 i=1,imax
c            write(unit,* )netlong_ocn(i,j)
c 130     continue
            write(unit,10 )netlong_ocn

c Ocean : evaporation flux
c      do 140 j=1,jmax
c         do 140 i=1,imax
c            write(unit,* )evap_ocn(i,j)
c 140     continue
            write(unit,10 )evap_ocn

c Ocean : precipitation flux
c      do 150 j=1,jmax
c         do 150 i=1,imax
c            write(unit,* )pptn_ocn(i,j)
c 150     continue
            write(unit,10 )pptn_ocn

c Ocean : runoff flux
c      do 160 j=1,jmax
c         do 160 i=1,imax
c            write(unit,* )runoff_ocn(i,j)
c 160     continue
            write(unit,10 )runoff_ocn

c Ocean : albedo
c      do 170 j=1,jmax
c         do 170 i=1,imax
c            write(unit,* )albedo_ocn(i,j)
c 170     continue
            write(unit,10 )albedo_ocn

c Ocean : surface wind speed
c      do 180 j=1,jmax
c         do 180 i=1,imax
c            write(unit,* )usurf_ocn(i,j)
c 180     continue
            write(unit,10 )usurf_ocn
 
c Atmosphere : latent heat flux
c      do 190 j=1,jmax
c         do 190 i=1,imax
c            write(unit,* )latent_atm(i,j)
c 190     continue
            write(unit,10 )latent_atm

c Atmosphere : sensible heat flux
c      do 200 j=1,jmax
c         do 200 i=1,imax
c            write(unit,* )sensible_atm(i,j)
c 200     continue
            write(unit,10 )sensible_atm

c Atmosphere : net solar heat flux
c      do 210 j=1,jmax
c         do 210 i=1,imax
c            write(unit,* )netsolar_atm(i,j)
c 210     continue
            write(unit,10 )netsolar_atm

c Atmosphere : net long-wave heat flux
c      do 220 j=1,jmax
c         do 220 i=1,imax
c            write(unit,* )netlong_atm(i,j)
c 220     continue
            write(unit,10 )netlong_atm

c Atmosphere : evaporation flux
c      do 230 j=1,jmax
c         do 230 i=1,imax
c            write(unit,* )evap_atm(i,j)
c 230     continue
            write(unit,10 )evap_atm

c Atmosphere : precipitation flux
c      do 240 j=1,jmax
c         do 240 i=1,imax
c            write(unit,* )pptn_atm(i,j)
c 240     continue
            write(unit,10 )pptn_atm

c Atmosphere : pCO2
c      do 250 j=1,jmax
c         do 250 i=1,imax
c            write(unit,* )co2_in(i,j)
c 250     continue
            write(unit,10 )co2_in

c Sea-ice : change in ice height
c      do 260 j=1,jmax
c         do 260 i=1,imax
c            write(unit,* )dhght_sic(i,j)
c 260     continue
           write(unit,10 )dhght_sic

c Sea-ice : change in ice area
c      do 270 j=1,jmax
c         do 270 i=1,imax
c            write(unit,* )darea_sic(i,j)
c 270     continue
            write(unit,10 )darea_sic
         
  10  format(e21.13)
      end
