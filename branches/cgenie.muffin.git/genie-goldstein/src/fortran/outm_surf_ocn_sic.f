c subroutine outm.f writes out data for goldstein last change  6/6/95
c expanded to write out atmos and sea ice data (Bob 10/5/02)
c
c AY (13/07/04) : modified to output surface fluxes
c                 note the organisation of output file (one field after
c                 the other, no clever interlacing)
c
c AY (23/07/04) : modified to reflect new wind speed/stress field
c                 organisation (i.e. flat 2D fields for u and v points)
c
c AY (12/07/05) : removed surplus argument to function
c
      subroutine outm_surf_ocn_sic(unit,
     :        otemp, osaln,
     :        atemp, ashum, apres, 
     :        sich, sica, tice,
     :        windspdxu_atm, windspdyu_atm,
     :        net_sw, net_lw,
     :        albedo_ocn,albedo_sic,
     :        stressxu_ocn, stressyu_ocn, usurf, 
     :        fxlho, fxsho, fxswo, fxlwo, evap_net,
     :        fxlha, fxsha, evap_atm,
     :        dthsic, dtareasic)

#include "ocean.cmn"

      integer i, j, unit

      real otemp(imax,jmax), osaln(imax,jmax),
     :     atemp(imax,jmax), ashum(imax,jmax), apres(imax,jmax),
     :     sich(imax,jmax), sica(imax,jmax), tice(imax,jmax),
     :     windspdxu_atm(imax,jmax), windspdyu_atm(imax,jmax),
     :     net_sw(imax,jmax), net_lw(imax,jmax),
     :     albedo_ocn(imax,jmax), albedo_sic(imax,jmax), 
     :     stressxu_ocn(imax,jmax), stressyu_ocn(imax,jmax),
     :     fxlho(imax,jmax), fxsho(imax,jmax),
     :     fxswo(imax,jmax), fxlwo(imax,jmax), evap_net(imax,jmax),
     :     fxlha(imax,jmax), fxsha(imax,jmax), evap_atm(imax,jmax),
     :     dthsic(imax,jmax), dtareasic(imax,jmax)
c
      real usurf(imax, jmax)
c
c Ocean : surface temperature
      do j=1,jmax
         do i=1,imax
            write(unit,* )otemp(i,j)
         enddo
      enddo
c
c Ocean : surface salinity
      do j=1,jmax
         do i=1,imax
c
c AY (01/08/04) : correct salinity to PSU
            if (k1(i,j).le.kmax) then
               write(unit,* )osaln(i,j)+saln0
            else
               write(unit,* )osaln(i,j)
            endif
         enddo
      enddo
c
c Atmosphere : lowest level temperature
      do j=1,jmax
         do i=1,imax
            write(unit,* )atemp(i,j)
         enddo
      enddo
c
c Atmosphere : lowest level specific humidity
      do j=1,jmax
         do i=1,imax
            write(unit,* )ashum(i,j)
         enddo
      enddo
c
c Atmosphere : lowest level pressure
      do j=1,jmax
         do i=1,imax
            write(unit,* )apres(i,j)
         enddo
      enddo
c
c Sea-ice : height
      do j=1,jmax
         do i=1,imax
            write(unit,* )sich(i,j)
         enddo
      enddo
c
c Sea-ice : fractional area
      do j=1,jmax
         do i=1,imax
            write(unit,* )sica(i,j)
         enddo
      enddo
c
c Sea-ice : surface temperature
      do j=1,jmax
         do i=1,imax
            write(unit,* )tice(i,j)
         enddo
      enddo
c
c Atmosphere : x wind speed at u point
      do j=1,jmax
         do i=1,imax
            write(unit,* )windspdxu_atm(i,j)
         enddo
      enddo
c
c Atmosphere : y wind speed at u point
      do j=1,jmax
         do i=1,imax
            write(unit,* )windspdyu_atm(i,j)
         enddo
      enddo
c
c Atmosphere : net short-wave flux
      do j=1,jmax
         do i=1,imax
            write(unit,* )net_sw(i,j)
         enddo
      enddo
c
c Atmosphere : net long-wave flux
      do j=1,jmax
         do i=1,imax
            write(unit,* )net_lw(i,j)
         enddo
      enddo
c
c Ocean : albedo
      do j=1,jmax
         do i=1,imax
            write(unit,* )albedo_ocn(i,j)
         enddo
      enddo
c
c Sea-ice : albedo
      do j=1,jmax
         do i=1,imax
            write(unit,* )albedo_sic(i,j)
         enddo
      enddo
c
c AY (23/09/04) : no longer calculated in surf_ocn_sic.F
c Ocean and Sea-ice : average albedo
c     do j=1,jmax
c        do i=1,imax
c           write(unit,* )albedo_net(i,j)
c        enddo
c     enddo
c
c Ocean and Sea-ice : average surface temperature
c     do j=1,jmax
c        do i=1,imax
c           write(unit,* )temp_net(i,j)
c        enddo
c     enddo
c
c Ocean : x wind stress at u point
      do j=1,jmax
         do i=1,imax
            write(unit,* )stressxu_ocn(i,j)
         enddo
      enddo
c
c Ocean : y wind stress at u point
      do j=1,jmax
         do i=1,imax
            write(unit,* )stressyu_ocn(i,j)
         enddo
      enddo
c
c Ocean : surface wind speed at tracer point
      do j=1,jmax
         do i=1,imax
            write(unit,* )usurf(i,j)
         enddo
      enddo
c
c Ocean : latent heat flux
      do j=1,jmax
         do i=1,imax
            write(unit,* )fxlho(i,j)
         enddo
      enddo
c
c Ocean : sensible heat flux
      do j=1,jmax
         do i=1,imax
            write(unit,* )fxsho(i,j)
         enddo
      enddo
c
c Ocean : short-wave heat flux
      do j=1,jmax
         do i=1,imax
            write(unit,* )fxswo(i,j)
         enddo
      enddo
c
c Ocean : long-wave heat flux
      do j=1,jmax
         do i=1,imax
            write(unit,* )fxlwo(i,j)
         enddo
      enddo
c
c Ocean : evaporation flux
      do j=1,jmax
         do i=1,imax
            write(unit,* )evap_net(i,j)
         enddo
      enddo
c
c Atmosphere : latent heat flux
      do j=1,jmax
         do i=1,imax
            write(unit,* )fxlha(i,j)
         enddo
      enddo
c
c Atmosphere : sensible heat flux
      do j=1,jmax
         do i=1,imax
            write(unit,* )fxsha(i,j)
         enddo
      enddo
c
c Atmosphere : evaporation flux
      do j=1,jmax
         do i=1,imax
            write(unit,* )evap_atm(i,j)
         enddo
      enddo
c
c Sea-ice : change in sea-ice height
      do j=1,jmax
         do i=1,imax
            write(unit,* )dthsic(i,j)
         enddo
      enddo
c
c Sea-ice : change in sea-ice area
      do j=1,jmax
         do i=1,imax
            write(unit,* )dtareasic(i,j)
         enddo
      enddo

c  10 format(10f10.4)
      end
