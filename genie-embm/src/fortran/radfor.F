c radfor.F fortran routine to calculate radiative forcing for c-goldstein
c started 30/5/3 Neil R. Edwards
c loosely based on Peter Cox's SUNNY.f or see Peixoto and Oort 1992 p.99
c
c nyear = no. dt per year
c osct  = angular time of year (0,..,2pi)
c oscsind = sin of declination (not an input)
c oscsob = sin of obliquity (input)
c osce = eccentricity (input)
c oscgam = mean longitude (after Berger 1978)
c oscday = 0.5 * sunlit (angular) fraction of day, ie 1/2 length of day
c solfor = solar forcing = scaling factor * integral of cosine of solar 
c          elevation during daylight
c

      subroutine radfor (iistep,gn_daysperyear,solconst,flag_ents)

#include "embm.cmn"

      real solconst

      integer istep, j
      real rpi, osce, oscsob, oscgam, tv, osce1, osce2, osce3, osce4
      real oscryr, osct, oscv, oscsolf, oscsind, oscss, osccc, osctt 
      real oscday
      real zw, zw1, zv, zm
      integer i

c SG (16/02/2K7) orbital variables
      integer iistep
      integer time_1,time_2
      real time_frac,osctau0,osctau1
      real gn_daysperyear

      real solavg(maxj)
      real alboavg(maxj)
      
      logical flag_ents

c      parameter(osce=0.0167, oscsob=0.397789, oscgam=1.352631)
c SG (16/02/2K7) : Changed static declaration of osce,oscsob
c & oscgam.
ccc      osce=0.0167
ccc      oscsob=0.397789
ccc      oscgam=1.352631
c pbh initialise osctau0 for angular time of year.
c value of osctau0=-0.5 (=> osctau1=0.0) reproduces old orbital calculation
ccc      osctau0=-0.5
c ajr namelist replacements
      osce=par_orbit_osce
      oscsob=par_orbit_oscsob
      osctau0=par_orbit_osctau0
      zw=par_orbit_oscgam

c skt calculate oscgam from AJR par_orbit_oscgam                                                                            
      if (zw.eq.99999) then
      oscgam=1.352631
      else
      zw1=180.+zw
      if (zw1.gt.360.0) zw1=zw-360.0
      zv=pi/180.*(360.-zw1)
      zm=zv
      do i=1,10
         zm=zv
     :      - (2.0*osce - 0.25*osce*osce*osce)
     :      *sin(zm)- 1.25*osce*osce*sin(2.*zm)
     :      - 13./12.*osce*osce*osce*sin(3.*zm)
      enddo
      oscgam=0.5*(zv+zm)
      end if

c pbh this if statement removed so that transient orbital forcing can be used without ents
c      if ((ents_radfor.eq.'y').or.(ents_radfor.eq.'Y')) then

      if ((orbit_radfor.eq.'y').or.(orbit_radfor.eq.'Y')) then
   
      if (t_orbit.eq.2) then
        osce=orbitecc_vect(1)
        oscsob=orbitobl_vect(1)
        oscgam=orbitpre_vect(1)
        osctau0=orbittau_vect(1)
        print*,'orbitvars:',iistep
        print*,'orbitosce,oscsob:',osce,oscsob
        print*,'orbitoscgam,orbitosctau0',oscgam,osctau0
      endif

      if (t_orbit.eq.1) then
        time_1=int(iistep/real(orbitsteps))+1
        time_2=time_1+1
        time_frac=(mod(iistep,orbitsteps))/real(orbitsteps)
        if (time_2.le.norbit) then
          osce=(1-time_frac)*orbitecc_vect(time_1)+
     &                time_frac*orbitecc_vect(time_2)
          oscsob=(1-time_frac)*orbitobl_vect(time_1)+
     &                time_frac*orbitobl_vect(time_2)

          if (abs(orbitpre_vect(time_1)-
     &        orbitpre_vect(time_2)).gt.pi) then

            if (orbitpre_vect(time_1).gt.orbitpre_vect(time_2)) then
              oscgam=mod((1-time_frac)*orbitpre_vect(time_1)+
     &                time_frac*(orbitpre_vect(time_2)+2*pi),2*pi)
            else
              oscgam=mod((1-time_frac)*(orbitpre_vect(time_1)+2*pi)+
     &                time_frac*(orbitpre_vect(time_2)),2*pi)
            endif
  
          else
          oscgam=(1-time_frac)*orbitpre_vect(time_1)+
     &                time_frac*orbitpre_vect(time_2)
          endif

          if (abs(orbittau_vect(time_1)-
     &        orbittau_vect(time_2)).gt.gn_daysperyear/2.0) then

            if (orbittau_vect(time_1).gt.orbittau_vect(time_2)) then
              osctau0=mod((1-time_frac)*orbittau_vect(time_1)+
     &                time_frac*(orbittau_vect(time_2)+
     &                gn_daysperyear),gn_daysperyear)
            else
              osctau0=mod((1-time_frac)*
     &                (orbittau_vect(time_1)+gn_daysperyear)+
     &                time_frac*(orbittau_vect(time_2)),gn_daysperyear)
            endif


          else
          osctau0=(1-time_frac)*orbittau_vect(time_1)+
     &                time_frac*orbittau_vect(time_2)
          endif

        else 
          if (time_frac.ne.0) print*,'Time out of bounds for orbit'
          osce=orbitecc_vect(norbit)
          oscsob=orbitobl_vect(norbit)
          oscgam=orbitpre_vect(norbit)
          osctau0=orbittau_vect(norbit)
        endif

      if (mod(iistep-1,10000).eq.0) then
         if (debug_loop) then
            print*,'orbitvars:',iistep,time_1,time_frac
            print*,'orbitosce,oscsob:',osce,oscsob
            print*,'orbitoscgam,orbitosctau0',oscgam,osctau0
         endif
      endif

      endif
      endif
c pbh conditionality on ents_radfor has been removed
c      endif

c SG < end of orbit modifications

c     open(1,file='oscsun.dat')

      rpi = 1.0/pi

      tv = osce*osce
      osce1 = osce * (2.0 - 0.25*tv)
      osce2 = 1.25 * tv 
      osce3 = osce*tv * 13./12.
      osce4 = ((1.0 + 0.5*tv)/(1.0 - tv))**2 
      oscryr = 2.0*pi/float(nyear)

c pbh
      osctau1 = osctau0 + 0.5
 
      do istep=1,nyear

c pbh Dan's offset for angular time of year
c         osct = float(mod(istep-1,nyear)+1)*oscryr
         osct = (float(mod(istep-1,nyear)+1) - 
     &          (nyear*osctau1/gn_daysperyear))*oscryr

         do j=1,jmax
            oscv = osct + osce1*sin(osct) + osce2*sin(2.0*osct) 
     &           + osce3*sin(3.0*osct)
            oscsolf = osce4*(1.0 + osce*cos(oscv))**2
            oscsind = oscsob*sin(oscv-oscgam)

            oscss = oscsind * s(j)
            osccc = sqrt(1.0 - oscsind**2) * c(j)
            osctt = min(1.0,max(-1.0,oscss/osccc))
            
            oscday = acos(- osctt)

            solfor(j,istep) = solconst*oscsolf*rpi*
     &                       (oscss*oscday + osccc*sin(oscday))
c SG > ENTS albedo scheme mod
         if (flag_ents) then
            call ocean_alb(oscss,osccc,oscday,j,istep)
         endif
c SG <
c           write(1,'(e15.5)')solfor(j,istep)
         enddo
      enddo

      if (dosc) then
      else
c
c replace variable forcing by its average
c
      do j=1,jmax
         solavg(j) = 0.
         do istep=1,nyear
            solavg(j) = solavg(j) + solfor(j,istep)
c SG > ENTS albedo scheme mod
         if (flag_ents) then
            alboavg(j) = alboavg(j) + albo(j,istep)
         endif
c SG <
         enddo
      enddo
      do j=1,jmax
         do istep=1,nyear
            solfor(j,istep) = solavg(j)/nyear
c SG > ENTS albedo scheme mod
         if (flag_ents) then
            albo(j,istep) = alboavg(j)/nyear
         endif
c SG <
         enddo
      enddo
      endif

c     close(1)

      end


c      subroutine update_radfor(genie_solconst)
c
c#include "embm.cmn"
c
c      real(rk_in) genie_solconst
cc
c      integer istep, j
cc     
c      do j=1,jmax
c         do istep=1,nyear
c            solfor(j,istep) = (genie_solconst/solconst)*solfor(j,istep)
c         enddo
c      enddo
c
c      end
