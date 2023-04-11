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
      real zw, zw1, zv, dn
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
c SKT updated namelist replacements to use longitude of perihleion rather than gamma
c     and calculate gamma below 
c SKT osctau1 = 0.0 does not make sense; this should be 2.5 for the time of perihelion
c     (from start of calendar year)
      osce=par_orbit_osce
      oscsob=par_orbit_oscsob
      zw=par_orbit_osclonperi
      

c pbh this if statement removed so that transient orbital forcing can be used without ents
c      if ((ents_radfor.eq.'y').or.(ents_radfor.eq.'Y')) then
c PV (05/10/2022) adjust the orbits file to take in the longitude of perihelion (anti-clockwise angle
c     between the vernal equinox and perihelion from heliocentric perspective), rather than 
c     the mean longitude converted  from heliocentric to geocentric perspective

      if ((orbit_radfor.eq.'y').or.(orbit_radfor.eq.'Y')) then
   
      if (t_orbit.eq.2) then
        osce=orbitecc_vect(1)
        oscsob=orbitobl_vect(1)
        zw=orbitlonperi_vect(1)
        
        print*,'orbitvars:',iistep
        print*,'orbitosce,oscsob:',osce,oscsob
        print*,'orbitlonperi',zw
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

          if (abs(orbitlonperi_vect(time_1)-
     &        orbitlonperi_vect(time_2)).gt.pi) then

            if (orbitlonperi_vect(time_1)
     &          .gt.orbitlonperi_vect(time_2)) then
              zw=mod((1-time_frac)*orbitlonperi_vect(time_1)+
     &                time_frac*(orbitlonperi_vect(time_2)+2*pi),2*pi)
            else
              zw=mod((1-time_frac)*(orbitlonperi_vect(time_1)+2*pi)+
     &                time_frac*(orbitlonperi_vect(time_2)),2*pi)
            endif
  
          else
          zw=(1-time_frac)*orbitlonperi_vect(time_1)+
     &                time_frac*orbitlonperi_vect(time_2)
          endif


        else 
          if (time_frac.ne.0) print*,'Time out of bounds for orbit'
          osce=orbitecc_vect(norbit)
          oscsob=orbitobl_vect(norbit)
          zw=orbitlonperi_vect(norbit)
        endif

      if (mod(iistep-1,10000).eq.0) then
         if (debug_loop) then
            print*,'orbitvars:',iistep,time_1,time_frac
            print*,'orbitosce,oscsob:',osce,oscsob
            print*,'orbitlonperi',zw
         endif
      endif

      endif
      
      endif
      
c skt calculate oscgam (gamma) from par_orbit_osclonperi (longitude of perihelion)
c     gamma = pi - longitude of perihelion (rad) and must be nonnegative, so first add 180
c PV (05/10/2022)  zw is the anti-clockwise angle between the vernal equinox and perihelion). The steps
c    below are needed to convert heliocentric to geocentric perspective (convert to zv)
c PV (13/10/2022) moved downward so the fixed orbital parameters and those in orbits file are
c     all subjected to the same calculations
      zw1=pi+zw
      if (zw1.gt.2*pi) then
         zw1=zw1-(2*pi)
      end if
      zv=(2*pi)-zw1
 
c PV (05/10/2022) convert the true geocentric longitude of perihelion to the mean longitude (oscgam)
      oscgam = zv
      do i=1,10
         oscgam =zv
     &      - (2.0*osce - 0.25*osce*osce*osce)
     &      *sin(oscgam)- 1.25*osce*osce*sin(2.*oscgam)
     &      - 13./12.*osce*osce*osce*sin(3.*oscgam)
      enddo
c PV (05/10/2022) convert mean longitude from radians to days (days between perihelion and vernal equinox)
      dn = oscgam*gn_daysperyear/(2*pi)
c PV (05/10/2022) cGENIE starts at Jan 1st, not the vernal equinox on March 21, so substract 79.265 days from
c     dn so that the first timestep always occurs ~80 days from the vernal equinox
      dn = dn - 79.265
c PV (05/10/2022) calculate the date of perihelion from Jan. 1st and make sure the days fall within 0-365.25 range
      osctau0 = -dn
      if (osctau0 < 0) then
	     osctau0 = gn_daysperyear+osctau0
      endif
      
c pbh conditionality on ents_radfor has been removed
c      endif

c SG < end of orbit modifications

c     open(1,file='oscsun.dat')

      rpi = 1.0/pi

c SKT terms for solar insolation calculations
      tv = osce*osce
      osce1 = osce * (2.0 - 0.25*tv)
      osce2 = 1.25 * tv 
      osce3 = osce*tv * 13./12.
c PV (05/10/2022) correcton for osce4 for consistency with other codes
c      old osce4 = ((1.0 + 0.5*tv)/(1.0 - tv))**2 
      osce4 = ((1.0)/(1.0 - tv))**2 
      oscryr = 2.0*pi/float(nyear)

c pbh
c SKT osctau1 is the time of perihelion and does not equal 0!
      osctau1 = osctau0 + 0.5
 
      do istep=1,nyear

c pbh Dan's offset for angular time of year
c         osct = float(mod(istep-1,nyear)+1)*oscryr
c SKT osct = mean anomaly (i.e. fraction of orbital period elapsed since
c     perihelion)
	     osct = (float(mod(istep-1,nyear)+1) - 
     &          (nyear*osctau1/gn_daysperyear))*oscryr
c     oscv = true anomaly calculated from eccentricity and mean anomaly
c          (i.e. the angular distance of the earth round its orbit from perihelion)
	     oscv = osct + osce1*sin(osct) + osce2*sin(2.0*osct) 
     &           + osce3*sin(3.0*osct)
c     oscsolf = insolation at the top of the atmosphere
c             (but without multiplying by the solar constant)
	     oscsolf = osce4*(1.0 + osce*cos(oscv))**2
c     PV (05/10/2022) oscsind = sine of solar declination in terms of the angle 
c       between the vernal equinox and earth, i.e. substract the geocentric (true) 
c       longitude of perihelion(zv) from the true anomaly (angle perihelion-Earth)
	     oscsind = sin(oscsob)*sin(oscv-zv)
c SKT for each latitude, calculate the following:
c     oscss = sin( solar declination) * sin(latitude)
c     oscccc = cos(solar declination)
c     osctt = tan(latitude)*tan(solar declination)
c     oscday = hour angle of sunset (the angle through which earth
c            has rotated between local solar noon and sunset)
c     solfor(j,istep) = incoming solar radiation at each latitude 
c                     (takes TOA insolation and multiples by the 
c                     mean cos(solar zenith angle))                   
         do j=1,jmax
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
c replace variable forcing by its average if seasonallity is OFF (dosc=.false.)
c
      do j=1,jmax
         solavg(j) = 0.
	     alboavg(j) = 0.
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
