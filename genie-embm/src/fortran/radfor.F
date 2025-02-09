c radfor.F fortran routine to calculate radiative forcing for c-goldstein
c started 30/5/3 Neil R. Edwards
c loosely based on Peter Cox's SUNNY.f or see Peixoto and Oort 1992 p.99
c
c nyear = no. dt per year
c osct  = angular time of year (0,..,2pi)
c oscsind = sin of declination 
c oscday = 0.5 * sunlit (angular) fraction of day, ie 1/2 length of day
c solfor = solar forcing = scaling factor * integral of cosine of solar 
c          elevation during daylight
c
c ----- if opt_orbit_old=TRUE, use old definitions 
c    osce = eccentricity 
c    oscsob = sin of obliquity 
c    oscgam = MEAN longitude (after Berger 1978) (in deg)
c    osctau0 = date of perihelion from Jan 1st (in days)
c ----- if opt_orbit_old=FALSE, use new definitions 
c    osce = eccentricity 
c    oscobl = obliquity (in rad)
c    osclonperi = TRUE longitude of perihelon (in rad)
c NOTE: the new orbits code contains (1) corrections for osce4 used to
c       calculate the insolation as a function of ecc; corrections for 
c      (2) calculation of solar declination, and for (3) applying
c      consistency between fixed and time-varying orbits

      subroutine radfor (iistep,gn_daysperyear,solconst,flag_ents)

#include "embm.cmn"

      real solconst

      integer istep, j
      real rpi, osce, oscsob, oscgam, tv, osce1, osce2, osce3, osce4
      real oscobl,osclonperi	  
      real oscryr, osct, oscv, oscsolf, oscsind, oscss, osccc, osctt 
      real oscday
      real zw, zw1, zv, zm, dn
      integer i

c SG (16/02/2K7) orbital variables
      integer iistep
      integer time_1,time_2
      real time_frac,osctau0,osctau1
      real gn_daysperyear

      real solavg(maxj)
      real alboavg(maxj)
      
      logical flag_ents

c SG (16/02/2K7): Changed static declaration of osce,oscsob, oscgam
c    parameter(osce=0.0167, oscsob=0.397789, oscgam=1.352631)
c pbh initialise osctau0 for angular time of year.
c    value of osctau0=-0.5 (=> osctau1=0.0) reproduces old orbital calculation
c ajr namelist replacements
c PV [17/04/2023] add conditional to check if OLD or NEW orbit code should be applied

c >>>>>>>>>> OLD ORBITS CODE STARTS HERE >>>>>>>>>>>>>>>>>>>>>>>>>
c >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

      if (opt_orbit_old) then

      osce=par_orbit_osce
      oscsob=par_orbit_oscsob
      osctau0=par_orbit_osctau0
      zw=par_orbit_oscgam

c skt calculate oscgam (mean longitude) from par_orbit_oscgam                                                                            
      if (zw.eq.99999) then
      oscgam=1.352631
      else
      zw1=180.+zw
      if (zw1.gt.360.0) then
      zw1=zw1-360.0
      end if
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
        oscsob=orbitsob_vect(1)
        oscgam=orbitgam_vect(1)
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
          oscsob=(1-time_frac)*orbitsob_vect(time_1)+
     &                time_frac*orbitsob_vect(time_2)

          if (abs(orbitgam_vect(time_1)-
     &        orbitgam_vect(time_2)).gt.pi) then

            if (orbitgam_vect(time_1).gt.orbitgam_vect(time_2)) then
              oscgam=mod((1-time_frac)*orbitgam_vect(time_1)+
     &                time_frac*(orbitgam_vect(time_2)+2*pi),2*pi)
            else
              oscgam=mod((1-time_frac)*(orbitgam_vect(time_1)+2*pi)+
     &                time_frac*(orbitgam_vect(time_2)),2*pi)
            endif
  
          else
          oscgam=(1-time_frac)*orbitgam_vect(time_1)+
     &                time_frac*orbitgam_vect(time_2)
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
          oscsob=orbitsob_vect(norbit)
          oscgam=orbitgam_vect(norbit)
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
	  
c >>>>>>>>>> NEW ORBITS CODE STARTS HERE >>>>>>>>>>>>>>>>>>>>>>>>>
c >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c PV if opt_orbit_old=false, do the below	
  
      else
		 
        osce=par_orbit_osce
        oscobl=par_orbit_oscobl
        zw=par_orbit_osclonperi
		
c PV (05/10/2022) orbits take TRUE longitude of perihelion (anti-clockwise angle
c     between vernal equinox and perihelion from heliocentric perspective), rather 
c than MEAN longitude converted from heliocentric to geocentric perspective

      if ((orbit_radfor.eq.'y').or.(orbit_radfor.eq.'Y')) then
   
      if (t_orbit.eq.2) then
        osce=orbitecc_vect(1)
        oscobl=orbitobl_vect(1)
        zw=orbitper_vect(1)
        
        print*,'orbitvars:',iistep
        print*,'orbitosce,oscobl:',osce,oscobl
        print*,'orbitlonperi',zw
      endif

      if (t_orbit.eq.1) then
        time_1=int(iistep/real(orbitsteps))+1
        time_2=time_1+1
        time_frac=(mod(iistep,orbitsteps))/real(orbitsteps)
        if (time_2.le.norbit) then
          osce=(1-time_frac)*orbitecc_vect(time_1)+
     &                time_frac*orbitecc_vect(time_2)
          oscobl=(1-time_frac)*orbitobl_vect(time_1)+
     &                time_frac*orbitobl_vect(time_2)

          if (abs(orbitper_vect(time_1)-
     &        orbitper_vect(time_2)).gt.pi) then

            if (orbitper_vect(time_1)
     &          .gt.orbitper_vect(time_2)) then
              zw=mod((1-time_frac)*orbitper_vect(time_1)+
     &                time_frac*(orbitper_vect(time_2)+2*pi),2*pi)
            else
              zw=mod((1-time_frac)*(orbitper_vect(time_1)+2*pi)+
     &                time_frac*(orbitper_vect(time_2)),2*pi)
            endif
  
          else
          zw=(1-time_frac)*orbitper_vect(time_1)+
     &                time_frac*orbitper_vect(time_2)
          endif


        else 
          if (time_frac.ne.0) print*,'Time out of bounds for orbit'
          osce=orbitecc_vect(norbit)
          oscobl=orbitobl_vect(norbit)
          zw=orbitper_vect(norbit)
        endif

      if (mod(iistep-1,10000).eq.0) then
         if (debug_loop) then
            print*,'orbitvars:',iistep,time_1,time_frac
            print*,'orbitosce,oscobl:',osce,oscobl
            print*,'orbitlonperi',zw
         endif
      endif
      endif
      endif
	  
c SKT calculate oscgam (gamma) from par_orbit_osclonperi (longitude of perihelion)
c     gamma = pi - longitude of perihelion (rad) and must be nonnegative, so first add 180
c PV (05/10/2022) zw=anti-clockwise angle between the vernal equinox and perihelion. Steps
c    below convert heliocentric to geocentric perspective (convert to zv)
c PV (13/10/2022) fixed and time-varying orbital parameters are now subjected to same calculations
      zw1=pi+zw
      if (zw1.gt.2*pi) then
         zw1=zw1-(2*pi)
      end if
      zv=(2*pi)-zw1
 
c PV (05/10/2022) convert true geocentric longitude of perihelion to the mean longitude (oscgam)
      oscgam = zv
      do i=1,10
         oscgam =zv
     &      - (2.0*osce - 0.25*osce*osce*osce)
     &      *sin(oscgam)- 1.25*osce*osce*sin(2.*oscgam)
     &      - 13./12.*osce*osce*osce*sin(3.*oscgam)
      enddo
c PV (05/10/2022) convert mean longitude from radians to days (days between perihelion & vernal equinox)
      dn = oscgam*gn_daysperyear/(2*pi)
c PV (05/10/2022) cGENIE starts at Jan 1st, not the vernal equinox on March 21, so substract 79.265 days 
c from dn so that the first timestep always occurs ~80 days from the vernal equinox
      dn = dn - 79.265
c PV (05/10/2022) calculate the date of perihelion from Jan. 1st and make sure the days fall 
c within the 0-365.25 day range
      osctau0 = -dn
      if (osctau0 < 0) then
	     osctau0 = gn_daysperyear+osctau0
      endif

c SKT terms for solar insolation calculations	  
      rpi = 1.0/pi
      tv = osce*osce
      osce1 = osce * (2.0 - 0.25*tv)
      osce2 = 1.25 * tv 
      osce3 = osce*tv * 13./12.
c PV (05/10/2022) correcton for osce4 
      osce4 = ((1.0)/(1.0 - tv))**2
      oscryr = 2.0*pi/float(nyear)

c SKT osctau1 is the time of perihelion and does not equal 0!
      osctau1 = osctau0 + 0.5
	  
      do istep=1,nyear

c SKT osct = mean anomaly (fraction of orbital period elapsed since perihelion)
	     osct = (float(mod(istep-1,nyear)+1) - 
     &          (nyear*osctau1/gn_daysperyear))*oscryr
c oscv = true anomaly calculated from eccentricity and mean anomaly
c       (i.e. the angular distance of the earth around its orbit from perihelion)
	     oscv = osct + osce1*sin(osct) + osce2*sin(2.0*osct) 
     &           + osce3*sin(3.0*osct)
c oscsolf = insolation at the top of the atmosphere
c             (but without multiplying by the solar constant)
	     oscsolf = osce4*(1.0 + osce*cos(oscv))**2
c PV (05/10/2022) oscsind = sine of solar declination in terms of the angle 
c       between the vernal equinox and earth, i.e. substract the geocentric (true) 
c       longitude of perihelion(zv) from the true anomaly (angle perihelion-Earth)
	     oscsind = sin(oscobl)*sin(oscv-zv)
c SKT for each latitude, calculate the following:
c     oscss = sin(solar declination) * sin(latitude)
c     oscccc = cos(solar declination)
c     osctt = tan(latitude)*tan(solar declination)
c     oscday = hour angle of sunset (the angle through which earth
c            has rotated between local solar noon and sunset)
c     solfor(j,istep) = incoming solar radiation at each latitude 
c            (TOA insolation multiplied by mean cos(solar zenith angle))                   
         do j=1,jmax
            oscss = oscsind * s(j)
            osccc = sqrt(1.0 - oscsind**2) * c(j)
            osctt = min(1.0,max(-1.0,oscss/osccc))
            oscday = acos(- osctt)
            solfor(j,istep) = solconst*oscsolf*rpi*
     &                       (oscss*oscday + osccc*sin(oscday))
            if (flag_ents) then
                call ocean_alb(oscss,osccc,oscday,j,istep)
            endif
         enddo
      enddo
	  
		
      endif
C PV >>>>>>>>> NEW ORBITS CODE ENDS HERE >>>>>>>>>>>>>>>>>>>>>>>>>
c >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

c SEASONALITY 

      if (dosc) then
	  
      else
c replace variable forcing by its average if seasons are disabled (dosc=false)
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
