* ======================================================================
* surflux.F
* ======================================================================
*
* AY (09/12/03) : restarting conversion of c-GOLDSTEIN into
*                 GOLDSTEIN + EMBM + sea-ice + surflux for genie.f
* 
*                 this code takes pieces from mains.F, gseto.F,
*                 gseta.F and surflux.F
*
* Notation; fx = heat flux, lh = latent, sh = sensible, sw = shortwave/solar,
*           lw = long wave, o = ocean, a = atm., sic = sea ice
*           thus eg fxsho = sensible heat flux into (???) ocean
*           dthsic,dtareasic are rates of change (thermodynamic only) of
*           sea-ice height and area
* 
* Note evap array passed to the ocean is the NET evaporation, NOT the 
* evaporation from the ocean alone, hence renamed
c AY (10/12/03) : output fluxes now dimensional (ie SI)
!
! PPH (01/09/04) : Partially removed use of Dland precompiler options.
* Under genie.F structure the Dland option is unsupported and you are
* strongly advised to not use it.  Instead set flag_land=.true. in the
* job script.
c
* Notes for coupling; for IGCM pptn and atm state variables atemp, ashum
* should become unalterable input fields. Ouput heat flux to atm includes
* both surface fluxes like surface LW (same for IGCM) and interior fluxes 
* like planetary OLWR (not needed for IGCM. Net fluxes are only diagnostic,
* however in componentised, GENIE version.
*
* AY (23/09/04) : upgraded to handle sea-ice albedo
*
* nre add 'time' to arguments and remove 'flag'
*
* AY (12/07/05) : removed surplus input argument to subroutine
*
* RM (16/05/05) : edited for variable sin(lat) resolution (from NRE, 6/12/04)
* 
* JDA 23/07/08 : adding in generalised (annual) time-varying forcing and
* "new" sensitivity method based on olr_adj
*
*** Additional history comments from surflux_ents.F:
*** SG (21/06/2K7) : edited for ENTS modifications. There is considerable code
***                  overlap with surflux.F
***
*** JDA (11/08/08) : editing for consistency with sensitivity and  forcings in surflux.F.
*** Use of forcings implies CO2 is set from a file
*
* SAM 31/1/10: merged surflux.F and surflux_ents.F

      subroutine surflux(istep,
     :     otemp, osaln,
     :     atemp, ashum,
     :     sich, sica, tice, albice,
     :     stressxu_ocn,stressyu_ocn,
     :     stressxv_ocn,stressyv_ocn,
     :     albedo,fxlho,fxsho,
     :     fxswo,fxlwo,
     :     evap_ocn,pptn_ocn,runoff_ocn,runoff_land,
     :     fxlha,fxsha,
     :     fxswa,fxlwa,
     :     evap_atm,pptn_atm,
     :     dthsic,dtareasic,
     :     atmos_lowestlh_atm,
     :     go_solfor,go_fxsw,
     :     dum_n_atm,dum_sfcatm,
     :     eb_ca,gn_daysperyear,
     :     eb_fx0a,eb_fx0o,eb_fxsen,eb_fxlw,
     :     eb_evap,eb_pptn,eb_relh,
     :     eb_uv,eb_usurf,
     :     solconst,
     :     co2_out,ch4_out,n2o_out,
     :     surf_orog_atm,                    !< orography (m)
     :     landice_slicemask_lic,            !< land ice sheet mask
     :     albs_atm,                         !< surface albedo
     :     land_albs_snow_lnd,               !< albedo over land for snowy conditions
     :     land_albs_nosnow_lnd,             !< albedo over land for snow-free conditions
     :     land_snow_lnd,                    !< land snow cover
     :     land_bcap_lnd,                    !< bucket capacity
     :     land_z0_lnd,                      !< roughness length (m)
     :     land_temp_lnd,                    !< land temperature
     :     land_moisture_lnd,                !< land moisture content
     :     flag_ents,                        !< flag for ENTS functionality
     :     lowestlu2_atm,                    !< zonal component of wind speed on u grid
     :     lowestlv3_atm                     !< meridional component of wind speed on v grid
     :     )

      use genie_util, ONLY: check_unit, check_iostat
#include "embm.cmn"
      include 'netcdf_grid.cmn'

c ======================================================================
c Declarations
c ======================================================================

c AY (02/12/03)
c Declare variables passed into this subroutine
      integer istep

      real
     :     otemp(imax,jmax),osaln(imax,jmax),
     :     atemp(imax,jmax),ashum(imax,jmax),
     :     sich(imax,jmax),sica(imax,jmax),tice(imax,jmax),
     :     albice(imax,jmax),
     :     stressxu_ocn(imax,jmax),stressyu_ocn(imax,jmax),
     :     stressxv_ocn(imax,jmax),stressyv_ocn(imax,jmax),
     :     albedo(imax,jmax),fxlho(imax,jmax),
     :     fxsho(imax,jmax),
     :     fxswo(imax,jmax),fxlwo(imax,jmax),
     :     evap_ocn(imax,jmax),pptn_ocn(imax,jmax),
     :     runoff_ocn(imax,jmax),runoff_land(imax,jmax),
     :     fxlha(imax,jmax), fxsha(imax,jmax),
     :     fxswa(imax,jmax),fxlwa(imax,jmax),
     :     evap_atm(imax,jmax),pptn_atm(imax,jmax),
     :     dthsic(imax,jmax),dtareasic(imax,jmax),
     :     co2_out(imax,jmax),ch4_out(imax,jmax),n2o_out(imax,jmax)
c     :     precip_rok(imax,jmax)
c 
      REAL,INTENT(out),DIMENSION(imax,jmax) :: atmos_lowestlh_atm

c Dummy variables to be passed to/from BIOGEM via genie.F
      real go_solfor(jmax)
      real go_fxsw(imax,jmax)
      integer,intent(in)::dum_n_atm
      real,intent(in),dimension(dum_n_atm,imax,jmax)::dum_sfcatm
      
      real,intent(inout)::surf_orog_atm(imax,jmax)

c land ice sheet mask
      real,dimension(maxi,maxj),intent(out)::landice_slicemask_lic

c surface albedo
      real,dimension(maxi,maxj),intent(inout)::albs_atm
      real,dimension(maxi,maxj),intent(in)::land_albs_snow_lnd
      real,dimension(maxi,maxj),intent(in)::land_albs_nosnow_lnd
c land snow cover
      real,dimension(maxi,maxj),intent(inout)::land_snow_lnd
c bucket capacity
      real,dimension(maxi,maxj),intent(inout)::land_bcap_lnd
c roughness length
      real,dimension(maxi,maxj),intent(inout)::land_z0_lnd
c land temperature
      real,dimension(maxi,maxj),intent(inout)::land_temp_lnd
c land moisture content
      real,dimension(maxi,maxj),intent(inout)::land_moisture_lnd

c SG > Dummy variables passed to/from ENTS      
      real orog_exact(imax,jmax)
      real lice_exact(imax,jmax)

c pbh for interpolation between time series d18o input
      real d18o_exact
      real surf_orog_atm_previous(imax,jmax)

c pbh for ice sheet melt runoff
      real ice_runoff(imax,jmax) 

c Variables required for/from ENTS
      real beta,tolld
      real tldlast,dtld,dtld1
      real dfxsens,dfxlw,devap,dqsato
      real dum_talt,dum_hum,dum_pptn
      real qsalt
      real fxswsica
      real,intent(in)::eb_ca(imax,jmax)
      
      integer itld,devapcheck,itldmax
      parameter( itldmax = 21, tolld = 1e-3)
      
      real deltq,talt,lmd

c pbh for orography
      real surf_tq2,surf_qsata

      real eb_fx0a(imax,jmax)
      real eb_fx0o(imax,jmax)
      real eb_fxsen(imax,jmax)
      real eb_fxlw(imax,jmax)
      real eb_evap(imax,jmax)
      real eb_pptn(imax,jmax)
      real eb_relh(imax,jmax)
      real eb_uv(2,imax,jmax)
      real eb_usurf(imax,jmax)

c Existing declarations

      real runoff(imax,jmax)
      real ce,ch,cesic,chsic,rq,tv0,tv,tv1,tv2,tv3,tol
c      real tv4,tv5
      real albsic, fxswsic , fxlwsic, fxsensic , fx0oa, fx0sica
      real qsatsic, zeroc, alw, ticold, cfxsensic, salt, dho, dhsic
      real tieqn, dtieq
c      real dhadj,set0

      real atm_latent, atm_sensible, atm_netsol, atm_netlong
      real atm_latenti, atm_sensiblei, atm_netsoli, atm_netlongi

      real gn_daysperyear
      real solconst

      parameter(zeroc = 273.15)

      integer i, j, iter, itice, istot, imth, ios

c AY (09/03/04) : "time" variables for Dan
#ifdef clock
      integer it1, it2, it3, it4
      real    rt1, rt2
#endif

c jda some local variables used for sensitivity

      real :: meantemp

c AY (05/02/04) : variables for surface flux output routine
      character ext*3, conv_surf*3

c AY (22/10/04) : edit to sea-ice convergence tolerance
c     parameter(itice = 21 , tol=1e-10)
      parameter(itice = 21)

c JDA adding some variables relating to the forcing
      integer my_year,io
      real co2lev,solar,vol,aero
      character header*40
c     radiative forcing terms
      real ch4_term,n2o_term
      real ch4_func

c pbh for time series co2
      integer time_1,time_2
      real time_frac
      real co2_exact

c dimensionalised surflux timestep
      real dtsurfdim,rdtsurfdim

c ENTS functionality
      logical flag_ents

c zonal and meridional wind speed components
      real,dimension(maxi,maxj),intent(inout)::lowestlu2_atm,
     &     lowestlv3_atm

c ------------------------------------------------------------ c
c INITIALIZE VARIABLES
c ------------------------------------------------------------ c
      talt  = 0.0
      lmd = 0.0
      surf_tq2 = 0.0
      surf_qsata = 0.0
      fxswsica = 0.0
      dum_talt = 0.0
      dum_hum = 0.0
      dum_pptn = 0.0
      beta = 0.0
      imth = 1
c ------------------------------------------------------------ c

c dimensionalised surflux timestep
      dtsurfdim = dtatm*ndta*tsc
      rdtsurfdim = 1./dtsurfdim

      istot = istep

c If running on real*4, need to use a lower tolerance than usual
         tol = 1e-10

c pbh initialise ice sheet meltwater
      ice_runoff(:,:)=0.0

      if (flag_ents) then
c SG > istot defined currently as zero
c N.B. RESTARTS WILL NOT WORK!!!
c       imth=mod(istep+istot-1,nyear)+1
       imth=mod(istep+0-1,nyear)+1

c If we have ents running, then calculate the 
c   atmospheric co2 concentration, orog, and icemask  if they're varying.
c      if (t_co2.eq.1) then
c        time_1=int(istot/real(co2steps))+1
c        time_2=time_1+1
c        time_frac=(mod(istot,co2steps))/real(co2steps)
c        if (time_2.le.nco2) then
c          co2_exact=(1-time_frac)*co2_vect(time_1)+
c     &                time_frac*co2_vect(time_2)
c        else 
c          if (time_frac.ne.0) print*,'Time out of bounds for co2'
c          co2_exact=co2_vect(nco2)
c        endif
c        if (mod(istot,10000).eq.0) then
c          print*,'co2:',istot,time_1,time_frac,co2_exact
c        endif
c      endif
      if (t_orog.eq.1) then
        time_1=int(istot/real(orogsteps))+1
        time_2=time_1+1
        time_frac=(mod(istot,orogsteps))/real(orogsteps)
        if (time_2.le.norog) then
          orog_exact(:,:)=(1-time_frac)*orog_vect(:,:,time_1)+
     &                time_frac*orog_vect(:,:,time_2)
        else
          if (time_frac.ne.0) print*,'Time out of bounds for orog'
          orog_exact(:,:)=orog_vect(:,:,norog)
        endif
        if (mod(istot,10000).eq.0) then
          print*,'orog:',istot,time_1,time_frac,
     &        sum(orog_exact)/size(orog_exact)
        endif
      endif

      if (t_lice.eq.1) then
        time_1=int(istot/real(licesteps))+1
        time_2=time_1+1
        time_frac=(mod(istot,licesteps))/real(licesteps)
        if (time_2.le.nlice) then
          lice_exact(:,:)=(1-time_frac)*lice_vect(:,:,time_1)+
     &                time_frac*lice_vect(:,:,time_2)
        else
          if (time_frac.ne.0) print*,'Time out of bounds for lice'
          lice_exact(:,:)=lice_vect(:,:,nlice)
        endif
        if (mod(istot,10000).eq.0) then
          print*,'lice:',istot,time_1,time_frac,
     &        sum(lice_exact)/size(lice_exact)
        endif
      endif

c pbh interpolate d18o for icesheets
      if (t_d18o.eq.1) then
        time_1=int(istot/real(d18osteps))+1
        time_2=time_1+1
        time_frac=(mod(istot,d18osteps))/real(d18osteps)
        if (time_2.le.nd18o) then
          d18o_exact=(1-time_frac)*d18o_vect(time_1)+
     &                time_frac*d18o_vect(time_2)
        else
          if (time_frac.ne.0) print*,'Time out of bounds for d18o'
          d18o_exact=d18o_vect(nlice)
        endif
c d18o orography and icemask.
c store orography at previous timestep to derive meltwater
        surf_orog_atm_previous(:,:)=surf_orog_atm(:,:)
        do i=1,imax
          do j=1,jmax
            if(d18o_ice_thresh(i,j).lt.1.0e-5) then
c ocean
              surf_orog_atm(i,j)=0.0
              landice_slicemask_lic(i,j)=0.0
            else if (d18o_exact.gt.d18o_ice_thresh(i,j)) then
c ice sheet
              surf_orog_atm(i,j)=d18o_orog_min(i,j)+
     &             d18o_orog_grad(i,j)*
     &             (d18o_exact-d18o_ice_thresh(i,j))/
     &             (d18o_exact-d18o_ice_thresh(i,j)+d18o_k)
              landice_slicemask_lic(i,j)=2.0
            else
c land (no ice)
              surf_orog_atm(i,j)=d18o_orog_min(i,j)
              landice_slicemask_lic(i,j)=1.0
            endif
          enddo
        enddo

c pbh ice runoff
        do i=1,imax
          do j=1,jmax
            ice_runoff(iroff(i,j),jroff(i,j))=
     &        ice_runoff(iroff(i,j),jroff(i,j))-
     &        (surf_orog_atm(i,j)-surf_orog_atm_previous(i,j))*
     &        (rhoice/1000.0)*rdtsurfdim*
     &        (asurf(j)/asurf(jroff(i,j)))*
     &        scale_mwfx
          enddo
        enddo

        if (mod(istot,10000).eq.0) then
          if (debug_loop) print*,
     & 'd18o:',istot,time_1,time_frac,d18o_exact
        endif
      endif

      endif

c ======================================================================
c Input field modifications
c ======================================================================

c AY (11/12/03) : This next section executes routines that calculate
c                 usurf from wind stresses.  If the winds are fixed,
c                 as they are with the EMBM, this only needs be run
c                 just prior to the first iteration.  If the winds
c                 are time-variant, as they are with the IGCM, this
c                 will need to be run every time-step.

c AY (19/12/03) : need to calculate wind stresses first
c                 code below taken from gseto.F

      do j=1,jmax
         do i=1,imax
            dztau(1,i,j) = scf*stressxu_ocn(i,j)
     1           /(rh0sc*dsc*usc*fsc)/dzz
            dztau(2,i,j) = scf*stressyu_ocn(i,j)
     1           /(rh0sc*dsc*usc*fsc)/dzz
            dztav(1,i,j) = scf*stressxv_ocn(i,j)
     1           /(rh0sc*dsc*usc*fsc)/dzz
            dztav(2,i,j) = scf*stressyv_ocn(i,j)
     1           /(rh0sc*dsc*usc*fsc)/dzz
            tau(1,i,j) = dztau(1,i,j)*dzz
            tau(2,i,j) = dztav(2,i,j)*dzz

c SG Transfer modified coefficients from ENTS
            if (flag_ents) then
               ca(i,j) = real(eb_ca(i,j),kind(ca))
c LG don't use ents winds when winds are unified
               if (unify_winds .eq. 0) then
                  lowestlu2_atm(i,j)=real(uatml(1,i,j,imth)*usc,
     &                 kind(lowestlu2_atm))
                  lowestlv3_atm(i,j)=real(uatml(2,i,j,imth)*usc,
     &                 kind(lowestlv3_atm))
               endif
               if (unify_winds .eq. 0 .or. unify_winds .eq. 2 ) then
                  usurf(i,j)=usurfl(i,j,imth)
               endif
            endif
         enddo
      enddo

c AY (11/12/03) : code below taken from gseta.F
      cd = 0.0013 
      if (unify_winds .eq. 1 .or..not.flag_ents) then
         do j=1,jmax
            tv3 = 0.
            do i=1,imax
               if(i.eq.1) then
                  tv = (tau(1,i,j)+tau(1,imax,j))/2
               else
                  tv = (tau(1,i,j)+tau(1,i-1,j))/2
               endif
               if(j.eq.1) then
                  tv2 = tau(2,i,j)/2
               else
                  tv2 = (tau(2,i,j)+tau(2,i,j-1))/2
               endif
c LG: use embm winds even with ents (unify_winds=1
               usurf(i,j) = sqrt((sqrt(tv**2 + tv2**2))
     1              *rh0sc*dsc*usc*fsc/(rhoair*cd*scf))
               tv3 = tv3 + usurf(i,j)
            enddo
c LG :  added wind_polar_avg that undoes the 
c zonal average of usurf near poles when eq 2
            if (par_wind_polar_avg.ne.2) then
               do i=1,imax
                  if(j.le.2.or.j.ge.jmax-1)usurf(i,j) = tv3/imax
               enddo
            endif
         enddo
c LG: and now replace usufl with usurf for the rest of the calculations
         do i=1,imax
            do j=1,jmax
               usurfl(i,j,imth) = usurf(i,j)
            enddo
         enddo
      endif
c LG: this is to output calculated surface winds if needed      
c$$$      write(imthstring,'(i50)') imth
c$$$      lenstring = lnsig1(imthstring)
c$$$      open(45,file='usurf_'//imthstring(1:lenstring)//'.dat')
c$$$      do j=1,jmax
c$$$         do i=1,imax
c$$$            write(45,*)usurf(i,j)
c$$$         enddo
c$$$      enddo
c$$$      close(45)
c$$$
c$$$

c     WHAT IS THIS FOR ... ?
      do j=1,jmax
         do i=1,imax
            atmos_lowestlh_atm(i,j) = REAL(z1_embm)
         enddo
      enddo

c ======================================================================
c     SET UP LOCAL ATMOSPHERIC GREENHOUSE GAS ARRAYS
c ======================================================================
c     Options:
c     (1) Test for climate feedback with ATCHEM (BIOGEM) is selected
c         NOTE: current atmospheric tracer assignment is:
c               CO2 == #3
c               CH4 == #10
c               N2O == #14
c         NOTE: catch situation where tracers are not selected in ATCHEM
c               (=> gas concentrations will be zero)
c     (2) Test for JDA forcings
c     (3) Time series co2 input (from Dan Lunt's goldstein option)
c     (4) Else, utilize original compound increase in CO2 (if specified)
c     *** 1 ***
      if ((atchem_radfor.eq.'y').or.(atchem_radfor.eq.'Y')) then
         do j=1,jmax
            do i=1,imax
               if (dum_sfcatm(3,i,j).lt.(co20/1000.0)) then 
                  co2(i,j) = co20
               else
                  co2(i,j) = dum_sfcatm(3,i,j)
               endif
               if (dum_sfcatm(10,i,j).lt.(ch40/1000.0)) then
                  ch4(i,j) = ch40
               else
                  ch4(i,j) = dum_sfcatm(10,i,j)
               endif
               if (dum_sfcatm(14,i,j).lt.(n2o0/1000.0)) then
                  n2o(i,j) = n2o0
               else
                  n2o(i,j) = dum_sfcatm(14,i,j)
               endif
            enddo
         enddo
c     *** 2 ***
c     JDA reading forcings (if appropriate)
      elseif (useforc) then
         if(mod(istep,nyear).eq.1)then
            my_year=istep/nyear+1
            co2lev=co2(1,1)*1.e6
            aero=0.
            vol=0.
            open(25,file=rstdir_name(1:lenrst)//forcname)
            read(25,*)header
c     if (io .eq. 0) then
c     file exists, will read contents and use                      
            j=0
            do while (j.lt.my_year)
               read(25,*,iostat=io)i,co2lev,vol,aero,solar
               if(io.lt.0) then
c     reached end of file
                  print *,'Warning: reached end of forcing file'
                  j=my_year
               endif
               j=j+1
            enddo
c     now set forcings
            print *,i,co2lev,vol,aero,solar
            co2(:,:)=co2lev*1.E-6
c pbh update global values
            co2_out(:,:)=co2(:,:)
c     
            call radfor(0,real(nyear),real((solar-1368.)*solfac+1368.
     &           +(aero*aerofac+vol*volfac)*4./0.7),flag_ents)
            close(25)
         endif
c     *** 3 ***
c time series input for co2
      elseif (t_co2.gt.0) then
        if (t_co2.eq.1) then
          time_1=int(istot/real(co2steps))+1
          time_2=time_1+1
          time_frac=(mod(istot,co2steps))/real(co2steps)
          if (time_2.le.nco2) then
            co2_exact=(1-time_frac)*co2_vect(time_1)+
     &                time_frac*co2_vect(time_2)
          else
            if (time_frac.ne.0) print*,'Time out of bounds for co2'
            co2_exact=co2_vect(nco2)
          endif
          if(mod(istot,10000).eq.0) then
            print*,'co2',istot,time_1,time_frac,co2_exact
          endif
          co2(:,:)=co2_exact
c pbh update global values
          co2_out(:,:)=co2(:,:)
        endif
      else
c     *** 4 ***
c     original simple compound increase
         do j=1,jmax
            do i=1,imax
               co2(i,j) = (1.0 + rate_co2)*co2(i,j)
               ch4(i,j) = (1.0 + rate_ch4)*ch4(i,j)
               n2o(i,j) = (1.0 + rate_n2o)*n2o(i,j)
            enddo
         enddo
c pbh update global values
         co2_out(:,:)=co2(:,:)
         ch4_out(:,:)=ch4(:,:)
         n2o_out(:,:)=n2o(:,:)
      endif

c ======================================================================
c SURFLUX model timestep
c ======================================================================

c AY (09/03/04) : Write out current time (days, years, etc.) if required
#ifdef clock
 120  format(a26,i8,a6,i6,a5,f9.4)

      it1 = istep - 1
      it2 = mod(it1, nyear)
      it3 = it1 - it2
      it4 = it3 / nyear
      rt1 = yearlen / nyear
      rt2 = real(it2)*rt1
      write(*,120) 'G-SURFLUX time : iteration',istep,', year',it4,
     +     ', day',rt2

#endif

c pbh istot defined at start of routine
c AY (15/01/04) : forgot about istot
c nre this must be reinstated to give correct continuation eg from time
c swop these lines as soon as time is available.
c     time = timein
c     istot = nint(time/dt(kmax)) + 1
c      istot = istep

c initialize integrated runoff (and other arrays)
      do j=1,jmax
         do i=1,imax
            evap(i,j) = 0.
            runoff(i,j) = 0.
            runoff_land(i,j) = 0.
            fxlho(i,j) = 0.
            fxsho(i,j) = 0.
            fxswo(i,j) = 0.
            fxlwo(i,j) = 0.
            if (flag_ents) then
               pptn(i,j) = 0.
            endif
            pptn_ocn(i,j) = 0.
            runoff_ocn(i,j) = 0.
            fxlha(i,j) = 0.
            fxsha(i,j) = 0.
            fxswa(i,j) = 0.
            fxlwa(i,j) = 0.
            dthsic(i,j) = 0.
            dtareasic(i,j) = 0.
            if (.not.flag_ents) then
               albedo(i,j) = real(albcl(i,j))
            endif
            albice(i,j) = 0.
         enddo
      enddo

c SG > Add Greenland melt here It will only work with 36*36 grid,
#ifdef icemelt
cmsw Add previous year's ice melt from Greenland to
cmsw ocean boxes around Greenland equally. Only works
cmsw for 36*36 model grid.
      if(icemeltfwfswitch.eq.1)then
         runoff(22,34)=glmelt
         runoff(23,34)=glmelt
         runoff(24,34)=glmelt
         runoff(22,33)=glmelt
         runoff(23,33)=glmelt
         runoff(24,33)=glmelt
      endif
#endif
c SG <

c JDA make global mean temp for use in sensitivity adjustment
      meantemp=0.
      do j = 1,jmax
           do i = 1,imax
                meantemp=meantemp+atemp(i,j)
           enddo
      enddo
      meantemp=meantemp/real(jmax*imax)
c      print *,'mean temp and parameters'
c      print *,meantemp,t_eqm,olr_adj,aerofac,volfac,solfac
c      stop
c JDA end

ccc main i,j loop to compute surface flux terms
c nre note for IGCM ; the first section defines P, OLWR, rel hum. CO2, all
c irrelevant to IGCM

c      do j=1,jmax
c         do i=1,imax
      do i=1,imax
         do j=1,jmax
            
            if (flag_ents) then
c SG > Adding mods for ENTS
c Optional effect of altitude in ENTS.
c Air temp calculated at altitude using lapse
c rate. This air temp used in calc.s of SVP and
c longwave radiation.
            if(orogswitch.ge.1)then
               talt=atemp(i,j)+(lapse_rate*surf_orog_atm(i,j))
            else
               talt=atemp(i,j)
            endif
            lmd = lambdapptn
c SG <
            endif
c pptn (over land and ocean)
c - need saturation vapour pressure, relative humidity

c pbh no orography for precipitation (orography only affects surface processes if orogswitch=2)
            if((orogswitch.lt.2).and.(flag_ents)) then
               qsata(i,j) = const1*exp(const4*talt
     1              /(talt+const5))
            else
               qsata(i,j) = const1*exp(const4*atemp(i,j)
     1              /(atemp(i,j)+const5))
            endif

            if (flag_ents) then
c pbh surface saturation humidity for evaporation (used if orogswitch=2)
               surf_qsata = const1*exp(const4*talt
     1              /(talt+const5))
               
c SG > replace ashum(i,j)_rmax*qsata(i,j) with deltq
               deltq = lmd*(ashum(i,j)-(rmax*qsata(i,j)))

               pptn(i,j) = max(0.0,deltq
     1              *rhoao*hatmbl(2)*rdtsurfdim)
            else
               pptn(i,j) = max(0.0,(ashum(i,j) - rmax*qsata(i,j))
     1              *rhoao*hatmbl(2)*rdtdim)
            endif

c nre instantaneous precipitation
c nre 6/10/3 set tq=tq1 for tstipa
c calc relative humidity rq after P not before

c AY (10/12/03) : the next two lines cause problems for surflux as
c                 they alter atmosphere properties independently of
c                 embm.F and the fluxes (but for good reasons)
c nre for IGCM remove these 3 lines of modification
c
            if (flag_ents) then
               ashum(i,j) = min(ashum(i,j),
     &              real(ashum(i,j)-deltq))
            else
               ashum(i,j) = min(ashum(i,j),
     &              real(rmax*qsata(i,j)))
            endif
            tq1(2,i,j) = ashum(i,j)
            tq(2,i,j) = tq1(2,i,j)

            rq = ashum(i,j)/qsata(i,j)
            if (flag_ents) then
c SG > rel humidity stored as a field for ENTS
               eb_relh(i,j) = real(rq)
c SG <

c pbh surface specific humidity, derived at talt assuming vertically constant relh (orogswitch=2)
               surf_tq2=rq*surf_qsata
            endif

c use climatological albedo in calculating incoming shortwave radiation
c shortwave radiation NB modified later over ice
c
            if (flag_ents) then
c SG > Modded for ENTS
               albedo(i,j) = atm_alb(i,j,imth)
c SG <
            endif
c cgv2 seasonal
            if (flag_ents) then
               fxsw(i,j) = solfor(j,mod(istot-1,nyear)+1)
     1              *(1.- albedo(i,j))
            else
               fxsw(i,j) = solfor(j,mod(istot-1,nyear)+1)
     &              *(1.- albcl(i,j))
            endif
c
c AY (08/03/04) : check solar radiation for simulation time
c           if(i.eq.18.and.j.eq.18) print*,'solfor(18,18) =',
c    +           fxsw(18,18),'at istep =',istep,', koverall =',koverall

            if (flag_ents) then
c SG > Added for ENTS
               if (t_orog.eq.1) then
c store orography at previous timestep to derive meltwater
                  surf_orog_atm_previous(i,j)=surf_orog_atm(i,j)
                  surf_orog_atm(i,j) = orog_exact(i,j)
               endif
               if (t_lice.eq.1) then
                  landice_slicemask_lic(i,j) = ceiling(lice_exact(i,j))
               else
                  lice_exact(i,j) = landice_slicemask_lic(i,j)
               endif
            endif
            
c-----------------------------------------------------------------------
c     OUTGOING PLANETARY LONGWAVE
c-----------------------------------------------------------------------
c     NOTE: JDA added in olr_adj term (equivalent to olr_1 in holden (clim dyn) but with opposite sign
c           pbh added in olr_adj0 term - globally uniform perturbation (reduction) to clear skies OLR
c     calculate coefficients
            tv0 = b00 + rq*(b10 + b20*rq)
            tv1 = b01 + rq*(b11 + b21*rq)
            tv2 = b02 + rq*(b12 + b22*rq)
            tv3 = b03 + rq*(b13 + b23*rq)
c     calculate CH4 and N2O contributions
c     NOTE units are ppb for CH4 and N2O calculation
            ch4_term = 
     1           alphach4*(sqrt(1e9*ch4(i,j)) - sqrt(1e9*ch40))
     2           - ch4_func(1e9*ch4(i,j),1e9*n2o0) 
     3           + ch4_func(1e9*ch40,1e9*n2o0)
            n2o_term = 
     1           alphan2o*(sqrt(1e9*n2o(i,j)) - sqrt(1e9*n2o0))
     2           - ch4_func(1e9*ch40,1e9*n2o(i,j))
     3           + ch4_func(1e9*ch40,1e9*n2o0)
c     calculate outgoign longwave
c pbh talt dependence removed for olr (orography only affects surface proceses now if orogswitch=2)
            if((orogswitch.lt.2).and.(flag_ents)) then
               fxplw(i,j) = tv0 + talt*(tv1 
     1              + talt*(tv2 
     2              + talt*tv3))
     3              - delf2x*log(co2(i,j)/co20)
     4              - ch4_term - n2o_term
     5              + olr_adj*(meantemp-t_eqm)
     6              - olr_adj0
            else
               fxplw(i,j) = tv0 + atemp(i,j)*(tv1 
     1              + atemp(i,j)*(tv2 
     2              + atemp(i,j)*tv3))
     3              - delf2x*log(co2(i,j)/co20)
     4              - ch4_term - n2o_term
     5              + olr_adj*(meantemp-t_eqm)
     6              - olr_adj0
            endif

c-----------------------------------------------------------------------
c     LATENT HEAT FLUX
c-----------------------------------------------------------------------
c     latent heat flux into atmos associated with condensation 
c     no account taken of snow melting, must assume all pptn is rain
            fxlata(i,j) = rho0*pptn(i,j)*hlv

c-----------------------------------------------------------------------
c calculate terms over ocean or ice
c-----------------------------------------------------------------------
            if(k1(i,j).le.kmax) then

c longwave radiation 

               alw = atemp(i,j)+zeroc
               alw = alw * alw
               alw = alw * alw
               alw = ema * alw

c surface salinity-dependent freezing point:

               salt = saln0+osaln(i,j)
               if (opt_freeze) then
                  tsfreez(i,j) = salt*(-0.0575 + 0.0017*sqrt(salt)
     1                                 - 0.0002*salt)
               else
                  tsfreez(i,j) = -999.0
               endif

c maximum amount of heat available in first layer
c nre rsictscsf must be changed if dt>17.5 days, see gseta

               qb(i,j) = rsictscsf*(tsfreez(i,j)-otemp(i,j))
               qbsic(i,j) = qb(i,j)

c-----------------------------------------------------------------------
c calculate terms over ice
c-----------------------------------------------------------------------

c AY (18/12/03) : standard c-GOLDSTEIN sea-ice routine
c                 would be nice to include dummy routine as well
               if(sica(i,j).gt.0.0)then
c              * Sea-ice present *       

c let albedo over sea ice vary as a function of tair (Holland et al. 1993)

                  albsic = max(par_albsic_min, 
     &             min(par_albsic_max,0.40 - 0.04*atemp(i,j)))
c cgv2 seasonal
c nre note for IGCM needs changing
                  if (flag_ents) then
c SG > Modded for ENTS
                     fxswsic = solfor(j,mod(istot-1,nyear)+1)
     &                    *(1. - albsic)
     1                    *(1.-albedo(i,j))

                     fxswsica = solfor(j,mod(istot-1,nyear)+1)
     1                    *(1.-albedo(i,j))
c SG <
                  else
                     fxswsic = solfor(j,mod(istot-1,nyear)+1)
     &                    *(1. - albsic)
                  endif

c first need to calculate T_ice

                  do iter=1,itice
                     ticold = tice(i,j)
c Dalton number
                     cesic = 1.0e-3*(1.0022 - 0.0822*(atemp(i,j)
     1                     - ticold) + 0.0266*usurf(i,j))
                     cesic = max(6.0e-5,min(2.19e-3,cesic))

                     chsic = 0.94*cesic

c sensible heat flux 
                     cfxsensic = rhoair*chsic*cpa*usurf(i,j)

                     qsatsic = const1*exp(const2*ticold         
     1                            /(ticold + const3))

                     evapsic(i,j) = max(0.0,(qsatsic - ashum(i,j))
     1                         *rhoao*cesic*usurf(i,j))

                     tieqn = sich(i,j)*((1-ca(i,j))*fxswsic + alw 
     1                     - emo*(ticold+zeroc)**4  - cfxsensic*(ticold 
     2                     - atemp(i,j)) - rho0*hls*evapsic(i,j) )
     3                     + consic*(tsfreez(i,j)-ticold)

                     dtieq = sich(i,j)*( 
     1                     - 4.0*emo*(ticold+zeroc)**3 - cfxsensic
     1                     - hls*rhoair*cesic*usurf(i,j)*qsatsic*const2
     2                     *const3/((ticold + const3)**2)
     4                     *0.5*(1.0 + sign(1.0,qsatsic - ashum(i,j))) )
     5                     - consic

                     tice(i,j) = real(ticold - tieqn/dtieq)

                     if(abs(tice(i,j) - ticold).lt.tol .or.     
     1                  ticold.gt.tfreez.and.tieqn.gt.0.0)goto 10
                  enddo

       print*,'warning sea-ice iteration failed at',istot,i,j
     &                ,tice(i,j),ticold,tice(i,j)-ticold,tieqn,dtieq
       print*,'cesic ',cesic,'cfxsensic ',cfxsensic,'qsatsic ',qsatsic
     +      ,'evapsic ',evapsic(i,j),'tieqn ',tieqn,'dtieq ',dtieq
       print*,'sich ',sich(i,j),'sica ',sica(i,j)
       print*,'atemp: ',atemp(i,j), ' / ','otemp: ',otemp(i,j)
                  if (.not.debug_loop) stop
   10             tice(i,j) = min(real(tfreez),tice(i,j))

c recalc everything in case of resetting of tice

                  fxlwsic = emo*(tice(i,j)+zeroc )**4 - alw

                  cesic = 1.0e-3*(1.0022 - 0.0822*(atemp(i,j)
     1                  - tice(i,j)) + 0.0266*usurf(i,j))
                  cesic = max(6.0e-5,min(2.19e-3,cesic))

                  chsic = 0.94*cesic

                  cfxsensic = rhoair*chsic*cpa*usurf(i,j)

                  fxsensic = cfxsensic*(tice(i,j) - atemp(i,j))

                  qsatsic = const1*exp(const2*tice(i,j)      
     1                            /(tice(i,j) + const3))

                  evapsic(i,j) = max(0.0,(qsatsic - ashum(i,j))
     1                         *rhoao*cesic*usurf(i,j))

                  fx0sic(i,j) = (1-ca(i,j))*fxswsic -fxsensic
     1                        - fxlwsic - rho0*hls*evapsic(i,j)

                  if (flag_ents) then
c SG > Modded for ENTS
                     fx0sica = ca(i,j)*fxswsica 
     1                    + fxlata(i,j)
     2                    + fxsensic + fxlwsic
     3                    - fxplw(i,j)
c SG <
                  else
                     fx0sica = ca(i,j)*fxswsic 
     1                    + fxlata(i,j)
     2                    + fxsensic + fxlwsic
     3                    - fxplw(i,j)
                  endif
                  if (flag_ents) then
c SG > Modded for ENTS
                     fx0sica = ca(i,j)*fxswsica 
     1                    + fxlata(i,j)
     2                    + fxsensic + fxlwsic
     3                    - fxplw(i,j)
c SG <
                  endif

c AY (10/12/03) : components of heat flux into atmosphere over sea-ice
c nre only fxlwsic and fxsensic relevant for IGCM

                  atm_latenti   = + fxlata(i,j)
                  atm_sensiblei = + fxsensic
                  if (flag_ents) then
                     atm_netsoli = + ca(i,j)*fxswsica
                  else
                     atm_netsoli = + ca(i,j)*fxswsic
                  endif
                  atm_netlongi  = + fxlwsic - fxplw(i,j)

                  dhsic = rrholf*(qb(i,j) - fx0sic(i,j)) 
     1                  - rhooi*evapsic(i,j)

c AR (19/12/12): assign a dummy value of qb(i,j) calculated to give
c                no further sea-ice growth if existing thickness 
c                exceeds a given  threshold (par_sich_max)
                  if (sich(i,j) >= par_sich_max) then
                     if (dhsic > 0.0) then
                        qbsic(i,j) = (0.0 + rhooi*evapsic(i,j))/rrholf
     1                       + fx0sic(i,j)
                        dhsic = rrholf*(qbsic(i,j) - fx0sic(i,j)) 
     1                       - rhooi*evapsic(i,j)
                     endif
                  endif

               else
c              * Sea-ice absent *
                  albsic        = 0.
                  fx0sica       = 0.
                  dhsic         = 0.
                  evapsic(i,j)  = 0.
                  tice(i,j)     = 0.
c AY (10/12/03) : components of atmosphere heat flux with sea-ice
                  atm_latenti   = 0.
                  atm_sensiblei = 0.
                  atm_netsoli   = 0.
                  atm_netlongi  = 0.
               endif

c-----------------------------------------------------------------------
c over open ocean
c-----------------------------------------------------------------------

               fxlw(i,j) = emo*(otemp(i,j)+zeroc)**4 - alw

c Dalton number

               ce = 1.0e-3*(1.0022 - 0.0822*(atemp(i,j)-
     1              otemp(i,j)) + 0.0266*usurf(i,j))
               ce = max(6.0e-5,min(2.19e-3,ce))

               ch = 0.94*ce

c sensible heat flux from ocean to atmosphere

               fxsen(i,j) = rhoair*ch*cpa*usurf(i,j)*
     1              (otemp(i,j)-atemp(i,j))

c evaporation/sublimation rate 

               qsato(i,j) = const1*exp(const4*otemp(i,j)
     1              /(otemp(i,j)+const5))

               evap(i,j) = max(0.0,(qsato(i,j) - ashum(i,j))
     1              *rhoao*ce*usurf(i,j))

c net heat flux into atmosphere
c nre only fxlw and fxsen relevant for IGCM 
c total fluxes fx0oa, fx0a etc not needed for IGCM except for diagnostics

               fx0oa = ca(i,j)*fxsw(i,j) + fxlata(i,j) + fxsen(i,j) 
     1              + fxlw(i,j) - fxplw(i,j)

c AY (10/12/03) : set up fluxes -> atmosphere
               atm_latent   = + fxlata(i,j) 
               atm_sensible = + fxsen(i,j)
               atm_netsol = + ca(i,j)*fxsw(i,j)
               atm_netlong  = + fxlw(i,j) - fxplw(i,j)

c add proportions over open ocean and sea ice

               fx0a(i,j) = (1-sica(i,j))*fx0oa
     1              + sica(i,j)*fx0sica

c AY (10/12/03) : add fluxes to include sea-ice and ocean components

               fxlha(i,j)   = real((sica(i,j)*atm_latenti)
     1              + ((1-sica(i,j))*atm_latent))
               fxsha(i,j) = real((sica(i,j)*atm_sensiblei)
     1              + ((1-sica(i,j))*atm_sensible))
               fxswa(i,j) = real((sica(i,j)*atm_netsoli)
     1              + ((1-sica(i,j))*atm_netsol))
               fxlwa(i,j)  = real((sica(i,j)*atm_netlongi)
     1              + ((1-sica(i,j))*atm_netlong))

c heat flux from atmosphere into open ocean 
               if (flag_ents) then
c SG > Modded for ENTS
                  fx0o(i,j) = ((1-ca(i,j))*fxsw(i,j)*
     1                 (1.-albo(j,imth)))
     2                 - fxsen(i,j)
     3                 - fxlw(i,j) - rho0*hlv*evap(i,j)
c SG <
               else
                  fx0o(i,j) = (1-ca(i,j))*fxsw(i,j) - fxsen(i,j)
     1                 - fxlw(i,j) - rho0*hlv*evap(i,j)
               endif

c net heat flux into ocean from atmosphere and sea ice
c including possible ice growth over open ocean

               fx0neto(i,j) = sica(i,j)*qbsic(i,j)
     1              + (1-sica(i,j))*max(qb(i,j)
     2              ,fx0o(i,j))

c AY (10/12/03) : set up fluxes -> ocean

               fxlho(i,j) = real((1-sica(i,j))*( - rho0*hlv*evap(i,j)
     &                    + max(0.0,qb(i,j) - fx0o(i,j)))
     &                    + sica(i,j)*qbsic(i,j))
               fxsho(i,j) = - real((1-sica(i,j))*fxsen(i,j))
               if (flag_ents) then
                  fxswo(i,j) = real((1-sica(i,j))*(1-ca(i,j))*fxsw(i,j)
     &                 *(1.-albo(j,imth)))
               else
                  fxswo(i,j) = real((1-sica(i,j))*(1-ca(i,j))*fxsw(i,j))
               endif
               fxlwo(i,j)  = - real((1-sica(i,j))*fxlw(i,j))

               dho = max(0.0,rrholf*(qb(i,j) - fx0o(i,j)))

               dthsic(i,j) = real(sica(i,j)*dhsic 
     1                     + (1-sica(i,j))*dho)

               dtareasic(i,j) = real(max(0.0,rhmin*dho*(1-sica(i,j))))
               if(sich(i,j).gt.1e-12) then
                  dtareasic(i,j) = real(dtareasic(i,j)
     1                 + min(0.0,0.5*sica(i,j)*sica(i,j)
     2                 * dhsic/sich(i,j)))
               endif

               if (flag_ents) then
c SG > Modded for ENTS
cmsw Calculate planetary albedo over ocean boxes
                  albs_atm(i,j)=(albo(j,imth)
     1                 - (sica(i,j)*(albo(j,imth)-albsic)))
                  palb(i,j)=(1.-albedo(i,j))*(albo(j,imth)
     1                 - (sica(i,j)*(albo(j,imth)-albsic)))
     2                 + albedo(i,j)
c SG <
               endif
                  
c nre albedo array, purely diagnostic variable

               if (.not.flag_ents) then
                  albedo(i,j)=real(sica(i,j)*albsic+(1-sica(i,j))*
     1                 albcl(i,j)) 
               endif

c AY (23/09/04) : sea-ice albedo now output
               albice(i,j) = real(albsic)

c AY (23/01/04) : excised for now
c global heat source diagnostic
c
c              ghs = ghs + sica(i,j)*(fx0sic(i,j) + fx0sica)
c    1              + (1-sica(i,j))*(fx0o(i,j) + fx0oa)
c    2              + rhoice*hlf*(dtareasic(i,j)
c    3              + evapsic(i,j)*sica(i,j)*rhooi)
            else
c-----------------------------------------------------------------------
c calculate terms over land
c-----------------------------------------------------------------------

#ifdef dland
c AY (10/12/03) : set up fluxes -> atmosphere
! pph (16/08/04) : land-to-atmos fluxes for EMBM are set by genie-land
c AY (03/09/04) : not so sure this is a good idea - might prevent us
c                 from running c-GOLDSTEIN through genie.F (as we can
c                 do now)
               if (.not.flag_ents) then
                  fx0a(i,j)  = 0.0
                  fxlw(i,j)  = 0.0
                  fxlha(i,j) = 0.0
                  fxsha(i,j) = 0.0
                  fxswa(i,j) = + ca(i,j)*fxsw(i,j)
                  fxlwa(i,j) = - fxplw(i,j)
                  fx0o(i,j)  = 0.0
               endif

c AY (23/01/04) : excised for now
c Update global heat souce over land
c              ghs = ghs + fx0a(i,j) + fx0o(i,j)
#else
               if (.not.flag_ents) then
                  fx0a(i,j) = fxsw(i,j) + fxlata(i,j)
     1                 - fxplw(i,j)
               endif

c AY (10/12/03) : set up fluxes -> atmosphere
               if (.not.flag_ents) then
                  fxlha(i,j)   = + real(fxlata(i,j))
                  fxsha(i,j) = + 0.
                  fxswa(i,j) = + real(fxsw(i,j))
                  fxlwa(i,j)  = - real(fxplw(i,j))
               endif

c runoff; add pptn to appropriate coastal grid cell

crma               if(ldeg)then
               if (.not.flag_ents) then
                  if(igrid.ne.0)then
                     runoff(iroff(i,j),jroff(i,j)) = runoff(iroff(i,j)
     1                    ,jroff(i,j)) + pptn(i,j)*ds(j)*rds(jroff(i,j))
                  else
                     runoff(iroff(i,j),jroff(i,j)) =
     1                    runoff(iroff(i,j),jroff(i,j)) + pptn(i,j)
                  endif
               endif

               if (.not.flag_ents) then
                  runoff_land(i,j) = real(pptn(i,j))
               endif

c AY (23/01/04) : excised for now
c Update global heat souce over land
c              ghs = ghs + fx0a(i,j)
#endif

               if (flag_ents) then
c-----------------------------------------------------------------------
c ENTS radiation and hydrology
c-----------------------------------------------------------------------

cmsw Offline model: switch variables

               if(ents_offlineswitch.eq.1)then
                  dum_talt=talt
                  dum_hum=ashum(i,j)
                  dum_pptn=pptn(i,j)
cmsw Replace with offline data
                  if(orogswitch.eq.1)then
                     talt=tncep(i,j,imth)
     &                    +(lapse_rate*surf_orog_atm(i,j))
                  else
                     talt=tncep(i,j,imth)
                  endif
                  qsalt=const1*exp(const4*talt
     1                           /(talt+const5))
                  ashum(i,j)=rhncep(i,j,imth)*qsalt
                  pptn(i,j)=pncep(i,j,imth)
               endif

cmsw Snow
c also update land surface albedo values according to changes here

               if(land_temp_lnd(i,j).lt.-5.and.talt.lt.-5.
     1            and.pptn(i,j).gt.0.)then
                 land_snow_lnd(i,j)=1
                 albs_atm(i,j)=land_albs_snow_lnd(i,j)
               endif

cmsw Snow melt
c also update land surface albedo values according to changes here

               if(land_temp_lnd(i,j).ge.-5.and.talt.ge.-5.
     1            and.land_snow_lnd(i,j).eq.1)then
                 land_snow_lnd(i,j)=0
                 albs_atm(i,j)=land_albs_nosnow_lnd(i,j)
               endif

cmsw transfer coefficients
c pbh LOOK should this be moved after affects of slicemask calculated?
               chl(i,j)=1./(((1./0.41)*log(10./land_z0_lnd(i,j)))**2)
               cel(i,j)=chl(i,j)

cmsw Albedo functionality

c#ifdef fixedveg
c               fv(i,j)=fvfv(i,j)
c#endif
               if(abs(landice_slicemask_lic(i,j)-2.0).lt.1e-19) then
c#ifdef fixedveg
c                  fv(i,j)=0.
c#endif
                  albs_atm(i,j)=albs_atm(i,j)
     &                 *(2-lice_exact(i,j)) +
     1                            0.8*(lice_exact(i,j)-1)
                  land_z0_lnd(i,j)=land_z0_lnd(i,j)*(2-lice_exact(i,j))+
     1                            0.001*(lice_exact(i,j)-1)
                  land_bcap_lnd(i,j)=land_bcap_lnd(i,j)
     &                 *(2-lice_exact(i,j)) +
     1                 lice_k9*(lice_exact(i,j)-1)
               endif

cmsw Newton-Raphson iteration loop to solve for eqm
cmsw land temperature. Justified for 1 ocean timestep.

              do itld=1,itldmax
c              do itld=1,1

cmsw sensible heat flux from land to atmosphere

                  fxsen(i,j) = rhoair*chl(i,j)*cpa*usurfl(i,j,imth)*
     1                         (land_temp_lnd(i,j)-talt)

cmsw longwave radiation
                  if(ents_offlineswitch.eq.1)then
                     alw = tncep(i,j,imth)+zeroc
                  else
                     alw = tq1(1,i,j)+zeroc
                  endif
                  alw = alw * alw
                  alw = alw * alw
                  alw = ema * alw
cmsw Net longwave over land (positive upward)
                  fxlw(i,j) = eml*((land_temp_lnd(i,j)+zeroc)**4)
     1                      - alw

cmsw Evaporation
                  if(land_temp_lnd(i,j).le.0.)then
                     qsato(i,j) = const1*exp(const2*land_temp_lnd(i,j)
     1                           /(land_temp_lnd(i,j)+const3))
                  else
                     qsato(i,j) = const1*exp(const4*land_temp_lnd(i,j)
     1                           /(land_temp_lnd(i,j)+const5))
                  endif

cmsw devapcheck used to find out if evap has a differential (0=N, 1=Y)
                  if(land_moisture_lnd(i,j).gt.0.)then
                     beta=min(1.,(land_moisture_lnd(i,j)
     &                    /land_bcap_lnd(i,j))**4)

c pbh evap calculated with surface humidity if orogswitch=2
                     if(orogswitch.lt.2) then
                       evap(i,j) = max(0.0,(qsato(i,j) - ashum(i,j))
     1                            *rhoao*cel(i,j)*usurfl(i,j,imth)*beta)
                     else
                       evap(i,j) = max(0.0,(qsato(i,j) - surf_tq2)
     1                            *rhoao*cel(i,j)*usurfl(i,j,imth)*beta)
                     endif

                     devapcheck=1
                  else
                     evap(i,j)=0.
                     devapcheck=0
                  endif

                  if(evap(i,j)*dtsurfdim.gt.land_moisture_lnd(i,j))then
                     evap(i,j)=land_moisture_lnd(i,j)*rdtsurfdim
                     devapcheck=0
                  endif

cmsw ODE for land temp wrt to time

                  dtld = rhcld*((1-ca(i,j))*fxsw(i,j)
     &                 *(1-albs_atm(i,j))
     1                  - fxsen(i,j) - fxlw(i,j) - rho0*hlv*evap(i,j))
cmsw Sensible heat derivative

                  dfxsens=rhoair*chl(i,j)*cpa*usurfl(i,j,imth)

cmsw net longwave derivative

                  dfxlw=4.*eml*((land_temp_lnd(i,j)+zeroc)**3)

cmsw evap derivative
                 if(devapcheck.eq.0)then
                   devap=0.
                 else
                    if(land_temp_lnd(i,j).le.0.)then
                       dqsato = ((const1*const2*const3)/
     1                         (land_temp_lnd(i,j)+const3)**2)
     2                          *exp(const2*land_temp_lnd(i,j)
     3                           /(land_temp_lnd(i,j)+const3))
                    else
                       dqsato = ((const1*const4*const5)/
     1                         (land_temp_lnd(i,j)+const5)**2)
     2                           *exp(const4*land_temp_lnd(i,j)
     3                           /(land_temp_lnd(i,j)+const5))
                    endif

cmsw Calculate evaporation diff. (bare soil) or evapotranspiration diff. (veg)
cmsw depending on whether carbon feedbacks on climate chosen (carbonswitch)

                    devap=rhoao*cel(i,j)*usurfl(i,j,imth)*beta
     1                    *dqsato
                 endif

cmsw total derivative

                 dtld1 = -rhcld*(dfxsens+dfxlw+
     1                   (rho0*hlv*devap))

cmsw update last value

                 tldlast=land_temp_lnd(i,j)

cmsw calculate new value of tqld(1,i,j)

                 land_temp_lnd(i,j)=land_temp_lnd(i,j)-(dtld/dtld1)

                 if(abs(tldlast-land_temp_lnd(i,j)).lt.tolld) exit

                 if(itld.eq.itldmax)then

       print*,'Land rad. calc. did not converge to specified tol'
       print*,'at point',i,j
       print*,'final value was ',land_temp_lnd(i,j)
       print*,'last iteration value was ',tldlast
       print*,'istep ',istep
       print*,' '
        
                 
                 endif

              enddo

cmsw Radiation fluxes

              fx0a(i,j) = ca(i,j)*fxsw(i,j) + fxlata(i,j)
     1                  + fxlw(i,j) + fxsen(i,j) - fxplw(i,j)

              fx0o(i,j) = (1-ca(i,j))*fxsw(i,j)*(1-albs_atm(i,j))
     1             - fxsen(i,j) - fxlw(i,j) - rho0*hlv*evap(i,j)

cmsw Bucket land hydrology: update bucket sizes

               land_moisture_lnd(i,j)=land_moisture_lnd(i,j)
     &             +((pptn(i,j)-evap(i,j))
     1                       *dtsurfdim)

cmsw runoff scheme: in the case of land water bucket gt bucket capacity, bcap,
cmsw find nearest ocean
cmsw gridbox/gridboxes and add the surplus as runoff there
               if(land_moisture_lnd(i,j).gt.land_bcap_lnd(i,j)) then
                  if (igrid.ne.0) then
                     runoff(iroff(i,j),jroff(i,j)) = runoff(iroff(i,j)
     1                    ,jroff(i,j)) +((land_moisture_lnd(i,j)
     2                    -land_bcap_lnd(i,j))*rdtsurfdim)*ds(j)
     3                    *rds(jroff(i,j))
! need the excess surface water skimmed off anyway before trying other schemes                 
                     if (par_runoff_scheme.gt.0) then
                     runoff_land(i,j) = runoff_land(i,j)+
     1        ((land_moisture_lnd(i,j)-land_bcap_lnd(i,j))*rdtsurfdim)*ds(j)
     2                    *rds(jroff(i,j))
                     endif
                  else
                     runoff(iroff(i,j),jroff(i,j)) = runoff(iroff(i,j)
     1                    ,jroff(i,j)) +((land_moisture_lnd(i,j)
     2                    -land_bcap_lnd(i,j))*rdtsurfdim)
! need the excess surface water skimmed off anyway before trying other schemes                 
                     if (par_runoff_scheme.gt.0) then
                     runoff_land(i,j) = runoff_land(i,j)+
     1        ((land_moisture_lnd(i,j)-land_bcap_lnd(i,j))*rdtsurfdim)
                     endif
                  endif
                  land_moisture_lnd(i,j)=land_bcap_lnd(i,j)
               endif

cghc Meissner et al (2003) leaky bucket scheme.
cghc 6.63 = b = Clapp-Hornberger exponent (=4.50 for coarse grain soil and 11.20 for fien grain)
cghc 0.0047 kg m�2 s�1 = K_s = 0.0047 mm s-1 = 0.0000047 m s-1 = Saturated hydraulic conductivity
cghc Y = runoff; M = moisture; M_sat = saturated soil moisture
cghc Y = K_s(M/M_Sat)^(2b+3) = 0.0047*(M/M_Sat)^15.2 [2b+3 = {12.00, 16.26, 25.40} = runoff_factor_1]
cghc kg m-2 is mm for rainfall, which rokgem uses.
cghc need to divide rather than multiply by rdtdim (= 1/ocean timestep in sec = 3.1688087814028956E-006 s-1),
cghc or just leave out altogether(!) as have a s-1 in K_s? Yes, am multiplying by a factor of 1000 below (m2mm) so all
cghc in all getting the right magnitude for the wrong reason (should be ~300x more!)
cghc could adjust K_s, or even have a 2d field for it...
cghc /(11.0-min(10,istep)) is used to ramp up runoff to full steam so as to avoid crashing the model

       if (par_runoff_scheme.eq.2) then
      runoff_land(i,j)=runoff_land(i,j)+
     1  ((0.0000047/(11.0-min(10,istep)))*
     2  (land_moisture_lnd(i,j)/land_bcap_lnd(i,j))**runoff_factor_1)
     
!       print*,land_moisture_lnd(i,j)/land_bcap_lnd(i,j)
     
cghc balance water budget

      land_moisture_lnd(i,j)=land_moisture_lnd(i,j)-
     1  runoff_land(i,j)*dtsurfdim/(11.0-min(10,istep))
       
      runoff(iroff(i,j),jroff(i,j)) =
     1               runoff(iroff(i,j),jroff(i,j))+runoff_land(i,j)
       endif
          
cghc Try Phil Holden's idea of soil moiture half-life (residence time) of 1-2 months (call it 1.5) from
cghc Pidwirny, M. (2006). "The Hydrologic Cycle". Fundamentals of Physical Geography, 2nd Edition. Date Viewed. http://www.physicalgeography.net/fundamentals/8b.html
cghc residence time = tau = 1.5 months = 1/8 year = 3944700sec. dtdim = timestep = 315576sec
cghc runoff_factor_2 = [1-exp(-tln2/tau)] =0.054 with 100 tsteps/year

       if (par_runoff_scheme.eq.3) then
      runoff_land(i,j)=runoff_land(i,j)+
     1   land_moisture_lnd(i,j)*
     2   runoff_factor_2*rdtsurfdim
     
cghc balance water budget

      land_moisture_lnd(i,j)=land_moisture_lnd(i,j)-runoff_land(i,j)*dtsurfdim
      
      runoff(iroff(i,j),jroff(i,j)) =
     1               runoff(iroff(i,j),jroff(i,j))+runoff_land(i,j)
       endif
       
       if (par_runoff_scheme.eq.0) then
          runoff_land(i,j) = real(pptn(i,j))
       endif
       
cmsw Calculate planetary albedo over land

               palb(i,j)=((1.-albedo(i,j))*albs_atm(i,j))
     &              +albedo(i,j)

c AY (10/12/03) : add fluxes to include sea-ice and ocean components

               fxlha(i,j) = real(fxlata(i,j))
               fxsha(i,j) = real(fxsen(i,j))
               fxswa(i,j) = real((ca(i,j)*fxsw(i,j)))
               fxlwa(i,j) = real((fxlw(i,j)-fxplw(i,j)))

cmsw Offline model: switch variables back

               if(ents_offlineswitch.eq.1)then
                  talt=dum_talt
                  ashum(i,j)=dum_hum
                  pptn(i,j)=dum_pptn
               endif

c Update global heat souce over land
               ghs = ghs + fx0a(i,j) + fx0o(i,j)

c---------------------------------------------------------------
            endif
            endif

         enddo
      enddo

c add in effects of runoff (non-local in i,j) and set up fluxes
c freshwater forcing in m/s open ocean P-E over ocean gridboxes
c evap zero over land, 
c nre P goes straight through as no snow allowed, but E is E total
c for atm model and solely E_ocean for the fwflux

      do j=1,jmax
         do i=1,imax
            if(k1(i,j).le.kmax) then
c
c wet points;
c
               pptn_ocn(i,j) = real(pptn(i,j))
               runoff_ocn(i,j) = real(runoff(i,j))+
c pbh ice sheet melwater
     1                           real(ice_runoff(i,j))
               evap_atm(i,j) = real(evap(i,j)*(1-sica(i,j))
     1              + evapsic(i,j)*sica(i,j))

c AY (19/12/03) : note that the FW anomaly normally applied to ocean
c                 FW fluxes has an odd status in a split-apart model.
c	          As present it is added here via the precipitation
c	          term.  It may make sense to add it differently or
c	          not at all in the future.
c nre Remove for IGCM 

               pptn_ocn(i,j) = real(pptn_ocn(i,j) + pmeadj(i,j))
            else
c AY (09/01/04) : set "ocean" fluxes to zero over land
               if (flag_ents) then
                  pptn_ocn(i,j) = real(pptn(i,j))
                  runoff_ocn(i,j) = real(runoff(i,j))
                  evap_atm(i,j) = real(evap(i,j))
               else
                  pptn_ocn(i,j) = 0.
                  runoff_ocn(i,j) = 0.
                  evap_atm(i,j) = 0.
               endif
            endif
c
c All cells (no evaporation over land - in this scheme)
c
            pptn_atm(i,j) = real(pptn(i,j))
c AY (31/03/04) : sign of ocean evaporation flux switched (to opposite
c	          atmosphere flux)
            evap_ocn(i,j) = -evap_atm(i,j)

c AY (23/07/04) : convert FW fluxes from m/s to mm/s for Dan
            pptn_ocn(i,j) = real(pptn_ocn(i,j) * m2mm)
            evap_ocn(i,j) = real(evap_ocn(i,j) * m2mm)
            runoff_ocn(i,j) = real(runoff_ocn(i,j) * m2mm)
            runoff_land(i,j) = real(runoff_land(i,j) * m2mm)     
            pptn_atm(i,j) = real(pptn_atm(i,j) * m2mm)
            evap_atm(i,j) = real(evap_atm(i,j) * m2mm)

         enddo
      enddo


c AY (10/12/03) : the sea-ice timestep and flux adjustment are now in
c                 genie-seaice. Note that these are an essential part of
c                 the sea-ice code and that they modify fluxes into the
c                 ocean but not the atmosphere.
c

c AY (05/04/02) : call routine to output surface flux fields (in the
c                 same way as a restart file)
c	          note : this file-writing uses the same index as
c	                 the EMBM (with which it shares a common block)
      if(mod(istep,iwstp).eq.0)then
         ext=conv_surf(mod(iw,10))
         if (debug_loop) print*,
     & 'Writing SURFLUX output file at time',istep
         call check_unit(2,__LINE__,__FILE__)
         open(2,file=outdir_name(1:lenout)//lout//'.sfx.'//ext,
     &        iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)
         rewind 2
c AY (12/07/04) : removed surplus input arguments to subroutine
         call outm_surf(2,co2,
     :     albedo,usurf,fxlho,fxsho,
     :     fxswo,fxlwo,
     :     evap_atm,pptn_ocn,runoff_ocn,
     :     fxlha,fxsha,
     :     fxswa,fxlwa,
     :     evap_atm,pptn_atm,
     :     dthsic,dtareasic)
         close(2,iostat=ios)
         call check_iostat(ios,__LINE__,__FILE__)
         if (debug_loop) print*
      endif

c Set values of dummy variables destined for BIOGEM & ENTS
c NOTE: convert precision between GOLDSTEIn (real*8) and GENIE 
c       (which can be real*4 or real* depending on the IGCM)
c      do j=1,jmax
c         do i=1,imax
      do i=1,imax
         do j=1,jmax
            go_fxsw(i,j) = real(fxsw(i,j))
ccc            if (flag_ents) then
               eb_fx0a(i,j)  = real(fx0a(i,j))
               eb_fx0o(i,j)  = real(fx0o(i,j))
               eb_fxsen(i,j) = real(fxsen(i,j))
               eb_fxlw(i,j)  = real(fxlw(i,j))
               eb_evap(i,j)  = real(evap(i,j))
               eb_pptn(i,j)  = real(pptn(i,j))
               eb_uv(1,i,j)  = usc*uatm(1,i,j)
               eb_uv(2,i,j)  = usc*uatm(2,i,j)
               eb_usurf(i,j) = usurf(i,j)
c     eb_relh(i,j) = real(relh(i,j))
cccc SHELF SOLAR RADIATION HACK
ccc               if(k1(i,j).ne.kmax) then
ccc                  go_fxsw(i,j) = 0.0
ccc               endif
ccc            endif
         enddo
      enddo
      do j=1,jmax
         go_solfor(j) = 
     :     real(solfor(j,mod(istot-1,nyear)+1))
      enddo
      
c pbh annual call to radfor if transient orbit is applied
      if(t_orbit.eq.1) then
        if(mod(istep-1,nyear).eq.0) then
          call radfor(istep+0,real(gn_daysperyear),solconst,flag_ents)
        endif
      endif
c end pbh

c ======================================================================
c end of surflux.F
c ======================================================================

      end

* ======================================================================
* conv function (called within surflux.F)
* ======================================================================

      character*3 function conv_surf(i)
      implicit none
      integer i,i1,i2,itemp,i3
      character*1 a,b,c
      if(i.lt.10)then
        a=char(i+48)
        conv_surf=a//'  '
      else if(i.lt.100)then
        i1=i/10
        i2=i-i1*10
        a=char(i1+48)
        b=char(i2+48)
        conv_surf=a//b//' '
      else
        i1=i/100
        itemp=i-100*i1
        i2=itemp/10
        i3=itemp-10*i2
        a=char(i1+48)
        b=char(i2+48)
        c=char(i3+48)
        conv_surf=a//b//c
      endif
      end

* ======================================================================
*     ch4_func function (called within surflux.F)
* ======================================================================

c     see: IPCC [1990, 2001]
      real function ch4_func(ch4, n2o)
      implicit none
      real ch4,n2o
      ch4_func = 0.47 * log( 
     1     1.0 +
     2     2.01e-5*(ch4*n2o)**0.75 +
     3     5.31e-15*ch4*(ch4*n2o)**1.52
     4     )
      end
