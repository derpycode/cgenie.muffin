cmsw
cmsw Sets up initial values for ENTS
cmsw
      subroutine setup_ents(
#ifdef icemelt
     :     lin,
#endif
     :     dum_rsc,dum_syr,
     :     dum_ds,dum_dphi,
     :     dum_ca,dum_tq,dum_rmax,
     :     dum_rdtdim,dum_co2_out,
     :     gn_daysperyear,
     :     landice_slicemask_lic,            !< land ice sheet mask
     :     albs_lnd,                         !< surface albedo
     :     land_snow_lnd                     !< land snow cover
     :     )

#include "genie_ents.cmn"
#include "var_ents.cmn"

ccccc FOR NETCDF
        include 'netcdf.inc'
ccccc

      real Cveg_ini
      real Csoil_ini
      real fv_ini
      real photo_ini
      real fws,fta,fco2
      real rland_pts
c      real asurf

      real z0_ini
c      real copt

      real tv

      integer i,j,l,isdump

      character*30 dumpfl
#ifdef icemelt
      character*13 lin
#endif
      character filename*200

c SG > Variables passed from other modules
      real :: dum_rsc
      real :: dum_syr
c      integer :: dum_nyear
      real :: dum_dphi
      real,dimension(maxj)::dum_ds
c      integer :: dum_kmax
c      integer,dimension(maxi,maxj) :: dum_k1
      real,intent (inout),dimension(maxi,maxj)::dum_ca
      real,dimension(2,maxi,maxj)::dum_tq
      real :: dum_rmax
      real :: dum_rdtdim
      real,dimension(maxi,maxj)::dum_co2_out
      real :: gn_daysperyear
c     double precision :: imax_real
c SG Remove imax_real if not needed
c SG <

c land ice mask
      real,dimension(maxi,maxj),intent(in)::landice_slicemask_lic

c surface albedo
      real,dimension(maxi,maxj),intent(inout)::albs_lnd
c land snow cover
      real,dimension(maxi,maxj),intent(inout)::land_snow_lnd

ccccccccccccccccccccccccc FOR NETCDF

c        interface
c
c         subroutine in_ents_netcdf(a,b)
c                character*200 a
c                integer b
c         end subroutine in_ents_netcdf
c
c        end interface
cccccccccccccccccccccccc

c     imax_real = 1.0/imax
cmsw values of tunable constants
c pbh k_constants are no longer read in from "k_constants.dat" but are now namelist paramaters
c k21 is now hard-wired. This is the Universal Gas Constant 

c      open(65,file='../genie-ents/data/k_constants.dat')
c      open(65,file=indir_name(1:lenin)//'k_constants.dat')

c      read(65,*)topt
c      read(65,*)copt

c      read(65,*)k7
c      read(65,*)k8
c      read(65,*)k9
c      read(65,*)k10
c      read(65,*)k11
c      read(65,*)k11a
c      read(65,*)k12
c      read(65,*)k13
c      read(65,*)k14
c      read(65,*)k16
c      read(65,*)k17
c      read(65,*)k18

      rk19=(copt-k13+k14)/(copt-k13)

c      read(65,*)k20
c      read(65,*)k21
      k21=8.314
c      read(65,*)k24

c      read(65,*)k26
c      read(65,*)k29
c      read(65,*)k31
c      read(65,*)k32
c      read(65,*)kz0

      k0=exp(-k31/(tzero-k32))
      q10=exp((10.*k31)/((tzero-k32)**2))

c      close(65)

! molar mass of carbon (kgC/molC)
      mu=0.012 

cmsw reciprocals used to speed up calculation

      rmu=1./mu
      rk30=1./exp(-k31/(topt-k32))
      rk25=1./exp(-k20/(k21*topt))
 
cmsw 'Proper' constants
 
      k_a = 1.773e20            ! number of moles of molecules in the atm

cmsw  Some conversion factors

      gtm = 1.e15/12.           ! GtC to moles C
      gtk = 1.e12               ! GtC to kgC
      rgtm = 12./1.e15          ! mol C to GtC
      rgtk = 1.e-12             ! kgC to GtC
      mtp = 1.e-6                ! ppmv to ppv
      rmtp = 1.e6               ! ppv to ppmv 
      asurfrea = dum_rsc*dum_rsc*dum_ds(1)*dum_dphi ! Each grid box area (m2)
      rasurf = 1./asurfrea      ! reciprocal of above
      rsyr = 1./dum_syr         ! reciprocal no. of secs/year
 
cmsw intial values of global carbon reservoirs (GtC)
   
      print*,' '
      print*,'ENTS (land) option used'

cmsw Read parameters from goin_ents (cm/cmv) or goin_bioents (cbm/cbms)
cmsw Read in parameters for pptn scheme
#ifdef biogem
#else
c      if(ans.eq.'n'.or.ans.eq.'N')then
c         read(5,*)dum
c	 print*,'dum ',dum
c      endif
#endif

cdjl  Read in filenames for orography and icemask
cdjl  Also, time-varying parameters
c      read(5,'(200a)') junk
c      print*,'junk ',trim(junk)
c      read(5,*) t_orog,norog,orogsteps
c      print*,'t_orog,norog,orogsteps ',t_orog,norog,orogsteps
c      read(5,'(200a)') filenameorog
c      print*,'filenameorog ',trim(filenameorog)
c      read(5,'(200a)') junk
c      print*,'junk ',trim(junk)
c      read(5,*) t_lice,nlice,licesteps
c      print*,'t_lice,nlice,licesteps ',t_lice,nlice,licesteps
c      read(5,'(200a)') filenamelice
c      print*,'filenamelice ',trim(filenamelice)

cmsw Precip parameters

      print*, 'rmax'
c      read(5,*)dum_rmax,timepptn
      print*,dum_rmax
       print*,'daysperyear ',gn_daysperyear
       print*,'ents_nyear ',real(ents_nyear)

cmsw Open goin file and read in restart if required

c      open(66,file='../genie-ents/config/ents_config.par')
      open(66,file=condir_name(1:lencon)//'ents_config.par')

cmsw Calculate length of timestep (yrs)

      read(66,*) msimpleland 
      print*,'ENTS called every',msimpleland,'ocean timesteps'
      
      print*,msimpleland,ents_nyear
      print*,dtland,real(msimpleland),ents_nyear
      print*,real(msimpleland)/real(ents_nyear)
      
      dtland=(real(msimpleland)/real(ents_nyear))
      
      print*,'hello?'
      
      print*,'dtland =',dtland,'yr'

cmsw Print out initial sizes of global carbon reservoirs

      print*,'Initial carbon reservoir sizes are...'
      read(66,*)Cveg_ini
      print*,'Cveg_ini =',Cveg_ini,'(GtC)'
      read(66,*)Csoil_ini
      print*,'Csoil_ini =',Csoil_ini,'(GtC)'

      rland_pts=1./land_pts_ents

cmsw Set up spatial initial carbon boxes
cmsw (carbon partitioned equally spatially)
cmsw as all grid boxes have same area, asurf
cmsw Units kgC/m2

      do i=1,imax
         do j=1,jmax
         
            if(ents_k1(i,j).gt.ents_kmax.and.
     &         landice_slicemask_lic(i,j).lt.2.)then

              Cveg(i,j) = Cveg_ini*gtk*rasurf*rland_pts
              Csoil(i,j) = Csoil_ini*gtk*rasurf*rland_pts

            else

              Cveg(i,j) = 0.
              Csoil(i,j) = 0.
    
            endif

         enddo
      enddo

cmsw Land surface albedo paramaters

      asnow=0.8
      asnowv=0.3
      aveg=0.1
      apeat=0.11
      asand=0.3

cmsw Ocean albedo is setup in radfor.F

cmsw Land albedo constant

      albedol = 0.2

cmsw Prescribed surface albedo

      do i=1,imax
         do j=1,jmax
            if(ents_k1(i,j).gt.ents_kmax)then
cmsw Constant land surface albedo

               albs_lnd(i,j)=albedol
 
cmsw Prescribe surface albedo over ice sheets to be 0.8

               if(landice_slicemask_lic(i,j).gt.1.)then
                  albs_lnd(i,j)=0.8
               endif

            else

               albs_lnd(i,j)=0.

            endif
         enddo
      enddo

      print*,'dum_syr ',dum_syr

cmsw Switches for different pptn scheme and orography

      print*,'THIS OPTION IS NO LONGER IN USE HERE - orography'//
     & 'functionality has been moved to genie-embm'
      read(66,*)tv
      print*,tv

cmsw Switch for snow albedo feedback

      print*,'Use snow scheme? 1=Y 0=N'
      read(66,*)snowswitch
      print*,snowswitch

cmsw Switch for offline model

      print*,'THIS OPTION IS NO LONGER IN USE HERE - offline'//
     & 'functionality has been moved to genie-embm'
c     read(66,*)offlineswitch
      print*,tv

cmsw Switch for seasonal fields

      print*,'THIS OPTION IS NO LONGER IN USE HERE - seasonal forcing'//
     & 'functionality has been moved to genie-embm'
      read(66,*)tv
      print*,tv

cmsw Initialise roughness length calculation

      z0_ini=max(0.001,kz0*Cveg(1,1))

cmsw setup initial land temperature,
cmsw bucket size and set ca over land

      print*,'Initial land temp and water'
      print*,dum_tq(1,1,1),min(k8,k9+(k10*Csoil(1,1)))
      print*,'Initial total water on land is'
      print*,min(k8,(k9+(k10*Csoil(1,1))))*asurfrea*land_pts_ents
     1          *1.e-12,'(*10^12 m^3)'
      print*,'Initial roughness length is'
      print*,z0_ini,'m'
      

      do i=1,imax
         do j=1,jmax
            if(ents_k1(i,j).gt.ents_kmax)then
cmsw initial bucket capacity
               bcap(i,j)=min(k8,k9+(k10*Csoil(i,j)))
cmsw initial temp
               tqld(1,i,j) = real(dum_tq(1,i,j),kind(tqld))
cmsw initial bucket size
               tqld(2,i,j) = real(bcap(i,j),kind(tqld))
cmsw absorption coeff over land
               dum_ca(i,j)=0.3
cmsw initial transfer coefficients
               z0(i,j)=z0_ini
            else
               tqld(1,i,j) = 0.
               tqld(2,i,j) = 0.
               z0(i,j) = 0.
            endif
cmsw snow matrix
             land_snow_lnd(i,j)=0
            enddo
       enddo
      
cmsw Length of ocean timestep (s)

       dtdim = 1./dum_rdtdim

cmsw Initialize photo and fv arrays for use in surflux 
cmsw (calculation of transpiration)

      fv_ini=max(1.e-5,1.-exp(-k17*Cveg_ini*gtk*rasurf
     1         *rland_pts))

cmsw New water stress function
      fws=1.

cmsw New temperature response function
      fta=((2.**(0.1*(dum_tq(1,1,1)-topt))) /
     &    ( (1.+exp(0.3*(dum_tq(1,1,1)-k11)))*
     &    (1.+exp(-0.3*(dum_tq(1,1,1)-k12))) ))
     & +  ((2.**(0.1*(dum_tq(1,1,1)-topt))) /
     &    ( (1.+exp(0.6*(dum_tq(1,1,1)-k11a)))*
     &    (1.+exp(-0.3*(dum_tq(1,1,1)-k12))) ))

 
      pco2ld=dum_co2_out(1,1)*rmtp

      if(pco2ld.ge.k13)then
         fco2=(pco2ld-k13)/(pco2ld-k13+k14)
      else
         fco2=0.
      endif

      photo_ini=k18*rk19*
     &         fco2*fws*fta*fv_ini

      do i=1,imax
         do j=1,jmax
            if(ents_k1(i,j).gt.ents_kmax.and.
     &         landice_slicemask_lic(i,j).lt.2.)then
               fv(i,j)=fv_ini
               photo(i,j)=photo_ini 
            else
               fv(i,j)=0.
               photo(i,j)=0.
            endif
         enddo
      enddo

cmsw Emissions option

      read(66,'(a1)') include_emissions
      print*,'Force model with an emissions timeseries?'
      print*,include_emissions

cmsw Continue run
c SG > if then loop is changed so that an alternative sland file is
c      only read for a restart.

      if(ents_restart.eq.'c'.or.ents_restart.eq.'C') then
c SG > ASCII restart files
c       open(1,file='../results/'//trim(lin)//'.sland')
c if netcdf restart output is not selected then ascii file is used

cccccccccccccc CHANGES FOR NETCDF
       if (ents_netin.eq.'y'.or.ents_netin.eq.'Y') then
        call in_ents_netcdf(ents_filenetin,iniday,land_snow_lnd)
        else
cccccccccccccccccccccccc
        filename=indir_name(1:lenin)//trim(ents_restart_file)

        open(1,file=trim(filename))
        call in_ents_ascii(1,land_snow_lnd)
c       close(1)

cmsw Continue ENTS from a different .sland file
c SG > This option has not been tested!!!
c SG It will be incorporated later. Do NOT comment it out.

        read(66,*) isdump
        print*,'Continue run from different .sland file? (0=N 1=Y)'
     1   ,isdump
        read(66,'(a35)') dumpfl
        if(isdump.eq.1) then
          print*,'Different land restart file name is'
          print*,dumpfl 
          open(1,file=trim(dumpfl),status='old')
          print*,'ENTS restart file is ',dumpfl
          call in_ents_ascii(1,land_snow_lnd)
          close(1)
        else
c         print*,'ENTS restart file is ',trim(lin),'.sland'
          print*,'ENTS restart file is ', filename
        endif
      close(66)
c SG <
        close(1)
      endif
      endif
      
#ifdef fixedveg
cmsw Read fixed vegetation carbon, soil carbon and vegetation fraction 
      open(66,file=indir_name(1:lenin)//'fixedveg.dat')
      read(66,*)((Cveg(i,j),i=1,imax),j=1,jmax)
      read(66,*)((Csoil(i,j),i=1,imax),j=1,jmax)
      read(66,*)((fvfv(i,j),i=1,imax),j=1,jmax)
      close(66)
      print*,'NOTE: FIXED VEGETATION OPTION USED, NO TERRESTRIAL CARBON'
      print*,'CYCLE'
      print*,'Vegetation carbon,'
      print*,'soil carbon and vegetation fraction overwritten.'
      print*,'Only physical quantities will be used in restart such as'
      print*,'snow cover and land water.'

cmsw Change fixed vegetation, soil carbon, vegetation fraction and 
cmsw photosynthesis to zero for evap calculation if land ice i.e.
cmsw slicemask(i,j)=2
c      do i=1,imax
c         do j=1,jmax
c            if(ents_k1(i,j).gt.ents_kmax.and.
c     &         slicemask(i,j).gt.1.)then
c               fv(i,j)=0.
c            else
c               fv(i,j)=fvfv(i,j)
c            endif
c         enddo
c      enddo
c
cmsw Calculate bucket sizes
      do i=1,imax
        do j=1,jmax
          if(ents_k1(i,j).gt.ents_kmax)then
             bcap(i,j)=min(k8,k9+(k10*Csoil(i,j)))
cmsw initial roughness length
             z0(i,j)=max(0.001,kz0*Cveg(i,j))
          endif
        enddo
      enddo
#endif

cmsw Initialise sealevel module

      open(66,file=condir_name(1:lencon)//'sealevel_config.par')
      print*,' '
      print*,'SEALEVEL MODULE INITIALIZATION'

cmsw For change in sea level need a reference average
cmsw ocean density
   
      print*,'Reference average ocean density used'
     &      ,' for change in sea-level calculation'
      read(66,*)rhoref
      print*,rhoref

#ifdef icemelt
      print*,'GREENLAND ICE SHEET MELT OPTION USED'

cmsw For change in sea level due to Greenland melt need
cmsw the annual average air temperature over Greenland at
cmsw the begging of the run. Must be preindustrial air temp.
cmsw for parameterisation to work properly.

      print*,'Annual average temp over Greenland at beginning of run'
     &      ,' for change in sea level calculation'
      read(66,*)glairtini
      print*,glairtini

cmsw Switch for adding Greenland freshwater melt to the ocean

      print*,'Greenland freshwater melt added to ocean? 1=Y 0=N'
      read(66,*)icemeltfwfswitch
      print*,icemeltfwfswitch

cmsw Restart icemelt?

      print*,'Restart from previous Greenland melt run? 1=Y 0=N'
      read(66,*)isdump
      print*,isdump

cmsw Initialize variables for Greenland melt

      issl=0.
      glairts=0.
      glmelt=0.

cmsw Read in restart file if continuing run
      if(isdump.eq.1) then
        open(1,file='../results/'//trim(lin)//'.icemelt')
        read(1,*)isslold
        read(1,*)glmelt
        close(1)
      endif

      if(icemeltfwfswitch.eq.1)then
         isslfwf=issl
      else
         isslfwf=0.
      endif
#endif

c #ifdef dosc
      if (dosc) then
cmsw zero annual average arrays

      pco2ld_tot=0.
      tot_mass_ocn_c=0.

      do i=1,imax
         do j=1,jmax
            sphoto(i,j)=0.
            srveg(i,j)=0.
            sleaf(i,j)=0.
            srsoil(i,j)=0.

            sCveg1(i,j)=0.
            sCsoil1(i,j)=0.
            sfv1(i,j)=0.
            sepsv1(i,j)=0.

            stqld(1,i,j)=0.
            stqld(2,i,j)=0.

            sfx0a(i,j)=0.
            sfx0o(i,j)=0.
            sfxsens(i,j)=0.
            sfxlw(i,j)=0.

            sevap(i,j)=0.
            spptn(i,j)=0.
            srelh(i,j)=0.

            sbcap(i,j)=0.
            salbs(i,j)=0.
            ssnow(i,j)=0.
            sz0(i,j)=0.

            tqldavg(1,i,j)=0.
            tqldavg(2,i,j)=0.
            snowavg(i,j)=0.
            albsavg(i,j)=0.
c            palbavg(i,j)=0.
            pptnavg(i,j)=0.
            runavg(i,j)=0.
            bcapavg(i,j)=0.
            evapavg(i,j)=0.
            z0avg(i,j)=0.

            do l=1,7
               fxavg(l,i,j)=0.
            enddo

            gmairttot=0.

         enddo
      enddo

c #endif
      endif
      end
