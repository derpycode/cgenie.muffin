* ======================================================================
* initialise_ents.F
* ======================================================================
* 
* SG (02/01/2K7)
* conversion of simpleland into geniefied ents (henceforth genie-ents)


      subroutine initialise_ents(
     :     dum_lin,
     :     dum_rsc,dum_syr,dum_nyear,
     :     dum_ds,dum_dphi,dum_kmax,
     :     dum_k1,
     :     dum_rmax,dum_rdtdim,
     :     dum_tstar_atm,dum_qstar_atm,
     :     dum_ca,dum_co2_out,
     :     gn_daysperyear,dum_lat,
     :     landice_slicemask_lic,            !< land ice sheet mask
     :     albs_lnd,                         !< surface albedo
     :     land_albs_snow_lnd,               !< albedo over land for snowy conditions
     :     land_albs_nosnow_lnd,             !< albedo over land for snow-free conditions
     :     land_snow_lnd,                    !< land snow cover
     :     land_bcap_lnd,                    !< bucket capacity
     :     land_z0_lnd,                      !< roughness length (m)
     :     land_temp_lnd,                    !< land temperataure
     :     land_moisture_lnd,                !< land moisture content
     :     ntrac_atm,                        !< number of atmospheric tracer
     :     sfcatm_lnd,                       !< atmospheric tracer concentrations
     :     sfxatm_lnd                        !< land-atmosphere exchange fluxes
     :     )

c      include 'var.cmn'
#include "genie_ents.cmn"
#include "var_ents.cmn"

c Variable declaration
      character dum_lin*13
      
      real dum_rsc
      real dum_syr
      integer :: dum_nyear
      real dum_dphi
      real ,dimension(maxj)::dum_ds
      integer :: dum_kmax
      integer,dimension(maxi,maxj) :: dum_k1
      real dum_rmax
      real dum_rdtdim
      real ,intent(inout),dimension(maxi,maxj)::dum_ca
      real ,dimension(maxi,maxj)::dum_tstar_atm
      real ,dimension(maxi,maxj)::dum_qstar_atm
      real ,dimension(maxi,maxj)::dum_co2_out
      real ,dimension(2,maxi,maxj)::dum_tq
      real, dimension(maxj) :: dum_lat
      real :: gn_daysperyear

c land ice mask
      real,dimension(maxi,maxj),intent(in)::landice_slicemask_lic

c surface falbedo
      real,dimension(maxi,maxj),intent(inout)::albs_lnd
      real,dimension(maxi,maxj),intent(out)::land_albs_snow_lnd
      real,dimension(maxi,maxj),intent(out)::land_albs_nosnow_lnd

c land snow cover
      real,dimension(maxi,maxj),intent(inout)::land_snow_lnd

c bucket capacity
      real,dimension(maxi,maxj),intent(out)::land_bcap_lnd

c roughness length
      real,dimension(maxi,maxj),intent(out)::land_z0_lnd

c land temperature
      real,dimension(maxi,maxj),intent(inout)::land_temp_lnd

c land moisture content
      real,dimension(maxi,maxj),intent(inout)::land_moisture_lnd

c atmospheric tracer concentrations and land-atmosphere fluxes
      integer,intent(in)::ntrac_atm
      real,intent(out),dimension(ntrac_atm,maxi,maxj)::sfcatm_lnd
      real,intent(out),dimension(ntrac_atm,maxi,maxj)::sfxatm_lnd

      real::asoil !< surface albedo of soil

c file check flag
      integer ios

c SG > namelist declaration for ENTS goin variables
      NAMELIST /ents_control/indir_name,outdir_name,condir_name
      NAMELIST /ents_control/ents_igrid
      NAMELIST /ents_control/ents_npstp,ents_iwstp
      NAMELIST /ents_control/ents_itstp,ents_ianav
      NAMELIST /ents_control/ents_restart,ents_yearlen
      NAMELIST /ents_control/topt,copt
      NAMELIST /ents_control/k7,k8,k9,k10,k11,k11a,k12
      NAMELIST /ents_control/k13,k14,k16,k17,k18,k20,k24,k26
      NAMELIST /ents_control/k29,k31,k32,kz0
      NAMELIST /ents_control/ents_out_name,ents_netin
      NAMELIST /ents_control/ents_netout,ents_ascout,ents_filenetin
      NAMELIST /ents_control/ents_dirnetout,rstdir_name
      NAMELIST /ents_control/ents_restart_file
      NAMELIST /ents_control/dosc
      NAMELIST /ents_control/atchem_fert
      NAMELIST /ents_control/atchem_update
      NAMELIST /ents_control/start_year
      NAMELIST /ents_control/opt_timeseries_output
      NAMELIST /ents_control/opt_append_data
      NAMELIST /ents_control/par_output_years_file_0d
      NAMELIST /ents_control/par_output_years_file_2d

c Local variables
      integer i,j,k
      integer lnsig1

c ------------------------------------------------------------ c
c INITIALIZE VARIABLES
c ------------------------------------------------------------ c
c ------------------------------------------------------------ c

c ======================================================================
c Setting up ENTS
c ======================================================================

      print*,'======================================================='
      print*,' >>> Initialising ENTS land surface module ...'

c ----------------------------------------------------------------------

c SG read namelist file
      open(unit=59,file='data_ENTS',status='old',iostat=ios)
      if (ios /= 0) then
         print*,'ERROR: could not open ENTS namelist file'
         stop
      end if

c SG read in namelist
      read(UNIT=59,NML=ents_control,IOSTAT=ios)
      if (ios /= 0) then
         print*,'ERROR: could not read ENTS namelist'
         stop
      else
         close(59)
      end if
      

c SG > : read GOIN file
c      open(unit=855,file='goin_ENTS',status='old')

c     Input directory name
c      read(855,'(a)')indir_name
      lenin = lnsig1(indir_name)
      if (indir_name(lenin:lenin).ne.'/') then
c        This 'if' checks for a terminal slash symbol
         lenin = lenin + 1
         indir_name(lenin:lenin) = '/'
      end if
      print*,'Input dir. name ',indir_name(1:lenin)

c     Output directory name
c      read(855,'(a)')outdir_name
      lenout = lnsig1(outdir_name)
      if (outdir_name(lenout:lenout).ne.'/') then
c        This 'if' checks for a terminal slash symbol
         lenout = lenout + 1
         outdir_name(lenout:lenout+1) = '/'
      end if
      print*,'Output dir. name ',outdir_name(1:lenout)
      
c     Config directory name
c      read(855,'(a)')condir_name
      lencon = lnsig1(condir_name)
      if (condir_name(lencon:lencon).ne.'/') then
c        This 'if' checks for a terminal slash symbol
         lencon = lencon + 1
         condir_name(lencon:lencon+1) = '/'
      end if
      print*,'Config dir. name ',condir_name(1:lencon)

c      read (855,*),en_npstp
      print*,'npstp ',ents_npstp
c      read(855,*) ents_out_name
      print*,'ents_out_name = ',trim(ents_out_name)
      print*,'ents_restart_file = ',trim(ents_restart_file)


c      close (855)      
      
      print*, 'lin = ',dum_lin
      print*,dum_rsc,dum_syr,ents_nyear
      print*,'rmax dphi ', dum_rmax,dum_dphi
c      print*,'ilat1_ocn: ',ilat1_ocn
      print*, 'Seasonality enabled: ',dosc

c SG Transfer variables to common variables within ents
       ents_kmax = dum_kmax
       ents_nyear = dum_nyear
C       daysperyear = gn_daysperyear
ccccccccccccccc VNL
        ents_lat=dum_lat
ccccccccccccccc

c Convert tstar_atm & qstar_atm arrays into one tq array  
         do j=1,jmax
            do i=1,imax
               dum_tq(1,i,j) = real(dum_tstar_atm(i,j),kind(dum_tq))
               dum_tq(2,i,j) = real(dum_qstar_atm(i,j),kind(dum_tq))
c	       co2(i,j) = dum_co2_out(i,j)
               ents_k1(i,j) = dum_k1(i,j)
            enddo
         enddo

c Initialise simple land variables
      call setup_ents(
#ifdef icemelt
     :     dum_lin,
#endif
     :     dum_rsc,dum_syr,
     :     dum_ds,dum_dphi,
     :     dum_ca,dum_tq,dum_rmax,dum_rdtdim,
     :     dum_co2_out,
     :     gn_daysperyear,
     :     landice_slicemask_lic,
     :     albs_lnd,
     :     land_snow_lnd
     :     )
cmsw Emissions setup
      if(include_emissions.eq.'y'.or
     &  .include_emissions.eq.'Y')then
c         call setup_emissions(nsteps,maxemit,lenemit,tts,ets)
c         print*,'Emissions timeseries array setup'
c         print*,' '
c         iemit = 1
      endif

c albedo
      do j=1,jmax
         do i=1,imax
            asoil=max(apeat,((apeat-asand)*k10*Csoil(i,j)
     1           /(k8-k9))+asand)
            land_albs_nosnow_lnd(i,j)=(fv(i,j)*aveg)
     &           +((1.-fv(i,j))*asoil)
            albs_lnd(i,j)=land_albs_nosnow_lnd(i,j)
            if (snowswitch.eq.1) then
               land_albs_snow_lnd(i,j)=((asnow-asnowv)
     &           *exp(-k7*Cveg(i,j)))+asnowv
            else
               land_albs_snow_lnd(i,j)=land_albs_nosnow_lnd(i,j)
            endif
            if(land_snow_lnd(i,j).eq.1.and.snowswitch.eq.1)then
               albs_lnd(i,j)=land_albs_snow_lnd(i,j)
            endif
            land_bcap_lnd(i,j)=real(bcap(i,j)
     &                   ,kind(land_bcap_lnd))
            land_z0_lnd(i,j)=real(z0(i,j),kind(land_z0_lnd))
            land_temp_lnd(i,j)=real(tqld(1,i,j)
     &                   ,kind(land_temp_lnd))
            land_moisture_lnd(i,j)=real(tqld(2,i,j)
     &                   ,kind(land_moisture_lnd))
         enddo
      enddo

c initialise land-atmosphere fluxes
      do j=1,jmax
         do i=1,imax
            do k=1,ntrac_atm
               sfcatm_lnd(k,i,j) = 0.0
               sfxatm_lnd(k,i,j) = 0.0
            enddo
         enddo
      enddo

      print*,' <<< Initialisation complete'
      print*,'======================================================='
            
* ======================================================================
* end of initialise_ents.F
* ======================================================================
      
      return
      end
