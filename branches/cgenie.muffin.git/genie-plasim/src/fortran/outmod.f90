!     =================
!     SUBROUTINE PACKGP
!     =================

      subroutine packgp(pu,pmin,psca,kp)
      use pumamod
      implicit none
      integer j
      real zmax,zran
      real         :: pu(2,NPGP)   ! in:  array to pack
      real(kind=4) :: pmin         ! out: minimum
      real(kind=4) :: psca         ! out: scaling factor
      integer(kind=4) :: kp(NPGP)  ! out: array for packed data
      integer(kind=4) :: ir,ii
      
      zmax = maxval(pu(:,:))
      pmin = minval(pu(:,:))
      zran = zmax - pmin           ! range of values
      if (zran > 1.0e-25) then
         psca = 64000.0 / zran
      else
         psca = 1.0
      endif

      do j = 1 , NPGP
         ir = ishft(nint((pu(1,j) - pmin) * psca),  16)
         ii = ibits(nint((pu(2,j) - pmin) * psca),0,16)
         kp(j) = ior(ir,ii)
      enddo

      return
      end

!     ===================
!     SUBROUTINE UNPACKGP
!     ===================

      subroutine unpackgp(pu,pmin,psca,kp)
      use pumamod
      implicit none
      integer j,ir,ii
      real            :: pu(2,NPGP)
      integer(kind=4) :: pmin
      integer(kind=4) :: psca
      integer(kind=4) :: kp(NPGP)

      do j = 1 , NPGP
         ir = iand(ishft(kp(j),-16),65535)
         ii = iand(kp(j),65535)
         pu(1,j) = ir / psca + pmin
         pu(2,j) = ii / psca + pmin
      enddo

      return
      end

!     =================
!     SUBROUTINE PACKSP
!     =================

      subroutine packsp(pu,pp,kp)
      use pumamod
      implicit none
      integer j
      real zmax,zmin,zabs,zsca
      real(kind=4)    :: pu(2,NCSP)       ! in:  array to pack
      real(kind=4)    :: pp(NTP1+1)       ! out: modes for m=0
      integer(kind=4) :: kp(NTP1+1:NCSP)  ! out: array for packed data
      integer(kind=4) :: ir,ii

      pp(1:NTP1) = pu(1,1:NTP1) ! store first modes unpacked

      zmax = maxval(pu(:,NTP1+1:NCSP))
      zmin = minval(pu(:,NTP1+1:NCSP))
      zabs = max(abs(zmax),abs(zmin))
      if (zabs > 1.0e-25) then
         zsca = 32000.0 / zabs
      else
         zsca = 1.0
      endif
      pp(NTP1+1) = zsca

      do j = NTP1+1 , NCSP
         ir = ishft(32768+nint(pu(1,j) * zsca),  16)
         ii = ibits(32768+nint(pu(2,j) * zsca),0,16)
         kp(j) = ior(ir,ii)
      enddo

      return
      end

!     ===================
!     SUBROUTINE UNPACKSP
!     ===================

      subroutine unpacksp(pu,pp,kp)
      use pumamod
      implicit none
      integer j,ir,ii
      real zsca
      real :: pu(2,NCSP)
      real(kind=4) :: pp(NTP1+1)
      integer(kind=4) :: kp(NTP1+1:NCSP)

      pu(1,1:NTP1) = pp(1:NTP1) ! first modes unpacked
      zsca = pp(NTP1+1)

      do j = NTP1+1 , NCSP
         ir = iand(ishft(kp(j),-16),65535) - 32768
         ii = iand(kp(j),65535) - 32768
         pu(1,j) = ir / zsca
         pu(2,j) = ii / zsca
      enddo

      return
      end

!     =================
!     SUBROUTINE OUTINI
!     =================

      subroutine outini
      use pumamod
      implicit none

!     If PLASIM runs as 64 bit version truncate output variables
!     to 32 bit (otherwise the pumaburner will hickup)

      real(kind=4) :: zsigmah(NLEV)
      integer(kind=4) :: itru,ilat,ilev
!
      character(len=8) ypuma
      data ypuma/'PUMA-II '/

      itru = NTRU
      ilat = NLAT
      ilev = NLEV
      open  (40,file=trim(outdir_name)//'/puma_output',form='unformatted')
      write (40) ypuma
      write (40) itru
      write (40) ilat
      write (40) ilev
      zsigmah(:)=sigmah(:)
      write (40) zsigmah

      return
      end

!     ==================
!     SUBROUTINE WRITEGP
!     ==================

      subroutine writegp(kunit,pf,kcode,klev)
      use pumamod
      implicit none
      integer nmin,nhour,nday,nmonth,nyear,istep
      integer kcode,kunit,klev
      real :: pf(NHOR)
      real :: zf(NUGP)
      integer(kind=4) :: ihead(8)
      integer(kind=4) :: la(NPGP)
      real(kind=4) :: zmin
      real(kind=4) :: zsca
      real(kind=4) :: zzf(NUGP)

      istep = nstep
      call ntomin(istep,nmin,nhour,nday,nmonth,nyear)

      ihead(1) = kcode
      ihead(2) = klev
      ihead(3) = nday + 100 * nmonth + 10000 * nyear
      ihead(4) = nmin + 100 * nhour
      ihead(5) = NLON
      ihead(6) = NLAT
      ihead(7) = 1
      ihead(8) = n_days_per_year

      call mpgagp(zf,pf,1)

      if (mypid == NROOT) then
         write (kunit) ihead
         if (npackgp == 1) then
            call packgp(zf,zmin,zsca,la)
            write (kunit) zmin,zsca
            write (kunit) la
         else
            zzf(:) = zf(:)
            write (kunit) zzf
         endif
      endif

      return
      end

!     ==================
!     SUBROUTINE WRITESP
!     ==================

      subroutine writesp(kunit,pf,kcode,klev,pscale,poff)
      use pumamod
      implicit none
      integer istep,nmin,nhour,nday,nmonth,nyear
      integer kunit,kcode,klev
      real pscale, poff
      real :: pf(NRSP)
      integer(kind=4) :: ihead(8)
      integer(kind=4) :: la(NTP1+1:NCSP)
      real(kind=4) :: zf(NRSP)
      real(kind=4) :: za(NTP1+1)

      istep = nstep
      call ntomin(istep,nmin,nhour,nday,nmonth,nyear)

      ihead(1) = kcode
      ihead(2) = klev
      ihead(3) = nday + 100 * nmonth + 10000 * nyear
      ihead(4) = nmin + 100 * nhour
      ihead(5) = NRSP
      ihead(6) = 1
      ihead(7) = 1
      ihead(8) = n_days_per_year

!     normalize ECHAM compatible and scale to physical dimensions

      zf(:) = pf(:) * spnorm(1:NRSP) * pscale
      zf(1) = zf(1) + poff ! Add offset if necessary
      write (kunit) ihead
      if (npacksp == 1) then
         call packsp(zf,za,la)
         write (kunit) za
         write (kunit) la
      else
         write (kunit) zf
      endif

      return
      end

!     ================
!     SUBROUTINE OUTSP
!     ================

      subroutine outsp
      use pumamod
      implicit none
      integer jlev
      real zsave

!     ************
!     * orograpy *
!     ************

      call writesp(40,so,129,0,CV*CV,0.)

!     ************
!     * pressure *
!     ************

      call writesp(40,sp,152,0,1.0,log(psurf))

!     ***************
!     * temperature *
!     ***************

      do jlev = 1 , NLEV
         call writesp(40,st(1,jlev),130,jlev,ct,t0(jlev) * ct)
      enddo

!     *********************
!     * specific humidity *
!     *********************

      if (nqspec == 1) then
         do jlev = 1 , NLEV
            call writesp(40,sqout(1,jlev),133,jlev,1.0,0.0)
         enddo
      endif

!     **************
!     * divergence *
!     **************

      do jlev = 1 , NLEV
         call writesp(40,sd(1,jlev),155,jlev,ww,0.0)
      enddo

!     *************
!     * vorticity *
!     *************

      do jlev = 1 , NLEV
         zsave = sz(3,jlev)
         sz(3,jlev) = sz(3,jlev) - plavor
         call writesp(40,sz(1,jlev),138,jlev,ww,0.0)
         sz(3,jlev) = zsave
      enddo

      return
      end


!     ================
!     SUBROUTINE OUTGP
!     ================

      subroutine outgp
!PBH netcdf
      use landmod	!needed for orography (includes pumamod)
      use netcdf
      implicit none
      include 'netcdf.inc'
!PBH netdcf variables
      character(len=100) netcdf_filename
      integer ierr,fileid
      integer dimidx,dimidy,dimidz,dimidm
      integer varidx,varidy,varidz,varidm
      integer start1(1),count1(1)
      integer start2(2),count2(2)
      integer start3(3),count3(3)
      integer start3m(3),count3m(3)
      integer dd(3),ddm(3)
!PBH gridpoint variables
      integer varidatsa			!surface air temperature
      integer varidats0			!surface temperature
      integer varidatsoil		!soil temperature
      integer varidahfls		!heat flux from soil
      integer varidaprl			!large scale precipitation
      integer varidaprc			!convective precipitation
      integer varidaprt			!total precipitation
      integer varidacc			!cloud cover
      integer varidaevap		!evaporation
      integer varidaroff		!runoff
      integer varidawatc		!water content of soil
      integer varidaqvi			!vertically integrated spec hum
      integer varidashfl		!sensible heat flux
      integer varidalhfl		!latent heat flux
      integer varidassol		!surface solar radiation
      integer varidasthr		!surface thermal radiation
      integer varidassolu		!surface solar upward
      integer varidasthru		!surface thermal upward
      integer varidatsol                !top solar radiation
      integer varidatthr                !top thermal radiation
      integer varidataux		!u-stress
      integer varidatauy		!v-stress
      integer varidaveg			!vegetation cover
      integer varidacveg		!vegetation carbon
      integer varidacsoil		!soil carbon
      integer varidaforest		!forest cover
      integer varidawmax		!field capacity
      integer varidalai			!leaf area index
      integer varidaz0			!surface roughness
      integer varidaalb			!albedo
      integer varidagpp			!gross primary production
      integer varidanpp			!net primary production
      integer varidaresh		!heterotrophic respiration
      integer varidalitter		!litter production
      integer varidagppl		!light limited GPP
      integer varidagppw		!water limited GPP
      integer variddglac		!glacier mask
      integer varidaicec		!sea ice cover
      integer varidaiced		!sea ice thickness
      integer varidasnow		!snow depth
      integer variddoro			!orography
      integer variddls			!land-sea mask
      integer variddarea		!grid cell area
      integer varidap			!pressure
      integer varidddummy 		!for debug
      integer varidadummy 		!for debug
!Seasonal gridpoint variables
      integer varidapr_s1		!DJF pptn
      integer varidapr2_s1
      integer varidatsa_s1		!DJF temperature
      integer varidatsa2_s1
      integer varidapr_s2		!MAM pptn
      integer varidapr2_s2
      integer varidatsa_s2		!MAM temperature
      integer varidatsa2_s2
      integer varidapr_s3		!JJA pptn
      integer varidapr2_s3
      integer varidatsa_s3		!JJA temperature
      integer varidatsa2_s3
      integer varidapr_s4		!SON pptn
      integer varidapr2_s4
      integer varidatsa_s4		!SON temperature
      integer varidatsa2_s4
      integer varidacc_s1               !DJF clouds
      integer varidacc_s2               !MAM clouds
      integer varidacc_s3               !JJA clouds
      integer varidacc_s4               !SON clouds
      integer varidaevap_s1             !DJF evaporation
      integer varidaevap_s2             !MAM evaporation
      integer varidaevap_s3             !JJA evaporation
      integer varidaevap_s4             !SON evaporation
      integer varidasst_s1             !DJF GENIE SST
      integer varidasst_s2             !MAM GENIE SST
      integer varidasst_s3             !JJA GENIE SST
      integer varidasst_s4             !SON GENIE SST
      integer varidasia_s1             !DJF GENIE SIA
      integer varidasia_s2             !MAM GENIE SIA
      integer varidasia_s3             !JJA GENIE SIA
      integer varidasia_s4             !SON GENIE SIA
      integer varidassol_s1             !DJF incoming solar
      integer varidassol_s2             !MAM incoming solar
      integer varidassol_s3             !JJA incoming solar
      integer varidassol_s4             !SON incoming solar
      integer varidasthr_s1             !DJF incoming thermal
      integer varidasthr_s2             !MAM incoming thermal
      integer varidasthr_s3             !JJA incoming thermal
      integer varidasthr_s4             !SON incoming thermal
      integer varidataux_s1             !DJF ustress
      integer varidataux_s2             !MAM ustress
      integer varidataux_s3             !JJA ustress
      integer varidataux_s4             !SON ustress
      integer varidatauy_s1             !DJF vstress
      integer varidatauy_s2             !MAM vstress
      integer varidatauy_s3             !JJA vstress
      integer varidatauy_s4             !SON vstress
      integer varidataux_m              !monthly ustress (12x array)
      integer varidatauy_m              !monthly vstress (12x array)
      integer varida3u_s1             !DJF 3D uspeed
      integer varida3u_s2             !MAM 3D uspeed
      integer varida3u_s3             !JJA 3D uspeed
      integer varida3u_s4             !SON 3D uspeed
      integer varida3v_s1             !DJF 3D vspeed
      integer varida3v_s2             !MAM 3D vspeed
      integer varida3v_s3             !JJA 3D vspeed
      integer varida3v_s4             !SON 3D vspeed
!PBH miscellaneous variables
      integer i,jlev
      integer istep,nmin,nhour,nday,nmonth,nyear
      integer month(12)
      real*4 lon(NLON),lat(NLAT),lev(NLEV)    !coordinates
      real*4 data(NHOR)                       !temporary data field
      real*4 dataM(NHOR,12)                   !temporary monthly field 12 x 2D
      real*4 data3(NHOR,NLEV)                 !temporary 3D data field

!PBH create netcdf file with date stamp
      istep=nstep
      call ntomin(istep,nmin,nhour,nday,nmonth,nyear)
      write(netcdf_filename,10000) nyear,nmonth,nday
10000 format('/',i4.4,'-',i2.2,'-',i2.2,'_plasim.nc')
      ierr=nf_create(trim(outdir_name)//trim(netcdf_filename),nf_clobber,fileid)


!PBH define netcdf dimensions and variables
      ierr=nf_def_dim(fileid,'longitude',NLON,dimidx)
      ierr=nf_def_dim(fileid,'latitude',NLAT,dimidy)
      ierr=nf_def_dim(fileid,'level',NLEV,dimidz)
      ierr=nf_def_dim(fileid,'month',12,dimidm)
      dd(1)=dimidx
      ierr=nf_def_var(fileid,'longitude',nf_real,1,dd,varidx) 
      dd(1)=dimidy
      ierr=nf_def_var(fileid,'latitude',nf_real,1,dd,varidy)
      dd(1)=dimidz
      ierr=nf_def_var(fileid,'level',nf_real,1,dd,varidz)
      dd(1)=dimidm
      ierr=nf_def_var(fileid,'month',nf_int,1,dd,varidm)
      dd(1)=dimidx
      dd(2)=dimidy
      dd(3)=dimidz
      ddm(1)=dimidx !monthly data
      ddm(2)=dimidy
      ddm(3)=dimidm
!PBH gridpoint
      ierr=nf_def_var(fileid,'puma_temperature_surface_air',nf_real,2,dd,varidatsa)
      ierr=nf_def_var(fileid,'puma_temperature_surface',nf_real,2,dd,varidats0)
      ierr=nf_def_var(fileid,'puma_temperature_soil',nf_real,2,dd,varidatsoil)
      ierr=nf_def_var(fileid,'puma_heat_flux_into_soil',nf_real,2,dd,varidahfls)
      ierr=nf_def_var(fileid,'puma_precipitation_large_scale',nf_real,2,dd,varidaprl)
      ierr=nf_def_var(fileid,'puma_precipitation_convective',nf_real,2,dd,varidaprc)
      ierr=nf_def_var(fileid,'puma_precipitation_total',nf_real,2,dd,varidaprt)
      ierr=nf_def_var(fileid,'puma_cloud_cover',nf_real,2,dd,varidacc)
      ierr=nf_def_var(fileid,'puma_evaporation',nf_real,2,dd,varidaevap)
      ierr=nf_def_var(fileid,'puma_runoff',nf_real,2,dd,varidaroff)
      ierr=nf_def_var(fileid,'puma_soil_moisture',nf_real,2,dd,varidawatc)
      ierr=nf_def_var(fileid,'puma_vert_intgrtd_spec_humidity',nf_real,2,dd,varidaqvi)
      ierr=nf_def_var(fileid,'ebal_sensible_heat_flux',nf_real,2,dd,varidashfl)
      ierr=nf_def_var(fileid,'ebal_latent_heat_flux',nf_real,2,dd,varidalhfl)
      ierr=nf_def_var(fileid,'ebal_surface_solar_radiation',nf_real,2,dd,varidassol)
      ierr=nf_def_var(fileid,'ebal_surface_thermal_radiation',nf_real,2,dd,varidasthr)
      ierr=nf_def_var(fileid,'ebal_surface_solar_upward',nf_real,2,dd,varidassolu)
      ierr=nf_def_var(fileid,'ebal_surface_thermal_upward',nf_real,2,dd,varidasthru)
      ierr=nf_def_var(fileid,'ebal_top_solar',nf_real,2,dd,varidatsol)
      ierr=nf_def_var(fileid,'ebal_top_thermal',nf_real,2,dd,varidatthr)
      ierr=nf_def_var(fileid,'puma_u-stress',nf_real,2,dd,varidataux)
      ierr=nf_def_var(fileid,'puma_v-stress',nf_real,2,dd,varidatauy)
      ierr=nf_def_var(fileid,'ents_fractional_vegetation',nf_real,2,dd,varidaveg)
      ierr=nf_def_var(fileid,'ents_vegetation_carbon',nf_real,2,dd,varidacveg)
      ierr=nf_def_var(fileid,'ents_soil_carbon',nf_real,2,dd,varidacsoil)
      ierr=nf_def_var(fileid,'simba_forest_cover',nf_real,2,dd,varidaforest)
      ierr=nf_def_var(fileid,'ents_field_capacity',nf_real,2,dd,varidawmax)
      ierr=nf_def_var(fileid,'simba_leaf_area_index',nf_real,2,dd,varidalai)
      ierr=nf_def_var(fileid,'ents_surface_roughness',nf_real,2,dd,varidaz0)
      ierr=nf_def_var(fileid,'ents_surface_albedo',nf_real,2,dd,varidaalb)
      ierr=nf_def_var(fileid,'ents_gross_primary_production',nf_real,2,dd,varidagpp)
      ierr=nf_def_var(fileid,'ents_net_primary_production',nf_real,2,dd,varidanpp)
      ierr=nf_def_var(fileid,'ents_soil_respiration',nf_real,2,dd,varidaresh)
      ierr=nf_def_var(fileid,'ents_litter_production',nf_real,2,dd,varidalitter)
      ierr=nf_def_var(fileid,'ents_gpp_temperature_limitation',nf_real,2,dd,varidagppl)
      ierr=nf_def_var(fileid,'ents_gpp_moisture_limitation',nf_real,2,dd,varidagppw)
      ierr=nf_def_var(fileid,'cryo_glacier_mask',nf_real,2,dd,variddglac)
      ierr=nf_def_var(fileid,'cryo_sea_ice_cover',nf_real,2,dd,varidaicec)
      ierr=nf_def_var(fileid,'cryo_sea_ice_thickness',nf_real,2,dd,varidaiced)
      ierr=nf_def_var(fileid,'cryo_snow_depth',nf_real,2,dd,varidasnow)
      ierr=nf_def_var(fileid,'puma_surface_pressure',nf_real,2,dd,varidap)
      ierr=nf_def_var(fileid,'grid_orography',nf_real,2,dd,variddoro)
      ierr=nf_def_var(fileid,'grid_landsea_mask',nf_real,2,dd,variddls)
      ierr=nf_def_var(fileid,'grid_cell_area',nf_real,2,dd,variddarea)
      ierr=nf_def_var(fileid,'debug_instantaneous',nf_real,2,dd,varidddummy)
      ierr=nf_def_var(fileid,'debug_average',nf_real,2,dd,varidadummy)

!seasonal
      if(noutseas.eq.1) then
       ierr=nf_def_var(fileid,'DJF_precipitation',nf_real,2,dd,varidapr_s1)
       ierr=nf_def_var(fileid,'DJF_precipitation_variability',nf_real,2,dd,varidapr2_s1)
       ierr=nf_def_var(fileid,'DJF_temperature',nf_real,2,dd,varidatsa_s1)
       ierr=nf_def_var(fileid,'DJF_temperature_variability',nf_real,2,dd,varidatsa2_s1)
       ierr=nf_def_var(fileid,'MAM_precipitation',nf_real,2,dd,varidapr_s2)
       ierr=nf_def_var(fileid,'MAM_precipitation_variability',nf_real,2,dd,varidapr2_s2)
       ierr=nf_def_var(fileid,'MAM_temperature',nf_real,2,dd,varidatsa_s2)
       ierr=nf_def_var(fileid,'MAM_temperature_variability',nf_real,2,dd,varidatsa2_s2)
       ierr=nf_def_var(fileid,'JJA_precipitation',nf_real,2,dd,varidapr_s3)
       ierr=nf_def_var(fileid,'JJA_precipitation_variability',nf_real,2,dd,varidapr2_s3)
       ierr=nf_def_var(fileid,'JJA_temperature',nf_real,2,dd,varidatsa_s3)
       ierr=nf_def_var(fileid,'JJA_temperature_variability',nf_real,2,dd,varidatsa2_s3)
       ierr=nf_def_var(fileid,'SON_precipitation',nf_real,2,dd,varidapr_s4)
       ierr=nf_def_var(fileid,'SON_precipitation_variability',nf_real,2,dd,varidapr2_s4)
       ierr=nf_def_var(fileid,'SON_temperature',nf_real,2,dd,varidatsa_s4)
       ierr=nf_def_var(fileid,'SON_temperature_variability',nf_real,2,dd,varidatsa2_s4)
       ierr=nf_def_var(fileid,'DJF_cloud_cover',nf_real,2,dd,varidacc_s1)
       ierr=nf_def_var(fileid,'MAM_cloud_cover',nf_real,2,dd,varidacc_s2)
       ierr=nf_def_var(fileid,'JJA_cloud_cover',nf_real,2,dd,varidacc_s3)
       ierr=nf_def_var(fileid,'SON_cloud_cover',nf_real,2,dd,varidacc_s4)
       ierr=nf_def_var(fileid,'DJF_evaporation',nf_real,2,dd,varidaevap_s1)
       ierr=nf_def_var(fileid,'MAM_evaporation',nf_real,2,dd,varidaevap_s2)
       ierr=nf_def_var(fileid,'JJA_evaporation',nf_real,2,dd,varidaevap_s3)
       ierr=nf_def_var(fileid,'SON_evaporation',nf_real,2,dd,varidaevap_s4)
       ierr=nf_def_var(fileid,'DJF_GENIE_SST',nf_real,2,dd,varidasst_s1)
       ierr=nf_def_var(fileid,'MAM_GENIE_SST',nf_real,2,dd,varidasst_s2)
       ierr=nf_def_var(fileid,'JJA_GENIE_SST',nf_real,2,dd,varidasst_s3)
       ierr=nf_def_var(fileid,'SON_GENIE_SST',nf_real,2,dd,varidasst_s4)
       ierr=nf_def_var(fileid,'DJF_GENIE_SIA',nf_real,2,dd,varidasia_s1)
       ierr=nf_def_var(fileid,'MAM_GENIE_SIA',nf_real,2,dd,varidasia_s2)
       ierr=nf_def_var(fileid,'JJA_GENIE_SIA',nf_real,2,dd,varidasia_s3)
       ierr=nf_def_var(fileid,'SON_GENIE_SIA',nf_real,2,dd,varidasia_s4)
       ierr=nf_def_var(fileid,'DJF_incoming_solar',nf_real,2,dd,varidassol_s1)
       ierr=nf_def_var(fileid,'MAM_incoming_solar',nf_real,2,dd,varidassol_s2)
       ierr=nf_def_var(fileid,'JJA_incoming_solar',nf_real,2,dd,varidassol_s3)
       ierr=nf_def_var(fileid,'SON_incoming_solar',nf_real,2,dd,varidassol_s4)
       ierr=nf_def_var(fileid,'DJF_incoming_thermal',nf_real,2,dd,varidasthr_s1)
       ierr=nf_def_var(fileid,'MAM_incoming_thermal',nf_real,2,dd,varidasthr_s2)
       ierr=nf_def_var(fileid,'JJA_incoming_thermal',nf_real,2,dd,varidasthr_s3)
       ierr=nf_def_var(fileid,'SON_incoming_thermal',nf_real,2,dd,varidasthr_s4)
       ierr=nf_def_var(fileid,'DJF_ustress',nf_real,2,dd,varidataux_s1)
       ierr=nf_def_var(fileid,'MAM_ustress',nf_real,2,dd,varidataux_s2)
       ierr=nf_def_var(fileid,'JJA_ustress',nf_real,2,dd,varidataux_s3)
       ierr=nf_def_var(fileid,'SON_ustress',nf_real,2,dd,varidataux_s4)
       ierr=nf_def_var(fileid,'DJF_vstress',nf_real,2,dd,varidatauy_s1)
       ierr=nf_def_var(fileid,'MAM_vstress',nf_real,2,dd,varidatauy_s2)
       ierr=nf_def_var(fileid,'JJA_vstress',nf_real,2,dd,varidatauy_s3)
       ierr=nf_def_var(fileid,'SON_vstress',nf_real,2,dd,varidatauy_s4)
      endif
!monthly fields
      if(noutmnth.eq.1) then
       ierr=nf_def_var(fileid,'monthly_ustress',nf_real,3,ddm,varidataux_m)
       ierr=nf_def_var(fileid,'monthly_vstress',nf_real,3,ddm,varidatauy_m)
      endif
!3D fields
      if(nout3D.eq.1) then
       ierr=nf_def_var(fileid,'DJF_uspeed',nf_real,3,dd,varida3u_s1)
       ierr=nf_def_var(fileid,'MAM_uspeed',nf_real,3,dd,varida3u_s2)
       ierr=nf_def_var(fileid,'JJA_uspeed',nf_real,3,dd,varida3u_s3)
       ierr=nf_def_var(fileid,'SON_uspeed',nf_real,3,dd,varida3u_s4)
       ierr=nf_def_var(fileid,'DJF_vspeed',nf_real,3,dd,varida3v_s1)
       ierr=nf_def_var(fileid,'MAM_vspeed',nf_real,3,dd,varida3v_s2)
       ierr=nf_def_var(fileid,'JJA_vspeed',nf_real,3,dd,varida3v_s3)
       ierr=nf_def_var(fileid,'SON_vspeed',nf_real,3,dd,varida3v_s4)
      endif

      ierr=nf_enddef(fileid)

!PBH netcdf coordinates
      do i=1,NLON
        lon(i)=float(i-1)*360.0/NLON
      enddo
      do i=1,NLAT
        lat(i)=deglat(i)
      enddo
      do i=1,NLEV
        lev(i)=sigma(i)
      enddo
      do i=1,12
       month(i)=i
      enddo
      start1(1)=1
      count1(1)=NLON
      ierr=nf_put_vara_real(fileid,varidx,start1,count1,lon)
      start1(1)=1
      count1(1)=NLAT
      ierr=nf_put_vara_real(fileid,varidy,start1,count1,lat)
      start1(1)=1
      count1(1)=NLEV
      ierr=nf_put_vara_real(fileid,varidz,start1,count1,lev)
      start1(1)=1
      count1(1)=12
      ierr=nf_put_vara_int(fileid,varidm,start1,count1,month)

!PBH netdcf data fields
      start2(1)=1
      start2(2)=1
      count2(1)=NLON
      count2(2)=NLAT
!PBH gridpoint
      data(:)=atsa(:)/real(naccuout)-273.15		!surface air temperature (Celcius)
      ierr=nf_put_vara_real(fileid,varidatsa,start2,count2,data)
      data(:)=ats0(:)/real(naccuout)-273.15		!surface temperature (Celcius)
      ierr=nf_put_vara_real(fileid,varidats0,start2,count2,data)
      data(:)=atsoil(:)/real(naccuout)-273.15*dls(:)	!soil temperature (Celcius)
      ierr=nf_put_vara_real(fileid,varidatsoil,start2,count2,data)
      data(:)=ahfls(:)/real(naccuout)	                !heat flux from soil
      ierr=nf_put_vara_real(fileid,varidahfls,start2,count2,data)
      data(:)=aprl(:)/real(naccuout)			!large scale precipitation
      ierr=nf_put_vara_real(fileid,varidaprl,start2,count2,data)
      data(:)=aprc(:)/real(naccuout)			!convective precipitation
      ierr=nf_put_vara_real(fileid,varidaprc,start2,count2,data)
      data(:)=(aprl(:)+aprc(:))/real(naccuout)		!total precipitation
      ierr=nf_put_vara_real(fileid,varidaprt,start2,count2,data)
      data(:)=acc(:)/real(naccuout)			!cloud cover
      ierr=nf_put_vara_real(fileid,varidacc,start2,count2,data)
      data(:)=aevap(:)/real(naccuout)			!evaporation
      ierr=nf_put_vara_real(fileid,varidaevap,start2,count2,data)
      data(:)=aroff(:)/real(naccuout)			!runoff
      ierr=nf_put_vara_real(fileid,varidaroff,start2,count2,data)
      data(:)=awatc(:)/real(naccuout)			!soil moisture
      ierr=nf_put_vara_real(fileid,varidawatc,start2,count2,data)
      data(:)=aqvi(:)/real(naccuout)			!vertically int spec hum
      ierr=nf_put_vara_real(fileid,varidaqvi,start2,count2,data)
      data(:)=ashfl(:)/real(naccuout)			!sensible heat flux
      ierr=nf_put_vara_real(fileid,varidashfl,start2,count2,data)
      data(:)=alhfl(:)/real(naccuout)			!latent heat flux
      ierr=nf_put_vara_real(fileid,varidalhfl,start2,count2,data)
      data(:)=assol(:)/real(naccuout)			!surface solar radiation
      ierr=nf_put_vara_real(fileid,varidassol,start2,count2,data)
      data(:)=asthr(:)/real(naccuout)			!surface thermal radiation
      ierr=nf_put_vara_real(fileid,varidasthr,start2,count2,data)
      data(:)=assolu(:)/real(naccuout)			!surface solar upward
      ierr=nf_put_vara_real(fileid,varidassolu,start2,count2,data)
      data(:)=asthru(:)/real(naccuout)			!surface thermal upward
      ierr=nf_put_vara_real(fileid,varidasthru,start2,count2,data)
      data(:)=atsol(:)/real(naccuout)                   !top solar
      ierr=nf_put_vara_real(fileid,varidatsol,start2,count2,data)
      data(:)=atthr(:)/real(naccuout)                   !top thermal
      ierr=nf_put_vara_real(fileid,varidatthr,start2,count2,data)
      data(:)=ataux(:)/real(naccuout)			!u-stress (acc)
      ierr=nf_put_vara_real(fileid,varidataux,start2,count2,data)
      data(:)=atauy(:)/real(naccuout)			!v-stress (acc)
      ierr=nf_put_vara_real(fileid,varidatauy,start2,count2,data)
      data(:)=aveg(:)/real(naccuout)			!vegetation cover
      ierr=nf_put_vara_real(fileid,varidaveg,start2,count2,data)
      data(:)=acveg(:)/real(naccuout)			!vegetation carbon
      ierr=nf_put_vara_real(fileid,varidacveg,start2,count2,data)
      data(:)=acsoil(:)/real(naccuout)			!soil carbon
      ierr=nf_put_vara_real(fileid,varidacsoil,start2,count2,data)
      data(:)=aforest(:)/real(naccuout)			!forest cover
      ierr=nf_put_vara_real(fileid,varidaforest,start2,count2,data)
      data(:)=awmax(:)*dls(:)/real(naccuout)		!field capacity (land only)
      ierr=nf_put_vara_real(fileid,varidawmax,start2,count2,data)
      data(:)=alai(:)/real(naccuout)			!leaf area index
      ierr=nf_put_vara_real(fileid,varidalai,start2,count2,data)
      data(:)=az0(:)/real(naccuout)			!surface roughness
      ierr=nf_put_vara_real(fileid,varidaz0,start2,count2,data)
      data(:)=aalb(:)/real(naccuout)			!albedo
      ierr=nf_put_vara_real(fileid,varidaalb,start2,count2,data)
      data(:)=agpp(:)/real(naccuout)			!gross primary production
      ierr=nf_put_vara_real(fileid,varidagpp,start2,count2,data)
      data(:)=anpp(:)/real(naccuout)			!net primary production
      ierr=nf_put_vara_real(fileid,varidanpp,start2,count2,data)
      data(:)=aresh(:)/real(naccuout)			!heterotrophic respiration
      ierr=nf_put_vara_real(fileid,varidaresh,start2,count2,data)
      data(:)=alitter(:)/real(naccuout)			!litter production
      ierr=nf_put_vara_real(fileid,varidalitter,start2,count2,data)
      data(:)=agppl(:)/real(naccuout)			!light limited GPP
      ierr=nf_put_vara_real(fileid,varidagppl,start2,count2,data)
      data(:)=agppw(:)/real(naccuout)			!water limited GPP
      ierr=nf_put_vara_real(fileid,varidagppw,start2,count2,data)
      data(:)=dglac(:)					!glacier mask
      ierr=nf_put_vara_real(fileid,variddglac,start2,count2,data)
      data(:)=aicec(:)/real(naccuout)			!sea ice cover
      ierr=nf_put_vara_real(fileid,varidaicec,start2,count2,data)
      data(:)=aiced(:)/real(naccuout)			!sea ice thickness
      ierr=nf_put_vara_real(fileid,varidaiced,start2,count2,data)
      data(:)=asnow(:)/real(naccuout)			!snow depth
      ierr=nf_put_vara_real(fileid,varidasnow,start2,count2,data)
      data(:)=ap(:)/real(naccuout)			!surface pressure
      ierr=nf_put_vara_real(fileid,varidap,start2,count2,data)
      data(:)=doro(:)/ga				!orography (convert from m2/s2 to m)
      ierr=nf_put_vara_real(fileid,variddoro,start2,count2,data)
      data(:)=dls(:)					!land-sea mask
      ierr=nf_put_vara_real(fileid,variddls,start2,count2,data)
      data(:)=darea(:)					!grid cell area
      ierr=nf_put_vara_real(fileid,variddarea,start2,count2,data)
      data(:)=ddummy(:)					!debug
      ierr=nf_put_vara_real(fileid,varidddummy,start2,count2,data)
      data(:)=adummy(:)/real(naccuout)					!debug
      ierr=nf_put_vara_real(fileid,varidadummy,start2,count2,data)
!seasonal gridpoint data
      if(noutseas.eq.1) then
       data(:)=4.0*apr_s(:,1)/real(naccuout)				!DJF avg pptn
       ierr=nf_put_vara_real(fileid,varidapr_s1,start2,count2,data)
       data(:)=apr2_s(:,1)-(4.0*apr_s(:,1)*apr_s(:,1)/real(naccuout))
       data=max(data,0.0) !stop blow up with high frequency output
       data(:)=sqrt(4.0*data(:)/real(naccuout))				!and SD
       ierr=nf_put_vara_real(fileid,varidapr2_s1,start2,count2,data)
       data(:)=4.0*apr_s(:,2)/real(naccuout)				!MAM avg pptn
       ierr=nf_put_vara_real(fileid,varidapr_s2,start2,count2,data)
       data(:)=apr2_s(:,2)-(4.0*apr_s(:,2)*apr_s(:,2)/real(naccuout))
       data=max(data,0.0) !stop blow up with high frequency output
       data(:)=sqrt(4.0*data(:)/real(naccuout))				!and SD
       ierr=nf_put_vara_real(fileid,varidapr2_s2,start2,count2,data)
       data(:)=4.0*apr_s(:,3)/real(naccuout)				!JJA avg pptn
       ierr=nf_put_vara_real(fileid,varidapr_s3,start2,count2,data)
       data(:)=apr2_s(:,3)-(4.0*apr_s(:,3)*apr_s(:,3)/real(naccuout))
       data=max(data,0.0) !stop blow up with high frequency output
       data(:)=sqrt(4.0*data(:)/real(naccuout))				!and SD
       ierr=nf_put_vara_real(fileid,varidapr2_s3,start2,count2,data)
       data(:)=4.0*apr_s(:,4)/real(naccuout)				!SON avg pptn
       ierr=nf_put_vara_real(fileid,varidapr_s4,start2,count2,data)
       data(:)=apr2_s(:,4)-(4.0*apr_s(:,4)*apr_s(:,4)/real(naccuout))
       data=max(data,0.0) !stop blow up with high frequency output
       data(:)=sqrt(4.0*data(:)/real(naccuout))				!and SD
       ierr=nf_put_vara_real(fileid,varidapr2_s4,start2,count2,data)
       data(:)=4.0*atsa_s(:,1)/real(naccuout)				!DJF avg air temp
       ierr=nf_put_vara_real(fileid,varidatsa_s1,start2,count2,data)
       data(:)=atsa2_s(:,1)-(4.0*atsa_s(:,1)*atsa_s(:,1)/real(naccuout))
       data=max(data,0.0) !stop blow up with high frequency output
       data(:)=sqrt(4.0*data(:)/real(naccuout))				!and SD
       ierr=nf_put_vara_real(fileid,varidatsa2_s1,start2,count2,data)
       data(:)=4.0*atsa_s(:,2)/real(naccuout)				!MAM avg air temp
       ierr=nf_put_vara_real(fileid,varidatsa_s2,start2,count2,data)
       data(:)=atsa2_s(:,2)-(4.0*atsa_s(:,2)*atsa_s(:,2)/real(naccuout))
       data=max(data,0.0) !stop blow up with high frequency output
       data(:)=sqrt(4.0*data(:)/real(naccuout))				!and SD
       ierr=nf_put_vara_real(fileid,varidatsa2_s2,start2,count2,data)
       data(:)=4.0*atsa_s(:,3)/real(naccuout)				!JJA avg air temp
       ierr=nf_put_vara_real(fileid,varidatsa_s3,start2,count2,data)
       data(:)=atsa2_s(:,3)-(4.0*atsa_s(:,3)*atsa_s(:,3)/real(naccuout))
       data=max(data,0.0) !stop blow up with high frequency output
       data(:)=sqrt(4.0*data(:)/real(naccuout))				!and SD
       ierr=nf_put_vara_real(fileid,varidatsa2_s3,start2,count2,data)
       data(:)=4.0*atsa_s(:,4)/real(naccuout)				!SON avg air temp
       ierr=nf_put_vara_real(fileid,varidatsa_s4,start2,count2,data)
       data(:)=atsa2_s(:,4)-(4.0*atsa_s(:,4)*atsa_s(:,4)/real(naccuout))
       data=max(data,0.0) !stop blow up with high frequency output
       data(:)=sqrt(4.0*data(:)/real(naccuout))				!and SD
       ierr=nf_put_vara_real(fileid,varidatsa2_s4,start2,count2,data)
       data(:)=4.0*acc_s(:,1)/real(naccuout)				!DJF clouds
       ierr=nf_put_vara_real(fileid,varidacc_s1,start2,count2,data)
       data(:)=4.0*acc_s(:,2)/real(naccuout)				!MAM clouds
       ierr=nf_put_vara_real(fileid,varidacc_s2,start2,count2,data)
       data(:)=4.0*acc_s(:,3)/real(naccuout)				!JJA clouds
       ierr=nf_put_vara_real(fileid,varidacc_s3,start2,count2,data)
       data(:)=4.0*acc_s(:,4)/real(naccuout)				!SON clouds
       ierr=nf_put_vara_real(fileid,varidacc_s4,start2,count2,data)
       data(:)=4.0*aevap_s(:,1)/real(naccuout)				!DJF evaporation
       ierr=nf_put_vara_real(fileid,varidaevap_s1,start2,count2,data)
       data(:)=4.0*aevap_s(:,2)/real(naccuout)				!MAM evaporation
       ierr=nf_put_vara_real(fileid,varidaevap_s2,start2,count2,data)
       data(:)=4.0*aevap_s(:,3)/real(naccuout)				!JJA evaporation
       ierr=nf_put_vara_real(fileid,varidaevap_s3,start2,count2,data)
       data(:)=4.0*aevap_s(:,4)/real(naccuout)				!SON evaporation
       ierr=nf_put_vara_real(fileid,varidaevap_s4,start2,count2,data)
       data(:)=4.0*asst_s(:,1)/real(naccuout)				!DJF SST
       ierr=nf_put_vara_real(fileid,varidasst_s1,start2,count2,data)
       data(:)=4.0*asst_s(:,2)/real(naccuout)				!MAM SST
       ierr=nf_put_vara_real(fileid,varidasst_s2,start2,count2,data)
       data(:)=4.0*asst_s(:,3)/real(naccuout)				!JJA SST
       ierr=nf_put_vara_real(fileid,varidasst_s3,start2,count2,data)
       data(:)=4.0*asst_s(:,4)/real(naccuout)				!SON SST
       ierr=nf_put_vara_real(fileid,varidasst_s4,start2,count2,data)
       data(:)=4.0*asia_s(:,1)/real(naccuout)				!DJF SIA
       ierr=nf_put_vara_real(fileid,varidasia_s1,start2,count2,data)
       data(:)=4.0*asia_s(:,2)/real(naccuout)				!MAM SIA
       ierr=nf_put_vara_real(fileid,varidasia_s2,start2,count2,data)
       data(:)=4.0*asia_s(:,3)/real(naccuout)				!JJA SIA
       ierr=nf_put_vara_real(fileid,varidasia_s3,start2,count2,data)
       data(:)=4.0*asia_s(:,4)/real(naccuout)				!SON SIA
       ierr=nf_put_vara_real(fileid,varidasia_s4,start2,count2,data)

       data(:)=4.0*assol_s(:,1)/real(naccuout)				!DJF incoming solar
       ierr=nf_put_vara_real(fileid,varidassol_s1,start2,count2,data)
       data(:)=4.0*assol_s(:,2)/real(naccuout)				!MAM incoming solar
       ierr=nf_put_vara_real(fileid,varidassol_s2,start2,count2,data)
       data(:)=4.0*assol_s(:,3)/real(naccuout)				!JJA incoming solar
       ierr=nf_put_vara_real(fileid,varidassol_s3,start2,count2,data)
       data(:)=4.0*assol_s(:,4)/real(naccuout)				!SON incoming solar
       ierr=nf_put_vara_real(fileid,varidassol_s4,start2,count2,data)

       data(:)=4.0*asthr_s(:,1)/real(naccuout)				!DJF incoming thermal
       ierr=nf_put_vara_real(fileid,varidasthr_s1,start2,count2,data)
       data(:)=4.0*asthr_s(:,2)/real(naccuout)				!MAM incoming thermal
       ierr=nf_put_vara_real(fileid,varidasthr_s2,start2,count2,data)
       data(:)=4.0*asthr_s(:,3)/real(naccuout)				!JJA incoming thermal
       ierr=nf_put_vara_real(fileid,varidasthr_s3,start2,count2,data)
       data(:)=4.0*asthr_s(:,4)/real(naccuout)				!SON incoming thermal
       ierr=nf_put_vara_real(fileid,varidasthr_s4,start2,count2,data)

       data(:)=4.0*ataux_s(:,1)/real(naccuout)
       ierr=nf_put_vara_real(fileid,varidataux_s1,start2,count2,data)
       data(:)=4.0*ataux_s(:,2)/real(naccuout)
       ierr=nf_put_vara_real(fileid,varidataux_s2,start2,count2,data)
       data(:)=4.0*ataux_s(:,3)/real(naccuout)
       ierr=nf_put_vara_real(fileid,varidataux_s3,start2,count2,data)
       data(:)=4.0*ataux_s(:,4)/real(naccuout)
       ierr=nf_put_vara_real(fileid,varidataux_s4,start2,count2,data)

       data(:)=4.0*atauy_s(:,1)/real(naccuout)
       ierr=nf_put_vara_real(fileid,varidatauy_s1,start2,count2,data)
       data(:)=4.0*atauy_s(:,2)/real(naccuout)
       ierr=nf_put_vara_real(fileid,varidatauy_s2,start2,count2,data)
       data(:)=4.0*atauy_s(:,3)/real(naccuout)
       ierr=nf_put_vara_real(fileid,varidatauy_s3,start2,count2,data)
       data(:)=4.0*atauy_s(:,4)/real(naccuout)
       ierr=nf_put_vara_real(fileid,varidatauy_s4,start2,count2,data)
      endif
!monthly fields
      start3m(1)=1
      start3m(2)=1
      start3m(3)=1
      count3m(1)=NLON
      count3m(2)=NLAT
      count3m(3)=12
      dataM(:,:)=12.0*ataux_m(:,:)/real(naccuout)
      ierr=nf_put_vara_real(fileid,varidataux_M,start3m,count3m,dataM)
      dataM(:,:)=12.0*atauy_m(:,:)/real(naccuout)
      ierr=nf_put_vara_real(fileid,varidatauy_M,start3m,count3m,dataM)

!3D fields
      start3(1)=1
      start3(2)=1
      start3(3)=1
      count3(1)=NLON
      count3(2)=NLAT
      count3(3)=NLEV
      data3(:,:)=4.0*a3u_s(:,:,1)/real(naccuout)				!DJF 3D uspeed
      ierr=nf_put_vara_real(fileid,varida3u_s1,start3,count3,data3)
      data3(:,:)=4.0*a3u_s(:,:,2)/real(naccuout)				!MAM 3D uspeed
      ierr=nf_put_vara_real(fileid,varida3u_s2,start3,count3,data3)
      data3(:,:)=4.0*a3u_s(:,:,3)/real(naccuout)				!JJA 3D uspeed
      ierr=nf_put_vara_real(fileid,varida3u_s3,start3,count3,data3)
      data3(:,:)=4.0*a3u_s(:,:,4)/real(naccuout)				!SON 3D uspeed
      ierr=nf_put_vara_real(fileid,varida3u_s4,start3,count3,data3)
      data3(:,:)=4.0*a3v_s(:,:,1)/real(naccuout)				!DJF 3D vspeed
      ierr=nf_put_vara_real(fileid,varida3v_s1,start3,count3,data3)
      data3(:,:)=4.0*a3v_s(:,:,2)/real(naccuout)				!MAM 3D vspeed
      ierr=nf_put_vara_real(fileid,varida3v_s2,start3,count3,data3)
      data3(:,:)=4.0*a3v_s(:,:,3)/real(naccuout)				!JJA 3D vspeed
      ierr=nf_put_vara_real(fileid,varida3v_s3,start3,count3,data3)
      data3(:,:)=4.0*a3v_s(:,:,4)/real(naccuout)				!SON 3D vspeed
      ierr=nf_put_vara_real(fileid,varida3v_s4,start3,count3,data3)

!PBH  close netcdf
      ierr=nf_close(fileid)

!     *********************
!     * specific humidity *
!     *********************

      if (nqspec == 0) then ! Semi Langrangian advection active
         do jlev = 1 , NLEV
            call writegp(40,dq(1,jlev),133,jlev)
         enddo
      endif

!     **********************************
!     * mixed-layer depth (from ocean) *
!     **********************************

      call writegp(40,dmld,110,0)

!     ***********************
!     * surface temperature *
!     ***********************

      call writegp(40,dt(1,NLEP),139,0)

!     ****************
!     * soil wetness *
!     ****************

      call writegp(40,dwatc,140,0)

!     **************
!     * snow depth *
!     **************

      call writegp(40,dsnow,141,0)

!     **********************
!     * large scale precip *
!     **********************

      aprl(:)=aprl(:)/real(naccuout)
      call writegp(40,aprl,142,0)

!     *********************
!     * convective precip *
!     *********************

      aprc(:)=aprc(:)/real(naccuout)
      call writegp(40,aprc,143,0)

!     *************
!     * snow fall *
!     *************

      aprs(:)=aprs(:)/real(naccuout)
      call writegp(40,aprs,144,0)

!     **********************
!     * sensible heat flux *
!     **********************

      ashfl(:)=ashfl(:)/real(naccuout)
      call writegp(40,ashfl,146,0)

!     ********************
!     * latent heat flux *
!     ********************

      alhfl(:)=alhfl(:)/real(naccuout)
      call writegp(40,alhfl,147,0)

!     ************************
!     * liquid water content *
!     ************************

      do jlev = 1 , NLEV
         call writegp(40,dql(:,jlev),161,jlev)
      enddo

!     *************
!     * u-star**3 *
!     *************

      call writegp(40,dust3,159,0)

!     **********
!     * runoff *
!     **********

      aroff(:)=aroff(:)/real(naccuout)
      call writegp(40,aroff,160,0)

!     ***************
!     * cloud cover *
!     ***************

      do jlev = 1 , NLEV
        call writegp(40,dcc(1,jlev),162,jlev)
      enddo
      acc(:)=acc(:)/real(naccuout)
      call writegp(40,acc,164,0)

!     ***************************
!     * surface air temperature *
!     ***************************

      atsa(:)=atsa(:)/real(naccuout)
      call writegp(40,atsa,167,0)

!     ******************************
!     * surface temperature (accu) *
!     ******************************

      ats0(:)=ats0(:)/real(naccuout)
      call writegp(40,ats0,169,0)

!     *************************
!     * deep soil temperature *
!     *************************

      call writegp(40,dtd5,170,0)

!     *****************
!     * land sea mask *
!     *****************

      call writegp(40,dls,172,0)

!     *********************
!     * surface roughness *
!     *********************

      call writegp(40,dz0,173,0)

!     **********
!     * albedo *
!     **********

      call writegp(40,dalb,175,0)

!     ***************************
!     * surface solar radiation *
!     ***************************

      assol(:)=assol(:)/real(naccuout)
      call writegp(40,assol,176,0)

!     *****************************
!     * surface thermal radiation *
!     *****************************

      asthr(:)=asthr(:)/real(naccuout)
      call writegp(40,asthr,177,0)

!     ***********************
!     * top solar radiation *
!     ***********************

      atsol(:)=atsol(:)/real(naccuout)
      call writegp(40,atsol,178,0)

!     *************************
!     * top thermal radiation *
!     *************************

      atthr(:)=atthr(:)/real(naccuout)
      call writegp(40,atthr,179,0)

!     ************
!     * u-stress *
!     ************

      ataux(:)=ataux(:)/real(naccuout)
      call writegp(40,ataux,180,0)

!     *************
!     * v- stress *
!     *************

      atauy(:)=atauy(:)/real(naccuout)
      call writegp(40,atauy,181,0)

!     ***************
!     * evaporation *
!     ***************

      aevap(:)=aevap(:)/real(naccuout)
      call writegp(40,aevap,182,0)

!     *********************
!     * soil temperature *
!     *********************

      call writegp(40,dtsoil,183,0)

!     ********************
!     * vegetation cover *
!     ********************

      call writegp(40,dveg,199,0)

!     *******************
!     * leaf area index *
!     *******************

      call writegp(40,dlai,200,0)

!     ********************
!     * top solar upward *
!     ********************

      atsolu(:)=atsolu(:)/real(naccuout)
      call writegp(40,atsolu,203,0)

!     ************************
!     * surface solar upward *
!     ************************

      assolu(:)=assolu(:)/real(naccuout)
      call writegp(40,assolu,204,0)

!     **************************
!     * surface thermal upward *
!     **************************

      asthru(:)=asthru(:)/real(naccuout)
      call writegp(40,asthru,205,0)

!     *******************************
!     * soil temperatures level 2-4 *
!     *******************************

      call writegp(40,dtd2,207,0)
      call writegp(40,dtd3,208,0)
      call writegp(40,dtd4,209,0)

!     *****************
!     * sea ice cover *
!     *****************

      call writegp(40,dicec,210,0)

!     *********************
!     * sea ice thickness *
!     *********************

      call writegp(40,diced,211,0)

!     ****************
!     * forest cover *
!     ****************

      call writegp(40,dforest,212,0)

!     *************
!     * snow melt *
!     *************

      asmelt(:)=asmelt(:)/real(naccuout)
      call writegp(40,asmelt,218,0)

!     *********************
!     * snow depth change *
!     *********************

      asndch(:)=asndch(:)/real(naccuout)
      call writegp(40,asndch,221,0)

!     ******************
!     * field capacity *
!     ******************

      call writegp(40,dwmax,229,0)

!     *****************************************
!     * vertical integrated specific humidity *
!     *****************************************

      aqvi(:)=aqvi(:)/real(naccuout)
      call writegp(40,aqvi,230,0)

!     ****************
!     * glacier mask *
!     ****************

      call writegp(40,dglac,232,0)

!     *********************
!     ***   S I M B A   ***
!     *********************

!     ****************************
!     * gross primary production *
!     ****************************

      agpp(:)=agpp(:)/real(naccuout)
      call writegp(40,agpp,300,0)

!     **************************
!     * net primary production *
!     **************************

      anpp(:)=anpp(:)/real(naccuout)
      call writegp(40,anpp,301,0)

!     *********************
!     * light limited GPP *
!     *********************

      agppl(:)=agppl(:)/real(naccuout)
      call writegp(40,agppl,302,0)

!     *********************
!     * water limited GPP *
!     *********************

      agppw(:)=agppw(:)/real(naccuout)
      call writegp(40,agppw,303,0)

!     *********************
!     * vegetation carbon *
!     *********************

      call writegp(40,dcveg,304,0)

!     ***************
!     * soil carbon *
!     ***************

      call writegp(40,dcsoil,305,0)

!     ************************
!     * no growth allocation *
!     ************************

      anogrow(:)=anogrow(:)/real(naccuout)
      call writegp(40,anogrow,306,0)

!     *****************************
!     * heterotrophic respiration *
!     *****************************

      aresh(:)=aresh(:)/real(naccuout)
      call writegp(40,aresh,307,0)

!     *********************
!     * litter production *
!     *********************

      alitter(:)=alitter(:)/real(naccuout)
      call writegp(40,alitter,308,0)

!     **************
!     * water loss *
!     **************

      awloss(:)=awloss(:)/real(naccuout)
      call writegp(40,awloss,309,0)
!
      return
      end

!     ==================
!     SUBROUTINE OUTDIAG
!     ==================

      subroutine outdiag
      use pumamod
      implicit none
      integer jdiag,jcode,jlev

!     *****************************************
!     * 2-D diagnostic arrays, if switched on *
!     *****************************************

      if(ndiagsp2d > 0 .and. mypid == NROOT) then
       do jdiag=1,ndiagsp2d
        jcode=50+jdiag
        call writesp(40,dsp2d(1,jdiag),jcode,0,1.,0.0)
       enddo
      end if

!     *****************************************
!     * 3-D diagnostic arrays, if switched on *
!     *****************************************

      if(ndiagsp3d > 0 .and. mypid == NROOT) then
       do jdiag=1,ndiagsp3d
        jcode=60+jdiag
        do jlev=1,NLEV
         call writesp(40,dsp3d(1,jlev,jdiag),jcode,jlev,1.,0.0)
        enddo
       enddo
      end if

!     *****************************************
!     * 2-D diagnostic arrays, if switched on *
!     *****************************************

      if(ndiaggp2d > 0) then
       do jdiag=1,ndiaggp2d
        jcode=jdiag
        call writegp(40,dgp2d(1,jdiag),jcode,0)
       enddo
      end if

!     *****************************************
!     * 3-D diagnostic arrays, if switched on *
!     *****************************************

      if(ndiaggp3d > 0) then
       do jdiag=1,ndiaggp3d
        jcode=20+jdiag
        do jlev=1,NLEV
         call writegp(40,dgp3d(1,jlev,jdiag),jcode,jlev)
        enddo
       enddo
      end if

!     ************************************************
!     * cloud forcing (clear sky fluxes) diagnostics *
!     ************************************************

      if(ndiagcf > 0) then
       call writegp(40,dclforc(1,1),101,0)
       call writegp(40,dclforc(1,2),102,0)
       call writegp(40,dclforc(1,3),103,0)
       call writegp(40,dclforc(1,4),104,0)
       call writegp(40,dclforc(1,5),105,0)
       call writegp(40,dclforc(1,6),106,0)
       call writegp(40,dclforc(1,7),107,0)
      end if

!     **************************************
!     * entropy diagnostics if switched on *
!     **************************************

      if(nentropy > 0) then
       do jdiag=1,33
        jcode=319+jdiag
        call writegp(40,dentropy(1,jdiag),jcode,0)
       enddo
      end if

!     *************************************
!     * energy diagnostics if switched on *
!     *************************************

      if(nenergy > 0) then
       do jdiag=1,28
        jcode=359+jdiag
        call writegp(40,denergy(1,jdiag),jcode,0)
       enddo
      end if
!
      return
      end

!     ===================
!     SUBROUTINE OUTRESET
!     ===================

      subroutine outreset
      use pumamod
      implicit none
!
!     reset accumulated arrays and counter
!
      aprl(:)=0.
      aprc(:)=0.
      aprs(:)=0.
      aevap(:)=0.
      ashfl(:)=0.
      alhfl(:)=0.
      acc(:)=0.
      assol(:)=0.
      asthr(:)=0.
      atsol(:)=0.
      atthr(:)=0.
      assolu(:)=0.
      asthru(:)=0.
      atsolu(:)=0.
      ataux(:)=0.
      atauy(:)=0.
      aroff(:)=0.
      asmelt(:)=0.
      asndch(:)=0.
      aqvi(:)=0.
      atsa(:)=0.
      ats0(:)=0.

      anpp(:)=0.
      alitter(:)=0.
      awloss(:)=0.
      anogrow(:)=0.
      agpp(:)=0.
      agppl(:)=0.
      agppw(:)=0.
      aresh(:)=0.

! PBH
      atsoil(:)=0.
      ahfls(:)=0.
      awatc(:)=0.
      aveg(:)=0.
      acveg(:)=0.
      acsoil(:)=0.
      aforest(:)=0.
      awmax(:)=0.
      alai(:)=0.
      az0(:)=0.
      aalb(:)=0.
      aicec(:)=0.
      aiced(:)=0.
      asnow(:)=0.
      ap(:)=0.
! seasonal
      apr_s(:,:)=0.
      apr2_s(:,:)=0.
      atsa_s(:,:)=0.
      atsa2_s(:,:)=0.
      acc_s(:,:)=0.
      aevap_s(:,:)=0.
      asst_s(:,:)=0.
      asia_s(:,:)=0.
      assol_s(:,:)=0.
      asthr_s(:,:)=0.
      ataux_s(:,:)=0.
      ataux_s(:,:)=0.
!monthly
      ataux_m(:,:)=0.
      atauy_m(:,:)=0.
! 3D seasonal
      a3u_s(:,:,:)=0.
      a3v_s(:,:,:)=0.
! debug
      adummy(:)=0.0

      naccuout=0

!     ************************************************
!     * cloud forcing (clear sky fluxes) diagnostics *
!     ************************************************

      if(ndiagcf > 0) then
       dclforc(:,:)=0.
      end if
!
      return
      end

!     ==================
!     SUBROUTINE OUTACCU
!     ==================

      subroutine outaccu
      use pumamod
      use landmod
      implicit none
      real zhelp(NHOR)
!PBH
      integer season,month

!
!     accumulate diagnostic arrays
!

!PBH calculate season
      season = nstep / ntspd				! no days since simulation start
      season = mod(season,n_days_per_year)		! day of year
      month = (season / n_days_per_month) + 1		! month
      season = (month / 3) + 1				! season
      if(season.eq.5) season = 1			! 1=DJF, 2=MAM, 3=JJA, 4=SON

      aprl(:)=aprl(:)+dprl(:)
      aprc(:)=aprc(:)+dprc(:)
      aprs(:)=aprs(:)+dprs(:)
      aevap(:)=aevap(:)+devap(:)
      ashfl(:)=ashfl(:)+dshfl(:)
      alhfl(:)=alhfl(:)+dlhfl(:)
      acc(:)=acc(:)+dcc(:,NLEP)
      assol(:)=assol(:)+dswfl(:,NLEP)
      asthr(:)=asthr(:)+dlwfl(:,NLEP)
      atsol(:)=atsol(:)+dswfl(:,1)
      atthr(:)=atthr(:)+dlwfl(:,1)
      assolu(:)=assolu(:)+dfu(:,NLEP)
      asthru(:)=asthru(:)+dftu(:,NLEP)
      atsolu(:)=atsolu(:)+dfu(:,1)
      ataux(:)=ataux(:)+dtaux(:)
      atauy(:)=atauy(:)+dtauy(:)
      aroff(:)=aroff(:)+drunoff(:)
      asmelt(:)=asmelt(:)+dsmelt(:)
      asndch(:)=asndch(:)+dsndch(:)
      aqvi(:)=aqvi(:)+dqvi(:)
      atsa(:)=atsa(:)+dtsa(:)
      ats0(:)=ats0(:)+dt(:,NLEP)

      anpp(:)=anpp(:)+dnpp(:)
      alitter(:)=alitter(:)+dlitter(:)
      awloss(:)=awloss(:)+dwloss(:)
      anogrow(:)=anogrow(:)+dnogrow(:)
      agpp(:)=agpp(:)+dgpp(:)
      agppl(:)=agppl(:)+dgppl(:)
      agppw(:)=agppw(:)+dgppw(:)
      aresh(:)=aresh(:)+dres(:)

!PBH
      atsoil(:)=atsoil(:)+dtsoil(:)
      ahfls(:)=ahfls(:)+dhfls(:)
      awatc(:)=awatc(:)+dwatc(:)
      aveg(:)=aveg(:)+dveg(:)
      acveg(:)=acveg(:)+dcveg(:)
      acsoil(:)=acsoil(:)+dcsoil(:)
      aforest(:)=aforest(:)+dforest(:)
      awmax(:)=awmax(:)+dwmax(:)
      alai(:)=alai(:)+dlai(:)
      az0(:)=az0(:)+dz0(:)
      aalb(:)=aalb(:)+dalb(:)
      aicec(:)=aicec(:)+dicec(:)
      aiced(:)=aiced(:)+diced(:)
      asnow(:)=asnow(:)+dsnow(:)
      ap(:)=ap(:)+dp(:)
! seasonal fields
      zhelp(:) = (dprl(:)+dprc(:)) * 8.64e7             !total pptn mm/day
      apr_s(:,season)=apr_s(:,season) + zhelp(:)
      apr2_s(:,season)=apr2_s(:,season) + zhelp(:)**2
      zhelp(:) = dtsa(:) - 273.15                       !SAT Celcius
      atsa_s(:,season)=atsa_s(:,season) + zhelp(:)
      atsa2_s(:,season)=atsa2_s(:,season) + zhelp(:)**2
      acc_s(:,season)=acc_s(:,season) + dcc(:,NLEP)       !cloud cover 
      aevap_s(:,season)=aevap_s(:,season) + devap(:)*8.64e7       !evap mm/day 
      asst_s(:,season)=asst_s(:,season) + genie_sst(:)       !genie sst 
      asia_s(:,season)=asia_s(:,season) + genie_frac_sic(:)       !genie sia 

      assol_s(:,season)=assol_s(:,season) + dswfl(:,NLEP) - dfu(:,NLEP)       !incoming solar
      asthr_s(:,season)=asthr_s(:,season) + dlwfl(:,NLEP) - dftu(:,NLEP)      !incoming solar

      ataux_s(:,season)=ataux_s(:,season) + dtaux(:)      !ustress
      atauy_s(:,season)=atauy_s(:,season) + dtauy(:)      !vstress

!2D monthly
      ataux_m(:,month)=ataux_m(:,month) + dtaux(:)      !ustress
      atauy_m(:,month)=atauy_m(:,month) + dtauy(:)      !vstress

!3D seasonal
      a3u_s(:,:,season)=a3u_s(:,:,season) + du(:,1:NLEV)      !uwindpseed
      a3v_s(:,:,season)=a3v_s(:,:,season) + dv(:,1:NLEV)      !vwindspeed

! debug
      adummy(:)=adummy(:)+ddummy(:)


      naccuout=naccuout+1

      return
      end
