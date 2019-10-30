
! ******************************************************************************************************************************** !
! ECOGEM LOOP SUBROUTINE
subroutine ecogem(          &
     & dum_dts,             &
     & dum_genie_clock,     &
     & dum_egbg_fxsw,       & ! input
     & dum_mld,             & ! input
     & dum_egbg_sfcocn,     & ! input  -- tracer concentrations
     & dum_egbg_sfcpart,    & ! output -- change in particulate concentration field
     & dum_egbg_sfcdiss     & ! output -- change in remin concentration field
     & )

  use gem_cmn
  use gem_carbchem
  use gem_util
  USE ecogem_lib
  use ecogem_data_netCDF
  USE ecogem_box

  IMPLICIT NONE
  ! ------------------------------------------------------------ !
  ! DUMMY ARGUMENTS
  ! ------------------------------------------------------------ !
  REAL   ,INTENT(IN)                                     :: dum_dts             ! biogem time-step length (seconds)
  integer(kind=8),intent(in)                             :: dum_genie_clock     ! genie clock (ms since start) NOTE: 8-byte integer
  real   ,intent(in) ,dimension(n_i,n_j)                 :: dum_egbg_fxsw       !
  real   ,intent(in) ,dimension(n_i,n_j)                 :: dum_mld             ! mixed layer depth
  real   ,intent(in) ,dimension(n_ocn ,n_i,n_j,n_k)      :: dum_egbg_sfcocn     ! ecology-interface ocean tracer composition; ocn grid
  real   ,intent(out),dimension(n_sed ,n_i,n_j,n_k)      :: dum_egbg_sfcpart    ! ocean -> ecology flux; ocn grid
  real   ,intent(out),dimension(n_ocn ,n_i,n_j,n_k)      :: dum_egbg_sfcdiss    ! ecology -> ocean flux; ocn grid

  ! ------------------------------------------------------------ !
  ! DEFINE LOCAL VARIABLES
  ! ------------------------------------------------------------ !
  INTEGER                                  ::i,j,k,l,ii,io,jp,ko,kbase,is,nsub,tstep ! counting indices
  INTEGER                                  ::jpred,jprey,i_tser ! counting indices

  REAL,DIMENSION(n_ocn,n_i,n_j,n_k)        ::loc_ocn

  REAL,DIMENSION(n_i,n_j)                  ::PAR,isocean
  REAL,DIMENSION(n_i,n_j,n_k)              ::omega,gamma
  REAL                                     ::templocal,PAR_layer,layerthick,layermid
  REAL                                     ::topD,PAR_in,PAR_out

  ! to convert per day rates into per second
  real,parameter :: pday = 86400.0

  !  REAL,DIMENSION(iomax+iChl,npmax,n_i,n_j,n_k) ::biomass_flux
  REAL,DIMENSION(iimax,n_i,n_j,n_k)          ::nutrient_flux
  REAL,DIMENSION(iomax,komax,n_i,n_j,n_k)    ::orgmat_flux
  REAL,DIMENSION(iimaxiso,n_i,n_j,n_k)       ::nutiso_flux !ckc isotopes
  REAL,DIMENSION(iomaxiso,komax,n_i,n_j,n_k) ::orgmatiso_flux !ckc isotopes

  REAL,DIMENSION(iomax,npmax)              ::quota,limit,qreg,qreg_h
  REAL,DIMENSION(iimax,npmax)              ::up_inorg
  REAL,DIMENSION(iimaxiso,npmax)           ::up_inorgiso !ckc isotopes
  REAL,DIMENSION(iomax+iChl,npmax,npmax)   ::GrazMat
  REAL,DIMENSION(npmax)                    ::VLlimit,chlsynth
  REAL                                     ::gamma_T,totPP
  REAL                                     ::mld,totchl,k_tot
  INTEGER                                  ::imld
  REAL,DIMENSION(npmax)                    ::Totzoolimit
  REAL,DIMENSION(npmax,npmax)                    ::zoolimit

  REAL,DIMENSION(iomax+iChl,npmax)         ::loc_biomass
  REAL,DIMENSION(iimax)                    ::loc_nuts
  REAL,DIMENSION(iomaxiso,npmax)           ::loc_bioiso !ckc isotopes
  REAL,DIMENSION(iimaxiso)                 ::loc_nutiso !ckc isotopes
  REAL,DIMENSION(iomax+iChl,npmax)              ::ratiobGraz, biobGraz !ckc for isotopes (jdw: added iChl)
  REAL,DIMENSION(iomax+iChl,komax)              ::ratioGraz, orgbGraz  !ckc for isotope grazing calculations  (jdw: added iChl)
  REAL,DIMENSION(n_i,n_j,n_k)              ::POC_Rfrac, CaCO3_Rfrac  !ckc local Corg d13C dependent on local water iso
  REAL                                     ::Corg_frac
  REAL                                     ::PDBstnd !ckc VPDB d13C standard
  REAL,DIMENSION(n_i,n_j,n_k)              ::frac_ratio !for iCarb multiplier
  REAL                                     ::loc_delta_CaCO3, loc_alpha, loc_R !CaCO3 13C calculation

  REAL,DIMENSION(iomax+iChl,npmax)         ::dbiomassdt
  REAL,DIMENSION(iimax)                    ::dnutrientdt
  REAL,DIMENSION(iomax,komax)              ::dorgmatdt
  REAL,DIMENSION(iomaxiso,npmax)           ::dbioisodt    !ckc isotopes
  REAL,DIMENSION(iimaxiso)                 ::dnutisodt    !ckc isotopes
  REAL,DIMENSION(iomaxiso,komax)           ::dorgmatisodt !ckc isotopes
  REAL,DIMENSION(iomax,npmax)              ::dorgmatdt_plankton ! POM export for each plankton    Fanny/Maria - Aug19
  !REAL,DIMENSION(npmax)                    ::diameter     !ckc for size dependent fractionation

  REAL,DIMENSION(npmax)                    ::mortality,respiration
  REAL,DIMENSION(npmax)                    ::beta_mort_1,beta_graz_1
  REAL,DIMENSION(iomax+iChl,npmax)         ::assimilated,unassimilated
  REAL,DIMENSION(npmax)                    ::BioC,PP
  REAL,DIMENSION(iomax+iChl,npmax)         ::GrazPredEat,GrazPreyEaten
  REAL,DIMENSION(npmax)                    ::BioCiso


  real		   		     ::loc_total_weights ! JDW total weights
  real                                     ::loc_weighted_mean_size ! JDW weighted geometric mean size


  REAL                                     ::loc_dts,loc_dtyr,loc_t,loc_yr ! local time and time step etc.
  REAL                                     ::loc_rdts,loc_rdtyr            ! time reciprocals


  ! ------------------------------------------------------- !
  ! INITIALIZE LOCAL VARIABLES
  ! ------------------------------------------------------- !
  ! local array for ocean tracers
  loc_ocn(:,:,:,:) = dum_egbg_sfcocn(:,:,:,:)

  ! JDW Overwrite surface temperature with input
  if(ctrl_force_T)then
	loc_ocn(io_T,:,:,n_k)  = T_input ! JDW: currently running with only 1 surface layer?
  end if

  ! zero output arrays
  dum_egbg_sfcpart = 0.0
  dum_egbg_sfcdiss = 0.0

  ! *** CALCULATE LOCAL CONSTANTS & VARIABLES ***
  ! sea surface temp (in degrees C)
  kbase        = n_k-n_keco+1
  isocean(:,:) = wet_mask_ij ! use SST~=abs.zero as ocean flag
  ! surface incident PAR
  PAR = PARfrac * dum_egbg_fxsw(:,:)
  ! fluxes passed back to biogem
  nutrient_flux(:,:,:,:) = 0.0
  orgmat_flux(:,:,:,:,:) = 0.0
  nutiso_flux(:,:,:,:) = 0.0 !ckc isotopes
  orgmatiso_flux(:,:,:,:,:) = 0.0 !ckc isotopes
  !
  phys_limit(:,:,:,:,:) = 0.0
  zoo_limit(:,:,:,:) = 0.0
  zoolimit(:,:)      = 0.0
  ! initialise subroutine returns to null values
  ! quota_status outputs
  quota(:,:)              = 0.0
  limit(:,:)              = 0.0
  VLlimit(:)              = 0.0
  qreg(:,:)               = 0.0
  qreg_h(:,:)             = 0.0
  ! nutrient_uptake outputs
  up_inorg(:,:)           = 0.0
  up_inorgiso(:,:)        = 0.0 !ckc isotopes
  ! t_limitation outputs
  gamma_T                 = 0.0
  ! photosynthesis outputs
  chlsynth(:)             = 0.0
  totPP                   = 0.0
  ! grazing outputs
  GrazMat(:,:,:)          = 0.0
  GrazPredEat(:,:)        = 0.0
  GrazPreyEaten(:,:)      = 0.0
  ! size-dependent partitioning between DOM and POM
  beta_mort_1(:)     = 1.0 - beta_mort(:)
  beta_graz_1(:)     = 1.0 - beta_graz(:)
  ! ---------------------------------------------------------- !
  ! CONVERT TRACER FORMATS -- IN
  ! ---------------------------------------------------------- !
  ! Dissolved nutrients from BIOGEM
  if (useDIC)  nutrient(iDIC ,:,:,:)= loc_ocn(io_DIC ,:,:,:) * 1.0e3 * conv_m3_kg ! mol kg^-1 to mmol m^-3
  if (useNO3)  nutrient(iNO3 ,:,:,:)= loc_ocn(io_NO3 ,:,:,:) * 1.0e3 * conv_m3_kg ! mol kg^-1 to mmol m^-3
  if (useNO2)  nutrient(iNO2 ,:,:,:)= loc_ocn(io_NO2 ,:,:,:) * 1.0e3 * conv_m3_kg ! mol kg^-1 to mmol m^-3
  if (useNH4)  nutrient(iNH4 ,:,:,:)= loc_ocn(io_NH4 ,:,:,:) * 1.0e3 * conv_m3_kg ! mol kg^-1 to mmol m^-3
  if (usePO4)  nutrient(iPO4 ,:,:,:)= loc_ocn(io_PO4 ,:,:,:) * 1.0e3 * conv_m3_kg ! mol kg^-1 to mmol m^-3
  if (useFe)   nutrient(iFe  ,:,:,:)= loc_ocn(io_TDFe,:,:,:) * 1.0e3 * conv_m3_kg ! mol kg^-1 to mmol m^-3
  if (useSiO2) nutrient(iSiO2,:,:,:)= loc_ocn(io_SiO2,:,:,:) * 1.0e3 * conv_m3_kg ! mol kg^-1 to mmol m^-3
  !ckc isotopes tracing
  if (useDIC_13C) nutiso(iDIC_13C,:,:,:)= loc_ocn(io_DIC_13C,:,:,:)  * 1.0e3 * conv_m3_kg ! mol kg^-1 to mmol m^-3
  ! plankton see negative nutrients as zero
  ! nutrient(:,:,:,:) = MERGE(nutrient(:,:,:,:),0.0,nutrient(:,:,:,:).ge.0.0) ! check nutrients non-negative
  ! ---------------------------------------------------------- !

  ! *** CALCULATE GEM TIME ***
  ! update model time
  ! NOTE: par_misc_t_runtime is counted DOWN in years
  !       => for ECOGEM, the 'end of the world' occurs when time reaches zero
  loc_t = par_misc_t_runtime - real(dum_genie_clock)/(1000.0*conv_yr_s)
  loc_dts   = dum_dts
  loc_rdts  = 1.0/loc_dts
  loc_dtyr  = loc_dts/conv_yr_s
  loc_rdtyr = 1.0/loc_dtyr
  ! calculate actual year (counting years Before Present or otherwise)
  IF (ctrl_misc_t_BP) THEN
     loc_yr = loc_t + par_misc_t_end
  ELSE
     loc_yr = par_misc_t_end - loc_t
  END IF
  ! ---------------------------------------------------------- !
  ! UPDATE CARBONATE CHEMSITRY
  ! ---------------------------------------------------------- !
  ! NOTE: currently, carbonate chemsitry is solved only once -- before the first ECOGEM time-step
  !       -> in order to track how carbonate chemsitry changes in response to the ecosystem,
  !          DIC (and d13C of DIC) (if not also ALK) need to be updated in the loc_ocn array
  !          (and then these routines moved into the nsub time-stepping loop)
  do i=1,n_i
     do j=1,n_j
        if (isocean(i,j).eq.1) then
           do k=n_k,kbase,-1
              CALL sub_calc_carbconst(         &
                   & layermid              , &
                   & loc_ocn(io_T   ,i,j,k), &
                   & loc_ocn(io_S   ,i,j,k), &
                   & eco_carbconst(:,i,j,k)  &
                   & )
              if (ocn_select(io_Ca) .AND. ocn_select(io_Mg)) then
                 call sub_adj_carbconst(        &
                      & loc_ocn(io_Ca  ,i,j,k), &
                      & loc_ocn(io_Mg  ,i,j,k), &
                      & eco_carbconst(:,i,j,k)  &
                      & )
              end if
              ! re-estimate Ca and borate concentrations from salinity (if not selected and therefore explicitly treated)
              IF (.NOT. ocn_select(io_Ca))  loc_ocn(io_Ca,i,j,n_k)  = fun_calc_Ca(loc_ocn(io_S,i,j,n_k))
              IF (.NOT. ocn_select(io_B))   loc_ocn(io_B,i,j,n_k)   = fun_calc_Btot(loc_ocn(io_S,i,j,n_k))
              IF (.NOT. ocn_select(io_SO4)) loc_ocn(io_SO4,i,j,n_k) = fun_calc_SO4tot(loc_ocn(io_S,i,j,n_k))
              IF (.NOT. ocn_select(io_F))   loc_ocn(io_F,i,j,n_k)   = fun_calc_Ftot(loc_ocn(io_S,i,j,n_k))
              call sub_calc_carb(           &
                   & loc_ocn(io_DIC ,i,j,k), &
                   & loc_ocn(io_ALK ,i,j,k), &
                   & loc_ocn(io_Ca  ,i,j,k), &
                   & loc_ocn(io_PO4 ,i,j,k), &
                   & loc_ocn(io_SiO2,i,j,k), &
                   & loc_ocn(io_B   ,i,j,k), &
                   & loc_ocn(io_SO4 ,i,j,k), &
                   & loc_ocn(io_F   ,i,j,k), &
                   & loc_ocn(io_H2S ,i,j,k), &
                   & loc_ocn(io_NH4 ,i,j,k), &
                   & eco_carbconst(:,i,j,k), &
                   & eco_carb(     :,i,j,k), &
                   & eco_carbalk(  :,i,j,k)  &
                   & )
              ! extract calcite saturation state
              omega(i,j,k) = eco_carb(ic_ohm_cal,i,j,k)
              !ckc added by kc, calc carbisor of ocean waters, as Biogem
              IF (ocn_select(io_DIC_13C)) then
                 call sub_calc_carb_r13C(              &
                      & loc_ocn(io_T       ,i,j,k),      &
                      & loc_ocn(io_DIC     ,i,j,k),      &
                      & loc_ocn(io_DIC_13C ,i,j,k),   &
                      & eco_carb(         :,i,j,k),     &
                      & eco_carbisor(     :,i,j,k)  &
                      & )
              end IF
           enddo ! end k
        endif ! end if ocean
     enddo ! end j
  enddo ! end i
  ! ---------------------------------------------------------- !
  ! CALL ECOPHYSIOLOGICAL PROCESS
  ! ---------------------------------------------------------- !
  do nsub=1,nsubtime
     !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
     do i=1,n_i
        do j=1,n_j
           if (isocean(i,j).eq.1) then
              ! incedent PAR
              PAR_in = PAR(i,j) ! incedent PAR
              topD   = 0.0
              mld    = dum_mld(i,j)
              imld   = n_k
              do k=n_k,kbase,-1 ! counting backwards, from surface to base of productive layer
                 ! (only doing ecosystem calculations in upper n_keco grid layers)
                 !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

                 layerthick = loc_grid_dz(k) * goldstein_dsc ! layer thickness
                 layermid   = topD + layerthick / 2.0
                 if (topD.lt.mld) imld=k                     ! if top of level is above MLD, include level in ML
                 topD = topD + layerthick                    ! get depth for top of next level
                 templocal = loc_ocn(io_T,i,j,k)
                 ! cap maximum temperature seen by ECOGEM
                 !temp_max=35
                 !if (templocal.gt.(temp_max+273.15)) print*,'\/'
                 !print*,templocal,temp_max,MERGE(templocal,(temp_max+273.15),templocal.lt.(temp_max+273.15))
                 templocal = MERGE(templocal,(temp_max+273.15),templocal.lt.(temp_max+273.15))

		IF(ctrl_limit_neg_biomass)THEN
                        !IF(ANY(plankton(:,:,i,j,k).lt.0.0)) print*,'\/',i,j
                 	loc_nuts(:)      = merge(  nutrient(:,i,j,k),0.0,  nutrient(:,i,j,k).gt.0.0) ! -ve nutrients to zero
                 	loc_biomass(:,:) = merge(plankton(:,:,i,j,k),1.0e-4,plankton(:,:,i,j,k).gt.0.0) ! -ve biomass to small
                 	BioC(:) = loc_biomass(iCarb,:)
                 else
                 	loc_nuts(:)      = merge(  nutrient(:,i,j,k),0.0,  nutrient(:,i,j,k).gt.0.0) ! -ve nutrients to zero
                 	loc_biomass(:,:) = merge(plankton(:,:,i,j,k),0.0,plankton(:,:,i,j,k).gt.0.0) ! -ve biomass to small
                 	BioC(:) = loc_biomass(iCarb,:)
                 endif

                 if (c13trace) then
                    !plankiso(iCarb13C,:,i,j,k) = plankton(iCarb,:,i,j,k) * 0.0109 !force for testing
                    loc_nutiso(:)    = merge(    nutiso(:,i,j,k),0.0,    nutiso(:,i,j,k).gt.0.0) !ckc -ve nutrients isotopes to zero
                    loc_bioiso(:,:)  = merge(plankiso(:,:,i,j,k),0.0,plankiso(:,:,i,j,k).gt.0.0) !ckc -ve biomass isotopes to small
                    BioCiso(:) = loc_bioiso(iCarb13C,:) !ckc carbon 13 in each plankton group
                    !BioCiso(:) = loc_biomass(iCarb,:) * 0.0109 !force for testing
                 end if

                 ! LIGHT ATTENUATION
                 if (n_keco.eq.1) then ! Calculate light as mean across (virtual) ML
                    ! [AR] restrict MLD to (top) layer thickness
                    if (ctrl_restrict_mld) then
                       if (topD > mld) mld = topD
                    end if
                    totchl    = sum(loc_biomass(iomax+iChl,:)) ! find sum of all chlorophyll for light attenuation
                    totchl    = totchl * layerthick / mld ! recalculate chl concentration as if spread evenly across ML
                    k_tot     = (k_w + k_chl*totchl) ! attenuation due to water and pigment
                    if (fundamental) k_tot = k_w ! no self-shading (i.e. no competition for light) in fundamental niche experiment
                    PAR_layer = PAR_in /mld /k_tot*(1-exp(-k_tot*mld)) ! average PAR in  layer
                    ! [AR] this ... doesn't 'do' anything? k_tot*(1-exp(-k_tot*mld)) is never 0.0 or less(?)
                    PAR_layer = MERGE(PAR_layer,0.0,k_tot*(1-exp(-k_tot*mld)).gt.0.0)
                 else ! Calculate mean light within each layer
                    totchl    = sum(loc_biomass(iomax+iChl,:)) ! find sum of all chlorophyll for light attenuation
                    k_tot     = (k_w + k_chl*totchl) ! attenuation due to water and pigment
                    PAR_layer = PAR_in /layerthick /k_tot*(1-exp(-k_tot*layerthick)) ! average PAR in  layer
                    PAR_layer = MERGE(PAR_layer,0.0,k_tot*(1-exp(-k_tot*layerthick)).gt.0.0)
                    PAR_out   = PAR_in * exp(-k_tot*layerthick) ! light leaving bottom of layer
                    PAR_in    = PAR_out
                 endif

                 ! ?
                 up_inorg(:,:) = 0.0 ! (iomax,npmax)
                 qreg(:,:) = 0.0 ! (iomax,npmax)
                 qreg_h(:,:) = 0.0 ! (iomax,npmax)
                 quota(:,:) = 0.0 ! (iomax,npmax)
                 VLlimit(:) = 0.0 ! (npmax)
                 gamma_T = 0.0 !
                 PP(:) = 0.0 ! (npmax)
                 chlsynth(:) = 0.0 ! (npmax)
                 totPP = 0.0 !

                 !ckc isotopes
                 up_inorgiso(:,:) = 0.0 ! (iomaxiso,npmax)

                 !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
                 ! call ecophysiological subroutines

                 call quota_status(loc_biomass(1:iomax,:),quota)

                 call quota_limitation(quota,limit,VLlimit,qreg,qreg_h)

                 call t_limitation(templocal,gamma_T)

                 call nutrient_uptake(qreg(:,:),loc_nuts(:),gamma_T,up_inorg(:,:))

                 call photosynthesis(PAR_layer,loc_biomass,limit,VLlimit,up_inorg,gamma_T,up_inorg(iDIC,:),chlsynth,totPP)

                 call grazing(loc_biomass,gamma_T,zoolimit(:,:),GrazMat(:,:,:))

                 !ckc isotopes uptake, from nutrient uptake, nutrient concentration and fractionation
                 if (c13trace) then
                    call nut_fractionation(up_inorg(:,:),loc_nuts,loc_nutiso,diameter,up_inorgiso(:,:)) !need isotope ratio fractionation
                 endif
                 !call exudation
                 !call sinking
                 !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
                 ! additional rate and efficiency calculations

                 ! calculate mortality rates
                 mortality(:)   = mort(:)   * (1.0 - exp(-1.0e10 * loc_biomass(iCarb,:))) ! reduce mortality at very low biomass
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                 ! Stoopid calcification related moratlity
                 !mortality(:)   = mortality(:) + mortality(:) * calcify(:) / omega(i,j,k) ! Coccolithophores and Forams only
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                 ! mortality(:) = mortality(:)  * gamma_T ! temp adjusted?

                 ! calculate respiration
                 respiration(:) = respir(:) !* (1.0 - exp(-1.0e10 * loc_biomass(iCarb,:))) ! reduce respiration at very low biomass

                 ! calculate assimilation efficiency based on quota status
                 Totzoolimit(:) = 0.0  !total food limitation - Maria May 2019 !!! Need to check if consistent!!!
                 do io=1,iomax+iChl
                    ! Integrate grazing interactions
                    GrazPredEat(io,:)   = 0.0 ! Total prey biomass killed (pre-assimilation), summed by predator and by prey
                    GrazPreyEaten(io,:) = 0.0
                    do jpred=1,npmax
                       do jprey=1,npmax
                          GrazPredEat(io,jpred) = GrazPredEat(io,jpred)   + BioC(jpred) * GrazMat(io,jpred,jprey)
                          GrazPreyEaten(io,jprey) = GrazPreyEaten(io,jprey) + BioC(jpred) * GrazMat(io,jpred,jprey)
                          if (io.eq.iCarb) then  !calculate total food limitation - Maria May 2019 !!! Need to check if consistent!!!
                             Totzoolimit(jpred) = zoolimit(jpred,jprey)
                          endif
                       enddo
                    enddo

                    if (io.eq.iCarb) then
                       assimilated(io,:) = ass_eff * VLlimit(:) ! assimilate less carbon if predator is nutrient limited
                    elseif (io.le.iomax) then
                       assimilated(io,:) = ass_eff * qreg(io,:) ! assimilate less nutrient (io) if predator nutrient quota (io) is full
                    elseif (io.eq.iChlo) then
                       assimilated(io,:) = 0.0                  ! don't assimilate chlorophyll
                    endif
                    unassimilated(:,:) = 1.0 - assimilated(:,:)
                 enddo
                 !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

                 ! ---------------------------------------------------------- !
                 ! CALCULATE VARIABLE TENDENCIES
                 ! ---------------------------------------------------------- !

                 !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
                 ! NUTRIENTS (Only direct interactions with biota - remineralisation of detritus done in BIOGEM)
                 dnutrientdt(:) = 0.0 ! initialise
                 dnutisodt(:) = 0.0 !ckc initialise isotopes
                 !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
                 ! uptake by phytoplankton
                 do ii=1,iimax
                    dnutrientdt(ii) = dnutrientdt(ii) - sum(BioC(:) * up_inorg(ii,:)) ! - uptake of inorganic nutrient
                 enddo ! ii
                 !ckc if tracing c13 through food web
                 if (c13trace) then
                    do ii=1,iimaxiso
                       dnutisodt(ii) = dnutisodt(ii) - sum(BioCiso(:) * up_inorgiso(ii,:)) !ckc uptake of inorganic isotopes of nutrients
                    enddo
                    dnutisodt(iDIC_13C) = dnutisodt(iDIC_13C) + sum(BioCiso(:) * respiration(:)) !ckc respired carbon 13 lost from biomass
                 endif
                 dnutrientdt(iDIC)     = dnutrientdt(iDIC) + sum(BioC(:) * respiration(:))
                 !! loss terms to inorganic tracers
                 !if (useDIC) dnutrientdt(iDIC) = dnutrientdt(iDIC)  + 0.2 * (sum(loc_biomass(iCarb,:) * mortality(:)) + sum(GrazPredEat(iCarb,:) * unassimilated(iCarb,:)))
                 !if (usePO4) dnutrientdt(iPO4) = dnutrientdt(iPO4)  + 0.2 * (sum(loc_biomass(iPhos,:) * mortality(:)) + sum(GrazPredEat(iPhos,:) * unassimilated(iPhos,:)))
                 !if (useFe ) dnutrientdt(iFe)  = dnutrientdt(iFe)   + 0.2 * (sum(loc_biomass(iIron,:) * mortality(:)) + sum(GrazPredEat(iIron,:) * unassimilated(iIron,:)))
                 ! no nutrient uptake in fundamental niche experiment
                 if (fundamental) dnutrientdt(:) = 0.0
                 if (c13trace) then
                    if (fundamental) dnutisodt(:) = 0.0 !ckc no nutrient isotope update either
                 end if
                 !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
                 ! BIOMASS
                 dbiomassdt(:,:) = 0.0 ! initialise
                 dbioisodt (:,:) = 0.0 !ckc initialise isotopes in biomass
                 !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
                 ratiobGraz(:,:) = 0.0       !ckc initialise grazing ratio
                 !ratioEaten(:,:) = 0.0     !ckc initialise grazing ratio
                 biobGraz(:,:) = 0.0
                 !biobEaten(:,:) = 0.0

                 ! INORGANIC RESOURCE UPTAKE
                 do ii=1,iimax
                    io=nut2quota(ii)
                    dbiomassdt(io,:) = dbiomassdt(io,:) + up_inorg(ii ,:) * BioC(:)
                 enddo

                 ! CHLOROPHYLL A SYNTHESIS
                 if (chlquota) dbiomassdt(iChlo,:) = chlsynth(:)
                 do io=1,iomax+iChl
                    ! MORTALITY
                    ! Apply mortality
                    dbiomassdt(io,:) = dbiomassdt(io,:) - mortality(:) * loc_biomass(io,:)
                    ! add tiny quadratic mortality term to prevent blow up in fundamental niche experiment
                    if (fundamental) dbiomassdt(io,:) = dbiomassdt(io,:) - mortality(:) * (1.0e-10*loc_biomass(io,:))**2
                    ! GRAZING INTERACTIONS
                    ! Collate predator Gains
                    biobGraz(io,:) = dbiomassdt(io,:) !ckc for isotope tracing
                    dbiomassdt(io,:) = dbiomassdt(io,:) + GrazPredEat(io,:) * assimilated(io,:)
                    !ratioEat(io,:) = biobEat(io,:)/dbiomassdt(io,:)
                    ! Collate prey Losses
                    !biobEaten(io,:) = dbiomassdt(io,:)
                    dbiomassdt(io,:) = dbiomassdt(io,:) - GrazPreyEaten(io,:)
                    !ratioEaten(io,:) = biobEaten(io,:)/dbiomassdt(io,:)
                    ratiobGraz(io,:) = dbiomassdt(io,:)/biobGraz(io,:) !end/start
                 enddo
                 ! Apply respiration
                 dbiomassdt(iCarb,:) = dbiomassdt(iCarb,:) - respiration(:) * loc_biomass(iCarb,:)

                 !**************************ckc ISOTOPES**************************************************
                 if (c13trace) then
                    ! INORGANIC RESOURCE UPTAKE (only for carbon 13)
                    dbioisodt(iCarb13C,:) = dbioisodt(iCarb13C,:) + up_inorgiso(iDIC_13C ,:) * BioCiso(:)
                    ! MORTALITY
                    ! Apply mortality
                    dbioisodt(iCarb13C,:) = dbioisodt(iCarb13C,:) - mortality(:) * loc_bioiso(iCarb13C,:)
                    ! add tiny quadratic mortality term to prevent blow up in fundamental niche experiment
                    if (fundamental) dbioisodt(iCarb13C,:) = dbioisodt(iCarb13C,:) - mortality(:) * (1.0e-10*loc_bioiso(iCarb13C,:))**2
                    ! GRAZING INTERACTIONS
                    ! prey and preditor losses as ratio of carbon flux
                    dbioisodt(iCarb13C,:) = dbioisodt(iCarb13C,:) * (ratiobGraz(iCarb,:))
                    ! Apply respiration
                    dbioisodt(iCarb13C,:) = dbioisodt(iCarb13C,:) - respiration(:) * loc_bioiso(iCarb13C,:)
                 end if
                 !******************************************************************************************************
                 !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
                 ! ORGANIC MATTER
                 dorgmatdt(:,:) = 0.0 ! initialise

                 dorgmatisodt(:,:) = 0.0
                 dorgmatdt_plankton(:,:) = 0.0           ! Fanny/Maria Aug19

                 !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
                 orgbGraz(:,:) = 0.0

                 do io=1,iomax
                    ! mortality
                    dorgmatdt(io,1) = dorgmatdt(io,1) + sum(loc_biomass(io,:) * mortality(:)        * beta_mort(:)  ) ! fraction to DOM
                    dorgmatdt(io,2) = dorgmatdt(io,2) + sum(loc_biomass(io,:) * mortality(:)        * beta_mort_1(:)) ! fraction to POM
                    orgbGraz(io,1) = dorgmatdt(io,1)
                    orgbGraz(io,2) = dorgmatdt(io,2)
                    dorgmatdt_plankton(io,:) = dorgmatdt_plankton(io,:) + loc_biomass(io,:) * mortality(:) * beta_mort_1(:) ! POM source from mortality per plankton group - Fanny/Maria Aug19

                    ! unassimilated grazing
                    dorgmatdt(io,1) = dorgmatdt(io,1) + sum(GrazPredEat(io,:) * unassimilated(io,:) * beta_graz(:)  ) ! fraction to DOM
                    dorgmatdt(io,2) = dorgmatdt(io,2) + sum(GrazPredEat(io,:) * unassimilated(io,:) * beta_graz_1(:)) ! fraction to POM
                    ratioGraz(io,1) = dorgmatdt(io,1)/orgbGraz(io,1)
                    ratioGraz(io,2) = dorgmatdt(io,2)/orgbGraz(io,2)
                    dorgmatdt_plankton(io,:) = dorgmatdt_plankton(io,:) + GrazPredEat(io,:) * unassimilated(io,:) * beta_graz_1(:) ! Total POM source per plankton group - Fanny/Maria Aug19
                    !end/start
                 enddo
                 ! no organic matter production in fundamental niche experiment
                 if (fundamental) dorgmatdt(:,:) = 0.0

                 ! ******* JDW size-dependent remineralisation *******
		 ! calculate weighted mean size for size-dependent remineralisation scheme
		 ! if(autotrophy) loop calculates weights for phytoplankton only. Comment out if(autotrophy) loop to calculate weights for all types!
                 if (sed_select(is_POC_size)) then

                 	loc_weighted_mean_size=0.0
                 	loc_total_weights=0.0

                 	do jp=1,npmax
				if(autotrophy(jp).gt.0.0)then

				! Biomass weighted
				loc_weighted_mean_size=loc_weighted_mean_size+loc_biomass(iCarb,jp)*logesd(jp) ! sum of weights * size
				loc_total_weights=loc_total_weights+loc_biomass(iCarb,jp) ! sum of weights

				! POC weighted
                 		!loc_weighted_mean_size=loc_weighted_mean_size+((loc_biomass(iCarb,jp) * mortality(jp) * beta_mort_1(jp))+(GrazPredEat(iCarb,jp) * unassimilated(iCarb,jp) * beta_graz_1(jp)))*logesd(jp) ! sum of weights * size
                 		!loc_total_weights=loc_total_weights+((loc_biomass(iCarb,jp) * mortality(jp) * beta_mort_1(jp))+(GrazPredEat(iCarb,jp) * unassimilated(iCarb,jp) * beta_graz_1(jp))) ! sum of weights
				endIF
			enddo

                 	dum_egbg_sfcpart(is_POC_size,i,j,k)=10**(loc_weighted_mean_size / loc_total_weights) ! to biogem
                 endif
                 ! ***************************************************

                 !**********************ckc ISOTOPES**********************************************************************

                 if (c13trace) then
                    dorgmatisodt(iCarb13C,1) = dorgmatisodt(iCarb13C,1) + sum(loc_bioiso(iCarb13C,:) * mortality(:) * beta_mort(:)  ) ! fraction to DOM
                    dorgmatisodt(iCarb13C,2) = dorgmatisodt(iCarb13C,2) + sum(loc_bioiso(iCarb13C,:) * mortality(:) * beta_mort_1(:)) ! fraction to POM
                    ! unassimilated grazing
                    dorgmatisodt(iCarb13C,1) = dorgmatisodt(iCarb13C,1)*ratioGraz(iCarb,1) ! fraction to DOM
                    dorgmatisodt(iCarb13C,2) = dorgmatisodt(iCarb13C,2)*ratioGraz(iCarb,2) ! fraction to POM
                    if (fundamental) dorgmatisodt(:,:) = 0.0
                 end if
                 !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
                 !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

                 ! collect fluxes for output
                 do io=1,iomax
                    uptake_flux(io,:,i,j,k) = up_inorg(io,:) * BioC(:) ! mmol/m^3/s
                    export_flux(io,:,i,j,k) = dorgmatdt_plankton(io,:) ! mmol/m^3/s   - Fanny/Maria Aug19
                 enddo

                 if (c13trace) then
                    do io=1,iomaxiso
                       up_flux_iso(io,:,i,j,k) = up_inorgiso(io,:) * BioCiso(:) ! mmol/m^3/s
                    enddo
                 end if
                 !ckc do the same for up_flux_iso and up_inorgiso, calculate dnutisodt and dbioisodt here

                 !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
                 !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

                 ! ---------------------------------------------------------- !
                 ! UPDATE ECOGEM STATE VARIABLES
                 ! ---------------------------------------------------------- !
                 nutrient(:,i,j,k)      = loc_nuts(:)            + dnutrientdt(:)  * dum_dts/real(nsubtime)
                 plankton(:,:,i,j,k)    = loc_biomass(:,:)       + dbiomassdt(:,:) * dum_dts/real(nsubtime)
                 if (c13trace) then
                    nutiso(:,i,j,k)        = loc_nutiso(:)          + dnutisodt(:)    * dum_dts/real(nsubtime)
                    plankiso(:,:,i,j,k)    = loc_bioiso(:,:)        + dbioisodt(:,:)  * dum_dts/real(nsubtime)
                 end if
                 ! ---------------------------------------------------------- !
                 ! PASS TENDENCIES BACK TO GLOBAL BIOGEM ARRAYS
                 ! ---------------------------------------------------------- !
                 nutrient_flux(:,i,j,k) = nutrient_flux(:,i,j,k) + dnutrientdt(:) * dum_dts/real(nsubtime)
                 orgmat_flux(:,:,i,j,k) = orgmat_flux(:,:,i,j,k) + dorgmatdt(:,:) * dum_dts/real(nsubtime)
                 !ckc pass isotopes back to Biogem
                 if (c13trace) then
                    nutiso_flux(:,i,j,k)      = nutiso_flux(:,i,j,k)      + dnutisodt(:)      * dum_dts/real(nsubtime) !ckc
                    orgmatiso_flux(:,:,i,j,k) = orgmatiso_flux(:,:,i,j,k) + dorgmatisodt(:,:) * dum_dts/real(nsubtime) !ckc
                 end if
                 ! save physiological limitation factors in global variable for output
                 phys_limit(iCarb,:,i,j,k)   = 0.0
                 do io=2,iomax
                    phys_limit(io,:,i,j,k)    = limit(io,:)
                 enddo
                 phys_limit(iomax+1,:,i,j,k) = 0.0
                 phys_limit(iomax+1,1,i,j,k) = gamma_T
                 phys_limit(iomax+2,:,i,j,k) = 0.0
                 zoo_limit(:,i,j,k) = Totzoolimit(:)
                 !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
                 !ckc ISOTOPES POC as Biogem
                 if (.NOT.c13trace) then
                    !ckc c13 only for DOC and POC, not during uptake (as Biogem)
                    dorgmatisodt(iCarb13C,:)  = 0.0
                    dnutisodt(iCarb13C) = 0.0
                    ! POC_Rfrac is big R
                    POC_Rfrac(i,j,k) = fun_Corg_Rfrac(loc_ocn(io_T,i,j,k),eco_carb(ic_conc_CO2,i,j,k),&
                         & eco_carbisor(ici_CO2_r13C,i,j,k),par_d13C_DIC_Corg_ef,.false.)
                    ! apply to carbon fluxes
                    dorgmatisodt(iCarb13C,:)  = dorgmatdt(iCarb,:) * POC_Rfrac(i,j,k)
                    dnutisodt(iCarb13C)       = - sum(dorgmatisodt(iCarb13C,:))
                    !dnutisodt(iCarb13C)       = - sum(dorgmatisodt(iCarb13C,:) - dorgmatisodt(iCarb13C,1))
                    nutiso_flux(:,i,j,k)      = nutiso_flux(:,i,j,k)      + dnutisodt(:)      * dum_dts/real(nsubtime) !ckc
                    orgmatiso_flux(:,:,i,j,k) = orgmatiso_flux(:,:,i,j,k) + dorgmatisodt(:,:) * dum_dts/real(nsubtime)
                 else
                    !
                 end if

                 !Isotopes for CaCO3
                 loc_delta_CaCO3 = 15.10 - 4232.0/loc_ocn(io_T,i,j,k)
                 loc_alpha = 1.0 + loc_delta_CaCO3/1000.0
                 loc_R = eco_carbisor(ici_HCO3_r13C,i,j,k)/(1.0 - eco_carbisor(ici_HCO3_r13C,i,j,k))
                 CaCO3_Rfrac(i,j,k) = loc_alpha*loc_R/(1.0 + loc_alpha*loc_R)

              enddo ! end k
              !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
              !! homogenise plankton across ML
              !          ! (let BOIOGEM deal with motions of nutrients and organic matter)
              !          do io=1,iomax+iChl
              !            do jp=1,npmax
              !              plankton(io,jp,i,j,imld:n_k) = &
              !                 & sum(plankton(io,jp,i,j,imld:n_k) *  loc_grid_dz(imld:n_k)) &
              !                 & / sum(loc_grid_dz(imld:n_k))
              !! WHAT TO DO WITH PLANKTON THAT ARE NOW BELOW KBASE????
              !             ! Just let them decay into POM???
              !              plankton(io,jp,i,j,1:kbase-1) = plankton(io,jp,i,j,1:kbase-1) &
              !     &      - plankton(io,jp,i,j,1:kbase-1) * 0.01/86400. * dum_dts/real(nsubtime)
              !              if (io.le.iomax) then ! chlorophyll is not conserved
              !                orgmat_flux(io,2,i,j,1:kbase-1) = orgmat_flux(io,2,i,j,1:kbase-1) &
              !     &        +   plankton(io,jp,i,j,1:kbase-1) * 0.01/86400. * dum_dts/real(nsubtime)
              !              endif
              !            enddo
              !          enddo
              !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
           endif ! end if ocean
        enddo ! end j
     enddo ! end i
     !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  enddo ! END SUB TIME STEP HERE

  ! record time-series site data
  if (n_tser.gt.0) then
     tstep=int((dum_genie_clock/dum_dts/1000.0)-48.*(ceiling((dum_genie_clock/dum_dts/1000.0)/48.)-1))
     time_tser(tstep)           = dum_genie_clock/dum_dts/1000.0
     do i_tser=1,n_tser
        nutrient_tser(:,i_tser,tstep) =      nutrient(:,tser_i(i_tser),tser_j(i_tser),n_k)
        plankton_tser(:,:,i_tser,tstep) =    plankton(:,:,tser_i(i_tser),tser_j(i_tser),n_k)
        uptake_tser(:,:,i_tser,tstep) = uptake_flux(:,:,tser_i(i_tser),tser_j(i_tser),n_k) * pday
        gamma_tser(:,:,i_tser,tstep) =  phys_limit(:,:,tser_i(i_tser),tser_j(i_tser),n_k)        ! mmol m^-3 d^-1
     enddo
  endif

  ! ---------------------------------------------------------- !
  ! CONVERT TRACER FORMATS -- OUT
  ! ---------------------------------------------------------- !
  ! Explicitly-resolved fluxes
  ! ---------------------------------------------------------- !
  !  Dissolved nutrients
  if (useDIC)  dum_egbg_sfcdiss(io_DIC ,:,:,:) = nutrient_flux(iDIC ,:,:,:) / 1.0e3 / conv_m3_kg ! convert back to mol kg^{-1} s^{-1}
  if (useNO3)  dum_egbg_sfcdiss(io_NO3 ,:,:,:) = nutrient_flux(iNO3 ,:,:,:) / 1.0e3 / conv_m3_kg ! convert back to mol kg^{-1} s^{-1}
  if (useNO2)  dum_egbg_sfcdiss(io_NO2 ,:,:,:) = nutrient_flux(iNO2 ,:,:,:) / 1.0e3 / conv_m3_kg ! convert back to mol kg^{-1} s^{-1}
  if (useNH4)  dum_egbg_sfcdiss(io_NH4 ,:,:,:) = nutrient_flux(iNH4 ,:,:,:) / 1.0e3 / conv_m3_kg ! convert back to mol kg^{-1} s^{-1}
  if (usePO4)  dum_egbg_sfcdiss(io_PO4 ,:,:,:) = nutrient_flux(iPO4 ,:,:,:) / 1.0e3 / conv_m3_kg ! convert back to mol kg^{-1} s^{-1}
  if (useFe)   dum_egbg_sfcdiss(io_TDFe,:,:,:) = nutrient_flux(iFe  ,:,:,:) / 1.0e3 / conv_m3_kg ! convert back to mol kg^{-1} s^{-1}
  if (useSiO2) dum_egbg_sfcdiss(io_SiO2,:,:,:) = nutrient_flux(iSiO2,:,:,:) / 1.0e3 / conv_m3_kg ! convert back to mol kg^{-1} s^{-1}
  !ckc  Isotopes for dissolved nutrients
  if (useDIC_13C) dum_egbg_sfcdiss(io_DIC_13C,:,:,:) = nutiso_flux(iDIC_13C,:,:,:) / 1.0e3 / conv_m3_kg ! convert back to mol kg^{-1} s^{-1}
  !  Organic matter
  ! Dissolved
  dum_egbg_sfcdiss(io_DOM_C ,:,:,:) = orgmat_flux(iCarb,1,:,:,:) / 1.0e3 / conv_m3_kg ! convert back to mol kg^{-1} s^{-1}
  if (nquota)  dum_egbg_sfcdiss(io_DOM_N ,:,:,:) = orgmat_flux(iNitr,1,:,:,:) / 1.0e3 / conv_m3_kg ! convert back to mol kg^{-1} s^{-1}
  if (pquota)  dum_egbg_sfcdiss(io_DOM_P ,:,:,:) = orgmat_flux(iPhos,1,:,:,:) / 1.0e3 / conv_m3_kg ! convert back to mol kg^{-1} s^{-1}
  if (fquota)  dum_egbg_sfcdiss(io_DOM_Fe,:,:,:) = orgmat_flux(iIron,1,:,:,:) / 1.0e3 / conv_m3_kg ! convert back to mol kg^{-1} s^{-1}
  !ckc Isotopes dissolved
  dum_egbg_sfcdiss(io_DOM_C_13C,:,:,:) = orgmatiso_flux(iCarb13C,1,:,:,:) / 1.0e3 / conv_m3_kg ! convert back to mol kg^{-1} s^{-1}
  !  if (squota)
  ! Particulate
  dum_egbg_sfcpart(is_POC   ,:,:,:) = orgmat_flux(iCarb,2,:,:,:) / 1.0e3 / conv_m3_kg ! convert back to mol kg^{-1} s^{-1}
  if (nquota)  dum_egbg_sfcpart(is_PON   ,:,:,:) = orgmat_flux(iNitr,2,:,:,:) / 1.0e3 / conv_m3_kg ! convert back to mol kg^{-1} s^{-1}
  if (pquota)  dum_egbg_sfcpart(is_POP   ,:,:,:) = orgmat_flux(iPhos,2,:,:,:) / 1.0e3 / conv_m3_kg ! convert back to mol kg^{-1} s^{-1}
  if (fquota)  dum_egbg_sfcpart(is_POFe  ,:,:,:) = orgmat_flux(iIron,2,:,:,:) / 1.0e3 / conv_m3_kg ! convert back to mol kg^{-1} s^{-1}
  !ckc Isotopes particulate
  dum_egbg_sfcpart(is_POC_13C,:,:,:) = orgmatiso_flux(iCarb13C,2,:,:,:) / 1.0e3 / conv_m3_kg ! convert back to mol kg^{-1} s^{-1}

  ! ---------------------------------------------------------- !
  ! Associated fluxes
  ! ---------------------------------------------------------- !
  ! O2 production (from DIC uptake)
  dum_egbg_sfcdiss(io_O2  ,:,:,:) = - 138.0 / 106.0 * nutrient_flux(iDIC ,:,:,:) / 1.0e3 / conv_m3_kg
  ! Photosynthesis-related changes in Alkalinity (REQUIRES MACRONUTRIENT UPTAKE FLUX - Ideally nitrate uptake flux)
  if (useNO3) then
     dum_egbg_sfcdiss(io_ALK ,:,:,:) = - 1.0 * nutrient_flux(iNO3 ,:,:,:) / 1.0e3 / conv_m3_kg ! convert back to mol kg^{-1} s^{-1}
  elseif (usePO4) then
     dum_egbg_sfcdiss(io_ALK ,:,:,:) = -16.0 * nutrient_flux(iPO4 ,:,:,:) / 1.0e3 / conv_m3_kg ! convert back to mol kg^{-1} s^{-1}
  else
     print*,"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
     print*,"No macronutrient uptake available to calculate alkalinity drawdown (NO3 or PO4)."
     print*,"Stopped in SUBROUTINE t_limitation (ecogem)."
     print*,"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
     STOP
  endif

  ! CaCO3 production
  gamma = (omega-1.0)**par_bio_red_POC_CaCO3_pP
  gamma = MERGE(gamma,0.0,omega.gt.1.0)
  dum_egbg_sfcpart(is_CaCO3,:,:,:) = dum_egbg_sfcpart(is_POC,:,:,:)       * par_bio_red_POC_CaCO3 * gamma
  dum_egbg_sfcdiss(io_DIC  ,:,:,:) = dum_egbg_sfcdiss(io_DIC,:,:,:) - 1.0 * dum_egbg_sfcpart(is_CaCO3,:,:,:)
  dum_egbg_sfcdiss(io_ALK  ,:,:,:) = dum_egbg_sfcdiss(io_ALK,:,:,:) - 2.0 * dum_egbg_sfcpart(is_CaCO3,:,:,:)
  dum_egbg_sfcdiss(io_Ca  ,:,:,:)  = dum_egbg_sfcdiss(io_Ca,:,:,:)  - 1.0 * dum_egbg_sfcpart(is_CaCO3,:,:,:)

  !cxarbon isotope for CaCO3
  if (useDIC_13C) then
     dum_egbg_sfcpart(is_CaCO3_13C,:,:,:) = dum_egbg_sfcpart(is_CaCO3,:,:,:) * CaCO3_Rfrac
     dum_egbg_sfcdiss(io_DIC_13C  ,:,:,:) = dum_egbg_sfcdiss(io_DIC_13C,:,:,:) - 1.0 * dum_egbg_sfcpart(is_CaCO3_13C,:,:,:)
  endif

  ! set initial values for protected fraction of POM and CaCO3
  dum_egbg_sfcpart(is_POC_frac2  ,:,:,n_k) = par_bio_remin_POC_frac2
  dum_egbg_sfcpart(is_CaCO3_frac2,:,:,n_k) = par_bio_remin_CaCO3_frac2

  ! ---------------------------------------------------------- !
  ! END
  ! ---------------------------------------------------------- !
end subroutine ecogem
! ******************************************************************************************************************************** !

! ******************************************************************************************************************************** !
! biogem DIAGNOSTICS - TIME-SLICE
SUBROUTINE diag_ecogem_timeslice( &
     & dum_dts,             &
     & dum_genie_clock,     &
     & dum_genie_tseries,   & ! input
     & dum_genie_tslice,    & ! input
     & dum_genie_intseries, & ! input
     & dum_genie_intslice,  & ! input
     & dum_endseries,       & ! input
     & dum_endslice,        & ! input
     & dum_egbg_fxsw,       & ! input
     & dum_mld,             & ! input
     & dum_egbg_sfcocn,     & ! input  -- tracer concentrations
     & dum_egbg_sfcpart,    &
     & dum_egbg_sfcdiss     &
     & )

  USE ecogem_lib
  USE ecogem_box
  USE ecogem_data
  use ecogem_data_netCDF

  IMPLICIT NONE

  ! ------------------------------------------------------------ !
  ! DUMMY ARGUMENTS
  ! ------------------------------------------------------------ !
  REAL   ,INTENT(IN)                               ::dum_dts             ! biogem time-step length (seconds)
  integer(kind=8),intent(in)                       ::dum_genie_clock     ! genie clock (ms since start) NOTE: 8-byte integer
  real   ,intent(in)                               ::dum_genie_tseries   ! BIOGEM time series time index
  real   ,intent(in)                               ::dum_genie_tslice    ! BIOGEM time slice time index
  logical,intent(in)                               ::dum_genie_intseries ! BIOGEM currently saving time series data
  logical,intent(in)                               ::dum_genie_intslice  ! BIOGEM currently saving time slice data
  logical,intent(in)                               ::dum_endseries       ! BIOGEM currently writing time series data
  logical,intent(in)                               ::dum_endslice        ! BIOGEM currently writing time slice data
  real   ,intent(in) ,dimension(n_i,n_j)           ::dum_egbg_fxsw       !
  real   ,intent(in) ,dimension(n_i,n_j)           ::dum_mld             ! mixed layer depth
  real   ,intent(in) ,dimension(n_ocn ,n_i,n_j,n_k)::dum_egbg_sfcocn     ! ecology-interface ocean tracer composition; ocn grid
  real   ,intent(out),dimension(n_sed ,n_i,n_j,n_k)::dum_egbg_sfcpart    ! ocean -> ecology flux; ocn grid
  real   ,intent(out),dimension(n_ocn ,n_i,n_j,n_k)::dum_egbg_sfcdiss    ! ecology -> ocean flux; ocn grid

  ! local variables
  integer :: i,j,k,l,io,ia,is
  integer :: loc_k1                                                !
  real    :: loc_t,loc_dts,loc_dtyr                                   !
  real    :: loc_yr_save                                              !
  logical :: write_timeslice
  integer :: dum_ntrec

  ! to convert per day rates into per second
  real,parameter :: pday = 86400.0

  loc_t = par_misc_t_runtime - real(dum_genie_clock)/(1000.0*conv_yr_s)
  loc_dts  = dum_dts
  loc_dtyr = loc_dts/conv_yr_s

  write_timeslice = dum_endslice

  ! UPDATE TIME SLICE INTEGRATION
  ! NOTE: carried out only when the BioGeM says so...
  if (dum_genie_intslice) then ! if BIOGEM says to add time step to integral

     ! UPDATE ECOGEM TIME SLICE DATA
     ! calculate local model time and time step length
     loc_t = par_misc_t_runtime - real(dum_genie_clock)/(1000.0*conv_yr_s)
     loc_dts  = dum_dts
     loc_dtyr = loc_dts/conv_yr_s

     ! NOTE: do not time increment weight quantities such as <int_bio_remin_timeslice> or <int_bio_settle_timeslice>,
     !       because they represent discrete increments rather than a flux or weightable concentration value
     int_plankton_timeslice(:,:,:,:,:) = int_plankton_timeslice(:,:,:,:,:) + loc_dtyr *      plankton(:,:,:,:,:)        ! mmol m^-3
     int_uptake_timeslice(:,:,:,:,:) =   int_uptake_timeslice(:,:,:,:,:) + loc_dtyr *   uptake_flux(:,:,:,:,:) * pday ! mmol m^-3 d^-1
     int_gamma_timeslice(:,:,:,:,:) =    int_gamma_timeslice(:,:,:,:,:) + loc_dtyr *    phys_limit(:,:,:,:,:)        ! mmol m^-3 d^-1
     int_nutrient_timeslice(:,:,:,:) =   int_nutrient_timeslice(:,:,:,:) + loc_dtyr *        nutrient(:,:,:,:)        ! mmol m^-3
     int_zoogamma_timeslice(:,:,:,:) =   int_zoogamma_timeslice(:,:,:,:) + loc_dtyr * zoo_limit(:,:,:,:)              ! mmol m^-3
     int_export_timeslice(:,:,:,:,:) =   int_export_timeslice(:,:,:,:,:) + loc_dtyr * export_flux(:,:,:,:,:) * pday ! mmol m^-3 d^-1  export calculation per plankton Fanny/Maria - Aug19
  end if

  ! write time-slice data and re-set integration
  if (   (write_timeslice) &
       & .OR.              &
       & (error_stop)      &
       & ) then

     loc_yr_save = dum_genie_tslice

     ! reporting
     if (write_timeslice) then
        WRITE(unit=6,fmt='(A57,f12.3)') &
             & ' >>> SAVING ECOGEM TIME-SLICE AVERAGE CENTERED @ year  : ', &
             & loc_yr_save
     elseif (error_stop) then
        WRITE(unit=6,fmt='(A57,f12.3)') &
             & ' >>> SAVING FATAL ERROR DATA DUMP @ year :               ', &
             & loc_yr_save
     else
        ! NOTHING
     end if

     ! Write timeseries data for each specified site for timeslice years
     if (n_tser.gt.0) then
        do k=1,n_tser
           call sub_update_netcdf_tser(k)
           call sub_save_netcdf_tseries(k)
           ncout1d_ntrec(k) = ncout1d_ntrec(k) + 1
           call sub_closefile(ncout1d_iou(k))
        enddo
     endif

     ! Write 2d array output for timeslice years
     call sub_update_netcdf(loc_yr_save,2)
     call sub_save_netcdf_2d()
     ncout2d_ntrec = ncout2d_ntrec + 1
     call sub_closefile(ncout2d_iou)


     ! reset array values after writing data
     ! NOTE: call subroutine here to always reset counter and integrated time (which otherwise accumulate in a GEMlite phase)
     call sub_init_int_timeslice()
  end if


end SUBROUTINE diag_ecogem_timeslice
! ******************************************************************************************************************************** !



! ******************************************************************************************************************************** !
! RESTART ECOGEM (save data)
SUBROUTINE ecogem_save_rst(dum_genie_clock)
  USE ecogem_lib
  use ecogem_data_netCDF
  IMPLICIT NONE
  ! ---------------------------------------------------------- !
  ! DEFINE DUMMY ARGUMENTS
  ! ---------------------------------------------------------- !
  integer(kind=8),INTENT(IN)::dum_genie_clock                  ! genie clock (milliseconds since start) NOTE: 8-byte integer
  ! ---------------------------------------------------------- !
  ! DEFINE LOCAL VARIABLES
  ! ---------------------------------------------------------- !
  integer::l
  integer::loc_iou
  real::loc_yr                                                 !
  CHARACTER(len=255)::loc_filename
  ! ---------------------------------------------------------- ! calculate local time (years)
  loc_yr = real(dum_genie_clock)/(48.0*1000.0*conv_yr_s)
  ! ---------------------------------------------------------- ! test for restart format
  IF (ctrl_ncrst) THEN
     ! ------------------------------------------------------- !
     ! SAVE RESTART DATA: NETCDF FORMAT
     ! ------------------------------------------------------- !
     string_ncrst = TRIM(par_outdir_name)//trim(par_ncrst_name)
     ncrst_ntrec = 0
     call sub_data_netCDF_ncrstsave(trim(string_ncrst),loc_yr,loc_iou)
  else
     ! ------------------------------------------------------- !
     ! SAVE RESTART DATA: BINARY DUMP FORMAT
     ! ------------------------------------------------------- !
     ! NOTE: data is saved unformatted for minimal file size
     !       also means that arrays can be written directly to file without needing to loop thought data
     loc_filename = TRIM(par_outdir_name)//trim(par_outfile_name)
     OPEN(unit=out,status='replace',file=loc_filename,form='unformatted',action='write')
     ! >>> WRITE STATEMENTS (FOR BINARY DUM) ... <<<<<<<<<<<<< !
     close(unit=out)
  end IF
  ! ---------------------------------------------------------- !
  ! END
  ! ---------------------------------------------------------- !
END SUBROUTINE ecogem_save_rst
! ******************************************************************************************************************************** !
