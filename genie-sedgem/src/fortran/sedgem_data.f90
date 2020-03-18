! ******************************************************************************************************************************** !
! sedgem_data.f90
! DATA LOADING/SAVING ROUTINES
! ******************************************************************************************************************************** !


MODULE sedgem_data


  use genie_control
  USE gem_cmn
  USE gem_util
  USE gem_netcdf
  USE sedgem_lib
  USE sedgem_box
  USE sedgem_data_netCDF
  USE sedgem_nnutils
  IMPLICIT NONE
  SAVE


CONTAINS


  ! ****************************************************************************************************************************** !
  ! DATA LOADING ROUTINES
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! LOAD SEDGEM 'goin' FILE OPTIONS
  SUBROUTINE sub_load_goin_sedgem()
    USE genie_util, ONLY: check_unit, check_iostat
    ! local variables
    integer::ios
    ! read data_SEDGEM file
    call check_unit(in,__LINE__,__FILE__)
    open(unit=in,file='data_SEDGEM',status='old',action='read',iostat=ios)
    if (ios /= 0) then
       print*,'ERROR: could not open SEDGEM initialisation namelist file'
       stop
    end if
    ! read in namelist and close data_SEDGEM file
    read(UNIT=in,NML=ini_sedgem_nml,IOSTAT=ios)
    if (ios /= 0) then
       print*,'ERROR: could not read SEDGEM namelist'
       stop
    else
       close(unit=in)
    end if
    ! set and report namelist data
    par_indir_name = trim(par_indir_name)//'/'
    par_outdir_name = trim(par_outdir_name)//'/'
    par_rstdir_name = trim(par_rstdir_name)//'/'
    if (ctrl_debug_init > 0) then
       ! --- RUN CONTROL --------------------------------------------------------------------------------------------------------- !
       print*,'--- RUN CONTROL ------------------------------------'
       print*,'Continuing run?                                     : ',ctrl_continuing
       print*,'Simulation start year [REAL]                        : ',start_year
       print*,'Simulation run length (yr)                          : ',par_misc_t_runtime
       ! --- PHYSICAL CONFIGURATION ---------------------------------------------------------------------------------------------- !
       print*,'--- PHYSICAL CONFIGURATION -------------------------'
       print*,'Top (well-mixed) sediment layer thickness (cm)      : ',par_sed_top_th
       print*,'Sub-surface detrital porosity (cm3 cm-3)            : ',par_sed_poros_det
       print*,'Sub-surface carbonate porosity (cm3 cm-3)           : ',par_sed_poros_CaCO3
       print*,'Maximum depth of shallow water sediments (m)        : ',par_sed_Dmax_neritic
       print*,'Force reef occurrence?                              : ',ctrl_sed_neritic_reef_force
       print*,'Minimum (basic) number of sedcore layers            : ',par_n_sedcore_tot_min
       print*,'Number of dimensioned sedcore layers ka-1 runtime   : ',par_n_sedcore_tot_perky
       print*,'# sedimentary stack sub-layers                      : ',n_sed_tot
       print*,'# initial sedimentary stack sub-layers filled       : ',n_sed_tot_init
       print*,'# sedimentary stack sub-layers to drop off bottom   : ',n_sed_tot_drop
       ! --- DETRITAL CONFIGURATION ---------------------------------------------------------------------------------------------- !
       print*,'Flux of refractory material (g cm-2 kyr-1)          : ',par_sed_fdet
       print*,'No pelagic (dust) detrital contribution?            : ',ctrl_sed_det_NOdust
       ! --- DIAGENESIS SCHEME: SELECTION ---------------------------------------------------------------------------------------- !
       print*,'--- DIAGENESIS SCHEME: SELECTION -------------------'
       print*,'CaCO3 diagenesis scheme                             : ',par_sed_diagen_CaCO3opt
       print*,'opal diagenesis scheme                              : ',par_sed_diagen_opalopt
       print*,'Corg diagenesis scheme                              : ',par_sed_diagen_Corgopt
       ! --- DIAGENESIS SCHEME: CONTROL ------------------------------------------------------------------------------------------ !
       print*,'--- DIAGENESIS SCHEME: CONTROL ---------------------'
       print*,'Bioturbate sediment stack?                          : ',ctrl_sed_bioturb
       print*,'Use Archer et al. [2002] bioturbation scheme?       : ',ctrl_sed_bioturb_Archer
       print*,'maximum layer depth for bioturbation                : ',par_n_sed_mix
       print*,'Max surface bioturbation mixing rate (cm2 yr-1)     : ',par_sed_mix_k_sur_max
       print*,'Min surface bioturbation mixing rate (cm2 yr-1)     : ',par_sed_mix_k_sur_min
       print*,'Prevent CaCO3 erosion (Fdis > Fsed)?                : ',ctrl_sed_noerosion
       print*,'CaCO3 interface dissolution?                        : ',ctrl_sed_interface
       print*,'CaCO3 red tracer tag fraction                       : ',par_sed_CaCO3_fred
       print*,'CaCO3 blue tracer tag fraction                      : ',par_sed_CaCO3_fblue
       print*,'Tag restart CaCO3?                                  : ',ctrl_sed_dyerestart
       print*,'Taged restart CaCO3 depth in layers (n)             : ',par_sed_dyerestart_n
       ! --- DIAGENESIS SCHEME: ORGANIC MATTER ----------------------------------------------------------------------------------- !
       print*,'--- DIAGENESIS SCHEME: ORGANIC MATTER --------------'
       print*,'Prevent frac2 from being remineralzied?             : ',ctrl_sed_diagen_preserve_frac2
       print*,'Fractional POC burial -- oxic conditions            : ',par_sed_diagen_fracCpres_ox
       print*,'Fractional POC burial -- anoxic conditions          : ',par_sed_diagen_fracCpres_anox
       print*,'Fractional POC burial -- euxinic conditions         : ',par_sed_diagen_fracCpres_eux
       print*,'Fraction of P relative to C buried -- oxic          : ',par_sed_diagen_fracC2Ppres_ox
       print*,'Fraction of P relative to C buried -- anoxic        : ',par_sed_diagen_fracC2Ppres_anox
       print*,'Fraction of P relative to C buried -- euxinic       : ',par_sed_diagen_fracC2Ppres_eux
       print*,'Return of PO4 to ocean in HUELSE 2017 scheme?       : ',ctrl_sed_huelse2017_remin_POP
       ! --- DIAGENESIS SCHEME: HUELSE 2017 -------------------------------------------------------------------------------------- !
       print*,'--- DIAGENESIS SCHEME: HUELSE 2017 -----------------'
       print*,'Corg rate constant parameterization scheme          : ',par_sed_huelse2017_kscheme
       print*,'Corg degradation rates redox dependent?             : ',par_sed_huelse2017_redox
       print*,'labile Corg degradation rate constant (1/yr)        : ',par_sed_huelse2017_k1
       print*,'refractory Corg degradation rate constant (1/yr)    : ',par_sed_huelse2017_k2
       print*,'refractory Corg deg. rate order compared to labile  : ',par_sed_huelse2017_k2_order
       print*,'anoxic refractory Corg deg. rate constant (1/yr)    : ',par_sed_huelse2017_k2_anoxic
       print*,'Include explicit P-cycle in OMEN-SED?               : ',par_sed_huelse2017_P_cycle
       print*,'Remove implicit Alk associated with buried sulf-OM? : ',par_sed_huelse2017_remove_impl_sulALK
       print*,'Simulate ocean Porg loss with buried sulf-OM?       : ',par_sed_huelse2017_sim_P_loss
       print*,'Simulate ocean Porg loss related to Corg burial?    : ',par_sed_huelse2017_sim_P_loss_pres_fracC
       print*,'Simulate increased P-regeneration under anoxia?     : ',par_sed_huelse2017_sim_P_regeneration
      ! --- DIAGENESIS SCHEME: ARCHER 1991 -------------------------------------------------------------------------------------- !
       print*,'--- DIAGENESIS SCHEME: ARCHER 1991 -----------------'
       print*,'dissolution rate constant, units of 1/s             : ',par_sed_archer1991_dissc
       print*,'dissolution rate constant scaling, (%)              : ',par_sed_archer1991_disscpct
       print*,'dissolution rate order                              : ',par_sed_archer1991_dissn
       print*,'organic degradation rate constant, 1/s              : ',par_sed_archer1991_rc
       print*,'loop limit in <o2org> subroutine                    : ',par_sed_archer1991_iterationmax
       ! --- DIAGENESIS SCHEME: opal --------------------------------------------------------------------------------------------- !
       print*,'base opal KSi value (yr-1)                          : ',par_sed_opal_KSi0
       ! --- CaCO3 PRODUCTION ---------------------------------------------------------------------------------------------------- !
       print*,'--- CaCO3 PRODUCTION -------------------------------'
       print*,'CaCO3 precip scale factor (abiotic) (mol cm-2 yr-1) : ',par_sed_CaCO3precip_sf
       print*,'CaCO3 precip rate law lower (abiotic)               : ',par_sed_CaCO3precip_exp
       print*,'CaCO3 precip scale factor (corals) (mol cm-2 yr-1)  : ',par_sed_reef_CaCO3precip_sf
       print*,'CaCO3 precip rate law lower (corals)                : ',par_sed_reef_CaCO3precip_exp
       print*,'CaCO3 precipitation as calcite (o/w aragonite)?     : ',par_sed_reef_calcite
       print*,'Min threshold for abiotic CaCO3 precipitation       : ',par_sed_CaCO3_abioticohm_min
       print*,'Reef CaCO3 porosity (cm3 cm-3)                      : ',par_sed_poros_CaCO3_reef
       print*,'prescribed CaCO3 production rate (mol cm-2 yr-1)    : ',par_sed_CaCO3burial
       print*,'prescribed global CaCO3 production rate (mol yr-1)  : ',par_sed_CaCO3burialTOT
       print*,'prescribed SrCO3 recryst rate (mol cm-2 yr-1)       : ',par_sed_SrCO3recryst
       print*,'prescribed global SrCO3 recryst rate (mol yr-1)     : ',par_sed_SrCO3recrystTOT
       print*,'carbonate recrystalization r87Sr                    : ',par_r87Sr_SrCO3recryst
       print*,'carbonate recrystalization d88Sr                    : ',par_d88Sr_SrCO3recryst
       ! --- Corg PRODUCTION ----------------------------------------------------------------------------------------------------- !
       print*,'--- Coeg PRODUCTION --------------------------------'
       print*,'prescribed Corg production rate (mol cm-2 yr-1)     : ',par_sed_Corgburial
       print*,'prescribed global Corg production rate (mol yr-1)   : ',par_sed_CorgburialTOT
       print*,'POC d13C offset compared to the d13C of CaCO3       : ',par_sed_Corgburial_Dd13C
       ! --- TRACE METALS -------------------------------------------------------------------------------------------------------- !
       print*,'--- TRACE METALS -----------------------------------'
       print*,'Default CaCO3 Ca:Li ratio                           : ',par_bio_red_CaCO3_LiCO3
       print*,'Partition coefficient (alpha)                       : ',par_bio_red_CaCO3_LiCO3_alpha
       print*,'Partition coefficient (alpha)                       : ',par_bio_red_CaCO3_SrCO3_alpha
       ! --- ISOTOPIC FRACTIONATION ---------------------------------------------------------------------------------------------- !
       print*,'--- ISOTOPIC FRACTIONATION -------------------------'
       print*,'set fixed d13C fractionation of Corg w.r.t. CaCO3?  : ',ctrl_sed_Corgburial_fixedD13C
       print*,'fractionation for intercellular C fixation          : ',par_d13C_DIC_Corg_ef
       print*,'Benthic foram 13C fractionation scheme ID string    : ',opt_sed_foram_b_13C_delta
       print*,'7/6Li fractionation between Li and LiCO3            : ',par_d7Li_LiCO3_epsilon
       print*,'44/40Ca fractionation between Ca and CaCO3 (corals) : ',par_d44Ca_CaCO3_epsilon
       print*,'abiotic 44/40Ca fractionation between Ca and cal    : ',par_d44Ca_abioticcal_epsilon0
       print*,'abiotic 44/40Ca fractionation between Ca and arg    : ',par_d44Ca_abioticarg_epsilon0
       print*,'T-dependence of cal abiotic 44/40Ca fractionation   : ',par_d44Ca_abioticcal_epsilondT
       print*,'T-dependence of arg abiotic 44/40Ca fractionation   : ',par_d44Ca_abioticarg_epsilondT
       print*,'88/86 fractionation between Sr and SrCO3            : ',par_d88Sr_SrCO3_epsilon
       ! --- HYDROTHERMAL, OCEAN CRUSTAL WEATHERING, & CLAY FORMATION ------------------------------------------------------------ !
       print*,'--- HYDROTHERMAL, WEATHERING, & CLAY FORMATION -----'
       print*,'hydrothermal Li flux (mol yr-1)                     : ',par_sed_hydroip_fLi
       print*,'hydrothermal Li flux d7Li (o/oo)                    : ',par_sed_hydroip_fLi_d7Li
       print*,'Li low-T alteration sink (mol yr-1) (Li/Ca norm)    : ',par_sed_lowTalt_fLi_alpha
       print*,'Li low-T alteration sink 7Li epsilon (o/oo)         : ',par_sed_lowTalt_7Li_epsilon
       print*,'Li clay formation sink (mol yr-1) (Li/Ca norm)      : ',par_sed_clay_fLi_alpha
       print*,'Li clay formation sink 7Li epsilon (o/oo)           : ',par_sed_clay_7Li_epsilon
       print*,'hydrothermal Ca flux (mol yr-1)                     : ',par_sed_hydroip_fCa
       print*,'hydrothermal Ca flux d44Ca (o/oo)                   : ',par_sed_hydroip_fCa_d44Ca
       print*,'hydrothermal Mg flux (mol yr-1)                     : ',par_sed_hydroip_fMg
       print*,'Ca low-T alteration sink (mol yr-1) (Ca/Mg norm)    : ',par_sed_lowTalt_fCa_alpha
       print*,'Ca low-T alteration sink 44Ca epsilon (o/oo)        : ',par_sed_lowTalt_44Ca_epsilon
       print*,'hydrothermal Sr flux (mol yr-1)                     : ',par_sed_hydroip_fSr
       print*,'hydrothermal Sr flux r87Sr (87/86)                  : ',par_sed_hydroip_fSr_r87Sr
       print*,'hydrothermal Sr flux d88Sr (o/oo)                   : ',par_sed_hydroip_fSr_d88Sr
       print*,'Sr low-T alteration sink (mol yr-1)                 : ',par_sed_lowTalt_fSr_alpha
       print*,'CO2 low-T alteration (weathering!) sink (mol yr-1)  : ',par_sed_lowTalt_fCO2
       print*,'hydrothermal CO2 outgassing (mol yr-1)              : ',par_sed_hydroip_fDIC
       print*,'d13C                                                : ',par_sed_hydroip_fDIC_d13C
       ! --- MISC CONTROLS ------------------------------------------------------------------------------------------------------- !
       print*,'--- MISC CONTROLS ----------------------------------'
       print*,'Ca-only adjustment for forced ocean saturation?     : ',ctrl_sed_forcedohmega_ca
       print*,'Forced minimum saturation (calcite ohmega) anywhere : ',par_sed_ohmegamin
       print*,'Imposed sed->ocn flux (mol Ca cm-2 (time-step)-1)   : ',par_sed_ohmegamin_flux
       print*,'Impose alt detrital burial flux forcing?            : ',ctrl_sed_Fdet
       print*,'Impose alt CaCO3 burial flux forcing?               : ',ctrl_sed_Fcaco3
       print*,'Impose alt opal burial flux forcing?                : ',ctrl_sed_Fopal
       print*,'Set dissolution flux = rain flux for CaCO3 only?    : ',ctrl_force_sed_closedsystem_CaCO3
       print*,'Set dissolution flux = rain flux for opal only?     : ',ctrl_force_sed_closedsystem_opal
       ! --- I/O: DIRECTORY DEFINITIONS ------------------------------------------------------------------------------------------ !
       print*,'--- I/O: DIRECTORY DEFINITIONS ---------------------'
       print*,'(Paleo config) input dir. name                      : ',trim(par_pindir_name)
       print*,'Input dir. name                                     : ',trim(par_indir_name)
       print*,'Output dir. name                                    : ',trim(par_outdir_name)
       print*,'Restart (input) dir. name                           : ',trim(par_rstdir_name)
       print*,'Filename for restart input                          : ',trim(par_infile_name)
       print*,'Filename for restart output                         : ',trim(par_outfile_name)
       print*,'Sediment water depth grid name                      : ',trim(par_sed_topo_D_name)
       print*,'Shallow water sediment (coral reef) mask name       : ',trim(par_sed_reef_mask_name)
       print*,'Sediment core save mask name                        : ',trim(par_sedcore_save_mask_name)
       print*,'Biodiffusion profile name                           : ',trim(par_sed_mix_k_name)
       print*,'File containing output years for 0D data            : ',trim(par_output_years_file_0d)
       print*,'File containing output years for 2D data            : ',trim(par_output_years_file_2d)
       print*,'Save threshold of accumulated time (yr)             : ',par_sed_age_save_dt
       print*,'Alt detrital burial flux forcing filename           : ',trim(par_sed_Fdet_name)
       print*,'Alt CaCO3 burial flux forcing filename              : ',trim(par_sed_Fcaco3_name)
       print*,'Alt opal burial flux forcing filename               : ',trim(par_sed_Fopal_name)
       ! --- I/O: MISC ----------------------------------------------------------------------------------------------------------- !
       print*,'--- I/O: MISC --------------------------------------'
       print*,'save timeseries output                              : ',ctrl_timeseries_output
       print*,'append data to output files on restart              : ',ctrl_append_data
       print*,'Save (sedcore) output in ascii format?              : ',ctrl_data_save_ascii
       print*,'Save sedcorenv output (ascii)?                      : ',ctrl_data_save_sedcorenv
       print*,'Report sediment data as a mass fraction?            : ',ctrl_data_save_wtfrac
       print*,'Debug level #1?                                     : ',ctrl_misc_debug1
       print*,'Debug level #2?                                     : ',ctrl_misc_debug2
       print*,'Debug level #3?                                     : ',ctrl_misc_debug3
       print*,'Debug level #4?                                     : ',ctrl_misc_debug4
       print*,'Report errors?                                      : ',ctrl_misc_report_err
       print*,'i sediment coordinate for debug reporting           : ',par_misc_debug_i
       print*,'j sediment coordinate for debug reporting           : ',par_misc_debug_j
       print*,'Report level #1 debug?                              : ',ctrl_debug_lvl1
       ! --- DATA SAVING: MISC --------------------------------------------------------------------------------------------------- !
       print*,'--- DATA SAVING: MISC ------------------------------'
       print*,'Restart in netCDF format?                           : ',ctrl_ncrst
       print*,'netCDF restart file name                            : ',trim(par_ncrst_name)
       ! #### INSERT CODE TO LOAD ADDITIONAL PARAMETERS ########################################################################## !
       !
       ! ######################################################################################################################### !
    end if
    ! set ash event flux (g cm-2 kyr-1) (e.g. set at ca. x10 typical dust/detrital value)
    par_sed_ashevent_fash = 1.8
    ! revise diagenesis options
    if (ctrl_sed_Fcaco3) par_sed_diagen_CaCO3opt = 'ALL'
    if (ctrl_sed_Fopal) par_sed_diagen_opalopt = 'ALL'

  END SUBROUTINE sub_load_goin_sedgem
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! *** LOAD SEDGEM RESTART DATA ************************************************************************************************* !
  ! ****************************************************************************************************************************** !
  SUBROUTINE sub_data_load_rst(dum_sfxsumsed,dum_sfxocn)
    USE sedgem_lib
    use gem_netcdf
    USE genie_util, ONLY:check_unit,check_iostat
    ! -------------------------------------------------------- !
    ! DEFINE DUMMY ARGUMENTS
    ! -------------------------------------------------------- !
    real,dimension(n_sed,n_i,n_j),intent(inout)::dum_sfxsumsed
    real,DIMENSION(n_ocn,n_i,n_j),intent(inout)::dum_sfxocn    ! sediment dissolution flux interface array
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    integer::i,j,k,l,io,is,iv                                           ! local counting variables
    integer::ios                                               !
    integer::loc_ncid                                          !
    CHARACTER(len=255)::loc_filename                           ! filename string
    integer::loc_n_l_sed                                       ! number of selected tracers in the re-start file
    integer,DIMENSION(n_sed)::loc_conv_iselected_is            ! number of selected sediment tracers in restart
    real,dimension(n_i,n_j)::loc_ij                            ! 
    integer::loc_ndims,loc_nvars
    integer,ALLOCATABLE,dimension(:)::loc_dimlen
    integer,ALLOCATABLE,dimension(:,:)::loc_varlen
    integer,ALLOCATABLE,dimension(:)::loc_vdims
    character(20),ALLOCATABLE,dimension(:)::loc_varname
    integer::loc_n_sed_tot                                     !
    real,ALLOCATABLE,dimension(:,:,:)::loc_ijk
    ! -------------------------------------------------------- !
    ! INITIALIZE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    IF (ctrl_misc_debug3) print*, 'INITIALIZE LOCAL VARIABLES'
    ! -------------------------------------------------------- ! set filename
    IF (ctrl_misc_debug4) print*, 'set filename'
    IF (ctrl_ncrst) THEN
       loc_filename = TRIM(par_rstdir_name)//par_ncrst_name
    else
       loc_filename = TRIM(par_rstdir_name)//trim(par_infile_name)
    endif
    ! -------------------------------------------------------- ! check file status
    IF (ctrl_misc_debug4) print*, 'check file status'
    call check_unit(in,__LINE__,__FILE__)
    OPEN(unit=in,status='old',file=loc_filename,form='unformatted',action='read',IOSTAT=ios)
    close(unit=in)
    If (ios /= 0) then
       CALL sub_report_error( &
            & 'sedgem_data','sub_data_load_restart', &
            & 'You have requested a CONTINUING run, but restart file <'//trim(loc_filename)//'> does not exist', &
            & 'SKIPPING - using default initial values', &
            & (/const_real_null/),.false. &
            & )
    else
       ! -------------------------------------------------------- !
       ! LOAD RESTART
       ! -------------------------------------------------------- !
       IF (ctrl_misc_debug3) print*, 'LOAD RESTART'
       IF (ctrl_ncrst) THEN
          call sub_openfile(loc_filename,loc_ncid)
          ! -------------------------------------------------------- ! determine number of variables
          IF (ctrl_misc_debug4) print*, 'determine number of variables'
          call sub_inqdims (loc_filename,loc_ncid,loc_ndims,loc_nvars)
          ! -------------------------------------------------------- ! allocate arrays
          IF (ctrl_misc_debug4) print*, 'allocate arrays'
          ALLOCATE(loc_dimlen(loc_ndims),STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          ALLOCATE(loc_varlen(2,loc_nvars),STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          ALLOCATE(loc_vdims(loc_nvars),STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          ALLOCATE(loc_varname(loc_nvars),STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          ! -------------------------------------------------------- ! get variable names
          IF (ctrl_misc_debug4) print*, 'get variable names'
          call sub_inqvars(loc_ncid,loc_ndims,loc_nvars,loc_dimlen,loc_varname,loc_vdims,loc_varlen)
          ! -------------------------------------------------------- ! determine stack height and allocate local array
          IF (ctrl_misc_debug4) print*, 'determine stack height and allocate local array'
          loc_n_sed_tot = loc_dimlen(5)
          If (loc_n_sed_tot > n_sed_tot) then
             CALL sub_report_error( &
                  & 'sedgem_data','sub_data_load_restart', &
                  & 'You have compiled in a smaller sediment stack <n='//fun_conv_num_char_n(4,n_sed_tot)// &
                  & '> than the restart <n='//fun_conv_num_char_n(4,loc_n_sed_tot)//'>', &
                  & 'I am not programmed for such liberal activities ... ENDING ...', &
                  & (/const_real_null/),.true. &
                  & )
          elseif (loc_n_sed_tot < n_sed_tot) then
             CALL sub_report_error( &
                  & 'sedgem_data','sub_data_load_restart', &
                  & 'You have compiled in a larger sediment stack <n='//fun_conv_num_char_n(4,n_sed_tot)// &
                  & '> than the restart <n='//fun_conv_num_char_n(4,loc_n_sed_tot)//'> ' // &
                  & 'You may experience a core hiatus ...', &
                  & 'CONTINUING ...', &
                  & (/const_real_null/),.false. &
                  & )
          end If
          ALLOCATE(loc_ijk(n_i,n_j,loc_n_sed_tot),STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          ! -------------------------------------------------------- ! load and apply sediment tracers that are selected
          IF (ctrl_misc_debug4) print*, 'load and apply sediment tracers that are selected'
          ! NOTE: the k dimension is flipped in sub_getvarijk
          IF (ctrl_debug_init == 1) print*,' * Loading sediment stack restart tracers: '
          DO iv=1,loc_nvars
             DO l=1,n_l_sed
                is = conv_iselected_is(l)
                if ('sed_'//trim(string_sed(is)) == trim(loc_varname(iv))) then
                   IF (ctrl_debug_init == 1) print*,'   ',trim(loc_varname(iv))
                   loc_ijk(:,:,:) = 0.0
                   call sub_getvarijk(loc_ncid,'sed_'//trim(string_sed(is)),n_i,n_j,loc_n_sed_tot,loc_ijk(:,:,:))
                   DO i = 1,n_i
                      DO j = 1,n_j
                         if (sed_mask(i,j)) then
                            sed_top(is,i,j) = loc_ijk(i,j,loc_n_sed_tot)
                            do k = 1,loc_n_sed_tot-1
                               sed(is,i,j,(n_sed_tot-loc_n_sed_tot)+k) = loc_ijk(i,j,k)
                            end do
                         else
                            sed(is,i,j,:) = 0.0
                         end if
                      end DO
                   END DO
                endif
             end do
          end DO
          ! -------------------------------------------------------- ! load and apply sediment stack height
          IF (ctrl_misc_debug4) print*, 'load and apply sediment stack height'
          call sub_getvarij(loc_ncid,'phys_dh',n_i,n_j,loc_ij(:,:))
          DO i = 1,n_i
             DO j = 1,n_j
                if (sed_mask(i,j)) then
                   sed_top_h(i,j) = real(n_sed_tot - 2) + loc_ij(i,j)
                end if
             end DO
          end DO
          ! -------------------------------------------------------- ! load and apply dissolution flux tracers
          IF (ctrl_misc_debug4) print*, 'load and apply dissolution flux tracers'
          IF (ctrl_debug_init == 1) print*,' * Loading dissolution flux restart tracers: '
          DO iv=1,loc_nvars
             DO l=1,n_l_ocn
                io = conv_iselected_io(l)
                if ('fdis_'//trim(string_ocn(io)) == trim(loc_varname(iv))) then
                   IF (ctrl_debug_init == 1) print*,'   ',trim(loc_varname(iv))
                   loc_ij(:,:) = 0.0
                   call sub_getvarij(loc_ncid,'fdis_'//trim(string_ocn(io)),n_i,n_j,loc_ij(:,:))
                   DO i = 1,n_i
                      DO j = 1,n_j
                         dum_sfxocn(io,i,j) = loc_ij(i,j)
                      end DO
                   END DO
                endif
             end do
          end DO
          ! -------------------------------------------------------- ! deallocate arrays
          IF (ctrl_misc_debug4) print*, 'deallocate arrays'
          deALLOCATE(loc_dimlen,STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          deALLOCATE(loc_varlen,STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          deALLOCATE(loc_vdims,STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          deALLOCATE(loc_varname,STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          deALLOCATE(loc_ijk,STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          ! -------------------------------------------------------- ! close file
          IF (ctrl_misc_debug4) print*, 'close file'
          call sub_closefile(loc_ncid)
       else
          OPEN(unit=in,status='old',file=loc_filename,form='unformatted',action='read',IOSTAT=ios)
          read(unit=in,iostat=ios)                                        &
               & loc_n_l_sed,                                             &
               & (loc_conv_iselected_is(l),l=1,loc_n_l_sed),              &
               & (sed(loc_conv_iselected_is(l),:,:,:),l=1,loc_n_l_sed),   &
               & (sed_top(loc_conv_iselected_is(l),:,:),l=1,loc_n_l_sed), &
               & sed_top_h(:,:),                                          &
               & (dum_sfxsumsed(conv_iselected_is(l),:,:),l=1,loc_n_l_sed)
          close(unit=in,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
       endif
    end If
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
    IF (ctrl_misc_debug3) print*, 'END'
  end SUBROUTINE sub_data_load_rst
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! *** INITIALIZE SEDCORES ****************************************************************************************************** !
  ! ****************************************************************************************************************************** !
  ! NOTE: this mask sets the grid point locations where synthetic sediment 'cores' will saved, specified in the mask file by;
  !       1.0 = 'save here'
  !       0.0 = 'don't save here'
  !       (other values are not valid, or rather, could give rather unpredictable results ...)
  SUBROUTINE sub_data_sedcore_init()
    USE genie_util, ONLY: check_unit, check_iostat
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    INTEGER::i,j,n
    integer::loc_len
    CHARACTER(len=255)::loc_filename
    REAL,DIMENSION(n_i,n_j)::loc_ij
    ! -------------------------------------------------------- !
    ! INITIALIZE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    loc_ij(:,:) = 0.0
    sed_save_mask(:,:) = .FALSE.
    ! set alt dir path string length
    loc_len = LEN_TRIM(par_pindir_name)
    ! -------------------------------------------------------- !
    ! DETERMINE SEDCORES TO BE SAVED
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! load sediment core save mask
    if (loc_len > 0) then
    loc_filename = TRIM(par_pindir_name)//TRIM(par_sedcore_save_mask_name)
    else
    loc_filename = TRIM(par_indir_name)//TRIM(par_sedcore_save_mask_name)
    endif
    CALL sub_load_data_ij(loc_filename,n_i,n_j,loc_ij(:,:))
    ! -------------------------------------------------------- ! set sediment save mask & count number of sedcores
    nv_sedcore = 0
    DO i=1,n_i
       DO j=1,n_j
          if ((loc_ij(i,j) < const_real_nullsmall) .OR. (.NOT. sed_mask(i,j))) then
             sed_save_mask(i,j) = .FALSE.
          else
             sed_save_mask(i,j) = .TRUE.
             nv_sedcore = nv_sedcore + 1
          end if
       end do
    end do
    ! -------------------------------------------------------- !
    ! ALLOCATED ARRAY SPACE
    ! -------------------------------------------------------- !
    ! NOTE: <sedcore_store> is used to accumulate the excess sed layers not retained (pop-ed off of the stack) in the full sed array
    !                       and as such needs to have a tracer dimension equal to the full sed tracer number
    !       <sedcore> is used to reconstruct the sediment cores
    !                 and only needs to be dimensioned as large as the number of saved tracers
    ALLOCATE(vsedcore_store(1:nv_sedcore),STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)
    do n=1,nv_sedcore
       allocate(vsedcore_store(n)%top(1:n_sedcore_tracer),STAT=alloc_error)
       call check_iostat(alloc_error,__LINE__,__FILE__)
       allocate(vsedcore_store(n)%lay(1:n_sedcore_tracer,1:n_sedcore_tot),STAT=alloc_error)
       call check_iostat(alloc_error,__LINE__,__FILE__)
    end do
    ! -------------------------------------------------------- !
    ! INITIALIZE SEDCORES
    ! -------------------------------------------------------- !
    if (nv_sedcore > 0) then
       DO n=1,nv_sedcore
          vsedcore_store(n)%ht = 0.0
          vsedcore_store(n)%top(:) = 0.0
          vsedcore_store(n)%lay(:,:) = 0.0
       end do
       ! set sedcore (i,j) locations
       ! NOTE: <n> used as counter to index [vsedcore_store]
       n = 0
       DO i=1,n_i
          DO j=1,n_j
             if (sed_save_mask(i,j)) then
                n = n + 1
                vsedcore_store(n)%i = i
                vsedcore_store(n)%j = j
                vsedcore_store(n)%save = .true.
             end if
          end do
       end do
    end if
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  end SUBROUTINE sub_data_sedcore_init
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALIZE SEDIMENT GRID
  ! NOTE: the grid as set up is specific to the GOLDSTEIN ocean model in an equal-area configuration
  !       so this subroutine needs ot be replaced or revised to make SEDGEM compatible with another ocean model
  ! NOTE: the lat-lon grid information is not critical, but sets the details of the grid associated with the saved data
  !       however, the depth set in this subroutine determinds the hydrostatic pressure on the sediments
  !       (and thus the stability of CaCO3 in the sediments)
  SUBROUTINE sub_init_phys_sed()
    ! local variables
    INTEGER::i,j
    INTEGER::loc_len
    CHARACTER(len=255)::loc_filename
    real::loc_th0,loc_th1,loc_s0,loc_s1,loc_ds
    real,dimension(0:n_j)::loc_s,loc_sv
    real,DIMENSION(n_i,n_j)::loc_ij                  ! 
    ! set alt dir path string length
    loc_len = LEN_TRIM(par_pindir_name)
    ! zero the grid information and 'physics' array
    loc_ij(:,:)     = 0.0
    phys_sed(:,:,:) = 0.0
    ! initialize masks
    sed_mask(:,:)      = .FALSE.
    sed_mask_reef(:,:) = .FALSE.
    sed_mask_muds(:,:) = .FALSE.
    ! calculate local constants
    loc_th0 = -const_pi/2                            ! 
    loc_th1 = const_pi/2                             ! 
    loc_s0 = sin(loc_th0)                            ! 
    loc_s1 = sin(loc_th1)                            !
    loc_ds = (loc_s1-loc_s0)/real(n_j)               ! 
    DO j=0,n_j
       loc_sv(j) = loc_s0 + real(j)*loc_ds           ! 
       loc_s(j) = loc_sv(j) - 0.5*loc_ds             ! 
    end do
    ! initialize array values
    DO i=1,n_i
       DO j=1,n_j
          phys_sed(ips_lat,i,j)  = (180.0/const_pi)*ASIN(loc_s(j))
          phys_sed(ips_lon,i,j)  = (360.0/real(n_i))*(real(i)-0.5) + par_grid_lon_offset
          phys_sed(ips_dlat,i,j) = (180.0/const_pi)*(ASIN(loc_sv(j)) - ASIN(loc_sv(j-1)))
          phys_sed(ips_dlon,i,j) = (360.0/real(n_i))
          phys_sed(ips_latn,i,j) = (180.0/const_pi)*ASIN(loc_sv(j))
          phys_sed(ips_lone,i,j) = (360.0/n_i)*real(i) + par_grid_lon_offset
          phys_sed(ips_A,i,j)    = 2.0*const_pi*(const_rEarth**2)*(1.0/real(n_i))*(loc_sv(j) - loc_sv(j-1))
          phys_sed(ips_rA,i,j)   = 1.0/phys_sed(ips_A,i,j)
       END DO
    END DO
    ! load sediment bathymetry
    if (loc_len > 0) then
        loc_filename = TRIM(par_pindir_name)//TRIM(par_sed_topo_D_name)
    else
        loc_filename = TRIM(par_indir_name)//TRIM(par_sed_topo_D_name)
    endif
    CALL sub_load_data_ij(loc_filename,n_i,n_j,loc_ij(:,:))
    phys_sed(ips_D,:,:) = loc_ij(:,:)
    ! load reef mask
    if (par_sed_Dmax_neritic > -const_real_nullsmall) then
       if (loc_len > 0) then
            loc_filename = TRIM(par_pindir_name)//TRIM(par_sed_reef_mask_name)
       else
            loc_filename = TRIM(par_indir_name)//TRIM(par_sed_reef_mask_name)            
       endif
       CALL sub_load_data_ij(loc_filename,n_i,n_j,loc_ij(:,:))
    else
       loc_ij(:,:) = 0.0
    endif
    ! define sediment masks - used as an area mulitplying factor
    ! (both in logial and area mulitplying factor (real) representations)
    ! NOTE: subsquently, the masks are updated depending on whether there ia an overlying ocean cell or not.
    !       (hence, these masks are just 'potential' locations here at the outset)
    DO i=1,n_i
       DO j=1,n_j
          if (phys_sed(ips_D,i,j) < const_real_nullsmall) then
             ! land! => no sediments!!!
             phys_sed(ips_mask_sed,i,j) = 0.0
             sed_mask(i,j) = .FALSE.
             phys_sed(ips_mask_sed_reef,i,j) = 0.0
             sed_mask_reef(i,j) = .FALSE.
             phys_sed(ips_mask_sed_muds,i,j) = 0.0
             sed_mask_muds(i,j) = .FALSE.
          else
             ! not land(!), so set sediment mask TRUE
             phys_sed(ips_mask_sed,i,j) = 1.0
             sed_mask(i,j) = .TRUE.
             if (phys_sed(ips_D,i,j) < par_sed_Dmax_neritic) then
                ! water shallower than generic neritic depth limit => either reef or mud!
                ! NOTE: if ctrl_sed_neritic_reef_force is set, then shallow points are forced to be reef
                !       (ctrl_sed_neritic_reef_force is .false. by default)
                if ((loc_ij(i,j) > const_real_nullsmall) .OR. ctrl_sed_neritic_reef_force) then
                   ! mask specified as reef ... therefore reef!
                   phys_sed(ips_mask_sed_reef,i,j) = 1.0
                   sed_mask_reef(i,j) = .TRUE.
                   phys_sed(ips_mask_sed_muds,i,j) = 0.0
                   sed_mask_muds(i,j) = .FALSE.
                else
                   ! mask not specified as reef -- you got mud instead!
                   phys_sed(ips_mask_sed_reef,i,j) = 0.0
                   sed_mask_reef(i,j) = .FALSE.
                   phys_sed(ips_mask_sed_muds,i,j) = 1.0
                   sed_mask_muds(i,j) = .TRUE.
                end if
             elseif (ctrl_sed_neritic_reef_force) then
                ! force reef occurrence regardless of depth (assuming depth greater than prescribed neritic limit)
                if (loc_ij(i,j) > const_real_nullsmall) then
                   phys_sed(ips_mask_sed_reef,i,j) = 1.0
                   sed_mask_reef(i,j) = .TRUE.
                   phys_sed(ips_mask_sed_muds,i,j) = 0.0
                   sed_mask_muds(i,j) = .FALSE.
                endif
             else
                ! otherwise ... no reef or mud!
                phys_sed(ips_mask_sed_reef,i,j) = 0.0
                sed_mask_reef(i,j) = .FALSE.
                phys_sed(ips_mask_sed_muds,i,j) = 0.0
                sed_mask_muds(i,j) = .FALSE.
             end if
          end if
       END DO
    END DO
  END SUBROUTINE sub_init_phys_sed
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! META-OPTION SETUP AND PARAMETER VALUE CONSISTENCY CHECK
  SUBROUTINE sub_check_par_sedgem()
    ! local variables
    LOGICAL::loc_flag
    ! initialize variables
    loc_flag = .FALSE.
    ! check that the i,j debug reporting indices specified in sedgem_config.par are within maxis and maxjs
    If (par_misc_debug_i > n_i .OR. par_misc_debug_i < 1) then
       loc_flag = .TRUE.
       par_misc_debug_i = 1
    end if
    If (par_misc_debug_j > n_j .OR. par_misc_debug_j < 1) then
       loc_flag = .TRUE.
       par_misc_debug_j = 1
    end if
    if (loc_flag) then
       CALL sub_report_error( &
            & 'sedgem_data','sub_check_par_sedgem', &
            & 'the i,j indices for spatially-explicit debugging '// &
            & 'must be within the sediment grid limit specification', &
            & 'SETTING OFFENDING PARAMETER VALUES TO 1; CONTINUING', &
            & (/const_real_null/),.false. &
            & )
       loc_flag = .FALSE.
    end If
    IF ((.NOT. sed_select(is_det)) .OR. (.NOT. sed_select(is_ash))) THEN
       CALL sub_report_error( &
            & 'sedgem_data','sub_check_par_sedgem','Both det and ash tracers must be selected '// &
            & '(FILE: gem_config_sed.par)', &
            & 'STOPPING', &
            & (/const_real_null/),.true. &
            & )
    ENDIF
  end SUBROUTINE sub_check_par_sedgem
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALIZE SEDIMENT PARAMETERS
  SUBROUTINE sub_init_sed()
    ! local variables
    INTEGER::l,is,n                      ! grid and tracer index counters
    CHARACTER(len=255)::loc_filename
    real,DIMENSION(n_i,n_j)::loc_ij      !  
    ! set default array values
    conv_sed_mol_cm3(:)      = 1.0       ! 
    conv_sed_cm3_mol(:)      = 1.0       ! 
    conv_sed_cm3_g(:)        = 1.0       ! 
    conv_sed_g_cm3(:)        = 1.0       ! 
    conv_sed_mask(:)         = 0.0       ! 
    ! zero flux arrays
    sed_fsed(:,:,:) = 0.0                ! 
    sed_fdis(:,:,:) = 0.0                ! 
    ! set up conversion of mol -> cm3 and cm3 -> g (and reciprocals)
    DO l=1,n_l_sed
       is = conv_iselected_is(l)
       ! criterion for particulate organic matter (POM), elemental components, and particle-reactive scavenged elements
       if ((sed_dep(is) == is_POC .AND. sed_type(is) < 10) .OR. (sed_type(is) == par_sed_type_POM)) then
          conv_sed_mol_cm3(is) = conv_POC_mol_cm3
          conv_sed_cm3_g(is)   = conv_POC_cm3_g
       end if
       ! criterion for carbonate, elemental components, and particle-reactive scavenged elements
       if ((sed_dep(is) == is_CaCO3 .AND. sed_type(is) < 10) .OR. (sed_type(is) == par_sed_type_CaCO3)) then
          conv_sed_mol_cm3(is) = conv_cal_mol_cm3
          conv_sed_cm3_g(is)   = conv_cal_cm3_g
       end if
       ! criterion for opal, elemental components, and particle-reactive scavenged elements
       if ((sed_dep(is) == is_opal .AND. sed_type(is) < 10) .OR. (sed_type(is) == par_sed_type_opal)) then
          conv_sed_mol_cm3(is) = conv_opal_mol_cm3
          conv_sed_cm3_g(is)   = conv_opal_cm3_g
       end if
       ! detrital and refractory material
       if ((sed_dep(is) == is_det .AND. sed_type(is) < 10) .OR. (sed_type(is) == par_sed_type_abio)) then
          conv_sed_mol_cm3(is) = conv_det_mol_cm3
          conv_sed_cm3_g(is)   = conv_det_cm3_g
       end if
       ! 'dependent' components (isotopes and 'age')
       conv_sed_mol_cm3(is) = conv_sed_mol_cm3(sed_dep(is))
       conv_sed_cm3_g(is)   = conv_sed_cm3_g(sed_dep(is))
       ! reciprocal conversion
       if(conv_sed_mol_cm3(is) > const_real_nullsmall) conv_sed_cm3_mol(is) = 1.0/conv_sed_mol_cm3(is)
       if(conv_sed_cm3_g(is) > const_real_nullsmall)   conv_sed_g_cm3(is)   = 1.0/conv_sed_cm3_g(is)
    end DO
    ! set up the mask for defining which sedimentary components contribute to the actual volume of the sediments
    ! (and which are therefore 'virtual')
    ! => POC, CaCO3, opal, miscellaneous detrital material ('det'), ash, iron oxides (FeO)
    do is=1,n_sed
       SELECT CASE (sed_type(is))
       case (par_sed_type_bio,par_sed_type_abio)
          conv_sed_mask(is) = 1.0
       case default
          conv_sed_mask(is) = 0.0
       end select
    end do
    ! allocate size of look-up tables and load data -- CaCO3
    ! NOTE: check for problems allocating array space
    if (par_sed_diagen_CaCO3opt == 'ridgwell2001lookup' .OR. par_sed_diagen_CaCO3opt == 'ridgwell2001lookupvec') then
       ALLOCATE(lookup_sed_dis_cal( &
            & lookup_i_D_min:lookup_i_D_max, &
            & lookup_i_dCO3_min:lookup_i_dCO3_max, &
            & lookup_i_frac_min:lookup_i_frac_max, &
            & lookup_i_fCorg_min:lookup_i_fCorg_max &
            & ),STAT=error)
       IF (error /= 0) THEN
          CALL sub_report_error( &
               & 'sedgem_data','sub_init_sed', &
               & 'Could not allocate space for CaCO3 diagenesis look-up table array', &
               & 'STOPPING', &
               & (/const_real_zero/),.TRUE. &
               & )
       ENDIF
       call sub_load_sed_dis_lookup_CaCO3()
    ENDIF
    ! allocate and populate lookup table vectors
    ! NOTE: check for problems allocating array space
    if (par_sed_diagen_CaCO3opt == 'ridgwell2001lookupvec') then
       ALLOCATE(lookup_vec_D(lookup_i_D_min:lookup_i_D_max),STAT=error)
       ALLOCATE(lookup_vec_dco3(lookup_i_dCO3_min:lookup_i_dCO3_max),STAT=error)
       ALLOCATE(lookup_vec_frac(lookup_i_frac_min:lookup_i_frac_max),STAT=error)
       ALLOCATE(lookup_vec_fCorg(lookup_i_fCorg_min:lookup_i_fCorg_max),STAT=error)
       IF (error /= 0) THEN
          CALL sub_report_error( &
               & 'sedgem_data','sub_init_sed', &
               & 'Could not allocate space for look-up table dimension vectors', &
               & 'STOPPING', &
               & (/const_real_zero/),.TRUE. &
               & )
       ENDIF
       lookup_vec_D     = (lookup_D_max/lookup_i_D_max)*(/ (n,n=lookup_i_D_min,lookup_i_D_max) /)
       lookup_vec_dco3  = (lookup_dCO3_max/lookup_i_dCO3_max)*(/ (n,n=lookup_i_dCO3_min,lookup_i_dCO3_max) /)
       lookup_vec_frac  = (lookup_frac_max/lookup_i_frac_max)*(/ (n,n=lookup_i_frac_min,lookup_i_frac_max) /)
       lookup_vec_fCorg = (lookup_fCorg_max/lookup_i_fCorg_max)*(/ (n,n=lookup_i_fCorg_min,lookup_i_fCorg_max) /)
    end if
    ! allocate size of look-up tables and load data -- CaCO3
    ! NOTE: check for problems allocating array space
    if (par_sed_diagen_opalopt == 'ridgwelletal2003lookup') then
       ALLOCATE(lookup_sed_dis_opal( &
            & lookup_i_opalpc_min:lookup_i_opalpc_max, &
            & lookup_i_concSi_min:lookup_i_concSi_max, &
            & lookup_i_T_min:lookup_i_T_max, &
            & lookup_i_KSi0_min:lookup_i_KSi0_max, &
            & lookup_i_opaltorefrac_min:lookup_i_opaltorefrac_max &
            & ),STAT=error)
       IF (error /= 0) THEN
          CALL sub_report_error( &
               & 'sedgem_data','sub_init_sed', &
               & 'Could not allocate space for opal diagenesis look-up table array', &
               & 'STOPPING', &
               & (/const_real_zero/),.TRUE. &
               & )
       ENDIF
       call sub_load_sed_dis_lookup_opal()
    ENDIF
    ! load and initialize neutral network
    if (par_sed_diagen_CaCO3opt == 'ridgwell2001nn') then
       call sub_init_neuralnetwork()
    end IF
    ! load alternative detrital flux field
    if (ctrl_sed_Fdet) then
       loc_filename = TRIM(par_indir_name)//TRIM(par_sed_Fdet_name)
       CALL sub_load_data_ij(loc_filename,n_i,n_j,loc_ij(:,:))
    else
       loc_ij(:,:) = 0.0
    endif
    sed_Fsed_det = loc_ij
    ! load alternative CaCO3 flux field
    if (ctrl_sed_Fcaco3) then
       loc_filename = TRIM(par_indir_name)//TRIM(par_sed_Fcaco3_name)
       CALL sub_load_data_ij(loc_filename,n_i,n_j,loc_ij(:,:))
    else
       loc_ij(:,:) = 0.0
    endif
    sed_Fsed_caco3 = loc_ij
    ! load alternative opal flux field
    if (ctrl_sed_Fopal) then
       loc_filename = TRIM(par_indir_name)//TRIM(par_sed_Fopal_name)
       CALL sub_load_data_ij(loc_filename,n_i,n_j,loc_ij(:,:))
    else
       loc_ij(:,:) = 0.0
    endif
    sed_Fsed_opal = loc_ij
    ! initialize diagnostics data array
    sed_diag(:,:,:) = 0.0
  END SUBROUTINE sub_init_sed
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CONFIGURE AND INITIALIZE SEDIMENT LAYERS
  ! NOTE: configured to initialze sediments with ash in the surface layer and detrital material throughout the stack
  SUBROUTINE sub_init_sed_layers_default()
    ! local variables
    INTEGER::i,j,o
    real::loc_sed_poros
    real::loc_sed_poros_top
    ! zero arrays
    sed(:,:,:,:)          = 0.0
    sed_top(:,:,:)        = 0.0
    sed_top_h(:,:)        = 0.0
    sed_top_INTdth(:,:)   = 0.0
    ! grid loop
    DO i=1,n_i
       DO j=1,n_j
          IF (sed_mask(i,j)) THEN
             ! set sediment porosity
             if (sed_mask_reef(i,j)) then
                loc_sed_poros = par_sed_poros_CaCO3_reef
                loc_sed_poros_top = par_sed_poros_CaCO3_reef
             elseif (sed_mask_muds(i,j)) then
                loc_sed_poros = par_sed_poros_det
                loc_sed_poros_top = fun_calc_sed_poros_nsur(0.0,par_sed_top_th)
             else
                loc_sed_poros = par_sed_poros_det
                loc_sed_poros_top = fun_calc_sed_poros_nsur(0.0,par_sed_top_th)
             endif
             ! set default sediment stack values
             ! NOTE: sediment component volumes are in the units of 
             !       actual volume of solid matter per cm2 area of sub-layer
             ! NOTE: the surface layer is initialized with ash to provide a constant sedimentation rate chronology
             sed_top(:,i,j)      = 0.0
             sed_top(is_ash,i,j) = 0.1*(1.0 - loc_sed_poros_top)*par_sed_top_th
             sed_top(is_det,i,j) = 0.9*(1.0 - loc_sed_poros_top)*par_sed_top_th
             if (sed_select(is_det_age)) sed_top(is_det_age,i,j) = par_misc_t_runtime*sed_top(is_det,i,j)
             DO o = 1,n_sed_tot_init
                sed(:,i,j,o)      = 0.0
                sed(is_det,i,j,o) = (1.0 - loc_sed_poros)*1.0
                if (sed_select(is_det_age)) sed(is_det_age,i,j,o) = par_misc_t_runtime*sed(is_det,i,j,o)
             END DO
             DO o = (n_sed_tot_init + 1),n_sed_tot
                sed(:,i,j,o) = 0.0
             END DO
          END if
       end DO
    END DO
    ! set height of top layer of old sediment
    DO i=1,n_i
       DO j=1,n_j
          IF (sed_mask(i,j)) THEN
             sed_top_h(i,j) = REAL(n_sed_tot_init)
          end IF
       end DO
    END DO

  END SUBROUTINE sub_init_sed_layers_default
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! LOAD SEDIMENT DIAGENESIS LOOK-UP TABLES - CACO3
  SUBROUTINE sub_load_sed_dis_lookup_CaCO3()
    USE genie_util, ONLY: check_unit, check_iostat
    ! local variables
    INTEGER::a,b,d,e
    CHARACTER(len=255)::loc_filename
    integer::ios ! for file checks
    ! *** read in calcite dissolution look-up data ***
    loc_filename = TRIM(par_indir_name)//'lookup_calcite_4.dat'
    call check_unit(in,__LINE__,__FILE__)
    OPEN(unit=in,file=loc_filename,action='read',iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    ! read in data
    DO a = lookup_i_D_min,lookup_i_D_max,1
       DO b = lookup_i_dCO3_min,lookup_i_dCO3_max,1
          DO d = lookup_i_frac_min,lookup_i_frac_max,1
             DO e = lookup_i_fCorg_min,lookup_i_fCorg_max,1
                READ(unit=in,FMT='(F7.3)',iostat=ios) lookup_sed_dis_cal(a,b,d,e)
                call check_iostat(ios,__LINE__,__FILE__)
             END DO
          END DO
       END DO
    END DO
    ! close file pipe
    CLOSE(unit=in,iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    ! change units from (umol cm-2 yr-1) to (mol cm-2 yr-1)
    lookup_sed_dis_cal(:,:,:,:) = conv_umol_mol*lookup_sed_dis_cal(:,:,:,:)
  END SUBROUTINE sub_load_sed_dis_lookup_CaCO3
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! LOAD SEDIMENT DIAGENESIS LOOK-UP TABLES - OPAL
  SUBROUTINE sub_load_sed_dis_lookup_opal()
    USE genie_util, ONLY: check_unit, check_iostat
    ! local variables
    INTEGER::a,b,c,d,e
    CHARACTER(len=255)::loc_filename
    integer::ios  ! for file checks
    ! *** read in opal dissolution look-up data ***
    loc_filename = TRIM(par_indir_name)//'lookup_opal_5.dat'
    call check_unit(in,__LINE__,__FILE__)
    OPEN(unit=in,file=loc_filename,action='read',iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    ! read in data
    DO a = lookup_i_opalpc_min,lookup_i_opalpc_max,1
       DO b = lookup_i_concSi_min,lookup_i_concSi_max,1
          DO c = lookup_i_T_min,lookup_i_T_max,1
             DO d = lookup_i_KSi0_min,lookup_i_KSi0_max,1
                DO e = lookup_i_opaltorefrac_min,lookup_i_opaltorefrac_max,1
                   READ(unit=in,FMT='(F7.3)',iostat=ios) lookup_sed_dis_opal(a,b,c,d,e)
                   call check_iostat(ios,__LINE__,__FILE__)
                END DO
             END DO
          END DO
       END DO
    END DO
    ! close file pipe
    CLOSE(unit=in,iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    ! change units from (umol cm-2 yr-1) to (mol cm-2 yr-1)
    lookup_sed_dis_opal(:,:,:,:,:) = conv_umol_mol*lookup_sed_dis_opal(:,:,:,:,:)
  END SUBROUTINE sub_load_sed_dis_lookup_opal
  ! ****************************************************************************************************************************** !


  ! ********************************************************************************************************************************
  ! CONFIGURE AND INITIALIZE NEURAL NETWORK
  SUBROUTINE sub_init_neuralnetwork()
!!$    real(kind=8) :: bias2
!!$    real(kind=8),dimension(par_nn_neurons) :: bias1
!!$    real(kind=8),dimension(par_nn_target,par_nn_neurons) :: wts2
!!$    real(kind=8),dimension(par_nn_neurons,par_nn_input)  :: wts1
!!$    character(50)  :: loc_name
!!$    INTEGER:: loc_iou, loc_ndims, loc_nvars
!!$    INTEGER,dimension(10)  :: loc_dimlen
!!$    INTEGER,dimension(10)  :: loc_vdims
!!$    INTEGER,dimension(2,20):: loc_varlen
!!$    character(20),dimension(10) :: loc_varname
!!$    call sub_nn_allocate_network()
!!$    loc_name = TRIM(par_indir_name)//'nn_calcite_4.nc'
!!$    call sub_openfile (loc_name, loc_iou)
!!$    call sub_inqdims (loc_name, loc_iou, loc_ndims, loc_nvars)
!!$    call sub_inqvars (loc_iou, loc_ndims, loc_nvars, loc_dimlen, loc_varname, &
!!$         & loc_vdims, loc_varlen)
!!$    call sub_getvar1d (loc_iou, loc_varname(1),loc_dimlen(loc_varlen(1,1)),nn_mint)
!!$    call sub_getvar1d (loc_iou, loc_varname(2),loc_dimlen(loc_varlen(1,2)),nn_maxt)
!!$    call sub_getvar1d (loc_iou, loc_varname(3),loc_dimlen(loc_varlen(1,3)),nn_maxp)
!!$    call sub_getvar1d (loc_iou, loc_varname(4),loc_dimlen(loc_varlen(1,4)),nn_minp)
!!$    call sub_getvar2d (loc_iou, loc_varname(5),loc_dimlen(loc_varlen(1,5)), &
!!$                  & loc_dimlen(loc_varlen(2,5)),w1)
!!$    call sub_getvar2d (loc_iou, loc_varname(6),loc_dimlen(loc_varlen(1,6)), &
!!$                  & loc_dimlen(loc_varlen(2,6)),w2)
!!$    call sub_getvar1d (loc_iou, loc_varname(7),loc_dimlen(loc_varlen(1,7)),b1)
!!$    call sub_getvar1d (loc_iou, loc_varname(8),loc_dimlen(loc_varlen(1,8)),b2)
!!$    call sub_closefile(loc_iou)
  END SUBROUTINE sub_init_neuralnetwork
  ! ********************************************************************************************************************************


  ! ****************************************************************************************************************************** !
  ! INITIALIZE SEDIMENT DATA SAVING
  ! NOTE: this mask sets the grid point locations where synthetic sediment 'cores' will saved, specified in the mask file by;
  !       1.0 = 'save here'
  !       0.0 = 'don't save here'
  !       (other values are not valid, or rather, could give rather unpredictable results ...)
  SUBROUTINE sub_init_sedgem_save_sed_data()
    ! local variables
    INTEGER::i,j
    integer::loc_len
    CHARACTER(len=255)::loc_filename
    REAL,DIMENSION(n_i,n_j)::loc_ij             ! 
    ! set alt dir path string length
    loc_len = LEN_TRIM(par_pindir_name)
    ! initialize variables
    loc_ij(:,:) = 0.0
    sed_save_mask(:,:) = .FALSE.
    ! load sediment sediment save mask
       if (loc_len > 0) then
    loc_filename = TRIM(par_pindir_name)//TRIM(par_sedcore_save_mask_name)
       else
    loc_filename = TRIM(par_indir_name)//TRIM(par_sedcore_save_mask_name)           
       endif
    CALL sub_load_data_ij(loc_filename,n_i,n_j,loc_ij(:,:))
    ! set sediment save mask
    DO i=1,n_i
       DO j=1,n_j
          if (loc_ij(i,j) < const_real_nullsmall) then
             sed_save_mask(i,j) = .FALSE.
          else
             sed_save_mask(i,j) = .TRUE.
          end if
       end do
    end do
  end SUBROUTINE sub_init_sedgem_save_sed_data
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INIT SAVE SEDCORE
  SUBROUTINE sub_sedgem_init_sedcoresenv()
    USE genie_util, ONLY: check_unit, check_iostat
    ! local variables
    INTEGER::i,j
    CHARACTER(len=255)::loc_filename
    integer::ios ! for file checks
    ! create a file and save header information for specified core locations
    DO i = 1,n_i
       DO j = 1,n_j
          if (sed_save_mask(i,j)) then
             loc_filename = TRIM(par_outdir_name)//'sedcoreenv_'// &
                  & fun_conv_num_char_n(2,i)//fun_conv_num_char_n(2,j)// &
                  & string_results_ext
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             write(unit=out,fmt='(A1,2A11,2A8,A8,A8,2A8,8A10,A10,A8,A10,A8,A10,5A10,12A10)',iostat=ios) &
                  & '%',                                          &
                  & '   time_kyr',                                &
                  & '  age_kyrBP',                                &
                  & '     lon',                                   &
                  & '     lat',                                   &
                  & '     D_m',                                   &
                  & '  k0_mix',                                   &
                  & '     T_C',                                   &
                  & '   S_mil',                                   &
                  & '    CO2_uM',                                 &
                  & '    ALK_uM',                                 &
                  & '    PO2_uM',                                 &
                  & '    NO3_uM',                                 &
                  & '     O2_uM',                                 &
                  & '     Ca_mM',                                 &
                  & '    SO4_mM',                                 &
                  & '   SiO2_uM',                                 &
                  & '      d13C',                                 &
                  & '  pH_SWS',                                   &
                  & '    CO3_uM',                                 &
                  & '     ohm',                                   &
                  & '   dCO3_uM',                                 &
                  & '     POC_%',                                 &
                  & '     cal_%',                                 &
                  & '      d13C',                                 &
                  & '    opal_%',                                 &
                  & '     det_%',                                 &
                  & '   fsedPOC',                                 &
                  & '      d13C',                                 &
                  & '   fsedcal',                                 &
                  & '      d13C',                                 &
                  & '  fsedopal',                                 &
                  & '   fseddet',                                 &
                  & '   fdisPOC',                                 &
                  & '      d13C',                                 &
                  & '   fdiscal',                                 &
                  & '      d13C',                                 &
                  & '  fdisopal',                                 &
                  & '   fdisdet'
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
          end if
       end DO
    end DO
  end SUBROUTINE sub_sedgem_init_sedcoresenv
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! SAVE SEDCORE ENVIRONMENT
  SUBROUTINE sub_sedgem_save_sedcoreenv( &
       & dum_dtyr,                       &
       & dum_i,dum_j,                    &
       & dum_sed,                        &
       & dum_sed_fsed,                   &
       & dum_sed_fdis,                   &
       & dum_ocn,                        &
       & dum_sed_carb                    &
       & )
    USE genie_util, ONLY: check_unit, check_iostat
    ! dummy variables
    REAL,INTENT(in)::dum_dtyr                                    ! time-step (years)
    integer,INTENT(in)::dum_i,dum_j                              ! 
    REAL,INTENT(in),DIMENSION(n_sed)::dum_sed                    ! 
    REAL,INTENT(in),DIMENSION(n_sed)::dum_sed_fsed,dum_sed_fdis  ! 
    real,intent(in),DIMENSION(n_ocn)::dum_ocn                    ! ocean composition
    REAL,INTENT(in),DIMENSION(n_carb)::dum_sed_carb              ! 
    ! local variables
    CHARACTER(len=255)::loc_filename                             !
    real::loc_age                                                !
    real::loc_sed_tot_wt                                         !
    REAL,DIMENSION(n_sed)::loc_sed                               !
    real::loc_ocn_DIC_d13C,loc_sed_CaCO3_d13C                                      !
    real::loc_sed_fsed_POC_d13C,loc_sed_fsed_CaCO3_d13C          !
    real::loc_sed_fdis_POC_d13C,loc_sed_fdis_CaCO3_d13C          !
    integer::ios                                                 ! file checks
    ! calculate sediment comcposition (weight fraction)
    loc_sed_tot_wt = fun_calc_sed_mass(dum_sed(:))
    IF (loc_sed_tot_wt > const_real_nullsmall) THEN
       loc_sed(:) = conv_sed_cm3_g(:)*dum_sed(:)/loc_sed_tot_wt
    end IF
    ! calculate sedimentation age
    IF (dum_sed_fsed(is_CaCO3) > const_real_nullsmall) THEN
       loc_age = dum_sed_fsed(is_CaCO3_age)/dum_sed_fsed(is_CaCO3)
    ELSE
       loc_age = 0.0
    ENDIF
    ! calculate local d13C
    ! NOTE: pass -999.999 rather than NaN values to fun_calc_isotope_delta and hence prevent overflow when writing out ASCII
    loc_ocn_DIC_d13C        = fun_calc_isotope_delta(dum_ocn(io_DIC),dum_ocn(io_DIC_13C),const_standards(11),.FALSE.,const_nulliso)
    loc_sed_CaCO3_d13C      = fun_calc_isotope_delta(loc_sed(is_CaCO3),loc_sed(is_CaCO3_13C),const_standards(11),.FALSE.,const_nulliso)
    loc_sed_fsed_POC_d13C   = fun_calc_isotope_delta(dum_sed_fsed(is_POC),dum_sed_fsed(is_POC_13C),const_standards(11),.FALSE.,const_nulliso)
    loc_sed_fsed_CaCO3_d13C = fun_calc_isotope_delta(dum_sed_fsed(is_CaCO3),dum_sed_fsed(is_CaCO3_13C),const_standards(11),.FALSE.,const_nulliso)
    loc_sed_fdis_POC_d13C   = fun_calc_isotope_delta(dum_sed_fdis(is_POC),dum_sed_fdis(is_POC_13C),const_standards(11),.FALSE.,const_nulliso)
    loc_sed_fdis_CaCO3_d13C = fun_calc_isotope_delta(dum_sed_fdis(is_CaCO3),dum_sed_fdis(is_CaCO3_13C),const_standards(11),.FALSE.,const_nulliso)
    ! re-open file and write (append) data
    loc_filename = TRIM(par_outdir_name)//'sedcoreenv_'// &
         & fun_conv_num_char_n(2,dum_i)//fun_conv_num_char_n(2,dum_j)// &
         & string_results_ext
    call check_unit(out,__LINE__,__FILE__)
    OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    write(unit=out,fmt='(1X,2f11.4,2f8.1,f8.1,f8.3,2f8.3,8f10.3,f10.3,f8.3,f10.3,f8.3,f10.3,5f10.3,12f10.3)',iostat=ios) &
         & conv_yr_kyr*(sed_time-0.5*dum_dtyr),                       &
         & conv_yr_kyr*loc_age,                                       &
         & phys_sed(ips_lon,dum_i,dum_j),                             &
         & phys_sed(ips_lat,dum_i,dum_j),                             &
         & phys_sed(ips_D,dum_i,dum_j),                               &
         & phys_sed(ips_mix_k0,dum_i,dum_j),                          &
         & dum_ocn(io_T) - const_zeroC,                               &
         & dum_ocn(io_S),                                             &
         & 1.0E+06*dum_ocn(io_DIC),                                   &
         & 1.0E+06*dum_ocn(io_ALK),                                   &
         & 1.0E+06*dum_ocn(io_PO4),                                   &
         & 1.0E+06*dum_ocn(io_NO3),                                   &
         & 1.0E+06*dum_ocn(io_O2),                                    &
         & 1.0E+03*dum_ocn(io_Ca),                                    &
         & 1.0E+03*dum_ocn(io_SO4),                                   &
         & 1.0E+06*dum_ocn(io_SiO2),                                  &
         & loc_ocn_DIC_d13C,                                          &
         & -log10(dum_sed_carb(ic_H)),                                &
         & 1.0E+06*dum_sed_carb(ic_conc_CO3),                         &
         & dum_sed_carb(ic_ohm_cal),                                  &
         & 1.0E+06*dum_sed_carb(ic_dCO3_cal),                         &
         & 100.0*loc_sed(is_POC),                                     &
         & 100.0*loc_sed(is_CaCO3),                                   &
         & loc_sed_CaCO3_d13C,                                        &
         & 100.0*loc_sed(is_opal),                                    &
         & 100.0*loc_sed(is_det),                                     &
         & 1.0E+06*dum_sed_fsed(is_POC)/dum_dtyr,                     &
         & loc_sed_fsed_POC_d13C,                                     &
         & 1.0E+06*dum_sed_fsed(is_CaCO3)/dum_dtyr,                   &
         & loc_sed_fsed_CaCO3_d13C,                                   &
         & 1.0E+06*dum_sed_fsed(is_opal)/dum_dtyr,                    &
         & 1.0E+06*dum_sed_fsed(is_det)/dum_dtyr,                     &
         & 1.0E+06*dum_sed_fdis(is_POC)/dum_dtyr,                     &
         & loc_sed_fdis_POC_d13C,                                     &
         & 1.0E+06*dum_sed_fdis(is_CaCO3)/dum_dtyr,                   &
         & loc_sed_fdis_CaCO3_d13C,                                   &
         & 1.0E+06*dum_sed_fdis(is_opal)/dum_dtyr,                    &
         & 1.0E+06*dum_sed_fdis(is_det)/dum_dtyr
    call check_iostat(ios,__LINE__,__FILE__)
    CLOSE(unit=out,iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
  end SUBROUTINE sub_sedgem_save_sedcoreenv
  ! ********************************************************************************************************************************


  ! ****************************************************************************************************************************** !
  ! LOAD IN SEDIMENT BIOTURBATIONAL MIXING PROFILE
  SUBROUTINE sub_load_sed_mix_k()
    USE genie_util, ONLY: check_unit, check_iostat
    ! local variables
    INTEGER::n
    INTEGER::loc_n_elements,loc_n_start
    CHARACTER(len=255)::loc_filename
    integer::ios  ! for file checks
    ! check file format
    loc_filename = TRIM(par_indir_name)//trim(par_sed_mix_k_name)
    CALL sub_check_fileformat(loc_filename,loc_n_elements,loc_n_start)
    ! set maximum number of sediment layers to bioturbate and therefore array size
    par_n_sed_mix = loc_n_elements - 1
    ALLOCATE(par_sed_mix_k(0:par_n_sed_mix),STAT=error)
    ! open file pipe
    call check_unit(in,__LINE__,__FILE__)
    OPEN(unit=in,file=loc_filename,action='read',iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    ! goto start-of-file tag
    DO n = 1,loc_n_start
       READ(unit=in,fmt='(1X)',iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
    END DO
    ! read mixed-layer sediment mixing profile (top down)
    ! NOTE: the mixing rate as measured the depth of the top (incomplete stack layer) surface of the sediment stack
    !       has a value equal to the first bioturbation rate value read in, and corresponds to an index value of 'par_n_sed_mix'
    ! NOTE: mixing rate units from data file are (cm2 yr-1)
    ! NOTE: the bottom value in the mixing rate should be zero to properly terminate the profile
    DO n = par_n_sed_mix,0,-1
       READ(unit=in,FMT=*,iostat=ios) par_sed_mix_k(n)
       call check_iostat(ios,__LINE__,__FILE__)
    END DO
    CLOSE(unit=in,iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    ! convert the normalized mixing profile loaded in to a profile of biodiffusion rates
    ! accodring to the parameter par_sed_mix_kmax set in sedgem_config.par
    par_sed_mix_k(:) = par_sed_mix_k_sur_max*par_sed_mix_k(:)
    ! check that the maximum mixing rate in the profile does not exceed the maximum rate 
    ! that can be accomodated by the mixing algorithm (assuming 1 cm stack layer spacing)
    IF (MAXVAL(par_sed_mix_k(:)) > 0.5) THEN
       CALL sub_report_error( &
            & 'sedgem_data','sub_load_sed_mix_k', &
            & 'mixing time-step weighted sediment mixing rate is too large; '//&
            & 'maximum mixing rate in profile (cm2 yr-1) = ', &
            & 'STOPPING', &
            & (/MAXVAL(par_sed_mix_k(:))/),.true. &
            & )
    ENDIF
  END SUBROUTINE sub_load_sed_mix_k
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! SAVE SEDIMENT DIAGNOSTICS DATA
  SUBROUTINE sub_data_save_seddiag_GLOBAL(dum_dtyr,dum_sfcsumocn)
    USE genie_util, ONLY: check_unit, check_iostat
    ! dummy valiables
    real,INTENT(in)::dum_dtyr                                  ! 
    real,DIMENSION(n_ocn,n_i,n_j),intent(in)::dum_sfcsumocn    ! 
    ! local variables
    INTEGER::i,j,l,is 
    integer::ios  ! for file checks
    CHARACTER(len=255)::loc_filename
    REAL,DIMENSION(n_sed,n_i,n_j)::loc_sed_coretop
    REAL,DIMENSION(n_sed,n_i,n_j)::loc_sed_preservation
    real::loc_tot1_sedgrid,loc_tot1a_sedgrid,loc_tot1b_sedgrid
    real::loc_tot2_sedgrid,loc_tot2a_sedgrid,loc_tot2b_sedgrid
    real::loc_pres_sedgrid
    real::loc_rain_sedgrid
    REAL,DIMENSION(n_sed,n_i,n_j)::loc_fsed                    ! 
    REAL,DIMENSION(n_sed,n_i,n_j)::loc_fdis                    ! 
    real::loc_mean_sedgrid                                     ! 
    real::loc_tot_mask_area                                    ! 
    real::loc_sed_d13C_mean                                    ! 
    real::loc_dt                                               ! local time-step for data saving (and averaging)
    REAL,DIMENSION(n_i,n_j)::loc_area                          ! local area (cm2)
    REAL,DIMENSION(n_i,n_j)::loc_mask                          ! local sediment (total) mask (copy)
    REAL,DIMENSION(n_i,n_j)::loc_mask_reef,loc_mask_muds       ! local reef, shallow sediment masks (copy)
    REAL,DIMENSION(n_i,n_j)::loc_mask_dsea                     ! local deep-sea sediment mask (derived variable)
    real::loc_tot,loc_frac,loc_standard,loc_sig                ! 
    REAL,DIMENSION(n_i,n_j)::loc_CaCO3_d13C,loc_POC_d13C       ! 

    ! *** INITIALIZE LOCAL VARIABLES ***
    ! averaging time-step
    loc_dt = 2.0*dum_dtyr
    ! area (units: cm2)
    loc_area(:,:) = conv_m2_cm2*phys_sed(ips_A,:,:)
    ! masks
    loc_mask(:,:)      = phys_sed(ips_mask_sed,:,:)
    loc_mask_reef(:,:) = phys_sed(ips_mask_sed_reef,:,:)
    loc_mask_muds(:,:) = phys_sed(ips_mask_sed_muds,:,:)
    loc_mask_dsea(:,:) = phys_sed(ips_mask_sed,:,:)*(1.0 - loc_mask_reef(:,:))*(1.0 - loc_mask_muds(:,:))
    ! calculate core-top sediment composition data
    loc_sed_coretop(:,:,:) = fun_sed_coretop()
    ! mean (last 2 time-step averaged) sediemnt and dissolution
    loc_fsed(:,:,:) = (sed_fsed(:,:,:) + sed_fsed_OLD(:,:,:))/loc_dt
    loc_fdis(:,:,:) = (sed_fdis(:,:,:) + sed_fdis_OLD(:,:,:))/loc_dt
    ! calculate local sediment preservation (normalized fraction)
    DO l=1,n_l_sed
       is = conv_iselected_is(l)
       DO i=1,n_i
          DO j=1,n_j
             IF (loc_fsed(is,i,j) > const_real_nullsmall) THEN
                loc_sed_preservation(is,i,j) = (loc_fsed(is,i,j) - loc_fdis(is,i,j))/loc_fsed(is,i,j)
             else
                loc_sed_preservation(is,i,j) = 0.0
             end if
          end do
       end do
    end do
    ! calculate d13C
    DO i=1,n_i
       DO j=1,n_j
          if (loc_mask_reef(i,j)*loc_fsed(is_CaCO3,i,j) > const_real_nullsmall) then
             loc_tot  = loc_fsed(sed_dep(is_CaCO3_13C),i,j)
             loc_frac = loc_fsed(is_CaCO3_13C,i,j)
             loc_standard = const_standards(sed_type(is_CaCO3_13C))
             loc_CaCO3_d13C(i,j) = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_nulliso)
          else
             loc_CaCO3_d13C(i,j) = 0.0
          end if
          if (loc_mask_reef(i,j)*loc_fsed(is_POC,i,j) > const_real_nullsmall) then
             loc_tot  = loc_fsed(sed_dep(is_POC_13C),i,j)
             loc_frac = loc_fsed(is_POC_13C,i,j)
             loc_standard = const_standards(sed_type(is_POC_13C))
             loc_POC_d13C(i,j) = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_nulliso)
          else
             loc_POC_d13C(i,j) = 0.0
          end if
       end do
    end do

    ! *** SAVE GLOBAL SUMMARY DATA ***
    ! set filename
    IF (ctrl_timeseries_output) THEN
       loc_filename = TRIM(par_outdir_name)//'seddiag_misc_DATA_GLOBAL_'//year_text//string_results_ext
    ELSE
       loc_filename = TRIM(par_outdir_name)//'seddiag_misc_DATA_GLOBAL'//string_results_ext
    ENDIF
    ! open file
    call check_unit(out,__LINE__,__FILE__)
    OPEN(out,file=TRIM(loc_filename),action='write',iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)

    ! HEADER
    Write(unit=out,fmt=*) '================================='
    Write(unit=out,fmt=*) '=== GLOBAL SEDIMENT DIAG DATA ==='
    Write(unit=out,fmt=*) '================================='

    ! DIAGNOSTICS ON SEDIMENT GRID
    Write(unit=out,fmt=*) ' '
    Write(unit=out,fmt=*) '--- DEEP-SEA SEDIMENT GRID ------'
    Write(unit=out,fmt=*) ' '
    ! MISC
    Write(unit=out,fmt=*) '---------------------------------'
    write(unit=out,fmt='(A28,I6)',iostat=ios) &
         & ' Total # deep-sea grid pts :',int(sum(loc_mask_dsea(:,:)))
    call check_iostat(ios,__LINE__,__FILE__)
    write(unit=out,fmt='(A28,e14.6,A3)',iostat=ios) &
         & ' Total deep-sea area       :',sum(loc_mask_dsea(:,:)*phys_sed(ips_A,:,:)),'m2'
    call check_iostat(ios,__LINE__,__FILE__)
    ! local variables 
    loc_tot_mask_area = sum(loc_mask_dsea(:,:)*loc_area(:,:))
    ! POC
    loc_tot1_sedgrid = sum(loc_mask_dsea(:,:)*loc_area(:,:)*loc_fsed(is_POC,:,:))
    loc_tot2_sedgrid = sum(loc_mask_dsea(:,:)*loc_area(:,:)*loc_fdis(is_POC,:,:))
    if (abs(loc_tot1_sedgrid) > const_real_nullsmall) then 
       loc_pres_sedgrid = 100.0*(loc_tot1_sedgrid - loc_tot2_sedgrid)/loc_tot1_sedgrid
    else
       loc_pres_sedgrid = 0.0
    end if
    if (loc_tot_mask_area > const_real_nullsmall) then 
       loc_mean_sedgrid = sum(loc_mask_dsea(:,:)*loc_sed_coretop(is_POC,:,:)*loc_area(:,:))/loc_tot_mask_area
    else
       loc_mean_sedgrid = 0.0
    end if
    Write(unit=out,fmt=*) '---------------------------------'
    write(unit=out,fmt='(A28,e14.6,A12,f7.3,A9)',iostat=ios) &
         & ' POC rain                  :',loc_tot1_sedgrid,' mol yr-1 = ',1.0E-12*conv_C_mol_kg*loc_tot1_sedgrid,' GtC yr-1'
    call check_iostat(ios,__LINE__,__FILE__)
    write(unit=out,fmt='(A28,e14.6,A12,f7.3,A9)',iostat=ios) &
         & ' POC diss                  :',loc_tot2_sedgrid,' mol yr-1 = ',1.0E-12*conv_C_mol_kg*loc_tot2_sedgrid,' GtC yr-1'
    call check_iostat(ios,__LINE__,__FILE__)
    write(unit=out,fmt='(A28,e14.6,A12,f7.3,A9,A6,f6.2,A2)',iostat=ios) &
         & ' Total POC pres            :',loc_tot1_sedgrid - loc_tot2_sedgrid,' mol yr-1 = ', &
         & 1.0E-12*conv_C_mol_kg*(loc_tot1_sedgrid - loc_tot2_sedgrid),' GtC yr-1','   =  ',loc_pres_sedgrid,' %'
    call check_iostat(ios,__LINE__,__FILE__)
    write(unit=out,fmt='(A28,f6.2,A2)',iostat=ios) &
         & ' Mean wt% POC              :',loc_mean_sedgrid,' %'
    call check_iostat(ios,__LINE__,__FILE__)
    ! CaCO3
    loc_tot1_sedgrid = sum(loc_mask_dsea(:,:)*loc_area(:,:)*loc_fsed(is_CaCO3,:,:))
    loc_tot2_sedgrid = sum(loc_mask_dsea(:,:)*loc_area(:,:)*loc_fdis(is_CaCO3,:,:))
    if (abs(loc_tot1_sedgrid) > const_real_nullsmall) then 
       loc_pres_sedgrid = 100.0*(loc_tot1_sedgrid - loc_tot2_sedgrid)/loc_tot1_sedgrid
    else
       loc_pres_sedgrid = 0.0
    end if
    if (loc_tot_mask_area > const_real_nullsmall) then 
       loc_mean_sedgrid = sum(loc_mask_dsea(:,:)*loc_sed_coretop(is_CaCO3,:,:)*loc_area(:,:))/loc_tot_mask_area
    else
       loc_mean_sedgrid = 0.0
    end if
    Write(unit=out,fmt=*) '---------------------------------'
    write(unit=out,fmt='(A28,e14.6,A12,f7.3,A9)',iostat=ios) &
         & ' CaCO3 rain                :',loc_tot1_sedgrid,' mol yr-1 = ',1.0E-12*conv_CaCO3_mol_kgC*loc_tot1_sedgrid,' GtC yr-1'
    call check_iostat(ios,__LINE__,__FILE__)
    write(unit=out,fmt='(A28,e14.6,A12,f7.3,A9)',iostat=ios) &
         & ' CaCO3 diss                :',loc_tot2_sedgrid,' mol yr-1 = ',1.0E-12*conv_CaCO3_mol_kgC*loc_tot2_sedgrid,' GtC yr-1'
    call check_iostat(ios,__LINE__,__FILE__)
    write(unit=out,fmt='(A28,e14.6,A12,f7.3,A9,A6,f6.2,A2)',iostat=ios) &
         & ' Total CaCO3 pres          :',loc_tot1_sedgrid - loc_tot2_sedgrid,' mol yr-1 = ', &
         & 1.0E-12*conv_CaCO3_mol_kgC*(loc_tot1_sedgrid - loc_tot2_sedgrid),' GtC yr-1','   =  ',loc_pres_sedgrid,' %'
    call check_iostat(ios,__LINE__,__FILE__)
    write(unit=out,fmt='(A28,f6.2,A2)',iostat=ios) &
         & ' Mean wt% CaCO3            :',loc_mean_sedgrid,' %'
    call check_iostat(ios,__LINE__,__FILE__)
    ! CaCO3:POC
    loc_tot1_sedgrid = SUM(loc_mask_dsea(:,:)*loc_fsed(is_POC,:,:))
    loc_tot2_sedgrid = SUM(loc_mask_dsea(:,:)*loc_fsed(is_CaCO3,:,:))
    if (abs(loc_tot1_sedgrid) > const_real_nullsmall) then 
       loc_rain_sedgrid = loc_tot2_sedgrid/loc_tot1_sedgrid
    else
       loc_rain_sedgrid = 0.0
    end if
    Write(unit=out,fmt=*) '---------------------------------'
    write(unit=out,fmt='(A28,f7.3)',iostat=ios) &
         & ' CaCO3/POC rain ratio      :',loc_rain_sedgrid
    call check_iostat(ios,__LINE__,__FILE__)
    ! opal
    loc_tot1_sedgrid = sum(loc_mask_dsea(:,:)*loc_area(:,:)*loc_fsed(is_opal,:,:))
    loc_tot2_sedgrid = sum(loc_mask_dsea(:,:)*loc_area(:,:)*loc_fdis(is_opal,:,:))
    if (abs(loc_tot1_sedgrid) > 0.0) then 
       loc_pres_sedgrid = 100.0*(loc_tot1_sedgrid - loc_tot2_sedgrid)/loc_tot1_sedgrid
    else
       loc_pres_sedgrid = 0.0
    end if
    if (loc_tot_mask_area > const_real_nullsmall) then 
       loc_mean_sedgrid = sum(loc_mask_dsea(:,:)*loc_sed_coretop(is_opal,:,:)*loc_area(:,:))/loc_tot_mask_area
    else
       loc_mean_sedgrid = 0.0
    end if
    Write(unit=out,fmt=*) '---------------------------------'
    write(unit=out,fmt='(A28,e14.6,A12,f7.3,A9)',iostat=ios) &
         & ' opal rain                 :',loc_tot1_sedgrid,' mol yr-1'
    call check_iostat(ios,__LINE__,__FILE__)
    write(unit=out,fmt='(A28,e14.6,A12,f7.3,A9)',iostat=ios) &
         & ' opal diss                 :',loc_tot2_sedgrid,' mol yr-1'
    call check_iostat(ios,__LINE__,__FILE__)
    write(unit=out,fmt='(A28,e14.6,A12,f6.2,A2)',iostat=ios) &
         & ' Total opal pres           :',loc_tot1_sedgrid - loc_tot2_sedgrid,' mol yr-1 = ',loc_pres_sedgrid,' %'
    call check_iostat(ios,__LINE__,__FILE__)
    write(unit=out,fmt='(A28,f6.2,A2)',iostat=ios) &
         & ' Mean wt% opal             :',loc_mean_sedgrid,' %'
    call check_iostat(ios,__LINE__,__FILE__)
    ! Li
    if (sed_select(is_LiCO3) .AND. sed_select(is_detLi)) then
       Write(unit=out,fmt=*) '---------------------------------'
       loc_tot1_sedgrid = sum(loc_mask_dsea(:,:)*loc_area(:,:)*loc_fsed(is_LiCO3,:,:))
       loc_tot2_sedgrid = sum(loc_mask_dsea(:,:)*loc_area(:,:)*loc_fdis(is_LiCO3,:,:))
       write(unit=out,fmt='(A28,e14.6,A12,f7.3,A9)',iostat=ios) &
            & ' Li CaCO3 sink             :',loc_tot1_sedgrid,' mol yr-1'
       call check_iostat(ios,__LINE__,__FILE__)
       write(unit=out,fmt='(A28,e14.6,A12,f7.3,A9)',iostat=ios) &
            & ' Li CaCO3 source           :',loc_tot2_sedgrid,' mol yr-1'
       call check_iostat(ios,__LINE__,__FILE__)
       loc_tot1_sedgrid = sum(loc_mask_dsea(:,:)*loc_area(:,:)*loc_fsed(is_detLi,:,:))
       loc_tot2_sedgrid = sum(loc_mask_dsea(:,:)*loc_area(:,:)*loc_fdis(is_detLi,:,:))
       write(unit=out,fmt='(A28,e14.6,A12,f7.3,A9)',iostat=ios) &
            & ' Li detrital sink          :',loc_tot1_sedgrid,' mol yr-1'
       call check_iostat(ios,__LINE__,__FILE__)
       write(unit=out,fmt='(A28,e14.6,A12,f7.3,A9)',iostat=ios) &
            & ' Li detrital source        :',loc_tot2_sedgrid,' mol yr-1'
       call check_iostat(ios,__LINE__,__FILE__)
    end if
    Write(unit=out,fmt=*) '---------------------------------'

    ! CORAL REEF DIAGNOSTICS
    Write(unit=out,fmt=*) ' '
    Write(unit=out,fmt=*) '--- REEF SEDIMENT GRID ----------'
    Write(unit=out,fmt=*) ' '
    ! MISC
    Write(unit=out,fmt=*) '---------------------------------'
    write(unit=out,fmt='(A28,I6)',iostat=ios) &
         & ' Total # reef grid pts     :',int(sum(loc_mask_reef(:,:)))
    call check_iostat(ios,__LINE__,__FILE__)
    write(unit=out,fmt='(A28,e14.6,A3)',iostat=ios) &
         & ' Total reef area           :',sum(loc_mask_reef(:,:)*phys_sed(ips_A,:,:)),' m2'
    call check_iostat(ios,__LINE__,__FILE__)
    Write(unit=out,fmt=*) '---------------------------------'
    ! local variables 
    loc_tot_mask_area = sum(loc_mask_reef(:,:)*loc_area(:,:))
    ! CaCO3
    loc_tot1_sedgrid = sum(loc_mask_reef(:,:)*loc_area(:,:)*loc_fsed(is_CaCO3,:,:))
    loc_tot2_sedgrid = sum(loc_mask_reef(:,:)*loc_area(:,:)*loc_fdis(is_CaCO3,:,:))
    if (abs(loc_tot1_sedgrid) > const_real_nullsmall) then 
       loc_pres_sedgrid = 100.0*(loc_tot1_sedgrid - loc_tot2_sedgrid)/loc_tot1_sedgrid
    else
       loc_pres_sedgrid = 0.0
    end if
    if (loc_tot_mask_area > const_real_nullsmall) then 
       loc_mean_sedgrid = sum(loc_mask_reef(:,:)*loc_sed_coretop(is_CaCO3,:,:)*loc_area(:,:))/loc_tot_mask_area
    else
       loc_mean_sedgrid = 0.0
    end if
    write(unit=out,fmt='(A28,e14.6,A12,f7.3,A9)',iostat=ios) &
         & ' CaCO3 production          :',loc_tot1_sedgrid,' mol yr-1 = ',1.0E-12*conv_CaCO3_mol_kgC*loc_tot1_sedgrid,' GtC yr-1'
    call check_iostat(ios,__LINE__,__FILE__)
    write(unit=out,fmt='(A28,f6.2,A2)',iostat=ios) &
         & ' Mean wt% CaCO3            :',loc_mean_sedgrid,' %'
    call check_iostat(ios,__LINE__,__FILE__)
    Write(unit=out,fmt=*) '---------------------------------'
    ! d13C (weighted by area and CaCO3 sedimentation rate)
    if (sum(loc_mask_reef(:,:)*loc_area(:,:)*loc_fsed(is_CaCO3,:,:)) > const_real_nullsmall) then
       loc_sed_d13C_mean = &
            & sum(loc_mask_reef(:,:)*loc_area(:,:)*loc_fsed(is_CaCO3,:,:)*loc_CaCO3_d13C(:,:))/ &
            & sum(loc_mask_reef(:,:)*loc_area(:,:)*loc_fsed(is_CaCO3,:,:))
    else
       loc_sed_d13C_mean = 0.0
    end if
    write(unit=out,fmt='(A28,f6.2,A5)',iostat=ios) &
         & ' Mean weighted d13C CaCO3  :',loc_sed_d13C_mean,'o/oo'
    Write(unit=out,fmt=*) '---------------------------------'
    ! Li
    if (sed_select(is_LiCO3) .AND. sed_select(is_detLi)) then
       loc_tot1_sedgrid = sum(loc_mask_reef(:,:)*loc_area(:,:)*loc_fsed(is_LiCO3,:,:))
       loc_tot2_sedgrid = sum(loc_mask_reef(:,:)*loc_area(:,:)*loc_fdis(is_LiCO3,:,:))
       write(unit=out,fmt='(A28,e14.6,A12,f7.3,A9)',iostat=ios) &
            & ' Li CaCO3 sink             :',loc_tot1_sedgrid,' mol yr-1'
       call check_iostat(ios,__LINE__,__FILE__)
       write(unit=out,fmt='(A28,e14.6,A12,f7.3,A9)',iostat=ios) &
            & ' Li CaCO3 source           :',loc_tot2_sedgrid,' mol yr-1'
       call check_iostat(ios,__LINE__,__FILE__)
       loc_tot1_sedgrid = sum(loc_mask_reef(:,:)*loc_area(:,:)*loc_fsed(is_detLi,:,:))
       loc_tot2_sedgrid = sum(loc_mask_reef(:,:)*loc_area(:,:)*loc_fdis(is_detLi,:,:))
       write(unit=out,fmt='(A28,e14.6,A12,f7.3,A9)',iostat=ios) &
            & ' Li detrital sink          :',loc_tot1_sedgrid,' mol yr-1'
       call check_iostat(ios,__LINE__,__FILE__)
       write(unit=out,fmt='(A28,e14.6,A12,f7.3,A9)',iostat=ios) &
            & ' Li detrital source        :',loc_tot2_sedgrid,' mol yr-1'
       call check_iostat(ios,__LINE__,__FILE__)
    end if
    ! Corg
    if (par_sed_Corgburial > const_real_nullsmall) then
       loc_tot1_sedgrid = sum(loc_mask_reef(:,:)*loc_area(:,:)*loc_fsed(is_POC,:,:))
       if (loc_tot_mask_area > const_real_nullsmall) then 
          loc_mean_sedgrid = sum(loc_mask_reef(:,:)*loc_sed_coretop(is_POC,:,:)*loc_area(:,:))/loc_tot_mask_area
       else
          loc_mean_sedgrid = 0.0
       end if
       Write(unit=out,fmt=*) '---------------------------------'
       write(unit=out,fmt='(A28,e14.6,A12,f7.3,A9)',iostat=ios) &
            & ' POC production            :',loc_tot1_sedgrid,' mol yr-1 = ',1.0E-12*conv_C_mol_kg*loc_tot1_sedgrid,' GtC yr-1'
       call check_iostat(ios,__LINE__,__FILE__)
       write(unit=out,fmt='(A28,f6.2,A2)',iostat=ios) &
            & ' Mean wt% POC              :',loc_mean_sedgrid,' %'
       call check_iostat(ios,__LINE__,__FILE__)
       Write(unit=out,fmt=*) '---------------------------------'
       ! d13C (weighted by area and POC sedimentation rate)
       if (sum(loc_mask_reef(:,:)*loc_area(:,:)*loc_fsed(is_POC,:,:)) > const_real_nullsmall) then
          loc_sed_d13C_mean = &
               & sum(loc_mask_reef(:,:)*loc_area(:,:)*loc_fsed(is_POC,:,:)*loc_POC_d13C(:,:))/ &
               & sum(loc_mask_reef(:,:)*loc_area(:,:)*loc_fsed(is_POC,:,:))
       else
          loc_sed_d13C_mean = 0.0
       end if
       write(unit=out,fmt='(A28,f6.2,A5)',iostat=ios) &
            & ' Mean weighted d13C CaCO3  :',loc_sed_d13C_mean,'o/oo'
    end if
    ! Sr
    IF (sed_select(is_SrCO3_87Sr) .AND. sed_select(is_SrCO3_88Sr)) THEN
       ! local variables
       loc_tot1_sedgrid  = sum(loc_mask_reef(:,:)*loc_area(:,:)*loc_fsed(is_SrCO3,:,:))
       loc_tot1a_sedgrid = sum(loc_mask_reef(:,:)*loc_area(:,:)*loc_fsed(is_SrCO3_87Sr,:,:))
       loc_tot1b_sedgrid = sum(loc_mask_reef(:,:)*loc_area(:,:)*loc_fsed(is_SrCO3_88Sr,:,:))
       loc_tot2_sedgrid  = sum(loc_mask_reef(:,:)*loc_area(:,:)*loc_fdis(is_SrCO3,:,:))
       loc_tot2a_sedgrid = sum(loc_mask_reef(:,:)*loc_area(:,:)*loc_fdis(is_SrCO3_87Sr,:,:))
       loc_tot2b_sedgrid = sum(loc_mask_reef(:,:)*loc_area(:,:)*loc_fdis(is_SrCO3_88Sr,:,:))
       ! bulk Sr
       write(unit=out,fmt='(A28,e14.6,A12,f7.3,A9)',iostat=ios) &
            & ' SrCO3 sink                :',loc_tot1_sedgrid,' mol yr-1'
       write(unit=out,fmt='(A28,e14.6,A12,f7.3,A9)',iostat=ios) &
            & ' Sr source                 :',loc_tot2_sedgrid,' mol yr-1'
       ! 87Sr
       loc_tot = loc_tot1_sedgrid-loc_tot1a_sedgrid-loc_tot1b_sedgrid
       if (loc_tot > const_real_nullsmall) then
          loc_sig = loc_tot1a_sedgrid/loc_tot
       else
          loc_sig = 0.0
       end if
       write(unit=out,fmt='(A28,f10.6)',iostat=ios) &
            & ' SrCO3 sink -- 87Sr        :',loc_sig
       loc_tot = loc_tot2_sedgrid-loc_tot2a_sedgrid-loc_tot2b_sedgrid
       if (loc_tot > const_real_nullsmall) then
          loc_sig = loc_tot2a_sedgrid/loc_tot
       else
          loc_sig = 0.0
       end if
       write(unit=out,fmt='(A28,f10.6)',iostat=ios) &
            & ' Sr source -- 87Sr         :',loc_sig
       ! 88Sr
       loc_tot = loc_tot1_sedgrid-loc_tot1a_sedgrid-loc_tot1b_sedgrid
       if (loc_tot > const_real_nullsmall) then
          loc_frac     = loc_tot1b_sedgrid
          loc_standard = const_standardsR(ocn_type(io_Sr_88Sr))
          loc_sig      = fun_calc_isotope_deltaR(loc_tot,loc_frac,loc_standard,const_real_null)
       else
          loc_sig = -999.9
       end if
       write(unit=out,fmt='(A28,f10.2,A5)',iostat=ios) &
            & ' SrCO3 sink -- 88Sr        :',loc_sig,' o/oo'
       loc_tot  = loc_tot2_sedgrid-loc_tot2a_sedgrid-loc_tot2b_sedgrid
       if (abs(loc_tot) > const_real_nullsmall) then
          loc_frac     = loc_tot2b_sedgrid
          loc_standard = const_standardsR(ocn_type(io_Sr_88Sr))
          loc_sig      = fun_calc_isotope_deltaR(loc_tot,loc_frac,loc_standard,const_real_null)
       else
          loc_sig = -999.9
       end if
       write(unit=out,fmt='(A28,f10.2,A5)',iostat=ios) &
            & ' Sr source -- 88Sr         :',loc_sig,' o/oo'
    end if
    Write(unit=out,fmt=*) '---------------------------------'

    ! SHALLOW WATER SEDIMENTS DIAGNOSTICS
    Write(unit=out,fmt=*) ' '
    Write(unit=out,fmt=*) '--- SHALLOW SEDIMENT GRID -------'
    Write(unit=out,fmt=*) ' '
    ! MISC
    Write(unit=out,fmt=*) '---------------------------------'
    write(unit=out,fmt='(A28,I6)',iostat=ios) &
         & ' Total # grid pts          :',int(sum(loc_mask_muds(:,:)))
    call check_iostat(ios,__LINE__,__FILE__)
    write(unit=out,fmt='(A28,e14.6,A3)',iostat=ios) &
         & ' Total area                :',sum(loc_mask_muds(:,:)*phys_sed(ips_A,:,:)),'m2'
    call check_iostat(ios,__LINE__,__FILE__)
    ! local variables 
    loc_tot_mask_area = sum(loc_mask_muds(:,:)*loc_area(:,:))
    ! POC
    loc_tot1_sedgrid = sum(loc_mask_muds(:,:)*loc_area(:,:)*loc_fsed(is_POC,:,:))
    loc_tot2_sedgrid = sum(loc_mask_muds(:,:)*loc_area(:,:)*loc_fdis(is_POC,:,:))
    if (abs(loc_tot1_sedgrid) > const_real_nullsmall) then 
       loc_pres_sedgrid = 100.0*(loc_tot1_sedgrid - loc_tot2_sedgrid)/loc_tot1_sedgrid
    else
       loc_pres_sedgrid = 0.0
    end if
    if (loc_tot_mask_area > const_real_nullsmall) then 
       loc_mean_sedgrid = sum(loc_mask_muds(:,:)*loc_sed_coretop(is_POC,:,:)*loc_area(:,:))/loc_tot_mask_area
    else
       loc_mean_sedgrid = 0.0
    end if
    Write(unit=out,fmt=*) '---------------------------------'
    write(unit=out,fmt='(A28,e14.6,A12,f7.3,A9)',iostat=ios) &
         & ' POC rain                  :',loc_tot1_sedgrid,' mol yr-1 = ',1.0E-12*conv_C_mol_kg*loc_tot1_sedgrid,' GtC yr-1'
    call check_iostat(ios,__LINE__,__FILE__)
    write(unit=out,fmt='(A28,e14.6,A12,f7.3,A9)',iostat=ios) &
         & ' POC diss                  :',loc_tot2_sedgrid,' mol yr-1 = ',1.0E-12*conv_C_mol_kg*loc_tot2_sedgrid,' GtC yr-1'
    call check_iostat(ios,__LINE__,__FILE__)
    write(unit=out,fmt='(A28,e14.6,A12,f7.3,A9,A6,f6.2,A2)',iostat=ios) &
         & ' Total POC pres            :',loc_tot1_sedgrid - loc_tot2_sedgrid,' mol yr-1 = ', &
         & 1.0E-12*conv_C_mol_kg*(loc_tot1_sedgrid - loc_tot2_sedgrid),' GtC yr-1','   =  ',loc_pres_sedgrid,' %'
    call check_iostat(ios,__LINE__,__FILE__)
    write(unit=out,fmt='(A28,f6.2,A2)',iostat=ios) &
         & ' Mean wt% POC              :',loc_mean_sedgrid,' %'
    call check_iostat(ios,__LINE__,__FILE__)
    ! CaCO3
    loc_tot1_sedgrid = sum(loc_mask_muds(:,:)*loc_area(:,:)*loc_fsed(is_CaCO3,:,:))
    loc_tot2_sedgrid = sum(loc_mask_muds(:,:)*loc_area(:,:)*loc_fdis(is_CaCO3,:,:))
    if (abs(loc_tot1_sedgrid) > const_real_nullsmall) then 
       loc_pres_sedgrid = 100.0*(loc_tot1_sedgrid - loc_tot2_sedgrid)/loc_tot1_sedgrid
    else
       loc_pres_sedgrid = 0.0
    end if
    if (loc_tot_mask_area > const_real_nullsmall) then 
       loc_mean_sedgrid = sum(loc_mask_muds(:,:)*loc_sed_coretop(is_CaCO3,:,:)*loc_area(:,:))/loc_tot_mask_area
    else
       loc_mean_sedgrid = 0.0
    end if
    Write(unit=out,fmt=*) '---------------------------------'
    write(unit=out,fmt='(A28,e14.6,A12,f7.3,A9)',iostat=ios) &
         & ' CaCO3 rain                :',loc_tot1_sedgrid,' mol yr-1 = ',1.0E-12*conv_CaCO3_mol_kgC*loc_tot1_sedgrid,' GtC yr-1'
    call check_iostat(ios,__LINE__,__FILE__)
    write(unit=out,fmt='(A28,e14.6,A12,f7.3,A9)',iostat=ios) &
         & ' CaCO3 diss                :',loc_tot2_sedgrid,' mol yr-1 = ',1.0E-12*conv_CaCO3_mol_kgC*loc_tot2_sedgrid,' GtC yr-1'
    call check_iostat(ios,__LINE__,__FILE__)
    write(unit=out,fmt='(A28,e14.6,A12,f7.3,A9,A6,f6.2,A2)',iostat=ios) &
         & ' Total CaCO3 pres          :',loc_tot1_sedgrid - loc_tot2_sedgrid,' mol yr-1 = ', &
         & 1.0E-12*conv_CaCO3_mol_kgC*(loc_tot1_sedgrid - loc_tot2_sedgrid),' GtC yr-1','   =  ',loc_pres_sedgrid,' %'
    call check_iostat(ios,__LINE__,__FILE__)
    write(unit=out,fmt='(A28,f6.2,A2)',iostat=ios) &
         & ' Mean wt% CaCO3            :',loc_mean_sedgrid,' %'
    call check_iostat(ios,__LINE__,__FILE__)
    ! CaCO3:POC
    loc_tot1_sedgrid = SUM(loc_mask_muds(:,:)*(sed_fsed(is_POC,:,:) + sed_fsed_OLD(is_POC,:,:)))
    loc_tot2_sedgrid = SUM(loc_mask_muds(:,:)*(sed_fsed(is_CaCO3,:,:) + sed_fsed_OLD(is_CaCO3,:,:)))
    if (abs(loc_tot1_sedgrid) > const_real_nullsmall) then 
       loc_rain_sedgrid = loc_tot2_sedgrid/loc_tot1_sedgrid
    else
       loc_rain_sedgrid = 0.0
    end if
    Write(unit=out,fmt=*) '---------------------------------'
    write(unit=out,fmt='(A28,f7.3)',iostat=ios) &
         & ' CaCO3/POC rain ratio      :', &
         & loc_rain_sedgrid
    call check_iostat(ios,__LINE__,__FILE__)
    ! opal
    loc_tot1_sedgrid = sum(loc_mask_muds(:,:)*loc_area(:,:)*loc_fsed(is_opal,:,:))
    loc_tot2_sedgrid = sum(loc_mask_muds(:,:)*loc_area(:,:)*loc_fdis(is_opal,:,:))
    if (abs(loc_tot1_sedgrid) > 0.0) then 
       loc_pres_sedgrid = 100.0*(loc_tot1_sedgrid - loc_tot2_sedgrid)/loc_tot1_sedgrid
    else
       loc_pres_sedgrid = 0.0
    end if
    if (loc_tot_mask_area > const_real_nullsmall) then 
       loc_mean_sedgrid = sum(loc_mask_muds(:,:)*loc_sed_coretop(is_opal,:,:)*loc_area(:,:))/loc_tot_mask_area
    else
       loc_mean_sedgrid = 0.0
    end if
    Write(unit=out,fmt=*) '---------------------------------'
    write(unit=out,fmt='(A28,e14.6,A12,f7.3,A9)',iostat=ios) &
         & ' opal rain                 :',loc_tot1_sedgrid,' mol yr-1'
    call check_iostat(ios,__LINE__,__FILE__)
    write(unit=out,fmt='(A28,e14.6,A12,f7.3,A9)',iostat=ios) &
         & ' opal diss                 :',loc_tot2_sedgrid,' mol yr-1'
    call check_iostat(ios,__LINE__,__FILE__)
    write(unit=out,fmt='(A28,e14.6,A12,f6.2,A2)',iostat=ios) &
         & ' Total opal pres           :',loc_tot1_sedgrid - loc_tot2_sedgrid,' mol yr-1 = ',loc_pres_sedgrid,' %'
    call check_iostat(ios,__LINE__,__FILE__)
    write(unit=out,fmt='(A28,f6.2,A2)',iostat=ios) &
         & ' Mean wt% opal             :',loc_mean_sedgrid,' %'
    call check_iostat(ios,__LINE__,__FILE__)
    ! Li
    if (sed_select(is_LiCO3) .AND. sed_select(is_detLi)) then
       Write(unit=out,fmt=*) '---------------------------------'
       loc_tot1_sedgrid = sum(loc_mask_muds(:,:)*loc_area(:,:)*loc_fsed(is_LiCO3,:,:))
       loc_tot2_sedgrid = sum(loc_mask_muds(:,:)*loc_area(:,:)*loc_fdis(is_LiCO3,:,:))
       write(unit=out,fmt='(A28,e14.6,A12,f7.3,A9)',iostat=ios) &
            & ' Li CaCO3 sink             :',loc_tot1_sedgrid,' mol yr-1'
       call check_iostat(ios,__LINE__,__FILE__)
       write(unit=out,fmt='(A28,e14.6,A12,f7.3,A9)',iostat=ios) &
            & ' Li CaCO3 source           :',loc_tot2_sedgrid,' mol yr-1'
       call check_iostat(ios,__LINE__,__FILE__)
       loc_tot1_sedgrid = sum(loc_mask_muds(:,:)*loc_area(:,:)*loc_fsed(is_detLi,:,:))
       loc_tot2_sedgrid = sum(loc_mask_muds(:,:)*loc_area(:,:)*loc_fdis(is_detLi,:,:))
       write(unit=out,fmt='(A28,e14.6,A12,f7.3,A9)',iostat=ios) &
            & ' Li detrital sink          :',loc_tot1_sedgrid,' mol yr-1'
       call check_iostat(ios,__LINE__,__FILE__)
       write(unit=out,fmt='(A28,e14.6,A12,f7.3,A9)',iostat=ios) &
            & ' Li detrital source        :',loc_tot2_sedgrid,' mol yr-1'
       call check_iostat(ios,__LINE__,__FILE__)
    end if
    Write(unit=out,fmt=*) '---------------------------------'

    ! DIAGNOSTICS ON GLOBAL GRID
    Write(unit=out,fmt=*) ' '
    Write(unit=out,fmt=*) '--- TOTAL SEDIMENT GRID ---------'
    Write(unit=out,fmt=*) '--- (equivalent to ocean grid) --'
    Write(unit=out,fmt=*) ' '
    ! MISC
    Write(unit=out,fmt=*) '---------------------------------'
    write(unit=out,fmt='(A28,I6)',iostat=ios) &
         & ' Total # sediment grid pts :',int(sum(loc_mask(:,:)))
    call check_iostat(ios,__LINE__,__FILE__)
    write(unit=out,fmt='(A28,e14.6,A3)',iostat=ios) &
         & ' Total sediment area       :',sum(loc_mask(:,:)*phys_sed(ips_A,:,:)),'m2'
    call check_iostat(ios,__LINE__,__FILE__)
    ! local variables 
    loc_tot_mask_area = sum(loc_mask(:,:)*loc_area(:,:))
    if (loc_tot_mask_area > const_real_nullsmall) then 
       loc_mean_sedgrid = sum(loc_mask(:,:)*loc_sed_coretop(is_CaCO3,:,:)*loc_area(:,:))/loc_tot_mask_area
    else
       loc_mean_sedgrid = 0.0
    end if
    Write(unit=out,fmt=*) '---------------------------------'
    write(unit=out,fmt='(A28,f6.2,A2)',iostat=ios) &
         & ' Mean wt% CaCO3            :',loc_mean_sedgrid,' %'
    call check_iostat(ios,__LINE__,__FILE__)
    Write(unit=out,fmt=*) '---------------------------------'

    ! FOOTER
    Write(unit=out,fmt=*) ' '
    Write(unit=out,fmt=*) '================================='

    ! close file
    Write(unit=out,fmt=*) ' '
    CLOSE(out,iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)

    ! *** SAVE FULL CORE-TOP DATA IN TEXT FILE FORMAT ***
    ! set filename
    loc_filename = TRIM(par_outdir_name)//'seddiag_misc_DATA_FULL'//string_results_ext
    ! open file
    call check_unit(out,__LINE__,__FILE__)
    OPEN(out,file=TRIM(loc_filename),action='write',iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    Write(unit=out,fmt=*) '% Sediment diagnostics data'
    Write(unit=out,fmt=*) '% ----------------------------------------'
    Write(unit=out,fmt=*) '% '
    write(unit=out,fmt='(A1,2A4,2A8,3A8,6A10,2A8,4A10,8A10)',iostat=ios) &
         & '%',                                          &
         & '   i','   j',                                &
         & '     lon',                                   &
         & '     lat',                                   &
         & '     D_m',                                   &
         & '     T_K',                                   &
         & '   S_mil',                                   &
         & '    CO2_uM',                                 &
         & '    ALK_uM',                                 &
         & '     O2_uM',                                 &
         & '     Ca_uM',                                 &
         & '   SiO2_uM',                                 &
         & '    CO3_uM',                                 &
         & '     ohm',                                   &
         & '   dCO3_uM',                                 &
         & '     POC_%',                                 &
         & '     cal_%',                                 &
         & '    opal_%',                                 &
         & '     det_%',                                 &
         & '   fsedPOC',                                 &
         & '   fsedcal',                                 &
         & '  fsedopal',                                 &
         & '   fseddet',                                 &
         & '   fdisPOC',                                 &
         & '   fdiscal',                                 &
         & '  fdisopal',                                 &
         & '    fdidet'
    call check_iostat(ios,__LINE__,__FILE__)
    Write(unit=out,fmt=*) ' '
    DO i=1,n_i
       DO j=1,n_j
          IF (sed_mask(i,j)) THEN
             write(unit=out,fmt='(1X,2I4,2f8.1,3f8.1,6f10.1,2f8.3,4f10.1,8f10.3)',iostat=ios) &
                  & i,j,                                                     &
                  & phys_sed(ips_lon,i,j),                                   &
                  & phys_sed(ips_lat,i,j),                                   &
                  & phys_sed(ips_D,i,j),                                     &
                  & dum_sfcsumocn(io_T,i,j),                                 &
                  & dum_sfcsumocn(io_S,i,j),                                 &
                  & 1.0E+06*dum_sfcsumocn(io_DIC,i,j),                       &
                  & 1.0E+06*dum_sfcsumocn(io_ALK,i,j),                       &
                  & 1.0E+06*dum_sfcsumocn(io_O2,i,j),                        &
                  & 1.0E+06*dum_sfcsumocn(io_Ca,i,j),                        &
                  & 1.0E+06*dum_sfcsumocn(io_SiO2,i,j),                      &
                  & 1.0E+06*sed_carb(ic_conc_CO3,i,j),                       &
                  & sed_carb(ic_ohm_cal,i,j),                                &
                  & 1.0E+06*sed_carb(ic_dCO3_cal,i,j),                       &
                  & loc_sed_coretop(is_POC,i,j),                             &
                  & loc_sed_coretop(is_CaCO3,i,j),                           &
                  & loc_sed_coretop(is_opal,i,j),                            &
                  & loc_sed_coretop(is_det,i,j),                             &
                  & 1.0E+06*sed_fsed(is_POC,i,j)/dum_dtyr,                   &
                  & 1.0E+06*sed_fsed(is_CaCO3,i,j)/dum_dtyr,                 &
                  & 1.0E+06*sed_fsed(is_opal,i,j)/dum_dtyr,                  &
                  & 1.0E+06*sed_fsed(is_det,i,j)/dum_dtyr,                   &
                  & 1.0E+06*sed_fdis(is_POC,i,j)/dum_dtyr,                   &
                  & 1.0E+06*sed_fdis(is_CaCO3,i,j)/dum_dtyr,                 &
                  & 1.0E+06*sed_fdis(is_opal,i,j)/dum_dtyr,                  &
                  & 1.0E+06*sed_fdis(is_det,i,j)/dum_dtyr
             call check_iostat(ios,__LINE__,__FILE__)
          end if
       end do
    end do
    ! close file
    CLOSE(out,iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)

  end SUBROUTINE sub_data_save_seddiag_GLOBAL
  ! ****************************************************************************************************************************** !


  ! ############################################################################################################################## !
  ! ### TO BE REMOVED? ########################################################################################################### !
  ! ############################################################################################################################## !


  ! ****************************************************************************************************************************** !
  ! SAVE SEDIMENT CORES
  SUBROUTINE sub_sedgem_save_sedcore()
    USE genie_util, ONLY: check_unit, check_iostat
    ! local variables
    integer::i,j,o,l,is                                                ! 
    integer::ios                                                       ! file checks
    CHARACTER(len=255)::loc_filename                                   ! 
    real::loc_tot,loc_frac,loc_standard                                ! 
    real::loc_delta
    REAL,ALLOCATABLE,DIMENSION(:,:,:,:)::loc_sed_save                  ! hold reordered data saving array
    REAL,ALLOCATABLE,DIMENSION(:,:,:)::loc_sed_save_age_cal            ! sediment age data saving array
    REAL,ALLOCATABLE,DIMENSION(:,:,:)::loc_sed_save_age_ash            ! sediment age data saving array
    REAL,ALLOCATABLE,DIMENSION(:,:,:)::loc_sed_save_ash_norm           ! sediment age data saving array
    REAL,ALLOCATABLE,DIMENSION(:,:,:)::loc_sed_save_age_14C            ! sediment age data saving array
    REAL,ALLOCATABLE,DIMENSION(:,:,:)::loc_sed_save_CaCO3_D14C         ! sediment CaCO3 D14C data saving array
    REAL,ALLOCATABLE,DIMENSION(:,:,:)::loc_sed_save_poros              ! sediment porosity
    REAL,ALLOCATABLE,DIMENSION(:,:,:)::loc_sed_save_th                 ! sediment layer thickness (as a check)
    REAL::loc_sed_tot_wt                                               ! total mass of solid coponents
    REAL::loc_sed_tot_vol                                              ! total volume of solid coponents
    INTEGER::loc_ash_max_o                                             ! running ash volume maximum sub-layer number
    REAL::loc_ash_max                                                  ! running ash volume maximum
    REAL::loc_ash_max_depth                                            ! running ash volume maximum down-core depth
    REAL::loc_ash_conv_dbs_age                                         ! convert depth to age using ash stratigraphy
    real::loc_ash_tot                                                  ! 
    INTEGER,DIMENSION(n_i,n_j)::loc_n_sed_stack_top    ! sediment stack top layer number
    REAL,DIMENSION(n_i,n_j)::loc_sed_stack_top_th      ! sediment stack top layer thickness
    integer::loc_l,loc_n_l_sed                                         ! 
    integer,DIMENSION(n_sed_tot)::loc_conv_iselected_is                ! 

    ! *** initialize variables ***
    ! allocate array for holding sediment data reordered for writing to file
    ! NOTE: the array bounds extend from ZERO up to 'n_sedtot' 
    !       so that core top layer data can be more easily assimilated
    ALLOCATE(loc_sed_save(n_sed,n_i,n_j,0:n_sed_tot),STAT=alloc_error) 
    call check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(loc_sed_save_age_cal(n_i,n_j,0:n_sed_tot),STAT=alloc_error)      
    call check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(loc_sed_save_age_ash(n_i,n_j,0:n_sed_tot),STAT=alloc_error)       
    call check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(loc_sed_save_ash_norm(n_i,n_j,0:n_sed_tot),STAT=alloc_error)     
    call check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(loc_sed_save_age_14C(n_i,n_j,0:n_sed_tot),STAT=alloc_error)     
    call check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(loc_sed_save_CaCO3_D14C(n_i,n_j,0:n_sed_tot),STAT=alloc_error)  
    call check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(loc_sed_save_poros(n_i,n_j,0:n_sed_tot),STAT=alloc_error)      
    call check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(loc_sed_save_th(n_i,n_j,0:n_sed_tot),STAT=alloc_error)     
    call check_iostat(alloc_error,__LINE__,__FILE__)
    ! check for problems allocating array space
    IF (alloc_error /= 0) THEN
       CALL sub_report_error( &
            & 'sedgem_data','sub_sedgem_save_sedcore', &
            & 'Array space could not be allocated', &
            & 'STOPPING', &
            & (/const_real_null/),.true. &
            & )
    ENDIF
    ! zero local variables
    loc_sed_save(:,:,:,:)          = const_real_zero
    loc_sed_save_age_cal(:,:,:)    = const_real_zero
    loc_sed_save_age_ash(:,:,:)    = const_real_zero
    loc_sed_save_ash_norm(:,:,:)   = const_real_zero
    loc_sed_save_age_14C(:,:,:)    = const_real_zero
    loc_sed_save_CaCO3_D14C(:,:,:) = const_real_zero
    loc_sed_save_poros(:,:,:)      = const_real_zero
    loc_sed_save_th(:,:,:)         = const_real_zero

    ! *** transform sediment array for saving to file ***
    ! NOTE: the sediment array needs to be re-ordered so that the youngest sediment in the sediment stack 
    !       starts with an array index of '1',
    !       and the sediment top material is added at index position '0'
    ! NOTE: sediment composition descriptors ired to %calcite, such as age and pH,
    !       need to be normailzed to %calcite
    ! NOTE: the sediment composition descriptors in the top layer sediments 
    !       need to be normailzed to a thickness of 1.0 cm
    ! NOTE: the sediment composition descriptors in the top (incomplete) sub-layer of the sediment stack 
    !       need to be normailzed to a thickness of 1.0 cm
    ! NOTE: the overall scheme is to loop through each sediment layer, and
    !       (a) calculate local constants
    !       (b) copy sediment core top layer data to data-file export array
    !       (c) copy sediment core stack sub-layer data to data-file export array
    !       (d) calculate age of CaCO3 sediment fraction
    !       (e) normailze solid sediment components to a mass fraction basis normalize sediment
    !       (f) normalize ash (volume) content to unit (cm) layer thickness
    !       (g) produce stratigraphic marker age scale
    !       (h) convert composition to percent
    !       (i) calculate isotope per mils
    !       (j) calculate D14C and radiocarbon age 

    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    ! *** (i,j) GRID PT LOOP START ***
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

    DO i = 1,n_i
       DO j = 1,n_j
          IF (sed_mask(i,j)) THEN

             ! *** (a) calculate local constants
             loc_n_sed_stack_top(i,j)  = INT(sed_top_h(i,j)) + 1
             loc_sed_stack_top_th(i,j) = sed_top_h(i,j) - REAL((loc_n_sed_stack_top(i,j) - 1))

             ! *** (b) copy core top layer data
             loc_sed_save(:,i,j,0) = sed_top(:,i,j)

             ! *** (c) copy core stack sub-layer data
             DO o = loc_n_sed_stack_top(i,j),1,-1
                loc_sed_save(:,i,j,(loc_n_sed_stack_top(i,j) - o + 1)) = sed(:,i,j,o)
             END DO

             ! *** (x) calculate porosity and actual layer thickness (actually, cm3 per cm) (as a check)
             !         NOTE: currently assume mud porosity the same as pelagic sediments
             o = 0
             loc_sed_tot_vol = fun_calc_sed_vol(loc_sed_save(:,i,j,o))
             if (loc_sed_tot_vol > const_real_nullsmall) then
                if (sed_mask_reef(i,j)) then
                   loc_sed_save_poros(i,j,o) = par_sed_poros_CaCO3_reef
                elseif (sed_mask_muds(i,j)) then
                   loc_sed_save_poros(i,j,o) = fun_calc_sed_poros_nsur(loc_sed_save(is_CaCO3,i,j,o)/loc_sed_tot_vol,par_sed_top_th)
                else
                   loc_sed_save_poros(i,j,o) = fun_calc_sed_poros_nsur(loc_sed_save(is_CaCO3,i,j,o)/loc_sed_tot_vol,par_sed_top_th)
                end if
                loc_sed_save_th(i,j,o) = loc_sed_tot_vol/(1.0 - loc_sed_save_poros(i,j,o))
             else
                loc_sed_save_poros(i,j,o) = 1.0
                loc_sed_save_th(i,j,o)    = 0.0
             end if
             DO o = 1,n_sed_tot
                loc_sed_tot_vol = fun_calc_sed_vol(loc_sed_save(:,i,j,o))
                if (loc_sed_tot_vol > const_real_nullsmall) then
                   if (sed_mask_reef(i,j)) then
                      loc_sed_save_poros(i,j,o) = par_sed_poros_CaCO3_reef
                   elseif (sed_mask_muds(i,j)) then
                      loc_sed_save_poros(i,j,o) = fun_calc_sed_poros(loc_sed_save(is_CaCO3,i,j,o)/loc_sed_tot_vol)
                   else
                      loc_sed_save_poros(i,j,o) = fun_calc_sed_poros(loc_sed_save(is_CaCO3,i,j,o)/loc_sed_tot_vol)
                   end if
                   loc_sed_save_th(i,j,o) = loc_sed_tot_vol/(1.0 - loc_sed_save_poros(i,j,o))
                else
                   loc_sed_save_poros(i,j,o) = 1.0
                   loc_sed_save_th(i,j,o)    = 0.0
                end if
             end DO

             ! *** (d) calculate carbonate internal age
             DO o = 0,n_sed_tot
                IF (loc_sed_save(is_CaCO3,i,j,o) > const_real_nullsmall) THEN
                   loc_sed_save_age_cal(i,j,o) = loc_sed_save(is_CaCO3_age,i,j,o)/loc_sed_save(is_CaCO3,i,j,o)
                ELSE
                   loc_sed_save_age_cal(i,j,o) = 0.0
                ENDIF
             ENDDO

             ! *** (e) normailze solid sediment components to a mass fraction basis (if required),
             !         + treat stable isotopes in a same manner
             !         NOTE: as a first step, calculate total mass of solid components in the sediment sub-layer
             DO o = 0,n_sed_tot
                IF (ctrl_data_save_wtfrac) THEN
                   loc_sed_tot_wt = fun_calc_sed_mass(loc_sed_save(:,i,j,o))
                   IF (loc_sed_tot_wt > const_real_nullsmall) THEN
                      loc_sed_save(:,i,j,o) = conv_sed_cm3_g(:)*loc_sed_save(:,i,j,o)/loc_sed_tot_wt
                   end IF
                else
                   loc_sed_tot_vol = fun_calc_sed_vol(loc_sed_save(:,i,j,o))
                   IF (loc_sed_tot_vol > const_real_nullsmall) THEN
                      loc_sed_save(:,i,j,o) = loc_sed_save(:,i,j,o)/loc_sed_tot_vol
                   end IF
                end if
             END DO

             ! *** (f) calculate normalized ash content
             !         NOTE: this is a teeny weeny bit redundant as the data is not currently saved ... d'uh!
             loc_ash_tot = &
                  & par_sed_top_th*loc_sed_save(is_ash,i,j,0) + &
                  & loc_sed_stack_top_th(i,j)*loc_sed_save(is_ash,i,j,1)+ &
                  & sum(loc_sed_save(is_ash,i,j,2:n_sed_tot))
             if (loc_ash_tot > const_real_nullsmall) then
                loc_sed_save_ash_norm(i,j,:) = loc_sed_save(is_ash,i,j,:)/loc_ash_tot
             else
                loc_sed_save_ash_norm(i,j,:) = const_real_zero
             end if

             ! *** (g) produce stratigraphic marker age scale
             !         NOTE: this assumes that the maximum ash volume fraction represents the ash impulse deposition age
             !               and that the sediment ages inbetween this depth and the surface
             !               can be linearly interpolated
             !         NOTE: sediment deeper then the ash maximum is aged by linear extrapolation
             !         NOTE: first, the ash maximum must be found
             !         NOTE: once the first maximum has been passed then stop searching,
             !               because there may be other maxima deeper down ...
             ! find ash maximum
             loc_ash_max = 0.0
             loc_ash_max_o = 0
             DO o = 0,n_sed_tot
                IF (loc_sed_save(is_ash,i,j,o) > (loc_ash_max + const_real_nullsmall)) THEN
                   loc_ash_max   = loc_sed_save(is_ash,i,j,o)
                   loc_ash_max_o = o
                ENDIF
                IF (loc_sed_save(is_ash,i,j,o) < (loc_ash_max - const_real_nullsmall)) exit
             END DO
             ! calculate ash maximum depth
             SELECT CASE (loc_ash_max_o)
             CASE (0)
                loc_ash_max_depth = par_sed_top_th/2.0
             CASE (1)
                loc_ash_max_depth = par_sed_top_th + loc_sed_stack_top_th(i,j)/2.0
             CASE default
                loc_ash_max_depth = par_sed_top_th + loc_sed_stack_top_th(i,j) + REAL((loc_ash_max_o - 2)) + 0.5
             END SELECT
             ! calculate linear age-depth relation
             loc_ash_conv_dbs_age = par_misc_t_runtime/loc_ash_max_depth
             ! generate age scale
             o = 0
             loc_sed_save_age_ash(i,j,o) = loc_ash_conv_dbs_age*(par_sed_top_th/2.0)
             o = 1
             loc_sed_save_age_ash(i,j,o) = loc_ash_conv_dbs_age*(par_sed_top_th + loc_sed_stack_top_th(i,j)/2.0)
             DO o = 2,n_sed_tot
                loc_sed_save_age_ash(i,j,o) = loc_ash_conv_dbs_age * &
                     (par_sed_top_th + loc_sed_stack_top_th(i,j) + REAL((o - 2)) + 0.5)
             END DO

             ! *** (i) calculate isotopic values in 'per mil' units
             !         NOTE: filter the result to remove the 'null' value when a delta cannot be calculated
             !               because this will screw up writing in the ASCII format later
             DO l=1,n_l_sed
                is = conv_iselected_is(l)
                SELECT CASE (sed_type(is))
                case (n_itype_min:n_itype_max)
                   DO o = 0,n_sed_tot
                      loc_tot  = loc_sed_save(sed_dep(is),i,j,o)
                      loc_frac = loc_sed_save(is,i,j,o)
                      loc_standard = const_standards(sed_type(is))
                      loc_delta = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_nulliso)
                      If (loc_delta == const_real_null) then
                         loc_sed_save(is,i,j,o) = const_real_zero
                      else
                         loc_sed_save(is,i,j,o) = loc_delta
                      end If
                   end DO
                end SELECT
             end do

             ! *** (h) convert mass or volume fraction to % units
             DO l=1,n_l_sed
                is = conv_iselected_is(l)
                SELECT CASE (sed_type(is))
                case (par_sed_type_bio,par_sed_type_abio)
                   DO o = 0,n_sed_tot
                      loc_sed_save(is,i,j,o) = 100.0*loc_sed_save(is,i,j,o)
                   end DO
                end SELECT
             end do

             ! *** (j) calculate D14C and radiocarbon age
             !         NOTE: this will be saved regardless of whether 14C is a included tracer in the model or not ...
             loc_sed_save_CaCO3_D14C(i,j,:) = const_real_zero
             loc_sed_save_age_14C(i,j,:) = const_real_zero
             if (sed_select(is_CaCO3_14C)) then
                DO o = 0,n_sed_tot
                   IF (loc_sed_save(is_CaCO3,i,j,o) > const_real_nullsmall) THEN
                      loc_sed_save_CaCO3_D14C(i,j,o) = &
                           & fun_convert_delta14CtoD14C(loc_sed_save(is_CaCO3_13C,i,j,o),loc_sed_save(is_CaCO3_14C,i,j,o))
                      loc_sed_save_age_14C(i,j,o) = &
                           & fun_convert_D14Ctoage(loc_sed_save_CaCO3_D14C(i,j,o))
                   end if
                end DO
             end if

          end IF
       END DO
    END DO

    ! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    ! *** (i,j) GRID PT LOOP END ***
    ! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

    ! *** save prescribed sediment (code) location data ***
    ! NOTE: data saved in plain text (ASCII) format
    ! NOTE: only save SELECTED sedimet tracer information
    ! NOTE: the '%' character is included at the start of the column header as an aid to MATLAB data importing
    ! define sub-set of selected tracer to be saved
    loc_conv_iselected_is(:) = 0
    loc_l = 0
    DO l=1,n_l_sed
       is = conv_iselected_is(l)
       SELECT CASE (sed_type(is))
       CASE (par_sed_type_bio,par_sed_type_abio,n_itype_min:n_itype_max)
          loc_l = loc_l + 1
          loc_conv_iselected_is(loc_l) = is
       END SELECT
    end DO
    loc_n_l_sed = loc_l
    ! save data
    DO i = 1,n_i
       DO j = 1,n_j
          if (sed_save_mask(i,j)) then
             loc_filename = TRIM(par_outdir_name)//'sedcore_'// &
                  & fun_conv_num_char_n(2,i)//fun_conv_num_char_n(2,j)// &
                  & string_results_ext
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             write(unit=out,fmt='(A4,2A10,3A14,A12,A15,999A10)',iostat=ios) &
                  & '%  #',                                      &
                  & '  dbs (cm)',                                &
                  & '   th (cm)',                                &
                  & '   CaCO3 age',                              &
                  & '  linear age',                              &
                  & '     14C age',                              &
                  & ' D14C (o/oo)',                              &
                  & ' Phi (cm3 cm-3)',                           &
                  & (trim(string_sed(loc_conv_iselected_is(loc_l))),loc_l=1,loc_n_l_sed)
             call check_iostat(ios,__LINE__,__FILE__)
             o = 0
             write(unit=out,fmt='(I4,2f10.3,3f14.3,f12.3,f15.3,999f10.3)',iostat=ios) &
                  & o,                                                     &
                  & par_sed_top_th/2.0,                                    &
                  & loc_sed_save_th(i,j,o),                                &
                  & loc_sed_save_age_cal(i,j,o),                           &
                  & loc_sed_save_age_ash(i,j,o),                           &
                  & loc_sed_save_age_14C(i,j,o),                           &
                  & loc_sed_save_CaCO3_D14C(i,j,o),                        &
                  & loc_sed_save_poros(i,j,o),                             &
                  & (loc_sed_save(loc_conv_iselected_is(loc_l),i,j,o),loc_l=1,loc_n_l_sed)
             call check_iostat(ios,__LINE__,__FILE__)
             o = 1
             write(unit=out,fmt='(I4,2f10.3,3f14.3,f12.3,f15.3,999f10.3)',iostat=ios) &
                  & o,                                                     &
                  & par_sed_top_th + loc_sed_stack_top_th(i,j)/2.0,        &
                  & loc_sed_save_th(i,j,o),                                &
                  & loc_sed_save_age_cal(i,j,o),                           &
                  & loc_sed_save_age_ash(i,j,o),                           &
                  & loc_sed_save_age_14C(i,j,o),                           &
                  & loc_sed_save_CaCO3_D14C(i,j,o),                        &
                  & loc_sed_save_poros(i,j,o),                             &
                  & (loc_sed_save(loc_conv_iselected_is(loc_l),i,j,o),loc_l=1,loc_n_l_sed)
             call check_iostat(ios,__LINE__,__FILE__)
             do o=2,n_sed_tot
                write(unit=out,fmt='(I4,2f10.3,3f14.3,f12.3,f15.3,999f10.3)',iostat=ios) &
                     & o,                                                                &
                     & par_sed_top_th + loc_sed_stack_top_th(i,j) + REAL((o - 2)) + 0.5, &
                     & loc_sed_save_th(i,j,o),                                           &
                     & loc_sed_save_age_cal(i,j,o),                                      &
                     & loc_sed_save_age_ash(i,j,o),                                      &
                     & loc_sed_save_age_14C(i,j,o),                                      &
                     & loc_sed_save_CaCO3_D14C(i,j,o),                                   &
                     & loc_sed_save_poros(i,j,o),                                        &
                     & (loc_sed_save(loc_conv_iselected_is(loc_l),i,j,o),loc_l=1,loc_n_l_sed)
                call check_iostat(ios,__LINE__,__FILE__)
             end do
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
          end if
       end DO
    end DO

    ! *** clean up ***
    ! deallocate local arrays
    if(allocated(loc_sed_save)) DEALLOCATE(loc_sed_save,STAT=dealloc_error)
    call check_iostat(dealloc_error,__LINE__,__FILE__)
    if(allocated(loc_sed_save_age_cal)) DEALLOCATE(loc_sed_save_age_cal,STAT=dealloc_error)
    call check_iostat(dealloc_error,__LINE__,__FILE__)
    if(allocated(loc_sed_save_age_ash)) DEALLOCATE(loc_sed_save_age_ash,STAT=dealloc_error)
    call check_iostat(dealloc_error,__LINE__,__FILE__)
    if(allocated(loc_sed_save_ash_norm)) DEALLOCATE(loc_sed_save_ash_norm,STAT=dealloc_error)
    call check_iostat(dealloc_error,__LINE__,__FILE__)
    if(allocated(loc_sed_save_age_14C)) DEALLOCATE(loc_sed_save_age_14C,STAT=dealloc_error)
    call check_iostat(dealloc_error,__LINE__,__FILE__)
    if(allocated(loc_sed_save_CaCO3_D14C)) DEALLOCATE(loc_sed_save_CaCO3_D14C,STAT=dealloc_error)
    call check_iostat(dealloc_error,__LINE__,__FILE__)
    if(allocated(loc_sed_save_poros)) DEALLOCATE(loc_sed_save_poros,STAT=dealloc_error)
    call check_iostat(dealloc_error,__LINE__,__FILE__)
    if(allocated(loc_sed_save_th)) DEALLOCATE(loc_sed_save_th,STAT=dealloc_error)
    call check_iostat(dealloc_error,__LINE__,__FILE__)
    ! check for problems de-allocating array space
    IF (dealloc_error /= 0) THEN
       CALL sub_report_error( &
            & 'sedgem_data','sub_sedgem_save_sedcore', &
            & 'Array space could not be deallocated', &
            & 'STOPPING', &
            & (/const_real_null/),.true. &
            & )
    ENDIF

  end SUBROUTINE sub_sedgem_save_sedcore
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! SAVE SEDIMENT DIAGNOSTICS DATA
  SUBROUTINE sub_data_save_seddiag_2D(dum_dtyr,dum_sfcsumocn)
    ! dummy valiables
    real,INTENT(in)::dum_dtyr
    real,DIMENSION(n_ocn,n_i,n_j),intent(in)::dum_sfcsumocn
    ! local variables
    INTEGER::i,j,l,io,is,ic,ips          ! 
    CHARACTER(len=255)::loc_filename
    REAL,DIMENSION(n_sed,n_i,n_j)::loc_sed_coretop
    REAL,DIMENSION(n_sed,n_i,n_j)::loc_sed_preservation
    REAL,DIMENSION(n_i,n_j)::loc_ij
    real::loc_tot,loc_frac,loc_standard
    ! calculate core-top sediment composition data
    loc_sed_coretop(:,:,:) = fun_sed_coretop()
    ! calculate local sediment preservation (%)
    DO l=1,n_l_sed
       is = conv_iselected_is(l)
       DO i=1,n_i
          DO j=1,n_j
             IF (sed_fsed(is,i,j) > const_real_nullsmall) THEN
                loc_sed_preservation(is,i,j) = 100.0*(sed_fsed(is,i,j) - sed_fdis(is,i,j))/sed_fsed(is,i,j)
             else
                loc_sed_preservation(is,i,j) = 0.0
             end if
          end do
       end do
    end do
    ! save grid data
    loc_filename = TRIM(par_outdir_name)//'seddiag_grid_topography'//TRIM(string_data_ext)
    CALL sub_save_data_ij(loc_filename,n_i,n_j,-phys_sed(ips_mask_sed,:,:)*phys_sed(ips_D,:,:))
    loc_filename = TRIM(par_outdir_name)//'seddiag_grid_lat_mid'//TRIM(string_data_ext)
    CALL sub_save_data_ij(loc_filename,n_i,n_j,phys_sed(ips_lat,:,:))
    loc_filename = TRIM(par_outdir_name)//'seddiag_grid_lon_mid'//TRIM(string_data_ext)
    CALL sub_save_data_ij(loc_filename,n_i,n_j,phys_sed(ips_lon,:,:))
    loc_filename = TRIM(par_outdir_name)//'seddiag_grid_lat_n'//TRIM(string_data_ext)
    CALL sub_save_data_ij(loc_filename,n_i,n_j,phys_sed(ips_latn,:,:))
    loc_filename = TRIM(par_outdir_name)//'seddiag_grid_lat_s'//TRIM(string_data_ext)
    CALL sub_save_data_ij(loc_filename,n_i,n_j,phys_sed(ips_latn,:,:) - phys_sed(ips_dlat,:,:))
    loc_filename = TRIM(par_outdir_name)//'seddiag_grid_lon_e'//TRIM(string_data_ext)
    CALL sub_save_data_ij(loc_filename,n_i,n_j,phys_sed(ips_lone,:,:))
    loc_filename = TRIM(par_outdir_name)//'seddiag_grid_lon_w'//TRIM(string_data_ext)
    CALL sub_save_data_ij(loc_filename,n_i,n_j,phys_sed(ips_lone,:,:) - phys_sed(ips_dlon,:,:))
    ! save interface flux data
    ! NOTE: flux data must be converted from units of (mol cm-2) to (mol cm-2 yr-1)
    DO l=1,n_l_sed
       is = conv_iselected_is(l)
       loc_ij(:,:) = const_real_zero
       DO i=1,n_i
          DO j=1,n_j
             SELECT CASE (sed_type(is))
             CASE (par_sed_type_bio,par_sed_type_abio)
                ! solids
                loc_ij(i,j) = sed_fsed(is,i,j)/dum_dtyr
             case (n_itype_min:n_itype_max)
                ! isotopes
                loc_tot  = sed_fsed(sed_dep(is),i,j)
                loc_frac = sed_fsed(is,i,j)
                loc_standard = const_standards(sed_type(is))
                loc_ij(i,j) = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_nulliso)
             END SELECT
          end do
       end do
       SELECT CASE (sed_type(is))
       CASE (par_sed_type_bio,par_sed_type_abio,n_itype_min:n_itype_max)
          loc_filename = &
               & TRIM(par_outdir_name)//'seddiag_fsed_'//TRIM(string_sed(is))//string_results_ext
          CALL sub_save_data_ij(loc_filename,n_i,n_j,loc_ij(:,:))
       END SELECT
    END DO
    DO l=1,n_l_sed
       is = conv_iselected_is(l)
       loc_ij(:,:) = const_real_zero
       DO i=1,n_i
          DO j=1,n_j
             SELECT CASE (sed_type(is))
             CASE (par_sed_type_bio,par_sed_type_abio)
                ! solids
                loc_ij(i,j) = sed_fdis(is,i,j)/dum_dtyr
             case (n_itype_min:n_itype_max)
                ! isotopes
                loc_tot  = sed_fsed(sed_dep(is),i,j)
                loc_frac = sed_fsed(is,i,j)
                loc_standard = const_standards(sed_type(is))
                loc_ij(i,j) = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_nulliso)
             END SELECT
          end do
       end do
       SELECT CASE (sed_type(is))
       CASE (par_sed_type_bio,par_sed_type_abio,n_itype_min:n_itype_max)
          loc_filename = &
               & TRIM(par_outdir_name)//'seddiag_fdis_'//TRIM(string_sed(is))//string_results_ext
          CALL sub_save_data_ij(loc_filename,n_i,n_j,loc_ij(:,:))
       END SELECT
    END DO
    DO l=1,n_l_sed
       is = conv_iselected_is(l)
       loc_ij(:,:) = const_real_zero
       DO i=1,n_i
          DO j=1,n_j
             SELECT CASE (sed_type(is))
             CASE (par_sed_type_bio,par_sed_type_abio)
                loc_ij(i,j) = loc_sed_preservation(is,i,j)
             END SELECT
          end do
       end do
       SELECT CASE (sed_type(is))
       CASE (par_sed_type_bio,par_sed_type_abio)
          loc_filename = &
               & TRIM(par_outdir_name)//'seddiag_pres_'//TRIM(string_sed(is))//string_results_ext
          CALL sub_save_data_ij(loc_filename,n_i,n_j,loc_ij(:,:))
       end SELECT
    END DO
    ! save interface flux data - misc
    ! dust (log10)
    IF (sed_select(is_det)) THEN
       loc_ij(:,:) = const_real_zero
       ! log10 data
       DO i=1,n_i
          DO j=1,n_j
             IF (sed_fsed(is_det,i,j) > 0.0) THEN
                loc_ij(:,:) = log10(sed_fsed(is_det,i,j)/dum_dtyr)
             else
                loc_ij(:,:) = const_real_null
             end if
          end do
       end do
       loc_filename = &
            & TRIM(par_outdir_name)//'seddiag_misc_fsed_'//TRIM(string_sed(is_det))//'_log10'//string_results_ext
       CALL sub_save_data_ij(loc_filename,n_i,n_j,loc_ij(:,:))
    END IF
    ! CaCO3:POC 'rain ratio'
    IF (sed_select(is_CaCO3) .AND. sed_select(is_POC)) THEN
       loc_ij(:,:) = const_real_zero
       DO i=1,n_i
          DO j=1,n_j
             if (sed_fsed(is_POC,i,j) > const_real_nullsmall) then
                loc_ij(i,j) = sed_fsed(is_CaCO3,i,j)/sed_fsed(is_POC,i,j)
             end if
          END DO
       END DO
       loc_filename = &
            & TRIM(par_outdir_name)//'seddiag_misc_fCaCO3tofPOC'//string_results_ext
       CALL sub_save_data_ij(loc_filename,n_i,n_j,loc_ij(:,:))
    end if
    ! save ocean interface tracer data field
    DO l=1,n_l_ocn
       io = conv_iselected_io(l)
       loc_ij(:,:) = const_real_zero
       DO i=1,n_i
          DO j=1,n_j
             SELECT CASE (ocn_type(io))
             CASE (1)
                loc_ij(i,j) = dum_sfcsumocn(io,i,j)
             case (n_itype_min:n_itype_max)
                loc_tot  = dum_sfcsumocn(ocn_dep(io),i,j)
                loc_frac = dum_sfcsumocn(io,i,j)
                loc_standard = const_standards(ocn_type(io))
                loc_ij(i,j) = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_nulliso)
             end SELECT
          end do
       end do
       loc_filename = &
            & TRIM(par_outdir_name)//'seddiag_ocn_'//TRIM(string_ocn(io))//string_results_ext
       CALL sub_save_data_ij(loc_filename,n_i,n_j,loc_ij(:,:))
    END DO
    ! save carbonate chemistry data field
    DO ic=1,n_carb
       loc_filename = &
            & TRIM(par_outdir_name)//'seddiag_carb_'//TRIM(string_carb(ic))//string_results_ext
       CALL sub_save_data_ij(loc_filename,n_i,n_j,sed_carb(ic,:,:))
    END DO
    ! save core-top data
    ! NOTE: the call to fun_sed_coretop made in populating <loc_sed_coretop> has already made the necessary type conversions
    !       for solid tracers as wt%, isotopes in per mill, and recovery of the carbonate 'age' value
    DO l=1,n_l_sed
       is = conv_iselected_is(l)
       SELECT CASE (sed_type(is))
       CASE (par_sed_type_bio,par_sed_type_abio,par_sed_type_age,n_itype_min:n_itype_max)
          loc_filename = &
               & TRIM(par_outdir_name)//'seddiag_sed_'//TRIM(string_sed(is))//string_results_ext
          CALL sub_save_data_ij(loc_filename,n_i,n_j,loc_sed_coretop(is,:,:))
       end SELECT
    END DO
    ! save phys (grid) details
    DO ips=1,n_phys_sed
       loc_filename = &
            & TRIM(par_outdir_name)//'seddiag_sedphys_'//TRIM(string_phys_sed(ips))//string_results_ext
       CALL sub_save_data_ij(loc_filename,n_i,n_j,phys_sed(ips,:,:))
    END DO
  end SUBROUTINE sub_data_save_seddiag_2D
  ! ****************************************************************************************************************************** !


  ! GHC 10/06/09
  !======= SUBROUTINE TO READ IN OUTPUT YEARS ========================================================!

  ! Subroutine: sub_data_output_years
  !
  ! Reads in years to output data from file.
  !
  ! Uses:
  !
  ! <genie_util>
  !
  ! Calls:
  !
  ! - <check_unit>
  ! - <check_iostat>

  SUBROUTINE sub_data_output_years()

    USE genie_util, ONLY: check_unit, check_iostat

    IMPLICIT NONE

    !local variables
    INTEGER:: i, n_years, n_output_years, ios, alloc_stat
    REAL:: year

    ! For 0d
    call check_unit(18,__LINE__,__FILE__)
    open(18,file=TRIM(par_indir_name)//TRIM(par_output_years_file_0d),action='read',iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    n_years = 0
    n_output_years=0
    DO
       READ(18,*,iostat=ios) year
       !call check_iostat(ios,__LINE__,__FILE__)
       IF (ios.lt.0) EXIT
       n_years = n_years + 1
       IF (year.gt.start_year) THEN
          n_output_years = n_output_years + 1
       ENDIF
    END DO

    PRINT*,'number of output years in '//TRIM(par_output_years_file_0d)//': ',n_output_years
    rewind(18)

    ALLOCATE(output_years_0d(n_output_years),stat=alloc_stat)
    call check_iostat(alloc_stat,__LINE__,__FILE__)
    ALLOCATE(output_tsteps_0d(n_output_years),stat=alloc_stat)
    call check_iostat(alloc_stat,__LINE__,__FILE__)

    i = 1
    DO
       READ(18,*,iostat=ios) year
       !call check_iostat(ios,__LINE__,__FILE__)
       IF (ios.lt.0) EXIT
       IF (year.gt.start_year) THEN
          output_years_0d(i) = year
          i = i + 1
       ENDIF
    END DO
    close(18)

    !PRINT*,'output years_0d:'
    !write(6,fmt='(f14.1)'),output_years_0d
    output_tsteps_0d = int(tsteps_per_year*(output_years_0d-start_year))
    output_counter_0d = 1

    ! For 2d
    call check_unit(18,__LINE__,__FILE__)
    open(18,file=TRIM(par_indir_name)//TRIM(par_output_years_file_2d),action='read',iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    n_years = 0
    n_output_years=0
    DO
       READ(18,*,iostat=ios) year
       !call check_iostat(ios,__LINE__,__FILE__)
       IF (ios.lt.0) EXIT
       n_years = n_years + 1
       IF (year.gt.start_year) THEN
          n_output_years = n_output_years + 1
       ENDIF
    END DO

    PRINT*,'number of output years in '//TRIM(par_output_years_file_2d)//': ',n_output_years
    rewind(18)

    ALLOCATE(output_years_2d(n_output_years),stat=alloc_stat)
    call check_iostat(alloc_stat,__LINE__,__FILE__)
    ALLOCATE(output_tsteps_2d(n_output_years),stat=alloc_stat)
    call check_iostat(alloc_stat,__LINE__,__FILE__)

    i = 1
    DO
       READ(18,*,iostat=ios) year
       !call check_iostat(ios,__LINE__,__FILE__)
       IF (ios.lt.0) EXIT
       IF (year.gt.start_year) THEN
          output_years_2d(i) = year
          i = i + 1
       ENDIF
    END DO
    close(18)

    !PRINT*,'output years_2d:'
    !write(6,fmt='(f14.1)'),output_years_2d
    output_tsteps_2d = int(tsteps_per_year*(output_years_2d-start_year))
    output_counter_2d = 1

  END SUBROUTINE sub_data_output_years


  !======= SUBROUTINE TO CHANGE OUTPUT YEAR  ==================================================!

  ! Subroutine: sub_output_year
  !
  ! year is read from list of output years depending on whether 0D or 2D output is due

  SUBROUTINE sub_output_year()

    IMPLICIT NONE

    IF (tstep_count.eq.output_tsteps_0d(output_counter_0d)) THEN
       year = output_years_0d(output_counter_0d)
    ENDIF

    IF (tstep_count.eq.output_tsteps_2d(output_counter_2d)) THEN 
       year = output_years_2d(output_counter_2d)
    ENDIF

    !print*,tstep_count,output_counter_0d,output_counter_2d,year

    year_int = int(year)
    year_remainder = int(1000*(year - real(year_int)))
    year_text = fun_conv_num_char_n(8,year_int)//'_'//fun_conv_num_char_n(3,year_remainder)

  END SUBROUTINE sub_output_year

  !======= SUBROUTINE TO INCREMENT OUTPUT COUNTERS  ==================================================!

  ! Subroutine: sub_output_counters
  !
  ! output_counters go up by 1 after each output

  SUBROUTINE sub_output_counters()

    IMPLICIT NONE

    IF (tstep_count.eq.output_tsteps_0d(output_counter_0d)) THEN
       output_counter_0d = output_counter_0d + 1
    ENDIF

    IF (tstep_count.eq.output_tsteps_2d(output_counter_2d)) THEN 
       output_counter_2d = output_counter_2d + 1 
    ENDIF

  END SUBROUTINE sub_output_counters


END MODULE sedgem_data

