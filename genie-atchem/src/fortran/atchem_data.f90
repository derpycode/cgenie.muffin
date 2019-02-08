! ******************************************************************************************************************************** !
! atchem_data.f90
! Atmospheric Chemistry
! DATA LOADING/SAVING/INITIALIZATION ROUTINES
! ******************************************************************************************************************************** !


MODULE atchem_data

  
  USE atchem_lib
  IMPLICIT NONE
  SAVE
  
  
CONTAINS
  
  
  ! ****************************************************************************************************************************** !
  ! LOAD AtCheM 'goin' FILE OPTIONS
  SUBROUTINE sub_load_goin_atchem()
    ! local variables
    integer::l,ia                                                ! tracer counter
    integer::ios                                                 !
    ! read data_ATCHEM file
    open(unit=in,file='data_ATCHEM',status='old',action='read',iostat=ios)
    if (ios /= 0) then
       print*,'ERROR: could not open ATCHEM initialisation namelist file'
       stop
    end if
    ! read in namelist and close data_ATCHEM file
    read(UNIT=in,NML=ini_atchem_nml,IOSTAT=ios)
    if (ios /= 0) then
       print*,'ERROR: could not read ATCHEM namelist'
       stop
    else
       close(unit=in)
    end if
    ! set and report namelist data
    par_indir_name = trim(par_indir_name)//'/'
    par_outdir_name = trim(par_outdir_name)//'/'
    par_rstdir_name = trim(par_rstdir_name)//'/'
    if (ctrl_debug_init > 0) then
       ! --- TRACER INITIALIZATION ----------------------------------------------------------------------------------------------- !
       print*,'--- TRACER INITIALIZATION --------------------------'
       DO l=1,n_l_atm
          ia = conv_iselected_ia(l)
          print*,'atm tracer initial value: ',trim(string_atm(ia)),' = ',atm_init(ia)
          print*,'atm tracer perturbation : ',trim(string_atm(ia)),' = ',atm_dinit(ia)
       end do
       print*,'Absolute (not relative) tracer re-start adjustment? : ',ctrl_atm_dinit
       ! --- COSMOGENIC & RADIOGENIC PRODUCTION ---------------------------------------------------------------------------------- !
       print*,'--- COSMOGENIC & RADIOGENIC PRODUCTION -------------'
       print*,'Global cosmogenic production rate of 14C (mol yr-1) : ',par_atm_F14C
       ! --- ATMOSPHERIC PHOTOCHEMISTRY ------------------------------------------------------------------------------------------ !
       print*,'--- ATMOSPHERIC PHOTOCHEMISTRY ---------------------'
       print*,'Atmospheric photochemistry scheme                   : ',par_atm_CH4_photochem
       print*,'Baseline atmospheric pCH4                           : ',par_pCH4_oxidation_C0
       print*,'Baseline CH4 lifetime                               : ',par_pCH4_oxidation_tau0
       print*,'Exponent for CH4 lifetime                           : ',par_pCH4_oxidation_N
       print*,'Atmospheric pO2 for fixed scheme                    : ',par_atm_pO2_fixed
       ! --- EMISSIONS-TO-ATMOSPHERE --------------------------------------------------------------------------------------------- !
       print*,'--- EMISSIONS-TO-ATMOSPHERE ------------------------'
       print*,'Wetlands CH4 flux (mol yr-1)                        : ',par_atm_wetlands_FCH4
       print*,'Wetlands CH4 d13C (o/oo)                            : ',par_atm_wetlands_FCH4_d13C
       ! --- SLAB BIOSPHERE ------------------------------------------------------------------------------------------------------ !
       print*,'--- SLAB BIOSPHERE ---------------------------------'
       print*,': ',par_atm_slabbiosphere_C
       print*,': ',par_atm_slabbiosphere_C_d13C
       print*,': ',par_atm_FterrCO2exchange
       ! --- RUN CONTROL --------------------------------------------------------------------------------------------------------- !
       print*,'--- RUN CONTROL ------------------------------------'
       print*,'Continuing run?                                     : ',ctrl_continuing
       ! --- I/O DIRECTORY DEFINITIONS ------------------------------------------------------------------------------------------- !
       print*,'--- I/O DIRECTORY DEFINITIONS ----------------------'
       print*,'(Paleo config) input dir. name                      : ',trim(par_pindir_name)
       print*,'Input dir. name                                     : ',trim(par_indir_name)
       print*,'Output dir. name                                    : ',trim(par_outdir_name)
       print*,'Restart (input) dir. name                           : ',trim(par_rstdir_name)
       print*,'Filename for restart input                          : ',trim(par_infile_name)
       print*,'Filename for restart output                         : ',trim(par_outfile_name)
       ! --- DATA SAVING: MISC --------------------------------------------------------------------------------------------------- !
       print*,'--- DATA SAVING: MISC ------------------------------'
       print*,'Restart in netCDF format?                           : ',ctrl_ncrst
       print*,'netCDF restart file name                            : ',trim(par_ncrst_name)
       ! --- DEBUGGING OPTIONS --------------------------------------------------------------------------------------------------- !
       print*,'--- DEBUGGING OPTIONS ----------'
       print*,'Report level #1 debug?                              : ',ctrl_debug_lvl1
       ! #### INSERT CODE TO LOAD ADDITIONAL PARAMETERS ########################################################################## !
       !
       ! ######################################################################################################################### !
    end if
  END SUBROUTINE sub_load_goin_atchem
  ! ****************************************************************************************************************************** !

  
  ! ****************************************************************************************************************************** !
  ! *** LOAD Atchem RESTART DATA ************************************************************************************************* !
  ! ****************************************************************************************************************************** !
  SUBROUTINE sub_data_load_rst()
    USE atchem_lib
    use gem_netcdf
    USE genie_util, ONLY:check_unit,check_iostat
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    integer::l,ia,iv                                           ! local counting variables
    integer::ios                                               !
    integer::loc_ncid                                          !
    CHARACTER(len=255)::loc_filename                           ! filename string
    integer::loc_n_l_atm                                       ! number of selected tracers in the re-start file
    integer,DIMENSION(n_atm)::loc_conv_iselected_ia            ! number of selected atmospheric tracers in restart
    real,dimension(n_i,n_j)::loc_atm                           ! 
    integer::loc_ndims,loc_nvars
    integer,ALLOCATABLE,dimension(:)::loc_dimlen
    integer,ALLOCATABLE,dimension(:,:)::loc_varlen
    integer,ALLOCATABLE,dimension(:)::loc_vdims
    character(20),ALLOCATABLE,dimension(:)::loc_varname
    ! -------------------------------------------------------- !
    ! INITIALIZE
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! set filename
    IF (ctrl_ncrst) THEN
       loc_filename = TRIM(par_rstdir_name)//par_ncrst_name
    else
       loc_filename = TRIM(par_rstdir_name)//trim(par_infile_name)
    endif
    ! -------------------------------------------------------- ! check file status
    call check_unit(in,__LINE__,__FILE__)
    OPEN(unit=in,status='old',file=loc_filename,form='unformatted',action='read',IOSTAT=ios)
    close(unit=in)
    If (ios /= 0) then
       CALL sub_report_error( &
            & 'atchem_data','sub_data_load_restart', &
            & 'You have requested a CONTINUING run, but restart file <'//trim(loc_filename)//'> does not exist', &
            & 'SKIPPING - using default initial values (FILE: gem_config_atm.par)', &
            & (/const_real_null/),.false. &
            & )
    else
       ! -------------------------------------------------------- !
       ! LOAD RESTART
       ! -------------------------------------------------------- !
       IF (ctrl_ncrst) THEN
          call sub_openfile(loc_filename,loc_ncid)
          ! -------------------------------------------------------- ! determine number of variables
          call sub_inqdims (loc_filename,loc_ncid,loc_ndims,loc_nvars)
          ! -------------------------------------------------------- ! allocate arrays
          ALLOCATE(loc_dimlen(loc_ndims),STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          ALLOCATE(loc_varlen(2,loc_nvars),STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          ALLOCATE(loc_vdims(loc_nvars),STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          ALLOCATE(loc_varname(loc_nvars),STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          ! -------------------------------------------------------- ! get variable names
          call sub_inqvars(loc_ncid,loc_ndims,loc_nvars,loc_dimlen,loc_varname,loc_vdims,loc_varlen)
          ! -------------------------------------------------------- ! load and apply only tracers that are selected
          IF (ctrl_debug_init == 1) print*,' * Loading restart tracers: '
          DO iv=1,loc_nvars
             DO l=1,n_l_atm
                ia = conv_iselected_ia(l)
                if ('atm_'//trim(string_atm(ia)) == trim(loc_varname(iv))) then
                   IF (ctrl_debug_init == 1) print*,'   ',trim(loc_varname(iv))
                   loc_atm = 0.0
                   call sub_getvarij(loc_ncid,'atm_'//trim(string_atm(ia)),n_i,n_j,loc_atm)
                   atm(ia,:,:) = loc_atm(:,:)
                endif
             end do
          end DO
          ! -------------------------------------------------------- ! deallocate arrays
          deALLOCATE(loc_dimlen,STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          deALLOCATE(loc_varlen,STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          deALLOCATE(loc_vdims,STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          deALLOCATE(loc_varname,STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          ! -------------------------------------------------------- ! close file
          call sub_closefile(loc_ncid)
       else
          OPEN(unit=in,status='old',file=loc_filename,form='unformatted',action='read',IOSTAT=ios)
          read(unit=in) &
               & loc_n_l_atm,                                          &
               & (loc_conv_iselected_ia(l),l=1,loc_n_l_atm),           &
               & (atm(loc_conv_iselected_ia(l),:,:),l=1,loc_n_l_atm)
          close(unit=in,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
       endif
       ! -------------------------------------------------------- ! back compatability
       if (sum(atm(ia_T,:,:))/size(atm(ia_T,:,:)) > 100.0) then
          atm(ia_T,:,:) = atm(ia_T,:,:) - const_zeroC
       endif
       ! -------------------------------------------------------- ! adjust restart data
       DO l=1,n_l_atm
          ia = conv_iselected_ia(l)
          if (ctrl_atm_dinit) then
             atm(ia,:,:) = atm(ia,:,:) + atm_dinit(ia)
          else
             atm(ia,:,:) = (1.0 + atm_dinit(ia))*atm(ia,:,:)             
          end if
       end do
    endif
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  end SUBROUTINE sub_data_load_rst
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! initialize atmosphere grid
  SUBROUTINE sub_init_phys_atm()
    ! local variables
    INTEGER::i,j
    ! zero array
    phys_atm(:,:,:) = 0.0
    ! initialize array values
    DO i=1,n_i
       DO j=1,n_j
          phys_atm(ipa_lat,i,j)  = (180.0/const_pi)*ASIN(goldstein_s(j))
          phys_atm(ipa_lon,i,j)  = (360.0/real(n_i))*(real(i)-0.5) + par_grid_lon_offset
          phys_atm(ipa_dlat,i,j) = (180.0/const_pi)*(ASIN(goldstein_sv(j)) - ASIN(goldstein_sv(j-1)))
          phys_atm(ipa_dlon,i,j) = (360.0/real(n_i))
          phys_atm(ipa_latn,i,j) = (180.0/const_pi)*ASIN(goldstein_sv(j))
          phys_atm(ipa_lone,i,j) = (360.0/real(n_i))*real(i) + par_grid_lon_offset
          phys_atm(ipa_dh,i,j)   = par_atm_th
          phys_atm(ipa_A,i,j)    = 2.0*const_pi*(const_rEarth**2)*(1.0/n_i)*(goldstein_sv(j) - goldstein_sv(j-1))
          phys_atm(ipa_rA,i,j)   = 1.0/phys_atm(ipa_A,i,j)
          phys_atm(ipa_V,i,j)    = phys_atm(ipa_dh,i,j)*phys_atm(ipa_A,i,j)
          phys_atm(ipa_rV,i,j)   = 1.0/phys_atm(ipa_V,i,j)
       END DO
    END DO
  END SUBROUTINE sub_init_phys_atm
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CONFIGURE AND INITIALIZE TRACER COMPOSITION - ATMOSPHERE
  SUBROUTINE sub_init_tracer_atm_comp()
    ! local variables
    INTEGER::i,j,ia
    real::loc_tot,loc_frac,loc_standard
    ! initialize global arrays
    atm(:,:,:)  = 0.0
    ! set <atm> array
    ! NOTE: need to seed ia_T as temperature is required in order to convert between mole (total) and partial pressure
    ! NOTE: ia_T as degrees C
    DO i=1,n_i
       DO j=1,n_j
          DO ia=1,n_atm
             IF (atm_select(ia)) THEN
                SELECT CASE (atm_type(ia))
                CASE (0)
                   if (ia == ia_T) atm(ia,i,j) = 0.0                
                CASE (1)
                   atm(ia,i,j) = atm_init(ia)
                CASE (11,12,13,14)
                   loc_tot  = atm_init(atm_dep(ia))
                   loc_standard = const_standards(atm_type(ia))
                   loc_frac = fun_calc_isotope_fraction(atm_init(ia),loc_standard)
                   atm(ia,i,j) = loc_frac*loc_tot
                END SELECT
             end if
          END DO
       END DO
    END DO
  END SUBROUTINE sub_init_tracer_atm_comp
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CONFIGURE AND INITIALIZE SLAB BIOSPHERE
  SUBROUTINE sub_init_slabbiosphere()
    ! local variables
    INTEGER::i,j
    real::loc_tot,loc_frac,loc_standard
    ! initialize global arrays
    atm_slabbiosphere(:,:,:)  = 0.0
    ! set <atm> array
    ! NOTE: units of (mol)
    DO i=1,n_i
       DO j=1,n_j
          IF (atm_select(ia_pCO2)) THEN
             atm_slabbiosphere(ia_pCO2,i,j) = par_atm_slabbiosphere_C/real(n_i*n_j)
          end if
          IF (atm_select(ia_pCO2_13C)) THEN
             loc_tot  = atm_slabbiosphere(ia_pCO2,i,j)
             loc_standard = const_standards(atm_type(ia_pCO2_13C))
             loc_frac = fun_calc_isotope_fraction(par_atm_slabbiosphere_C_d13C,loc_standard)
             atm_slabbiosphere(ia_pCO2_13C,i,j) = loc_frac*loc_tot
          end if
       END DO
    END DO
  END SUBROUTINE sub_init_slabbiosphere
  ! ****************************************************************************************************************************** !


END MODULE atchem_data
