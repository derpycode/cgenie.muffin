! ******************************************************************************************************************************** !
! gem_data.f90
! DATA LOADING/SAVING/INITIALIZATION ROUTINES
! ******************************************************************************************************************************** !


MODULE gem_data

  
  USE gem_cmn
  IMPLICIT NONE
  SAVE
  
  
CONTAINS
  
  
  ! ****************************************************************************************************************************** !
  ! LOAD SEDGEM 'goin' FILE OPTIONS
  SUBROUTINE sub_load_goin_gem()
    ! local variables
    integer::ios                                                 !
    ! read data_GEM file
    open(unit=in,file='data_GEM',status='old',action='read',iostat=ios)
    if (ios /= 0) then
       print*,'ERROR: could not open GEM initialisation namelist file'
       stop
    end if
    ! read in namelist and close data_GEM file
    read(UNIT=in,NML=ini_gem_nml,IOSTAT=ios)
    if (ios /= 0) then
       print*,'ERROR: could not read GEM namelist'
       stop
    else
       close(unit=in)
    end if
    ! set and report namelist data
    par_carbconstset_name = trim(par_carbconstset_name)//'/'
    par_adj_carbconst_option = trim(par_adj_carbconst_option)
    par_gem_indir_name = trim(par_gem_indir_name)//'/'
 
if (ctrl_debug_init > 0) then
    ! --- TRACER SELECTION  ------------------------------------------------------------------------------------------------------ !
    ! NOTE: reported at end of initialise_gem when tracer name information is available
    ! --- GEOCHEM CONTROLS ------------------------------------------------------------------------------------------------------- !
    print*,'--- GEOCHEM CONTROLS ---'
    print*,'carbonate dissociation constants set                : ',trim(par_carbconstset_name)
    print*,'pH solution tolerance                               : ',par_carbchem_pH_tolerance
    print*,'pH solution maximum number of iterations            : ',par_carbchem_pH_iterationmax
    print*,'Attempt pH re-seed if solution fails (else exit)?   : ',ctrl_carbchem_pHseed_retry
    print*,'Ignore H3SiO4 in the calculation of carbonate ALK   : ',ctrl_carbchem_noH3SiO4
    ! --- MISC CONTROLS  --------------------------------------------------------------------------------------------------------- !
    print*,'--- MISC CONTROLS ---'
    print*,'minimum T used in empirical geochem calculations    : ',par_geochem_Tmin 
    print*,'maximum T used in empirical geochem calculations    : ',par_geochem_Tmax
    print*,'minimum S used in empirical geochem calculations    : ',par_geochem_Smin 
    print*,'maximum S used in empirical geochem calculations    : ',par_geochem_Smax 
    print*,'minimum T used in empirical carbchem calculations   : ',par_carbchem_Tmin 
    print*,'maximum T used in empirical carbchem calculations   : ',par_carbchem_Tmax
    print*,'minimum S used in empirical carbchem calculations   : ',par_carbchem_Smin 
    print*,'maximum S used in empirical carbchem calculations   : ',par_carbchem_Smax 
    print*,'assumed longitudinal offset of the grid             : ',par_grid_lon_offset
    print*,'Debug (initialization) level                        : ',ctrl_debug_init
    print*,'Debug (loop) level                                  : ',ctrl_debug_loop
    print*,'Debug (end) level                                   : ',ctrl_debug_end
    print*,'Report all run-time warnings?                       : ',ctrl_debug_reportwarnings
    ! --- I/O: DIRECTORY DEFINITIONS --------------------------------------------------------------------------------------------- !
    print*,'--- I/O: DIRECTORY DEFINITIONS ---'
    print*,'Input dir. name                                     : ',trim(par_gem_indir_name)
    print*,'filetype for series output files for GEM modules    : ',trim(string_results_ext)
    ! #### INSERT CODE TO LOAD ADDITIONAL PARAMETERS ############################################################################# !
    !
    ! ############################################################################################################################ !
end if
  END SUBROUTINE sub_load_goin_gem
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! LOAD CARBONATE CONSTANT LOOK-UP TABLES - K1
  SUBROUTINE sub_load_gem_MyAMI_lookup_K1()
    USE gem_util, ONLY: sub_report_error
    !USE genie_util, ONLY: check_unit, check_iostat
    ! local variables
    INTEGER::a,b,d,e
    CHARACTER(len=255)::loc_filename
    INTEGER::ios ! for file checks
    ! allocate size of look-up tables and load data -- CaCO3
    ! NOTE: check for problems allocating array space
    ALLOCATE(lookup_gem_MyAMI_k1( &
         & lookup_i_Ca_min:lookup_i_Ca_max, &
         & lookup_i_Mg_min:lookup_i_Mg_max, &
         & lookup_i_sal_min:lookup_i_sal_max, &
         & lookup_i_temp_min:lookup_i_temp_max &
         & ),STAT=error)

     IF (error /= 0) THEN
        CALL sub_report_error( &
             & 'initialize_gem','initialise_gem', &
             & 'Could not allocate space for MyAMI K1 look-up table array', &
             & 'STOPPING', &
             & (/const_real_zero/),.TRUE. &
             & )
     ENDIF
    ! *** read in look-up data ***
    loc_filename = '../../cgenie.muffin/genie-main/data/input/MyAMI_lookup_K1.dat'
    !call check_unit(in,__LINE__,__FILE__)
    OPEN(unit=in,file=loc_filename,action='read',iostat=ios)
    !call check_iostat(ios,__LINE__,__FILE__)
    ! read in data
    DO a = lookup_i_Ca_min,lookup_i_Ca_max,1
       DO b = lookup_i_Mg_min,lookup_i_Mg_max,1
          DO d = lookup_i_sal_min,lookup_i_sal_max,1
             DO e = lookup_i_temp_min,lookup_i_temp_max,1
                READ(unit=in,FMT='(D23.16)',iostat=ios) lookup_gem_MyAMI_k1(a,b,d,e)
                !call check_iostat(ios,__LINE__,__FILE__)
             END DO
          END DO
       END DO
    END DO
    ! close file pipe
    CLOSE(unit=in,iostat=ios)
    !call check_iostat(ios,__LINE__,__FILE__)
  END SUBROUTINE sub_load_gem_MyAMI_lookup_K1
  ! ****************************************************************************************************************************** !

  ! ****************************************************************************************************************************** !
  ! LOAD CARBONATE CONSTANT LOOK-UP TABLES - K2
  SUBROUTINE sub_load_gem_MyAMI_lookup_K2()
    USE gem_util, ONLY: sub_report_error
    !USE genie_util, ONLY: check_unit, check_iostat
    ! local variables
    INTEGER::a,b,d,e
    CHARACTER(len=255)::loc_filename
    INTEGER::ios ! for file checks
    ! allocate size of look-up tables and load data -- CaCO3
    ! NOTE: check for problems allocating array space
    ALLOCATE(lookup_gem_MyAMI_k2( &
         & lookup_i_Ca_min:lookup_i_Ca_max, &
         & lookup_i_Mg_min:lookup_i_Mg_max, &
         & lookup_i_sal_min:lookup_i_sal_max, &
         & lookup_i_temp_min:lookup_i_temp_max &
         & ),STAT=error)

     IF (error /= 0) THEN
        CALL sub_report_error( &
             & 'initialize_gem','initialise_gem', &
             & 'Could not allocate space for MyAMI K1 look-up table array', &
             & 'STOPPING', &
             & (/const_real_zero/),.TRUE. &
             & )
     ENDIF
    ! *** read in look-up data ***
    loc_filename = '../../cgenie.muffin/genie-main/data/input/MyAMI_lookup_K2.dat'
    !call check_unit(in,__LINE__,__FILE__)
    OPEN(unit=in,file=loc_filename,action='read',iostat=ios)
    !call check_iostat(ios,__LINE__,__FILE__)
    ! read in data
    DO a = lookup_i_Ca_min,lookup_i_Ca_max,1
       DO b = lookup_i_Mg_min,lookup_i_Mg_max,1
          DO d = lookup_i_sal_min,lookup_i_sal_max,1
             DO e = lookup_i_temp_min,lookup_i_temp_max,1
                READ(unit=in,FMT='(D23.16)',iostat=ios) lookup_gem_MyAMI_k2(a,b,d,e)
                !call check_iostat(ios,__LINE__,__FILE__)
             END DO
          END DO
       END DO
    END DO
    ! close file pipe
    CLOSE(unit=in,iostat=ios)
    !call check_iostat(ios,__LINE__,__FILE__)
  END SUBROUTINE sub_load_gem_MyAMI_lookup_K2
  ! ****************************************************************************************************************************** !

  ! ****************************************************************************************************************************** !
  ! LOAD CARBONATE CONSTANT LOOK-UP TABLES - Kcal
  SUBROUTINE sub_load_gem_MyAMI_lookup_Kcal()
    USE gem_util, ONLY: sub_report_error
    !USE genie_util, ONLY: check_unit, check_iostat
    ! local variables
    INTEGER::a,b,d,e
    CHARACTER(len=255)::loc_filename
    INTEGER::ios ! for file checks
    ! allocate size of look-up tables and load data -- CaCO3
    ! NOTE: check for problems allocating array space
    ALLOCATE(lookup_gem_MyAMI_kcal( &
         & lookup_i_Ca_min:lookup_i_Ca_max, &
         & lookup_i_Mg_min:lookup_i_Mg_max, &
         & lookup_i_sal_min:lookup_i_sal_max, &
         & lookup_i_temp_min:lookup_i_temp_max &
         & ),STAT=error)

     IF (error /= 0) THEN
        CALL sub_report_error( &
             & 'initialize_gem','initialise_gem', &
             & 'Could not allocate space for MyAMI Kcal look-up table array', &
             & 'STOPPING', &
             & (/const_real_zero/),.TRUE. &
             & )
     ENDIF
    ! *** read in look-up data ***
    loc_filename = '../../cgenie.muffin/genie-main/data/input/MyAMI_lookup_Kcal.dat'
    !call check_unit(in,__LINE__,__FILE__)
    OPEN(unit=in,file=loc_filename,action='read',iostat=ios)
    !call check_iostat(ios,__LINE__,__FILE__)
    ! read in data
    DO a = lookup_i_Ca_min,lookup_i_Ca_max,1
       DO b = lookup_i_Mg_min,lookup_i_Mg_max,1
          DO d = lookup_i_sal_min,lookup_i_sal_max,1
             DO e = lookup_i_temp_min,lookup_i_temp_max,1
                READ(unit=in,FMT='(D23.16)',iostat=ios) lookup_gem_MyAMI_kcal(a,b,d,e)
                !call check_iostat(ios,__LINE__,__FILE__)
             END DO
          END DO
       END DO
    END DO
    ! close file pipe
    CLOSE(unit=in,iostat=ios)
    !call check_iostat(ios,__LINE__,__FILE__)
  END SUBROUTINE sub_load_gem_MyAMI_lookup_Kcal
  ! ****************************************************************************************************************************** !

  ! ****************************************************************************************************************************** !
  ! LOAD CARBONATE CONSTANT LOOK-UP TABLES - Karg
  SUBROUTINE sub_load_gem_MyAMI_lookup_Karg()
    USE gem_util, ONLY: sub_report_error
    !USE genie_util, ONLY: check_unit, check_iostat
    ! local variables
    INTEGER::a,b,d,e
    CHARACTER(len=255)::loc_filename
    INTEGER::ios ! for file checks
    ! allocate size of look-up tables and load data -- CaCO3
    ! NOTE: check for problems allocating array space
    ALLOCATE(lookup_gem_MyAMI_karg( &
         & lookup_i_Ca_min:lookup_i_Ca_max, &
         & lookup_i_Mg_min:lookup_i_Mg_max, &
         & lookup_i_sal_min:lookup_i_sal_max, &
         & lookup_i_temp_min:lookup_i_temp_max &
         & ),STAT=error)

     IF (error /= 0) THEN
        CALL sub_report_error( &
             & 'initialize_gem','initialise_gem', &
             & 'Could not allocate space for MyAMI Karg look-up table array', &
             & 'STOPPING', &
             & (/const_real_zero/),.TRUE. &
             & )
     ENDIF
    ! *** read in look-up data ***
    loc_filename = '../../cgenie.muffin/genie-main/data/input/MyAMI_lookup_Karg.dat'
    !call check_unit(in,__LINE__,__FILE__)
    OPEN(unit=in,file=loc_filename,action='read',iostat=ios)
    !call check_iostat(ios,__LINE__,__FILE__)
    ! read in data
    DO a = lookup_i_Ca_min,lookup_i_Ca_max,1
       DO b = lookup_i_Mg_min,lookup_i_Mg_max,1
          DO d = lookup_i_sal_min,lookup_i_sal_max,1
             DO e = lookup_i_temp_min,lookup_i_temp_max,1
                READ(unit=in,FMT='(D23.16)',iostat=ios) lookup_gem_MyAMI_karg(a,b,d,e)
                !call check_iostat(ios,__LINE__,__FILE__)
             END DO
          END DO
       END DO
    END DO
    ! close file pipe
    CLOSE(unit=in,iostat=ios)
    !call check_iostat(ios,__LINE__,__FILE__)
  END SUBROUTINE sub_load_gem_MyAMI_lookup_Karg
  ! ****************************************************************************************************************************** !

  ! ****************************************************************************************************************************** !
  ! LOAD CARBONATE CONSTANT LOOK-UP TABLES - Kw
  SUBROUTINE sub_load_gem_MyAMI_lookup_Kw()
    USE gem_util, ONLY: sub_report_error
    !USE genie_util, ONLY: check_unit, check_iostat
    ! local variables
    INTEGER::a,b,d,e
    CHARACTER(len=255)::loc_filename
    INTEGER::ios ! for file checks
    ! allocate size of look-up tables and load data -- CaCO3
    ! NOTE: check for problems allocating array space
    ALLOCATE(lookup_gem_MyAMI_kW( &
         & lookup_i_Ca_min:lookup_i_Ca_max, &
         & lookup_i_Mg_min:lookup_i_Mg_max, &
         & lookup_i_sal_min:lookup_i_sal_max, &
         & lookup_i_temp_min:lookup_i_temp_max &
         & ),STAT=error)

     IF (error /= 0) THEN
        CALL sub_report_error( &
             & 'initialize_gem','initialise_gem', &
             & 'Could not allocate space for MyAMI Kw look-up table array', &
             & 'STOPPING', &
             & (/const_real_zero/),.TRUE. &
             & )
     ENDIF
    ! *** read in look-up data ***
    loc_filename = '../../cgenie.muffin/genie-main/data/input/MyAMI_lookup_Kw.dat'
    !call check_unit(in,__LINE__,__FILE__)
    OPEN(unit=in,file=loc_filename,action='read',iostat=ios)
    !call check_iostat(ios,__LINE__,__FILE__)
    ! read in data
    DO a = lookup_i_Ca_min,lookup_i_Ca_max,1
       DO b = lookup_i_Mg_min,lookup_i_Mg_max,1
          DO d = lookup_i_sal_min,lookup_i_sal_max,1
             DO e = lookup_i_temp_min,lookup_i_temp_max,1
                READ(unit=in,FMT='(D23.16)',iostat=ios) lookup_gem_MyAMI_kW(a,b,d,e)
                !call check_iostat(ios,__LINE__,__FILE__)
             END DO
          END DO
       END DO
    END DO
    ! close file pipe
    CLOSE(unit=in,iostat=ios)
    !call check_iostat(ios,__LINE__,__FILE__)
  END SUBROUTINE sub_load_gem_MyAMI_lookup_Kw
  ! ****************************************************************************************************************************** !

  ! ****************************************************************************************************************************** !
  ! LOAD CARBONATE CONSTANT LOOK-UP TABLES - K
  SUBROUTINE sub_load_gem_MyAMI_lookup_K0()
    USE gem_util, ONLY: sub_report_error
    !USE genie_util, ONLY: check_unit, check_iostat
    ! local variables
    INTEGER::a,b,d,e
    CHARACTER(len=255)::loc_filename
    INTEGER::ios ! for file checks
    ! allocate size of look-up tables and load data -- CaCO3
    ! NOTE: check for problems allocating array space
    ALLOCATE(lookup_gem_MyAMI_k( &
         & lookup_i_Ca_min:lookup_i_Ca_max, &
         & lookup_i_Mg_min:lookup_i_Mg_max, &
         & lookup_i_sal_min:lookup_i_sal_max, &
         & lookup_i_temp_min:lookup_i_temp_max &
         & ),STAT=error)

     IF (error /= 0) THEN
        CALL sub_report_error( &
             & 'initialize_gem','initialise_gem', &
             & 'Could not allocate space for MyAMI K0 look-up table array', &
             & 'STOPPING', &
             & (/const_real_zero/),.TRUE. &
             & )
     ENDIF
    ! *** read in look-up data ***
    loc_filename = '../../cgenie.muffin/genie-main/data/input/MyAMI_lookup_K0.dat'
    !call check_unit(in,__LINE__,__FILE__)
    OPEN(unit=in,file=loc_filename,action='read',iostat=ios)
    !call check_iostat(ios,__LINE__,__FILE__)
    ! read in data
    DO a = lookup_i_Ca_min,lookup_i_Ca_max,1
       DO b = lookup_i_Mg_min,lookup_i_Mg_max,1
          DO d = lookup_i_sal_min,lookup_i_sal_max,1
             DO e = lookup_i_temp_min,lookup_i_temp_max,1
                READ(unit=in,FMT='(D23.16)',iostat=ios) lookup_gem_MyAMI_k(a,b,d,e)
                !call check_iostat(ios,__LINE__,__FILE__)
             END DO
          END DO
       END DO
    END DO
    ! close file pipe
    CLOSE(unit=in,iostat=ios)
    !call check_iostat(ios,__LINE__,__FILE__)
  END SUBROUTINE sub_load_gem_MyAMI_lookup_K0
  ! ****************************************************************************************************************************** !

  ! ****************************************************************************************************************************** !
  ! DEFINE SCHMIDT NUMBER COEFFICIENTS
  SUBROUTINE sub_def_schmidtnumber()
    ! Schmidt Number coefficients
    ! NOTE: limits from 1:n_atm (not natm) and reversed ordering of tracer and 2nd dimension from 'normal'
    !       because the data for this array reshaped
    ! NOTE: H2S Schmidt Number estimated from molecular weight [Lee Kump, pers com]
    par_Sc_coef(:,:) = reshape( &
         & (/ &
         &      0.0,   0.00, 0.0000, 0.000000, & ! T
         &      0.0,   0.00, 0.0000, 0.000000, & ! Q
         &   2073.1, 125.62, 3.6276, 0.043219, & ! pCO2
         &      0.0,   0.00, 0.0000, 0.000000, & ! pCO2_13C
         &      0.0,   0.00, 0.0000, 0.000000, & ! pCO2_14C
         &   1953.4, 128.00, 3.9918, 0.050091, & ! pO2
         &      0.0,   0.00, 0.0000, 0.000000, & ! d18O_pO2
         &   2206.1, 144.86, 4.5413, 0.056988, & ! pN2
         &      0.0,   0.00, 0.0000, 0.000000, & ! pN2_15N
         &   2039.2, 120.31, 3.4209, 0.040437, & ! CH4
         &      0.0,   0.00, 0.0000, 0.000000, & ! pCH4_13C
         &      0.0,   0.00, 0.0000, 0.000000, & ! pCH4_14C
         &   4039.8, 264.70, 8.2552, 0.103590, & ! pSF6
         &   2301.1, 151.10, 4.7364, 0.059431, & ! pN2O
         &      0.0,   0.00, 0.0000, 0.000000, & ! pN2O_15N
         &   1956.9, 127.20, 3.9979, 0.050878, & ! pH2S
         &      0.0,   0.00, 0.0000, 0.000000, & ! pH2S_34S
         &   4039.8, 264.70, 8.2552, 0.103590, & ! pCFC11
         &   3713.2, 243.40, 7.5879, 0.095215, & ! pCFC12
         &      0.0,   0.00, 0.0000, 0.000000, & ! 
         &      0.0,   0.00, 0.0000, 0.000000  & ! 
         & /), &
         & (/ &
         &   4,n_atm &
         & /) &
         & )
  END SUBROUTINE sub_def_schmidtnumber
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! DEFINE BUNSEN SOLUBILITY COEFFICIENT COEFFICIENTS
  SUBROUTINE sub_def_bunsencoefficient()
    !  Bunsen Solubility Coefficient coefficients
    ! NOTE: limits from 1:n_atm (not natm) and reversed ordering of tracer and 2nd dimension from 'nromal'
    !       because the data for this array reshaped
    ! NOTE: H2S; Lee Kump [per com]
    par_bunsen_coef(:,:) = reshape( &
         & (/ &
         &      0.0000,   0.0000,  0.0000,  0.000000,  0.000000,  0.0000000, & ! T
         &      0.0000,   0.0000,  0.0000,  0.000000,  0.000000,  0.0000000, & ! Q
         &    -60.2409,  93.4517, 23.3585,  0.023517, -0.023656,  0.0047036, & ! pCO2
         &      0.0000,   0.0000,  0.0000,  0.000000,  0.000000,  0.0000000, & ! pCO2_13C
         &      0.0000,   0.0000,  0.0000,  0.000000,  0.000000,  0.0000000, & ! pCO2_14C
         &    -58.3877,  85.8079, 23.8439, -0.034892,  0.015568, -0.0019387, & ! pO2
         &      0.0000,   0.0000,  0.0000,  0.000000,  0.000000,  0.0000000, & ! d18O_pO2
         &    -59.6274,  85.7661, 24.3696, -0.051580,  0.026329, -0.0037252, & ! pN2
         &      0.0000,   0.0000,  0.0000,  0.000000,  0.000000,  0.0000000, & ! pN2_15N
         &    -68.8862, 101.4956, 28.7314, -0.076146,  0.043970, -0.0068672, & ! CH4
         &      0.0000,   0.0000,  0.0000,  0.000000,  0.000000,  0.0000000, & ! pCH4_13C
         &      0.0000,   0.0000,  0.0000,  0.000000,  0.000000,  0.0000000, & ! pCH4_14C
         &   -520.6060, 250.6000, 75.7010, -0.011700,  0.000000,  0.0000000, & ! pSF6
         &    -64.8539, 100.2520, 25.2049, -0.062544,  0.035337, -0.0054699, & ! pN2O
         &      0.0000,   0.0000,  0.0000,  0.000000,  0.000000,  0.0000000, & ! pN2O_15N
         &    -41.0563, 66.40050, 15.1060, -0.060583,  0.037975, -0.0060234, & ! pH2S
         &      0.0000,   0.0000,  0.0000,  0.000000,  0.000000,  0.0000000, & ! pH2S_34S
         &   -136.2685, 206.1150, 57.2805, -0.148598,  0.095114, -0.0163396, & ! pCFC11
         &   -124.4395, 185.4299, 51.6383, -0.149779,  0.094668, -0.0160043, & ! pCFC12
         &      0.0000,   0.0000,  0.0000,  0.000000,  0.000000,  0.0000000, & ! 
         &      0.0000,   0.0000,  0.0000,  0.000000,  0.000000,  0.0000000  & ! 
         & /), &
         & (/ &
         &   6,n_atm &
         & /) &
         & )
  END SUBROUTINE sub_def_bunsencoefficient
  ! ****************************************************************************************************************************** !


END MODULE gem_data
