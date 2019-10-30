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
    par_gem_indir_name = trim(par_gem_indir_name)//'/'
if (ctrl_debug_init > 0) then
    ! --- TRACER SELECTION  ------------------------------------------------------------------------------------------------------ !
    ! NOTE: reported at end of initialise_gem when tracer name information is available
    ! --- MISC CONTROLS  --------------------------------------------------------------------------------------------------------- !
    print*,'--- GEOCHEM CONTROLS ---'
    print*,'carbonate dissociation constants set                : ',trim(par_carbconstset_name)
    print*,'pH solution tolerance                               : ',par_carbchem_pH_tolerance
    print*,'pH solution maximum number of iterations            : ',par_carbchem_pH_iterationmax
    print*,'Exit upon pH solution failure?                      : ',ctrl_carbchem_fail
    print*,'minimum T used in empirical geochem calculations    : ',par_geochem_Tmin 
    print*,'maximum T used in empirical geochem calculations    : ',par_geochem_Tmax
    print*,'minimum S used in empirical geochem calculations    : ',par_geochem_Smin 
    print*,'maximum S used in empirical geochem calculations    : ',par_geochem_Smax 
    print*,'minimum T used in empirical carbchem calculations   : ',par_carbchem_Tmin 
    print*,'maximum T used in empirical carbchem calculations   : ',par_carbchem_Tmax
    print*,'minimum S used in empirical carbchem calculations   : ',par_carbchem_Smin 
    print*,'maximum S used in empirical carbchem calculations   : ',par_carbchem_Smax 
    print*,'--- MISC CONTROLS ---'
    print*,'assumed longitudinal offset of the grid             : ',par_grid_lon_offset
    print*,'Debug (initialization) level                        : ',ctrl_debug_init
    print*,'Debug (loop) level                                  : ',ctrl_debug_loop
    print*,'Debug (end) level                                   : ',ctrl_debug_end
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
