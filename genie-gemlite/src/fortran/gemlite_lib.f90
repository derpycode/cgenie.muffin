! ******************************************************************************************************************************** !
! gemlite_lib.f90
! Accelerated global biogeochemical cycles
! LIBRARY MODULE
! ******************************************************************************************************************************** !


MODULE gemlite_lib


  use genie_control
  use gem_util
  use gem_carbchem
  IMPLICIT NONE
  SAVE


  ! ****************************************************************************************************************************** !
  ! *** NAMELIST DEFINITIONS ***************************************************************************************************** !
  ! ****************************************************************************************************************************** !

  ! ### EDIT ADD AND/OR EXTEND NAME-LIST PARAMETER AND CONTROL OPTIONS ########################################################### !
  logical::ctrl_update_pCO2                                             ! 
  NAMELIST /ini_gemlite_nml/ctrl_update_pCO2
  real::par_DpCO2_thresh                                                ! threshold for convergence of CO2 repartitioning (atm)
  NAMELIST /ini_gemlite_nml/par_DpCO2_thresh
  ! ############################################################################################################################## !


  ! ****************************************************************************************************************************** !
  ! *** MODEL CONFIGURATION CONSTANTS - ARRAY DIMENSIONS ************************************************************************* !
  ! ****************************************************************************************************************************** !

  ! *** array dimensions ***
  ! grid dimensions
  INTEGER,PARAMETER::n_i = ilon1_ocn                                    ! 
  INTEGER,PARAMETER::n_j = ilat1_ocn                                    ! 
  INTEGER,PARAMETER::n_k = inl1_ocn                                     !


  ! ****************************************************************************************************************************** !
  ! *** MODEL CONFIGURATION CONSTANTS - PARAMETERS ******************************************************************************* !
  ! ****************************************************************************************************************************** !

  ! *** miscellaneous ***
  ! effective thickness of atmosphere (m) in the case of a 1-cell thick atmosphere
  ! NOTE: was 8000.0 m in Ridgwell et al. [2007]
  ! NOTE: must be the same as in *atchem*
  REAL,parameter::par_atm_th = 7777.0


  ! ****************************************************************************************************************************** !
  ! *** GLOBAL VARIABLES AND RUN-TIME SET PARAMETERS ***************************************************************************** !
  ! ****************************************************************************************************************************** !

  ! *** Array definitions ***
  ! tracer arrays
  REAL,ALLOCATABLE,DIMENSION(:,:,:)::atm                                ! atmosphere tracer array
  REAL,ALLOCATABLE,DIMENSION(:,:,:)::datm                               ! atmosphere anomaly tracer array
  REAL,ALLOCATABLE,DIMENSION(:,:,:)::datm_sum                           ! atmosphere anomaly tracer array (summed)
  REAL,ALLOCATABLE,DIMENSION(:,:,:,:)::ocn                              ! ocean tracer array
  REAL,ALLOCATABLE,DIMENSION(:,:,:,:)::docn                             ! ocean anomaly tracer array
  REAL,ALLOCATABLE,DIMENSION(:,:,:,:)::docn_sum                         ! ocean anomaly tracer array (summed)
  ! 'physics'
  REAL,ALLOCATABLE,DIMENSION(:,:)::phys_atm_A                           !
  REAL,ALLOCATABLE,DIMENSION(:,:)::phys_atm_V                           !
  REAL,ALLOCATABLE,DIMENSION(:,:,:)::phys_ocn_Dmid                      ! 
  REAL,ALLOCATABLE,DIMENSION(:,:,:)::phys_ocn_A                         ! 
  REAL,ALLOCATABLE,DIMENSION(:,:,:)::phys_ocn_V                         ! 
  REAL,ALLOCATABLE,DIMENSION(:,:,:)::phys_ocn_mask                      ! 
  REAL,ALLOCATABLE,DIMENSION(:,:)::phys_ocnatm_seaice                   !
  ! aqueous carbonate system [SURFACE ONLY]
  REAL,ALLOCATABLE,DIMENSION(:,:,:)::carb                               ! 
  REAL,ALLOCATABLE,DIMENSION(:,:,:)::carbconst                          ! 
  REAL,ALLOCATABLE,DIMENSION(:,:,:)::carbalk                            ! 
  REAL,ALLOCATABLE,DIMENSION(:,:,:)::carbisor                           ! carbonate (carbon) isotopic properties array

  ! *** copies of GOLDSTEIn variables ***
  ! dimensional scale values for the ocean
  REAL::goldstein_dsc                                                   ! 
  ! depth and location of oceans
  INTEGER,DIMENSION(n_i,n_j)::goldstein_k1                              ! 
  ! miscellaneous
  REAL,DIMENSION(n_k)::goldstein_dz                                     ! 
  REAL,DIMENSION(n_k)::goldstein_dza                                    ! 
  REAL,DIMENSION(0:n_j)::goldstein_sv                                   !

  ! *** MISC ***
  integer::gemcycle_count                                               ! GEMlite cycle counter
  real::gemcycle_mean_fCO2                                              ! mean (ice-free area weighted) fCO2
  real::gemcycle_mean_pCO2                                              ! mean (ice-free area weighted) pCO2
  real::gemcycle_mean_DpCO2_0                                           ! cycle start ocn-atm pCO2 offset


END MODULE gemlite_lib
