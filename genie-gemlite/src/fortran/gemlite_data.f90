! ******************************************************************************************************************************** !
! gemlite_data.f90
! Accelerated global biogeochemical cycles
! DATA LOADING/SAVING/INITIALIZATION ROUTINES
! ******************************************************************************************************************************** !


MODULE gemlite_data

  
  USE gemlite_lib
  IMPLICIT NONE
  SAVE
  
  
CONTAINS
  
  
  ! ****************************************************************************************************************************** !
  ! LOAD GEMlite 'goin' FILE OPTIONS
  SUBROUTINE sub_load_goin_gemlite()
    USE genie_util, ONLY: check_unit,check_iostat
    ! local variables
    integer::ios                                                        !
    ! read data_GEMLTIE file
    call check_unit(in,__LINE__,__FILE__)
    open(unit=in,file='data_GEMLITE',status='old',action='read',iostat=ios)
    if (ios /= 0) then
       print*,'ERROR: could not open GEMLTIE initialisation namelist file'
       stop
    end if
    ! read in namelist and close data_GEMLTIE file
    read(UNIT=in,NML=ini_gemlite_nml,IOSTAT=ios)
    if (ios /= 0) then
       print*,'ERROR: could not read GEMLTIE namelist'
       stop
    else
       close(unit=in)
    end if
if (ctrl_debug_init > 0) then
    ! #### INSERT CODE TO LOAD ADDITIONAL PARAMETERS ############################################################################# !
    print*,'Update pCO2?                                        : ',ctrl_update_pCO2
    print*,'Threshold for convergence of CO2 repartit. (atm)    : ',par_DpCO2_thresh
    ! ############################################################################################################################ !
end if
  END SUBROUTINE sub_load_goin_gemlite
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALIZE 'PHYSICS' - OCEAN
  SUBROUTINE sub_init_phys_ocn()
    ! local variables
    INTEGER::i,j,k
    REAL,DIMENSION(0:n_k+1)::loc_grid_dz,loc_grid_dza
    ! initialize local variables
    loc_grid_dz(0:n_k+1)  = 0.0
    loc_grid_dz(1:n_k)    = goldstein_dz(:)
    loc_grid_dza(0:n_k+1) = 0.0
    loc_grid_dza(1:n_k)   = goldstein_dza(:); loc_grid_dza(n_k) = loc_grid_dz(n_k)/2.0
    ! initialise arrays
    phys_atm_A(:,:)         = 0.0
    phys_atm_V(:,:)         = 0.0
    phys_ocn_Dmid(:,:,:)    = 0.0
    phys_ocn_A(:,:,:)       = 0.0
    phys_ocn_V(:,:,:)       = 0.0
    phys_ocn_mask(:,:,:)    = 0.0
    phys_ocnatm_seaice(:,:) = 0.0
    ! initialize array values
    ! NOTE: initialize basic grid structure values for the (i,j,k) grid, not just ocean-only points
    ! NOTE: depth in in unit of m BELOW sealevel (i.e., a +ve scale)
    ! NOTE: set default rho
    DO i=1,n_i
       DO j=1,n_j
          phys_atm_A(i,j)    = 2.0*const_pi*(const_rEarth**2)*(1.0/real(n_i))*(goldstein_sv(j) - goldstein_sv(j-1))
          phys_atm_V(i,j)    = par_atm_th*phys_atm_A(i,j)
          DO k=goldstein_k1(i,j),n_k
             phys_ocn_Dmid(i,j,k) = SUM(goldstein_dsc*loc_grid_dza(k:n_k))
             phys_ocn_A(i,j,k)    = 2.0*const_pi*(const_rEarth**2)*(1.0/real(n_i))*(goldstein_sv(j) - goldstein_sv(j-1))
             phys_ocn_V(i,j,k)    = goldstein_dsc*loc_grid_dz(k)*phys_ocn_A(i,j,k)
             phys_ocn_mask(i,j,k) = 1.0
          END DO
       END DO
    END DO
  END SUBROUTINE sub_init_phys_ocn
  ! ****************************************************************************************************************************** !


END MODULE gemlite_data
