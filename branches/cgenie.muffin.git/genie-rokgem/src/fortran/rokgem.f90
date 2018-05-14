! File: rokgem.f90
!
! Contains main time-stepping subroutine for RokGeM  
!
! Subroutine: rokgem
!
! Main time-stepping subroutine for RokGeM 
!
! Uses:
!
! - <rokgem_lib.f90>
! - <rokgem_data.f90>
! - <rokgem_box.f90>
! 
! Calls:
!
! - <sub_glob_avg_weath>
! - <sub_GKWM>
! - <sub_GEM_CO2>
! - <sum_calcium_flux_CaSi>
!
! Input:
!
! dum_sfcatm1 - atmosphere composition interface array
! dum_runoff - run-off to be read in from exernal module (EMBM, or ENTS)
! dum_photo(n_i,n_j) - photosynthesis array from land veg module (ENTS)
! dum_respveg(n_i,n_j) - vegetation respiration array from land veg module (ENTS). 
! NOTE - run_off, photo and respveg only work for same grid as RokGeM at the moment
!
! Output:
!
! dum_sfxrok - ocean flux interface array (same no of tracers as used in biogem ocean)
! dum_sfxatm1 - atmosphere flux interface array (same no of tracers as used in atchem atmosphere)

subroutine rokgem (dum_dts,dum_sfcatm1,dum_runoff,dum_photo,dum_respveg,dum_sfxrok,dum_sfxatm1)   

  use rokgem_lib
  use rokgem_data
  use rokgem_box

  IMPLICIT NONE

  ! dummy variables
  REAL,INTENT(in)::dum_dts                                              ! time-step
  REAL,INTENT(in)         :: dum_sfcatm1(n_atm,n_io,n_jo)         ! atmosphere composition interface array
  REAL,INTENT(in)         :: dum_runoff(n_i,n_j)                  ! run-off to be read in from exernal module (EMBM, or ENTS)
  REAL,INTENT(in)               :: dum_photo(n_i,n_j)           ! photosythesis from land veg module (ENTS)
  REAL,INTENT(in)               :: dum_respveg(n_i,n_j)         ! vegetation respiration from land veg module (ENTS)
  ! -> NOTE - run_off, photo and respveg only work 
  ! for same grid as RokGeM at the moment
  REAL,INTENT(inout)              :: dum_sfxrok(n_ocn,n_i,n_j)   ! ocean flux interface array
  !  (same no of tracers as used in biogem ocean)
  REAL,INTENT(inout)            :: dum_sfxatm1(n_atm,n_io,n_jo) ! atmosphere flux interface array

  ! increment timestep counter
  tstep_count = tstep_count + 1

  ! if output due then change year
  CALL sub_output_year()

  ! reset rokgem flux array
  dum_sfxrok(:,:,:) = 0.0

  ! calculate weathering fluxes dependent on chosen scheme, and then dump them into the relevant points on the coast
  SELECT case (par_weathopt)
  case ('Global_avg')
     ! global average weathering
     CALL sub_glob_avg_weath(dum_dts,dum_sfcatm1,dum_runoff,dum_photo,dum_respveg,dum_sfxrok,dum_sfxatm1)
     ! Gibbs et al (1999) 2D lithology-dependent weathering
  case ('GKWM')
     CALL sub_GKWM(dum_runoff,lithology,calcium_flux)
     CALL sum_calcium_flux_CaSi(calcium_flux,total_calcium_flux_Ca,total_calcium_flux_Si)
     CALL sub_2D_weath(dum_sfcatm1,dum_runoff,dum_photo,dum_respveg,dum_sfxrok,dum_sfxatm1)
     ! Amiotte-Suchet et al (2003) 2D lithology-dependent weathering
  case ('GEM_CO2')
     CALL sub_GEM_CO2(dum_runoff,lithology,calcium_flux)
     CALL sum_calcium_flux_CaSi(calcium_flux,total_calcium_flux_Ca,total_calcium_flux_Si)
     CALL sub_2D_weath(dum_sfcatm1,dum_runoff,dum_photo,dum_respveg,dum_sfxrok,dum_sfxatm1)
  end SELECT

  ! if output then increment output counter
  CALL sub_output_counters()

end subroutine rokgem

! Subroutine: rest_rokgem
!
! Restart RokGeM - saves netcdf record number for use in appending data to netcdfs when using restarts
!
! Uses:
!
! - <rokgem_lib.f90>

SUBROUTINE rest_rokgem()

  USE rokgem_lib

  IMPLICIT NONE

  ! local variables
  integer::ios
  CHARACTER(len=255)::loc_filename

  if (debug_init > 1) PRINT*,'saving netcdf record number',ncout2d_ntrec_rg

  ! dump restart data
  loc_filename = TRIM(par_outdir_name)//trim(par_outfile_name)
  OPEN(20,status='replace',file=loc_filename,form='formatted',action='write',iostat=ios)
  !call check_iostat(ios,__LINE__,__FILE__)
  WRITE(20,fmt='(i6)') ncout2d_ntrec_rg                             
  close(20)


  !        Conditionals commented out because no calibration is done for stage 1 spin-up 
  ! and files are needed to be written for stage 2.
  !        IF (opt_calibrate_T_2D) THEN
  if (debug_init > 1) PRINT*,'saving 2D temperature reference field for calibration: ',TRIM(par_ref_T0_2D)
  OPEN(20,status='replace',file=TRIM(par_outdir_name)//'rg_par_ref_T0_2D',form='formatted',action='write',iostat=ios)
  WRITE(20,fmt='(A100)') par_ref_T0_2D
  CLOSE(20)
  CALL sub_save_data_ij(TRIM(par_outdir_name)//TRIM(par_ref_T0_2D),n_i,n_j,ref_T0_2D(:,:))   
  !        ENDIF
  !        IF (opt_calibrate_R_2D) THEN
  if (debug_init > 1) PRINT*,'saving 2D runoff reference field for calibration: ',TRIM(par_ref_R0_2D)
  OPEN(20,status='replace',file=TRIM(par_outdir_name)//'rg_par_ref_R0_2D',form='formatted',action='write',iostat=ios)
  WRITE(20,fmt='(A100)') par_ref_R0_2D
  CLOSE(20)
  CALL sub_save_data_ij(TRIM(par_outdir_name)//TRIM(par_ref_R0_2D),n_i,n_j,ref_R0_2D(:,:))   
  !        ENDIF
  !        IF (opt_calibrate_P_2D) THEN
  if (debug_init > 1) PRINT*,'saving 2D productivity reference field for calibration: ',TRIM(par_ref_P0_2D)
  OPEN(20,status='replace',file=TRIM(par_outdir_name)//'rg_par_ref_P0_2D',form='formatted',action='write',iostat=ios)
  WRITE(20,fmt='(A100)') par_ref_P0_2D
  CLOSE(20)
  CALL sub_save_data_ij(TRIM(par_outdir_name)//TRIM(par_ref_P0_2D),n_i,n_j,ref_P0_2D(:,:))   
  !        ENDIF

END SUBROUTINE rest_rokgem


! ******************************************************************************************************************************** !
! REINITIALIZE ROKGEM OCN INTERFACE FLUX ARRAY
subroutine reinit_flux_rokatm( &
     & dum_sfxsumatm1               &
     & )
  USE rokgem_lib
  IMPLICIT NONE
  ! dummy arguments
  REAL,dimension(n_atm,n_i,n_j),intent(inout)::dum_sfxsumatm1           ! 

  ! *** RE-INITIALIZE VARIABLES ***
  ! reset cumulative weathering array
  dum_sfxsumatm1(:,:,:) = 0.0

end subroutine reinit_flux_rokatm
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! REINITIALIZE ROKGEM OCN INTERFACE FLUX ARRAY
subroutine reinit_flux_rokocn( &
     & dum_sfxsumrok1               &
     & )
  USE rokgem_lib
  IMPLICIT NONE
  ! dummy arguments
  REAL,dimension(n_ocn,n_i,n_j),intent(inout)::dum_sfxsumrok1           ! 

  ! *** RE-INITIALIZE VARIABLES ***
  ! reset cumulative weathering array
  dum_sfxsumrok1(:,:,:) = 0.0

end subroutine reinit_flux_rokocn
! ******************************************************************************************************************************** !
