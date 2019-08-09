
! ******************************************************************************************************************************** !
! TIMESTEP AtChem
SUBROUTINE atchem(    &
     & dum_dts,       &
     & dum_sfxsumatm, &
     & dum_sfcatm     &
     )
  USE atchem_lib
  USE atchem_box
  IMPLICIT NONE
  ! dummy arguments
  real,intent(in)::dum_dts  
  real,dimension(n_atm,n_i,n_j),intent(inout)::dum_sfxsumatm
  real,dimension(n_atm,n_i,n_j),intent(inout)::dum_sfcatm
  ! local variables
  integer::ia,l,i,j                                                     ! 
  real::loc_dtyr                                                        ! 
  REAL::loc_atm_tot_V                                                   ! 
  REAL,DIMENSION(n_atm)::loc_atm_tot                                    !  
  REAL,DIMENSION(n_i,n_j)::loc_conv_atm_mol,loc_conv_mol_atm            !
  REAL,DIMENSION(n_atm,n_i,n_j)::locij_fatm                             ! local flux to atmosphere (mol)
  REAL,DIMENSION(n_atm)::loc_fracdecay_atm                              ! local reduction factor for decaying atmospheric tracers

  ! *** INITIALIZE LOCAL VARIABLES ***
  locij_fatm(:,:,:) = 0.0

  ! *** CALCULATE LOCAL CONSTANTS ***
  ! local constants for converting between partial pressure and molar quantity
  ! NOTE: atm(ia_T,:,:) in C
  loc_conv_atm_mol(:,:) = phys_atm(ipa_V,:,:)/(conv_Pa_atm*const_R_SI*(atm(ia_T,:,:)+const_zeroC))
  loc_conv_mol_atm(:,:) = 1.0/loc_conv_atm_mol(:,:)
  ! time
  loc_dtyr = dum_dts/conv_yr_s
  ! fractional reduction factors for decaying isotopes
  loc_fracdecay_atm(:) = EXP(-loc_dtyr*const_lambda_atm(:))

  ! *** OXIDIZE CH4 [NOTE: for schemes that initially homogenize atmospheric composition]***
  select case (par_atm_CH4_photochem)
     case ('claire06')
      IF (atm_select(ia_pCH4) .AND. atm_select(ia_pCO2) .AND. atm_select(ia_pO2)) THEN
         CALL sub_calc_oxidize_CH4_claire06(loc_dtyr,loc_conv_atm_mol(:,:))
      END IF
     case('claire06_fixed')
      IF (atm_select(ia_pCH4) .AND. atm_select(ia_pCO2) .AND. atm_select(ia_pO2)) THEN
         CALL sub_calc_oxidize_CH4_claire06_fixed(loc_dtyr,loc_conv_atm_mol(:,:))
      END IF
     case ('claire06H')
      IF (atm_select(ia_pCH4) .AND. atm_select(ia_pCO2) .AND. atm_select(ia_pO2)) THEN
         CALL sub_calc_oxidize_CH4_claire06H(loc_dtyr,loc_conv_atm_mol(:,:))
      END IF
     case ('goldblatt06')
      IF (atm_select(ia_pCH4) .AND. atm_select(ia_pCO2) .AND. atm_select(ia_pO2)) THEN
         CALL sub_calc_oxidize_CH4_goldblatt06(loc_dtyr,loc_conv_atm_mol(:,:))
      END IF
  case default
  !!! NOTHING
  end select
  
  
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> !
  ! *** (i,j) GRID PT LOOP START *** !
  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> !
              
  block_iloop: DO i=1,n_i
     block_jloop: DO j=1,n_j 
        
        ! *** DECAY RADIOACTIVE TRACERS ***
        DO l=3,n_l_atm
           ia = conv_iselected_ia(l)
           ! radioactive decay of isotopes
           IF (abs(const_lambda_atm(ia)).gt.const_real_nullsmall) THEN
              atm(ia,i,j) = loc_fracdecay_atm(ia)*atm(ia,i,j)
           END if
        end do
  
        ! *** OXIDIZE CH4 ***
        select case (par_atm_CH4_photochem)
        case ('default')      
           IF (atm_select(ia_pCH4) .AND. atm_select(ia_pCO2) .AND. atm_select(ia_pO2)) THEN
              CALL sub_calc_oxidize_CH4_default(i,j,loc_dtyr)
           END IF
        case ('snowball')      
           IF (atm_select(ia_pCH4) .AND. atm_select(ia_pCO2) .AND. atm_select(ia_pO2)) THEN
              if (sum(dum_sfcatm(ia_T,:,:))/size(dum_sfcatm(ia_T,:,:)) > 0.0) then
                 IF (atm(ia_pCH4,i,j) > 700.0E-9) THEN
                    CALL sub_calc_oxidize_CH4_default(i,j,loc_dtyr)
                 END if
              END IF
           END IF
        case ('schmidt03')
           IF (atm_select(ia_pCH4) .AND. atm_select(ia_pCO2) .AND. atm_select(ia_pO2)) THEN
              CALL sub_calc_oxidize_CH4_schmidt03(i,j,loc_dtyr)
           END IF
        case default
            !!! NOTHING
        end select

        ! *** ADD CH4 ***
        IF (atm_select(ia_pCH4) .AND. atm_select(ia_pCO2) .AND. atm_select(ia_pO2)) THEN
           CALL sub_calc_wetlands_CH4(loc_dtyr,locij_fatm(:,i,j))
        END IF
        
        ! *** PRODUCE RADIOACTIVE TRACERS ***
        IF (atm_select(ia_pCO2_14C)) THEN
           CALL sub_calc_generate_14C(loc_dtyr,locij_fatm(:,i,j))
        END IF
        
        ! *** EXCHANGE CO2 WITH A VIRTUAL TERRESTRIAL RESERVOIR ***
        IF ((par_atm_FterrCO2exchange > const_real_nullsmall) .AND. atm_select(ia_pCO2) .AND. atm_select(ia_pCO2_13C)) THEN
           CALL sub_calc_terrCO2exchange(i,j,loc_dtyr,locij_fatm(:,i,j))
        END IF
        
     END DO block_jloop
  END DO block_iloop
  
  ! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< !
  ! *** (i,j) GRID PT LOOP END *** !
  ! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<< !

  ! *** UPDATE ATMOSPHERIC COMPOSITION ***
  ! set internal atmospheric flux
  fatm(:,:,:) = dum_sfxsumatm(:,:,:)
  ! NOTE: flux <fatm> in (mol m-2 per timestep)
  ! update atmospheric composition
  ! NOTE: units of partial pressure (atm)
  ! NOTE: carry out at every (i.e, wet + dry) grid point
  DO l=3,n_l_atm
     ia = conv_iselected_ia(l)
     ! update atmospheric tracers
     atm(ia,:,:) = atm(ia,:,:) + loc_conv_mol_atm(:,:)*phys_atm(ipa_A,:,:)*fatm(ia,:,:) + loc_conv_mol_atm(:,:)*locij_fatm(ia,:,:)
     ! <HACK TO HOMOGENIZE ATMOSPHERIC COMPOSITION>
     ! homogenize the partial pressure of tracers in the atmopshere across (all grid points)
     loc_atm_tot(ia) = SUM(loc_conv_atm_mol(:,:)*atm(ia,:,:))
     loc_atm_tot_V = SUM(phys_atm(ipa_V,:,:))
     atm(ia,:,:) = (loc_atm_tot(ia)/loc_atm_tot_V)*conv_Pa_atm*const_R_SI*(atm(ia_T,:,:)+const_zeroC)
  end do

  ! *** UPDATE INTERFACE ARRAYS ***
  ! return new <atm>
  dum_sfcatm(:,:,:) = atm(:,:,:)
  ! reset integrated flux array
  dum_sfxsumatm(:,:,:) = 0.0

END SUBROUTINE atchem
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! ATCHEM LOOP SUBROUTINE - CLIMATE STATE UPDATE
subroutine atchem_climate( &
     & dum_tstar_atm,      &
     & dum_surf_qstar_atm  &
     & )
  USE atchem_lib
  IMPLICIT NONE
  ! DUMMY ARGUMENTS
  REAL,DIMENSION(n_i,n_j),INTENT(in)::dum_tstar_atm            !
  REAL,DIMENSION(n_i,n_j),INTENT(in)::dum_surf_qstar_atm       !

  ! *** UPDATE CLIMATE PROPERTIES ***
  atm(ia_T,:,:) = dum_tstar_atm(:,:)
  atm(ia_q,:,:) = dum_surf_qstar_atm(:,:)

end subroutine atchem_climate
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! RESTART AtChem (save data)
SUBROUTINE atchem_save_rst(dum_genie_clock)
  USE atchem_lib
  use atchem_data_netCDF
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
  loc_yr = real(dum_genie_clock)/(1000.0*conv_yr_s)
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
     WRITE(unit=out)                                    &
          & n_l_atm,                                    &
          & (conv_iselected_ia(l),l=1,n_l_atm),         &
          & (atm(conv_iselected_ia(l),:,:),l=1,n_l_atm)
     close(unit=out)
  end IF
  ! ---------------------------------------------------------- !
  ! END
  ! ---------------------------------------------------------- !
END SUBROUTINE atchem_save_rst
! ******************************************************************************************************************************** !
