! ******************************************************************************************************************************** !
! atchem_box.f90
! Atmosphere Chemistry
! MISCELLANEOUS ROUTINES
! ******************************************************************************************************************************** !


MODULE atchem_box


  USE atchem_lib
  IMPLICIT NONE
  SAVE


CONTAINS


  ! ****************************************************************************************************************************** !
  ! EXCHANGE CARBON WITH A VIRTUAL TERRESTRIAL RESERVOIR
  SUBROUTINE sub_calc_terrCO2exchange(dum_i,dum_j,dum_dtyr,dum_fatm)
    IMPLICIT NONE
    ! dummy arguments
    INTEGER,INTENT(in)::dum_i,dum_j
    real,intent(in)::dum_dtyr
    REAL,DIMENSION(n_atm),intent(inout)::dum_fatm              ! net flux to atmosphere (mol)
    ! local variables
    real::loc_Fatm,loc_Fterr                                   ! flux to atm, flux to terrestrial biosphere
    real::loc_Ratm,loc_Rterr                                   ! local isotopic variables

    ! *** INITIALIZE LOCAL VARIABLES ***
    loc_Fatm  = dum_dtyr*par_atm_FterrCO2exchange/real(n_i*n_j)
    loc_Fterr = dum_dtyr*par_atm_FterrCO2exchange/real(n_i*n_j)
    loc_Ratm = atm(ia_pCO2_13C,dum_i,dum_j)/atm(ia_pCO2,dum_i,dum_j)
    loc_Rterr = atm_slabbiosphere(ia_pCO2_13C,dum_i,dum_j)/atm_slabbiosphere(ia_pCO2,dum_i,dum_j)
        
    ! *** EXCHANGE CO2 ***
    ! NOTE: atm_slabbiosphere in units of mol
    ! bulk CO2
    dum_fatm(ia_pCO2) = dum_fatm(ia_pCO2) + loc_Fatm - loc_Fterr
    atm_slabbiosphere(ia_pCO2,dum_i,dum_j) = &
         & atm_slabbiosphere(ia_pCO2,dum_i,dum_j) - loc_Fatm + loc_Fterr
    ! d13C
    dum_fatm(ia_pCO2_13C) = dum_fatm(ia_pCO2_13C) + loc_Rterr*loc_Fatm - loc_Ratm*loc_Fterr
    atm_slabbiosphere(ia_pCO2_13C,dum_i,dum_j) = &
         & atm_slabbiosphere(ia_pCO2_13C,dum_i,dum_j) - loc_Rterr*loc_Fatm + loc_Ratm*loc_Fterr

  END SUBROUTINE sub_calc_terrCO2exchange
  ! ****************************************************************************************************************************** !

  ! *****************************************************************************************************************************!
  ! OXIDIZE CH4 -- DEFAULT (ORIGINAL) SCHEME
  SUBROUTINE sub_calc_oxidize_CH4_default(dum_i,dum_j,dum_dtyr)
    IMPLICIT NONE
    ! dummy arguments
    INTEGER,INTENT(in)::dum_i,dum_j
    real,intent(in)::dum_dtyr
    ! local variables
    real::loc_tau
    real::loc_fracdecay
    real::loc_CH4 

    ! *** CALCULATE LOCAL CONSTANTS ***
    ! atmospheric lifetime from Osborn and Wigley [1994]
    ! NOTE1: restrict lifetime calculation to 0.5*CH4(0) at the lower end (limit of calibration curve)
    ! NOTE2: strictly, calibration curve ends at 4.0*CH4(0) in Osborn and Wigley [1994]
    ! NOTE3: omitting [OH], [NOx] etc etc
    loc_CH4 = atm(ia_pCH4,dum_i,dum_j)
    if (loc_CH4 < 0.5*const_pCH4_oxidation_C0) loc_CH4 = 0.5*const_pCH4_oxidation_C0
    loc_tau = const_pCH4_oxidation_tau0*(loc_CH4/const_pCH4_oxidation_C0)**const_pCH4_oxidation_N
    loc_fracdecay = dum_dtyr/loc_tau

    ! *** ATMOSPHERIC CH4->CO2 ***
    atm(ia_pO2,dum_i,dum_j)      = atm(ia_pO2,dum_i,dum_j)      - 2.0*loc_fracdecay*atm(ia_pCH4,dum_i,dum_j)
    atm(ia_pCO2,dum_i,dum_j)     = atm(ia_pCO2,dum_i,dum_j)     + loc_fracdecay*atm(ia_pCH4,dum_i,dum_j)
    atm(ia_pCO2_13C,dum_i,dum_j) = atm(ia_pCO2_13C,dum_i,dum_j) + loc_fracdecay*atm(ia_pCH4_13C,dum_i,dum_j)
    atm(ia_pCO2_14C,dum_i,dum_j) = atm(ia_pCO2_14C,dum_i,dum_j) + loc_fracdecay*atm(ia_pCH4_14C,dum_i,dum_j)
    atm(ia_pCH4,dum_i,dum_j)     = (1.0 - loc_fracdecay)*atm(ia_pCH4,dum_i,dum_j)
    atm(ia_pCH4_13C,dum_i,dum_j) = (1.0 - loc_fracdecay)*atm(ia_pCH4_13C,dum_i,dum_j)
    atm(ia_pCH4_14C,dum_i,dum_j) = (1.0 - loc_fracdecay)*atm(ia_pCH4_14C,dum_i,dum_j)
    
  END SUBROUTINE sub_calc_oxidize_CH4_default
  ! ****************************************************************************************************************************** !

  ! *****************************************************************************************************************************!
  ! OXIDIZE CH4 -- FIT TO DATA FROM 2-D PHOTOCHEMISTRY MODEL IN SCHMIDT & SCHINDELL [2003] (CTR|12-2017)
  SUBROUTINE sub_calc_oxidize_CH4_schmidt03(dum_i,dum_j,dum_dtyr)
    IMPLICIT NONE
    ! dummy arguments
    INTEGER,INTENT(in)::dum_i,dum_j
    real,intent(in)::dum_dtyr
    ! local variables
    real::loc_tau
    real::loc_fracdecay
    real::loc_CH4

    ! *** CALCULATE LOCAL CONSTANTS ***
    ! atmospheric lifetime from Schmidt and Shindell [2003]
    ! NOTE1: restrict lifetime calculation to 1.0*CH4(0) at the lower end (limit of calibration curve)
    ! NOTE2: strictly, calibration curve ends at 200.0*CH4(0) in Schmidt and Shindell [2003]
    ! NOTE3: 2-D model includes HOx-NOx-Ox-CO-CH4 chemistry
    loc_CH4 = atm(ia_pCH4,dum_i,dum_j)
    if (loc_CH4 < par_pCH4_oxidation_C0) loc_CH4 = par_pCH4_oxidation_C0
    loc_tau = par_pCH4_oxidation_tau0*(loc_CH4/par_pCH4_oxidation_C0)**par_pCH4_oxidation_N
    loc_fracdecay = dum_dtyr/loc_tau

    ! *** ATMOSPHERIC CH4->CO2 ***
    atm(ia_pO2,dum_i,dum_j)      = atm(ia_pO2,dum_i,dum_j)      - 2.0*loc_fracdecay*atm(ia_pCH4,dum_i,dum_j)
    atm(ia_pCO2,dum_i,dum_j)     = atm(ia_pCO2,dum_i,dum_j)     + loc_fracdecay*atm(ia_pCH4,dum_i,dum_j)
    atm(ia_pCO2_13C,dum_i,dum_j) = atm(ia_pCO2_13C,dum_i,dum_j) + loc_fracdecay*atm(ia_pCH4_13C,dum_i,dum_j)
    atm(ia_pCO2_14C,dum_i,dum_j) = atm(ia_pCO2_14C,dum_i,dum_j) + loc_fracdecay*atm(ia_pCH4_14C,dum_i,dum_j)
    atm(ia_pCH4,dum_i,dum_j)     = (1.0 - loc_fracdecay)*atm(ia_pCH4,dum_i,dum_j)
    atm(ia_pCH4_13C,dum_i,dum_j) = (1.0 - loc_fracdecay)*atm(ia_pCH4_13C,dum_i,dum_j)
    atm(ia_pCH4_14C,dum_i,dum_j) = (1.0 - loc_fracdecay)*atm(ia_pCH4_14C,dum_i,dum_j)

  END SUBROUTINE sub_calc_oxidize_CH4_schmidt03
  ! ****************************************************************************************************************************** !

  ! ****************************************************************************************************************************** !
  ! OXIDIZE CH4 -- PHOTOCHEMICAL SCHEME AFTER CLAIRE ET AL. [2006], NO H ESCAPE (CTR|01-2018)
   SUBROUTINE sub_calc_oxidize_CH4_claire06(dum_dtyr,dum_conv_atm_mol)
     IMPLICIT NONE
     ! dummy arguments
     real,intent(in)::dum_dtyr
     real,dimension(n_i,n_j),intent(in)::dum_conv_atm_mol
     ! local variables
     real::loc_tau
     real::loc_fracdecay
     real::loc_O2, loc_CH4, loc_CO2
     real::loc_13CH4, loc_14CH4
     real::loc_r13CH4, loc_r14CH4
     real::loc_13CO2, loc_14CO2
     real::loc_r13CO2, loc_r14CO2
     real::loc_atmV
     real::loc_p00, loc_p10, loc_p01, loc_p20, loc_p11, loc_p02, loc_p30, loc_p21, loc_p12,    &
           &  loc_p03, loc_p40, loc_p31, loc_p22, loc_p13, loc_p04, loc_p50, loc_p41, loc_p32, &
           &  loc_p23, loc_p14, loc_p05
     real::loc_phi_o2, loc_phi_ch4, loc_k
     real::loc_oxrate
     
     ! sum tracers
     loc_O2  = SUM(dum_conv_atm_mol*atm(ia_pO2,:,:))
     loc_CH4 = SUM(dum_conv_atm_mol*atm(ia_pCH4,:,:))
     loc_CO2 = SUM(dum_conv_atm_mol*atm(ia_pCO2,:,:))

     loc_13CH4 = SUM(dum_conv_atm_mol*atm(ia_pCH4_13C,:,:))
     loc_14CH4 = SUM(dum_conv_atm_mol*atm(ia_pCH4_14C,:,:))
     loc_13CO2 = SUM(dum_conv_atm_mol*atm(ia_pCO2_13C,:,:))
     loc_14CO2 = SUM(dum_conv_atm_mol*atm(ia_pCO2_13C,:,:))

     loc_r13CH4 = loc_13CH4/loc_CH4
     loc_r14CH4 = loc_14CH4/loc_CH4
     loc_r13CO2 = loc_13CO2/loc_CO2
     loc_r14CO2 = loc_14CO2/loc_CO2

     loc_atmV = SUM(phys_atm(ipa_V,:,:))

     ! CH4-O3-O2 photochemistry after Claire et al. [2006] // no H escape from the atmosphere
     ! NOTE: neither O2 nor CH4 are truncated at edge of training values here, so USE CAUTION
     ! NOTE: CH4 truncated lifetime at lower limit below [see Schmidt & Shindell 2003]

     IF (loc_O2 > 1.0E8) THEN ! proceed with CH4 oxidation

     ! *** LOCAL CONSTANTS ***
     loc_p00 =  1.712
     loc_p10 = -0.3212
     loc_p01 = -1.97
     loc_p20 = -0.2595
     loc_p11 =  0.02261
     loc_p02 =  0.6206
     loc_p30 = -0.01508
     loc_p21 =  0.1081
     loc_p12 = -0.03527
     loc_p03 = -0.1487
     loc_p40 =  0.003142
     loc_p31 = -0.003905
     loc_p22 = -0.01894
     loc_p13 =  0.01487
     loc_p04 =  0.01797
     loc_p50 =  0.0001997
     loc_p41 = -0.000598
     loc_p32 =  0.0001878
     loc_p23 =  0.001942
     loc_p14 = -0.001568
     loc_p05 = -0.0009482

     ! *** CALCULATE METHANE OXIDATION RATE ***
     ! NOTE: fit in Tmol CH4 per y, but tracers summed as mol
     loc_phi_o2  = LOG10(loc_O2*1.0E-12)                        ! convert to Tmol
     loc_phi_ch4 = LOG10(loc_CH4*1.0E-12)                       ! convert to Tmol
     loc_k = (10.0**( loc_p00                                       &
                     &  + loc_p10*loc_phi_o2                        &
                     &  + loc_p01*loc_phi_ch4                       &
                     &  + loc_p20*loc_phi_o2**2                     &
                     &  + loc_p11*loc_phi_o2*loc_phi_ch4            &
                     &  + loc_p02*loc_phi_ch4**2                    &
                     &  + loc_p30*loc_phi_o2**3                     &
                     &  + loc_p21*(loc_phi_o2**2)*loc_phi_ch4       &
                     &  + loc_p12*loc_phi_o2*(loc_phi_ch4**2)       &
                     &  + loc_p03*loc_phi_ch4**3                    &
                     &  + loc_p40*loc_phi_o2**4                     &
                     &  + loc_p31*(loc_phi_o2**3)*loc_phi_ch4       &
                     &  + loc_p22*(loc_phi_o2**2)*(loc_phi_ch4**2)  &
                     &  + loc_p13*loc_phi_o2*(loc_phi_ch4**3)       &
                     &  + loc_p04*loc_phi_ch4**4                    &
                     &  + loc_p50*loc_phi_o2**5                     &
                     &  + loc_p41*(loc_phi_o2**4)*loc_phi_ch4       &
                     &  + loc_p32*(loc_phi_o2**3)*(loc_phi_ch4**2)  &
                     &  + loc_p23*(loc_phi_o2**2)*(loc_phi_ch4**3)  &
                     &  + loc_p14*loc_phi_o2*(loc_phi_ch4**4)       &
                     &  + loc_p05*loc_phi_ch4**5))*1.0E-12          ! converted to mol-1 y-1
     loc_oxrate = loc_k*loc_O2*loc_CH4     
     loc_tau    = max(loc_CH4/loc_oxrate,7.6161)                 ! in yr
     loc_fracdecay = dum_dtyr/loc_tau
       
     ! *** PERFORM METHANE OXIDATION ***
     ! NOTE: stoichiometry of CH4 oxidation : CH4 + 2O2 + hv  --> CO2 + 2H2O
     loc_CH4     = loc_CH4   - loc_fracdecay*loc_CH4
     loc_13CH4   = loc_13CH4 - loc_fracdecay*loc_13CH4
     loc_14CH4   = loc_14CH4 - loc_fracdecay*loc_14CH4
     loc_CO2     = loc_CO2   + loc_fracdecay*loc_CH4
     loc_13CO2   = loc_13CO2 + loc_fracdecay*loc_13CH4
     loc_14CO2   = loc_14CO2 + loc_fracdecay*loc_14CH4
     loc_O2      = loc_O2    - 2.0*loc_fracdecay*loc_CH4

     END IF     

     ! *** ADJUST INVENTORIES TO AVOID -VE CH4 ***
     IF (loc_CH4 < const_real_zero) THEN
      loc_CH4   = const_real_zero
      loc_13CH4 = loc_13CH4 - loc_r13CH4*(loc_CH4)
      loc_14CH4 = loc_14CH4 - loc_r14CH4*(loc_CH4)
      loc_CO2   = loc_CO2   + loc_CH4
      loc_13CO2 = loc_13CO2 + loc_r13CH4*(loc_CH4)
      loc_14CO2 = loc_14CO2 + loc_r14CH4*(loc_CH4)
      loc_O2    = loc_O2    - 2.0*loc_CH4
     END IF    

     ! *** UPDATE ATM. TRACERS ***
     atm(ia_pCH4,:,:)     = (loc_CH4/loc_atmV)*conv_Pa_atm*const_R_SI*(atm(ia_T,:,:)+const_zeroC)
     atm(ia_pCH4_13C,:,:) = (loc_13CH4/loc_atmV)*conv_Pa_atm*const_R_SI*(atm(ia_T,:,:)+const_zeroC)
     atm(ia_pCH4_14C,:,:) = (loc_14CH4/loc_atmV)*conv_Pa_atm*const_R_SI*(atm(ia_T,:,:)+const_zeroC)
     atm(ia_pCO2,:,:)     = (loc_CO2/loc_atmV)*conv_Pa_atm*const_R_SI*(atm(ia_T,:,:)+const_zeroC)
     atm(ia_pCO2_13C,:,:) = (loc_13CO2/loc_atmV)*conv_Pa_atm*const_R_SI*(atm(ia_T,:,:)+const_zeroC)
     atm(ia_pCO2_14C,:,:) = (loc_14CO2/loc_atmV)*conv_Pa_atm*const_R_SI*(atm(ia_T,:,:)+const_zeroC)
     atm(ia_pO2,:,:)      = (loc_O2 /loc_atmV)*conv_Pa_atm*const_R_SI*(atm(ia_T,:,:)+const_zeroC)

  END SUBROUTINE sub_calc_oxidize_CH4_claire06
  ! ****************************************************************************************************************************** !
  
  ! ****************************************************************************************************************************** !
  ! OXIDIZE CH4 -- PHOTOCHEMICAL SCHEME AFTER CLAIRE ET AL. [2006], NO H ESCAPE, FIXED ATMOSPHERIC O2 (CTR|09-2018)
   SUBROUTINE sub_calc_oxidize_CH4_claire06_fixed(dum_dtyr,dum_conv_atm_mol)
     IMPLICIT NONE
     ! dummy arguments
     real,intent(in)::dum_dtyr
     real,dimension(n_i,n_j),intent(in)::dum_conv_atm_mol
     ! local variables
     real::loc_tau
     real::loc_fracdecay
     real::loc_O2, loc_CH4, loc_CO2
     real::loc_13CH4, loc_14CH4
     real::loc_r13CH4, loc_r14CH4
     real::loc_13CO2, loc_14CO2
     real::loc_r13CO2, loc_r14CO2
     real::loc_atmV
     real::loc_p00, loc_p10, loc_p01, loc_p20, loc_p11, loc_p02, loc_p30, loc_p21, loc_p12,    &
           &  loc_p03, loc_p40, loc_p31, loc_p22, loc_p13, loc_p04, loc_p50, loc_p41, loc_p32, &
           &  loc_p23, loc_p14, loc_p05
     real::loc_phi_o2, loc_phi_ch4, loc_k
     real::loc_oxrate
     
     ! sum (or fix) tracers
     loc_O2   = par_atm_pO2_fixed*1.7692e020              ! convert fixed pO2 parameter atm to mol
     loc_CH4  = SUM(dum_conv_atm_mol*atm(ia_pCH4,:,:))
     loc_CO2  = SUM(dum_conv_atm_mol*atm(ia_pCO2,:,:))

     loc_13CH4 = SUM(dum_conv_atm_mol*atm(ia_pCH4_13C,:,:))
     loc_14CH4 = SUM(dum_conv_atm_mol*atm(ia_pCH4_14C,:,:))
     loc_13CO2 = SUM(dum_conv_atm_mol*atm(ia_pCO2_13C,:,:))
     loc_14CO2 = SUM(dum_conv_atm_mol*atm(ia_pCO2_13C,:,:))

     loc_r13CH4 = loc_13CH4/loc_CH4
     loc_r14CH4 = loc_14CH4/loc_CH4
     loc_r13CO2 = loc_13CO2/loc_CO2
     loc_r14CO2 = loc_14CO2/loc_CO2

     loc_atmV = SUM(phys_atm(ipa_V,:,:))

     ! CH4-O3-O2 photochemistry after Claire et al. [2006] // no H escape from the atmosphere
     ! NOTE: CH4 truncated lifetime at lower limit below [see Schmidt & Shindell 2003]

     ! *** LOCAL CONSTANTS ***
     loc_p00 =  1.712
     loc_p10 = -0.3212
     loc_p01 = -1.97
     loc_p20 = -0.2595
     loc_p11 =  0.02261
     loc_p02 =  0.6206
     loc_p30 = -0.01508
     loc_p21 =  0.1081
     loc_p12 = -0.03527
     loc_p03 = -0.1487
     loc_p40 =  0.003142
     loc_p31 = -0.003905
     loc_p22 = -0.01894
     loc_p13 =  0.01487
     loc_p04 =  0.01797
     loc_p50 =  0.0001997
     loc_p41 = -0.000598
     loc_p32 =  0.0001878
     loc_p23 =  0.001942
     loc_p14 = -0.001568
     loc_p05 = -0.0009482

     ! *** CALCULATE METHANE OXIDATION RATE ***
     ! NOTE: fit in Tmol CH4 per y, but tracers summed as mol
     loc_phi_o2  = LOG10(loc_O2*1.0E-12)                        ! convert to Tmol
     loc_phi_ch4 = LOG10(loc_CH4*1.0E-12)                       ! convert to Tmol
     loc_k = (10.0**( loc_p00                                       &
                     &  + loc_p10*loc_phi_o2                        &
                     &  + loc_p01*loc_phi_ch4                       &
                     &  + loc_p20*loc_phi_o2**2                     &
                     &  + loc_p11*loc_phi_o2*loc_phi_ch4            &
                     &  + loc_p02*loc_phi_ch4**2                    &
                     &  + loc_p30*loc_phi_o2**3                     &
                     &  + loc_p21*(loc_phi_o2**2)*loc_phi_ch4       &
                     &  + loc_p12*loc_phi_o2*(loc_phi_ch4**2)       &
                     &  + loc_p03*loc_phi_ch4**3                    &
                     &  + loc_p40*loc_phi_o2**4                     &
                     &  + loc_p31*(loc_phi_o2**3)*loc_phi_ch4       &
                     &  + loc_p22*(loc_phi_o2**2)*(loc_phi_ch4**2)  &
                     &  + loc_p13*loc_phi_o2*(loc_phi_ch4**3)       &
                     &  + loc_p04*loc_phi_ch4**4                    &
                     &  + loc_p50*loc_phi_o2**5                     &
                     &  + loc_p41*(loc_phi_o2**4)*loc_phi_ch4       &
                     &  + loc_p32*(loc_phi_o2**3)*(loc_phi_ch4**2)  &
                     &  + loc_p23*(loc_phi_o2**2)*(loc_phi_ch4**3)  &
                     &  + loc_p14*loc_phi_o2*(loc_phi_ch4**4)       &
                     &  + loc_p05*loc_phi_ch4**5))*1.0E-12          ! converted to mol-1 y-1
     loc_oxrate = loc_k*loc_O2*loc_CH4     
     loc_tau    = max(loc_CH4/loc_oxrate,7.6161)                 ! in yr
     loc_fracdecay = dum_dtyr/loc_tau
       
     ! *** PERFORM METHANE OXIDATION ***
     ! NOTE: stoichiometry of CH4 oxidation : CH4 + 2O2 + hv  --> CO2 + 2H2O
     loc_CH4     = loc_CH4   - loc_fracdecay*loc_CH4
     loc_13CH4   = loc_13CH4 - loc_fracdecay*loc_13CH4
     loc_14CH4   = loc_14CH4 - loc_fracdecay*loc_14CH4
     loc_CO2     = loc_CO2   + loc_fracdecay*loc_CH4
     loc_13CO2   = loc_13CO2 + loc_fracdecay*loc_13CH4
     loc_14CO2   = loc_14CO2 + loc_fracdecay*loc_14CH4
     loc_O2      = loc_O2    - 2.0*loc_fracdecay*loc_CH4
     
     ! *** ADJUST INVENTORIES TO AVOID -VE CH4 ***
     IF (loc_CH4 < const_real_zero) THEN
      loc_CH4   = const_real_zero
      loc_13CH4 = loc_13CH4 - loc_r13CH4*(loc_CH4)
      loc_14CH4 = loc_14CH4 - loc_r14CH4*(loc_CH4)
      loc_CO2   = loc_CO2   + loc_CH4
      loc_13CO2 = loc_13CO2 + loc_r13CH4*(loc_CH4)
      loc_14CO2 = loc_14CO2 + loc_r14CH4*(loc_CH4)
      loc_O2    = loc_O2    - 2.0*loc_CH4
     END IF    

     ! *** UPDATE ATM. TRACERS ***
     atm(ia_pCH4,:,:)     = (loc_CH4/loc_atmV)*conv_Pa_atm*const_R_SI*(atm(ia_T,:,:)+const_zeroC)
     atm(ia_pCH4_13C,:,:) = (loc_13CH4/loc_atmV)*conv_Pa_atm*const_R_SI*(atm(ia_T,:,:)+const_zeroC)
     atm(ia_pCH4_14C,:,:) = (loc_14CH4/loc_atmV)*conv_Pa_atm*const_R_SI*(atm(ia_T,:,:)+const_zeroC)
     atm(ia_pCO2,:,:)     = (loc_CO2/loc_atmV)*conv_Pa_atm*const_R_SI*(atm(ia_T,:,:)+const_zeroC)
     atm(ia_pCO2_13C,:,:) = (loc_13CO2/loc_atmV)*conv_Pa_atm*const_R_SI*(atm(ia_T,:,:)+const_zeroC)
     atm(ia_pCO2_14C,:,:) = (loc_14CO2/loc_atmV)*conv_Pa_atm*const_R_SI*(atm(ia_T,:,:)+const_zeroC)
  !   atm(ia_pO2,:,:)      = (loc_O2 /loc_atmV)*conv_Pa_atm*const_R_SI*(atm(ia_T,:,:)+const_zeroC)

  END SUBROUTINE sub_calc_oxidize_CH4_claire06_fixed
  ! ****************************************************************************************************************************** !
 
  ! ****************************************************************************************************************************** !
  ! OXIDIZE CH4 -- PHOTOCHEMICAL SCHEME AFTER CLAIRE ET AL. [2006], H ESCAPE ENABLED (CTR|05-2017)
  SUBROUTINE sub_calc_oxidize_CH4_claire06H(dum_dtyr,dum_conv_atm_mol)
    IMPLICIT NONE
    ! DUMMY ARGUMENTS
    REAL, INTENT(in)::dum_dtyr
    REAL,DIMENSION(n_i,n_j),INTENT(in)::dum_conv_atm_mol
    ! LOCAL VARIABLES
    real::loc_tau
    real::loc_fracdecay
    real::loc_O2, loc_CH4, loc_CO2
    real::loc_13CH4, loc_14CH4
    real::loc_r13CH4, loc_r14CH4
    real::loc_13CO2, loc_14CO2
    real::loc_r13CO2, loc_r14CO2
    real::loc_atmV
    real::loc_p00, loc_p10, loc_p01, loc_p20, loc_p11, loc_p02, loc_p30, loc_p21, loc_p12,   &
          & loc_p03, loc_p40, loc_p31, loc_p22, loc_p13, loc_p04, loc_p50, loc_p41, loc_p32, &
          & loc_p23, loc_p14, loc_p05
    real::loc_phi_o2, loc_phi_ch4, loc_k
    real::loc_oxrate
    REAL::H_esc, dH_esc, esc_const
  
    ! sum tracers
    loc_O2  = SUM(dum_conv_atm_mol*atm(ia_pO2,:,:))
    loc_CH4 = SUM(dum_conv_atm_mol*atm(ia_pCH4,:,:))
    loc_CO2 = SUM(dum_conv_atm_mol*atm(ia_pCO2,:,:))

    loc_13CH4 = SUM(dum_conv_atm_mol*atm(ia_pCH4_13C,:,:))
    loc_14CH4 = SUM(dum_conv_atm_mol*atm(ia_pCH4_14C,:,:))
    loc_13CO2 = SUM(dum_conv_atm_mol*atm(ia_pCO2_13C,:,:))
    loc_14CO2 = SUM(dum_conv_atm_mol*atm(ia_pCO2_13C,:,:))

    loc_r13CH4 = loc_13CH4/loc_CH4
    loc_r14CH4 = loc_14CH4/loc_CH4
    loc_r13CO2 = loc_13CO2/loc_CO2
    loc_r14CO2 = loc_14CO2/loc_CO2
  
    loc_atmV = SUM(phys_atm(ipa_V,:,:))
  
     ! CH4-O3-O2 photochemistry after Claire et al. [2006] // no H escape from the atmosphere
     ! NOTE: neither O2 nor CH4 are truncated at edge of training values here, so USE CAUTION
     ! NOTE: CH4 truncated lifetime at lower limit below [see Schmidt & Shindell 2003]
     IF (loc_O2 > 1.0E8) THEN ! proceed with CH4 oxidation 
     ! *** LOCAL CONSTANTS ***
     loc_p00 =  1.712
     loc_p10 = -0.3212
     loc_p01 = -1.97
     loc_p20 = -0.2595
     loc_p11 =  0.02261
     loc_p02 =  0.6206
     loc_p30 = -0.01508
     loc_p21 =  0.1081
     loc_p12 = -0.03527
     loc_p03 = -0.1487
     loc_p40 =  0.003142
     loc_p31 = -0.003905
     loc_p22 = -0.01894
     loc_p13 =  0.01487
     loc_p04 =  0.01797
     loc_p50 =  0.0001997
     loc_p41 = -0.000598
     loc_p32 =  0.0001878
     loc_p23 =  0.001942
     loc_p14 = -0.001568
     loc_p05 = -0.0009482
  
     ! *** CALCULATE METHANE OXIDATION RATE ***
     ! NOTE: fit in Tmol CH4 per y, but tracers summed as mol
     loc_phi_o2  = LOG10(loc_O2*1.0E-12)                        ! convert to Tmol
     loc_phi_ch4 = LOG10(loc_CH4*1.0E-12)                       ! convert to Tmol
     loc_k = (10.0**( loc_p00                                       &
                     &  + loc_p10*loc_phi_o2                        &
                     &  + loc_p01*loc_phi_ch4                       &
                     &  + loc_p20*loc_phi_o2**2                     &
                     &  + loc_p11*loc_phi_o2*loc_phi_ch4            &
                     &  + loc_p02*loc_phi_ch4**2                    &
                     &  + loc_p30*loc_phi_o2**3                     &
                     &  + loc_p21*(loc_phi_o2**2)*loc_phi_ch4       &
                     &  + loc_p12*loc_phi_o2*(loc_phi_ch4**2)       &
                     &  + loc_p03*loc_phi_ch4**3                    &
                     &  + loc_p40*loc_phi_o2**4                     &
                     &  + loc_p31*(loc_phi_o2**3)*loc_phi_ch4       &
                     &  + loc_p22*(loc_phi_o2**2)*(loc_phi_ch4**2)  &
                     &  + loc_p13*loc_phi_o2*(loc_phi_ch4**3)       &
                     &  + loc_p04*loc_phi_ch4**4                    &
                     &  + loc_p50*loc_phi_o2**5                     &
                     &  + loc_p41*(loc_phi_o2**4)*loc_phi_ch4       &
                     &  + loc_p32*(loc_phi_o2**3)*(loc_phi_ch4**2)  &
                     &  + loc_p23*(loc_phi_o2**2)*(loc_phi_ch4**3)  &
                     &  + loc_p14*loc_phi_o2*(loc_phi_ch4**4)       &
                     &  + loc_p05*loc_phi_ch4**5))*1.0E-12          ! converted to mol-1 y-1
     loc_oxrate = loc_k*loc_O2*loc_CH4     
     loc_tau    = max(loc_CH4/loc_oxrate,7.6161)                 ! in yr
     loc_fracdecay = dum_dtyr/loc_tau
  
     ! *** DETERMINE H2 ESCAPE RATE ***
     ! NOTE: assumes diffusion-limited H2 escape with CH4 as the dominant H-bearing species above the cold trap
     esc_const = 3.7E-05               ! units of y-1; see Goldblatt et al. (2006) and  Claire et al. (2006)
     H_esc = loc_CH4*esc_const
     dH_esc = H_esc*dum_dtyr
  
     ! *** PERFORM METHANE OXIDATION AND HYDROGEN ESCAPE ***
     ! NOTE: stoichiometry of CH4 oxidation : CH4 + 2O2 + hv  --> CO2 + 2H2O
     ! NOTE: stoichiometry of H2 escape : CH4 + O2 + hv --> CO2 + 4H_space
     loc_CH4    = loc_CH4   - loc_fracdecay*loc_CH4     - dH_esc
     loc_13CH4  = loc_13CH4 - loc_fracdecay*loc_13CH4   - dH_esc*loc_r13CH4
     loc_14CH4  = loc_14CH4 - loc_fracdecay*loc_14CH4   - dH_esc*loc_r14CH4
     loc_CO2    = loc_CO2   + loc_fracdecay*loc_CH4     + dH_esc
     loc_13CO2  = loc_13CO2 + loc_fracdecay*loc_13CH4   - dH_esc*loc_r13CH4
     loc_14CO2  = loc_14CO2 + loc_fracdecay*loc_14CH4   - dH_esc*loc_r14CH4
     loc_O2     = loc_O2    - 2.0*loc_fracdecay*loc_CH4 - dH_esc
     
     END IF
     
     ! *** ADJUST INVENTORIES TO AVOID -VE CH4 ***
     IF (loc_CH4 < const_real_zero) THEN
      loc_CH4   = const_real_zero
      loc_13CH4 = loc_13CH4 - loc_r13CH4*(loc_CH4)
      loc_14CH4 = loc_14CH4 - loc_r14CH4*(loc_CH4)
      loc_CO2   = loc_CO2   + loc_CH4
      loc_13CO2 = loc_13CO2 + loc_r13CH4*(loc_CH4)
      loc_14CO2 = loc_14CO2 + loc_r14CH4*(loc_CH4)
      loc_O2    = loc_O2    - 2.0*loc_CH4
     END IF    

     ! *** UPDATE ATM. TRACERS ***
     atm(ia_pCH4,:,:)     = (loc_CH4/loc_atmV)*conv_Pa_atm*const_R_SI*(atm(ia_T,:,:)+const_zeroC)
     atm(ia_pCH4_13C,:,:) = (loc_13CH4/loc_atmV)*conv_Pa_atm*const_R_SI*(atm(ia_T,:,:)+const_zeroC)
     atm(ia_pCH4_14C,:,:) = (loc_14CH4/loc_atmV)*conv_Pa_atm*const_R_SI*(atm(ia_T,:,:)+const_zeroC)
     atm(ia_pCO2,:,:)     = (loc_CO2/loc_atmV)*conv_Pa_atm*const_R_SI*(atm(ia_T,:,:)+const_zeroC)
     atm(ia_pCO2_13C,:,:) = (loc_13CO2/loc_atmV)*conv_Pa_atm*const_R_SI*(atm(ia_T,:,:)+const_zeroC)
     atm(ia_pCO2_14C,:,:) = (loc_14CO2/loc_atmV)*conv_Pa_atm*const_R_SI*(atm(ia_T,:,:)+const_zeroC)
     atm(ia_pO2,:,:)      = (loc_O2 /loc_atmV)*conv_Pa_atm*const_R_SI*(atm(ia_T,:,:)+const_zeroC)
  
  END SUBROUTINE sub_calc_oxidize_CH4_claire06H
  ! ****************************************************************************************************************************** !
  
  ! ****************************************************************************************************************************** !
  ! OXIDIZE CH4 -- UPDATED PHOTOCHEMICAL SCHEME AFTER GOLDBLATT ET AL. [2006] (SLO|2015, CTR|05-2017)
  SUBROUTINE sub_calc_oxidize_CH4_goldblatt06(dum_dtyr,dum_conv_atm_mol)
    IMPLICIT NONE
    ! dummy arguments
    REAL,INTENT(in)::dum_dtyr
    REAL,DIMENSION(n_i,n_j),INTENT(in)::dum_conv_atm_mol
    ! local variables
    REAL:: loc_O2, loc_CH4, loc_CO2
    REAL:: loc_13CH4, loc_r13CH4
    REAL:: loc_13CO2, loc_r13CO2
    REAL:: loc_atmV
    REAL:: loc_a1, loc_a2, loc_a3, loc_a4, loc_a5
    REAL:: loc_phi, loc_psi
    REAL:: loc_oxrate, loc_dCH4

    loc_O2  = SUM(dum_conv_atm_mol*atm(ia_pO2,:,:))
    loc_CH4 = SUM(dum_conv_atm_mol*atm(ia_pCH4,:,:))
    loc_CO2 = SUM(dum_conv_atm_mol*atm(ia_pCO2,:,:))

    loc_13CH4 = SUM(dum_conv_atm_mol*atm(ia_pCH4_13C,:,:))
    loc_13CO2 = SUM(dum_conv_atm_mol*atm(ia_pCO2_13C,:,:))

    loc_r13CH4 = loc_13CH4/loc_CH4
    loc_r13CO2 = loc_13CO2/loc_CO2

    loc_atmV = SUM(phys_atm(ipa_V,:,:))

    ! CH4-O3-O2 photochemistry after Goldblatt et al. [2006] // long-form constants updated from Daines & Lenton [2016]
      IF (loc_O2 > 1.0E8) THEN ! proceed with CH4 oxidation

      ! *** LOCAL CONSTANTS ***
      loc_a1 =  0.002998345  !    0.0030
      loc_a2 = -0.165030964  !   -0.1655
      loc_a3 =  3.221048922  !    3.2305
      loc_a4 = -25.757487116 !   -25.8343
      loc_a5 =  70.985147970 !    71.5398

      ! *** CALCULATE METHANE OXIDATION RATE ***
      loc_phi = LOG10(loc_O2)
      loc_psi = 10.0**(loc_a1*loc_phi**4 + loc_a2*loc_phi**3 + loc_a3*loc_phi**2 + loc_a4*loc_phi + loc_a5)
      loc_oxrate = 0.5*loc_psi*loc_CH4**0.7
      loc_dCH4 = min(loc_oxrate*dum_dtyr, 0.5*loc_O2, loc_CH4)

      ! *** PERFORM METHANE OXIDATION ***
      loc_CH4 = loc_CH4 - loc_dCH4
      loc_CO2 = loc_CO2 + loc_dCH4
      loc_O2  = loc_O2  - 2.0*loc_dCH4
      loc_13CH4 = loc_13CH4 - loc_r13CH4*loc_dCH4
      loc_13CO2 = loc_13CO2 + loc_r13CH4*loc_dCH4

      ! *** UPDATE ATM. TRACERS ***
      atm(ia_pCH4,:,:)     = (loc_CH4/loc_atmV)*conv_Pa_atm*const_R_SI*(atm(ia_T,:,:)+const_zeroC)
      atm(ia_pCO2,:,:)     = (loc_CO2/loc_atmV)*conv_Pa_atm*const_R_SI*(atm(ia_T,:,:)+const_zeroC)
      atm(ia_pO2,:,:)      = (loc_O2 /loc_atmV)*conv_Pa_atm*const_R_SI*(atm(ia_T,:,:)+const_zeroC)
      atm(ia_pCH4_13C,:,:) = (loc_13CH4/loc_atmV)*conv_Pa_atm*const_R_SI*(atm(ia_T,:,:)+const_zeroC)
      atm(ia_pCO2_13C,:,:) = (loc_13CO2/loc_atmV)*conv_Pa_atm*const_R_SI*(atm(ia_T,:,:)+const_zeroC)

      END IF

  END SUBROUTINE sub_calc_oxidize_CH4_goldblatt06
  ! ****************************************************************************************************************************** !

  ! ****************************************************************************************************************************** !
  ! WETLANDS CH4 FLUX
  SUBROUTINE sub_calc_wetlands_CH4(dum_dtyr,dum_fatm)
    IMPLICIT NONE
    ! dummy arguments
    real,intent(in)::dum_dtyr
    REAL,DIMENSION(n_atm),intent(inout)::dum_fatm              ! flux to atmosphere (mol)
    ! local variables
    REAL::loc_flux_CH4,loc_flux_CH4_13C                        ! local CH4 flux
    real::loc_tot,loc_standard                                 ! local isotopic variables

    ! *** INITIALIZE LOCAL VARIABLES ***
    dum_fatm(:) = 0.0
        
    ! *** CALCULATE 'WETLANDS' CH4 FLUX TO ATMOSPHERE ***
    ! NOTE: multiply by 1/(imax x jmax) to divide up total emissions equally across all grid cells
    ! NOTE: loc_fatm in units of (mol), par_atm_wetlands_FCH4 in units of (mol yr-1)
    loc_flux_CH4 = dum_dtyr*(1.0/real(n_i*n_j))*par_atm_wetlands_FCH4
        
    ! *** ADD 'WETLAND' CH4 EMISSIONS SOURCE TO ATMOSPHERE ***
    dum_fatm(ia_pCH4) = dum_fatm(ia_pCH4) + loc_flux_CH4
    IF (atm_select(ia_pCH4_13C)) THEN
       loc_tot = loc_flux_CH4
       loc_standard = const_standards(atm_type(ia_pCH4_13C))
       loc_flux_CH4_13C = fun_calc_isotope_fraction(par_atm_wetlands_FCH4_d13C,loc_standard)*loc_tot
       dum_fatm(ia_pCH4_13C) = dum_fatm(ia_pCH4_13C) + loc_flux_CH4_13C
       IF (atm_select(ia_pCH4_14C)) THEN
          dum_fatm(ia_pCH4_14C) = dum_fatm(ia_pCH4_14C)
       end IF
    end IF
        
    ! *** BALANCE CO2 and O2 BUDGETS ***
    ! remove CO2 from atmosphere, assuming CO2 has at some point been removed to form wetland Corg and hence CH4
    ! add (2x) O2 to the atmopshere, again implicitly accounting for net O2 release upon Corg deposition in wetlands
    dum_fatm(ia_pCO2) = dum_fatm(ia_pCO2) - loc_flux_CH4
    IF (atm_select(ia_pCH4_13C) .AND. atm_select(ia_pCH4_13C)) THEN
       dum_fatm(ia_pCO2_13C) = dum_fatm(ia_pCO2_13C) - loc_flux_CH4_13C
       IF (atm_select(ia_pCO2_14C)) THEN
          dum_fatm(ia_pCO2_14C) = dum_fatm(ia_pCO2_14C)
       end IF
    end IF
    dum_fatm(ia_pO2) = dum_fatm(ia_pO2) + 2.0*loc_flux_CH4
    
  END SUBROUTINE sub_calc_wetlands_CH4
  ! ****************************************************************************************************************************** !

  ! ****************************************************************************************************************************** !
  ! PRODUCE 14C
  SUBROUTINE sub_calc_generate_14C(dum_dtyr,dum_fatm)
    IMPLICIT NONE
    ! dummy arguments
    real,intent(in)::dum_dtyr
    REAL,DIMENSION(n_atm),intent(inout)::dum_fatm              ! flux to atmosphere (mol)
    
    ! *** CALCULATE COSMOGENIC 14C FLUX ***
    dum_fatm(ia_pCO2_14C) = dum_fatm(ia_pCO2_14C) + dum_dtyr*(1.0/real(n_i*n_j))*par_atm_F14C
    
  END SUBROUTINE sub_calc_generate_14C
  ! ****************************************************************************************************************************** !
  

END MODULE atchem_box
