! ******************************************************************************************************************************** !
! sedgem_box_benthic.f90
! Hülse et al. (2017) GMD - sediment diagenesis routines
! ******************************************************************************************************************************** !


MODULE sedgem_box_benthic


    use genie_control
    USE sedgem_lib
    IMPLICIT NONE
    
    !sediment characteristics
    real rho_sed                                ! sediment density (g/cm3)
    real wdepth                                 ! water depth (m)
    real w                                      ! burial velocity  (cm/yr)
    real z0, zox                                ! surface
    real zbio                                   ! bioturbation depth (cm)

    real zinf                                   !Inifinity (cm)
    real Dbio                                   !bioturbation coefficient (cm2/yr)
    real Dunbio                                 ! 2nd diffusion coefficient in case used for 2nd Layer
    real por                                    !porosity (-)
    real tort                                   !tortuosity (-)
    real irrigationFactor                       !irrigation factor (-)
    real dispFactor                             !dispersion factor (-)

    !stoichiometric factors
    real X_C, Y_N, Z_P
    real SD                                     !volume factor solid->dissolved phase
    real OC                                     !O2/C (mol/mol)
    real NC1                                    !N/C first TOC fraction (mol/mol)
    real NC2                                    !N/C second TOC fraction (mol/mol)
    real PC1                                    !P/C first TOC fraction (mol/mol)
    real PC2                                    !P/C second TOC fraction (mol/mol)
    real SO4C                                   !SO4/C (mol/mol)
    real O2H2S                                  !O2/H2S O2 needed to oxidize H2S (mol/mol) 
    real DICC1                                  !DIC/C until zSO4 (mol/mol)
    real DICC2                                  !DIC/C below zSO$ (mol/mol)
    real MC                                     !CH4/C (mol/mol)
    real gamma                                  !fraction of NH4 that is oxidised in oxic layer
    real gammaH2S                               !fraction of H2S that is oxidised in oxic layer
    real gammaCH4                               !fraction of CH4 that is oxidised at SO4
    real satSO4                                 ! SO4 saturation
    real NO3CR                                  ! NO3 consumed by Denitrification
    ! Alkalinity productio/consumption from:
    real ALKROX;                                ! Aerobic degradation
    real ALKRNIT;                               ! Nitrification
    real ALKRDEN;                               ! Denitrification
    real ALKRSUL;                               ! Sulfato reduction
    real ALKRH2S;                               ! H2S oxydation (CHECK THIS VALUE!!!)
    real ALKRMET;                               ! Methanogenesis
    real ALKRAOM;                               ! AOM

    real zoxgf                                  ! cm, rolloff NH4, H2S oxidation for small zox depth

    !bottom water concentrations (initialized locally)
    real dum_POC1_conc_swi                  !TOC flux at SWI (mol/(cm2 yr)) -> (mol/cm3 bulk phase)
    real dum_POC2_conc_swi                  !TOC flux at SWI (mol/(cm2 yr)) -> (mol/cm3 bulk phase)
    real dum_POC3_conc_swi                  !TOC flux at SWI (mol/(cm2 yr)) -> (mol/cm3 bulk phase)
    real dum_POC1_conc_swi_nonbio           !TOC flux at SWI (mol/(cm2 yr)) -> (mol/cm3 bulk phase) not taking into account biodiffusion loss
    real dum_POC2_conc_swi_nonbio           !TOC flux at SWI (mol/(cm2 yr)) -> (mol/cm3 bulk phase) not taking into account biodiffusion loss
    real dum_POC_total_flux_zinf             !total TOC flux at SWI (mol/(cm2 yr)) -> (mol/cm3 bulk phase)
    real dum_swiconc_O2                     ! O2 concentration at SWI (mol/cm3)
    real dum_swiconc_SO4                    ! SO4 concentration at SWI (mol/cm3)
    real dum_swiconc_H2S                    ! H2S concentration at SWI (mol/cm3)
    real dum_swiconc_NO3                    ! NO3 concentration at SWI (mol/cm3)
    real dum_swiconc_NH4                    ! NH4 concentration at SWI (mol/cm3)
    real dum_swiconc_PO4                    !PO4 concentration at SWI (mol/cm3)
    real dum_swiflux_M                      ! flux of M to the sediment (mol/(cm2*yr)) -> is converted into concentration (mol/cm3)
    real dum_swiconc_DIC                    ! DIC concentration at SWI (mol/cm3)
    real dum_swiconc_ALK                    ! ALK concentration at SWI (mol/cm3)


    ! ORGANIC MATTER
    real DC1, DC2                           !TOC diffusion coefficient (cm2/yr)
    !    real C, C1, C2
    real k1                                 !TOC degradation rate constnat (1/yr)
    real k2                                 !TOC degradation rate constant (1/yr)

    ! O2
    real qdispO2                            !O2 diffusion coefficient in water (cm2/yr)
    real adispO2                            !O2 linear coefficient for temperature dependence (cm2/yr/oC)
    real DO21                               !O2 diffusion coefficient in bioturbated layer (cm2/yr)
    real DO22                               !O2 diffusion coefficient in non-bioturbated layer (cm2/yr)
    real r_zxf                              !roll off oxidation at low zox

    ! global constants, variables
    real aa11, bb11, aa21, A11, A21, aa12, bb12, aa22, A12, A22
    real ls_a, ls_b, ls_c, ls_d, ls_e, ls_f

    ! Nitrate (NO3)
    real qdispNO3                   ! NO3 diffusion coefficient in water (cm2/yr)
    real adispNO3                   ! NO3 linear coefficient for temperature dependence (cm2/yr/oC)
    real DN1                        ! NO3 diffusion coefficient in bioturbated layer (cm2/yr)
    real DN2                        ! NO3 diffusion coefficient in non-bioturbated layer (cm2/yr)
    real zno3
    real KNH4                       ! Adsorption coefficient (same in ocix and anoxic layer) (-)

    ! Sulfate (SO4)
    real qdispSO4                   ! SO4 diffusion coefficient in water (cm2/yr)
    real adispSO4                   ! SO4 linear coefficient for temperature dependence (cm2/yr/oC)
    real DSO41                      ! SO4 diffusion coefficient in bioturbated layer (cm2/yr)
    real DSO42                      ! SO4 diffusion coefficient in non-bioturbated layer (cm2/yr)
    real zso4

    ! Ammonium (NH4)
    real qdispNH4                   ! NH4 diffusion coefficient in water (cm2/yr)
    real adispNH4                   ! NH4 linear coefficient for temperature dependence (cm2/yr/oC)
    real DNH41                      ! NH4 diffusion coefficient in bioturbated layer (cm2/yr)
    real DNH42                      ! NH4 diffusion coefficient in non-bioturbated layer (cm2/yr)

    ! Hydrogen sulfide (H2S)
    real qdispH2S                   ! H2S diffusion coefficient in water (cm2/yr)
    real adispH2S                   ! H2S linear coefficient for temperature dependence (cm2/yr/oC)
    real DH2S1                      ! H2S diffusion coefficient in bioturbated layer (cm2/yr)
    real DH2S2                      ! H2S diffusion coefficient in non-bioturbated layer (cm2/yr)

    ! Phosphate (PO4)
    real qdispPO4                   ! PO4 diffusion coefficient in water (cm2/yr)
    real adispPO4                   ! PO4 linear coefficient for temperature dependence (cm2/yr/oC)
    real DPO41                      ! PO4 diffusion coefficient in bioturbated layer (cm2/yr)
    real DPO42                      ! PO4 diffusion coefficient in non-bioturbated layer (cm2/yr)
    real KPO4_ox                    ! Adsorption coefficient in oxic layer (-)
    real KPO4_anox                  ! Adsorption coefficient in anoxic layer (-)
    real ksPO4                      ! Rate constant for kinetic P sorption (1/yr)
    real kmPO4                      ! Rate constant for Fe-bound P release upon Fe oxide reduction
    real kaPO4                      ! Rate constant for authigenic P formation (1/yr)
    real PO4s                       ! Equilibrium concentration for P sorption (mol/cm3)
    real PO4a                       ! Equilibrium concentration for authigenic P formation (mol/cm3)
    real Minf                       ! asymptotic concentration for Fe-bound P (mol/cm3)

    ! DIC
    real qdispDIC                   ! DIC diffusion coefficient in water (cm2/yr)
    real adispDIC                   ! DIC linear coefficient for temperature dependence (cm2/yr/oC)
    real DDIC1                      ! DIC diffusion coefficient in bioturbated layer (cm2/yr)
    real DDIC2                      ! DIC diffusion coefficient in non-bioturbated layer (cm2/yr)

    ! Alkalinity
    real qdispALK                   ! ALK diffusion coefficient in water (cm2/yr)
    real adispALK                   ! ALK linear coefficient for temperature dependence (cm2/yr/oC)
    real DALK1                      ! ALK diffusion coefficient in bioturbated layer (cm2/yr)
    real DALK2                      ! ALK diffusion coefficient in non-bioturbated layer (cm2/yr)

    SAVE


CONTAINS


    !------------------------------------------------------------------------------------
    !   *****************************************************************
    !   *****************************************************************

    !             Huelse & Arndt et al. 2017 OMEN sediment model

    !   *****************************************************************
    !   *****************************************************************
    !------------------------------------------------------------------------------------

    !    !!!SUBROUTINE sub_huelseetal2016_main &
    !    !!!(dum_i, dum_j, dum_dtyr, dum_D, loc_new_sed, dum_is_POC_frac2, dum_sfcsumocn, dum_sed_pres_fracC, dum_sed_pres_fracP, dum_new_swifluxes, dum_sed_mean_OM)
    !    SUBROUTINE sub_huelseetal2016_main &
    !    (dum_dtyr, dum_D, loc_new_sed, dum_is_POC_frac2, dum_sfcsumocn, dum_sed_pres_fracC, dum_sed_pres_fracP, dum_new_swifluxes, dum_sed_mean_OM)
    SUBROUTINE sub_huelseetal2016_main &
    (dum_i, dum_j, dum_dtyr, dum_D, dum_sed_OM_bur, loc_new_sed, dum_is_POC_frac2, dum_sfcsumocn, dum_sed_pres_fracC, dum_sed_pres_fracP, dum_new_swifluxes, dum_sed_mean_OM, dum_sed_OM_wtpc_bot)
        !   __________________________________________________________
        !
        !   Main subroutine: 
        !   gets SWI POC wtpct and array of concentrations of solutes 
        !   call other subroutines 
        !   passes back fraction of POC preserved in sediments (dum_sed_pres_fracC)
        !   passess back calculated array of SWI fluxes of solutes
        !   __________________________________________________________
        
        IMPLICIT NONE
        ! dummy arguments
        REAL,INTENT(in)::dum_dtyr                               ! time-step
        integer,intent(in) :: dum_i, dum_j                      ! grid point (i,j)
        REAL,INTENT(in)::dum_D                                  ! depth
        REAL,INTENT(in)::dum_sed_OM_bur                         ! burial rate (w) for OMEN-SED (solid cm3 cm-2 yr-1, NOTE: from previous time-step)
        REAL,INTENT(in)::dum_is_POC_frac2                       ! fraction of refractory POC
        REAL,DIMENSION(n_sed),intent(in)::loc_new_sed                         ! new (sedimenting) top layer material
        real,DIMENSION(n_ocn),intent(in)::dum_sfcsumocn                     ! ocean composition interface array
        !        real,INTENT(in)::dum_POC1_wtpct_swi, dum_POC2_wtpct_swi             ! POC concentrations at SWI [wt%]
        real,INTENT(inout)::dum_sed_pres_fracC                              ! fraction POC-preserved/POC-deposited [-]
        real,INTENT(inout)::dum_sed_pres_fracP                              ! fraction POP-preserved/POP-deposited [-]
        real,DIMENSION(n_ocn),intent(inout)::dum_new_swifluxes              ! SWI return fluxes of solutes, calculated with sediment-model [pos values: flux from sediments to water-column]
        real,INTENT(inout)::dum_sed_mean_OM                              ! mean OM wt% in upper mixed layer
        real,INTENT(inout)::dum_sed_OM_wtpc_bot                     ! POC wt% at bottom of sediment column (geologic preservation)

        ! local variables        
        real::loc_BW_O2_anoxia                                      ! BW [O2} threshold for zbio switch to 0.01cm 
        real::loc_total_POC_flux                                    ! total POC flux at SWI (POC1 + POC2) [mol/(cm^2 yr)]
        real::loc_POC1_flux_swi, loc_POC2_flux_swi                  ! POC flux at SWI [mol/(cm^2 yr)]
        real::loc_POC3_flux_swi                                     ! inert/sulfurized POC flux at SWI [mol/(cm^2 yr)]
        real::loc_O2_swiflux                                        ! SWI return fluxes of O2 [mol/(cm^2 yr)]
        real::loc_SO4_swiflux                                       ! SWI return fluxes of SO4 [mol/(cm^2 yr)]
        real::loc_NO3_swiflux                                       ! SWI return fluxes of NO3 [mol/(cm^2 yr)]
        real::loc_H2S_swiflux                                       ! SWI return fluxes of H2S [mol/(cm^2 yr)]
        real::loc_NH4_swiflux                                       ! SWI return fluxes of H2S [mol/(cm^2 yr)]
        real::loc_PO4_swiflux                                       ! SWI return fluxes of PO4 [mol/(cm^2 yr)]
        real::loc_M_swiflux                                         ! SWI return fluxes of M - DOES NOT EXIST, JUST FOR DEBUGGING
        real::loc_DIC_swiflux                                       ! SWI return fluxes of DIC [mol/(cm^2 yr)]
        real::loc_DIC_13C_swiflux                                   ! SWI return fluxes of DIC [mol/(cm^2 yr)]
        real::loc_ALK_swiflux                                       ! SWI return fluxes of ALK [mol/(cm^2 yr)]
        real::loc_mixed_layer
        real::loc_k_apparent
        real::loc_POM_S_H2S_swiflux                                 ! SWI return flux related to H2S in sulphurised POC
        logical::loc_sed_pres_insane                                ! true if integration constants are too high -> calculate SWI-flux manually
        logical::loc_O2_swiflux_pos

        ! parameters for temperature dependent rate constants (as in John et al. 2014)
        real::loc_T                                                 ! local temperature
        real::loc_par_bio_remin_POC_K1, loc_par_bio_remin_POC_K2    ! rate constants for temperature dependent degradation
        real::loc_par_bio_remin_POC_Ea1, loc_par_bio_remin_POC_Ea2  ! activation energies

        real::loc_fPOC                                              ! Corg flux to the sediment [cm3 cm-2]
        real::loc_fPOC_13C                                          ! 13C of POC flux to the sediment [cm3 cm-2]
        !        real::dum_POC1_wtpct_swi, dum_POC2_wtpct_swi       ! POC concentrations at SWI [wt%]
        logical :: loc_print_results
        logical :: loc_calc_ALK
!	logical :: par_sed_huelse2017_sim_P_loss, par_sed_huelse2017_remove_impl_sulALK
	real::loc_new_sed_vol                                       ! new sediment volume - settling flux (as SOLID material)
        real::loc_sed_burial                                        ! burial rate (w) - corrected for porosity
        !        integer:: loc_k = 0


        loc_sed_pres_insane = .false.
        loc_print_results = .false.
        loc_calc_ALK = .false.
!        par_sed_huelse2017_sim_P_loss = .false.		! simulate a P-loss to sediments due to OM-sulfurization?
!	par_sed_huelse2017_remove_impl_sulALK = .true.		! remove implicit Alk associated with buried sulf-OM - needed for steady-state simulations (then just see the sulf -> OM-burial effect)?
	loc_O2_swiflux_pos = .false.

        loc_O2_swiflux = 0.0
        loc_DIC_swiflux = 0.0
        loc_DIC_13C_swiflux = 0.0
        loc_ALK_swiflux = 0.0
        loc_M_swiflux = 0.0
        
        loc_POC1_flux_swi = 0.0
        loc_POC2_flux_swi = 0.0
        loc_POC3_flux_swi = 0.0

        loc_POM_S_H2S_swiflux = 0.0

        por = 0.85                                                  ! porosity (-) defined as: porewater_vol./(solid_sed_vol.+porewater_vol.)
        loc_BW_O2_anoxia = 5.0e-9                                   ! set to 5.0 nanomol/cm^3 - 5.0e-9 
        loc_mixed_layer = 5.0                                      ! mixed layer depth, to compare wt% with observations

        ! set local variables - temperature (K)
        loc_T = dum_sfcsumocn(io_T)
        loc_par_bio_remin_POC_K1 = 9.0E11
        loc_par_bio_remin_POC_K2 = 1.0E14
        loc_par_bio_remin_POC_Ea1 = 55000.0
        loc_par_bio_remin_POC_Ea2 = 80000.0

!                   print*,' '
!                   print*,'---------- IN OMEN MAIN ----------- '
!                   print*,'dum_i, dum_j, dum_D ', dum_i, dum_j, dum_D
        ! initialize BW concentrations 
        !   THE FOLLOWING VALUES WILL BE PASSED DOWN FROM GENIE
        ! *****************************************************************

        ! dum_sfcsumocn mol/kg -> SEDIMENT MODEL needs mol/cm^3
        dum_swiconc_O2 = dum_sfcsumocn(io_O2)*1e-3
        if(ocn_select(io_NO3))then
            dum_swiconc_NO3 = dum_sfcsumocn(io_NO3)*1e-3
            dum_swiconc_NH4 = dum_sfcsumocn(io_NH4)*1e-3
        end if
        if(ocn_select(io_SO4))then
            dum_swiconc_SO4 = dum_sfcsumocn(io_SO4)*1e-3
            dum_swiconc_H2S = dum_sfcsumocn(io_H2S)*1e-3
        end if
        if(ocn_select(io_PO4))then
            dum_swiconc_PO4 = dum_sfcsumocn(io_PO4)*1e-3
        !            dum_swiflux_M = 365*0.2e-10 ! Flux input 365*0.2e-10
        !            dum_swiflux_M = 365*0.2e-10*1/(1-por)*1/w ! Flux concerted to concentration
        end if
        dum_swiconc_DIC = dum_sfcsumocn(io_DIC)*1e-3
        dum_swiconc_ALK = dum_sfcsumocn(io_ALK)*1e-3

!        print*,'dum_i, dum_j, dum_D ', dum_i, dum_j, dum_D

        ! calculate wt% of mol from POC flux (both fractions)
        ! NOTE: the units of the Corg flux are in (cm3 cm-2 yr-1)
        loc_fPOC = loc_new_sed(is_POC)/dum_dtyr
        ! also get flux 13C of POC in (cm3 cm-2 yr-1)
        loc_fPOC_13C = loc_new_sed(is_POC_13C)/dum_dtyr

        ! calculate sediment accumulation in (cm3 cm-2)
        ! w after Middelburg
        !        loc_new_sed_vol=10.0**(-0.87478367-0.00043512*dum_D)*3.3              ! sedimentation rate, cm/yr / burial velocity / advection (Middelburg et al., Deep Sea Res. 1, 1997)
        !        print*,'loc_new_sed_vol Middelburg =', loc_new_sed_vol
        ! w from GENIE
        loc_new_sed_vol = 1/(1-por)*fun_calc_sed_vol(loc_new_sed(:))            ! using sedimentation rate (OLD)
        loc_sed_burial = 1/(1-por)*dum_sed_OM_bur                               ! new actual burial rate: (Andy mail 11.07.2017)

	! DH TODO: some of initialize should be called just once, not for every grid point
	call sub_huelseetal2016_initialize(dum_D, loc_T, loc_sed_burial/dum_dtyr)
	! OLD with settling flux
	!        call sub_huelseetal2016_initialize(dum_D, loc_T, loc_new_sed_vol/dum_dtyr)


        !	Model crashed for low sediment accumulation rates, therefore:
	! 	Check for no detrital flux .OR. No burial rate from previous time-step -> Remineralize everything manually

        if((loc_new_sed(is_det) .LE. const_real_nullsmall) .OR. (loc_sed_burial .LE. const_real_nullsmall))then
            !!! Remineralize everything manually
!                                print*,' '
!                                print*,'no detrital/burial flux !!!!!!', loc_new_sed(is_det)
!            			print*,'dum_D, loc_fPOC ', dum_D, loc_fPOC
!            			print*,'initial?? SD, OC, PC1, DICC1, ALKROX', SD, OC, PC1, DICC1, ALKROX
            !            print*,'1/(1-por)*loc_new_sed(is_det) = ', 1/(1-por)*loc_new_sed(is_det)

            dum_sed_pres_fracC = 0.0        ! sed POC preservation to zero
            dum_sed_OM_wtpc_bot = 0.0
            loc_O2_swiflux = conv_POC_cm3_mol*loc_fPOC*(-OC/SD)
            if(ocn_select(io_NO3))then
                ! TODO: change if NO3 is selected
                loc_NO3_swiflux = 0.0
                loc_NH4_swiflux = 0.0
            end if
            if(ocn_select(io_SO4))then
                loc_SO4_swiflux = 0.0 !conv_POC_cm3_mol*loc_fPOC*(-SO4C/SD)
                loc_H2S_swiflux = -loc_SO4_swiflux
            end if
            if(ocn_select(io_PO4))then
                loc_PO4_swiflux = conv_POC_cm3_mol*loc_fPOC*PC1/SD
            end if
            if(ocn_select(io_DIC))then
                loc_DIC_swiflux = conv_POC_cm3_mol*loc_fPOC*DICC1/SD
            end if
             if(ocn_select(io_DIC_13C))then
                loc_DIC_13C_swiflux = conv_POC_cm3_mol*loc_fPOC_13C*DICC1/SD
            end if
            if(ocn_select(io_ALK))then
                loc_ALK_swiflux = 2.0*loc_H2S_swiflux + conv_POC_cm3_mol*loc_fPOC*ALKROX/SD   !16/106
            end if
        else    ! Detrital flux > const_real_nullsmall

            ! CHECK new burial rate for lower than detrital flux!
            if(loc_sed_burial .LE. 1/(1-por)*loc_new_sed(is_det))then
!                                  	print*,''
!                                      	print*,'OMEN burial < detrital !!!!!!!!!!!!!!!!!!!!!!!!!!'
!                			print*,'dum_i, dum_j, dum_D', dum_i, dum_j, dum_D
!                			print*,'loc_new_sed_vol_OLD =', loc_new_sed_vol
!                                     	print*,'loc_sed_burial =', loc_sed_burial
!                                      	print*,'1/(1-por)*loc_new_sed(is_det) = ', 1/(1-por)*loc_new_sed(is_det)
!					print*,'Corg flux to Sediment: loc_fPOC', loc_fPOC
                loc_sed_burial = 1/(1-por)*loc_new_sed(is_det)      ! set burial flux as detrital flux
            end if

            ! CHECK if still lower than 4.0e-4 (5.0e-4 for OAE2), than cut there, as OMEN produces positive O2 SWI-fluxes
            if(loc_sed_burial .LE. 5.0e-4)then
!                                                print*,''
!                                                print*,'OMEN burial < 5.0e-4 !!!!!!!!!!!!!!!!!!!!!!!!!!'
!                                                print*,'dum_i, dum_j, dum_D', dum_i, dum_j, dum_D
                !                print*,'loc_new_sed_vol_OLD =', loc_new_sed_vol
!                                                print*,'loc_sed_burial_NEW before cut=', loc_sed_burial
                !                                print*,'1/(1-por)*loc_new_sed(is_det) = ', 1/(1-por)*loc_new_sed(is_det)
                loc_sed_burial = 5.0e-4     !(5.0e-4 for OAE2; 4.0e-4 for modern)
            end if

!	Dom: Old position of initialize:
!            ! DH TODO: some of initialize should be called just once, not for every grid point
!            call sub_huelseetal2016_initialize(dum_D, loc_T, loc_sed_burial/dum_dtyr)
		! Now just update w:
		w = loc_sed_burial/dum_dtyr
        
             !  NEW version: using TOC-flux, convert units from cm3 to mol
            ! JUST TWO FRACTIONS:
            !            loc_POC1_flux_swi = conv_POC_cm3_mol*(1-dum_is_POC_frac2)*loc_fPOC
            !            loc_POC2_flux_swi = conv_POC_cm3_mol*dum_is_POC_frac2*loc_fPOC
            ! THIRD INERT/SULFURIZED FRACTION
            loc_POC1_flux_swi = conv_POC_cm3_mol*(1-dum_is_POC_frac2)*loc_fPOC
            loc_POC3_flux_swi = conv_POC_cm3_mol*loc_new_sed(is_POM_S)
            loc_POC2_flux_swi = conv_POC_cm3_mol*dum_is_POC_frac2*loc_fPOC-loc_POC3_flux_swi

            ! make the k1 - k2 relation depth dependent:
            !            if(dum_D .LE. 1000.0)then
            !                par_sed_huelse2017_k2_order = 5.0
            !            elseif (dum_D .LE. 2000.0)then
            !                par_sed_huelse2017_k2_order = 8.0
            !            elseif (dum_D .LE. 3000.0)then
            !                par_sed_huelse2017_k2_order = 12.0
            !        !                print*,' '
            !        !                print*,' below 3000'
            !            !elseif (dum_D .LE. 4000.0)then
            !            !    par_sed_huelse2017_k2_order = 50.0
            !            !elseif (dum_D .LE. 5000.0)then
            !            !    par_sed_huelse2017_k2_order = 100.0
            !            else
            !                par_sed_huelse2017_k2_order = 25.0
            !            end if

            ! use oxic degradation rates
            select case (par_sed_huelse2017_kscheme)
                case ('boudreau1997')
                    ! use parameterisation of Boudreau 1997 dependent on sediment accumulation rate (w)
                    loc_k_apparent = 0.38*w**0.59
                    k1=loc_k_apparent/((1-dum_is_POC_frac2)+dum_is_POC_frac2/par_sed_huelse2017_k2_order)
                    k2=k1/par_sed_huelse2017_k2_order
                !                    print*,' '
                !                    print*,'boudreau1997 oxic dum_D, k2_order, k1, k2 =', dum_D, par_sed_huelse2017_k2_order, k1, k2
                case ('tromp1995')
                    ! use parameterisation of Tromp et al. 1995:
                    loc_k_apparent = 2.97*w**0.62
                    k1=loc_k_apparent/((1-dum_is_POC_frac2)+dum_is_POC_frac2/par_sed_huelse2017_k2_order)
                    k2=k1/par_sed_huelse2017_k2_order
                !                print*,'tromp1995 oxic k1, k2 =', k1, k2
                case ('stolpovsky2016')
                    ! use parameterisation of Stolpovsky et al. 2015 dependent on sediment accumulation rate (w)
                    loc_k_apparent = 1.02*w**0.5
                    k1=loc_k_apparent/((1-dum_is_POC_frac2)+dum_is_POC_frac2/par_sed_huelse2017_k2_order)
                    k2=k1/par_sed_huelse2017_k2_order
                !                print*,'stolpovsky2016 oxic k1, k2 =', k1, k2
                case ('boudreau1997fPOC')
                    ! k dependent on OM flux, after Boudreau 1997:
                    loc_total_POC_flux = conv_POC_cm3_mol*loc_fPOC*10**6
                    k1 = 2.2*1e-5*loc_total_POC_flux**2.1
                case ('temp_dependent')
                    ! k1 and k2 temperature dependent as in John et al. 2014:
                    k1 = loc_par_bio_remin_POC_K1*exp(-loc_par_bio_remin_POC_Ea1/(const_R_SI*loc_T))
                    k2 = loc_par_bio_remin_POC_K2*exp(-loc_par_bio_remin_POC_Ea2/(const_R_SI*loc_T))
                    print*,'Tmp dep: dum_D, loc_T, k1, k2 =', dum_D, loc_T, k1, k2
                case default
                    ! globally invariant k1 and k2 as set in par_sed_huelse2017_k1, par_sed_huelse2017_k2
                    k1=par_sed_huelse2017_k1
                    k2=par_sed_huelse2017_k2        !0.005
            !        k2=k1/par_sed_huelse2017_k2_order
            !                                print*,' '
            !                                print*,'oxic default degradation: k1, k2 =', k1, k2
                    ! make the k1 - k2 relation depth dependent:
            !                if(dum_D .LE. 2000.0)then
            !                    loc_k_apparent = par_sed_huelse2017_k1
            !                else
            !                    loc_k_apparent = par_sed_huelse2017_k2
            !                end if
            !                k1=loc_k_apparent/((1-dum_is_POC_frac2)+dum_is_POC_frac2/par_sed_huelse2017_k2_order)
            !                k2=k1/par_sed_huelse2017_k2_order

            !                print*,'default oxic dum_D, loc_k_apparent, k1, k2 =', dum_D, loc_k_apparent, k1, k2
            !            ! MIN oxic from Arndt et al. 2013
            !                        k1=1.0e-4
            !                        k2=1.0e-6
            !
            !                        ! oxic from PALASTANGA ET AL. 2011
            !                        if(dum_D .LE. 2000)then
            !                            k1=0.01
            !                        else
            !                            k1=0.005
            !                        end if
            end select
            ! use anoxic rate:
            if(par_sed_huelse2017_redox)then
!                print*,'USE anoxic degradation rate'
                ! if anoxic, decrease zbio and use anoxic degradation rate
                if(dum_swiconc_O2 .LE. loc_BW_O2_anoxia)then
                    ! decrease bioturbation depth
!		    print*,'USE anoxic degradation rate'
                    zbio = 0.01
                    select case (par_sed_huelse2017_kscheme)
                        case ('boudreau1997')
                            ! use parameterisation of Boudreau 1997 dependent on sediment accumulation rate (w)
                            ! which is actually Toth and Lerman (1977) - as no anoxic rate in Boudreau 1997:
                            loc_k_apparent = 0.04*w**2.0
                            k1=loc_k_apparent/((1-dum_is_POC_frac2)+dum_is_POC_frac2/par_sed_huelse2017_k2_order)
                            k2=k1/par_sed_huelse2017_k2_order
                        !                print*,'boudreau1997 anoxic k1, k2 =', k1, k2
                        case ('tromp1995')
                            ! use parameterisation of Tromp et al. 1995:
                            loc_k_apparent = 0.057*w**1.94
                            k1=loc_k_apparent/((1-dum_is_POC_frac2)+dum_is_POC_frac2/par_sed_huelse2017_k2_order)
                            k2=k1/par_sed_huelse2017_k2_order
                        case ('stolpovsky2016')
                            ! use parameterisation of Stolpovsky et al. 2015 dependent on sediment accumulation rate (w)
                            ! which is actually Toth and Lerman (1977) - as no anoxic rate in Stolpovsky et al. 2015:
                            loc_k_apparent = 0.04*w**2.0
                            k1=loc_k_apparent/((1-dum_is_POC_frac2)+dum_is_POC_frac2/par_sed_huelse2017_k2_order)
                            k2=k1/par_sed_huelse2017_k2_order
                        !                print*,'stolpovsky2016 anoxic k1, k2 =', k1, k2
                        case ('boudreau1997fPOC')
                           ! ### <INSERT CODE> ####################################################################################################### !
                           !
                           ! ######################################################################################################################### !
                        case default
                            ! globally invariant k1 and k2 as set in par_sed_huelse2017_k1, par_sed_huelse2017_k2
                            k1=par_sed_huelse2017_k1
                            k2=par_sed_huelse2017_k2_anoxic
                    !                           print*,' '
                    !        print*,'anoxic default degradation: k1, k2 =', k1, k2
                    ! MIN anoxic from Arndt et al. 2013
                    !            k1=6.0e-7;
                    !            k2=1.25e-8;
                    ! anoxic from PALASTANGA ET AL. 2011
                    !            if(dum_D .LE. 2000)then
                    !                k1=0.008
                    !            else
                    !                k1=0.002
                    !            end if
                    end select
                end if  ! (dum_swiconc_O2 .LE. loc_BW_O2_anoxia)
            end if  ! use anoxic rate constants

            !        print*,'k1, k2 =', k1, k2

            ! Check for no POC deposited -> nothing preserved
            if(loc_POC1_flux_swi .LE. const_real_nullsmall .AND. loc_POC2_flux_swi .LE. const_real_nullsmall  .AND. loc_POC3_flux_swi .LE. const_real_nullsmall)then
                !            print*,'no POC deposited  dum_D ', dum_D, dum_i, dum_j
                !            print*,' grid point (i,j) ', dum_i, dum_j
                !            print*,' '
                dum_sed_pres_fracC = 0.0
                dum_sed_OM_wtpc_bot = 0.0
                loc_O2_swiflux = 0.0
                loc_NO3_swiflux = 0.0
                loc_SO4_swiflux = 0.0
                loc_NH4_swiflux = 0.0
                loc_H2S_swiflux = 0.0
                loc_PO4_swiflux = 0.0
                loc_DIC_swiflux = 0.0
                loc_DIC_13C_swiflux = 0.0
                loc_ALK_swiflux = 0.0
            else
                !!!! OLD version: TOC concentration:  call sub_huelseetal2016_zTOC(loc_POC1_wtpct_swi, loc_POC2_wtpct_swi, dum_sed_pres_fracC)

                if(dum_swiconc_O2 .LE. loc_BW_O2_anoxia)then
                    ! decrease bioturbation depth
                    zbio = 0.01
                end if

                call sub_huelseetal2016_zTOC(dum_D, loc_POC1_flux_swi, loc_POC2_flux_swi, loc_POC3_flux_swi, dum_sed_pres_fracC, loc_sed_pres_insane, dum_sed_OM_wtpc_bot)
                ! CHECK IF TOC preservation results in insane values, i.e. everything remineralized
                ! Then calculate SWI-fluxes "manually"
!                if(dum_sed_pres_fracC .LE. 0.0)then
!                    print*,' '
!                    print*,'!!!!!!!! NEGATIVE POC preservation ', dum_sed_pres_fracC
!                    print*,'dum_D, dum_i, dum_j', dum_D, dum_i, dum_j
!                end if
                if((dum_sed_pres_fracC .NE. dum_sed_pres_fracC) .OR. (dum_sed_pres_fracC .LE. 0.0) .OR. (dum_sed_pres_fracC > 1.0))then
!                    print*,' '
!                    print*,'weird dum_sed_pres_fracC ', dum_sed_pres_fracC, dum_D, dum_i, dum_j
                    !                print*,'dum_D, dum_i, dum_j', dum_D, dum_i, dum_j
                    !                print*,'loc_sed_burial', loc_sed_burial
                
                    dum_sed_pres_fracC = 0.0        ! sed TOC preservation to zero
                    dum_sed_OM_wtpc_bot = 0.0
                    loc_O2_swiflux = conv_POC_cm3_mol*loc_fPOC*(-OC/SD)
                    if(ocn_select(io_NO3))then
                        ! TODO: change if NO3 is selected
                        loc_NO3_swiflux = 0.0
                        loc_NH4_swiflux = 0.0
                    end if
                    if(ocn_select(io_SO4))then
                        loc_SO4_swiflux = 0.0 !conv_POC_cm3_mol*loc_fPOC*(-SO4C/SD)
                        loc_H2S_swiflux = -loc_SO4_swiflux
                    end if
                    if(ocn_select(io_PO4))then
                        loc_PO4_swiflux = conv_POC_cm3_mol*loc_fPOC*PC1/SD
                    end if
                    if(ocn_select(io_DIC))then
                        loc_DIC_swiflux = conv_POC_cm3_mol*loc_fPOC*DICC1/SD
                    end if
                    if(ocn_select(io_DIC_13C))then
                        loc_DIC_13C_swiflux = conv_POC_cm3_mol*loc_fPOC_13C*DICC1/SD
                    end if
                    if(ocn_select(io_ALK))then
                        loc_ALK_swiflux = 2.0*loc_H2S_swiflux + conv_POC_cm3_mol*loc_fPOC*ALKROX/SD   !16/106
                    end if
            
                else ! dum_sed_pres_fracC <> NaN: normal OMEN call
                    !                print*,'Normal sed_pres ', dum_sed_pres_fracC, dum_D, dum_i, dum_j
                    !                print*,' '
                
                    !                if(dum_sed_pres_fracC .NE. 0.0)then
                    !                    print*,'Something is preserved', dum_sed_pres_fracC, dum_i, dum_j
                    !           !                STOP
                    !                end if
!                    print*,' '
!                    print*,'dum_sed_pres_fracC ', dum_sed_pres_fracC


                    if(dum_swiconc_O2 .LE. const_real_nullsmall) then   ! 10.0E-9
                        loc_O2_swiflux = 0.0            ! if negative [O2] -> no SWI flux
!                        print*,'dum_swiconc_O2 = ', dum_swiconc_O2
                    else
                        call sub_huelseetal2016_zO2(dum_D, dum_swiconc_O2, loc_O2_swiflux)
                        !                    print*,'OMEN loc_O2_swiflux = ', loc_O2_swiflux
                        if(loc_O2_swiflux .GE. 0.0)then
!                            print*,' '
                            print*,'---------- loc_O2_swiflux positiv ----------', loc_O2_swiflux, dum_i, dum_j, dum_D
				loc_O2_swiflux_pos = .true.
!                            print*,'dum_i, dum_j, dum_D', dum_i, dum_j, dum_D
!                            print*,'sedimentation flux =', loc_new_sed_vol
!                            print*,'loc_sed_burial_NEW =', loc_sed_burial
!                            print*,'1/(1-por)*loc_new_sed(is_det) = ', 1/(1-por)*loc_new_sed(is_det)
                            loc_O2_swiflux = 0.0
                        !                     STOP
			else
!                            	print*,' '
!				print*,'---------- NEGATIVE O2 SWI-flux----------'
                        end if
                    end if

                    if(ocn_select(io_NO3))then
                        call sub_huelseetal2016_zNO3(dum_swiconc_NO3, loc_NO3_swiflux)
                    else
                        zno3 = zox
!                        print*,'OMEN zno3 = ', zno3
                    end if

                    ! here check for SWI concentration, as problem with root-finding
                    ! when is zero. And as no SO4 produced no need to call subroutine anyway
                    if(ocn_select(io_SO4))then
                        if(dum_swiconc_SO4 > const_real_nullsmall)then
                            !               Hack
                            !                    loc_SO4_swiflux = loc_new_sed(is_POC)*conv_POC_cm3_mol*(-138.0/212.0)
                            call sub_huelseetal2016_zSO4(dum_swiconc_SO4, loc_SO4_swiflux)
                            !                    print*,'OMEN loc_SO4_swiflux = ', loc_SO4_swiflux
                            if(loc_SO4_swiflux > 0.0)then
!                                print*,' '
                                print*,'---------- loc_SO4_swiflux positiv ----------', loc_SO4_swiflux
                                print*,'dum_i, dum_j, dum_D', dum_i, dum_j, dum_D
                                print*,'loc_sed_burial_NEW =', loc_sed_burial
                                print*,'1/(1-por)*loc_new_sed(is_det) = ', 1/(1-por)*loc_new_sed(is_det)
                                loc_SO4_swiflux = 0.0
                            !                    !                STOP
                            end if

                        else
                            zso4 = zno3
                        end if
                    else
                        zso4 = zno3
                    end if

                    if(ocn_select(io_NH4))then
                        call sub_huelseetal2016_zNH4(dum_swiconc_NH4, loc_NH4_swiflux)
                    else
                        ! If not selected nothing needs to be done
                    end if

                    if(ocn_select(io_H2S))then
                        !               Hack
                        !                    loc_H2S_swiflux = loc_new_sed(is_POC)*conv_POC_cm3_mol*(138.0/212.0)
                        call sub_huelseetal2016_zH2S(dum_swiconc_H2S, loc_H2S_swiflux)
                    !                    print*,'OMEN loc_H2S_swiflux = ', loc_H2S_swiflux
                    ! Now check for H2S in sulphurised OM which is given back to BIOGEM (in 1:1 ratio)
                    ! in order to conserve S balance
        ! We decided to bury the H2S and 'remove' just the associated ALK (Email POC-S 05.09.2017)
!                    if(loc_new_sed(is_POM_S) .GE. const_real_nullsmall)then
!                     loc_POM_S_H2S_swiflux = loc_new_sed(is_POM_S)*conv_POC_cm3_mol*1.0
!                     loc_H2S_swiflux = loc_H2S_swiflux + loc_POM_S_H2S_swiflux
!                    !    print*,'NEW loc_H2S_swiflux ', loc_H2S_swiflux
!                    end if

                    else
                        ! If not selected nothing needs to be done
                    end if

                    if(ocn_select(io_PO4))then
                        if(par_sed_huelse2017_P_cycle)then
                            ! explicit PO4 calculation
                            call sub_huelseetal2016_zPO4_M(dum_swiconc_PO4, loc_PO4_swiflux, dum_swiflux_M, loc_M_swiflux)
!                            print*,'explicit OMEN loc_PO4_swiflux = ', loc_PO4_swiflux
!                            print*,'Hack OMEN loc_PO4_swiflux     = ', loc_fPOC*conv_POC_cm3_mol*1.0/106.0
!                            print*,' '                            
                        else
!				print*,'Do not use OMEN P-cycle '
				if(par_sed_huelse2017_sim_P_loss)then
					! PO4 hack: remineralise just the non-sulfurised POC and calculate PO4 return flux
		                        loc_PO4_swiflux = (loc_POC1_flux_swi+loc_POC2_flux_swi)*PC1/SD
!		                        print*,'Sim P-loss PO4 = ', loc_PO4_swiflux
				else								
				        ! PO4 hack: remineralise all POC and calculate PO4 return flux
				        loc_PO4_swiflux = loc_fPOC*conv_POC_cm3_mol*PC1/SD
!					print*,'all PO4 returned = ', loc_PO4_swiflux
				end if
                        end if  ! par_sed_huelse2017_P_cycle                       
                        !                        print*,' '
                        !                        print*,'Hack OMEN loc_PO4_swiflux = ', loc_PO4_swiflux
!                            print*,'CALC OMEN loc_PO4_swiflux = ', loc_PO4_swiflux
                    else
                        ! If not selected nothing needs to be done
                    end if

                    if(ocn_select(io_DIC))then
                        call sub_huelseetal2016_zDIC(dum_swiconc_DIC, loc_DIC_swiflux)
!				print*,' '
!				print*,'---------- loc_DIC_swiflux ----------', loc_DIC_swiflux
!				print*,'HACK loc_DIC_swiflux = ', loc_new_sed(is_POC)*conv_POC_cm3_mol*DICC1/SD
!                                    print*,'OMEN loc_POC_swiflux = ', conv_POC_cm3_mol*loc_fPOC
!				loc_DIC_swiflux = loc_new_sed(is_POC)*conv_POC_cm3_mol*(1-dum_sed_pres_fracC) ! DIC HACK 
                    !                loc_DIC_swiflux = loc_new_sed(is_POC)*conv_POC_cm3_mol ! *SD   DIC HACK all returned
!			if(loc_O2_swiflux_pos)then
!				print*,'loc_new_sed(is_POC) = ', loc_new_sed(is_POC)*conv_POC_cm3_mol
!				print*,'dum_sed_pres_fracC = ', dum_sed_pres_fracC
!				print*,'(1-dum_sed_pres_fracC)*POCin = ', (1-dum_sed_pres_fracC)*loc_new_sed(is_POC)*conv_POC_cm3_mol
!				print*,'loc_DIC_swiflux =', loc_DIC_swiflux
!			end if
                    else
                        ! If not selected nothing needs to be done
                    end if

                    if(ocn_select(io_DIC_13C))then
!                        call sub_huelseetal2016_zDIC(dum_swiconc_DIC, loc_DIC_swiflux)
                    ! not explicitly modelled - assume no fractionation during remineralisation in sediment
                    ! calculate as DIC_flux_OUT / POC_flux_IN * POC_13C_IN
                        loc_DIC_13C_swiflux = loc_DIC_swiflux/(conv_POC_cm3_mol*loc_fPOC)*(conv_POC_cm3_mol*loc_fPOC_13C)
!                                    print*,'OMEN OUT: loc_DIC_13C_swiflux = ', loc_DIC_13C_swiflux
                    !                loc_DIC_swiflux = loc_new_sed(is_POC)*conv_POC_cm3_mol ! *SD   DIC hack
                    else
                        ! If not selected nothing needs to be done
                    end if

                    if(loc_calc_ALK)then
                        if(ocn_select(io_ALK))then
                            call sub_huelseetal2016_zALK(dum_swiconc_ALK, loc_ALK_swiflux)
!                            print*,'CAL loc_ALK_swiflux = ', loc_ALK_swiflux
!                            print*,'POC burial = ', dum_POC_total_flux_zinf*ALKROX/SD
                            loc_ALK_swiflux = loc_ALK_swiflux + dum_POC_total_flux_zinf*ALKROX/SD
!                            print*,'FINAL loc_ALK_swiflux = ', loc_ALK_swiflux
!                            print*,' '
			    if(par_sed_huelse2017_remove_impl_sulALK)then
				! JUST FOR CLOSED-SYSTEM: Account for burial of negative ALK with OM
				! will just the the sulf affect on OM-burial
				! We decided to bury the H2S and 'remove' just the associated ALK (Email POC-S 05.09.2017)
		                    if(loc_new_sed(is_POM_S) .GE. const_real_nullsmall)then
		                        loc_POM_S_H2S_swiflux = loc_new_sed(is_POM_S)*conv_POC_cm3_mol*1.0
		                        loc_ALK_swiflux = loc_ALK_swiflux - 2*loc_POM_S_H2S_swiflux
	!                                print*,'loc_POM_S_H2S_swiflux ', loc_POM_S_H2S_swiflux
	!                                print*,'NEW loc_ALK_swiflux ', loc_ALK_swiflux
	!                                print*,' '
		                    end if
			    end if
                        else
                            ! If not selected nothing needs to be done
                        end if
                    else    ! use ALK hack
!                        print*,'ALK HACK '
                        loc_ALK_swiflux = 2.0*loc_H2S_swiflux + loc_new_sed(is_POC)*conv_POC_cm3_mol*ALKROX/SD  !16/106 !NC1
			    if(par_sed_huelse2017_remove_impl_sulALK)then
				! JUST FOR CLOSED-SYSTEM: Account for burial of negative ALK with OM
				! will just the the sulf affect on OM-burial
				! We decided to bury the H2S and 'remove' just the associated ALK (Email POC-S 05.09.2017)
		                    if(loc_new_sed(is_POM_S) .GE. const_real_nullsmall)then
		                        loc_POM_S_H2S_swiflux = loc_new_sed(is_POM_S)*conv_POC_cm3_mol*1.0
		                        loc_ALK_swiflux = loc_ALK_swiflux - 2*loc_POM_S_H2S_swiflux
!	                                print*,'ALK hack - remove impl ALk'
		                    end if
			    end if
                    end if      ! loc_calc_ALK

                end if  ! dum_sed_pres_fracC <> NaN
        
            end if  ! loc_POC1/2_wtpct_swi .LE. const_real_nullsmall

        end if ! (loc_new_sed(is_det) .LE. const_real_nullsmall)
        !        ! SO4 drift check:
        !        if(abs(loc_SO4_swiflux) .NE. abs(loc_H2S_swiflux))then
        !            print*,'Fluxes SO4 <> H2S ', loc_SO4_swiflux, loc_H2S_swiflux
        !        else
        !            print*,'.'
        !        end if

        !        if(loc_O2_swiflux > 0.0)then
        !            print*,' '
        !            print*,'---------- loc_O2_swiflux positiv ----------'
        !            print*,'loc_O2_swiflux ', loc_O2_swiflux
        !            print*,'loc_SO4_swiflux ', loc_SO4_swiflux
        !            print*,'loc_H2S_swiflux ', loc_H2S_swiflux
        !            print*,'loc_ALK_swiflux ', loc_ALK_swiflux
        !            print*,'dum_i, dum_j, dum_D', dum_i, dum_j, dum_D
        !        end if


        ! Now pass back the values to the global field
        dum_new_swifluxes(io_O2) = loc_O2_swiflux                                   ! Dom TODO convert mol*cm^-2 yr^-1 (SEDIMENT) -> mol yr^-1 (GENIE)
        if(ocn_select(io_NO3))then
            dum_new_swifluxes(io_NO3) = loc_NO3_swiflux                             ! Dom TODO convert mol*cm^-2 yr^-1 (SEDIMENT) -> mol yr^-1 (GENIE)
            dum_new_swifluxes(io_NH4) = loc_NH4_swiflux                             ! Dom TODO convert mol*cm^-2 yr^-1 (SEDIMENT) -> mol yr^-1 (GENIE)
        end if
        if(ocn_select(io_SO4))then
            dum_new_swifluxes(io_SO4) = loc_SO4_swiflux                             ! Dom TODO convert mol*cm^-2 yr^-1 (SEDIMENT) -> mol yr^-1 (GENIE)
            dum_new_swifluxes(io_H2S) = loc_H2S_swiflux                             ! Dom TODO convert mol*cm^-2 yr^-1 (SEDIMENT) -> mol yr^-1 (GENIE)
        end if
        if(ocn_select(io_PO4))then
            dum_new_swifluxes(io_PO4) = loc_PO4_swiflux                             ! Dom TODO convert mol*cm^-2 yr^-1 (SEDIMENT) -> mol yr^-1 (GENIE)
        end if
        if(ocn_select(io_DIC))then
            dum_new_swifluxes(io_DIC) = loc_DIC_swiflux                             ! Dom TODO convert mol*cm^-2 yr^-1 (SEDIMENT) -> mol yr^-1 (GENIE)
        end if
        if(ocn_select(io_DIC_13C))then
            dum_new_swifluxes(io_DIC_13C) = loc_DIC_13C_swiflux                     ! Dom TODO convert mol*cm^-2 yr^-1 (SEDIMENT) -> mol yr^-1 (GENIE)
        end if
        !        if(loc_calc_ALK)then
        if(ocn_select(io_ALK))then
            dum_new_swifluxes(io_ALK) = loc_ALK_swiflux                             ! Dom TODO convert mol*cm^-2 yr^-1 (SEDIMENT) -> mol yr^-1 (GENIE)
        end if
        !        end if      ! loc_calc_ALK

!	print*,' '
!	print*,'dum_D = ', dum_D
!	print*,'loc_POC_flux_swi_total [mol/(cm^2 yr)] = ', loc_POC1_flux_swi + loc_POC2_flux_swi
!	print*,'FINAL DIC SWI flux [mol/(cm^2 yr)] = ', dum_new_swifluxes(io_DIC)
!	print*,'CALCU DIC SWI flux [mol/(cm^2 yr)] = ' , (1-dum_sed_pres_fracC)*(loc_POC1_flux_swi + loc_POC2_flux_swi)
!	print*,'dum_sed_pres_fracC =' , dum_sed_pres_fracC

        if(loc_print_results) then
            !            if(dum_D < 1000.0)then
            !            if(loc_O2_swiflux > 0.0)then
!            print*,'dum_D = ', dum_D
!            print*,' grid point (i,j) = ', dum_i, dum_j
            print*,'Temp C =', dum_sfcsumocn(io_T) - 273.15
            print*,'loc_POC1_flux_swi = ', loc_POC1_flux_swi
            print*,'loc_POC2_flux_swi = ', loc_POC2_flux_swi
	    print*,'loc_POC3_flux_swi = ', loc_POC3_flux_swi
	    print*,'TOTAL POC3_flux   = ', loc_POC1_flux_swi+loc_POC2_flux_swi+loc_POC3_flux_swi
            print*,'Fraction POC-pres =' , dum_sed_pres_fracC
            print*,'(1-presC)*POCin   = ', (1-dum_sed_pres_fracC)*(loc_POC1_flux_swi+loc_POC2_flux_swi+loc_POC3_flux_swi)
            print*,'FINAL DIC SWI flx = ', dum_new_swifluxes(io_DIC)
            print*,'loc_sed_burial    = ', loc_sed_burial/dum_dtyr
            print*,'dum_swiconc_O2 = ', dum_swiconc_O2
            print*,'dum_swiconc_SO4 = ', dum_swiconc_SO4
            print*,'dum_swiconc_H2S = ', dum_swiconc_H2S
            print*,'dum_swiconc_PO4 = ', dum_swiconc_PO4
            print*,'dum_swiflux_M = ', dum_swiflux_M
            print*,'dum_swiconc_DIC = ', dum_swiconc_DIC
            print*,'dum_swiconc_ALK = ', dum_swiconc_ALK

            print*,' '
            print*,'zox = ', zox
            print*,'FINAL O2 SWI flux = ', dum_new_swifluxes(io_O2)
            print*,'zno3 = ', zno3
            print*,'FINAL NO3 SWI flux = ', loc_NO3_swiflux
            print*,'zso4 = ', zso4
            print*,'FINAL SO4 SWI flux = ', dum_new_swifluxes(io_SO4)
            print*,'FINAL NH4 SWI flux = ', loc_NH4_swiflux
            print*,'FINAL H2S SWI flux = ', dum_new_swifluxes(io_H2S)
            print*,'FINAL PO4 SWI flux = ', dum_new_swifluxes(io_PO4)
            print*,'FINAL M SWI flux = ', loc_M_swiflux
            print*,'FINAL DIC SWI flux = ', dum_new_swifluxes(io_DIC)
            print*,'FINAL ALK SWI flux = ', loc_ALK_swiflux
            print*,'Fraction POC-preserved/POC-deposited =' , dum_sed_pres_fracC
            print*,' '

            !            print*,'loc_new_sed(is_CaCO3)= ', loc_new_sed(is_CaCO3)
            !            print*,'loc_new_sed(is_opal)= ', loc_new_sed(is_opal)
            !            print*,'loc_wtpct = ', loc_wtpct
            !            print*,'SWI wt% POC frac 1 = ', loc_POC1_wtpct_swi
            !            print*,'SWI wt% POC frac 2 = ', loc_POC2_wtpct_swi
        !                STOP
            !            print*,' '
            
            !loc_filename=trim(par_outdir_name)//'ecogem_series_resources_'//fun_conv_num_char_n(2,i)//fun_conv_num_char_n(2,j)
            !OPEN(88,file=loc_filename,action='write',position='append',iostat=ios)
            !CALL check_iostat(ios,__LINE__,__FILE__)
            !WRITE(88,fmt='(f12.3,2e15.7)',iostat=ios) &
            !    & dum_t-(dum_dtyr*par_data_save_slice_n)/2, &
            !    & int_resources(loc,:)
            !CALL check_iostat(ios,__LINE__,__FILE__)
            !CLOSE(88,iostat=ios)
            !CALL check_iostat(ios,__LINE__,__FILE__)
        !            end if
        end if

        dum_sed_pres_fracP = dum_sed_pres_fracC

        ! calculate mean OM concentration [wt%] in upper x cm
        ! convert from mol/cm3 to wt%
        ! just two fractions:
        !        dum_sed_mean_OM = 1.0/loc_mixed_layer * 100.0*12.0/rho_sed*FUN_calcOM(0.0, loc_mixed_layer, 1.0, 1.0)

        ! with third inert/sulfurised fraction
        dum_sed_mean_OM = 1.0/loc_mixed_layer * 100.0*12.0/rho_sed*FUN_calcOM(0.0, loc_mixed_layer, 1.0, 1.0) + 100.0*12.0/rho_sed*dum_POC3_conc_swi
    !       print*,'dum_sed_mean_OM = ', dum_sed_mean_OM
    !       print*,'POC3_wtprc  ', 100.0*12.0/rho_sed*dum_POC3_conc_swi
    !        if(isnan(dum_sed_mean_OM))then
    !            print*,'NNNNNNNNNNNNAAAAAAAAAAAAAAAANNNNNNNNNNN dum_sed_mean_OM = ', dum_sed_mean_OM
    !        end if
    !    print*,'END dum_sed_OM_wtpc_bot = ', dum_sed_OM_wtpc_bot

    end SUBROUTINE sub_huelseetal2016_main
    
    !------------------------------------------------------------------------------------
    !   *****************************************************************
    !   *****************************************************************

    !                           INITIALIZE

    !   *****************************************************************
    !   *****************************************************************
    !------------------------------------------------------------------------------------
    
    SUBROUTINE sub_huelseetal2016_initialize(dum_D, dum_TempK, dum_depos_rate)
        !   __________________________________________________________
        !
        !   initalize
        !   __________________________________________________________

        real,INTENT(in)::dum_D                      ! ocean depth (m) +vs downwards
        real,INTENT(in)::dum_TempK                  ! temperature (K)
        real,INTENT(in)::dum_depos_rate             ! sedimentation rate, cm/yr / advection
        
        ! local variables
        real loc_TempC                            ! temperature (degree C)         

        !        print*, ' '
        !        print*, '----------- start sub_huelseetal2016_initialize --------------'


        ! *********************************************************************************
        !
        ! initialize globally (so just once in the very beginning)
        !
        ! *********************************************************************************       
        
        rho_sed = 2.6                                       ! sediment density (g/cm3)
        z0 = 0.0                                            ! top of the sediments
        zox = 0.0
        zno3 = 0.0
        zso4 = 0.0
        zinf = 100.0                                        ! Inifinity - bottom of the sediments (cm)
        zbio = 10.0                                        ! bioturbation depth (cm)

        Dbio = 5.2*(10.0**(0.7624-0.0003972*dum_D))        !bioturbation coefficient (cm2/yr) - after Middelburg at al. 1997
        Dunbio = 0.01                                       ! "diffusion coefficient" for unbioturbated layer - in case we use it
        tort = 3.0                                          ! tortuosity (-)
        irrigationFactor = 1.0

        gamma = 0.9                                         ! fraction of NH4 that is oxidised in oxic layer
        gammaH2S = 0.95                                     ! fraction of H2S that is oxidised in oxic layer
        gammaCH4 = 1.0     !0.99                            ! fraction of CH4 that is oxidised at SO4
        satSO4 = 0.0                                        ! SO4 saturation

        KNH4 = 1.3                                          !Adsorption coefficient (same in oxic and anoxic layer) (-)

        zoxgf = 0.1            ! was 0.1                             ! cm, rolloff NH4, H2S oxidation for small zox depth
        r_zxf=0.0

        dispFactor=por**(tort-1.0)*irrigationFactor             ! dispersion factor (-) - Ausbreitung - type of mixing that accompanies hydrodynamic                                    ! flows -> ~builds out paths of flow
        SD=(1.0-por)/por                                          ! volume factor solid->dissolved phase
        OC= SD*(138.0/106.0)!1.0*SD                                               ! O2/C (mol/mol)
        NC1=0.0 !16.0/106.0*SD  ! 0.1509*SD                                        ! N/C first TOC fraction (mol/mol)
        NC2=0.0 !16.0/106.0*SD  !0.13333*SD                                          ! N/C second TOC fraction (mol/mol)
        PC1=SD*1.0/106.0 !0.0094*SD                                          ! P/C first TOC fraction (mol/mol)
        PC2=SD*1.0/106.0 !0.0094*SD                                          ! P/C second TOC fraction (mol/mol)
        SO4C=SD*(138.0/212.0)!0.5*SD                                             ! SO4/C (mol/mol)
        O2H2S=2.0                                               ! 2 mole O2 oxidize 1 mole H2S
        DICC1=1.0*SD                                           ! DIC/C until zSO4 (mol/mol)
        DICC2=0.5*SD                                           ! DIC/C below zSO4 (mol/mol)
        MC=0.5*SD                                              ! CH4/C (mol/mol)
        NO3CR=(94.4/106.0)*SD                                    ! NO3 consumed by Denitrification

        X_C=106.0                                                 ! Carbon Redfield stoichiometry
        Y_N=16.0                                                  ! Nitrogen Redfield stoichiometry
        Z_P=1.0                                                   ! Phosphorous Redfield stoichiometry

        ALKROX= -((Y_N)/X_C)*SD                 ! Aerobic degradation -16/106*SD
        ALKRNIT=0.0                             ! Nitrification explicit -2.0
        ALKRDEN=0.0                             ! Denitrification explicit: (4*X_C+3*Y_N-10*Z_P)/(5*X_C)*SD
        ALKRSUL= ((X_C+Y_N)/X_C)*SD       ! ((X_C+Y_N)/X_C)*SD = +122/106*SD!,  Sulfate reduction (N explicit: ((X_C+Y_N-2*Z_P)/X_C)*SD = +120/106*SD)
        ALKRH2S= -2.0                           ! H2S oxydation
        !        ALKRH2S= 0.0       ! no secondary redox!
        ALKRMET= -((Y_N)/X_C)*SD   !0.0    ! Methanogenesis explicitly: ((Y_N-2*Z_P)/X_C)*SD
        ALKRAOM= 2.0     !0.0                   ! AOM
        
        ! ORGANIC MATTER
        DC1 = Dbio
        DC2 = Dunbio
               
        !        if(dum_D<1000)then
        !            print*, 'dum_D, dum_depos_rate, k1, k2 ', dum_D, dum_depos_rate, k1, k2
        !        end if

        ! GLOBAL DIFFUSION COEFFICIENTS
        ! O2
        qdispO2=348.62172
        adispO2=14.08608

        ! Nitrate (NO3)
        qdispNO3=308.42208
        adispNO3=12.2640
        qdispNH4=309.0528
        adispNH4=12.2640
                
        ! Sulfate (SO4) - Hydrogen sulfide (H2S)
        qdispSO4=157.68                 ! SO4 diffusion coefficient in water at 0 degree C  (cm2/yr)
        adispSO4=7.884                  ! SO4 linear coefficient for temperature dependence (cm2/yr/oC)
        qdispH2S=307.476
        adispH2S=9.636

        ! Phosphate (PO4)
        qdispPO4=112.90764
        adispPO4=5.586252

        ! DIC
        qdispDIC=151.69                 ! DIC diffusion coefficient in water (cm2/yr)
        adispDIC=7.93                   ! DIC linear coefficient for temperature dependence (cm2/yr/oC)

        ! Alkalinity
        qdispALK=151.69                 ! ALK diffusion coefficient in water (cm2/yr)
        adispALK=7.93                   ! ALK linear coefficient for temperature dependence (cm2/yr/oC)


        ! *********************************************************************************

        ! initialize locally

        ! *********************************************************************************

        loc_TempC = dum_TempK - 273.15

        w = dum_depos_rate              ! sedimentation rate, cm/yr / burial velocity / advection
        !        w=10.0**(-0.87478367-0.00043512*dum_D)*3.3              ! sedimentation rate, cm/yr / burial velocity / advection (Middelburg et al., Deep Sea Res. 1, 1997)


        ! Diffusion coefficients
        ! O2
        DO21=(qdispO2+adispO2*loc_TempC)*dispFactor+Dbio        ! O2 diffusion coefficient in bioturbated layer (cm2/yr)
        DO22=(qdispO2+adispO2*loc_TempC)*dispFactor             ! O2 diffusion coefficient in non-bioturbated layer (cm2/yr)


        ! Nitrate (NO3) - Ammonium (NH4)        
        DN1=(qdispNO3+adispNO3*loc_TempC)*dispFactor+Dbio
        DN2=(qdispNO3+adispNO3*loc_TempC)*dispFactor
        DNH41=((qdispNH4+adispNH4*loc_TempC)*dispFactor+Dbio)/(1.0+KNH4)
        DNH42=((qdispNH4+adispNH4*loc_TempC)*dispFactor)/(1.0+KNH4)

        ! Sulfate (SO4) - Hydrogen sulfide (H2S)
        DSO41=(qdispSO4+adispSO4*loc_TempC)*dispFactor+Dbio     ! SO4 diffusion coefficient in bioturbated layer (cm2/yr)
        DSO42=(qdispSO4+adispSO4*loc_TempC)*dispFactor          ! SO4 diffusion coefficient in non-bioturbated layer (cm2/yr)
        DH2S1=(qdispH2S+adispH2S*loc_TempC)*dispFactor+Dbio
        DH2S2=(qdispH2S+adispH2S*loc_TempC)*dispFactor

        ! Phosphate (PO4)
        DPO41=((qdispPO4+adispPO4*loc_TempC)*dispFactor+Dbio)               ! PO4 diffusion coefficient in bioturbated layer (cm2/yr)
        DPO42=((qdispPO4+adispPO4*loc_TempC)*dispFactor);                   ! PO4 diffusion coefficient in non-bioturbated layer (cm2/yr)
        KPO4_ox = 200.0   ! 0.0                 ! Adsorption coefficient in oxic layer (-)
        KPO4_anox = 1.3   ! 0.0                ! Adsorption coefficient in anoxic layer (-)

        dum_swiflux_M = 365*0.2e-10     ! Flux input 365*0.2e-10 flux of M to the sediment (mol/(cm2*yr)) - this needs to equal advective flux in sediment z=0
        ! dum_swiflux_M = 365.0*0.2e-10*1/(1-por)*1/w ! Flux converted to concentration (NOT NEEDED!!!)

        !       WAS BEFORE:
        !        ksPO4 = 1.0       ! 0.0  !          ! Rate constant for kinetic P sorption (1/yr)
        !        kmPO4 = 2.2e-6*24*365  ! 0.0                 ! Rate constant for Fe-bound P release upon Fe oxide reduction
        !        kaPO4 = 10.0      ! 0.0             ! Rate constant for authigenic P formation (1/yr)
        !        PO4s = 1.0e-9     ! 0.0               ! Equilibrium concentration for P sorption (mol/cm3)
        !        PO4a = 3.7e-9     ! 0.0              ! Equilibrium concentration for authigenic P formation (mol/cm3)
        !        Minf = 1.99e-10    ! 0.0                ! asymptotic concentration for Fe-bound P (mol/cm3)

        ! Palastanga et al. (2011)
        kmPO4 = 0.19  ! 0.05               ! Rate constant for Fe-bound P release upon Fe oxide reduction (from Slomp et al. (1996)
        kaPO4 = 0.37      ! 0.0             ! Rate constant for authigenic P formation (1/yr)
        PO4a = 3.7e-9     ! 0.0              ! Equilibrium concentration for authigenic P formation (mol/cm3)
        if(dum_D .LE. 2000)then     ! sediment margins
            ksPO4 = 36.5       ! 0.0            ! Rate constant for kinetic P sorption (1/yr)
            PO4s = 2.0e-9     ! 0.0               ! Equilibrium concentration for P sorption (mol/cm3)
            Minf = 5.2e-15    ! 2.0e-15                ! asymptotic concentration for Fe-bound P (mol/cm3)
        else        ! deep sea sediments
            ksPO4 = 3.65       ! lower sorption rate for deep sea   ! Rate constant for kinetic P sorption (1/yr)
            PO4s = 2.0e-9     ! DH 09/07/18 was 12.0e-9 (but too high)             ! Equilibrium concentration for P sorption (mol/cm3)
            Minf = 5.2e-15       ! Palastanga: 5.2e-9 ! 0.0                ! asymptotic concentration for Fe-bound P (mol/cm3)
        end if

        ! DIC
        DDIC1=(qdispDIC+adispDIC*loc_TempC)*dispFactor+Dbio                 ! DIC diffusion coefficient in bioturbated layer (cm2/yr)
        DDIC2=(qdispDIC+adispDIC*loc_TempC)*dispFactor                      ! DIC diffusion coefficient in non-bioturbated layer (cm2/yr)

        ! Alkalinity
        DALK1=(qdispALK+adispALK*loc_TempC)*dispFactor+Dbio;                ! ALK diffusion coefficient in bioturbated layer (cm2/yr)
        DALK2=(qdispALK+adispALK*loc_TempC)*dispFactor;                     ! ALK diffusion coefficient in non-bioturbated layer (cm2/yr)

    end SUBROUTINE sub_huelseetal2016_initialize
    

    !------------------------------------------------------------------------------------
    !   *****************************************************************
    !   *****************************************************************

    !                           TOC

    !   *****************************************************************
    !   *****************************************************************
    !------------------------------------------------------------------------------------

    SUBROUTINE sub_huelseetal2016_zTOC(dum_D, dum_POC1_flux_swi, dum_POC2_flux_swi, dum_POC3_flux_swi, dum_sed_pres_fracC, dum_sed_pres_insane, dum_sed_OM_wtpc_bot)
        !   __________________________________________________________
        !
        !   calculate benthic burial/recycling fluxes (see documentation for details!)
        !   __________________________________________________________

        !   organic matter burial 2 fractions

        ! dummy arguments
        real,INTENT(in)::dum_POC1_flux_swi, dum_POC2_flux_swi, dum_POC3_flux_swi   ! POC flux at SWI   [mol/(cm2 yr)]
        real,INTENT(inout)::dum_sed_pres_fracC                              ! POC concentrations at zinf
        REAL,INTENT(in)::dum_D                                     ! depth
        logical, INTENT(inout) :: dum_sed_pres_insane                       ! true if integration constants are too high -> calculate SWI-flux manually
        real,INTENT(inout)::dum_sed_OM_wtpc_bot                     ! POC wt% at bottom of sediment column (geologic preservation)
        ! local variables
        !        real loc_POC1_conc_swi, dum_POC2_conc_swi                           ! POC concentration at SWI   [mol/cm3]
        real loc_POC1_conc_zinf, loc_POC2_conc_zinf
        !    real aa11, bb11, aa21, A11, A21, aa12, bb12, aa22, A12, A22
        real dC1dz, C1flx, dC2dz, C2flx, Cflx                             ! Cflx: Sed input flux to upper boundary, per cm^2 water column
        real F_TOC1, F_TOC2, F_TOC                                        ! Flux through lower boundary zinf, per cm^2 water-column


!		print*,' '
!		print*,' ------------------ START zTOC ---------------------'
!	       	print*,' sedimentation rate/burial velocity w = ', w
!	        print*,'dum_D,  w = ', dum_D, w
!		print*, 'dum_POC1_flux_swi', char(9), dum_POC1_flux_swi
!               print*, 'dum_POC2_flux_swi', char(9), dum_POC2_flux_swi

        ! Dom: Use this when comparing with MATLAB, here we use wt% of g -> *1/12
        !        loc_POC1_conc_swi=0.01*dum_POC1_wtpct_swi/12.0*rho_sed              ! %TOC concentration frac1 at SWI (wt%) -> (mol/cm3 bulk phase)
        !        dum_POC2_conc_swi=0.01*dum_POC2_wtpct_swi/12.0*rho_sed              ! %TOC concentration frac2 at SWI (wt%) -> (mol/cm3 bulk phase)
        	

        ! calculate concentration (mol/cm3) at very top of sediments not accounting for biodiffusion (just account for advection)
	! NOTE: this is needed when calculating final fraction of POC preservation
	! if taking [POC] at SWI including biodiffusion term (e.g. dum_POC1_conc_swi) 
	! the preservation is too high (as not all incoming POC is accounted for)
	dum_POC1_conc_swi_nonbio = dum_POC1_flux_swi*1/(1-por)*1/w
	dum_POC2_conc_swi_nonbio = dum_POC2_flux_swi*1/(1-por)*1/w
        dum_POC3_conc_swi = dum_POC3_flux_swi*1/(1-por)*1/w

        aa11 = (w-sqrt(w**2+4*DC1*k1))/(2*DC1)
        bb11 = (w+sqrt(w**2+4*DC1*k1))/(2*DC1)
        aa21 = (-k1/w)


        ! calculate TOC SWI concentration from flux
        dum_POC1_conc_swi = (dum_POC1_flux_swi*(-aa11*exp(aa11*zbio)+bb11*exp(bb11*zbio)))/(-DC1*bb11*aa11*exp(bb11*zbio) + DC1*bb11*aa11*exp(aa11*zbio) + &
        DC1*bb11*aa11*por*exp(bb11*zbio) - DC1*bb11*aa11*por*exp(aa11*zbio) - w*aa11*exp(aa11*zbio) + w*bb11*exp(bb11*zbio) + &
        w*por*aa11*exp(aa11*zbio) - w*por*bb11*exp(bb11*zbio))

        A11 = -(dum_POC1_conc_swi*bb11*exp(bb11*zbio))/(aa11*exp(aa11*zbio)-bb11*exp(bb11*zbio)) !+const_real_nullsmall) !/100000000)
        !        if(exp(aa21*zbio) > const_real_nullsmall) then
        A21=(A11*(exp(aa11*zbio)-exp(bb11*zbio))+dum_POC1_conc_swi*exp(bb11*zbio))/(exp(aa21*zbio)) !+const_real_nullsmall) !/100000000)
        !        else
        !            A21=(A11*(exp(aa11*zbio)-exp(bb11*zbio))+dum_POC1_conc_swi*exp(bb11*zbio))/const_real_nullsmall
        !            print*,'in small exp(aa21*zbio)', +exp(aa21*zbio)
        !        end if
        aa12 = (w-sqrt(w**2+4*DC1*k2))/(2*DC1)
        bb12 = (w+sqrt(w**2+4*DC1*k2))/(2*DC1)
        aa22 = (-k2/w)

        ! calculate TOC SWI concentration from flux
        dum_POC2_conc_swi = (dum_POC2_flux_swi*(-aa12*exp(aa12*zbio)+bb12*exp(bb12*zbio)))/(-DC1*bb12*aa12*exp(bb12*zbio) + DC1*bb12*aa12*exp(aa12*zbio) + &
        DC1*bb12*aa12*por*exp(bb12*zbio) - DC1*bb12*aa12*por*exp(aa12*zbio) - w*aa12*exp(aa12*zbio) + w*bb12*exp(bb12*zbio) + &
        w*por*aa12*exp(aa12*zbio) - w*por*bb12*exp(bb12*zbio))

        !        print*,' '
        !        print*,'dum_POC1_conc_swi = ', 100.0*12.0/rho_sed*dum_POC1_conc_swi
        !        print*,'dum_POC2_conc_swi = ', 100.0*12.0/rho_sed*dum_POC2_conc_swi
        !        print*,'dum_POC3_conc_swi = ', 100.0*12.0/rho_sed*dum_POC3_conc_swi

        A12=-(dum_POC2_conc_swi*bb12*exp(bb12*zbio))/(aa12*exp(aa12*zbio)-bb12*exp(bb12*zbio)) !+const_real_nullsmall) !/100000000)
        A22=(A12*(exp(aa12*zbio)-exp(bb12*zbio))+dum_POC2_conc_swi*exp(bb12*zbio))/(exp(aa22*zbio)) !+const_real_nullsmall) !/100000000)

        !        print*, 'const_real_nullsmall', const_real_nullsmall
        !        print*,' '

        !!! no need for this as this is SWI concentration for z0 = 0!
        !!!    ! % Calculate concentration at z0
        !!!    if(z0<=zbio) then
        !!!        C1=A11*(exp(aa11*z0)-exp(bb11*z0))+dum_POC1_conc_swi*exp(bb11*z0)
        !!!        C2=A12*(exp(aa12*z0)-exp(bb12*z0))+dum_POC2_conc_swi*exp(bb12*z0)
        !!!    else
        !!!        C1=A21*exp(aa21*z0)
        !!!        C2=A22*exp(aa22*z0)
        !!!    end if
        !!!    C = C1 + C2
        !!!
        !!!    print*, 'C = C1 + C2 ', C
        !!!    print*, ' '

        ! Cflx: Sed input flux to upper boundary, per cm^2 water column
        if(z0 < zbio) then
            dC1dz =  A11*(aa11*exp(aa11*z0)-bb11*exp(bb11*z0))+dum_POC1_conc_swi*bb11*exp(bb11*z0)
            C1flx = - (1-por)*(-DC1*dC1dz + w*dum_POC1_conc_swi)
            dC2dz =  A12*(aa12*exp(aa12*z0)-bb12*exp(bb12*z0))+dum_POC2_conc_swi*bb12*exp(bb12*z0)
            C2flx = - (1-por)*(-DC1*dC2dz + w*dum_POC2_conc_swi)
        else
            C1flx = - (1-por)*w*dum_POC1_conc_swi
            C2flx = - (1-por)*w*dum_POC2_conc_swi
        end if
        Cflx = C1flx + C2flx

        !        print*, 'Cflx swi', char(9), Cflx
        !        print*, 'C1flx swi', char(9), C1flx
        !        print*, 'C2flx swi', char(9), C2flx


        ! Flux through lower boundary zinf, per cm^2 water-column (Dom 08.02.2018 was -(1-por)*w*A21*exp(aa21*zinf))
        F_TOC1 = (1-por)*w*A21*exp(aa21*zinf)
        F_TOC2 = (1-por)*w*A22*exp(aa22*zinf)
        F_TOC = F_TOC1 + F_TOC2
!        print*, 'F_TOC1 zinf', char(9), F_TOC1
!        print*, 'F_TOC2 zinf', char(9), F_TOC2
        !        print*, 'F_TOC zinf', char(9), F_TOC

        ! Concentration at lower boundary zinf
        if(zinf<zbio) then
            loc_POC1_conc_zinf=A11*(exp(aa11*zinf)-exp(bb11*zinf))+dum_POC1_conc_swi*exp(bb11*zinf)
            loc_POC2_conc_zinf=A12*(exp(aa12*zinf)-exp(bb12*zinf))+dum_POC2_conc_swi*exp(bb12*zinf)
        else
            loc_POC1_conc_zinf=A21*exp(aa21*zinf)
            loc_POC2_conc_zinf=A22*exp(aa22*zinf)
        end if

        ! DH: need to give back fraction buried of initially deposited (so fraction of the input values to this subroutine)
        !        print*, 'loc_POC1_conc_zinf ', char(9), loc_POC1_conc_zinf
        !        print*, 'loc_POC2_conc_zinf ', char(9), loc_POC2_conc_zinf

        ! just two fractions
        !        dum_sed_pres_fracC = (loc_POC1_conc_zinf+loc_POC2_conc_zinf)/(dum_POC1_conc_swi+dum_POC2_conc_swi) !+const_real_nullsmall)
        ! with third inert/sulfurised fraction
        ! Flux through lower boundary zinf, mol cm^-2 yr^-1
        dum_POC_total_flux_zinf = F_TOC1 + F_TOC2 + dum_POC3_flux_swi
!        print*, 'F_TOC1, F_TOC2, dum_POC3_flux_swi', F_TOC1, F_TOC2, dum_POC3_flux_swi
!        print*, ' '

	! new, correct preservation:
        dum_sed_pres_fracC = (loc_POC1_conc_zinf+loc_POC2_conc_zinf+dum_POC3_conc_swi)/(dum_POC1_conc_swi_nonbio+dum_POC2_conc_swi_nonbio+dum_POC3_conc_swi) !+const_real_nullsmall)
	! old wrong preservation:
!        dum_sed_pres_fracC = (loc_POC1_conc_zinf+loc_POC2_conc_zinf+dum_POC3_conc_swi)/(dum_POC1_conc_swi+dum_POC2_conc_swi+dum_POC3_conc_swi) !+const_real_nullsmall)
!	print*, ' '
!	print*, 'dum_sed_pres_fracC', dum_sed_pres_fracC

        dum_sed_OM_wtpc_bot = 100.0*12.0/rho_sed*(loc_POC1_conc_zinf+loc_POC2_conc_zinf+dum_POC3_conc_swi)

    !       ! this catches too many cases, sometimes A2i is large but Corg preservation can still be calculated
    !        if(abs(A21) .GE. 1/const_real_nullsmall .OR. abs(A22) .GE. 1/const_real_nullsmall)then
    !            dum_sed_pres_insane = .TRUE.
    !            print*, '1. dum_sed_pres_insane, A21 or A22 HUGE: abs(A21), abs(A22) ', abs(A21), abs(A22), dum_D
    !        end if

    !    print*, ' '
    !    print*, 'F_TOC2', char(9), F_TOC2
    !    print*, 'F_TOC', char(9), F_TOC


    end SUBROUTINE sub_huelseetal2016_zTOC



    !------------------------------------------------------------------------------------
    !   *****************************************************************
    !   *****************************************************************

    !                               Oxygen

    !   *****************************************************************
    !   *****************************************************************
    !------------------------------------------------------------------------------------

    SUBROUTINE sub_huelseetal2016_zO2(dum_D, dum_swiconc_O2, loc_new_swiflux_O2)

        ! dummy arguments
        !        integer,intent(in) :: dum_i, dum_j                      ! grid point (i,j)
        real,INTENT(in)::dum_D                      ! ocean depth (m) +vs downwards
        real,INTENT(in)::dum_swiconc_O2          ! O2 concentrations at SWI
        real,INTENT(inout)::loc_new_swiflux_O2      ! O2 flux: TODO check! (+) sediment -> bottom waters

        ! local variables
        real flxzox, conczox, loc_conczinf, fun0, zL, tol
        integer bctype

        !    print*, ''
        !    print*, ''
        !        print*, '---------------------- START zO2 ------------------------ '
        !        print*,'--- SWI O2 =', dum_swiconc_O2
        zox = 1e-10
        flxzox = 0.0
        conczox = 0.0
        loc_conczinf = 0.0
        loc_new_swiflux_O2 = 0.0
        fun0 = 0.0
        zL = 0.0
        tol = 0.0
        !        flxswi = 0.0

        !    call sub_huelseetal2016_zO2_calcbc(zox, bctype, flxzox, conczox, flxswi, r_zxf)

        !    fun0 = flxzox + calcFO2(zox)
        !    print*,'!!!!!!!!!!!! fun0', fun0

        fun0 = FUN_zO2(zox)
        !    print*,' fun0', fun0

        !    print*,' '
        !    print*,'Try zero flux at zinf and see if we have any O2 left'
        ! Try zero flux at zinf and see if we have any O2 left
        bctype = 2
        !        print*,'Try zero flux at zinf: '
        call sub_huelseetal2016_zO2_calcbc(dum_D, zinf, bctype, flxzox, loc_conczinf, loc_new_swiflux_O2, r_zxf);
        !    print*,'flxzox', flxzox
        !    print*,'conczox at zinf ', conczox
        !    print*,'flxswi', flxswi

        if (fun0 .ge. 0.0)then   ! eg zero oxygen at swi
            zox = 0.0   ! DH 241016 was 1e-10
            bctype = 1
            loc_conczinf = 0.0
        elseif (loc_conczinf .ge. 0.0)then      ! still O2 at zinf -> zox = zinf
            !            print*,'in here mate: dum_i, dum_j ', dum_i, dum_j
            !            print*,'dum_POC1_conc_swi ', dum_POC1_conc_swi
            !            print*,'dum_POC2_conc_swi ', dum_POC2_conc_swi
            zox = zinf
            zno3 = zinf
            bctype = 2
        else                        ! search zox in the interval
            bctype = 1
            zL=1e-10
            tol=1e-16
            zox = FUN_zbrent(FUN_zO2, zL, zinf, tol)
            zno3 = zox
            loc_conczinf = 0.0
        !            stop
        end if

        call sub_huelseetal2016_zO2_calcbc(dum_D, zox, bctype, flxzox, conczox, loc_new_swiflux_O2, r_zxf)

        loc_new_swiflux_O2 = loc_new_swiflux_O2 - por*w*(dum_swiconc_O2-loc_conczinf)
    !        print*,' loc_new_swiflux_O2', loc_new_swiflux_O2
    !        print*,' dum_swiconc_O2', dum_swiconc_O2
    !        print*,' loc_conczinf', loc_conczinf
        !    print*,'---------- FINAL RESULTS zO2 --------- '
    !        print*,'zox ', char(9), zox
        !        print*,'r_zxf', char(9), r_zxf
        !        print*,''
    !        print*,'flxzox', char(9), flxzox
    !        print*,'conczox', char(9), conczox
    !        print*,'loc_new_swiflux_O2', char(9), loc_new_swiflux_O2


    END SUBROUTINE sub_huelseetal2016_zO2

    !------------------------------------------------------------------------------------
    !   *****************************************************************
    !   *****************************************************************
    !------------------------------------------------------------------------------------

    SUBROUTINE sub_huelseetal2016_zO2_calcbc(dum_D, zox, bctype, flxzox, conczox, flxswi,r_zxf)

        !   Solve O2
        real,INTENT(in)::dum_D                      ! ocean depth (m) +vs downwards
        real, intent(in)::zox
        !        real, intent(in)::loc_swiconc_O2
        integer, intent(in)::bctype
        real, intent(inout)::flxzox, conczox, flxswi, r_zxf
        !   local variables

        !    real qdispO2                          !O2 diffusion coefficient in water (cm2/yr)
        !    real adispO2                          !O2 linear coefficient for temperature dependence (cm2/yr/oC)
        !    real DO21                             !O2 diffusion coefficient in bioturbated layer (cm2/yr)
        !    real DO22                             !O2 diffusion coefficient in non-bioturbated layer (cm2/yr)
        real reac1, reac2 !ls , z0, zox
        integer ltype
        real ls_a, ls_b, ls_c, ls_d, ls_e, ls_f
        real e_0, dedz_0, f_0, dfdz_0, g_0, dgdz_0
        real e_zox, dedz_zox, f_zox, dfdz_zox, g_zox, dgdz_zox
        !    real bctype1_AO2, bctype1_BO2, bctype2_AO2, bctype2_BO2

        real rO2_AO2, rO2_BO2, Dzox, Dswi

        !    real FO2

        !    qdispO2=348.5750
        !    adispO2=14.0890

        !    DO21=(qdispO2+adispO2*T)*dispFactor+Dbio
        !    DO22=(qdispO2+adispO2*T)*dispFactor


        !   reactive terms: OM degradation (-) and nitrification (-)
        reac1=-OC-2*gamma*NC1
        reac2=-OC-2*gamma*NC2

        !    print*, ''
        !    print*, '------- START sub_huelseetal2016_zO2_calcbc ----- zox:', zox

        ! calculate solution for given zox

        !    print*, 'Preparation: sort out solution-matching across bioturbation boundary (if necessary)'
        ! Preparation: sort out solution-matching across bioturbation boundary (if necessary)
        call sub_prepfg_l12(reac1, reac2, 0.0, z0, zox, DO21, DO22, ls_a, ls_b, ls_c, ls_d, ls_e, ls_f, ltype)

        ! basis functions at upper boundary
        !        print*, 'z0, reac1, reac2 ', z0, reac1, reac2
        !        print*, 'ls_a, ls_b, ls_c, ls_d, ls_e, ls_f, DO21, DO22', ls_a, ls_b, ls_c, ls_d, ls_e, ls_f, DO21, DO22
        !        print*, 'ltype, DO21, DO22, ltype', ltype, DO21, DO22, ltype
        call sub_calcfg_l12(z0, reac1, reac2, 0.0, ls_a, ls_b, ls_c, ls_d, ls_e, ls_f, DO21, DO22, ltype, &
        e_0, dedz_0, f_0, dfdz_0, g_0, dgdz_0)

        ! ... and lower boundary
        call sub_calcfg_l12(zox, reac1, reac2, 0.0, ls_a, ls_b, ls_c, ls_d, ls_e, ls_f, DO21, DO22, ltype, e_zox, dedz_zox,&
        f_zox, dfdz_zox, g_zox, dgdz_zox)

        ! Solve for AO2, BO2 given boundary conditions (expressed in terms of transformed soln)

        IF(bctype==1)THEN
            ! Case 1 zero concentration at zox
            ! AO2*e_zox   +   BO2*f_zox  + g_zox = 0;
            ! AO2*e_0     +   BO2*f_0     + g_0  = swi.O20;

            ! | e_zox f_zox |  |AO2|   = | -g_zox         |
            ! | e_0   f_0   |  |BO2|     | swi.O20 - gt_0 |

            call sub_solve2eqn(e_zox, f_zox, e_0, f_0, -g_zox, dum_swiconc_O2 - g_0, rO2_AO2, rO2_BO2)
        ELSE
            ! Case  2 zero flux at zox
            ! AO2*dedz_zox +  BO2*dfz_zox + dgz_zox = 0;
            ! AO2*e_0     +   BO2*f_0     + g_0     = swi.O20;
                         ! a            b       c   d       e           f
            !            print*, 'dedz_zox, dfdz_zox, e_0, f_0, -dgdz_zox, dum_swiconc_O2 - g_0:', dedz_zox, dfdz_zox, e_0, f_0, -dgdz_zox, dum_swiconc_O2 - g_0
            !            print*, 'dum_swiconc_O2, g_0:', dum_swiconc_O2, g_0
            call sub_solve2eqn(dedz_zox, dfdz_zox, e_0, f_0, -dgdz_zox, dum_swiconc_O2 - g_0, rO2_AO2, rO2_BO2)
        END IF

        IF(zox < zbio)THEN
            Dzox = DO21
        ELSE
            Dzox = DO22
        END IF

        flxzox =  Dzox*(rO2_AO2*dedz_zox + rO2_BO2*dfdz_zox + dgdz_zox)         ! no por factor as this is per cm^2 pore area
        conczox = rO2_AO2*e_zox + rO2_BO2 * f_zox + g_zox
        !        print*, 'conczox:', conczox
        !        print*, 'rO2_AO2, e_zox, rO2_BO2, f_zox, g_zox:', rO2_AO2, e_zox, rO2_BO2, f_zox, g_zox

        if(0 < zbio)then
            Dswi = DO21
        else
            Dswi = DO22
        end if

        flxswi = por*(Dswi*(rO2_AO2*dedz_0+rO2_BO2*dfdz_0 + dgdz_0)) ! just diffusive flux - w*dum_swiconc_O2)   ! por fac so this is per cm^2 water column

        r_zxf = zox/(zoxgf + zox) ! + const_real_nullsmall)   ! roll off oxidation at low zox
                ! TODO: ASK STUART or ANDY
        if( &
        (flxzox /= flxzox) .OR. &
        (conczox /= conczox) .OR. &
        (flxswi /= flxswi)) then !check for NaN if then give value as in matlab.....
            print*,' '
            print*,' '
            print*,'------ zO2_calcbc --------- flxzox is INFFFFFFFFFFFFFFFF', dum_D
            print*,' zox tested', zox
            print*,'flxzox ', flxzox
            print*,'conczox ', conczox
            print*,'flxswi ', flxswi
            print*,'dum_swiconc_O2 ', dum_swiconc_O2
            !            print*,'Dzox, rO2_AO2, dedz_zox, rO2_BO2, dfdz_zox, dgdz_zox', Dzox, rO2_AO2, dedz_zox, rO2_BO2, dfdz_zox, dgdz_zox
            !            print*,'rO2_AO2, e_zox, rO2_BO2, f_zox, g_zox', rO2_AO2, e_zox, rO2_BO2, f_zox, g_zox
            print*,' '

            !            IF(bctype==1)THEN
            !            print*,' 1: solve2eqn(e_zox, f_zox, e_0, f_0, -g_zox, dum_swiconc_O2, -g_0', e_zox, f_zox, e_0, f_0, -g_zox, dum_swiconc_O2, -g_0
            !        ELSE
            !             print*,' 2: solve2eqn(dedz_zox, dfdz_zox, e_0, f_0, -dgdz_zox, dum_swiconc_O2, - g_0', dedz_zox, dfdz_zox, e_0, f_0, -dgdz_zox, dum_swiconc_O2, - g_0
            !        END IF
            !            flxzox = 0.0
            conczox = -1.0
            !            flxswi = 0.0
            flxzox = -1.0
        !            stop
        end if

    !        if(flxswi /= flxswi)then !check for NaN if then give value as in matlab.....
    !            print*,' '
    !            print*,' '
    !            print*,'------ zO2_calcbc --------- flxswi is INFFFFFFFFFFFFFFFF', dum_D
    !            print*,'flxzox ', flxzox
    !            print*,'conczox ', conczox
    !            print*,'flxswi ', flxswi
    !            print*,' '
    !            print*,' '
    !            flxswi = 0.0
    !            flxzox = -1.2e6
    !            conczox = -1.2e6
    !            stop
    !        end if

    !    FO2 = calcFO2()

    !    print*,'END zO2_calcbc: flxzox', flxzox
    !    print*,'conczox', conczox
    !    print*,'rO2_AO2, e_zox, rO2_BO2, f_zox, g_zox', rO2_AO2, e_zox, rO2_BO2, f_zox, g_zox

    end SUBROUTINE sub_huelseetal2016_zO2_calcbc

    ! ****************************************************************************************************************************** !
    !   *****************************************************************
    !   *****************************************************************
    !------------------------------------------------------------------------------------

    FUNCTION FUN_huelseetal2016_calcFO2(z)

        real FUN_huelseetal2016_calcFO2, z, tmpreac1, tmpreac2

        FUN_huelseetal2016_calcFO2 = 0.0
        ! Oxydation of reduced species at zox (NEED A RATIO for ODU! and add NH4
        ! adsporption!
        tmpreac1=gammaH2S*O2H2S*SO4C+2.0*gamma*NC1
        tmpreac2=gammaH2S*O2H2S*SO4C+2.0*gamma*NC2

        !    print*,' '
        !    print*,'..... START calcFO2'

        !tmpreac1=OC+2*gamma*NC1
        !tmpreac2=OC+2*gamma*NC2
        !FLUX of NH4 and Reduced species from ZOX to ZINF
        !        FUN_huelseetal2016_calcFO2 = 0.0    ! no secondary redox!
        FUN_huelseetal2016_calcFO2 = z/(zoxgf + z) * FUN_calcReac(z, zinf, tmpreac1, tmpreac2)   ! had in denominator: ... + const_real_nullsmall

    !    print*,'calcFO2', calcFO2

    END FUNCTION FUN_huelseetal2016_calcFO2

    ! ****************************************************************************************************************************** !
    !   *****************************************************************
    !   *****************************************************************
    !------------------------------------------------------------------------------------

    FUNCTION FUN_zO2(z)

        real FUN_zO2, z, flxzox, conczox, flxswi, r_zxf
        integer bctype
        flxzox = 0.0
        conczox = 0.0
        flxswi = 0.0
        !    print*,' '
        !    print*,'..... START FUN_zO2'
        bctype = 1
        call sub_huelseetal2016_zO2_calcbc(9999.9, z, bctype, flxzox, conczox, flxswi, r_zxf)

        FUN_zO2 = flxzox + FUN_huelseetal2016_calcFO2(z)

    !    print*,'FUN_zO2, flxzox, calcFO2(z)', FUN_zO2, flxzox, calcFO2(z)
    !    print*,' '
    END FUNCTION FUN_zO2

    !------------------------------------------------------------------------------------
    !   *****************************************************************
    !   *****************************************************************

    !                       Nitrate

    !   *****************************************************************
    !   *****************************************************************
    !------------------------------------------------------------------------------------

    SUBROUTINE sub_huelseetal2016_zNO3(dum_swiconc_NO3, loc_new_swiflux_NO3)

    
        ! dummy arguments
        real,INTENT(in)::dum_swiconc_NO3                ! NO3 concentrations at SWI
        real,INTENT(inout)::loc_new_swiflux_NO3         ! NO3 flux: TODO check! (+) sediment -> bottom waters

        real flxzinf, conczinf, flxzno3, conczno3, zL, tol
        integer bctype

        !    print*, ''
        !    print*, '------------------------------------------------------------------'
        !    print*, '---------------------- START zNO3 ------------------------------- '

        ! Try zero flux at zinf and see if we have any NO3 left - in
        ! the rare case of zox < zinf but zNO3 = zinf, also
        ! calculate [NO3] at zinf for advective loss
        bctype = 2
        call sub_huelseetal2016_zNO3_calcbc(zinf, bctype, flxzinf, conczinf, loc_new_swiflux_NO3)

        IF(zox == zinf)THEN
            zno3 = zinf
            bctype = 2
        ELSE
            !    print*,'RESULTS Try zero flux at zinf zNO3_calcbc flxzinf, conczinf, loc_new_swiflux_NO3', flxzinf, conczinf, loc_new_swiflux_NO3

            IF (conczinf > 0.0) THEN
                zno3 = zinf
                bctype = 2;
            ELSE
                zL=1e-10
                tol=1e-16
                zno3 = FUN_zbrent(FUN_zNO3, max(zL,zox), zinf, tol)
                !        print*,'$$$$ calculated zno3 =', zno3
                !        zno3 = 7.4319         ! use qualifier d0 fuer double: 7.4319d0
                bctype = 1;
                conczinf = 0.0
            END IF
        END IF ! (zox == zinf)

        call sub_huelseetal2016_zNO3_calcbc(zno3, bctype, flxzno3, conczno3, loc_new_swiflux_NO3)
        loc_new_swiflux_NO3 = loc_new_swiflux_NO3 - por*w*(dum_swiconc_NO3-conczinf)

    !        IF(ABS(loc_new_swiflux_NO3) .LE. const_real_nullsmall)then
    !            !            print*,'small loc_new_swiflux_NO3', loc_new_swiflux_NO3
    !            loc_new_swiflux_NO3 = 0.0
    !        END IF


    !    print*,' ---- FINAL RESULTS zNO3: ----'
    !    print*,'zno3', char(9), zno3
    !    print*,''
    !    print*,'flxzno3', char(9), flxzno3
    !    print*,'conczno3', char(9), conczno3
    !    print*,'loc_new_swiflux_NO3', char(9), loc_new_swiflux_NO3

    END SUBROUTINE sub_huelseetal2016_zNO3


    !------------------------------------------------------------------------------------
    !   *****************************************************************
    !   *****************************************************************
    !------------------------------------------------------------------------------------

    SUBROUTINE sub_huelseetal2016_zNO3_calcbc(zNO3, bctype, flxzno3, conczno3, loc_new_swiflux_NO3)

        real zNO3, flxzno3, conczNO3, loc_new_swiflux_NO3
        integer bctype, ltype1, ltype2
        real ls_a1, ls_b1, ls_c1, ls_d1, ls_e1, ls_f1
        real ls_a2, ls_b2, ls_c2, ls_d2, ls_e2, ls_f2
        real e2_zno3, dedz2_zno3, f2_zno3, dfdz2_zno3, g2_zno3, dgdz2_zno3
        real e1_zox, dedz1_zox, f1_zox, dfdz1_zox, g1_zox, dgdz1_zox
        real e2_zox, dedz2_zox, f2_zox, dfdz2_zox, g2_zox, dgdz2_zox
        real zox_a, zox_b, zox_c, zox_d, zox_e, zox_f
        real e1_00, f1_00, g1_00, dedz1_00, dfdz1_00, dgdz1_00
        real e1_0, dedz1_0, f1_0, dfdz1_0, g1_0, dgdz1_0
        real bctype1_A2, bctype1_B2, bctype2_A2, bctype2_B2
        real rNO3_A2, rNO3_B2, rNO3_A1, rNO3_B1

        real FNH4

        ! Calculate trial solution for given zno3, matching boundary conditions from layer-by-layer solutions


        ! Preparation: for each layer, sort out solution - matching across bioturbation boundary (if necessary)
        ! layer 1: 0 < z < zox, nitrification
        !    sub_prepfg_l12(reac1, reac2,  ktemp ,     zU , zL , D1,  D2)
        call sub_prepfg_l12(gamma*NC1, gamma*NC2,  0.0,  0.0, zox, DN1, DN2, ls_a1, ls_b1, ls_c1, ls_d1, ls_e1, ls_f1, ltype1)
        !    print*, ''
        !    print*, '1. sub_prepfg_l12 RESULTS: ls_a1, ls_b1, ls_c1, ls_d1, ls_e1, ls_f1, ltype1', ls_a1, ls_b1, ls_c1, &
        !           & ls_d1, ls_e1, ls_f1, ltype1
        ! layer 2: zox < z < zno3, denitrification
        call sub_prepfg_l12(-NO3CR, -NO3CR, 0.0,  zox, zNO3, DN1, DN2, ls_a2, ls_b2, ls_c2, ls_d2, ls_e2, ls_f2, ltype2)
        !    print*, ''
        !    print*, '2. sub_prepfg_l12 RESULTS: ls_a2, ls_b2, ls_c2, ls_d2, ls_e2, ls_f2, ltype2', ls_a2, ls_b2, ls_c2, ls_d2, &
        !            & ls_e2, ls_f2, ltype2


        ! Work up from the bottom, matching solutions at boundaries
        ! Basis functions at bottom of layer 2 zno3
        call sub_calcfg_l12(zno3, -NO3CR, -NO3CR, 0.0, ls_a2, ls_b2, ls_c2, ls_d2, ls_e2, ls_f2, DN1, DN2, ltype2, &
        e2_zno3, dedz2_zno3, f2_zno3, dfdz2_zno3, g2_zno3, dgdz2_zno3)

        ! Match at zox, layer 1 - layer 2 (continuity, flux discontinuity from NH4 -> NO3 source)

        ! basis functions at bottom of layer 1
        call sub_calcfg_l12(zox, gamma*NC1, gamma*NC2, 0.0, ls_a1, ls_b1, ls_c1, ls_d1, ls_e1, ls_f1, DN1, DN2, ltype1, &
        e1_zox, dedz1_zox, f1_zox, dfdz1_zox, g1_zox, dgdz1_zox)

        ! basis functions at top of layer 2

        call sub_calcfg_l12(zox, -NO3CR, -NO3CR, 0.0, ls_a2, ls_b2, ls_c2, ls_d2, ls_e2, ls_f2, DN1, DN2, ltype2, &
        e2_zox, dedz2_zox, f2_zox, dfdz2_zox, g2_zox, dgdz2_zox)

        ! flux of NH4 to zox  TODO NH4 production by denitrification?


        FNH4 = FUN_calcReac(zno3, zinf, NC1/(1.0+KNH4), NC2/(1.0+KNH4))   ! MULTIPLY BY 1/POR ????
        !    print*, 'FNH4 = ', FNH4

        ! match solutions at zox - continuous concentration, flux discontinuity from H2S ox

        !    call sub_matchsoln(e_zbio_l1, f_zbio_l1, g_zbio_l1, D1*dedz_zbio_l1, D1*dfdz_zbio_l1, D1*dgdz_zbio_l1, &
        !                         & e_zbio_l2, f_zbio_l2, g_zbio_l2, D2*dedz_zbio_l2, D2*dfdz_zbio_l2, D2*dgdz_zbio_l2, &
        !                                0.0, 0.0, ls_a, ls_b, ls_c, ls_d, ls_e, ls_f)
        IF(zox .le. zbio)then
            call sub_matchsoln(e1_zox, f1_zox, g1_zox, dedz1_zox, dfdz1_zox, dgdz1_zox, &
            e2_zox, f2_zox, g2_zox, dedz2_zox, dfdz2_zox, dgdz2_zox, &
            0.0, -r_zxf*gamma*FNH4/DN1, zox_a, zox_b, zox_c, zox_d, zox_e, zox_f)
        !        print*, ''
        !        print*,'zox<= zbio: RESULTS:  zox_a, zox_b, zox_c, zox_d, zox_e, zox_f', zox_a, zox_b, zox_c, zox_d, zox_e, zox_f
        ELSE
            call sub_matchsoln(e1_zox, f1_zox, g1_zox, dedz1_zox, dfdz1_zox, dgdz1_zox, &
            e2_zox, f2_zox, g2_zox, dedz2_zox, dfdz2_zox, dgdz2_zox, &
            0.0, -r_zxf*gamma*FNH4/DN2, zox_a, zox_b, zox_c, zox_d, zox_e, zox_f)
        !        print*, ''
        !        print*,'zox> zbio: RESULTS:  zox_a, zox_b, zox_c, zox_d, zox_e, zox_f', zox_a, zox_b, zox_c, zox_d, zox_e, zox_f
        END IF

        ! Solution at swi, top of layer 1
        call sub_calcfg_l12(0.0, gamma*NC1, gamma*NC2, 0.0, ls_a1, ls_b1, ls_c1, ls_d1, ls_e1, ls_f1, DN1, DN2, ltype1, &
        e1_00, dedz1_00, f1_00, dfdz1_00, g1_00, dgdz1_00)
        !    print*, ''
        !    print*,'top of layer 1; sub_calcfg_l12: RESULTS:  e1_00, dedz1_00, f1_00, dfdz1_00, g1_00, dgdz1_00', e1_00, &
        !            & dedz1_00, f1_00, dfdz1_00, g1_00, dgdz1_00

        ! transform to use coeffs from l2
        call sub_xformsoln(e1_00, f1_00, g1_00, dedz1_00, dfdz1_00, dgdz1_00, zox_a , zox_b , zox_c , zox_d , zox_e ,zox_f, &
        e1_0, f1_0, g1_0, dedz1_0,  dfdz1_0, dgdz1_0)
        !    print*, ''
        !    print*,'transform to use coeffs from l2; sub_xformsoln: RESULTS:  e1_0, f1_0, g1_0, dedz1_0,  dfdz1_0, dgdz1_0', e1_0, &
        !            & f1_0, g1_0, dedz1_0,  dfdz1_0, dgdz1_0

        ! Solve for ANO3, BNO3 given boundary conditions (expressed in terms of transformed basis fns, layer 2 A, B)

        ! Case 1 zero concentration at zno3
        ! ANO3*e2_zno3   +  BNO3*f2_zno3  + g2_zno3 = 0;
        ! ANO3*e1_0     +   BNO3*f1_0     + g1_0  = swi.dum_swiconc_NO3;

        ! | e2_zno3 f2_zno3 |  |ANO3|   = | -g2_zno3       |
        ! | e1_0     f1_0   |  |BNO3|     | swi.dum_swiconc_NO3 - g1_0 |

        call sub_solve2eqn(e2_zno3, f2_zno3, e1_0, f1_0, -g2_zno3, dum_swiconc_NO3 - g1_0, bctype1_A2, bctype1_B2)

        ! Case  2 zero flux at zno3
        ! ANO3*de2dz_zno3   +  BNO3*dfdz2_zno3  + dgdz2_zno3 = 0;
        ! ANO3*e1_0         +   BNO3*f1_0       + g1_0       = swi.dum_swiconc_NO3;

        call sub_solve2eqn(dedz2_zno3, dfdz2_zno3, e1_0, f1_0, -dgdz2_zno3, dum_swiconc_NO3 - g1_0, bctype2_A2, bctype2_B2)

        ! Choose type of solution requested
        IF(bctype==1) THEN
            rNO3_A2 = bctype1_A2
            rNO3_B2 = bctype1_B2
        ELSE
            rNO3_A2 = bctype2_A2
            rNO3_B2 = bctype2_B2
        END IF

        ! calculate flux at zno3
        IF(zno3 .le. zbio) THEN
            flxzno3 = DN1*(rNO3_A2*dedz2_zno3+rNO3_B2*dfdz2_zno3 + dgdz2_zno3)      ! includes 1/por ie flux per (cm^2 pore area)
        ELSE
            flxzno3 = DN2*(rNO3_A2*dedz2_zno3+rNO3_B2*dfdz2_zno3 + dgdz2_zno3)      ! includes 1/por ie flux per (cm^2 pore area)
        END IF

        conczno3 = rNO3_A2*e2_zno3+rNO3_B2*f2_zno3 + g2_zno3
        ! flux at swi - DO include por so this is per cm^2 water column area
        loc_new_swiflux_NO3 = por*(DN1*(rNO3_A2*dedz1_0+rNO3_B2*dfdz1_0 + dgdz1_0)) ! - w*dum_swiconc_NO3)              ! NB: use A2, B2 as these are _xformed_ layer 1 basis functions

        !    print*,'flxzno3', flxzno3
        !    print*,'conczno3', conczno3
        !    print*,'loc_new_swiflux_NO3', loc_new_swiflux_NO3

        ! save coeffs for layer 1
        rNO3_A1 = zox_a*rNO3_A2 + zox_b*rNO3_B2 + zox_e
        rNO3_B1 = zox_c*rNO3_A2 + zox_d*rNO3_B2 + zox_f

    !    print*,'rNO3_A1, rNO3_B1', rNO3_A1, rNO3_B1

    END SUBROUTINE sub_huelseetal2016_zNO3_calcbc

    ! ****************************************************************************************************************************** !
    !   *****************************************************************
    !   *****************************************************************
    !------------------------------------------------------------------------------------

    FUNCTION FUN_zNO3(z)

        real FUN_zNO3, z, flxzno3, conczno3, flxswi

        !    print*,' '
        !    print*,'..... START FUN_zNO3'

        call sub_huelseetal2016_zNO3_calcbc(z, 1, flxzno3, conczno3, flxswi)

        FUN_zNO3 = -flxzno3

    !    print*,'FUN_zNO3', FUN_zNO3
    !    print*,' '
    END FUNCTION FUN_zNO3


    !------------------------------------------------------------------------------------
    !   *****************************************************************
    !   *****************************************************************

    !                           Sulfate

    !   *****************************************************************
    !   *****************************************************************
    !------------------------------------------------------------------------------------

    SUBROUTINE sub_huelseetal2016_zSO4(dum_swiconc_SO4, loc_new_swiflux_SO4)
    
        ! dummy arguments
        real,INTENT(in)::dum_swiconc_SO4                ! SO4 concentrations at SWI
        real,INTENT(inout)::loc_new_swiflux_SO4         ! SO4 flux: TODO check! (+) sediment -> bottom waters


        ! local variables
        real flxzso4, conczso4, loc_conczinf, zL, tol
        integer bctype


        !        print*, '------------------------------------------------------------------'
        !        print*, '---------------------- START zSO4 ------------------------------- '
        !        print*, ' BWI SO4 concentration = ', dum_swiconc_SO4

        ! Iteratively solve for zso4

        ! try zero flux at zinf and see if we have any SO4 left, also
        ! calculate [SO4] at zinf for advective loss
        bctype = 2
        call sub_huelseetal2016_zSO4_calcbc(zinf, bctype, flxzso4, loc_conczinf, loc_new_swiflux_SO4)


        IF(zno3 == zinf)THEN
            zso4 = zinf
            bctype = 2
        ELSE

            !        print*,'conczso4 at zinf', char(9), conczso4

            IF(loc_conczinf .ge. 0)THEN
                zso4 = zinf
                bctype = 2
            ELSE
                bctype = 1
                loc_conczinf = 0.0
                zL=1e-10
                tol=1e-16
                zso4 = FUN_zbrent(FUN_zSO4, max(zno3,zL), zinf, tol)
!                print*,'CALCULATE zso4 = ', zso4
            END IF
        !    print*,'bctype, zso4 ', bctype, zso4
        END IF !(zno3 == zinf)
        call sub_huelseetal2016_zSO4_calcbc(zso4, bctype, flxzso4, conczso4, loc_new_swiflux_SO4)
        loc_new_swiflux_SO4 = loc_new_swiflux_SO4 - por*w*(dum_swiconc_SO4 - loc_conczinf)

        IF(ABS(loc_new_swiflux_SO4) .LE. const_real_nullsmall)then
            !            print*,'small loc_new_swiflux_SO4', loc_new_swiflux_SO4
            loc_new_swiflux_SO4 = 0.0
        END IF


        !    print*,' '
        !    print*,'-------------------------------- FINAL RESULTS zSO4 --------------------------------'
    !        print*,'zso4', char(9), zso4
    !        print*,' '
    !        print*,'flxzso4', char(9), flxzso4
    !        print*,'conczso4', char(9), conczso4
    !        print*,'loc_new_swiflux_SO4', char(9), loc_new_swiflux_SO4

    END SUBROUTINE sub_huelseetal2016_zSO4

    !------------------------------------------------------------------------------------
    !   *****************************************************************
    !   *****************************************************************
    !------------------------------------------------------------------------------------

    SUBROUTINE sub_huelseetal2016_zSO4_calcbc(zso4, bctype, flxzso4, conczso4, flxswi)

        real, intent(in)::zso4
        integer, intent(in)::bctype
        real, intent(inout)::flxzso4, conczso4, flxswi

        ! local variable
        integer ltype1, ltype2, ltype3
        real reac1_so4, reac2_so4
        real ls_a1, ls_b1, ls_c1, ls_d1, ls_e1, ls_f1
        real ls_a2, ls_b2, ls_c2, ls_d2, ls_e2, ls_f2
        real ls_a3, ls_b3, ls_c3, ls_d3, ls_e3, ls_f3
        real e3_zso4, dedz3_zso4, f3_zso4, dfdz3_zso4, g3_zso4, dgdz3_zso4
        real e2_zno3, dedz2_zno3, f2_zno3, dfdz2_zno3, g2_zno3, dgdz2_zno3
        real e3_zno3, dedz3_zno3, f3_zno3, dfdz3_zno3, g3_zno3, dgdz3_zno3
        real zno3_a, zno3_b, zno3_c, zno3_d, zno3_e, zno3_f
        real e1_zox, dedz1_zox, f1_zox, dfdz1_zox, g1_zox, dgdz1_zox
        real e2_zox0, f2_zox0, g2_zox0, dedz2_zox0, dfdz2_zox0, dgdz2_zox0
        real e2_zox, dedz2_zox, f2_zox, dfdz2_zox, g2_zox, dgdz2_zox
        real zox_a, zox_b, zox_c, zox_d, zox_e, zox_f
        real e1_0, dedz1_0, f1_0, dfdz1_0, g1_0, dgdz1_0
        real e1_00, f1_00, g1_00, dedz1_00, dfdz1_00, dgdz1_00
        real bctype1_A3, bctype1_B3, bctype2_A3, bctype2_B3
        real rSO4_A1, rSO4_B1, rSO4_A2, rSO4_B2, rSO4_A3, rSO4_B3

        real FH2S

        !    print*, ' '
        !    print*, '---------------------- START zSO4_calcbc ------------------------------- '

        reac1_so4=-SO4C
        reac2_so4=-SO4C

        ! Calculate trial solution for given zso4, matching boundary conditions from layer-by-layer solutions


        ! Preparation: for each layer, sort out solution-matching across bioturbation boundary if necessary
        ! layer 1: 0 < z < zox, passive diffn
        !      ls =      sub_prepfg_l12( bsd, swi, r, reac1,     reac2,     ktemp, zU, zL, D1,        D2)

        call sub_prepfg_l12(0.0, 0.0, 0.0, 0.0, zox, DSO41, DSO42, ls_a1, ls_b1, ls_c1, ls_d1, ls_e1, ls_f1, ltype1)

        ! layer 2: zox < z < zno3, passive diffn
        call sub_prepfg_l12(0.0, 0.0, 0.0, zox, zno3, DSO41, DSO42, ls_a2, ls_b2, ls_c2, ls_d2, ls_e2, ls_f2, ltype2)

        ! layer 3: zno3 < z < zso4, SO4 consumption by OM oxidation
                !rSO4.ls3 = r.zTOC.sub_prepfg_l12(bsd, swi, r, reac1, reac2, 0, r.zno3, zso4, DSO41, DSO42)
        call sub_prepfg_l12(reac1_so4, reac2_so4, 0.0, zno3, zso4, DSO41, DSO42, ls_a3, ls_b3, ls_c3, ls_d3, ls_e3, ls_f3, ltype3)


        ! Work up from the bottom, matching solutions at boundaries
        ! Basis functions at bottom of layer 3 zso4

        call sub_calcfg_l12(zso4, reac1_so4, reac2_so4, 0.0, ls_a3, ls_b3, ls_c3, ls_d3, ls_e3, ls_f3, DSO41, DSO42, ltype3, &
        e3_zso4, dedz3_zso4, f3_zso4, dfdz3_zso4, g3_zso4, dgdz3_zso4)

        ! Match at zno3, layer 2 - layer 3 (continuity and flux)
        ! basis functions at bottom of layer 2
        call sub_calcfg_l12(zno3, 0.0, 0.0, 0.0, ls_a2, ls_b2, ls_c2, ls_d2, ls_e2, ls_f2, DSO41, DSO42, ltype2, &
        e2_zno3, dedz2_zno3, f2_zno3, dfdz2_zno3, g2_zno3, dgdz2_zno3)

        ! ... and top of layer 3
        call sub_calcfg_l12(zno3, reac1_so4, reac2_so4, 0.0, ls_a3, ls_b3, ls_c3, ls_d3, ls_e3, ls_f3, DSO41, DSO42, ltype3, &
        e3_zno3, dedz3_zno3, f3_zno3, dfdz3_zno3, g3_zno3, dgdz3_zno3)

        ! match solutions at zno3 - continuous concentration and flux
        !    [zno3.a, zno3.b, zno3.c, zno3.d, zno3.e, zno3.f] = benthic_utils.sub_matchsoln(e2_zno3, f2_zno3, g2_zno3, dedz2_zno3, dfdz2_zno3, dgdz2_zno3, ...
        !                                                                e3_zno3, f3_zno3, g3_zno3, dedz3_zno3, dfdz3_zno3, dgdz3_zno3, ...
        !                                                                0, 0);
        call sub_matchsoln(e2_zno3, f2_zno3, g2_zno3, dedz2_zno3, dfdz2_zno3, dgdz2_zno3, &
        e3_zno3, f3_zno3, g3_zno3, dedz3_zno3, dfdz3_zno3, dgdz3_zno3, &
        0.0, 0.0, zno3_a, zno3_b, zno3_c, zno3_d, zno3_e, zno3_f)


        ! Match at zox, layer 1 - layer 2 (continuity, flux discontinuity from H2S source)
        ! flux of H2S to oxic interface (Source of SO4)
        ! NB: include methane region as AOM will produce sulphide as well..

        !        FH2S = 0.0  !FUN_calcReac(zno3, zso4, SO4C, SO4C) + 0.0  ! no secondary redox!
        FH2S = FUN_calcReac(zno3, zso4, SO4C, SO4C) & ! MULTIPLY BY 1/POR ????
        + gammaCH4*FUN_calcReac(zso4, zinf, MC, MC)

        !    print*,' '
        !    print*,'FH2S ', FH2S

        ! basis functions at bottom of layer 1
        call sub_calcfg_l12(zox, 0.0, 0.0, 0.0, ls_a1, ls_b1, ls_c1, ls_d1, ls_e1, ls_f1, DSO41, DSO42, ltype1, &
        e1_zox, dedz1_zox, f1_zox, dfdz1_zox, g1_zox, dgdz1_zox)

        ! basis functions at top of layer 2
        call sub_calcfg_l12(zox, 0.0, 0.0, 0.0, ls_a2, ls_b2, ls_c2, ls_d2, ls_e2, ls_f2, DSO41, DSO42, ltype2, &
        e2_zox0, dedz2_zox0, f2_zox0, dfdz2_zox0, g2_zox0, dgdz2_zox0)

        ! transform to use coeffs from l3
        call sub_xformsoln(e2_zox0, f2_zox0, g2_zox0, dedz2_zox0, dfdz2_zox0, dgdz2_zox0, &
        zno3_a , zno3_b , zno3_c , zno3_d , zno3_e ,zno3_f, &
        e2_zox, f2_zox, g2_zox, dedz2_zox, dfdz2_zox, dgdz2_zox )

        ! match solutions at zox - continuous concentration, flux discontinuity from H2S ox
        IF(zox .le. zbio)THEN
            call sub_matchsoln(e1_zox, f1_zox, g1_zox, dedz1_zox, dfdz1_zox, dgdz1_zox, &
            e2_zox, f2_zox, g2_zox, dedz2_zox, dfdz2_zox, dgdz2_zox, &
            0.0, -r_zxf*gammaH2S*FH2S/DSO41, zox_a, zox_b, zox_c, zox_d, zox_e, zox_f)
        ELSE
            call sub_matchsoln(e1_zox, f1_zox, g1_zox, dedz1_zox, dfdz1_zox, dgdz1_zox, &
            e2_zox, f2_zox, g2_zox, dedz2_zox, dfdz2_zox, dgdz2_zox, &
            0.0, -r_zxf*gammaH2S*FH2S/DSO42, zox_a, zox_b, zox_c, zox_d, zox_e, zox_f)
        END IF

        ! Solution at swi, top of layer 1
        call sub_calcfg_l12(0.0, 0.0, 0.0, 0.0, ls_a1, ls_b1, ls_c1, ls_d1, ls_e1, ls_f1, DSO41, DSO42, ltype1, &
        e1_00, dedz1_00, f1_00, dfdz1_00, g1_00, dgdz1_00)

        ! transform to use coeffs from l3
        call sub_xformsoln(e1_00, f1_00, g1_00, dedz1_00, dfdz1_00, dgdz1_00, zox_a , zox_b , zox_c , zox_d , zox_e ,zox_f, &
        e1_0, f1_0, g1_0, dedz1_0,  dfdz1_0, dgdz1_0)

        ! Find solutions for two possible types of lower bc
        !  case 1  zero concentration at zso4
        ! Solve for ASO4, BSO4 given boundary conditions (expressed in terms of transformed basis fns, layer 3 A, B)
        ! ASO4*e3_zso4   +  BSO4*f3_zso4  + g3_zso4 = 0;
        ! ASO4*e1_0     +   BSO4*f1_0     + g1_0  = swi.dum_swiconc_SO4

        ! | e3_zso4 f3_zso4 |  |ASO4|   = | -g3_zso4       |
        ! | e1_0     f1_0   |  |BSO4|     | swi.dum_swiconc_SO4 - g1_0 |

        call sub_solve2eqn(e3_zso4, f3_zso4, e1_0, f1_0, -g3_zso4, dum_swiconc_SO4 - g1_0, bctype1_A3, bctype1_B3)

        ! case  2 zero flux at zso4
        ! ASO4*de3dz_zso4   +  BSO4*dfdz3_zso4  + dgdz3_zso4 = 0;
        ! ASO4*e1_0         +   BSO4*f1_0       + g1_0       = swi.dum_swiconc_SO4;
        call sub_solve2eqn(dedz3_zso4, dfdz3_zso4, e1_0, f1_0, -dgdz3_zso4, dum_swiconc_SO4 - g1_0, bctype2_A3, bctype2_B3)

        ! Choose type of solution requested
        !rSO4.A3 = (bctype==1).*bctype1_A3 + (bctype==2).*bctype2_A3;
        !        rSO4.B3 = (bctype==1).*bctype1_B3 + (bctype==2).*bctype2_B3;
        IF(bctype==1)THEN
            rSO4_A3=bctype1_A3
            rSO4_B3=bctype1_B3
        ELSE
            rSO4_A3=bctype2_A3
            rSO4_B3=bctype2_B3
        END IF

        ! calculate conc and flux at zso4
        conczso4 = rSO4_A3*e3_zso4+rSO4_B3*f3_zso4 + g3_zso4
        !D = (zso4 <= zbio).*DSO41 + (zso4 > zbio).*DSO42
        IF(zso4 .le. zbio)THEN
            flxzso4 = DSO41*(rSO4_A3*dedz3_zso4+rSO4_B3*dfdz3_zso4 + dgdz3_zso4)        ! includes 1/por ie flux per (cm^2 pore area)
        ELSE
            flxzso4 = DSO42*(rSO4_A3*dedz3_zso4+rSO4_B3*dfdz3_zso4 + dgdz3_zso4)
        END IF

        ! flux at swi - DO include por so this is per cm^2 water column area
        flxswi = por*(DSO41*(rSO4_A3*dedz1_0+rSO4_B3*dfdz1_0 + dgdz1_0)) ! - w*dum_swiconc_SO4)   ! NB: use A3, B3 as these are _xformed_ layer 1 basis functions

        !    print*,' '
        !    print*,'RESULTS zso4_calcbc_: conczso4, flxzso4, flxswi', conczso4, flxzso4, flxswi

        ! save coeffs for layers 2 and 1
        rSO4_A2 = zno3_a*rSO4_A3 + zno3_b*rSO4_B3 + zno3_e
        rSO4_B2 = zno3_c*rSO4_A3 + zno3_d*rSO4_B3 + zno3_f

        rSO4_A1 = zox_a*rSO4_A3 + zox_b*rSO4_B3 + zox_e
        rSO4_B1 = zox_c*rSO4_A3 + zox_d*rSO4_B3 + zox_f

    !    print*,'rSO4_A3, rSO4_B3, rSO4_A2, rSO4_B2, rSO4_A1, rSO4_B1', rSO4_A3, rSO4_B3, rSO4_A2, rSO4_B2, rSO4_A1, rSO4_B1
    !    print*,' '

    END SUBROUTINE sub_huelseetal2016_zSO4_calcbc

    ! ****************************************************************************************************************************** !
    !   *****************************************************************
    !   *****************************************************************
    !------------------------------------------------------------------------------------

    FUNCTION FUN_calcFSO4(z)

        real FUN_calcFSO4, z, tmpreac1, tmpreac2

        ! Calculate SO4 consumption below zso4, by organic matter and indirectly via methane oxidation

        !        print*,' '
        !        print*,'..... START FUN_calcFSO4'

        tmpreac1    = MC*gammaCH4
        tmpreac2    = MC*gammaCH4
        !        FUN_calcFSO4 = 0.0 ! no secondary redox!
        FUN_calcFSO4 = FUN_calcReac(z, zinf, tmpreac1, tmpreac2)
    ! TODO confirm (1-por)*  has been added (to k1 & k2 ?)
    !    print*,'=============== IN FUN_calcFSO4 =====', FUN_calcFSO4

    END FUNCTION FUN_calcFSO4

    ! ****************************************************************************************************************************** !
    !   *****************************************************************
    !   *****************************************************************
    !------------------------------------------------------------------------------------

    FUNCTION FUN_zSO4(z)

        real FUN_zSO4, z, flxzso4, conczso4, flxswi

        !    print*,' '
        !    print*,'..... START FUN_zSO4'

        call sub_huelseetal2016_zSO4_calcbc(z, 1, flxzso4, conczso4, flxswi)

        FUN_zSO4 = -flxzso4 - FUN_calcFSO4(z)

    !    print*,'FUN_zSO4, flxzso4, FUN_calcFSO4(z)', FUN_zSO4, flxzso4, FUN_calcFSO4(z)
    !    print*,' '
    END FUNCTION FUN_zSO4

    !------------------------------------------------------------------------------------
    !   *****************************************************************
    !   *****************************************************************

    !                       Ammonium

    !   *****************************************************************
    !   *****************************************************************
    !------------------------------------------------------------------------------------

    SUBROUTINE sub_huelseetal2016_zNH4(dum_swiconc_NH4, loc_new_swiflux_NH4)

        ! dummy arguments
        real,INTENT(in)::dum_swiconc_NH4                ! NH4 concentrations at SWI
        real,INTENT(inout)::loc_new_swiflux_NH4         ! NH4 flux: TODO check! (+) sediment -> bottom waters



        !    real, intent(in)::zox, zno3

        ! local variables
        real loc_conczinf
        integer ltype1, ltype2, ltype3
        real ls_a1, ls_b1, ls_c1, ls_d1, ls_e1, ls_f1
        real ls_a2, ls_b2, ls_c2, ls_d2, ls_e2, ls_f2
        real ls_a3, ls_b3, ls_c3, ls_d3, ls_e3, ls_f3
        real e3_zinf, dedz3_zinf, f3_zinf, dfdz3_zinf, g3_zinf, dgdz3_zinf
        real e2_zno3, dedz2_zno3, f2_zno3, dfdz2_zno3, g2_zno3, dgdz2_zno3
        real e3_zno3, dedz3_zno3, f3_zno3, dfdz3_zno3, g3_zno3, dgdz3_zno3
        real zno3_a, zno3_b, zno3_c, zno3_d, zno3_e, zno3_f
        real e1_zox, dedz1_zox, f1_zox, dfdz1_zox, g1_zox, dgdz1_zox
        real e2_zox0, dedz2_zox0, f2_zox0, dfdz2_zox0, g2_zox0, dgdz2_zox0
        real e2_zox, dedz2_zox, f2_zox, dfdz2_zox, g2_zox, dgdz2_zox
        real zox_a, zox_b, zox_c, zox_d, zox_e, zox_f
        real e1_00, dedz1_00, f1_00, dfdz1_00, g1_00, dgdz1_00
        real e1_0, dedz1_0, f1_0, dfdz1_0, g1_0, dgdz1_0
        real rNH4_A3, rNH4_B3
        real rNH4_A2, rNH4_B2
        real rNH4_A1, rNH4_B1

        real FNH4

        !    print*, ''
        !    print*, '------------------------------------------------------------------'
        !    print*, '---------------------- START zNH4 ------------------------------- '

        ! Preparation: for each layer, sort out solution-matching across bioturbation boundary if necessary
        ! layer 1: 0 < z < zox, NH4 prod (remaining after oxidation)
        !      ls =      sub_prepfg_l12( bsd, swi, r, reac1,     reac2,     ktemp, zU, zL, D1,        D2)
        call sub_prepfg_l12((1-gamma)*NC1/(1.0+KNH4),(1-gamma)*NC2/(1.0+KNH4),0.0, 0.0, zox, DNH41, DNH42, ls_a1, ls_b1, ls_c1, ls_d1, ls_e1, ls_f1, ltype1)

        ! layer 2: zox < z < zno3, passive diffn TODO NH4 from denitrification?
        call sub_prepfg_l12(0.0, 0.0, 0.0, zox, zno3, DNH41, DNH42, ls_a2, ls_b2, ls_c2, ls_d2, ls_e2, ls_f2, ltype2)

        ! layer 3: zno3 < z < zinf, NH4 production
        call sub_prepfg_l12(NC1/(1.0+KNH4), NC2/(1.0+KNH4), 0.0, zno3, zinf, DNH41, DNH42, ls_a3, ls_b3, ls_c3, ls_d3, ls_e3, ls_f3, ltype3)

        ! Work up from the bottom, matching solutions at boundaries
        ! Basis functions at bottom of layer 3 zinf
        call sub_calcfg_l12(zinf, NC1/(1.0+KNH4), NC2/(1.0+KNH4), 0.0, ls_a3, ls_b3, ls_c3, ls_d3, ls_e3, ls_f3, DNH41, DNH42, ltype3, &
        e3_zinf, dedz3_zinf, f3_zinf, dfdz3_zinf, g3_zinf, dgdz3_zinf)

        ! Match at zno3, layer 2 - layer 3 (continuity and flux)
        ! basis functions at bottom of layer 2
        call sub_calcfg_l12(zno3, 0.0, 0.0, 0.0, ls_a2, ls_b2, ls_c2, ls_d2, ls_e2, ls_f2, DNH41, DNH42, ltype2, &
        e2_zno3, dedz2_zno3, f2_zno3, dfdz2_zno3, g2_zno3, dgdz2_zno3)

        ! ... and top of layer 3
        call sub_calcfg_l12(zno3, NC1/(1.0+KNH4), NC2/(1.0+KNH4), 0.0, ls_a3, ls_b3, ls_c3, ls_d3, ls_e3, ls_f3, DNH41, DNH42, ltype3, &
        e3_zno3, dedz3_zno3, f3_zno3, dfdz3_zno3, g3_zno3, dgdz3_zno3)

        ! match solutions at zno3 - continuous concentration and flux
        call sub_matchsoln(e2_zno3, f2_zno3, g2_zno3, dedz2_zno3, dfdz2_zno3, dgdz2_zno3, &
        e3_zno3, f3_zno3, g3_zno3, dedz3_zno3, dfdz3_zno3, dgdz3_zno3, &
        0.0, 0.0, zno3_a, zno3_b, zno3_c, zno3_d, zno3_e, zno3_f)

        ! Match at zox, layer 1 - layer 2 (continuity, flux discontinuity from NH4 sink)
        ! flux of NH4 to oxic interface  TODO NH4 prod by denitrification?
        FNH4 = FUN_calcReac(zno3, zinf, NC1/(1.0+KNH4), NC2/(1.0+KNH4))   ! MULTIPLY BY 1/POR ????

        ! basis functions at bottom of layer 1
        call sub_calcfg_l12(zox, (1-gamma)*NC1/(1.0+KNH4),(1-gamma)*NC2/(1.0+KNH4) , 0.0, ls_a1, ls_b1, ls_c1, ls_d1, ls_e1, ls_f1, DNH41, DNH42, ltype1, &
        e1_zox, dedz1_zox, f1_zox, dfdz1_zox, g1_zox, dgdz1_zox)
        ! basis functions at top of layer 2
        call sub_calcfg_l12(zox, 0.0, 0.0, 0.0, ls_a2, ls_b2, ls_c2, ls_d2, ls_e2, ls_f2, DNH41, DNH42, ltype2, &
        e2_zox0, dedz2_zox0, f2_zox0, dfdz2_zox0, g2_zox0, dgdz2_zox0)

        ! transform to use coeffs from l3
        call sub_xformsoln(e2_zox0, f2_zox0, g2_zox0, dedz2_zox0, dfdz2_zox0, dgdz2_zox0, &
        zno3_a , zno3_b , zno3_c , zno3_d , zno3_e ,zno3_f, &
        e2_zox, f2_zox, g2_zox, dedz2_zox, dfdz2_zox, dgdz2_zox)

        ! match solutions at zox - continuous concentration, flux discontinuity from NH4 ox
        IF(zox .le. zbio)THEN
            call sub_matchsoln(e1_zox, f1_zox, g1_zox, dedz1_zox, dfdz1_zox, dgdz1_zox, &
            e2_zox, f2_zox, g2_zox, dedz2_zox, dfdz2_zox, dgdz2_zox, &
            0.0, r_zxf*gamma*FNH4/DNH41, zox_a, zox_b, zox_c, zox_d, zox_e, zox_f)
        ELSE
            call sub_matchsoln(e1_zox, f1_zox, g1_zox, dedz1_zox, dfdz1_zox, dgdz1_zox, &
            e2_zox, f2_zox, g2_zox, dedz2_zox, dfdz2_zox, dgdz2_zox, &
            0.0, r_zxf*gamma*FNH4/DNH42, zox_a, zox_b, zox_c, zox_d, zox_e, zox_f)

        END IF

        ! Solution at swi, top of layer 1
        call sub_calcfg_l12(0.0, (1-gamma)*NC1/(1.0+KNH4), (1-gamma)*NC2/(1.0+KNH4), 0.0, ls_a1, ls_b1, ls_c1, ls_d1, ls_e1, ls_f1, DNH41, DNH42, ltype1, &
        e1_00, dedz1_00, f1_00, dfdz1_00, g1_00, dgdz1_00)

        ! transform to use coeffs from l3
        call sub_xformsoln(e1_00, f1_00, g1_00, dedz1_00, dfdz1_00, dgdz1_00, &
        zox_a , zox_b , zox_c , zox_d , zox_e ,zox_f, &
        e1_0, f1_0, g1_0, dedz1_0,  dfdz1_0, dgdz1_0)

        ! Solve for ANH4, BNH4 given boundary conditions (expressed in terms of transformed basis fns, layer 3 A, B)
        ! ANH4*dedz3_zinf   +  BNH4*dfdz3_zinf  + dgdz3_zinf = 0;
        ! ANH4*e1_0     +   BNH4*f1_0     + g1_0  = swi.;

        ! | dedz3_zinf dfdz3_zinf |  |ANH4|   = | -dgdz3_zinf       |
        ! | e1_0     f1_0         |  |BNH4|     | swi. - g1_0 |

        call sub_solve2eqn(dedz3_zinf, dfdz3_zinf, e1_0, f1_0, -dgdz3_zinf,  - g1_0, rNH4_A3, rNH4_B3)

        !calculate concentration at zinf
        loc_conczinf = rNH4_A3*e3_zinf+rNH4_B3*f3_zinf + g3_zinf


        ! flux at swi - DO include por so this is per cm^2 water column area
        loc_new_swiflux_NH4 = por*(DNH41*(rNH4_A3*dedz1_0+rNH4_B3*dfdz1_0 + dgdz1_0) - w*(dum_swiconc_NH4 - loc_conczinf))   ! NB: use A3, B3 as these are _xformed_ layer 1 basis functions
        IF(ABS(loc_new_swiflux_NH4) .LE. const_real_nullsmall)then
            !            print*,'small loc_new_swiflux_NH4', loc_new_swiflux_NH4
            loc_new_swiflux_NH4 = 0.0
        END IF

        ! save coeffs for layers 2 and 1
        rNH4_A2 = zno3_a*rNH4_A3 + zno3_b*rNH4_B3 + zno3_e
        rNH4_B2 = zno3_c*rNH4_A3 + zno3_d*rNH4_B3 + zno3_f

        rNH4_A1 = zox_a*rNH4_A3 + zox_b*rNH4_B3 + zox_e
        rNH4_B1 = zox_c*rNH4_A3 + zox_d*rNH4_B3 + zox_f


    !    print*,'INPUT1: por, DNH41, rNH4_A3, dedz1_0, rNH4_B3, dfdz1_0 , dgdz1_0', &
    !            & por, DNH41, rNH4_A3, dedz1_0, rNH4_B3, dfdz1_0 , dgdz1_0
    !    print*,'INPUT2: zox_a , zox_b , zox_c , zox_d , zox_e ,zox_f', &
    !            & zox_a , zox_b , zox_c , zox_d , zox_e ,zox_f
    !    print*,' '
    !    print*,' ---------------- RESULTS: benthic_zNH4 ---------------- '
    !    print*,'loc_new_swiflux_NH4', char(9), loc_new_swiflux_NH4
    !    print*,' '
    !    print*,'rNH4_A3, rNH4_B3, rNH4_A2, rNH4_B2, rNH4_A1, rNH4_B1', &
    !          &  rNH4_A3, rNH4_B3, rNH4_A2, rNH4_B2, rNH4_A1, rNH4_B1


    END SUBROUTINE sub_huelseetal2016_zNH4
    
    !------------------------------------------------------------------------------------
    !   *****************************************************************
    !   *****************************************************************

    !                           Hydrogen Sulfide

    !   *****************************************************************
    !   *****************************************************************
    !------------------------------------------------------------------------------------

    SUBROUTINE sub_huelseetal2016_zH2S(dum_swiconc_H2S, loc_new_swiflux_H2S)

        ! dummy arguments
        real,INTENT(in)::dum_swiconc_H2S                ! SO4 concentrations at SWI
        real,INTENT(inout)::loc_new_swiflux_H2S         ! SO4 flux: TODO check! (+) sediment -> bottom waters


        ! local variables
        real loc_conczinf
        real reac1_h2s, reac2_h2s                 ! reactive terms: OM degradation
        integer ltype1, ltype2, ltype3, ltype4
        real ls_a1, ls_b1, ls_c1, ls_d1, ls_e1, ls_f1
        real ls_a2, ls_b2, ls_c2, ls_d2, ls_e2, ls_f2
        real ls_a3, ls_b3, ls_c3, ls_d3, ls_e3, ls_f3
        real ls_a4, ls_b4, ls_c4, ls_d4, ls_e4, ls_f4
        real e4_zinf, dedz4_zinf, f4_zinf, dfdz4_zinf, g4_zinf, dgdz4_zinf
        real e3_zso4, dedz3_zso4, f3_zso4, dfdz3_zso4, g3_zso4, dgdz3_zso4
        real e4_zso4, dedz4_zso4, f4_zso4, dfdz4_zso4, g4_zso4, dgdz4_zso4
        real zso4_a, zso4_b, zso4_c, zso4_d, zso4_e, zso4_f
        real e2_zno3, dedz2_zno3, f2_zno3, dfdz2_zno3, g2_zno3, dgdz2_zno3
        real e3_zno30, dedz3_zno30, f3_zno30, dfdz3_zno30, g3_zno30, dgdz3_zno30
        real e3_zno3, dedz3_zno3, f3_zno3, dfdz3_zno3, g3_zno3, dgdz3_zno3
        real zno3_a, zno3_b, zno3_c, zno3_d, zno3_e, zno3_f
        real e1_zox, dedz1_zox, f1_zox, dfdz1_zox, g1_zox, dgdz1_zox
        real e2_zox0, dedz2_zox0, f2_zox0, dfdz2_zox0, g2_zox0, dgdz2_zox0
        real e2_zox, dedz2_zox, f2_zox, dfdz2_zox, g2_zox, dgdz2_zox
        real zox_a, zox_b, zox_c, zox_d, zox_e, zox_f
        real e1_00, dedz1_00, f1_00, dfdz1_00, g1_00, dgdz1_00
        real e1_0, dedz1_0, f1_0, dfdz1_0, g1_0, dgdz1_0

        real rH2S_A4, rH2S_B4
        real rH2S_A3, rH2S_B3
        real rH2S_A2, rH2S_B2
        real rH2S_A1, rH2S_B1

        real zso4FH2S, zoxFH2S
        !    real flxswiH2S

        reac1_h2s=SO4C
        reac2_h2s=SO4C

        !    print*, ''
        !    print*, '------------------------------------------------------------------'
        !    print*, '---------------------- START zH2S ------------------------------- '
        !        print*, ' BWI H2S concentration = ', dum_swiconc_H2S

        ! Calculate H2S

        ! Preparation: for each layer, sort out solution-matching across bioturbation boundary if necessary
        ! layer 1: 0 < z < zox, passive diffn
        !  ls =      sub_prepfg_l12( bsd, swi, r, reac1,     reac2,     ktemp, zU, zL, D1,        D2)
        call sub_prepfg_l12(0.0, 0.0, 0.0, 0.0, zox, DH2S1, DH2S2, ls_a1, ls_b1, ls_c1, ls_d1, ls_e1, ls_f1, ltype1)

        ! layer 2: zox < z < zno3, passive diffn
        call sub_prepfg_l12(0.0, 0.0, 0.0, zox, zno3, DH2S1, DH2S2, ls_a2, ls_b2, ls_c2, ls_d2, ls_e2, ls_f2, ltype2)

        ! layer 3: zno3 < z < zso4, H2S consumption by OM oxidation
        call sub_prepfg_l12(reac1_h2s, reac2_h2s, 0.0, zno3, zso4, DH2S1, DH2S2, ls_a3, ls_b3, ls_c3, ls_d3, ls_e3, ls_f3, ltype3)

        ! layer 4: zso4 < z < zinf, passive diffn
        call sub_prepfg_l12(0.0, 0.0, 0.0, zso4, zinf, DH2S1, DH2S2, ls_a4, ls_b4, ls_c4, ls_d4, ls_e4, ls_f4, ltype4)

        ! Work up from the bottom, matching solutions at boundaries
        ! Basis functions at bottom of layer 4 zinf
        call sub_calcfg_l12(zinf, 0.0, 0.0, 0.0, ls_a4, ls_b4, ls_c4, ls_d4, ls_e4, ls_f4, DH2S1, DH2S2, ltype4, &
        e4_zinf, dedz4_zinf, f4_zinf, dfdz4_zinf, g4_zinf, dgdz4_zinf)

        ! Match at zso4, layer 3 - layer 4 (continuity and flux with AOM production)
        ! basis functions at bottom of layer 3
        call sub_calcfg_l12(zso4, reac1_h2s, reac2_h2s, 0.0, ls_a3, ls_b3, ls_c3, ls_d3, ls_e3, ls_f3, DH2S1, DH2S2, ltype3, &
        e3_zso4, dedz3_zso4, f3_zso4, dfdz3_zso4, g3_zso4, dgdz3_zso4)

        !    print*, 'e3_zso4, dedz3_zso4, f3_zso4, dfdz3_zso4, g3_zso4, dgdz3_zso4 ', e3_zso4, dedz3_zso4, f3_zso4, dfdz3_zso4, g3_zso4, dgdz3_zso4

        ! ... and top of layer 4
        call sub_calcfg_l12(zso4, 0.0,  0.0, 0.0, ls_a4, ls_b4, ls_c4, ls_d4, ls_e4, ls_f4, DH2S1, DH2S2, ltype4, &
        e4_zso4, dedz4_zso4, f4_zso4, dfdz4_zso4, g4_zso4, dgdz4_zso4)
        !    print*, 'e4_zso4, dedz4_zso4, f4_zso4, dfdz4_zso4, g4_zso4, dgdz4_zso4 ', e4_zso4, dedz4_zso4, f4_zso4, dfdz4_zso4, g4_zso4, dgdz4_zso4

        ! flux of H2S produced by AOM interface (Source of H2S)
        !        zso4FH2S = 0.0 ! no secondary redox!
        zso4FH2S = FUN_calcReac(zso4, zinf, MC, MC) ! MULTIPLY BY 1/POR ????
        !   print*,'flux of H2S produced by AOM interface zso4FH2S = ', zso4FH2S

        ! match solutions at zso4 - continuous concentration and flux
        call sub_matchsoln(e3_zso4, f3_zso4, g3_zso4, dedz3_zso4, dfdz3_zso4, dgdz3_zso4, &
        e4_zso4, f4_zso4, g4_zso4, dedz4_zso4, dfdz4_zso4, dgdz4_zso4, &
        0.0, -gammaCH4*zso4FH2S/DH2S2, zso4_a, zso4_b, zso4_c, zso4_d, zso4_e, zso4_f)
        !    print*, 'zso4_a, zso4_b, zso4_c, zso4_d, zso4_e, zso4_f ', zso4_a, zso4_b, zso4_c, zso4_d, zso4_e, zso4_f

        ! Match at zno3, layer 2 - layer 3 (continuity and flux)
        ! basis functions at bottom of layer 2
        call sub_calcfg_l12(zno3, 0.0, 0.0, 0.0, ls_a2, ls_b2, ls_c2, ls_d2, ls_e2, ls_f2, DH2S1, DH2S2, ltype2, &
        e2_zno3, dedz2_zno3, f2_zno3, dfdz2_zno3, g2_zno3, dgdz2_zno3)
        !        print*, 'e2_zno3, dedz2_zno3, f2_zno3, dfdz2_zno3, g2_zno3, dgdz2_zno3 ', e2_zno3, dedz2_zno3, f2_zno3, dfdz2_zno3, g2_zno3, dgdz2_zno3

        ! ... and top of layer 3
        call sub_calcfg_l12(zno3, reac1_h2s, reac2_h2s, 0.0, ls_a3, ls_b3, ls_c3, ls_d3, ls_e3, ls_f3, DH2S1, DH2S2, ltype3, &
        e3_zno30, dedz3_zno30, f3_zno30, dfdz3_zno30, g3_zno30, dgdz3_zno30)
        !        print*, 'e3_zno30, dedz3_zno30, f3_zno30, dfdz3_zno30, g3_zno30, dgdz3_zno30 ', e3_zno30, dedz3_zno30, f3_zno30, dfdz3_zno30, g3_zno30, dgdz3_zno30

        ! ... transformed to use coeffs from l4
        call sub_xformsoln(e3_zno30, f3_zno30, g3_zno30, dedz3_zno30, dfdz3_zno30, dgdz3_zno30, &
        zso4_a , zso4_b , zso4_c , zso4_d , zso4_e ,zso4_f, &
        e3_zno3, f3_zno3, g3_zno3, dedz3_zno3, dfdz3_zno3, dgdz3_zno3)
        !    print*, 'e3_zno3, f3_zno3, g3_zno3, dedz3_zno3, dfdz3_zno3, dgdz3_zno3 ', e3_zno3, f3_zno3, g3_zno3, dedz3_zno3, dfdz3_zno3, dgdz3_zno3


        ! match solutions at zno3 - continuous concentration and flux
        call sub_matchsoln(e2_zno3, f2_zno3, g2_zno3, dedz2_zno3, dfdz2_zno3, dgdz2_zno3, &
        e3_zno3, f3_zno3, g3_zno3, dedz3_zno3, dfdz3_zno3, dgdz3_zno3, &
        0.0, 0.0, zno3_a, zno3_b, zno3_c, zno3_d, zno3_e, zno3_f)
        !    print*, 'zno3_a, zno3_b, zno3_c, zno3_d, zno3_e, zno3_f ', zno3_a, zno3_b, zno3_c, zno3_d, zno3_e, zno3_f


        ! Match at zox, layer 1 - layer 2 (continuity, flux discontinuity from H2S source)
        ! flux of H2S to oxic interface (from all sources of H2S below)
        ! NB: include methane region as AOM will produce sulphide as well..
        !        zoxFH2S = 0.0   !FUN_calcReac(zno3, zso4, SO4C, SO4C)  + 0.0   ! no secondary redox!
        zoxFH2S = FUN_calcReac(zno3, zso4, SO4C, SO4C)  + FUN_calcReac(zso4, zinf, MC, MC)
        !    print*,' '
        !    print*,'flux of H2S to oxic interface zoxFH2S = ', zoxFH2S
        !    print*,' '

        ! basis functions at bottom of layer 1
        call sub_calcfg_l12(zox, 0.0, 0.0, 0.0, ls_a1, ls_b1, ls_c1, ls_d1, ls_e1, ls_f1, DH2S1, DH2S2, ltype1, &
        e1_zox, dedz1_zox, f1_zox, dfdz1_zox, g1_zox, dgdz1_zox)

        ! basis functions at top of layer 2
        call sub_calcfg_l12(zox, 0.0, 0.0, 0.0, ls_a2, ls_b2, ls_c2, ls_d2, ls_e2, ls_f2, DH2S1, DH2S2, ltype2, &
        e2_zox0, dedz2_zox0, f2_zox0, dfdz2_zox0, g2_zox0, dgdz2_zox0)

        !   transform to use coeffs from l4
        call sub_xformsoln(e2_zox0, f2_zox0, g2_zox0, dedz2_zox0, dfdz2_zox0, dgdz2_zox0, &
        zno3_a , zno3_b , zno3_c , zno3_d , zno3_e ,zno3_f, &
        e2_zox, f2_zox, g2_zox, dedz2_zox, dfdz2_zox, dgdz2_zox)

        ! match solutions at zox - continuous concentration, flux discontinuity from H2S ox

        IF(zox .le. zbio) THEN
            call sub_matchsoln(e1_zox, f1_zox, g1_zox, dedz1_zox, dfdz1_zox, dgdz1_zox, &
            e2_zox, f2_zox, g2_zox, dedz2_zox, dfdz2_zox, dgdz2_zox, &
            0.0, r_zxf*gammaH2S*zoxFH2S/DH2S1, zox_a, zox_b, zox_c, zox_d, zox_e, zox_f)
        ELSE
            call sub_matchsoln(e1_zox, f1_zox, g1_zox, dedz1_zox, dfdz1_zox, dgdz1_zox, &
            e2_zox, f2_zox, g2_zox, dedz2_zox, dfdz2_zox, dgdz2_zox, &
            0.0, r_zxf*gammaH2S*zoxFH2S/DH2S2, zox_a, zox_b, zox_c, zox_d, zox_e, zox_f)
        END IF

        ! Solution at swi, top of layer 1
        call sub_calcfg_l12(0.0, 0.0, 0.0, 0.0, ls_a1, ls_b1, ls_c1, ls_d1, ls_e1, ls_f1, DH2S1, DH2S2, ltype1, &
        e1_00, dedz1_00, f1_00, dfdz1_00, g1_00, dgdz1_00)

        ! transform to use coeffs from l4
        call sub_xformsoln(e1_00, f1_00, g1_00, dedz1_00, dfdz1_00, dgdz1_00, &
        zox_a , zox_b , zox_c , zox_d , zox_e ,zox_f, &
        e1_0, f1_0, g1_0, dedz1_0,  dfdz1_0, dgdz1_0)

        ! Solve for AH2S, BH2S given boundary conditions (expressed in terms of transformed basis fns, layer 4 A, B)
        !  AH2S*dedz4_zinf   +  BH2S*dfz4_zinf  + dgz4_zinf = 0;          % zero flux at zinf
        !  AH2S*e1_0     +   BH2S*f1_0     + g1_0  = swi.dum_swiconc_H2S;

        !  | dedz4_zinf dfdz4_zinf |  |AH2S|   = | -dgz4_zinf       |
        !  | e1_0     f1_0         |  |BH2S|     | swi.dum_swiconc_H2S - g1_0 |

        call sub_solve2eqn(dedz4_zinf, dfdz4_zinf, e1_0, f1_0, -dgdz4_zinf, dum_swiconc_H2S - g1_0, rH2S_A4, rH2S_B4)
        !   print*,' dedz4_zinf, dfdz4_zinf, e1_0, f1_0, dgdz4_zinf, dum_swiconc_H2S, g1_0 ',  dedz4_zinf, dfdz4_zinf, e1_0, f1_0, dgdz4_zinf, dum_swiconc_H2S, g1_0

        ! calculate concentration at zinf
        loc_conczinf = rH2S_A4*e4_zinf+rH2S_B4*f4_zinf + g4_zinf

        ! flux at swi - DO include por so this is per cm^2 water column area
        loc_new_swiflux_H2S = por*(DH2S1*(rH2S_A4*dedz1_0+rH2S_B4*dfdz1_0 + dgdz1_0) - w*(dum_swiconc_H2S - loc_conczinf))   ! NB: use A4, B4 as these are _xformed_ layer 1 basis functions
        IF(ABS(loc_new_swiflux_H2S) .LE. const_real_nullsmall)then
            !            print*,'small loc_new_swiflux_H2S', loc_new_swiflux_H2S
            loc_new_swiflux_H2S = 0.0
        END IF


        ! save coeffs for layers 3, 2 and 1
        rH2S_A3 = zso4_a*rH2S_A4 + zso4_b*rH2S_B4 + zso4_e
        rH2S_B3 = zso4_c*rH2S_A4 + zso4_d*rH2S_B4 + zso4_f

        rH2S_A2 = zno3_a*rH2S_A4 + zno3_b*rH2S_B4 + zno3_e
        rH2S_B2 = zno3_c*rH2S_A4 + zno3_d*rH2S_B4 + zno3_f

        rH2S_A1 = zox_a*rH2S_A4 + zox_b*rH2S_B4 + zox_e
        rH2S_B1 = zox_c*rH2S_A4 + zox_d*rH2S_B4 + zox_f

    !    print*,' ---------------- RESULTS: benthic_zH2S ---------------- '
    !    print*,'loc_new_swiflux_H2S ', char(9), loc_new_swiflux_H2S
    !    print*,'rH2S_A4, rH2S_B4, rH2S_A3, rH2S_B3, rH2S_A2, rH2S_B2, rH2S_A1, rH2S_B1', &
    !          &  rH2S_A4, rH2S_B4, rH2S_A3, rH2S_B3, rH2S_A2, rH2S_B2, rH2S_A1, rH2S_B1


    !   print*,'INPUT1: dedz4_zinf, dfdz4_zinf, e1_0, f1_0, -dgdz4_zinf, dum_swiconc_H2S - g1_0', &
    !           & dedz4_zinf, dfdz4_zinf, e1_0, f1_0, -dgdz4_zinf, dum_swiconc_H2S - g1_0
    !    print*,'INPUT2: zox_a , zox_b , zox_c , zox_d , zox_e ,zox_f', &
    !            & zox_a , zox_b , zox_c , zox_d , zox_e ,zox_f
    !    print*,'RESULTS:  rH2S_A4, rH2S_B4', &
    !            & rH2S_A4, rH2S_B4

    END SUBROUTINE sub_huelseetal2016_zH2S


    !------------------------------------------------------------------------------------
    !   *****************************************************************
    !   *****************************************************************

    !                           Phosphate and Fe-bound P

    !   *****************************************************************
    !   *****************************************************************
    !------------------------------------------------------------------------------------

    SUBROUTINE sub_huelseetal2016_zPO4_M(dum_swiconc_PO4, loc_new_swiflux_PO4, dum_swiflux_M, loc_new_swiflux_M)

        ! dummy arguments
        real,INTENT(in)::dum_swiconc_PO4                ! PO4 concentrations at SWI
        real,INTENT(inout)::loc_new_swiflux_PO4         ! PO4 flux: TODO check! (+) sediment -> bottom waters
        real,INTENT(in)::dum_swiflux_M                ! PO4 concentrations at SWI
        real,INTENT(inout)::loc_new_swiflux_M         ! PO4 flux: TODO check! (+) sediment -> bottom waters

        ! local variables
        real loc_conczinf
        ! Integration constants
        real rPO4_M_A2, rPO4_M_B2, rPO4_M_C2, rPO4_M_D2

        integer dum_ltype1, dum_ltype2
        real, dimension (4, 4) :: dum_mat_C1, dum_mat_C2
        real, dimension (1:4) ::  dum_vec_D1, dum_vec_D2
        integer loc_i, loc_j

        real reac1_po4_ox, reac2_po4_ox, reac1_po4_anox, reac2_po4_anox                 ! reactive terms: OM degradation

        ! all the base functions for boundary matching
        ! i.e. ODE solutions (E, F, P, Q) and the particulat integral (G) and their derivatives
        ! at zinf
        real e2_zinf_P, dedz2_zinf_P, f2_zinf_P, dfdz2_zinf_P, g2_zinf_P, dgdz2_zinf_P
        real p2_zinf_P, dpdz2_zinf_P, q2_zinf_P, dqdz2_zinf_P
        real e2_zinf_M, dedz2_zinf_M, f2_zinf_M, dfdz2_zinf_M, g2_zinf_M, dgdz2_zinf_M
        real p2_zinf_M, dpdz2_zinf_M, q2_zinf_M, dqdz2_zinf_M
        ! at zox (above)
        real e1_zox_P, dedz1_zox_P, f1_zox_P, dfdz1_zox_P, g1_zox_P, dgdz1_zox_P
        real p1_zox_P, dpdz1_zox_P, q1_zox_P, dqdz1_zox_P
        real e1_zox_M, dedz1_zox_M, f1_zox_M, dfdz1_zox_M, g1_zox_M, dgdz1_zox_M
        real p1_zox_M, dpdz1_zox_M, q1_zox_M, dqdz1_zox_M
        ! at zox (below)
        real e2_zox_P, dedz2_zox_P, f2_zox_P, dfdz2_zox_P, g2_zox_P, dgdz2_zox_P
        real p2_zox_P, dpdz2_zox_P, q2_zox_P, dqdz2_zox_P
        real e2_zox_M, dedz2_zox_M, f2_zox_M, dfdz2_zox_M, g2_zox_M, dgdz2_zox_M
        real p2_zox_M, dpdz2_zox_M, q2_zox_M, dqdz2_zox_M
        ! at SWI (z0)
        real e1_z0_P, dedz1_z0_P, f1_z0_P, dfdz1_z0_P, g1_z0_P, dgdz1_z0_P
        real p1_z0_P, dpdz1_z0_P, q1_z0_P, dqdz1_z0_P
        real e1_z0_M, dedz1_z0_M, f1_z0_M, dfdz1_z0_M, g1_z0_M, dgdz1_z0_M
        real p1_z0_M, dpdz1_z0_M, q1_z0_M, dqdz1_z0_M
        ! the final g's (other are saved in matrices, e.g. loc_EFPQ_P_t, ...)
        real g_P, dgdz_P
        real g_M, dgdz_M


        real loc_Vb, loc_Fb                                   ! discontinuity constants
        real, dimension (1:4,1:4) :: loc_mat_X_4x4, loc_mat_Y_4x4
        real, dimension (1:4) ::  loc_vec_Z_4
        real, dimension (1:3,1:3) :: loc_mat_X_3x3, loc_mat_Y_3x3
        real, dimension (1:3) ::  loc_vec_Z_3

        ! Matrix/Vector calculated by sub_matchsoln_PO4_M
        real, dimension (4, 4) :: loc_mat_C_4x4
        real, dimension (1:4) ::  loc_vec_D_4
        real, dimension (3, 3) :: loc_mat_C_3x3
        real, dimension (1:3) ::  loc_vec_D_3
        integer loc_dim

        ! save the ODE solutions in vectors to make calculation easier (DH?: however, is this faster?)
        real, dimension (1:4) :: loc_EFPQ_P, loc_dEFPQdz_P, loc_EFPQ_M, loc_dEFPQdz_M
        ! the transformed ODE solutions coming from sub_xformsoln_PO4_M
        real, dimension (1:4) :: loc_EFPQ_P_t, loc_dEFPQdz_P_t, loc_EFPQ_M_t, loc_dEFPQdz_M_t

        ! calculated integration constants for Layer 1 & 2 - in case we want to calculate profiles later - calculated first as vector to save code
        real, dimension (1:4) :: loc_Layer1_IC, loc_Layer2_IC

        reac1_po4_ox = 1/(1+KPO4_ox)*PC1
        reac2_po4_ox = 1/(1+KPO4_ox)*PC2
        reac1_po4_anox = 1/(1+KPO4_anox)*PC1
        reac2_po4_anox = 1/(1+KPO4_anox)*PC2

        loc_Vb = 0.0
        loc_Fb = 0.0

        ! Initialize loc_mat_C_4x4 & loc_vec_D_4 with zeros as, so I don't need to add them later manually ...
        loc_mat_C_4x4 = 0.0
        loc_vec_D_4 = 0.0

        !    print*, ' '
        !    print*, '------------------------------------------------------------------'
        !    print*, '---------------------- START zPO4_M ------------------------------- '

        !   Preparation: for each layer, sort out solution-matching across bioturbation boundary if necessary

        !   layer 1: 0 < z < zox, OM degradation (-) Sorption to sediment Fe-oxides (ktemp)

        call sub_prepfg_l12_PO4_M(reac1_po4_ox, reac2_po4_ox, ksPO4/(1+KPO4_ox), PO4s*ksPO4/(1+KPO4_ox), 0.0, zox, &
        DPO41/(1+KPO4_ox), DPO42/(1+KPO4_ox), 0.0, 0.0, 0.0, Dbio, 0.0, (1/SD)*ksPO4, &
        dum_mat_C1, dum_vec_D1, dum_ltype1)

        !   layer 2: zox < z < zinf,
        !   OM degradation (-) authigenic P formation (ktemp) (+) P desorption due to Fe-bound P release upon Fe oxide reduction
        !       rPO4_M.ls2 = r.zTOC.sub_prepfg_l12_PO4_M(bsd, swi, r, reac1_anox, reac2_anox, kaPO4/(1+KPO4_anox), PO4a*kaPO4/(1+KPO4_anox), r.zox, zinf, DPO41/(1+KPO4_anox), DPO42/(1+KPO4_anox), SD*kmPO4/(1+KPO4_anox), ...
        !                                           kmPO4, kmPO4.*Minf, Dbio, 0, 0);
        call sub_prepfg_l12_PO4_M(reac1_po4_anox, reac2_po4_anox, kaPO4/(1+KPO4_anox), PO4a*kaPO4/(1+KPO4_anox), zox, zinf, &
        DPO41/(1+KPO4_anox), DPO42/(1+KPO4_anox), SD*kmPO4/(1+KPO4_anox), kmPO4, kmPO4*Minf, Dbio, &
        0.0, 0.0, dum_mat_C2, dum_vec_D2, dum_ltype2)

            ! Work up from the bottom, matching solutions at boundaries
            ! Basis functions at bottom of layer 2 zinf
        call sub_calcfg_l12_PO4_M(zinf, reac1_po4_anox, reac2_po4_anox, kaPO4/(1+KPO4_anox), PO4a*kaPO4/(1+KPO4_anox), &
        DPO41/(1+KPO4_anox), DPO42/(1+KPO4_anox), SD*kmPO4/(1+KPO4_anox), dum_mat_C2, dum_vec_D2, &
        dum_ltype2, kmPO4, kmPO4*Minf, Dbio, 0.0, 0.0, e2_zinf_P, dedz2_zinf_P, f2_zinf_P, &
        dfdz2_zinf_P, g2_zinf_P, dgdz2_zinf_P, p2_zinf_P, dpdz2_zinf_P, q2_zinf_P, dqdz2_zinf_P, &
        e2_zinf_M, dedz2_zinf_M, f2_zinf_M, dfdz2_zinf_M, g2_zinf_M, dgdz2_zinf_M, &
        p2_zinf_M, dpdz2_zinf_M, q2_zinf_M, dqdz2_zinf_M)

        ! Match at zox, layer 1 - layer 2 (continuity and flux)
        ! basis functions at bottom of layer 1
        call sub_calcfg_l12_PO4_M(zox, reac1_po4_ox, reac2_po4_ox, ksPO4/(1+KPO4_ox), PO4s*ksPO4/(1+KPO4_ox), &
        DPO41/(1+KPO4_ox), DPO42/(1+KPO4_ox), 0.0, dum_mat_C1, dum_vec_D1, &
        dum_ltype1, 0.0, 0.0, Dbio, 0.0, (1/SD)*ksPO4, e1_zox_P, dedz1_zox_P, f1_zox_P, &
        dfdz1_zox_P, g1_zox_P, dgdz1_zox_P, p1_zox_P, dpdz1_zox_P, q1_zox_P, dqdz1_zox_P, &
        e1_zox_M, dedz1_zox_M, f1_zox_M, dfdz1_zox_M, g1_zox_M, dgdz1_zox_M, &
        p1_zox_M, dpdz1_zox_M, q1_zox_M, dqdz1_zox_M)

        !  and top of layer 2
        call sub_calcfg_l12_PO4_M(zox, reac1_po4_anox, reac2_po4_anox, kaPO4/(1+KPO4_anox), PO4a*kaPO4/(1+KPO4_anox), &
        DPO41/(1+KPO4_anox), DPO42/(1+KPO4_anox), SD*kmPO4/(1+KPO4_anox), dum_mat_C2, dum_vec_D2, &
        dum_ltype2, kmPO4, kmPO4*Minf, Dbio, 0.0, 0.0, e2_zox_P, dedz2_zox_P, f2_zox_P, &
        dfdz2_zox_P, g2_zox_P, dgdz2_zox_P, p2_zox_P, dpdz2_zox_P, q2_zox_P, dqdz2_zox_P, &
        e2_zox_M, dedz2_zox_M, f2_zox_M, dfdz2_zox_M, g2_zox_M, dgdz2_zox_M, &
        p2_zox_M, dpdz2_zox_M, q2_zox_M, dqdz2_zox_M)

        ! match solutions at zox - continuous concentration and flux
        ! organize the data in matrices and let the intrinsic fortran function do the calculation
        ! DH: Maybe this could be done more efficiently !?
        !  |x1        |   | A_l |      | y1        | | A_r|    |z1|    always PO4 continuity
        !  |    .     |   | B_l |      |    .      | | B_r|    |z2|    always PO4 flux
        !  |      .   |   | C_l |   =  |      .    | | C_r|  + |z3|    always M continuity
        !  |       x16|   | D_l |      |        y16| | D_r|    |z4|    SD M flux only in bioturbated case, otherwise not an independent constraint

        if(zox < zbio)then  ! 1. CASE: 4 int const. in each layer
            ! weird FORTRAN matrices makes the transpose necessary
            loc_mat_X_4x4 = transpose(reshape((/ e1_zox_P, f1_zox_P, p1_zox_P, q1_zox_P, &
            dedz1_zox_P, dfdz1_zox_P, dpdz1_zox_P, dqdz1_zox_P, &
            e1_zox_M, f1_zox_M, p1_zox_M, q1_zox_M, &
            dedz1_zox_M, dfdz1_zox_M, dpdz1_zox_M, dqdz1_zox_M /), shape(loc_mat_X_4x4)))

            loc_mat_Y_4x4 = transpose(reshape((/ e2_zox_P, f2_zox_P, p2_zox_P, q2_zox_P, &
            dedz2_zox_P, dfdz2_zox_P, dpdz2_zox_P, dqdz2_zox_P, &
            e2_zox_M, f2_zox_M, p2_zox_M, q2_zox_M, &
            dedz2_zox_M, dfdz2_zox_M, dpdz2_zox_M, dqdz2_zox_M /), shape(loc_mat_Y_4x4)))

            loc_vec_Z_4 = (/ g2_zox_P-g1_zox_P + loc_Vb, &
            dgdz2_zox_P - dgdz1_zox_P + loc_Fb - w*loc_Vb, &
            g2_zox_M-g1_zox_M + loc_Vb, &
            dgdz2_zox_M - dgdz1_zox_M + loc_Fb - w*loc_Vb /)

            loc_dim = 4
            call sub_matchsoln_PO4_M(loc_mat_X_4x4, loc_mat_Y_4x4, loc_vec_Z_4, loc_dim, loc_mat_C_4x4, loc_vec_D_4)


        else    ! 2. CASE: 3 int const. in each layer
                ! DH: zox non-bioturbated
                ! DH: this should generate 3x3 matrices as no M flux boundary condition (and then 4x4 C with zeros)

            loc_mat_X_3x3 = transpose(reshape((/ e1_zox_P, f1_zox_P, p1_zox_P, &
            dedz1_zox_P, dfdz1_zox_P, dpdz1_zox_P, &
            e1_zox_M, f1_zox_M, p1_zox_M /), shape(loc_mat_X_3x3)))

            loc_mat_Y_3x3 = transpose(reshape((/ e2_zox_P, f2_zox_P, p2_zox_P, &
            dedz2_zox_P, dfdz2_zox_P, dpdz2_zox_P, &
            e2_zox_M, f2_zox_M, p2_zox_M /), shape(loc_mat_Y_3x3)))

            loc_vec_Z_3 = (/ g2_zox_P-g1_zox_P + loc_Vb, &
            dgdz2_zox_P - dgdz1_zox_P + loc_Fb - w*loc_Vb, &
            g2_zox_M-g1_zox_M + loc_Vb /)
            loc_dim = 3

            call sub_matchsoln_PO4_M(loc_mat_X_3x3, loc_mat_Y_3x3, loc_vec_Z_3, loc_dim, loc_mat_C_3x3, loc_vec_D_3)

            ! integrate the 3x3 matrix in the 4x4 matrix
            do loc_i = 1,3
                do loc_j = 1,3
                    loc_mat_C_4x4(loc_i, loc_j) = loc_mat_C_3x3(loc_i, loc_j)
                end do
                loc_vec_D_4(loc_i)  = loc_vec_D_3(loc_i)
            end do

        end if !(zox < zbio)

        !201     format (6f12.6)
        !        print*,'final loc_mat_C_4x4 '
        !        do loc_i=1,4
        !            write (*,201) (loc_mat_C_4x4(loc_i,loc_j),loc_j=1,4)
        !        end do
        !        print*,'loc_vec_D_4 ', char(9), loc_vec_D_4

        ! Solution at SWI, top of layer 1
        call sub_calcfg_l12_PO4_M(z0, reac1_po4_ox, reac2_po4_ox, ksPO4/(1+KPO4_ox), PO4s*ksPO4/(1+KPO4_ox), &
        DPO41/(1+KPO4_ox), DPO42/(1+KPO4_ox), 0.0, dum_mat_C1, dum_vec_D1, &
        dum_ltype1, 0.0, 0.0, Dbio, 0.0, (1/SD)*ksPO4, e1_z0_P, dedz1_z0_P, f1_z0_P, &
        dfdz1_z0_P, g1_z0_P, dgdz1_z0_P, p1_z0_P, dpdz1_z0_P, q1_z0_P, dqdz1_z0_P, &
        e1_z0_M, dedz1_z0_M, f1_z0_M, dfdz1_z0_M, g1_z0_M, dgdz1_z0_M, &
        p1_z0_M, dpdz1_z0_M, q1_z0_M, dqdz1_z0_M)

        ! transform to use coeffs from l2
        ! Now find 'transformed' basis functions such that in layer 1 (here for O2, PO4 is a bit more complex)
        ! O2 = A_2*et + B_2*ft + gt  (ie layer 1 soln written in terms of layer 2 coeffs A_2, B_2)
        loc_EFPQ_P = (/ e1_z0_P, f1_z0_P, p1_z0_P, q1_z0_P /)
        loc_dEFPQdz_P = (/ dedz1_z0_P, dfdz1_z0_P, dpdz1_z0_P, dqdz1_z0_P /)
        loc_EFPQ_M = (/ e1_z0_M, f1_z0_M, p1_z0_M, q1_z0_M /)
        loc_dEFPQdz_M = (/dedz1_z0_M, dfdz1_z0_M, dpdz1_z0_M, dqdz1_z0_M /)

        call sub_xformsoln_PO4_M(loc_EFPQ_P, loc_EFPQ_M, loc_dEFPQdz_P, loc_dEFPQdz_M, &
        g1_z0_P, g1_z0_M, dgdz1_z0_P, dgdz1_z0_M, loc_mat_C_4x4, loc_vec_D_4, &
        loc_EFPQ_P_t, g_P, loc_dEFPQdz_P_t, dgdz_P, loc_EFPQ_M_t, &
        g_M, loc_dEFPQdz_M_t, dgdz_M)

        ! SD assume D2 == 0 (as q, dqdz2_zinf = 0 ) and solve for 3 unknowns
        loc_mat_X_3x3 =  transpose(reshape((/ dedz2_zinf_P, dfdz2_zinf_P, dpdz2_zinf_P, &
        loc_EFPQ_P_t(1), loc_EFPQ_P_t(2), loc_EFPQ_P_t(3), &
        w*loc_EFPQ_M_t(1), w*loc_EFPQ_M_t(2), w*loc_EFPQ_M_t(3) /), shape(loc_mat_X_3x3)))
        loc_vec_Z_3= (/ -dgdz2_zinf_P, &
        dum_swiconc_PO4 - g_P, &
        dum_swiflux_M - w*g_M  /)

        ! calculate the integration conctants for Layer 2
        ! just need it once, so actually no need for subroutine, but maybe for later
        loc_dim = 3
        call sub_solve2eqn_PO4_M(loc_mat_X_3x3, loc_vec_Z_3, rPO4_M_A2, rPO4_M_B2, rPO4_M_C2, loc_dim)
        rPO4_M_D2 = 0.0
        ! save IC in a vector for a later calculation
        loc_Layer2_IC = (/ rPO4_M_A2, rPO4_M_B2, rPO4_M_C2, rPO4_M_D2 /)

        ! calculate concentration at zinf
        loc_conczinf = rPO4_M_A2*e2_zinf_P+rPO4_M_B2*f2_zinf_P + g2_zinf_P

        ! CALCULATE FINAL SWI fluxes and save the coefficients for
        ! DH: use A2, B2, C2, D2 as these are _xformed_ layer 1 basis functions
        loc_new_swiflux_PO4 = por*(DPO41/(1+KPO4_ox)*(rPO4_M_A2*loc_dEFPQdz_P_t(1)+rPO4_M_B2*loc_dEFPQdz_P_t(2) &
        + rPO4_M_C2*loc_dEFPQdz_P_t(3)+rPO4_M_D2*loc_dEFPQdz_P_t(4) + dgdz_P) - w*(dum_swiconc_PO4 - loc_conczinf))
        ! Does actually not exist, as it is a solid, just calculate for debugging
        loc_new_swiflux_M = por*Dbio*(rPO4_M_A2*loc_dEFPQdz_M_t(1)+rPO4_M_B2*loc_dEFPQdz_M_t(2) + &
        rPO4_M_C2*loc_dEFPQdz_M_t(3) + rPO4_M_D2*loc_dEFPQdz_M_t(4) + dgdz_M)

        ! save coeffs for layer 1 - in case I want to calculate a profile later
        loc_Layer1_IC = matmul(loc_mat_C_4x4, loc_Layer2_IC) + loc_vec_D_4

    !        print*,' '
    !        print*,'loc_new_swiflux_PO4 ', char(9), loc_new_swiflux_PO4
    !        print*,'loc_new_swiflux_M ', char(9), loc_new_swiflux_M
    !        print*,'loc_Layer1_IC ', char(9), loc_Layer1_IC
    !        print*,'loc_Layer2_IC ', char(9), loc_Layer2_IC

    END SUBROUTINE sub_huelseetal2016_zPO4_M


    ! ****************************************************************************************************************************** !
    !   *****************************************************************
    !   *****************************************************************
    !------------------------------------------------------------------------------------

    !------------------------------------------------------------------------------------
    !   *****************************************************************
    !   *****************************************************************

    !                   Dissolved Inorganic Carbon (DIC)

    !   *****************************************************************
    !   *****************************************************************
    !------------------------------------------------------------------------------------

    SUBROUTINE sub_huelseetal2016_zDIC(dum_swiconc_DIC, loc_new_swiflux_DIC)


        ! dummy arguments
        real,INTENT(in)::dum_swiconc_DIC                ! DIC concentrations at SWI
        real,INTENT(inout)::loc_new_swiflux_DIC         ! DIC flux

        ! local variables
        real loc_conczinf
        real reac1_dic, reac2_dic                 ! reactive terms: OM degradation
        integer ltype1 , ltype2
        real ls_a1, ls_b1, ls_c1, ls_d1, ls_e1, ls_f1
        real ls_a2, ls_b2, ls_c2, ls_d2, ls_e2, ls_f2
        real zso4_a, zso4_b, zso4_c, zso4_d, zso4_e, zso4_f
        real e2_zinf, dedz2_zinf, f2_zinf, dfdz2_zinf, g2_zinf, dgdz2_zinf
        real e1_zso4, dedz1_zso4, f1_zso4, dfdz1_zso4, g1_zso4, dgdz1_zso4
        real e2_zso4, dedz2_zso4, f2_zso4, dfdz2_zso4, g2_zso4, dgdz2_zso4
        real e1_00, dedz1_00, f1_00, dfdz1_00, g1_00, dgdz1_00
        real e1_0, dedz1_0, f1_0, dfdz1_0, g1_0, dgdz1_0
        !
        real rDIC_A2, rDIC_B2
        real rDIC_A1, rDIC_B1

        real zso4FDIC

        reac1_dic = DICC1                 ! DIC/C until zSO4 (mol/mol)
        reac2_dic = DICC2                 !DIC/C below zSO4 (mol/mol)

        !    print*, ''
        !    print*, '------------------------------------------------------------------'
        !    print*, '---------------------- START zDIC ------------------------------- '
        !    print*, ''

        ! Calculate DIC

        ! Preparation: for each layer, sort out solution-matching across bioturbation boundary if necessary
        ! layer 1: 0 < z < zso4, DIC produced my OM degradation
        !    prepfg_l12(    reac1,      reac2,   ktemp, zU,  zL,   D1,    D2, ls_a, ls_b, ls_c, ls_d, ls_e, ls_f, ltype)
        call sub_prepfg_l12(reac1_dic, reac1_dic, 0.0, 0.0, zso4, DDIC1, DDIC2, ls_a1, ls_b1, ls_c1, ls_d1, ls_e1, ls_f1, ltype1)

        ! layer 2: zso4 < z < zinf, DIC production by OM degradation (Methanogenesis) -> different production rate
        call sub_prepfg_l12(reac2_dic, reac2_dic, 0.0, zso4, zinf, DDIC1, DDIC2, ls_a2, ls_b2, ls_c2, ls_d2, ls_e2, ls_f2, ltype2)


        ! Work up from the bottom, matching solutions at boundaries
        ! Basis functions at bottom of layer 2 zinf
        !   calcfg_l12(  z,     reac1,      reac2, ktemp, ls_a, ls_b,  ls_c, ls_d,   ls_e, ls_f,  ls_D1, ls_D2, ltype, e, dedz, f, dfdz, g, dgdz)
        call sub_calcfg_l12(zinf, reac2_dic, reac2_dic, 0.0, ls_a2, ls_b2, ls_c2, ls_d2, ls_e2, ls_f2, DDIC1, DDIC2, ltype2, &
        e2_zinf, dedz2_zinf, f2_zinf, dfdz2_zinf, g2_zinf, dgdz2_zinf)

        ! Match at zso4, layer 1 - layer 2 (continuity and flux with AOM production)
        ! basis functions at bottom of layer 1
        call sub_calcfg_l12(zso4, reac1_dic, reac1_dic, 0.0, ls_a1, ls_b1, ls_c1, ls_d1, ls_e1, ls_f1, DDIC1, DDIC2, ltype1, &
        e1_zso4, dedz1_zso4, f1_zso4, dfdz1_zso4, g1_zso4, dgdz1_zso4)

        ! ... and top of layer 2
        call sub_calcfg_l12(zso4, reac2_dic, reac2_dic, 0.0, ls_a2, ls_b2, ls_c2, ls_d2, ls_e2, ls_f2, DDIC1, DDIC2, ltype2, &
        e2_zso4, dedz2_zso4, f2_zso4, dfdz2_zso4, g2_zso4, dgdz2_zso4)

        ! flux of DIC produced by AOM interface (Source of DIC)
!        zso4FDIC = FUN_calcReac(zso4, zinf, MC, MC) ! MULTIPLY BY 1/POR ????
        if(zso4 .EQ. zinf)then
            	zso4FDIC = 0.0
        else
		zso4FDIC = FUN_calcReac(zso4, zinf, MC, MC) ! MULTIPLY BY 1/POR ????!             
        end if
!        print*,'zso4 = ', zso4
!        print*,'flux of DIC produced by AOM interface zso4FDIC = ', zso4FDIC

        ! match solutions at zso4 - continuous concentration and flux
        call sub_matchsoln(e1_zso4, f1_zso4, g1_zso4, dedz1_zso4, dfdz1_zso4, dgdz1_zso4, &
        e2_zso4, f2_zso4, g2_zso4, dedz2_zso4, dfdz2_zso4, dgdz2_zso4, &
        0.0, -gammaCH4*zso4FDIC/DDIC2, zso4_a, zso4_b, zso4_c, zso4_d, zso4_e, zso4_f)

        ! Solution at swi, top of layer 1
        call sub_calcfg_l12(0.0, reac1_dic, reac1_dic, 0.0, ls_a1, ls_b1, ls_c1, ls_d1, ls_e1, ls_f1, DDIC1, DDIC2, ltype1, &
        e1_00, dedz1_00, f1_00, dfdz1_00, g1_00, dgdz1_00)

        ! transform to use coeffs from l2
        call sub_xformsoln(e1_00, f1_00, g1_00, dedz1_00, dfdz1_00, dgdz1_00, &
        zso4_a, zso4_b, zso4_c, zso4_d, zso4_e, zso4_f, &
        e1_0, f1_0, g1_0, dedz1_0,  dfdz1_0, dgdz1_0)

        ! Solve for ADIC, BDIC given boundary conditions (expressed in terms of transformed basis fns, layer 4 A, B)
        !  ADIC*dedz4_zinf   +  BDIC*dfz4_zinf  + dgz4_zinf = 0;          % zero flux at zinf
        !  ADIC*e1_0     +   BDIC*f1_0     + g1_0  = swi.DIC0;

        !  | dedz4_zinf dfdz4_zinf |  |ADIC|   = | -dgz4_zinf       |
        !  | e1_0     f1_0         |  |BDIC|     | swi.DIC0 - g1_0 |

        call sub_solve2eqn(dedz2_zinf, dfdz2_zinf, e1_0, f1_0, -dgdz2_zinf, dum_swiconc_DIC - g1_0, rDIC_A2, rDIC_B2)

        ! calculate concentration at zinf
        loc_conczinf = rDIC_A2*e2_zinf+rDIC_B2*f2_zinf + g2_zinf

        ! flux at swi - DO include por so this is per cm^2 water column area
        ! DH: added advective flux 28.05.2016
        loc_new_swiflux_DIC = por*(DDIC1*(rDIC_A2*dedz1_0+rDIC_B2*dfdz1_0 + dgdz1_0) - w*(dum_swiconc_DIC - loc_conczinf))   ! NB: use A2, B2 as these are _xformed_ layer 1 basis functions

        ! save coeffs for layers 1
        rDIC_A1 = zso4_a*rDIC_A2 + zso4_b*rDIC_B2 + zso4_e
        rDIC_B1 = zso4_c*rDIC_A2 + zso4_d*rDIC_B2 + zso4_f

        !    print*,' ---------------- RESULTS: benthic_zDIC ---------------- '
!    	print*,' '
!    	print*,' '
!	print*,'flxswiDIC ', char(9), loc_new_swiflux_DIC
!	print*,'adv z0 ', char(9), w*dum_swiconc_DIC
!	print*,'adv zinf ', char(9), w*loc_conczinf
!	print*,'flxswiDIC - advzinf ', char(9), loc_new_swiflux_DIC - w*loc_conczinf
    !    print*,'rH2S_A4, rH2S_B4, rH2S_A3, rH2S_B3, rH2S_A2, rH2S_B2, rH2S_A1, rH2S_B1', &
    !          &  rH2S_A4, rH2S_B4, rH2S_A3, rH2S_B3, rH2S_A2, rH2S_B2, rH2S_A1, rH2S_B1


    END SUBROUTINE sub_huelseetal2016_zDIC

    !------------------------------------------------------------------------------------
    !   *****************************************************************
    !   *****************************************************************

    !                           Alkalinity (ALK)

    !   *****************************************************************
    !   *****************************************************************
    !------------------------------------------------------------------------------------

    SUBROUTINE sub_huelseetal2016_zALK(dum_swiconc_ALK, loc_new_swiflux_ALK)

        ! dummy arguments
        real,INTENT(in)::dum_swiconc_ALK                ! ALK concentrations at SWI
        real,INTENT(inout)::loc_new_swiflux_ALK         ! ALK flux

        ! local variables
        real loc_conczinf
        real reac11_alk, reac12_alk, reac21_alk, reac22_alk, reac3_alk, reac4_alk                 ! reactive terms: OM degradation
        integer ltype1, ltype2, ltype3, ltype4
        real ls_a1, ls_b1, ls_c1, ls_d1, ls_e1, ls_f1
        real ls_a2, ls_b2, ls_c2, ls_d2, ls_e2, ls_f2
        real ls_a3, ls_b3, ls_c3, ls_d3, ls_e3, ls_f3
        real ls_a4, ls_b4, ls_c4, ls_d4, ls_e4, ls_f4
        real e4_zinf, dedz4_zinf, f4_zinf, dfdz4_zinf, g4_zinf, dgdz4_zinf
        real e3_zso4, dedz3_zso4, f3_zso4, dfdz3_zso4, g3_zso4, dgdz3_zso4
        real e4_zso4, dedz4_zso4, f4_zso4, dfdz4_zso4, g4_zso4, dgdz4_zso4
        real zso4_a, zso4_b, zso4_c, zso4_d, zso4_e, zso4_f
        real e2_zno3, dedz2_zno3, f2_zno3, dfdz2_zno3, g2_zno3, dgdz2_zno3
        real e3_zno30, dedz3_zno30, f3_zno30, dfdz3_zno30, g3_zno30, dgdz3_zno30
        real e3_zno3, dedz3_zno3, f3_zno3, dfdz3_zno3, g3_zno3, dgdz3_zno3
        real zno3_a, zno3_b, zno3_c, zno3_d, zno3_e, zno3_f
        real e1_zox, dedz1_zox, f1_zox, dfdz1_zox, g1_zox, dgdz1_zox
        real e2_zox0, dedz2_zox0, f2_zox0, dfdz2_zox0, g2_zox0, dgdz2_zox0
        real e2_zox, dedz2_zox, f2_zox, dfdz2_zox, g2_zox, dgdz2_zox
        real zox_a, zox_b, zox_c, zox_d, zox_e, zox_f
        real e1_00, dedz1_00, f1_00, dfdz1_00, g1_00, dgdz1_00
        real e1_0, dedz1_0, f1_0, dfdz1_0, g1_0, dgdz1_0

        real rALK_A4, rALK_B4
        real rALK_A3, rALK_B3
        real rALK_A2, rALK_B2
        real rALK_A1, rALK_B1

        real zso4FALK, zoxFALK

        reac11_alk = gamma*NC1/(1+KNH4)*ALKRNIT+ALKROX           ! z < zox:  Nitrification (-2) Aerobic degradation (+15/106)
        reac12_alk = gamma*NC2/(1+KNH4)*ALKRNIT+ALKROX           ! z < zox:  Nitrification (-2) Aerobic degradation (+15/106)
        reac21_alk = ALKRDEN                                     ! zox < z < zno3: Denitrification (+93.4/106)
        reac22_alk = ALKRDEN                                     ! zox < z < zno3: Denitrification (+93.4/106)
        reac3_alk = ALKRSUL                                    ! zno3 < z < zso4: Sulfate reduction (+15/106)
        reac4_alk = ALKRMET                                      ! zso4 < z < zinf: Methanogenesis (+14/106)
        !
        !        reac11_alk = gamma*NC1/(1+KNH4)*ALKRNIT+ALKROX*SD           ! z < zox:  Nitrification (-2) Aerobic degradation (+15/106)
        !        reac12_alk = gamma*NC2/(1+KNH4)*ALKRNIT+ALKROX*SD           ! z < zox:  Nitrification (-2) Aerobic degradation (+15/106)
        !        reac2_alk = SD*ALKRDEN                                      ! zox < z < zno3: Denitrification (+93.4/106)
        !        reac3_alk = SD*ALKRSUL                                      ! zno3 < z < zso4: Sulfate reduction (+15/106)
        !        reac4_alk = SD*ALKRMET                                      ! zso4 < z < zinf: Methanogenesis (+14/106)

        !    print*, ''
        !    print*, '------------------------------------------------------------------'
        !    print*, '---------------------- START zALK ------------------------------- '
        !    print*, ''

        ! Calculate ALK

        ! Preparation: for each layer, sort out solution-matching across bioturbation boundary if necessary
        ! layer 1: 0 < z < zox, Nitrification (-) Aerobic degradation (+)
        !    prepfg_l12(reac1,      reac2,    ktemp, zU,  zL,   D1,     D2, ls_a, ls_b, ls_c, ls_d, ls_e, ls_f, ltype)
        call sub_prepfg_l12(reac11_alk, reac12_alk, 0.0, 0.0, zox, DALK1, DALK2, ls_a1, ls_b1, ls_c1, ls_d1, ls_e1, ls_f1, ltype1)

        ! layer 2: zox < z < zno3, Denitrification (+)
        call sub_prepfg_l12(reac21_alk, reac22_alk, 0.0, zox, zno3, DALK1, DALK2, ls_a2, ls_b2, ls_c2, ls_d2, ls_e2, ls_f2, ltype2)

        ! layer 3: zno3 < z < zso4, Sulfate reduction (+)
        call sub_prepfg_l12(reac3_alk, reac3_alk, 0.0, zno3, zso4, DALK1, DALK2, ls_a3, ls_b3, ls_c3, ls_d3, ls_e3, ls_f3, ltype3)

        ! layer 4: zso4 < z < zinf, Methanogenesis (+)
        call sub_prepfg_l12(reac4_alk, reac4_alk, 0.0, zso4, zinf, DALK1, DALK2, ls_a4, ls_b4, ls_c4, ls_d4, ls_e4, ls_f4, ltype4)

        ! Work up from the bottom, matching solutions at boundaries
        ! Basis functions at bottom of layer 4 zinf
        !   calcfg_l12(  z,     reac1,      reac2, ktemp, ls_a, ls_b,  ls_c, ls_d,   ls_e, ls_f,  ls_D1, ls_D2, ltype, e, dedz, f, dfdz, g, dgdz)
        call sub_calcfg_l12(zinf, reac4_alk, reac4_alk, 0.0, ls_a4, ls_b4, ls_c4, ls_d4, ls_e4, ls_f4, DALK1, DALK2, ltype4, &
        e4_zinf, dedz4_zinf, f4_zinf, dfdz4_zinf, g4_zinf, dgdz4_zinf)

        ! Match at zso4, layer 3 - layer 4 (continuity and flux with AOM production)
        ! basis functions at bottom of layer 3
        call sub_calcfg_l12(zso4, reac3_alk, reac3_alk, 0.0, ls_a3, ls_b3, ls_c3, ls_d3, ls_e3, ls_f3, DALK1, DALK2, ltype3, &
        e3_zso4, dedz3_zso4, f3_zso4, dfdz3_zso4, g3_zso4, dgdz3_zso4)

        ! ... and top of layer 4
        call sub_calcfg_l12(zso4, reac4_alk, reac4_alk, 0.0, ls_a4, ls_b4, ls_c4, ls_d4, ls_e4, ls_f4, DALK1, DALK2, ltype4, &
        e4_zso4, dedz4_zso4, f4_zso4, dfdz4_zso4, g4_zso4, dgdz4_zso4)

        ! flux of ALK produced by AOM interface (Source of ALK)
!!!        zso4FALK = 0.0      !no secondary redox!
        if(zso4 .EQ. zinf)then
            zso4FALK = 0.0
        else
!          zso4FALK = ALKRAOM*gammaCH4*FUN_calcReac(zso4, zinf, SD, SD) ! Dominik was before 12.03.2018
!             print*,'zso4FALK SD', zso4FALK
            zso4FALK = ALKRAOM*gammaCH4*FUN_calcReac(zso4, zinf, MC, MC) ! MULTIPLY BY 1/POR ????
!             print*,'zso4FALK MC', zso4FALK
        end if

        ! match solutions at zso4 - continuous concentration and flux
        call sub_matchsoln(e3_zso4, f3_zso4, g3_zso4, dedz3_zso4, dfdz3_zso4, dgdz3_zso4, &
        e4_zso4, f4_zso4, g4_zso4, dedz4_zso4, dfdz4_zso4, dgdz4_zso4, &
        0.0, -zso4FALK/DALK2, zso4_a, zso4_b, zso4_c, zso4_d, zso4_e, zso4_f)

        ! Match at zno3, layer 2 - layer 3 (continuity and flux)
        ! basis functions at bottom of layer 2
        call sub_calcfg_l12(zno3, reac21_alk, reac22_alk, 0.0, ls_a2, ls_b2, ls_c2, ls_d2, ls_e2, ls_f2, DALK1, DALK2, ltype2, &
        e2_zno3, dedz2_zno3, f2_zno3, dfdz2_zno3, g2_zno3, dgdz2_zno3)

        ! ... and top of layer 3
        call sub_calcfg_l12(zno3, reac3_alk, reac3_alk, 0.0, ls_a3, ls_b3, ls_c3, ls_d3, ls_e3, ls_f3, DALK1, DALK2, ltype3, &
        e3_zno30, dedz3_zno30, f3_zno30, dfdz3_zno30, g3_zno30, dgdz3_zno30)

        ! ... transformed to use coeffs from l4
        call sub_xformsoln(e3_zno30, f3_zno30, g3_zno30, dedz3_zno30, dfdz3_zno30, dgdz3_zno30, &
        zso4_a , zso4_b , zso4_c , zso4_d , zso4_e ,zso4_f, &
        e3_zno3, f3_zno3, g3_zno3, dedz3_zno3, dfdz3_zno3, dgdz3_zno3)
        ! match solutions at zno3 - continuous concentration and flux
        call sub_matchsoln(e2_zno3, f2_zno3, g2_zno3, dedz2_zno3, dfdz2_zno3, dgdz2_zno3, &
        e3_zno3, f3_zno3, g3_zno3, dedz3_zno3, dfdz3_zno3, dgdz3_zno3, &
        0.0, 0.0, zno3_a, zno3_b, zno3_c, zno3_d, zno3_e, zno3_f)

        ! Match at zox, layer 1 - layer 2 (continuity, flux discontinuity from ALK source)
        ! flux of ALK to oxic interface (from all sources of ALK below) from NH4 and H2S
!        zoxFALK = 0.0       !no secondary redox!
        ! with implicit N alkalinity
        ! Dom 12.03.18 Don't need the -1.0 because all oxidised is accounted for in ALKSO4
!        zoxFALK = -1.0*gamma*FUN_calcReac(zno3, zinf, (16.0/106.0)*SD*1/(1+KNH4),(16.0/106.0)*SD*1/(1+KNH4)) &      ! was until 27.02. -1.0 before -2.0* gamma ... MULTIPLY BY 1/POR ????
!        + ALKRH2S*gammaH2S*FUN_calcReac(zno3, zso4, SO4C, SO4C)                 ! Dominik 25.02.2016
        zoxFALK = ALKRNIT*gamma*FUN_calcReac(zno3, zinf, NC1/(1+KNH4),NC2/(1+KNH4)) &      ! was until 27.02. -1.0 before -2.0* gamma ... MULTIPLY BY 1/POR ????
        + ALKRH2S*gammaH2S*FUN_calcReac(zno3, zso4, SO4C, SO4C)                 ! Dominik 25.02.2016
!        zoxFALK = ALKRH2S*gammaH2S*FUN_calcReac(zno3, zso4, SO4C, SO4C)                 ! Dominik 05.02.2018 try to fix ALK (here no NH4 contribution)
        !        zoxFALK = ALKRNIT*gamma*FUN_calcReac(zno3, zinf, NC1/(1+KNH4),NC2/(1+KNH4)) &      ! MULTIPLY BY 1/POR ????
        !                  + ALKRH2S*gammaH2S*FUN_calcReac(zno3, zso4, SO4C, SO4C)                 ! Dominik 25.02.2016
        ! DH 24.02.2016: actually should be 2 integrals for ALK produced: ALK-reduction + AOM (see documentation, but has the same reac const = 0.5) :
!        if(zso4 < zinf)then
!            print*,'zno3 = ', zno3
!        print*,'zoxFALK = ', zoxFALK
!            print*,'impl flux of ALK to oxic interface zoxFALK = ', -1.0*gamma*FUN_calcReac(zno3, zinf, (16.0/106.0)*SD*1/(1+KNH4),(16.0/106.0)*SD*1/(1+KNH4)) + ALKRH2S*gammaH2S*FUN_calcReac(zno3, zso4, SO4C, SO4C) 
!            print*,'flux of ALK to oxic interface zoxFALK = ', zoxFALK
!            print*,'flux N   = ', -1.0*gamma*FUN_calcReac(zno3, zinf, (16.0/106.0)*SD*1/(1+KNH4),(16.0/106.0)*SD*1/(1+KNH4))
!            print*,'flux N = ', ALKRNIT*gamma*FUN_calcReac(zno3, zinf, NC1/(1+KNH4),NC2/(1+KNH4))        
!            print*,'flux H2S = ', ALKRH2S*gammaH2S*FUN_calcReac(zno3, zso4, SO4C, SO4C)
!            print*,' '
!        end if
        ! basis functions at bottom of layer 1
        call sub_calcfg_l12(zox, reac11_alk, reac12_alk, 0.0, ls_a1, ls_b1, ls_c1, ls_d1, ls_e1, ls_f1, DALK1, DALK2, ltype1, &
        e1_zox, dedz1_zox, f1_zox, dfdz1_zox, g1_zox, dgdz1_zox)

        ! basis functions at top of layer 2
        call sub_calcfg_l12(zox, reac21_alk, reac22_alk, 0.0, ls_a2, ls_b2, ls_c2, ls_d2, ls_e2, ls_f2, DALK1, DALK2, ltype2, &
        e2_zox0, dedz2_zox0, f2_zox0, dfdz2_zox0, g2_zox0, dgdz2_zox0)

        !   transform to use coeffs from l4
        call sub_xformsoln(e2_zox0, f2_zox0, g2_zox0, dedz2_zox0, dfdz2_zox0, dgdz2_zox0, &
        zno3_a , zno3_b , zno3_c , zno3_d , zno3_e ,zno3_f, &
        e2_zox, f2_zox, g2_zox, dedz2_zox, dfdz2_zox, dgdz2_zox)

        ! match solutions at zox - continuous concentration, flux discontinuity from ALK ox

        IF(zox .le. zbio) THEN
            call sub_matchsoln(e1_zox, f1_zox, g1_zox, dedz1_zox, dfdz1_zox, dgdz1_zox, &
            e2_zox, f2_zox, g2_zox, dedz2_zox, dfdz2_zox, dgdz2_zox, &
            0.0, -r_zxf*zoxFALK/DALK1, zox_a, zox_b, zox_c, zox_d, zox_e, zox_f)
            ! Dominik 07.02.2018 change sign to + r_zxf*zoxFALK/DALK1
!            call sub_matchsoln(e1_zox, f1_zox, g1_zox, dedz1_zox, dfdz1_zox, dgdz1_zox, &
!            e2_zox, f2_zox, g2_zox, dedz2_zox, dfdz2_zox, dgdz2_zox, &
!            0.0, +r_zxf*zoxFALK/DALK1, zox_a, zox_b, zox_c, zox_d, zox_e, zox_f)
        ELSE
            call sub_matchsoln(e1_zox, f1_zox, g1_zox, dedz1_zox, dfdz1_zox, dgdz1_zox, &
            e2_zox, f2_zox, g2_zox, dedz2_zox, dfdz2_zox, dgdz2_zox, &
            0.0, -r_zxf*zoxFALK/DALK2, zox_a, zox_b, zox_c, zox_d, zox_e, zox_f)
            ! Dominik 07.02.2018 change sign to + r_zxf*zoxFALK/DALK1
!            call sub_matchsoln(e1_zox, f1_zox, g1_zox, dedz1_zox, dfdz1_zox, dgdz1_zox, &
!            e2_zox, f2_zox, g2_zox, dedz2_zox, dfdz2_zox, dgdz2_zox, &
!            0.0, +r_zxf*zoxFALK/DALK2, zox_a, zox_b, zox_c, zox_d, zox_e, zox_f)
        END IF

        ! Solution at swi, top of layer 1
        call sub_calcfg_l12(0.0, reac11_alk, reac12_alk, 0.0, ls_a1, ls_b1, ls_c1, ls_d1, ls_e1, ls_f1, DALK1, DALK2, ltype1, &
        e1_00, dedz1_00, f1_00, dfdz1_00, g1_00, dgdz1_00)

        ! transform to use coeffs from l4
        call sub_xformsoln(e1_00, f1_00, g1_00, dedz1_00, dfdz1_00, dgdz1_00, &
        zox_a , zox_b , zox_c , zox_d , zox_e ,zox_f, &
        e1_0, f1_0, g1_0, dedz1_0,  dfdz1_0, dgdz1_0)

        ! Solve for AALK, BALK given boundary conditions (expressed in terms of transformed basis fns, layer 4 A, B)
        !  AALK*dedz4_zinf   +  BALK*dfz4_zinf  + dgz4_zinf = 0;          % zero flux at zinf
        !  AALK*e1_0     +   BALK*f1_0     + g1_0  = swi.ALK0;

        !  | dedz4_zinf dfdz4_zinf |  |AALK|   = | -dgz4_zinf       |
        !  | e1_0     f1_0         |  |BALK|     | swi.ALK0 - g1_0 |

        call sub_solve2eqn(dedz4_zinf, dfdz4_zinf, e1_0, f1_0, -dgdz4_zinf, dum_swiconc_ALK - g1_0, rALK_A4, rALK_B4)

        ! calculate concentration at zinf
        loc_conczinf = rALK_A4*e4_zinf+rALK_B4*f4_zinf + g4_zinf

        ! flux at swi - DO include por so this is per cm^2 water column area
        loc_new_swiflux_ALK = por*(DALK1*(rALK_A4*dedz1_0+rALK_B4*dfdz1_0 + dgdz1_0) - w*(dum_swiconc_ALK - loc_conczinf))   ! NB: use A4, B4 as these are _xformed_ layer 1 basis functions

        ! save coeffs for layers 3, 2 and 1
        rALK_A3 = zso4_a*rALK_A4 + zso4_b*rALK_B4 + zso4_e
        rALK_B3 = zso4_c*rALK_A4 + zso4_d*rALK_B4 + zso4_f

        rALK_A2 = zno3_a*rALK_A4 + zno3_b*rALK_B4 + zno3_e
        rALK_B2 = zno3_c*rALK_A4 + zno3_d*rALK_B4 + zno3_f

        rALK_A1 = zox_a*rALK_A4 + zox_b*rALK_B4 + zox_e
        rALK_B1 = zox_c*rALK_A4 + zox_d*rALK_B4 + zox_f

        !    print*,' ---------------- RESULTS: benthic_zALK ---------------- '
    !        print*,'flxswiALK ', char(9), flxswiALK
    !    print*,'rALK_A4, rALK_B4, rALK_A3, rALK_B3, rALK_A2, rALK_B2, rALK_A1, rALK_B1', &
    !          &  rALK_A4, rALK_B4, rALK_A3, rALK_B3, rALK_A2, rALK_B2, rALK_A1, rALK_B1


    !   print*,'INPUT1: dedz4_zinf, dfdz4_zinf, e1_0, f1_0, -dgdz4_zinf, ALK0 - g1_0', &
    !           & dedz4_zinf, dfdz4_zinf, e1_0, f1_0, -dgdz4_zinf, ALK0 - g1_0
    !    print*,'INPUT2: zox_a , zox_b , zox_c , zox_d , zox_e ,zox_f', &
    !            & zox_a , zox_b , zox_c , zox_d , zox_e ,zox_f
    !    print*,'RESULTS:  rALK_A4, rALK_B4', &
    !            & rALK_A4, rALK_B4

    END SUBROUTINE sub_huelseetal2016_zALK


    FUNCTION FUN_calcReac(zU, zL, reac1, reac2)

        real,intent(in):: zU, zL, reac1, reac2
        real FUN_calcReac

        ! Integral of reacted organic matter from zU to zL,
        ! multiplied by stoichiometric factors reac1, reac2 (for the two OC phases)

        ! Vector-friendly way of handling 3 cases:
        ! 1) wholly within bioturbated layer:    FUN_calcReac_l1(zU,zL)     + (0 =) FUN_calcReac_l2(zbio, zbio)
        ! 2) wholly within non-bio     layer:  (0=) FUN_calcReac_l1(zbio, zbio) +   FUN_calcReac_l2(zU, zL)
        ! 3) crossing zbio                       calcRead_l1(zU,zbio)   +       FUN_calcReac_l2(zbio, zL)

        FUN_calcReac = FUN_calcReac_l1(min(zU,zbio), min(zL,zbio), reac1, reac2) &
        + FUN_calcReac_l2(max(zU,zbio), max(zL, zbio), reac1, reac2)

    ! TODO confirm (1-por)*  has been added (to k1 & k2 ?)

    END FUNCTION FUN_calcReac

    ! ****************************************************************************************************************************** !
    !   *****************************************************************
    !   *****************************************************************
    !------------------------------------------------------------------------------------

    FUNCTION FUN_calcReac_l1(zU, zL, reac1, reac2)

        real,intent(in):: zU, zL, reac1, reac2
        real FUN_calcReac_l1, reacf1, reacf2

        reacf1 = k1*reac1
        reacf2 = k2*reac2
        FUN_calcReac_l1 = -reacf1*(A11*(exp(aa11*zU)*bb11 - exp(bb11*zU)*aa11 - exp(aa11*zL)*bb11 + exp(bb11*zL)*aa11) &
        + dum_POC1_conc_swi*exp(bb11*zU)*aa11 - dum_POC1_conc_swi*exp(bb11*zL)*aa11)/(aa11*bb11 + const_real_nullsmall) &
        -reacf2*(A12*(exp(aa12*zU)*bb12 - exp(bb12*zU)*aa12 - exp(aa12*zL)*bb12 + exp(bb12*zL)*aa12) &
        + dum_POC2_conc_swi*exp(bb12*zU)*aa12 - dum_POC2_conc_swi*exp(bb12*zL)*aa12)/(aa12*bb12 + const_real_nullsmall)
    !    print*,'in FUN_calcReac_l1 dum_POC1_conc_swi = ', dum_POC1_conc_swi
    !    print*,'in FUN_calcReac_l1 dum_POC2_conc_swi = ', dum_POC2_conc_swi


    END FUNCTION FUN_calcReac_l1

    ! ****************************************************************************************************************************** !
    !   *****************************************************************
    !   *****************************************************************
    !------------------------------------------------------------------------------------

    FUNCTION FUN_calcReac_l2(zU, zL, reac1, reac2)

        real,intent(in):: zU, zL, reac1, reac2
        real FUN_calcReac_l2, reacf1, reacf2

        reacf1 = k1*reac1
        reacf2 = k2*reac2

        FUN_calcReac_l2 = -reacf1*A21*(exp(aa21*zU) - exp(aa21*zL))/(aa21 + const_real_nullsmall) &
        -reacf2*A22*(exp(aa22*zU) - exp(aa22*zL))/(aa22 + const_real_nullsmall)

    !    print*,'FUN_calcReac_l2', FUN_calcReac_l2

    END FUNCTION FUN_calcReac_l2


    ! ****************************************************************************************************************************** !
    !   *****************************************************************
    !   *****************************************************************
    !------------------------------------------------------------------------------------

    FUNCTION FUN_calcOM(zU, zL, reac1, reac2)

        real,intent(in):: zU, zL, reac1, reac2
        real FUN_calcOM

        ! local variables
        real loc_FUN_calcOM_l1, loc_FUN_calcOM_l2


        ! Integral of organic matter from zU to zL,
        ! multiplied by stoichiometric factors reac1, reac2 (for the two OC phases)

        ! Vector-friendly way of handling 3 cases:
        ! 1) wholly within bioturbated layer:    FUN_calcReac_l1(zU,zL)     + (0 =) FUN_calcReac_l2(zbio, zbio)
        ! 2) wholly within non-bio     layer:  (0=) FUN_calcReac_l1(zbio, zbio) +   FUN_calcReac_l2(zU, zL)
        ! 3) crossing zbio                       calcRead_l1(zU,zbio)   +       FUN_calcReac_l2(zbio, zL)

        loc_FUN_calcOM_l1 = FUN_calcOM_l1(min(zU,zbio), min(zL,zbio), reac1, reac2)
        loc_FUN_calcOM_l2 = FUN_calcOM_l2(max(zU,zbio), max(zL, zbio), reac1, reac2)

        if(isnan(loc_FUN_calcOM_l1) .AND. isnan(loc_FUN_calcOM_l2))then
            FUN_calcOM = 0.0
        elseif(isnan(loc_FUN_calcOM_l2)) then
            FUN_calcOM = loc_FUN_calcOM_l1
        else
            FUN_calcOM = loc_FUN_calcOM_l1 + loc_FUN_calcOM_l2
        end if

    !            print*,' '
    !            print*,'FUN_calcOM_l1 ', loc_FUN_calcOM_l1
    !            print*,'FUN_calcOM_l2 ', loc_FUN_calcOM_l2
    !            print*,'FUN_calcOM ', FUN_calcOM



    ! TODO confirm (1-por)*  has been added (to k1 & k2 ?)

    END FUNCTION FUN_calcOM

    ! ****************************************************************************************************************************** !
    !   *****************************************************************
    !   *****************************************************************
    !------------------------------------------------------------------------------------

    FUNCTION FUN_calcOM_l1(zU, zL, reac1, reac2)

        real,intent(in):: zU, zL, reac1, reac2
        real FUN_calcOM_l1, reacf1, reacf2

        reacf1 = reac1
        reacf2 = reac2
        FUN_calcOM_l1 = -reacf1*(A11*(exp(aa11*zU)*bb11 - exp(bb11*zU)*aa11 - exp(aa11*zL)*bb11 + exp(bb11*zL)*aa11) &
        + dum_POC1_conc_swi*exp(bb11*zU)*aa11 - dum_POC1_conc_swi*exp(bb11*zL)*aa11)/(aa11*bb11 + const_real_nullsmall) &
        -reacf2*(A12*(exp(aa12*zU)*bb12 - exp(bb12*zU)*aa12 - exp(aa12*zL)*bb12 + exp(bb12*zL)*aa12) &
        + dum_POC2_conc_swi*exp(bb12*zU)*aa12 - dum_POC2_conc_swi*exp(bb12*zL)*aa12)/(aa12*bb12 + const_real_nullsmall)

    !    print*,'in FUN_calcReac_l1 dum_POC2_conc_swi = ', dum_POC2_conc_swi


    END FUNCTION FUN_calcOM_l1

    ! ****************************************************************************************************************************** !
    !   *****************************************************************
    !   *****************************************************************
    !------------------------------------------------------------------------------------

    FUNCTION FUN_calcOM_l2(zU, zL, reac1, reac2)

        real,intent(in):: zU, zL, reac1, reac2
        real FUN_calcOM_l2, reacf1, reacf2

        reacf1 = reac1
        reacf2 = reac2

        FUN_calcOM_l2 = -reacf1*A21*(exp(aa21*zU) - exp(aa21*zL))/(aa21 + const_real_nullsmall) &
        -reacf2*A22*(exp(aa22*zU) - exp(aa22*zL))/(aa22 + const_real_nullsmall)


    !    print*,'FUN_calcReac_l2', FUN_calcReac_l2

    END FUNCTION FUN_calcOM_l2


    ! ****************************************************************************************************************************** !
    !   *****************************************************************
    !   *****************************************************************
    !------------------------------------------------------------------------------------



    SUBROUTINE sub_calcfg_l12(z, reac1, reac2, ktemp, ls_a, ls_b, ls_c, ls_d, ls_e, ls_f, ls_D1, ls_D2,&
    ltype, e, dedz, f, dfdz, g, dgdz)
            ! calculate solution basis functions, for layer which may cross bioturbation boundary
            !
            ! reac1, reac2        - mol/mol S released per organic carbon C
            !
            ! General solution for solute S is given by
            !  S(z) = A * e(z) + B * f(z) + g(z)
            !
            ! Where e,f,g are generated by matching solutions across bioturbation boundary (if necessary)
            ! Solution properties (matching etc) are input in ls
            ! On input, ls should contain fields generated by sub_prepfg_l12

        real,intent(in)::       z, reac1, reac2, ktemp
        INTEGER, INTENT(in)::   ltype
        real,intent(inout)::    e, dedz, f, dfdz, g, dgdz
        real                    ls_a, ls_b, ls_c, ls_d, ls_e, ls_f, ls_D1, ls_D2

        ! local variables
        real e_1, f_1, g_1, dedz_1, dfdz_1, dgdz_1
        !real ls_a, ls_b, ls_c, ls_d, ls_e, ls_f


        select case (ltype)
            case (1)    ! bioturbated
                !                print*, 'sub_calcfg_l12 CASE 1 bioturbated'
                call sub_calcfg_l1(z, reac1, reac2, ls_D1, ktemp, e, dedz, f, dfdz, g, dgdz)
            case (2)    ! not bioturbated
                call sub_calcfg_l2(z, reac1, reac2, ls_D2, ktemp, e, dedz, f, dfdz, g, dgdz)
            case (3)    ! crossing boundary
                !                   print*, 'sub_calcfg_l12 CASE 3 crossing boundary'
                IF(z >= zbio) THEN      ! below bioturbated region
                    call sub_calcfg_l2(z, reac1, reac2, ls_D2, ktemp, e, dedz, f, dfdz, g, dgdz)
                else    ! above bioturbated region
                    !                    print*, 'CASE 3 crossing boundary ELSEEEEE'
                    call sub_calcfg_l1(z, reac1, reac2, ls_D1, ktemp, e_1, dedz_1, f_1, dfdz_1, g_1, dgdz_1)
                    !                    print*, 'sub_calcfg_l1111111111111: z, reac1, reac2, DO21, ktemp, e_1, dedz_1, f_1, dfdz_1,&
                    !                            & g_1, dgdz_1', z, reac1, reac2, DO21, ktemp, e_1, dedz_1, f_1, dfdz_1, g_1, dgdz_1
                    ! Now find 'transformed' basis functions such that in layer 1, O2 = A_2*et + B_2*ft + gt
                    ! (ie layer 1 soln written in terms of layer 2 coeffs A_2, B_2)
                    call sub_xformsoln(e_1, f_1, g_1, dedz_1, dfdz_1, dgdz_1, ls_a, ls_b, ls_c, ls_d, ls_e, ls_f,&
                    e, f, g, dedz, dfdz, dgdz)
                end if
            case default
                STOP
        end select

    END SUBROUTINE sub_calcfg_l12

    ! ****************************************************************************************************************************** !
    !   *****************************************************************
    !   *****************************************************************
    !------------------------------------------------------------------------------------


    SUBROUTINE sub_calcfg_l1(z, reac1, reac2, Dtemp, ktemp, e, dedz, f, dfdz, g, dgdz)
        ! Basis functions for solutes, case z <= zbio
        !
        ! reac1, reac2        - mol./mol S released per organic carbon C
        !
        ! General solution for solute S is given by
        !  S(z) = A * e(z) + B * f(z) + g(z)

        real z, reac1, reac2, Dtemp, ktemp                                                          ! in from SUBROUTINE before
        real,INTENT(inout)::  e, dedz, f, dfdz, g, dgdz             ! out

        ! local variables
        real b1, pfac, PhiI1, PhiII1, PhiIII1, PhiI2, PhiII2, PhiIII2
        real ea11z, eb11z, ea12z, eb12z


        e = 1.0
        dedz = 0.0
        b1=w/(Dtemp + const_real_nullsmall)
        f=exp(z*b1)
        dfdz = b1*exp(z*b1)

        pfac = 1                    ! in fact, already has (1-por)/por

        PhiI1 = -pfac*k1*(reac1)*A11/(Dtemp*aa11**2-w*aa11-ktemp + const_real_nullsmall)
        PhiII1  = pfac*k1*(reac1)*A11/(Dtemp*bb11**2-w*bb11-ktemp + const_real_nullsmall)
        PhiIII1 =-pfac*k1*(reac1)*dum_POC1_conc_swi/(Dtemp*bb11**2-w*bb11-ktemp + const_real_nullsmall)
        PhiI2   =-pfac*k2*(reac2)*A12/(Dtemp*aa12**2-w*aa12-ktemp + const_real_nullsmall)
        PhiII2  = pfac*k2*(reac2)*A12/(Dtemp*bb12**2-w*bb12-ktemp + const_real_nullsmall)
        PhiIII2 =-pfac*k2*(reac2)*dum_POC2_conc_swi/(Dtemp*bb12**2-w*bb12-ktemp + const_real_nullsmall)


        ea11z = exp(aa11*z)
        eb11z = exp(bb11*z)
        ea12z = exp(aa12*z)
        eb12z = exp(bb12*z)

        !        print*, 'PhiI1, PhiII1, PhiIII1', PhiI1, PhiII1, PhiIII1
        !        print*, 'PhiI2, PhiII2, PhiIII2', PhiI2, PhiII2, PhiIII2
        !        print*, 'ea11z, eb11z, ea12z, eb12z', ea11z, eb11z, ea12z, eb12z

        g =  PhiI1*ea11z + PhiII1*eb11z + PhiIII1*eb11z + &
        PhiI2*ea12z + PhiII2*eb12z + PhiIII2*eb12z

        dgdz = PhiI1*aa11*ea11z + PhiII1*bb11*eb11z + PhiIII1*bb11*eb11z + &
        PhiI2*aa12*ea12z + PhiII2*bb12*eb12z + PhiIII2*bb12*eb12z

    !            print*, 'INPUT sub_calcfg_l1', z, reac1, reac2, Dtemp, ktemp
    !                print*, 'IN  sub_calcfg_l1 g', g
    !                print*, 'IN  sub_calcfg_l1 dgdz', dgdz

    end SUBROUTINE sub_calcfg_l1

    !------------------------------------------------------------------------------------
    !   *****************************************************************
    !   *****************************************************************
    !------------------------------------------------------------------------------------

    SUBROUTINE sub_calcfg_l2(z, reac1, reac2, Dtemp, ktemp, e, dedz, f, dfdz, g, dgdz)
        ! Basis functions for solutes, case z > zbio
        ! reac1, reac2        - mol/mol S released per organic carbon C
        !
        ! General solution for solute S is given by
        !  S(z) = A * e(z) + B * f(z) + g(z)

        real z, reac1, reac2, Dtemp, ktemp                                                          ! in from SUBROUTINE before
        real,INTENT(inout)::  e, dedz, f, dfdz, g, dgdz             ! out

        ! local variables
        real b2, pfac, PhiI1, PhiI2

        e = 1.0
        dedz = 0.0
        b2 = w/(Dtemp + const_real_nullsmall)
        f=exp(z*b2)
        dfdz = b2*exp(z*b2)

        !pfac=1./por;   ! assume org matter already *(1-por)
        pfac = 1            !in fact, already has (1-por)/por



        PhiI1 = -pfac*k1*(reac1)*A21/(Dtemp*aa21**2-w*aa21-ktemp + const_real_nullsmall)
        PhiI2 = -pfac*k2*(reac2)*A22/(Dtemp*aa22**2-w*aa22-ktemp + const_real_nullsmall)

        g = PhiI1*exp(aa21*z) + PhiI2*exp(aa22*z)
        dgdz = PhiI1*aa21*exp(aa21*z) + PhiI2*aa22*exp(aa22*z)

    !            print*, 'IN  sub_calcfg_l2 g', g
    !            print*, 'IN  sub_calcfg_l1 dgdz', dgdz

    end SUBROUTINE sub_calcfg_l2


    !------------------------------------------------------------------------------------
    !   *****************************************************************
    !   *****************************************************************
    !------------------------------------------------------------------------------------

    SUBROUTINE sub_prepfg_l12(reac1, reac2, ktemp, zU, zL, D1, D2, ls_a, ls_b, ls_c, ls_d, ls_e, ls_f, ltype)
        real reac1, reac2, ktemp, zU, zL, D1, D2
        real e_zbio_l1, dedz_zbio_l1, f_zbio_l1, dfdz_zbio_l1, g_zbio_l1, dgdz_zbio_l1
        real e_zbio_l2, dedz_zbio_l2, f_zbio_l2, dfdz_zbio_l2, g_zbio_l2, dgdz_zbio_l2
        real ls_a, ls_b, ls_c, ls_d, ls_e, ls_f

        INTEGER,INTENT(inout):: ltype

        if(zL <= zbio)then  ! wholly within bioturbated layer
            ltype = 1
        elseif(zU >= zbio) then ! wholly within non-bioturbated layer
            ltype = 2
        else             ! crossing boundary - sort out solution matching at zbio
            !            print*, 'IN  CROSS BOUNDARY CASE '
            ltype = 3
            call sub_calcfg_l1(zbio, reac1, reac2, D1, ktemp, e_zbio_l1, dedz_zbio_l1, f_zbio_l1, dfdz_zbio_l1, g_zbio_l1, dgdz_zbio_l1)
            call sub_calcfg_l2(zbio, reac1, reac2, D2, ktemp, e_zbio_l2, dedz_zbio_l2, f_zbio_l2, dfdz_zbio_l2, g_zbio_l2, dgdz_zbio_l2)

            ! match solutions at zbio - continuous concentration and flux
            !            print*, 'in prepfg: '
            !            print*, 'e_zbio_l1, dedz_zbio_l1', e_zbio_l1, dedz_zbio_l1
            !            print*, 'f_zbio_l1, dfdz_zbio_l1', f_zbio_l1, dfdz_zbio_l1
            !            print*, 'g_zbio_l1, dgdz_zbio_l1', g_zbio_l1, dgdz_zbio_l1
            !            print*, 'e_zbio_l2, dedz_zbio_l2', e_zbio_l2, dedz_zbio_l2
            !            print*, 'f_zbio_l2, dfdz_zbio_l2', f_zbio_l2, dfdz_zbio_l2
            !            print*, 'g_zbio_l2, dgdz_zbio_l2', g_zbio_l2, dgdz_zbio_l2

            call sub_matchsoln(e_zbio_l1, f_zbio_l1, g_zbio_l1, D1*dedz_zbio_l1, D1*dfdz_zbio_l1, D1*dgdz_zbio_l1, &
            e_zbio_l2, f_zbio_l2, g_zbio_l2, D2*dedz_zbio_l2, D2*dfdz_zbio_l2, D2*dgdz_zbio_l2, &
            0.0, 0.0, ls_a, ls_b, ls_c, ls_d, ls_e, ls_f)
        !           print*, 'in sub_prepfg_l12 AFTER sub_matchsoln:  ls_a, ls_b, ls_c, ls_d, ls_e, ls_f', ls_a, ls_b, ls_c, ls_d, ls_e, ls_f
        end if

    END SUBROUTINE sub_prepfg_l12


    !------------------------------------------------------------------------------------
    !   *****************************************************************
    !   *****************************************************************

    !                               Utility subroutines for PO4/Fe-bound P

    !   *****************************************************************
    !   *****************************************************************
    !------------------------------------------------------------------------------------

    SUBROUTINE sub_calcfg_l12_PO4_M(z, reac1P, reac2P, dum_ktempP, dum_QtempP, &
    dum_D1P, dum_D2P, dum_alphaP, dum_mat_C, dum_vec_D, dum_ltype, &
    dum_ktempM, dum_QtempM, dum_D1M, dum_D2M, dum_alphaM, e_P, dedz_P, f_P, dfdz_P, g_P, dgdz_P, &
    p_P, dpdz_P, q_P, dqdz_P, e_M, dedz_M, f_M, dfdz_M, g_M, dgdz_M, p_M, dpdz_M, q_M, dqdz_M)
            ! calculate solution basis functions, for layer which may cross bioturbation boundary
            !
            ! reac1, reac2        - mol/mol S released per organic carbon C
            !
            ! General solution for solute S is given by
            !  S(z) = A * e(z) + B * f(z) + g(z)
            !
            ! Where e,f,g are generated by matching solutions across bioturbation boundary (if necessary)
            ! Solution properties (matching etc) are input in ls
            ! On input, ls should contain fields generated by prepfg_l12

        real,intent(in):: z, reac1P, reac2P, dum_ktempP, dum_QtempP, dum_alphaP, dum_D1P, dum_D2P
        real,intent(in):: dum_ktempM, dum_QtempM, dum_alphaM, dum_D1M, dum_D2M
        INTEGER, INTENT(in):: dum_ltype
        real, dimension (4, 4), INTENT(in) :: dum_mat_C
        real, dimension (1:4), INTENT(in) ::  dum_vec_D

        ! ODE solutions (E, F, P, Q) and the particulat integral (G) and their derivatives
        real,intent(inout):: e_P, dedz_P, f_P, dfdz_P, g_P, dgdz_P, p_P, dpdz_P, q_P, dqdz_P
        real,intent(inout):: e_M, dedz_M, f_M, dfdz_M, g_M, dgdz_M, p_M, dpdz_M, q_M, dqdz_M

        ! local variables
        real loc_a1_P, loc_b1_P, loc_a1_M, loc_b1_M, loc_a2_M, loc_a2_P, loc_b2_P
        real, dimension (1:6) :: loc_Phi1_P, loc_Phi2_P
        real loc_e_P_1, loc_dedz_P_1, loc_f_P_1, loc_dfdz_P_1, loc_g_P_1, loc_dgdz_P_1
        real loc_p_P_1, loc_dpdz_P_1, loc_q_P_1, loc_dqdz_P_1
        real loc_e_M_1, loc_dedz_M_1, loc_f_M_1, loc_dfdz_M_1, loc_g_M_1, loc_dgdz_M_1
        real loc_p_M_1, loc_dpdz_M_1, loc_q_M_1, loc_dqdz_M_1
        ! save the ODE solutions in vectors to make calculation easier (DH?: however, is this faster?)
        real, dimension (1:4) :: loc_EFPQ_P, loc_dEFPQdz_P, loc_EFPQ_M, loc_dEFPQdz_M
        ! the transformed ODE solutions coming from sub_xformsoln_PO4_M
        real, dimension (1:4) :: loc_EFPQ_P_t, loc_dEFPQdz_P_t, loc_EFPQ_M_t, loc_dEFPQdz_M_t

        loc_Phi1_P = (/ 0, 0, 0, 0, 0, 0 /)
        loc_Phi2_P = (/ 0, 0, 0, 0, 0, 0 /)

        select case (dum_ltype)
            case (1)    ! bioturbated
                if(dum_alphaP==0)then   ! oxic layer -> call PO4 first
                    call sub_calcfg_l1_PO4(z, reac1P, reac2P, dum_D1P, dum_ktempP, dum_QtempP, 0.0, 0.0, dum_alphaP, &
                    e_P, dedz_P, f_P, dfdz_P, g_P, dgdz_P, p_P, dpdz_P, q_P, dqdz_P, loc_a1_P, loc_b1_P, loc_Phi1_P)
                    call sub_calcfg_l1_M(z, dum_D1M, dum_ktempM, dum_QtempM, loc_a1_P, loc_b1_P, loc_Phi1_P, dum_alphaM, &
                    e_M, dedz_M, f_M, dfdz_M, g_M, dgdz_M, p_M, dpdz_M, q_M, dqdz_M, loc_a1_M, loc_b1_M)
                else        ! anoxic layer -> call M first
                    call sub_calcfg_l1_M(z, dum_D1M, dum_ktempM, dum_QtempM, 0.0, 0.0, loc_Phi1_P, dum_alphaM, &
                    e_M, dedz_M, f_M, dfdz_M, g_M, dgdz_M, p_M, dpdz_M, q_M, dqdz_M, loc_a1_M, loc_b1_M)
                    call sub_calcfg_l1_PO4(z, reac1P, reac2P, dum_D1P, dum_ktempP, dum_QtempP, loc_a1_M, loc_b1_M, dum_alphaP, &
                    e_P, dedz_P, f_P, dfdz_P, g_P, dgdz_P, p_P, dpdz_P, q_P, dqdz_P, loc_a1_P, loc_b1_P, loc_Phi1_P)
                end if

            case (2)    ! not bioturbated
                if(dum_alphaP==0)then   ! oxic layer -> call PO4 first
                    call sub_calcfg_l2_PO4(z, reac1P, reac2P, dum_D2P, dum_ktempP, dum_QtempP, 0.0, dum_alphaP, &
                    e_P, dedz_P, f_P, dfdz_P, g_P, dgdz_P, p_P, dpdz_P, q_P, dqdz_P, loc_a2_P, loc_b2_P, loc_Phi2_P)
                    call sub_calcfg_l2_M(z, dum_ktempM, dum_QtempM, loc_a2_P, loc_b2_P, loc_Phi2_P, dum_alphaM, &
                    e_M, dedz_M, f_M, dfdz_M, g_M, dgdz_M, p_M, dpdz_M, q_M, dqdz_M, loc_a2_M)
                else    ! anoxic layer -> call M first
                    call sub_calcfg_l2_M(z, dum_ktempM, dum_QtempM, 0.0, 0.0, loc_Phi2_P, dum_alphaM, &
                    e_M, dedz_M, f_M, dfdz_M, g_M, dgdz_M, p_M, dpdz_M, q_M, dqdz_M, loc_a2_M)
                    call sub_calcfg_l2_PO4(z, reac1P, reac2P, dum_D2P, dum_ktempP, dum_QtempP, loc_a2_M, dum_alphaP, &
                    e_P, dedz_P, f_P, dfdz_P, g_P, dgdz_P, p_P, dpdz_P, q_P, dqdz_P, loc_a2_P, loc_b2_P, loc_Phi2_P)
                end if

            case (3)    ! crossing boundary
                IF(z > zbio) THEN      ! not bioturbated region
                    if(dum_alphaP==0)then   ! oxic layer -> call PO4 first NOTE: BUT DECIDE VIA ALPHA_M NOT WITH <= ZOX!!! DOESN't WORK FOR BOUNDARY ZOX
                        call sub_calcfg_l2_PO4(z, reac1P, reac2P, dum_D2P, dum_ktempP, dum_QtempP, 0.0, dum_alphaP, &
                        e_P, dedz_P, f_P, dfdz_P, g_P, dgdz_P, p_P, dpdz_P, q_P, dqdz_P, loc_a2_P, loc_b2_P, loc_Phi2_P)
                        call sub_calcfg_l2_M(z, dum_ktempM, dum_QtempM, loc_a2_P, loc_b2_P, loc_Phi2_P, dum_alphaM, &
                        e_M, dedz_M, f_M, dfdz_M, g_M, dgdz_M, p_M, dpdz_M, q_M, dqdz_M, loc_a2_M)
                    else ! anoxic layer -> call M first
                        call sub_calcfg_l2_M(z, dum_ktempM, dum_QtempM, 0.0, 0.0, loc_Phi2_P, dum_alphaM, &
                        e_M, dedz_M, f_M, dfdz_M, g_M, dgdz_M, p_M, dpdz_M, q_M, dqdz_M, loc_a2_M)
                        call sub_calcfg_l2_PO4(z, reac1P, reac2P, dum_D2P, dum_ktempP, dum_QtempP, loc_a2_M, dum_alphaP, &
                        e_P, dedz_P, f_P, dfdz_P, g_P, dgdz_P, p_P, dpdz_P, q_P, dqdz_P, loc_a2_P, loc_b2_P, loc_Phi2_P)
                    end if ! (dum_alphaP==0)
                ELSE    ! bioturbated region z <= zbio
                    if(dum_alphaP==0)then   ! oxic layer -> call PO4 first
                        ! CASE 1 & 2: LAYER 1: have 4 int. const.
                        call sub_calcfg_l1_PO4(z, reac1P, reac2P, dum_D1P, dum_ktempP, dum_QtempP, 0.0, 0.0, dum_alphaP, &
                        loc_e_P_1, loc_dedz_P_1, loc_f_P_1, loc_dfdz_P_1, loc_g_P_1, loc_dgdz_P_1, loc_p_P_1, loc_dpdz_P_1, &
                        loc_q_P_1, loc_dqdz_P_1, loc_a1_P, loc_b1_P, loc_Phi1_P)
                        call sub_calcfg_l1_M(z, dum_D1M, dum_ktempM, dum_QtempM, loc_a1_P, loc_b1_P, loc_Phi1_P, dum_alphaM, &
                        loc_e_M_1, loc_dedz_M_1, loc_f_M_1, loc_dfdz_M_1, loc_g_M_1, loc_dgdz_M_1, loc_p_M_1, loc_dpdz_M_1, &
                        loc_q_M_1, loc_dqdz_M_1, loc_a1_M, loc_b1_M)
                        ! DH: FOR CASE 2: DON'T HAVE D FROM LAYER 2
                    else    ! anoxic layer -> call M first
                        ! DH: CASE 1: LAYER 2: have 4 int. const.
                        call sub_calcfg_l1_M(z, dum_D1M, dum_ktempM, dum_QtempM, 0.0, 0.0, loc_Phi1_P, dum_alphaM, &
                        loc_e_M_1, loc_dedz_M_1, loc_f_M_1, loc_dfdz_M_1, loc_g_M_1, loc_dgdz_M_1, loc_p_M_1, loc_dpdz_M_1, &
                        loc_q_M_1, loc_dqdz_M_1, loc_a1_M, loc_b1_M)
                        call sub_calcfg_l1_PO4(z, reac1P, reac2P, dum_D1P, dum_ktempP, dum_QtempP, loc_a1_M, loc_b1_M, dum_alphaP, &
                        loc_e_P_1, loc_dedz_P_1, loc_f_P_1, loc_dfdz_P_1, loc_g_P_1, loc_dgdz_P_1, loc_p_P_1, loc_dpdz_P_1, &
                        loc_q_P_1, loc_dqdz_P_1, loc_a1_P, loc_b1_P, loc_Phi1_P)
                    end if  ! (dum_alphaP==0)

                    ! Now find 'transformed' basis functions such that in layer 1,
                    ! O2 = A_2*et + B_2*ft + gt  (ie layer 1 solution written in terms of layer 2 coeffs A_2, B_2)

                    loc_EFPQ_P = (/ loc_e_P_1, loc_f_P_1, loc_p_P_1, loc_q_P_1 /)
                    loc_dEFPQdz_P = (/ loc_dedz_P_1, loc_dfdz_P_1, loc_dpdz_P_1, loc_dqdz_P_1 /)
                    loc_EFPQ_M = (/ loc_e_M_1, loc_f_M_1, loc_p_M_1, loc_q_M_1 /)
                    loc_dEFPQdz_M = (/loc_dedz_M_1, loc_dfdz_M_1, loc_dpdz_M_1, loc_dqdz_M_1/)

                    call sub_xformsoln_PO4_M(loc_EFPQ_P, loc_EFPQ_M, loc_dEFPQdz_P, loc_dEFPQdz_M, &
                    loc_g_P_1, loc_g_M_1,loc_dgdz_P_1, loc_dgdz_M_1, dum_mat_C, dum_vec_D, &
                    loc_EFPQ_P_t, g_P, loc_dEFPQdz_P_t, dgdz_P, loc_EFPQ_M_t, &
                    g_M, loc_dEFPQdz_M_t, dgdz_M)

                    ! WHEN lTYPE=3 - DEAL WITH ONE VARIABLE SHORT FROM LAYER BELOW
                    ! FOR CASE 1 & 2: deal with missing values from layer below
                    if(zox .LE. zbio)then   ! CASE 1: no F from layer below - DH: no e_M & f_M as well !? but 0 anyway at the moment
                        e_P = loc_EFPQ_P_t(1)
                        f_P = loc_EFPQ_P_t(2)
                        p_P = loc_EFPQ_P_t(3)
                        q_P = loc_q_P_1

                        dedz_P = loc_dEFPQdz_P_t(1)
                        dfdz_P = loc_dEFPQdz_P_t(2)
                        dpdz_P = loc_dEFPQdz_P_t(3)
                        dqdz_P = loc_dqdz_P_1

                        e_M = loc_EFPQ_M_t(1)
                        f_M = loc_EFPQ_M_t(2)
                        p_M = loc_EFPQ_M_t(3)
                        q_M = loc_q_M_1

                        dedz_M = loc_dEFPQdz_M_t(1)
                        dfdz_M = loc_dEFPQdz_M_t(2)
                        dpdz_M = loc_dEFPQdz_M_t(3)
                        dqdz_M = loc_dqdz_M_1

                    else            ! CASE 2: no Q from layer below - DH: no p_P as well !?
                        e_P = loc_EFPQ_P_t(1)
                        f_P = loc_EFPQ_P_t(2)
                        p_P = loc_p_P_1             ! DH:  was = EFPQ_P_t(3);
                        q_P = loc_q_P_1

                        dedz_P = loc_dEFPQdz_P_t(1)
                        dfdz_P = loc_dEFPQdz_P_t(2)
                        dpdz_P = loc_dpdz_P_1       !DH: was = dEFPQ_P_t(3);
                        dqdz_P = loc_dqdz_P_1

                        e_M = loc_EFPQ_M_t(1)
                        f_M = loc_EFPQ_M_t(2)
                        p_M = loc_EFPQ_M_t(3)
                        q_M = loc_q_M_1

                        dedz_M = loc_dEFPQdz_M_t(1)
                        dfdz_M = loc_dEFPQdz_M_t(2)
                        dpdz_M = loc_dEFPQdz_M_t(3)
                        dqdz_M = loc_dqdz_M_1

                    end if ! (zox .LE. zbio)

                END IF  ! (z > zbio)
            case default
                print*, ' unrecognized ltype in  sub_calcfg_l2_PO4 ', dum_ltype
                STOP
        end select

    !        print*, ' '
    !        print*, 'IN  calcfg_l12_PO4 --------'
    !        print*, ' z = ', z
    !        print*,'e_P, dedz_P ', char(9), e_P, dedz_P
    !        print*,'f_P, dfdz_P', char(9), f_P, dfdz_P
    !        print*,'g_P, dgdz_P', char(9), g_P, dgdz_P
    !        print*,'p_P, dpdz_P', char(9), p_P, dpdz_P
    !        print*,' q_P, dqdz_P ', char(9), q_P, dqdz_P
    !        print*, ' '
    !        print*,'e_M, dedz_M ', char(9), e_M, dedz_M
    !        print*,'f_M, dfdz_M', char(9), f_M, dfdz_M
    !        print*,'g_M, dgdz_M', char(9), g_M, dgdz_M
    !        print*,'p_M, dpdz_M', char(9), p_M, dpdz_M
    !        print*,' q_M, dqdz_M ', char(9), q_M, dqdz_M


    END SUBROUTINE sub_calcfg_l12_PO4_M


    ! ****************************************************************************************************************************** !
    !   *****************************************************************
    !   *****************************************************************
    !------------------------------------------------------------------------------------



    SUBROUTINE sub_calcfg_l1_PO4(z, reac1, reac2, Dtemp, ktemp, Qtemp, a1_M, b1_M, alpha, e, dedz, f, dfdz, g, dgdz, &
    p, dpdz, q, dqdz, a1, b1, Phi1)
        ! Basis functions for solutes, case z <= zbio
        !
        ! reac1, reac2        - mol./mol S released per organic carbon C
        ! depend1,   depend2 coming from other species
        !
        ! General solution for solute S is given by
        !  S(z) = A * e(z) + B * f(z) + g(z)
        ! and for dependent species
        !       S(z) = A .* e(z) + B .* f(z) + C .* p(z) +  D.* q(z) + g(z)

        real z, reac1, reac2, Dtemp, ktemp, Qtemp, a1_M, b1_M, alpha              ! in from SUBROUTINE before
        real,INTENT(inout)::  e, dedz, f, dfdz, g, dgdz, p, dpdz, q, dqdz, a1, b1 ! out
        real, dimension (1:6), intent (inout) :: Phi1                               ! out

        ! local variables
        real pfac
        real ea11z, eb11z, ea12z, eb12z


        a1=(w-sqrt(w**2+4.0*Dtemp*ktemp))/(2.0*Dtemp + const_real_nullsmall)
        e=exp(z*a1)
        dedz = a1*exp(z*a1)

        b1=(w+sqrt(w**2+4.0*Dtemp*ktemp))/(2.0*Dtemp + const_real_nullsmall)
        f=exp(z*b1)
        dfdz = b1*exp(z*b1);

        pfac = 1                    ! in fact, already has (1-por)/por

        ! NOW to OM reaction terms!
        ! save all Phis in one variable to pass back
        Phi1(1) = -pfac*k1*(reac1)*A11/(Dtemp*aa11**2-w*aa11-ktemp + const_real_nullsmall)
        Phi1(2)  = pfac*k1*(reac1)*A11/(Dtemp*bb11**2-w*bb11-ktemp + const_real_nullsmall)
        Phi1(3) =-pfac*k1*(reac1)*dum_POC1_conc_swi/(Dtemp*bb11**2-w*bb11-ktemp + const_real_nullsmall)
        Phi1(4)   =-pfac*k2*(reac2)*A12/(Dtemp*aa12**2-w*aa12-ktemp + const_real_nullsmall)
        Phi1(5)  = pfac*k2*(reac2)*A12/(Dtemp*bb12**2-w*bb12-ktemp + const_real_nullsmall)
        Phi1(6) =-pfac*k2*(reac2)*dum_POC2_conc_swi/(Dtemp*bb12**2-w*bb12-ktemp + const_real_nullsmall)


        ea11z = exp(aa11*z);
        eb11z = exp(bb11*z);
        ea12z = exp(aa12*z);
        eb12z = exp(bb12*z);

        if(ktemp==0)then        !CHECK: actually no need as ktemp always <> 0
            g =  Phi1(1)*ea11z + Phi1(2)*eb11z + Phi1(3)*eb11z + &
            Phi1(4)*ea12z + Phi1(5)*eb12z + Phi1(6)*eb12z
        else
            g =  Phi1(1)*ea11z + Phi1(2)*eb11z + Phi1(3)*eb11z + &
            Phi1(4)*ea12z + Phi1(5)*eb12z + Phi1(6)*eb12z + Qtemp/(ktemp + const_real_nullsmall)   ! here problem if ktemp=0
        end if

        dgdz = Phi1(1)*aa11*ea11z + Phi1(2)*bb11*eb11z + Phi1(3)*bb11*eb11z + &
        Phi1(4)*aa12*ea12z + Phi1(5)*bb12*eb12z + Phi1(6)*bb12*eb12z

        if(alpha == 0)then      ! was z<=res.zox PO4 is independent of M (no info in alpha)
            p = 0
            dpdz = 0
            q = 0
            dqdz = 0
        else                    ! PO4 is dependent on M
            p = -alpha/(Dtemp*a1_M**2-w*a1_M-ktemp + const_real_nullsmall)*exp(z*a1_M)
            dpdz = a1_M*p
            q = -alpha/(Dtemp*b1_M**2-w*b1_M-ktemp + const_real_nullsmall)*exp(z*b1_M)
            dqdz = b1_M*q
        end if

    !            print*, 'INPUT calcfg_l1', z, reac1, reac2, Dtemp, ktemp
    !            print*, 'IN  calcfg_l1 g', g
    !            print*, 'IN  calcfg_l1 dgdz', dgdz

    end SUBROUTINE sub_calcfg_l1_PO4

    !------------------------------------------------------------------------------------
    !   *****************************************************************
    !   *****************************************************************
    !------------------------------------------------------------------------------------

    SUBROUTINE sub_calcfg_l2_PO4(z, reac1, reac2, Dtemp, ktemp, Qtemp, a2_M, alpha, &
    e, dedz, f, dfdz, g, dgdz, p, dpdz, q, dqdz, a2, b2, Phi2)
        ! Basis functions for solutes, case z > zbio
        ! reac1, reac2        - mol/mol S released per organic carbon C
        !
        ! General solution for solute S is given by
        !  S(z) = A * e(z) + B * f(z) + g(z)

        ! in from SUBROUTINE before
        real z, reac1, reac2, Dtemp, ktemp, Qtemp , a2_M, alpha
        ! out
        real,INTENT(inout)::  e, dedz, f, dfdz, g, dgdz, p, dpdz, q, dqdz, a2, b2
        real, dimension (1:2), intent (inout) :: Phi2

        ! local variables
        real pfac


        a2=(w-sqrt(w**2+4.0*Dtemp*ktemp))/(2.0*Dtemp + const_real_nullsmall)
        e=exp(z*a2)
        dedz = a2*exp(z*a2)

        b2=(w+sqrt(w**2+4.0*Dtemp*ktemp))/(2.0*Dtemp + const_real_nullsmall)
        f=exp(z*b2)
        dfdz = b2*exp(z*b2)

        !pfac=1./por;   ! assume org matter already *(1-por)
        pfac = 1            !in fact, already has (1-por)/por

        Phi2(1) = -pfac*k1*(reac1)*A21/(Dtemp*aa21**2-w*aa21-ktemp + const_real_nullsmall)
        Phi2(2) = -pfac*k2*(reac2)*A22/(Dtemp*aa22**2-w*aa22-ktemp + const_real_nullsmall)


        if(ktemp==0)then            ! CHECK: think no need for this as always ktemp <> 0
            g = Phi2(1)*exp(aa21*z) + Phi2(2)*exp(aa22*z)
        else
            g = Phi2(1)*exp(aa21*z) + Phi2(2)*exp(aa22*z) + Qtemp/(ktemp + const_real_nullsmall)
        end if
        dgdz = Phi2(1)*aa21*exp(aa21*z) + Phi2(2)*aa22*exp(aa22*z)

        if(alpha==0)then            ! was z<=res.zox PO4 is independent of M (no info in alpha)
            p = 0
            dpdz = 0
            q = 0
            dqdz = 0
        else                        ! PO4 is dependent on M
            p = -alpha/(Dtemp*a2_M**2-w*a2_M-ktemp + const_real_nullsmall)*exp(a2_M*z)
            dpdz = a2_M*p
            q=0
            dqdz=0
        end if

    !            print*, 'IN  calcfg_l2 g', g
    !            print*, 'IN  calcfg_l1 dgdz', dgdz

    end SUBROUTINE sub_calcfg_l2_PO4


    !------------------------------------------------------------------------------------
    !   *****************************************************************
    !   *****************************************************************
    !------------------------------------------------------------------------------------

    SUBROUTINE sub_calcfg_l1_M(z, Dtemp, ktemp, Qtemp, a1_P, b1_P, Phi1_P, alpha, e, dedz, f, dfdz, g, dgdz, p, dpdz, q, dqdz, c1, d1)
        ! Basis functions for solutes, case z <= zbio
        !
        ! reac1, reac2        - mol./mol S released per organic carbon C
        ! depend1,   depend2 coming from other species
        !
        ! General solution for solute S is given by
        !  S(z) = A * e(z) + B * f(z) + g(z)
        ! and for dependent species
        !       S(z) = A .* e(z) + B .* f(z) + C .* p(z) +  D.* q(z) + g(z)

        real z, Dtemp, ktemp, Qtemp, a1_P, b1_P, alpha                                ! in from SUBROUTINE before
        real, dimension (1:6), intent (in) :: Phi1_P                                            ! in from SUBROUTINE before
        real,INTENT(inout)::  e, dedz, f, dfdz, g, dgdz, p, dpdz, q, dqdz, c1, d1             ! out

        ! local variables


        c1=(w-sqrt(w**2+4.0*Dtemp*ktemp))/(2.0*Dtemp + const_real_nullsmall)
        p=exp(z*c1)
        dpdz = c1*exp(z*c1)

        d1=(w+sqrt(w**2+4.0*Dtemp*ktemp))/(2.0*Dtemp + const_real_nullsmall)
        q=exp(z*d1)
        dqdz = d1*exp(z*d1);

        if(alpha .NE. 0)then      ! oxic layer: was z<=res.zox BUT problems at boundary. M is dependent on PO4
            c1=0
            d1=0
            e = -alpha/(Dtemp*a1_P**2-w*a1_P-ktemp + const_real_nullsmall)*exp(z*a1_P)
            dedz = a1_P*e
            f = -alpha/(Dtemp*b1_P**2-w*b1_P-ktemp + const_real_nullsmall)*exp(z*b1_P)
            dfdz = b1_P*f
            g = -alpha*(Phi1_P(1)/(Dtemp*aa11**2-w*aa11-ktemp + const_real_nullsmall)*exp(z*aa11) + Phi1_P(2)/(Dtemp*bb11**2-w*bb11-ktemp + const_real_nullsmall)*exp(z*bb11) + &
            Phi1_P(3)/(Dtemp*bb11**2-w*bb11-ktemp + const_real_nullsmall)*exp(z*bb11) + &
            Phi1_P(4)/(Dtemp*aa12**2-w*aa12-ktemp + const_real_nullsmall)*exp(z*aa12) + Phi1_P(5)/(Dtemp*bb12**2-w*bb12-ktemp + const_real_nullsmall)*exp(z*bb12) + &
            Phi1_P(6)/(Dtemp*bb12**2-w*bb12-ktemp + const_real_nullsmall)*exp(z*bb12))
            dgdz = -alpha*(Phi1_P(1)/(Dtemp*aa11**2-w*aa11-ktemp + const_real_nullsmall)*exp(z*aa11)*aa11 + Phi1_P(2)/(Dtemp*bb11**2-w*bb11-ktemp + const_real_nullsmall) &
            *exp(z*bb11)*bb11 + Phi1_P(3)/(Dtemp*bb11**2-w*bb11-ktemp + const_real_nullsmall)*exp(z*bb11)*bb11 + &
            Phi1_P(4)/(Dtemp*aa12**2-w*aa12-ktemp + const_real_nullsmall)*exp(z*aa12)*aa12 + Phi1_P(5)/(Dtemp*bb12**2-w*bb12-ktemp + const_real_nullsmall)*exp(z*bb12)*bb12 + &
            Phi1_P(6)/(Dtemp*bb12**2-w*bb12-ktemp + const_real_nullsmall)*exp(z*bb12)*bb12)

        else                    ! anoxic layer: M is independent of PO4 (no value in alpha!)
            g = Qtemp/(ktemp + const_real_nullsmall)
            dgdz = 0
            e = 0
            dedz = 0
            f = 0
            dfdz = 0
        end if

    !            print*, 'INPUT sub_calcfg_l1_M', z, reac1, reac2, Dtemp, ktemp
    !            print*, 'IN  calcfg_l1 g', g
    !            print*, 'IN  calcfg_l1 dgdz', dgdz

    end SUBROUTINE sub_calcfg_l1_M


    !------------------------------------------------------------------------------------
    !   *****************************************************************
    !   *****************************************************************
    !------------------------------------------------------------------------------------

    SUBROUTINE sub_calcfg_l2_M(z, ktemp, Qtemp, a2_P, b2_P, Phi2_P, alpha, e, dedz, f, dfdz, g, dgdz, p, dpdz, q, dqdz, c2)
        ! Basis functions for solutes, case z > zbio
        ! reac1, reac2        - mol/mol S released per organic carbon C
        !
        ! General solution for solute S is given by
        !  S(z) = A * e(z) + B * f(z) + g(z)

        real z, ktemp, Qtemp, a2_P, b2_P, alpha                                           ! in from SUBROUTINE before
        real,INTENT(inout)::  e, dedz, f, dfdz, g, dgdz, p, dpdz, q, dqdz, c2             ! out
        real, dimension (1:2), intent (in) :: Phi2_P                                        ! out

        ! local variables

        c2=0

        if(alpha .NE. 0)then            ! M is dependent of PO4, was z<=res.zox
            e=alpha/(w*a2_P + const_real_nullsmall)*exp(z*a2_P)
            dedz = a2_P*e
            f=alpha/(w*b2_P + const_real_nullsmall)*exp(z*b2_P)
            dfdz = b2_P*f
            p = 1                       ! DH CHECK/TODO: integration constant just C
            dpdz = 0
            q=0
            dqdz=0
            g = alpha/(w + const_real_nullsmall)*(Phi2_P(1)/(aa21 + const_real_nullsmall)*exp(aa21*z) + &
            Phi2_P(2)/(aa22 + const_real_nullsmall)*exp(aa22*z))
            dgdz = alpha/(w + const_real_nullsmall)*(Phi2_P(1)*exp(aa21*z) + &
            Phi2_P(2)*exp(aa22*z))
        else                        ! M is independent of PO4 - z > res.zox
            c2=-ktemp/(w + const_real_nullsmall)
            p=exp(c2*z)
            dpdz = c2*exp(c2*z)
            q=0
            dqdz=0
            g = Qtemp/(ktemp + const_real_nullsmall)
            dgdz = 0
            e=0
            dedz=0
            f=0
            dfdz=0

        end if

    end SUBROUTINE sub_calcfg_l2_M

    !------------------------------------------------------------------------------------
    !   *****************************************************************
    !   *****************************************************************
    !------------------------------------------------------------------------------------

    SUBROUTINE sub_prepfg_l12_PO4_M(reac1, reac2, ktempP, QtempP, zU, zL, D1P, D2P, alphaP, &
    ktempM, QtempM, D1M, D2M, alphaM, loc_mat_C, loc_vec_D, ltype)
        real reac1, reac2, ktempP, QtempP, zU, zL, D1P, D2P, alphaP, ktempM, QtempM, D1M, D2M, alphaM
        real, dimension (4, 4), intent (inout) :: loc_mat_C
        real, dimension (1:4), intent (inout) ::  loc_vec_D
        INTEGER,INTENT(inout):: ltype

        ! local variables
        real e_zbio_l1_P, dedz_zbio_l1_P, f_zbio_l1_P, dfdz_zbio_l1_P, g_zbio_l1_P, dgdz_zbio_l1_P
        real p_zbio_l1_P, dpdz_zbio_l1_P, q_zbio_l1_P, dqdz_zbio_l1_P, a1_P, b1_P
        real e_zbio_l2_P, dedz_zbio_l2_P, f_zbio_l2_P, dfdz_zbio_l2_P, g_zbio_l2_P, dgdz_zbio_l2_P
        real p_zbio_l2_P, dpdz_zbio_l2_P, q_zbio_l2_P, dqdz_zbio_l2_P, a2_P, b2_P
        real e_zbio_l1_M, dedz_zbio_l1_M, f_zbio_l1_M, dfdz_zbio_l1_M, g_zbio_l1_M, dgdz_zbio_l1_M
        real p_zbio_l1_M, dpdz_zbio_l1_M, q_zbio_l1_M, dqdz_zbio_l1_M, a1_M, b1_M
        real e_zbio_l2_M, dedz_zbio_l2_M, f_zbio_l2_M, dfdz_zbio_l2_M, g_zbio_l2_M, dgdz_zbio_l2_M
        real p_zbio_l2_M, dpdz_zbio_l2_M, q_zbio_l2_M, dqdz_zbio_l2_M, a2_M
        real Vb, Fb
        real, dimension (1:6) :: Phi1_P, Phi2_P
        real, dimension (1:4,1:4) :: mat_X, mat_Y
        real, dimension (1:4) ::  vec_Z
        integer :: loc_dim

        Phi1_P = (/ 0, 0, 0, 0, 0, 0 /)
        Phi2_P = (/ 0, 0, 0, 0, 0, 0 /)

        if(zL <= zbio)then  ! wholly within bioturbated layer
            ltype = 1
        elseif(zU >= zbio) then ! wholly within non-bioturbated layer
            ltype = 2
        else             ! crossing boundary - sort out solution matching at zbio
            !            print*, 'IN  CROSS BOUNDARY CASE '
            ltype = 3

            if(zL <= zox)then       ! oxic layer -> call PO4 first
                call sub_calcfg_l1_PO4(zbio, reac1, reac2, D1P, ktempP, QtempP, 0.0, 0.0, alphaP, &
                e_zbio_l1_P, dedz_zbio_l1_P, f_zbio_l1_P, dfdz_zbio_l1_P, g_zbio_l1_P, &
                dgdz_zbio_l1_P, p_zbio_l1_P, dpdz_zbio_l1_P, q_zbio_l1_P, dqdz_zbio_l1_P, a1_P, b1_P, Phi1_P)
                call sub_calcfg_l2_PO4(zbio, reac1, reac2, D2P, ktempP, QtempP, 0.0, alphaP, &
                e_zbio_l2_P, dedz_zbio_l2_P, f_zbio_l2_P, dfdz_zbio_l2_P, g_zbio_l2_P, &
                dgdz_zbio_l2_P, p_zbio_l2_P, dpdz_zbio_l2_P, q_zbio_l2_P, dqdz_zbio_l2_P, a2_P, b2_P, Phi2_P)
                call sub_calcfg_l1_M(zbio, D1M, ktempM, QtempM, a1_P, b1_P, Phi1_P, alphaM, &
                e_zbio_l1_M, dedz_zbio_l1_M, f_zbio_l1_M, dfdz_zbio_l1_M, g_zbio_l1_M, dgdz_zbio_l1_M, &
                p_zbio_l1_M, dpdz_zbio_l1_M, q_zbio_l1_M, dqdz_zbio_l1_M, a1_M, b1_M)
                call sub_calcfg_l2_M(zbio, ktempM, QtempM, a2_P, b2_P, Phi2_P, alphaM, &
                e_zbio_l2_M, dedz_zbio_l2_M, f_zbio_l2_M, dfdz_zbio_l2_M, g_zbio_l2_M, dgdz_zbio_l2_M, &
                p_zbio_l2_M, dpdz_zbio_l2_M, q_zbio_l2_M, dqdz_zbio_l2_M, a2_M)

            else                ! anoxic layer -> call M first
                call sub_calcfg_l1_M(zbio, D1M, ktempM, QtempM, 0.0, 0.0, Phi1_P, alphaM, &
                e_zbio_l1_M, dedz_zbio_l1_M, f_zbio_l1_M, dfdz_zbio_l1_M, g_zbio_l1_M, dgdz_zbio_l1_M, &
                p_zbio_l1_M, dpdz_zbio_l1_M, q_zbio_l1_M, dqdz_zbio_l1_M, a1_M, b1_M)
                call sub_calcfg_l2_M(zbio, ktempM, QtempM, 0.0, 0.0, Phi2_P, alphaM, &
                e_zbio_l2_M, dedz_zbio_l2_M, f_zbio_l2_M, dfdz_zbio_l2_M, g_zbio_l2_M, dgdz_zbio_l2_M, &
                p_zbio_l2_M, dpdz_zbio_l2_M, q_zbio_l2_M, dqdz_zbio_l2_M, a2_M)
                call sub_calcfg_l1_PO4(zbio, reac1, reac2, D1P, ktempP, QtempP, a1_M, b1_M, alphaP, &
                e_zbio_l1_P, dedz_zbio_l1_P, f_zbio_l1_P, dfdz_zbio_l1_P, g_zbio_l1_P, dgdz_zbio_l1_P, &
                p_zbio_l1_P, dpdz_zbio_l1_P, q_zbio_l1_P, dqdz_zbio_l1_P, a1_P, b1_P, Phi1_P)
                call sub_calcfg_l2_PO4(zbio, reac1, reac2, D2P, ktempP, QtempP, a2_M, alphaP, &
                e_zbio_l2_P, dedz_zbio_l2_P, f_zbio_l2_P, dfdz_zbio_l2_P, g_zbio_l2_P, dgdz_zbio_l2_P, &
                p_zbio_l2_P, dpdz_zbio_l2_P, q_zbio_l2_P, dqdz_zbio_l2_P, a2_P, b2_P, Phi2_P)
            end if

            ! match solutions at zbio - continuous concentration and flux
            ! organize the data in matrices, and use the intrinsic fortran fct.
            ! DH: Maybe more efficient when written out !?

            !  |x1        |   | A_l |      | y1        | | A_r|    |z1|    always PO4 continuity
            !  |    .     |   | B_l |      |    .      | | B_r|    |z2|    always PO4 flux
            !  |      .   |   | C_l |   =  |      .    | | C_r|  + |z3|    always M continuity
            !  |       x16|   | D_l |      |        y16| | D_r|    |z4|    SD always M _diffusive_ flux  = 0 (cf org C)

            ! discontinuity constants
            Vb = 0
            Fb = 0

            ! weird FORTRAN matrices makes the transpose necessary
            ! matrix mat_X
            mat_X = transpose(reshape((/ e_zbio_l1_P, f_zbio_l1_P, p_zbio_l1_P, q_zbio_l1_P, &
            D1P*dedz_zbio_l1_P, D1P*dfdz_zbio_l1_P, D1P*dpdz_zbio_l1_P, D1P*dqdz_zbio_l1_P, &
            e_zbio_l1_M, f_zbio_l1_M, p_zbio_l1_M, q_zbio_l1_M, &
            D1M*dedz_zbio_l1_M, D1M*dfdz_zbio_l1_M, D1M*dpdz_zbio_l1_M, D1M*dqdz_zbio_l1_M/), shape(mat_X)))
            !            data (mat_X(1,loc_i), loc_i=1,4) /  e_zbio_l1_P, f_zbio_l1_P, p_zbio_l1_P, q_zbio_l1_P /
            !            data (mat_X(2,loc_i), loc_i=1,4) /  D1P*dedz_zbio_l1_P, D1P*dfdz_zbio_l1_P, D1P*dpdz_zbio_l1_P, D1P*dqdz_zbio_l1_P /
            !            data (mat_X(3,loc_i), loc_i=1,4) /  e_zbio_l1_M, f_zbio_l1_M, p_zbio_l1_M, q_zbio_l1_M /
            !            data (mat_X(4,loc_i), loc_i=1,4) /  D1M*dedz_zbio_l1_M, D1M*dfdz_zbio_l1_M, D1M*dpdz_zbio_l1_M, D1M*dqdz_zbio_l1_M /

            ! matrix mat_Y
            mat_Y = transpose(reshape((/ e_zbio_l2_P, f_zbio_l2_P, p_zbio_l2_P, q_zbio_l2_P, &
            D2P*dedz_zbio_l2_P, D2P*dfdz_zbio_l2_P, D2P*dpdz_zbio_l2_P, D2P*dqdz_zbio_l2_P, &
            e_zbio_l2_M, f_zbio_l2_M, p_zbio_l2_M, q_zbio_l2_M, &
            D2M*dedz_zbio_l2_M, D2M*dfdz_zbio_l2_M, D2M*dpdz_zbio_l2_M, D2M*dqdz_zbio_l2_M/), shape(mat_Y)))
            !            data (mat_Y(1,loc_i), loc_i=1,4) /  e_zbio_l2_P, f_zbio_l2_P, p_zbio_l2_P, q_zbio_l2_P /
            !            data (mat_Y(2,loc_i), loc_i=1,4) /  D2P*dedz_zbio_l2_P, D2P*dfdz_zbio_l2_P, D2P*dpdz_zbio_l2_P, D2P*dqdz_zbio_l2_P /
            !            data (mat_Y(3,loc_i), loc_i=1,4) /  e_zbio_l2_M, f_zbio_l2_M, p_zbio_l2_M, q_zbio_l2_M /
            !            data (mat_Y(4,loc_i), loc_i=1,4) /  D2M*dedz_zbio_l2_M, D2M*dfdz_zbio_l2_M, D2M*dpdz_zbio_l2_M, D2M*dqdz_zbio_l2_M /

            vec_Z = (/ g_zbio_l2_P-g_zbio_l1_P + Vb, &
            D2P*dgdz_zbio_l2_P - D1P*dgdz_zbio_l1_P + Fb - w*Vb, &
            g_zbio_l2_M-g_zbio_l1_M + Vb, &
            D2M*dgdz_zbio_l2_M - D1M*dgdz_zbio_l1_M + Fb - w*Vb /)


            !            201         format (6f12.6)
            !                        print*,'mat_X '
            !                        do loc_i=1,4
            !                            write (*,201) (mat_X(loc_i,loc_j),loc_j=1,4)
            !                        end do
            !                        print*,'mat_Y '
            !                        do loc_i=1,4
            !                            write (*,201) (mat_Y(loc_i,loc_j),loc_j=1,4)
            !                        end do
            !                        print*,'vec_Z ', char(9), vec_Z
            !                        print*,' '

            loc_dim = 4
            call sub_matchsoln_PO4_M(mat_X, mat_Y, vec_Z, loc_dim, loc_mat_C, loc_vec_D)
        end if

    END SUBROUTINE sub_prepfg_l12_PO4_M


    SUBROUTINE sub_matchsoln(E_l, F_l, G_l, dEdx_l, dFdx_l, dGdx_l, &
    E_r, F_r, G_r, dEdx_r, dFdx_r, dGdx_r, &
    Vb, Db, ls_a, ls_b, ls_c, ls_d, ls_e, ls_f)

        real, INTENT(in):: E_l, F_l, G_l, dEdx_l, dFdx_l, dGdx_l
        real, INTENT(in):: E_r, F_r, G_r, dEdx_r, dFdx_r, dGdx_r, Vb, Db
        real, INTENT(inout):: ls_a, ls_b, ls_c, ls_d, ls_e, ls_f
        real:: alden, blden

        ! Match two solutions at a boundary:
        ! 'left' solution   y_l(x) = A_l*E_l(x) + B_l*F_l(x) + G_l(x)
        ! 'right' solution  y_r(x) = A_r*E_r(x) + B_r*F_l(x) + G_r(x)
        !
        ! (Dis)continuity conditions at boundary:
        !                   y_r(xb)    = y_l(xb)     + Vb
        !                   dydx_r(xb) = dydx_l(xb)  + Db
        !
        ! Find a,b,c,d,e,f such that:
        !         | A_l |   =  | a  b | | A_r|  + |e|
        !         | B_l |      | c  d | | B_r|    |f|

        alden = dFdx_l*E_l - F_l*dEdx_l + const_real_nullsmall
        ls_a     = (dFdx_l*E_r - F_l*dEdx_r)/alden
        ls_b     = (dFdx_l*F_r - F_l*dFdx_r)/alden
        ls_e     = (F_l*(dGdx_l - dGdx_r + Db) + dFdx_l*(-G_l + G_r - Vb))/alden

        blden = dEdx_l*F_l - E_l*dFdx_l + const_real_nullsmall
        ls_c     = (dEdx_l*E_r - E_l*dEdx_r)/blden
        ls_d     = (dEdx_l*F_r - E_l*dFdx_r)/blden;
        ls_f     = (E_l*(dGdx_l - dGdx_r + Db) + dEdx_l*(-G_l+G_r - Vb))/blden

    END SUBROUTINE sub_matchsoln


    ! ****************************************************************************************************************************** !
    !   *****************************************************************
    !   *****************************************************************
    !------------------------------------------------------------------------------------


    SUBROUTINE sub_xformsoln(E, F, G, dEdx, dFdx, dGdx, ls_a , ls_b , ls_c , ls_d , ls_e ,ls_f, Etr, Ftr, Gtr, dEtdx, dFtdx, dGtdx)

        real, INTENT(in):: E, F, G, dEdx, dFdx, dGdx, ls_a , ls_b , ls_c , ls_d , ls_e ,ls_f
        real, INTENT(inout):: Etr, Ftr, Gtr, dEtdx, dFtdx, dGtdx

        ! Find 'transformed' soln such that in layer l,
        !    y_l = A_r*et + B_r*ft + gt
        ! (ie l soln written in terms of r solution coefficents A_r, B_r)

        Etr      = ls_a*E    + ls_c*F
        dEtdx   = ls_a*dEdx + ls_c*dFdx
        Ftr      = ls_b*E    + ls_d*F
        dFtdx   = ls_b*dEdx + ls_d*dFdx
        Gtr      = G       + ls_e*E      + ls_f*F
        dGtdx   = dGdx    + ls_e*dEdx   + ls_f*dFdx

    !            print*, 'IN sub_xformsoln E, F, G, dEdx, dFdx, dGdx, ls_a , ls_b , ls_c , ls_d , ls_e ,ls_f', &
    !                    & E, F, G, dEdx, dFdx, dGdx, ls_a , ls_b , ls_c , ls_d , ls_e ,ls_f
    !            print*, 'OUT sub_xformsoln Etr, Ftr, Gtr, dEtdx, dFtdx, dGtdx', Etr, Ft, Gt, dEtdx, dFtdx, dGtdx

    END SUBROUTINE sub_xformsoln

    ! ****************************************************************************************************************************** !
    !   *****************************************************************
    !   *****************************************************************
    !------------------------------------------------------------------------------------

    SUBROUTINE sub_solve2eqn(a, b, c, d, e, f, x, y)

        ! Find soln of
        ! | a    b |  |x|   = | e |
        ! | c    d |  |y|     | f |

        real,INTENT(IN)::a, b, c, d, e, f
        real,INTENT(OUT)::x, y

        ! local variable
        real::det

        det = a*d-b*c+const_real_nullsmall
        if(det == 0.0) then
            print*,'det too small ', det
        end if
        x    =  (e*d-b*f)/det
        y    =  (a*f-e*c)/det

    !    print*,'a, b, c, d, e, f', a, b, c, d, e, f
    !    print*,'det', det
    !    print*,'x', x
    !    print*,'y', y

    END SUBROUTINE sub_solve2eqn

    ! ****************************************************************************************************************************** !
    !   *****************************************************************
    !   *****************************************************************
    !------------------------------------------------------------------------------------

    SUBROUTINE sub_matchsoln_PO4_M(dum_mat_X, dum_mat_Y, dum_vec_Z, dum_dim, loc_mat_C, loc_vec_D)

        integer dum_dim                      ! dimension of the matrices
        real, dimension (1:dum_dim,1:dum_dim), intent (in) :: dum_mat_X, dum_mat_Y
        real, dimension (1:dum_dim), intent (in) ::  dum_vec_Z
        real, dimension (1:dum_dim,1:dum_dim), intent (inout) :: loc_mat_C
        real, dimension (1:dum_dim), intent (inout) ::  loc_vec_D

        ! local variables
        real, dimension (1:dum_dim,1:dum_dim) :: loc_mat_X, loc_inv_mat_X
        !        integer :: loc_i, loc_j

        ! Match four solutions at a boundary:
        !  for PO4
        ! 'left' solution   y_l(z) = A_l*E_l(z) + B_l*F_l(z) + C_l*P_l(z) + D_l*Q_l(z) + G_l(z)
        ! 'right' solution  y_r(z) = A_r*E_r(z) + B_r*F_r(z) + C_r*P_r(z) + D_r*Q_r(z) + G_r(z)
        !
        !  and the same for M
        !
        ! (Dis)continuity conditions at boundary:
        !                   y_r(xb)    = y_l(xb)     + Vb
        !                   dydx_r(xb) = dydx_l(xb)  + Db

        !         | A_l |         | A_r|
        !         | B_l |         | B_r|
        !     X   | C_l |   =  Y  | C_r|  + Z
        !         | D_l |         | D_r|

        !
        ! Find C and D such that:
        !         | A_l |         | A_r|
        !         | B_l |         | B_r|
        !         | C_l |   =  C  | C_r|  +  D
        !         | D_l |         | D_r|

        ! save matrix locally, as the original matrix loc_mat_X(4,4) will be destroyed during the calculation
        loc_mat_X = dum_mat_X
        ! calculate loc_mat_X^{-1}
        call sub_inverse(loc_mat_X,loc_inv_mat_X,dum_dim)
        loc_mat_C = matmul(loc_inv_mat_X, dum_mat_Y)
        loc_vec_D = matmul(loc_inv_mat_X, dum_vec_Z)


    END SUBROUTINE sub_matchsoln_PO4_M

    ! ****************************************************************************************************************************** !
    !   *****************************************************************
    !   *****************************************************************
    !------------------------------------------------------------------------------------

    SUBROUTINE sub_xformsoln_PO4_M(dum_EFPQ_P, dum_EFPQ_M, dum_dEFPQdz_P, dum_dEFPQdz_M, &
    dum_g_P, dum_g_M, dum_dgdz_P, dum_dgdz_M, dum_mat_C, dum_vec_D, &
    loc_EFPQ_P_t, loc_G_P_t, loc_dEFPQ_P_t, loc_dG_P_t, loc_EFPQ_M_t, loc_G_M_t, &
    loc_dEFPQ_M_t, loc_dG_M_t)

        real, dimension (1:4), intent(in) :: dum_EFPQ_P, dum_dEFPQdz_P, dum_EFPQ_M, dum_dEFPQdz_M
        real,INTENT(IN):: dum_g_P, dum_g_M, dum_dgdz_P, dum_dgdz_M
        real, dimension (4, 4), INTENT(in) :: dum_mat_C
        real, dimension (1:4), INTENT(in) ::  dum_vec_D

        ! output variables
        real, dimension (1:4), intent(inout) :: loc_EFPQ_P_t, loc_dEFPQ_P_t, loc_EFPQ_M_t, loc_dEFPQ_M_t
        real,intent(inout):: loc_G_P_t, loc_dG_P_t, loc_G_M_t, loc_dG_M_t

        ! Find 'transformed' soln such that in layer l,
        !    y_l = A_r*et + B_r*ft + gt
        ! (ie l soln written in terms of r solution coefficents A_r, B_r)
        !
        ! here save values in matrices - as this saves a lot of code

        loc_EFPQ_P_t = matmul(transpose(dum_mat_C), dum_EFPQ_P)     ! DH TODO: check multiplication with transpose, especially if vector*vector
        loc_dEFPQ_P_t = matmul(transpose(dum_mat_C), dum_dEFPQdz_P)
        loc_G_P_t = dot_product(dum_vec_D, dum_EFPQ_P)+dum_g_P
        loc_dG_P_t = dot_product(dum_vec_D,dum_dEFPQdz_P)+dum_dgdz_P

        loc_EFPQ_M_t = matmul(transpose(dum_mat_C), dum_EFPQ_M)
        loc_dEFPQ_M_t = matmul(transpose(dum_mat_C), dum_dEFPQdz_M)
        loc_G_M_t = dot_product(dum_vec_D, dum_EFPQ_M) + dum_g_M
        loc_dG_M_t = dot_product(dum_vec_D,dum_dEFPQdz_M) + dum_dgdz_M

    !        print*,' '
    !        print*, 'IN sub_xformsoln_PO4_M '
    !        print*, 'loc_EFPQ_P_t', loc_EFPQ_P_t
    !        print*, 'loc_dEFPQ_P_t', loc_dEFPQ_P_t
    !        print*, 'loc_G_P_t', loc_G_P_t
    !        print*, 'loc_dG_P_t', loc_dG_P_t
    !        print*,' '
    !        print*, 'loc_EFPQ_M_t', loc_EFPQ_M_t
    !        print*, 'loc_dEFPQ_M_t', loc_dEFPQ_M_t
    !        print*, 'loc_G_M_t', loc_G_M_t
    !        print*, 'loc_dG_M_t', loc_dG_M_t

    END SUBROUTINE sub_xformsoln_PO4_M


    ! ****************************************************************************************************************************** !
    !   *****************************************************************
    !   *****************************************************************
    !------------------------------------------------------------------------------------

    SUBROUTINE sub_solve2eqn_PO4_M(dum_mat_X, dum_vec_Y, dum_A, dum_B, dum_C, dum_dim)

        ! Find solution of
        ! | x1  .  . x4|  |A|     | y1 |
        ! |     .      |  |B|     | y2 |
        ! |       .    |  |C|   = | y3 |
        ! | .       x16|  |D|     | y4 |
        integer, intent(in) :: dum_dim                             ! dim of input matrix to inverse
        real, dimension (1:dum_dim,1:dum_dim), intent(in) :: dum_mat_X
        real, dimension (1:dum_dim), intent(in) ::  dum_vec_Y
        real,INTENT(INOUT) :: dum_A, dum_B, dum_C

        ! local variable
        real, dimension (1:dum_dim,1:dum_dim) :: loc_mat_X, loc_inv_mat_X
        real, dimension (1:dum_dim) :: loc_vec_Z
        ! save matrix locally, as the original matrix dum_mat_X(4,4) will be destroyed during the calculation
        loc_mat_X = dum_mat_X
        ! calculate loc_mat_X^{-1}
        call sub_inverse(loc_mat_X,loc_inv_mat_X,dum_dim)
        loc_vec_Z = matmul(loc_inv_mat_X, dum_vec_Y)

        dum_A = loc_vec_Z(1)
        dum_B = loc_vec_Z(2)
        dum_C = loc_vec_Z(3)

    !        print*,' IN SOLVE2EQN_PO4_M '
    !        print*,'dum_A ', dum_A
    !        print*,'dum_B', dum_B
    !        print*,'dum_C', dum_C

    END SUBROUTINE sub_solve2eqn_PO4_M

    ! ****************************************************************************************************************************** !
    !   *****************************************************************
    !   *****************************************************************
    !------------------------------------------------------------------------------------


    subroutine sub_inverse(a, c,n)
        !============================================================
        ! Inverse matrix
        ! Method: Based on Doolittle LU factorization for Ax=b
        ! Alex G. December 2009
        !-----------------------------------------------------------
        ! input ...
        ! a(n,n) - array of coefficients for matrix A
        ! n      - dimension
        ! output ...
        ! c(n,n) - inverse matrix of A
        ! comments ...
        ! the original matrix a(n,n) will be destroyed
        ! during the calculation
        !===========================================================
        implicit none
        integer n
        real, dimension (1:n,1:n) :: a, c

        !        ! local variables
        !        double precision a(n,n), c(n,n)
        real, dimension (1:n,1:n) :: L, U
        real, dimension (1:n) :: b, d, x
        real coeff
        !        double precision a(n,n), c(n,n)
        !        double precision L(n,n), U(n,n), b(n), d(n), x(n)
        !        double precision coeff
        integer i, j, k

        !        ! DH: Cast input real to double precision
        !        a = dble(dum_a)
        !        c = dble(dum_c)

        ! step 0: initialization for matrices L and U and b
        ! Fortran 90/95 aloows such operations on matrices
        L=0.0
        U=0.0
        b=0.0

        ! step 1: forward elimination
        do k=1, n-1
            do i=k+1,n
                coeff=a(i,k)/(a(k,k) + const_real_nullsmall)
                L(i,k) = coeff
                do j=k+1,n
                    a(i,j) = a(i,j)-coeff*a(k,j)
                end do
            end do
        end do

        ! Step 2: prepare L and U matrices
        ! L matrix is a matrix of the elimination coefficient
        ! + the diagonal elements are 1.0
        do i=1,n
            L(i,i) = 1.0
        end do
        ! U matrix is the upper triangular part of A
        do j=1,n
            do i=1,j
                U(i,j) = a(i,j)
            end do
        end do

        ! Step 3: compute columns of the inverse matrix C
        do k=1,n
            b(k)=1.0
            d(1) = b(1)
            ! Step 3a: Solve Ld=b using the forward substitution
            do i=2,n
                d(i)=b(i)
                do j=1,i-1
                    d(i) = d(i) - L(i,j)*d(j)
                end do
            end do
            ! Step 3b: Solve Ux=d using the back substitution
            ! DH: check for division by zero
            !            if(U(n,n) > const_real_nullsmall) then
            x(n)=d(n)/(U(n,n)+const_real_nullsmall)
            !            else
            !                print*,'U(n,n) small ', U(n,n)
            !                x(n)=d(n)/const_real_nullsmall
            !            end if
            do i = n-1,1,-1
                x(i) = d(i)
                do j=n,i+1,-1
                    x(i)=x(i)-U(i,j)*x(j)
                end do
                x(i) = x(i)/(u(i,i) + const_real_nullsmall)
            end do
            ! Step 3c: fill the solutions x(n) into column k of C
            do i=1,n
                c(i,k) = x(i)
            end do
            b(k)=0.0
        end do

    !        ! DH cast back to real
    !        dum_c = real(c)
    end subroutine sub_inverse


    ! ****************************************************************************************************************************** !
    !   *****************************************************************
    !   *****************************************************************
    !------------------------------------------------------------------------------------

    !!!! TODO: better put outside module, in kind of collection of auxiliary functions

    FUNCTION FUN_zbrent(func,x1,x2,tol)

        ! calculate root of func in the interval [x1,x2]

        INTEGER ITMAX
        real FUN_zbrent,tol,x1,x2,func,EPS
        EXTERNAL func
        PARAMETER (ITMAX=100,EPS=3.e-8)
        INTEGER iter
        real a,b,c,d,e,fa,fb,fc,p,q,r,s,tol1,xm

        !    print*,' '
        !    print*,'++++++++++++ START FUN_zbrent ++++++++++++++++ '

        a=x1
        b=x2
        fa=func(a)
        fb=func(b)
        !was      if((fa.gt.0..and.fb.gt.0.).or.(fa.lt.0..and.fb.lt.0.))pause
        !was     *'root must be bracketed for FUN_zbrent'
        IF((fa.gt.0..and.fb.gt.0.).or.(fa.lt.0..and.fb.lt.0.))THEN
            print*,'root must be bracketed for FUN_zbrent'
            STOP
        ELSE
            c=b
            fc=fb
            do 11 iter=1,ITMAX
                if((fb.gt.0..and.fc.gt.0.).or.(fb.lt.0..and.fc.lt.0.))then
                    c=a
                    fc=fa
                    d=b-a
                    e=d
                endif
                if(abs(fc).lt.abs(fb)) then
                    a=b
                    b=c
                    c=a
                    fa=fb
                    fb=fc
                    fc=fa
                endif
                tol1=2.*EPS*abs(b)+0.5*tol
                xm=.5*(c-b)
                if(abs(xm).le.tol1 .or. fb.eq.0.)then
                    FUN_zbrent=b
                    return
                endif
                if(abs(e).ge.tol1 .and. abs(fa).gt.abs(fb)) then
                    s=fb/fa
                    if(a.eq.c) then
                        p=2.*xm*s
                        q=1.-s
                    else
                        q=fa/fc
                        r=fb/fc
                        p=s*(2.*xm*q*(q-r)-(b-a)*(r-1.))
                        q=(q-1.)*(r-1.)*(s-1.)
                    endif
                    if(p.gt.0.) q=-q
                    p=abs(p)
                    if(2.*p .lt. min(3.*xm*q-abs(tol1*q),abs(e*q))) then
                        e=d
                        d=p/q
                    else
                        d=xm
                        e=d
                    endif
                else
                    d=xm
                    e=d
                endif
                a=b
                fa=fb
                if(abs(d) .gt. tol1) then
                    b=b+d
                else
                    b=b+sign(tol1,xm)
                endif
                fb=func(b)
11          continue
        END IF
        ! was      pause 'FUN_zbrent exceeding maximum iterations'
        print*,'FUN_zbrent exceeding maximum iterations'
        FUN_zbrent=b
        STOP

        return

    END FUNCTION FUN_zbrent


    SUBROUTINE sub_huelseetal2016_zTOC_2Diffcoeff(dum_POC1_flux_swi, dum_POC2_flux_swi, dum_sed_pres_fracC)
        ! NOT USED IN CURRENT VERSION
        ! THIS ROUTINE USES A SMALL DIFFUSION COEFFICIENT FOR THE UNBIOTURBATED LAYER TO DEAL WITH NUMERICAL INACCURACIES!
        !   __________________________________________________________
        !
        !   calculate benthic burial/recycling fluxes (see documentation for details!)
        !   __________________________________________________________

        !   organic matter burial 2 fractions

        ! dummy arguments
        real,INTENT(in)::dum_POC1_flux_swi, dum_POC2_flux_swi               ! POC flux at SWI   [mol/(cm2 yr)]
        real,INTENT(inout)::dum_sed_pres_fracC                              ! POC concentrations at zinf

        ! local variables
        !        real loc_POC1_conc_swi, dum_POC2_conc_swi                           ! POC concentration at SWI   [mol/cm3]
        real loc_POC1_conc_zinf, loc_POC2_conc_zinf
        !    real aa11, bb11, aa21, A11, A21, aa12, bb12, aa22, A12, A22
        real dC1dz, C1flx, dC2dz, C2flx, Cflx                             ! Cflx: Sed input flux to upper boundary, per cm^2 water column
        real F_TOC1, F_TOC2, F_TOC                                        ! Flux through lower boundary zinf, per cm^2 water-column


        !                print*,' ------------------ START zTOC 2 diff coeffs---------------------'
        !                print*,' DC2 ', DC2
        !        print*,' sedimentation rate/burial velocity w = ', w

        ! Dom: Use this when comparing with MATLAB, here we use wt% of g -> *1/12
        !        loc_POC1_conc_swi=0.01*dum_POC1_wtpct_swi/12.0*rho_sed              ! %TOC concentration frac1 at SWI (wt%) -> (mol/cm3 bulk phase)
        !        dum_POC2_conc_swi=0.01*dum_POC2_wtpct_swi/12.0*rho_sed              ! %TOC concentration frac2 at SWI (wt%) -> (mol/cm3 bulk phase)
        !        print*, 'loc_POC1_conc_swi', char(9), loc_POC1_conc_swi
        !        print*, 'dum_POC2_conc_swi', char(9), dum_POC2_conc_swi

        aa11 = (w-sqrt(w**2+4*DC1*k1))/(2*DC1)
        bb11 = (w+sqrt(w**2+4*DC1*k1))/(2*DC1)
        aa21 = (w-sqrt(w**2+4*DC2*k1))/(2*DC2)

        !        print*,'DC1, aa11, bb11, aa21 = ', DC1, aa11, bb11, aa21


        ! calculate TOC SWI concentration from flux
        dum_POC1_conc_swi = (dum_POC1_flux_swi*(-aa11*exp(aa11*zbio)+bb11*exp(bb11*zbio)))/(-DC1*bb11*aa11*exp(bb11*zbio) + DC1*bb11*aa11*exp(aa11*zbio) + &
        DC1*bb11*aa11*por*exp(bb11*zbio) - DC1*bb11*aa11*por*exp(aa11*zbio) - w*aa11*exp(aa11*zbio) + w*bb11*exp(bb11*zbio) + &
        w*por*aa11*exp(aa11*zbio) - w*por*bb11*exp(bb11*zbio))
        !        print*,'dum_POC1_conc_swi = ', dum_POC1_conc_swi

        A11=((DC2*aa21-DC1*bb11)*(dum_POC1_conc_swi*exp(bb11*zbio)))/(DC1*(aa11*exp(aa11*zbio)-bb11*exp(bb11*zbio))-DC2*aa21*(exp(aa11*zbio)-exp(bb11*zbio)))
        !        if(exp(aa21*zbio) > const_real_nullsmall) then
        A21=(A11*(exp(aa11*zbio)-exp(bb11*zbio))+dum_POC1_conc_swi*exp(bb11*zbio))/(exp(aa21*zbio)) !+const_real_nullsmall/100000000)
        !        else
        !            A21=(A11*(exp(aa11*zbio)-exp(bb11*zbio))+dum_POC1_conc_swi*exp(bb11*zbio))/const_real_nullsmall
        !            print*,'in small exp(aa21*zbio)', +exp(aa21*zbio)
        !        end if
        aa12 = (w-sqrt(w**2+4*DC1*k2))/(2*DC1)
        bb12 = (w+sqrt(w**2+4*DC1*k2))/(2*DC1)
        aa22 = (w-sqrt(w**2+4*DC2*k2))/(2*DC2)

        ! calculate TOC SWI concentration from flux
        dum_POC2_conc_swi = (dum_POC2_flux_swi*(-aa12*exp(aa12*zbio)+bb12*exp(bb12*zbio)))/(-DC1*bb12*aa12*exp(bb12*zbio) + DC1*bb12*aa12*exp(aa12*zbio) + &
        DC1*bb12*aa12*por*exp(bb12*zbio) - DC1*bb12*aa12*por*exp(aa12*zbio) - w*aa12*exp(aa12*zbio) + w*bb12*exp(bb12*zbio) + &
        w*por*aa12*exp(aa12*zbio) - w*por*bb12*exp(bb12*zbio))
        !        print*,'dum_POC2_conc_swi = ', dum_POC2_conc_swi

        A12=((DC2*aa22-DC1*bb12)*(dum_POC2_conc_swi*exp(bb12*zbio)))/(DC1*(aa12*exp(aa12*zbio)-bb12*exp(bb12*zbio))-DC2*aa22*(exp(aa12*zbio)-exp(bb12*zbio)))
        A22=(A12*(exp(aa12*zbio)-exp(bb12*zbio))+dum_POC2_conc_swi*exp(bb12*zbio))/(exp(aa22*zbio)) !+const_real_nullsmall/100000000)

        !!! no need for this as this is SWI concentration for z0 = 0!
        !!!    ! % Calculate concentration at z0
        !!!    if(z0<=zbio) then
        !!!        C1=A11*(exp(aa11*z0)-exp(bb11*z0))+dum_POC1_conc_swi*exp(bb11*z0)
        !!!        C2=A12*(exp(aa12*z0)-exp(bb12*z0))+dum_POC2_conc_swi*exp(bb12*z0)
        !!!    else
        !!!        C1=A21*exp(aa21*z0)
        !!!        C2=A22*exp(aa22*z0)
        !!!    end if
        !!!    C = C1 + C2
        !!!
        !!!    print*, 'C = C1 + C2 ', C
        !!!    print*, ' '

        ! Cflx: Sed input flux to upper boundary, per cm^2 water column
        if(z0 < zbio) then
            dC1dz =  A11*(aa11*exp(aa11*z0)-bb11*exp(bb11*z0))+dum_POC1_conc_swi*bb11*exp(bb11*z0)
            C1flx = - (1-por)*(-DC1*dC1dz + w*dum_POC1_conc_swi)
            dC2dz =  A12*(aa12*exp(aa12*z0)-bb12*exp(bb12*z0))+dum_POC2_conc_swi*bb12*exp(bb12*z0)
            C2flx = - (1-por)*(-DC1*dC2dz + w*dum_POC2_conc_swi)
        else
            C1flx = - (1-por)*w*dum_POC1_conc_swi
            C2flx = - (1-por)*w*dum_POC2_conc_swi
        end if
        Cflx = C1flx + C2flx

        !        print*, 'Cflx swi', char(9), Cflx
        !        print*, 'C1flx swi', char(9), C1flx
        !        print*, 'C2flx swi', char(9), C2flx


        ! Flux through lower boundary zinf, per cm^2 water-column (Dom 08.02.2018 was -(1-por)*w*A21*exp(aa21*zinf))
        F_TOC1 = (1-por)*w*A21*exp(aa21*zinf)
        F_TOC2 = (1-por)*w*A22*exp(aa22*zinf)
        F_TOC = F_TOC1 + F_TOC2
        !        print*, 'F_TOC1 zinf', char(9), F_TOC1
        !        print*, 'F_TOC2 zinf', char(9), F_TOC2
        !        print*, 'F_TOC zinf', char(9), F_TOC

        ! Concentration at lower boundary zinf
        if(zinf<zbio) then
            loc_POC1_conc_zinf=A11*(exp(aa11*zinf)-exp(bb11*zinf))+dum_POC1_conc_swi*exp(bb11*zinf)
            loc_POC2_conc_zinf=A12*(exp(aa12*zinf)-exp(bb12*zinf))+dum_POC2_conc_swi*exp(bb12*zinf)
        else
            loc_POC1_conc_zinf=A21*exp(aa21*zinf)
            loc_POC2_conc_zinf=A22*exp(aa22*zinf)
        end if

        ! DH: need to give back fraction buried of initially deposited (so fraction of the input values to this subroutine)
        !        print*, 'loc_POC1_conc_zinf ', char(9), loc_POC1_conc_zinf
        !        print*, 'loc_POC2_conc_zinf ', char(9), loc_POC2_conc_zinf

        dum_sed_pres_fracC = (loc_POC1_conc_zinf+loc_POC2_conc_zinf)/(dum_POC1_conc_swi+dum_POC2_conc_swi+const_real_nullsmall)

    !    print*, ' '
    !    print*, 'F_TOC1', char(9), F_TOC1
    !    print*, 'F_TOC2', char(9), F_TOC2
    !    print*, 'F_TOC', char(9), F_TOC


    end SUBROUTINE sub_huelseetal2016_zTOC_2Diffcoeff



    SUBROUTINE sub_huelseetal2016_zTOC_CONCENTRATION(dum_POC1_wtpct_swi, dum_POC2_wtpct_swi, dum_sed_pres_fracC)
        !!!!!!!!!!!!!!!!!!!!!!!!!
        !
        !       OLD APPROACH, USING TOC CONCENTRATION AT SWI
        !
        !!!!!!!!!!!!!!!!!!!!!!!!!
        !   __________________________________________________________
        !
        !   calculate benthic burial/recycling fluxes (see documentation for details!)
        !   __________________________________________________________

        !   organic matter burial 2 fractions

        ! dummy arguments
        real,INTENT(in)::dum_POC1_wtpct_swi, dum_POC2_wtpct_swi             ! POC concentrations at SWI
        real,INTENT(inout)::dum_sed_pres_fracC                              ! POC concentrations at zinf

        ! local variables
        real loc_POC1_conc_zinf, loc_POC2_conc_zinf
        !    real aa11, bb11, aa21, A11, A21, aa12, bb12, aa22, A12, A22
        real dC1dz, C1flx, dC2dz, C2flx, Cflx                             ! Cflx: Sed input flux to upper boundary, per cm^2 water column
        real F_TOC1, F_TOC2, F_TOC                                        ! Flux through lower boundary zinf, per cm^2 water-column


        !        print*,' ------------------ START zTOC ---------------------'
        !        print*,' sedimentation rate/burial velocity w = ', w

        ! initialize BW conentration POC1,2 in mol/cm3   DH WHY here not divided by 12?????
        !        dum_POC1_conc_swi=0.01*dum_POC1_wtpct_swi*rho_sed              ! %TOC concentration frac1 at SWI (wt%) -> (mol/cm3 bulk phase)
        !        dum_POC2_conc_swi=0.01*dum_POC2_wtpct_swi*rho_sed              ! %TOC concentration frac2 at SWI (wt%) -> (mol/cm3 bulk phase)
        ! Dom: Use this when comparing with MATLAB, here we use wt% of g -> *1/12
        dum_POC1_conc_swi=0.01*dum_POC1_wtpct_swi/12.0*rho_sed              ! %TOC concentration frac1 at SWI (wt%) -> (mol/cm3 bulk phase)
        dum_POC2_conc_swi=0.01*dum_POC2_wtpct_swi/12.0*rho_sed              ! %TOC concentration frac2 at SWI (wt%) -> (mol/cm3 bulk phase)
        !        print*, 'dum_POC1_conc_swi', char(9), dum_POC1_conc_swi
        !        print*, 'dum_POC2_conc_swi', char(9), dum_POC2_conc_swi

        aa11 = (w-sqrt(w**2+4*DC1*k1))/(2*DC1)
        bb11 = (w+sqrt(w**2+4*DC1*k1))/(2*DC1)
        aa21 = (-k1/w)
        A11 = -(dum_POC1_conc_swi*bb11*exp(bb11*zbio))/(aa11*exp(aa11*zbio)-bb11*exp(bb11*zbio)+const_real_nullsmall)
        !        if(exp(aa21*zbio) > const_real_nullsmall) then
        A21=(A11*(exp(aa11*zbio)-exp(bb11*zbio))+dum_POC1_conc_swi*exp(bb11*zbio))/(exp(aa21*zbio)+const_real_nullsmall)
        !        else
        !            A21=(A11*(exp(aa11*zbio)-exp(bb11*zbio))+dum_POC1_conc_swi*exp(bb11*zbio))/const_real_nullsmall
        !            print*,'in small exp(aa21*zbio)', +exp(aa21*zbio)
        !        end if
        aa12 = (w-sqrt(w**2+4*DC1*k2))/(2*DC1)
        bb12 = (w+sqrt(w**2+4*DC1*k2))/(2*DC1)
        aa22 = (-k2/w)
        A12=-(dum_POC2_conc_swi*bb12*exp(bb12*zbio))/(aa12*exp(aa12*zbio)-bb12*exp(bb12*zbio)+const_real_nullsmall)
        A22=(A12*(exp(aa12*zbio)-exp(bb12*zbio))+dum_POC2_conc_swi*exp(bb12*zbio))/(exp(aa22*zbio)+const_real_nullsmall)

        !!! no need for this as this is SWI concentration for z0 = 0!
        !!!    ! % Calculate concentration at z0
        !!!    if(z0<=zbio) then
        !!!        C1=A11*(exp(aa11*z0)-exp(bb11*z0))+dum_POC1_conc_swi*exp(bb11*z0)
        !!!        C2=A12*(exp(aa12*z0)-exp(bb12*z0))+dum_POC2_conc_swi*exp(bb12*z0)
        !!!    else
        !!!        C1=A21*exp(aa21*z0)
        !!!        C2=A22*exp(aa22*z0)
        !!!    end if
        !!!    C = C1 + C2
        !!!
        !!!    print*, 'C = C1 + C2 ', C
        !!!    print*, ' '

        ! Cflx: Sed input flux to upper boundary, per cm^2 water column
        if(z0 < zbio) then
            dC1dz =  A11*(aa11*exp(aa11*z0)-bb11*exp(bb11*z0))+dum_POC1_conc_swi*bb11*exp(bb11*z0)
            C1flx = - (1-por)*(-DC1*dC1dz + w*dum_POC1_conc_swi)
            dC2dz =  A12*(aa12*exp(aa12*z0)-bb12*exp(bb12*z0))+dum_POC2_conc_swi*bb12*exp(bb12*z0)
            C2flx = - (1-por)*(-DC1*dC2dz + w*dum_POC2_conc_swi)
        else
            C1flx = - (1-por)*w*dum_POC1_conc_swi
            C2flx = - (1-por)*w*dum_POC2_conc_swi
        end if
        Cflx = C1flx + C2flx

        !        print*, 'Cflx swi', char(9), Cflx
        !        print*, 'C1flx swi', char(9), C1flx
        !        print*, 'C2flx swi', char(9), C2flx


        ! Flux through lower boundary zinf, per cm^2 water-column
        F_TOC1 = -(1-por)*w*A21*exp(aa21*zinf)
        F_TOC2 = -(1-por)*w*A22*exp(aa22*zinf)
        F_TOC = F_TOC1 + F_TOC2
        !        print*, 'F_TOC1 zinf', char(9), F_TOC1
        !        print*, 'F_TOC2 zinf', char(9), F_TOC2
        !        print*, 'F_TOC zinf', char(9), F_TOC

        ! Concentration at lower boundary zinf
        if(zinf<zbio) then
            loc_POC1_conc_zinf=A11*(exp(aa11*zinf)-exp(bb11*zinf))+dum_POC1_conc_swi*exp(bb11*zinf)
            loc_POC2_conc_zinf=A12*(exp(aa12*zinf)-exp(bb12*zinf))+dum_POC2_conc_swi*exp(bb12*zinf)
        else
            loc_POC1_conc_zinf=A21*exp(aa21*zinf)
            loc_POC2_conc_zinf=A22*exp(aa22*zinf)
        end if

        ! DH: need to give back fraction buried of initially deposited (so fraction of the input values to this subroutine)
        !        print*, 'loc_POC1_conc_zinf ', char(9), loc_POC1_conc_zinf
        !        print*, 'loc_POC2_conc_zinf ', char(9), loc_POC2_conc_zinf

        dum_sed_pres_fracC = (loc_POC1_conc_zinf+loc_POC2_conc_zinf)/(dum_POC1_conc_swi+dum_POC2_conc_swi+const_real_nullsmall)

    !    print*, ' '
    !    print*, 'F_TOC1', char(9), F_TOC1
    !    print*, 'F_TOC2', char(9), F_TOC2
    !    print*, 'F_TOC', char(9), F_TOC


    end SUBROUTINE sub_huelseetal2016_zTOC_CONCENTRATION

END MODULE sedgem_box_benthic

