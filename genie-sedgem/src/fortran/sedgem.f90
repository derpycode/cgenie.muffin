
! ******************************************************************************************************************************** !
! TIME-STEP SEDGEM
SUBROUTINE sedgem(          &
     & dum_dts,             &
     & dum_sfxsumsed,       &
     & dum_sfcsumocn,       &
     & dum_sfcsed,          &
     & dum_sfxocn,          &
     & dum_reinit_sfxsumsed &
     & )
  USE sedgem_lib
  USE sedgem_box
  USE sedgem_data
  IMPLICIT NONE
  ! dummy arguments
  REAL,INTENT(in)::dum_dts                                              ! time-step
  real,DIMENSION(n_sed,n_i,n_j),intent(inout)::dum_sfxsumsed            ! sediment rain flux interface array
  real,DIMENSION(n_ocn,n_i,n_j),intent(in)::dum_sfcsumocn               ! ocean composition interface array
  real,DIMENSION(n_sed,n_i,n_j),intent(out)::dum_sfcsed                 ! sediment composition interface array
  real,DIMENSION(n_ocn,n_i,n_j),intent(out)::dum_sfxocn                 ! sediment dissolution flux interface array
  logical::dum_reinit_sfxsumsed                                         ! reinitialize sedimentation array?
  ! local variables
  integer::i,j,l,io,is                                         ! COUNTING AND ARRAY INDEX VARIABLES
  integer::loc_i,loc_tot_i                                     ! array index conversion variables
  real::loc_dtyr                                               ! local time step (in years)
  real::loc_dts                                                ! local time step (in seconds)
  real::loc_tot_A,loc_tot_A_reef,loc_tot_A_muds                                             ! local total area
  logical::loc_flag_save                        ! local flag
  REAL,DIMENSION(n_sed)::loc_fracdecay_sed                     ! local reduction factor for decaying sediment tracers
  real,DIMENSION(n_ocn)::loc_fhydrothermal                     ! local dissolved tracer array for hydrothermal input
  real,DIMENSION(n_ocn)::loc_flowTalteration                   ! local dissolved tracer array for low T alteration sink
  real,DIMENSION(n_i,n_j)::loc_phys_sed_mask_deepsea           ! 
  real::loc_tot,loc_standard                                   ! 
  real::loc_r7Li,loc_r44Ca                                     ! 
  real::loc_87Sr,loc_88Sr
  real::loc_alpha,loc_R,loc_delta                              ! 
  real::loc_fsed                                               ! 
  real,DIMENSION(n_sed,n_i,n_j)::loc_sfxsumsed_OLD                      ! sediment rain flux interface array (COPY)
  real,DIMENSION(n_sed,n_i,n_j)::loc_sed_fsed_OLD                      ! 

  ! *** STORE PREVIOUS ITERATION DATA ***
  sed_fsed_OLD(:,:,:) = sed_fsed(:,:,:) 
  sed_fdis_OLD(:,:,:) = sed_fdis(:,:,:)
  ! copy current (passed) sediemnt flux
  loc_sfxsumsed_OLD(:,:,:) = dum_sfxsumsed(:,:,:)

  ! *** INITIALIZE RESULTS ARRAYS ***
  dum_sfxocn(:,:,:)  = 0.0     ! 
  sed_fdis(:,:,:)    = 0.0     ! 
  sedocn_fnet(:,:,:) = 0.0     !

  ! *** INITIALIZE LOCAL ARRAYS ***
  loc_fhydrothermal(:)           = 0.0
  loc_flowTalteration(:)         = 0.0
  loc_phys_sed_mask_deepsea(:,:) = 0.0

  ! *** CALCULATE SEDGEM TIME STEP ***
  IF (ctrl_misc_debug4) print*,'*** CALCULATE SEDGEM TIME ***'
  ! calculate sediment model time step length
  ! NOTE: convert between time units of BioGeM (years) and GOLDSTEIn (use <tsc> scale factor to convert to seconds)
  loc_dts  = dum_dts
  loc_dtyr = loc_dts/conv_yr_s

  ! *** DECAY RADIOACTIVE TRACERS ***
  ! calculate fractional reduction factors for decaying isotopes
  loc_fracdecay_sed(:) = EXP(-loc_dtyr*const_lambda_sed(:))
  ! decay radioactive tracers
  DO l=1,n_l_sed
     is = conv_iselected_is(l)
     IF (abs(const_lambda_sed(is)).gt.const_real_nullsmall) THEN
        sed_top(is,:,:) = loc_fracdecay_sed(is)*sed_top(is,:,:)
        sed(is,:,:,:)   = loc_fracdecay_sed(is)*sed(is,:,:,:)
     END if
  end do

  ! *** UpDATE SAVE COUNTER ***
  ! update save counter
  sed_time_save = sed_time_save + loc_dtyr
  sed_time      = sed_time + loc_dtyr
  ! test for whether sedcorenv data should be saved
  if (sed_time_save >= (par_sed_age_save_dt - const_real_nullsmall)) then
     loc_flag_save = .true.
     sed_time_save = 0.0
  else
     loc_flag_save = .false.
  endif

  ! *** UPDATE MASKS ***
  IF (ctrl_misc_debug4) print*,'*** UPDATE MASKS ***'
  DO i=1,n_i
     DO j=1,n_j
        ! catch a zero salinity as an indication that there is no valid corresponding ocean grid point
        ! => permanently amend sediment mask
        IF (sed_mask(i,j) .AND. (dum_sfcsumocn(io_S,i,j) < const_real_nullsmall)) then
           sed_mask(i,j)      = .FALSE.
           sed_mask_reef(i,j) = .FALSE.
           sed_mask_muds(i,j) = .FALSE.
           phys_sed(ips_mask_sed,i,j)      = 0.0
           phys_sed(ips_mask_sed_reef,i,j) = 0.0
           phys_sed(ips_mask_sed_muds,i,j) = 0.0
           sed_save_mask(i,j) = .FALSE.
        end IF
     end DO
  end DO
  ! set local deep-sea mask & calculate total area
  loc_phys_sed_mask_deepsea(:,:) = phys_sed(ips_mask_sed,:,:) - phys_sed(ips_mask_sed_reef,:,:) - phys_sed(ips_mask_sed_muds,:,:)
  loc_tot_A      = sum(loc_phys_sed_mask_deepsea(:,:)*phys_sed(ips_A,:,:))
  loc_tot_A_reef = sum(phys_sed(ips_mask_sed_reef,:,:)*phys_sed(ips_A,:,:))
  loc_tot_A_muds = sum(phys_sed(ips_mask_sed_muds,:,:)*phys_sed(ips_A,:,:))

  ! *** CONVERT UNITS ***
  ! convert from global (mol yr-1) units of par_sed_CaCO3burialTOT to (mol cm-2 yr-1) of par_sed_CaCO3burial
  if ((par_sed_CorgburialTOT*(loc_tot_A_muds+loc_tot_A_reef)) > const_real_nullsmall) then
     par_sed_Corgburial   = conv_cm2_m2*par_sed_CorgburialTOT/(loc_tot_A_muds+loc_tot_A_reef)
  end if
  if ((par_sed_CaCO3burialTOT*loc_tot_A_reef) > const_real_nullsmall) then
     par_sed_CaCO3burial  = conv_cm2_m2*par_sed_CaCO3burialTOT/loc_tot_A_reef
  end if
  if ((par_sed_SrCO3recrystTOT*loc_tot_A_reef) > const_real_nullsmall) then
     par_sed_SrCO3recryst = conv_cm2_m2*par_sed_SrCO3recrystTOT/loc_tot_A_reef
  end if

  ! *** UPDATE CARBONATE CHEMSITRY ***
  IF (ctrl_misc_debug4) print*,'*** UPDATE CARBONATE CHEMSITRY ***'
  DO i=1,n_i
     DO j=1,n_j
        IF (sed_mask(i,j)) then
           call sub_calc_carbconst(        &
                & phys_sed(ips_D,i,j),     &
                & dum_sfcsumocn(io_T,i,j), &
                & dum_sfcsumocn(io_S,i,j), &
                & sed_carbconst(:,i,j)     &
                & )
           if (ocn_select(io_Ca) .AND. ocn_select(io_Mg)) then
              call sub_adj_carbconst(          &
                   & dum_sfcsumocn(io_Ca,i,j), &
                   & dum_sfcsumocn(io_Mg,i,j), &
                   & sed_carbconst(:,i,j)      &
                   & )
           end if
           call sub_calc_carb(                &
                & dum_sfcsumocn(io_DIC,i,j),  &
                & dum_sfcsumocn(io_ALK,i,j),  &
                & dum_sfcsumocn(io_Ca,i,j),   &
                & dum_sfcsumocn(io_PO4,i,j),  &
                & dum_sfcsumocn(io_SiO2,i,j), &
                & dum_sfcsumocn(io_B,i,j),    &
                & dum_sfcsumocn(io_SO4,i,j),  &
                & dum_sfcsumocn(io_F,i,j),    &
                & dum_sfcsumocn(io_H2S,i,j),  &
                & dum_sfcsumocn(io_NH4,i,j),  &
                & sed_carbconst(:,i,j),       &
                & sed_carb(:,i,j),            &
                & sed_carbalk(:,i,j)          &
                & )
           ! re-calculate carbonate system isotopic properties
           if (ocn_select(io_DIC_13C)) then
              call sub_calc_carb_r13C(              &
                   & dum_sfcsumocn(io_T,i,j),       &
                   & dum_sfcsumocn(io_DIC,i,j),     &
                   & dum_sfcsumocn(io_DIC_13C,i,j), &
                   & sed_carb(:,i,j),               &
                   & sed_carbisor(:,i,j)            &
                   & )
           end IF
           if (ocn_select(io_DIC_14C)) then
              call sub_calc_carb_r14C(              &
                   & dum_sfcsumocn(io_T,i,j),       &
                   & dum_sfcsumocn(io_DIC,i,j),     &
                   & dum_sfcsumocn(io_DIC_14C,i,j), &
                   & sed_carb(:,i,j),               &
                   & sed_carbisor(:,i,j)            &
                   & )
           end IF
        end IF
     end DO
  end DO

  ! *** EARLY DIAGENESIS PROCESSES ***
  ! NOTE: <dum_sfxsumsed> in units of (mol m-2 per time-step)
  ! NOTE: dum_sfxocn(io,:,:) in units of (mol m-2 s-1)
  DO i=1,n_i
     DO j=1,n_j
        IF (sed_mask(i,j)) THEN
           ! amend sediment rain flux according to prescribed detrital input
           ! NOTE: convert units from (g cm-2 kyr-1) to (mol m-2 (per time-step))
           ! NOTE: add age tracer if selected
           ! NOTE: assume that not both surface-derived flux and prescribed det fluxes are allowed ...
           !       ... but allow prescribed (uniform) sed det flux PLUS spatial burial flux
           ! NOTE: if an opal flux is provided but opal is not selected as a tracer, add to the detrital field
           if (sed_select(is_det)) then
              if (ctrl_sed_det_NOdust) then
                 ! zero det flux, which at this point is assumed to be all pelagic (dust)
                 dum_sfxsumsed(is_det,i,j) = 0.0
              endif
              if (ctrl_sed_Fdet) then
                 dum_sfxsumsed(is_det,i,j) =  &
                      & conv_m2_cm2*conv_det_g_mol*(conv_yr_kyr*loc_dtyr)*par_sed_fdet + &
                      & conv_m2_cm2*conv_det_g_mol*(conv_yr_kyr*loc_dtyr)*sed_Fsed_det(i,j)
                 if (.NOT. sed_select(is_opal) .AND. ctrl_sed_Fopal) then
                    ! special case of pretending that opal is detrital material ...
                    dum_sfxsumsed(is_det,i,j) = dum_sfxsumsed(is_det,i,j) + &
                       & conv_m2_cm2*conv_det_g_mol*(conv_yr_kyr*loc_dtyr)*sed_Fsed_opal(i,j)
                 endif
              else
                 dum_sfxsumsed(is_det,i,j) = dum_sfxsumsed(is_det,i,j) + &
                      & conv_m2_cm2*conv_det_g_mol*(conv_yr_kyr*loc_dtyr)*par_sed_fdet              
              endif
           endif
           if (sed_select(is_det_age)) then
              if (ctrl_sed_Fdet) then
                 dum_sfxsumsed(is_det_age,i,j) = sed_age*conv_m2_cm2*conv_det_g_mol*(conv_yr_kyr*loc_dtyr)*sed_Fsed_det(i,j)
              else
                 dum_sfxsumsed(is_det_age,i,j) = dum_sfxsumsed(is_det_age,i,j) + &
                      & sed_age*conv_m2_cm2*conv_det_g_mol*(conv_yr_kyr*loc_dtyr)*par_sed_fdet          
              endif
           endif
           ! add ash layer (if selected)
           if (sed_select(is_ash)) then
              if (par_sed_ashevent) then
                 dum_sfxsumsed(is_ash,i,j) = dum_sfxsumsed(is_ash,i,j) + &
                      & conv_m2_cm2*conv_det_g_mol*(conv_yr_kyr*loc_dtyr)*par_sed_ashevent_fash
              end if
           endif
           ! replace sediment rain flux according to prescribed CaCO3 input
           if (sed_select(is_CaCO3) .AND. ctrl_sed_Fcaco3) then
              dum_sfxsumsed(is_CaCO3,i,j) = conv_m2_cm2*conv_cal_g_mol*(conv_yr_kyr*loc_dtyr)*sed_Fsed_caco3(i,j)
           endif
           ! replace sediment rain flux according to prescribed opal input
           if (sed_select(is_opal) .AND. ctrl_sed_Fopal) then
              dum_sfxsumsed(is_opal,i,j) = conv_m2_cm2*conv_opal_g_mol*(conv_yr_kyr*loc_dtyr)*sed_Fsed_opal(i,j)
           endif
           ! tag CaCO3 'color'
           if (sed_select(is_CaCO3_red)) dum_sfxsumsed(is_CaCO3_red,i,j) = par_sed_CaCO3_fred*dum_sfxsumsed(is_CaCO3,i,j)
           if (sed_select(is_CaCO3_blue)) dum_sfxsumsed(is_CaCO3_blue,i,j) = par_sed_CaCO3_fblue*dum_sfxsumsed(is_CaCO3,i,j)
           ! account for clay formation
           If (ocn_select(io_Li)) then
              loc_fsed = par_sed_clay_fLi_alpha*dum_sfxsumsed(is_det,i,j)*dum_sfcsumocn(io_Li,i,j)
              dum_sfxsumsed(is_detLi,i,j) = dum_sfxsumsed(is_detLi,i,j) + loc_fsed
              dum_sfxocn(io_Li,i,j) = dum_sfxocn(io_Li,i,j) - loc_fsed/dum_dts
              if (ocn_select(io_Li_7Li)) then
                 loc_standard = const_standards(ocn_type(io_Li_7Li))
                 if (dum_sfcsumocn(io_Li,i,j) > const_real_nullsmall) then
                    loc_r7Li = dum_sfcsumocn(io_Li_7Li,i,j)/dum_sfcsumocn(io_Li,i,j)
                 else
                    loc_r7Li = 0.0
                 end if
                 loc_alpha = 1.0 + par_sed_clay_7Li_epsilon/1000.0
                 loc_R = loc_r7Li/(1.0 - loc_r7Li)
                 loc_fsed = (loc_alpha*loc_R/(1.0 + loc_alpha*loc_R))*loc_fsed
                 dum_sfxsumsed(is_detLi_7Li,i,j) = dum_sfxsumsed(is_detLi_7Li,i,j) + loc_fsed
                 dum_sfxocn(io_Li_7Li,i,j) = dum_sfxocn(io_Li_7Li,i,j) - loc_fsed/dum_dts
              end if
           end if
        end IF
     end DO
  end DO
  ! deselect ash fall
  if (sed_select(is_ash)) then
     if (par_sed_ashevent) par_sed_ashevent = .false.
  endif

  ! *** FORAM TRACERS ***
  ! 
  DO i=1,n_i
     DO j=1,n_j
        IF (sed_mask(i,j)) THEN
           ! add foram tracers
           if (sed_select(is_CaCO3) .AND. sed_select(is_foram_b_13C)) then
              ! calculate 13C/12C fractionation between DIC and CaCO3
              SELECT CASE (opt_sed_foram_b_13C_delta)
              CASE ('NONE')
                 loc_delta = 0.0
              case ('SPERO')
                 ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                 loc_delta = 0.0
                 ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
              end SELECT
              loc_alpha = 1.0 + loc_delta/1000.0
              loc_R = sed_carbisor(ici_HCO3_r13C,i,j)/(1.0 - sed_carbisor(ici_HCO3_r13C,i,j))
              loc_fsed = (loc_alpha*loc_R/(1.0 + loc_alpha*loc_R))*dum_sfxsumsed(is_CaCO3,i,j)
              dum_sfxsumsed(is_foram_b_13C,i,j) = loc_fsed
           end if
        end IF
     end DO
  end DO

  ! *** UPDATE SEDIMENTS ***
  IF (ctrl_misc_debug4) print*,'*** UPDATE SEDIMENTS ***'
  DO i=1,n_i
     DO j=1,n_j
        ! calculate sediment rain flux from sediment->ocean flux (convert units)
        ! NOTE: if the sediment grid point lies outside of the sediment mask, then
        !       dissolve all sediment tracers and set the ocean tracer dissolution flux equal to this
        ! convert units of sedimentation flux
        ! NOTE: <sed_fsed> in units of (mol cm-2)
        ! NOTE: <sedocn_fnet> in units of (mol cm-2)
        ! NOTE: <dum_sfxsumsed> in units of (mol m-2)
        sed_fsed(:,i,j)         = conv_cm2_m2*dum_sfxsumsed(:,i,j)
        loc_sed_fsed_OLD(:,i,j) = conv_cm2_m2*loc_sfxsumsed_OLD(:,i,j)
        IF (sed_mask(i,j)) THEN
           ! call sediment composition update
           ! NOTE: the values in both <sed_fsed> and <ocnsed_fnet> are updated by this routine
           if (sed_mask_reef(i,j)) then
              IF (ctrl_misc_debug3) print*,'> UPDATE SED: reef'
              call sub_update_sed_reef(    &
                   & loc_dtyr,             &
                   & i,j,                  &
                   & phys_sed(ips_D,i,j),  &
                   & dum_sfcsumocn(:,i,j)  &
                   & )
           elseif (sed_mask_muds(i,j)) then
              IF (ctrl_misc_debug3) print*,'> UPDATE SED: mud'
              call sub_update_sed_mud(     &
                   & loc_dtyr,             &
                   & i,j,                  &
                   & phys_sed(ips_D,i,j),  &
                   & dum_sfcsumocn(:,i,j)  &
                   & )
           else
              IF (ctrl_misc_debug3) print*,'> UPDATE SED: (normal)'
              call sub_update_sed(         &
                   & loc_dtyr,             &
                   & i,j,                  &
                   & phys_sed(ips_D,i,j),  &
                   & dum_sfcsumocn(:,i,j)  &
                   & )
           end if
           ! save sedcore environmental conditions
           if (sed_save_mask(i,j) .AND. loc_flag_save .AND. ctrl_data_save_sedcorenv) then
              call sub_sedgem_save_sedcoreenv( &
                   & loc_dtyr,                 &
                   & i,j,                      &
                   & sed_top(:,i,j),           &
                   & sed_fsed(:,i,j),          &
                   & sed_fdis(:,i,j),          &
                   & dum_sfcsumocn(:,i,j),     &
                   & sed_carb(:,i,j)           &
                   & )
           end if
           ! force closed w.r.t. CaCO3
           if (ctrl_force_sed_closedsystem_CaCO3) then
           !
              DO l=1,n_l_sed
                 is = conv_iselected_is(l)
                 if ( &
                      & (sed_dep(is) == is_CaCO3) .OR. &
                      & (sed_type(is) == par_sed_type_CaCO3) .OR. &
                      & (sed_type(sed_dep(is)) == par_sed_type_CaCO3) &
                      & ) then
           ! set dissolution flux (as sediment solids)
           sed_fdis(is,i,j) = loc_sed_fsed_OLD(is,i,j)
           ! calculate equivalent ocean tracer flux
              loc_tot_i = conv_sed_ocn_i(0,is)
              do loc_i=1,loc_tot_i
                 io = conv_sed_ocn_i(loc_i,is)
                 sedocn_fnet(io,i,j) = sedocn_fnet(io,i,j) + conv_sed_ocn(io,is)*sed_fdis(is,i,j)
              end do
                 end if
              end DO           
           end if
           ! force closed w.r.t. opal
           if (ctrl_force_sed_closedsystem_opal) then
           !
              DO l=1,n_l_sed
                 is = conv_iselected_is(l)
                 if ( &
                      & (sed_dep(is) == is_opal) .OR. &
                      & (sed_type(is) == par_sed_type_opal) .OR. &
                      & (sed_type(sed_dep(is)) == par_sed_type_opal) &
                      & ) then
           ! set dissolution flux (as sediment solids)
           sed_fdis(is,i,j) = loc_sed_fsed_OLD(is,i,j)
           ! calculate equivalent ocean tracer flux
              loc_tot_i = conv_sed_ocn_i(0,is)
              do loc_i=1,loc_tot_i
                 io = conv_sed_ocn_i(loc_i,is)
                 sedocn_fnet(io,i,j) = sedocn_fnet(io,i,j) + conv_sed_ocn(io,is)*sed_fdis(is,i,j)
              end do
                 end if
              end DO           
           end if
        else
           ! NO SEDIMENTS HERE
           ! set dissolution flux (as sediment solids)
           ! NOTE: use <sed_fsed> copy in case of burial flux replacement
           sed_fdis(:,i,j) = loc_sed_fsed_OLD(:,i,j)
           ! calculate equivalent ocean tracer flux
           DO l=1,n_l_sed
              is = conv_iselected_is(l)
              loc_tot_i = conv_sed_ocn_i(0,is)
              do loc_i=1,loc_tot_i
                 io = conv_sed_ocn_i(loc_i,is)
                 sedocn_fnet(io,i,j) = sedocn_fnet(io,i,j) + conv_sed_ocn(io,is)*sed_fdis(is,i,j)
              end do
           end DO
        end if
     end do
  end do

  ! *** HYDROTHERMAL / SEAFLOOR ALTERATION ***
  IF (ctrl_misc_debug4) print*,'*** HYDROTHERMAL / SEAFLOOR ALTERATION ***'
  ! calculate hydrothermal source fluxes (inputs)
  ! NOTE: loc_fhydrothermal_input(io) in units of (mol yr-1)
  ! NOTE: dum_sfxocn(io,:,:) in units of (mol m-2 s-1)
  ! CO2
  ! NOTE: radiocarbon dead DIC
  If (ocn_select(io_DIC)) then
     loc_fhydrothermal(io_DIC) = par_sed_hydroip_fDIC
           If (ocn_select(io_DIC_13C)) then
              loc_standard = const_standards(ocn_type(io_DIC_13C))
              loc_fhydrothermal(io_DIC_13C) = fun_calc_isotope_fraction(par_sed_hydroip_fDIC_d13C,loc_standard)*loc_fhydrothermal(io_DIC)
           endif
           If (ocn_select(io_DIC_14C)) then
              loc_fhydrothermal(io_DIC_14C) = 0.0
           endif
  end if
  ! Li
  If (ocn_select(io_Li)) then
     loc_fhydrothermal(io_Li) = par_sed_hydroip_fLi
     if (ocn_select(io_Li_7Li)) then
        loc_tot = loc_fhydrothermal(io_Li)
        loc_standard = const_standards(ocn_type(io_Li_7Li))
        loc_fhydrothermal(io_Li_7Li) = fun_calc_isotope_fraction(par_sed_hydroip_fLi_d7Li,loc_standard)*loc_tot
     end if
  end if
  ! add Ca, remove Mg
  ! NOTE: assumes that the prescribed Mg flux is negative
  ! NOTE: original code automatically balanced Ca input with Mg removal (par_sed_hydroip_fMg was not defined)
  If (ocn_select(io_Ca)) then
     loc_fhydrothermal(io_Ca) = par_sed_hydroip_fCa
     if (ocn_select(io_Ca_44Ca)) then
        loc_tot = loc_fhydrothermal(io_Ca)
        loc_standard = const_standards(ocn_type(io_Ca_44Ca))
        loc_fhydrothermal(io_Ca_44Ca) = fun_calc_isotope_fraction(par_sed_hydroip_fCa_d44Ca,loc_standard)*loc_tot
     end if
     If (ocn_select(io_Mg)) then
        loc_fhydrothermal(io_Mg) = par_sed_hydroip_fMg
        loc_fhydrothermal(io_ALK) = 2.0*par_sed_hydroip_fMg + 2.0*par_sed_hydroip_fCa
     else
        loc_fhydrothermal(io_ALK) = 2.0*par_sed_hydroip_fCa
     end If
  end if
  ! Sr
  IF (ocn_select(io_Sr_87Sr) .AND. ocn_select(io_Sr_88Sr)) THEN
     ! initialization -- populate array with bulk flux and isotopic delta values
     loc_fhydrothermal(io_Sr) = par_sed_hydroip_fSr
     loc_fhydrothermal(io_Sr_87Sr) = 1000.0*(par_sed_hydroip_fSr_r87Sr/const_standardsR(ocn_type(io_Sr_87Sr)) - 1.0)
     loc_fhydrothermal(io_Sr_88Sr) = par_sed_hydroip_fSr_d88Sr
     ! calculate Sr ISOTOPES -- 87Sr
     ! NOTE: do not update <loc_fhydrothermal> yet as it is needed for the d88Sr calculation ...
     loc_87Sr = fun_calc_isotope_abundanceR012ocn(io_Sr_87Sr,io_Sr_88Sr,loc_fhydrothermal(:),1)
     ! calculate Sr ISOTOPES -- 88Sr
     loc_88Sr = fun_calc_isotope_abundanceR012ocn(io_Sr_87Sr,io_Sr_88Sr,loc_fhydrothermal(:),2)
     ! update flux array
     loc_fhydrothermal(io_Sr_87Sr) = loc_87Sr
     loc_fhydrothermal(io_Sr_88Sr) = loc_88Sr
  end IF
  ! re-scale global total and apread over *valid* grid points
  DO i=1,n_i
     DO j=1,n_j
        if (loc_phys_sed_mask_deepsea(i,j) > const_real_nullsmall) then
           DO l=1,n_l_ocn
              io = conv_iselected_io(l)
              dum_sfxocn(io,i,j) = dum_sfxocn(io,i,j) + loc_fhydrothermal(io)/loc_tot_A/conv_yr_s
           end DO
        end if
     end DO
  end DO
  ! calculate weathering/alteration fluxes (net sink)
  ! NOTE: dum_sfxocn(io,:,:) in units of (mol m-2 s-1)
  DO i=1,n_i
     DO j=1,n_j
        if (loc_phys_sed_mask_deepsea(i,j) > const_real_nullsmall) then
           ! NOTE: the value of par_sed_lowTalt_fLi_alpha is calculated on the basis of a sink of x mol yr-1
           !          where Atot is the total sediment area and [Li] assumed to be 26 umol Li
           !       => e.g. 1.0E10 (mol yr-1) = [Li] * x / Atot / (365.25*24*3600)
           ! Li
           If (ocn_select(io_Li)) then
              loc_flowTalteration(io_Li) = par_sed_lowTalt_fLi_alpha*dum_sfcsumocn(io_Li,i,j)
              if (ocn_select(io_Li_7Li)) then
                 loc_standard = const_standards(ocn_type(io_Li_7Li))
                 if (dum_sfcsumocn(io_Li,i,j) > const_real_nullsmall) then
                    loc_r7Li = dum_sfcsumocn(io_Li_7Li,i,j)/dum_sfcsumocn(io_Li,i,j)
                 else
                    loc_r7Li = 0.0
                 end if
                 loc_alpha = 1.0 + par_sed_lowTalt_7Li_epsilon/1000.0
                 loc_R = loc_r7Li/(1.0 - loc_r7Li)
                 loc_flowTalteration(io_Li_7Li) = (loc_alpha*loc_R/(1.0 + loc_alpha*loc_R))*loc_flowTalteration(io_Li)
              end if
           end if
           ! Ca
           If (ocn_select(io_Ca)) then
              ! NOTE: the calcium sink is calculated on the basis of a sink of x mol yr-1
              !       where Atot is the total sediment area and [Ca] assumed to be 0.01025 mol Ca
              !       => e.g. [Ca] * x / Atot / (365.25*24*3600)
              loc_flowTalteration(io_Ca) = par_sed_lowTalt_fCa_alpha*dum_sfcsumocn(io_Ca,i,j)
              If (ocn_select(io_Ca_44Ca)) then
                 loc_standard = const_standards(ocn_type(io_Ca_44Ca))
                 if (dum_sfcsumocn(io_Ca,i,j) > const_real_nullsmall) then
                    loc_r44Ca = dum_sfcsumocn(io_Ca_44Ca,i,j)/dum_sfcsumocn(io_Ca,i,j)
                 else
                    loc_r44Ca = 0.0
                 end if
                 loc_alpha = 1.0 + par_sed_lowTalt_44Ca_epsilon/1000.0
                 loc_R = loc_r44Ca/(1.0 - loc_r44Ca)
                 loc_flowTalteration(io_Ca_44Ca) = (loc_alpha*loc_R/(1.0 + loc_alpha*loc_R))*loc_flowTalteration(io_Ca)
              end If
           end if
           ! Sr
           ! NOTE: assume no fractionation in Sr uptake
           ! NOTE: OLD scheme:
           !       loc_flowTalteration(io_Sr)      = par_sed_lowTalt_fSr/loc_tot_A/conv_yr_s
           !       scale to a m-2 s-1 basis from the original parameter mol yr-1 units
           ! NOTE: the Sr sink is calculated on the basis of a sink of x mol yr-1
           !       where Atot is the total sediment area and [Sr] assumed to be 88.0E-6 mol kg-2 Sr
           !       => e.g. [Sr] * x / Atot / (365.25*24*3600)
           !       i.e. par_sed_lowTalt_fSr_alpha has units of mol [Sr] m-2 s-1 per (mol Sr kg-1)
           IF (ocn_select(io_Sr_87Sr) .AND. ocn_select(io_Sr_88Sr)) THEN
              loc_flowTalteration(io_Sr)      = par_sed_lowTalt_fSr_alpha*dum_sfcsumocn(io_Sr,i,j)
              loc_flowTalteration(io_Sr_87Sr) = loc_flowTalteration(io_Sr)*dum_sfcsumocn(io_Sr_87Sr,i,j)/dum_sfcsumocn(io_Sr,i,j)
              loc_flowTalteration(io_Sr_88Sr) = loc_flowTalteration(io_Sr)*dum_sfcsumocn(io_Sr_88Sr,i,j)/dum_sfcsumocn(io_Sr,i,j)
           end IF
           ! CO2(!) (aka 'weathering')
           ! NOTE: assume no fractionation in CO2 uptake
           ! NOTE: scale to a m-2 s-1 basis from the original parameter mol yr-1 units
           loc_flowTalteration(io_DIC) = par_sed_lowTalt_fCO2/loc_tot_A/conv_yr_s
           If (ocn_select(io_DIC_13C)) then
              loc_flowTalteration(io_DIC_13C) = loc_flowTalteration(io_DIC)*dum_sfcsumocn(io_DIC_13C,i,j)/dum_sfcsumocn(io_DIC,i,j)
           endif
           If (ocn_select(io_DIC_14C)) then
              loc_flowTalteration(io_DIC_14C) = loc_flowTalteration(io_DIC)*dum_sfcsumocn(io_DIC_14C,i,j)/dum_sfcsumocn(io_DIC,i,j)
           endif
           ! update flux array
           DO l=1,n_l_ocn
              io = conv_iselected_io(l)
              dum_sfxocn(io,i,j) = dum_sfxocn(io,i,j) - loc_flowTalteration(io)
           end DO
        end if
     end DO
  end DO

  ! *** UPDATE INTERFACE ***
  IF (ctrl_misc_debug4) print*,'*** UPDATE INTERFACE ***'
  ! update update sed->ocn interface
  ! NOTE: <sedocn_fnet> in units of (mol cm-2)
  ! NOTE: <dum_sfxocn> in units of (mol m-2 s-1)
  DO l=1,n_l_ocn
     io = conv_iselected_io(l)
     dum_sfxocn(io,:,:) = dum_sfxocn(io,:,:) + conv_m2_cm2*sedocn_fnet(io,:,:)/loc_dts
  end DO
  ! re-initialize the interfacing integrated sediment rain flux array
  ! NOTE: in normal operation, dum_reinit_sfxsumsed is .true., hence once sediment rain has been added to the sediments
  !       the flux array is zero-ed
  !       (during GEMlite phases, there is no BIOGEM updating of the ocean rain flux, hence need to preserve the flux value)
  if (dum_reinit_sfxsumsed) then
     dum_sfxsumsed(:,:,:) = 0.0
  else
     dum_sfxsumsed(:,:,:) = loc_sfxsumsed_OLD(:,:,:)
  end if
  ! update sediment interface composition data
  dum_sfcsed(:,:,:) = fun_sed_coretop()

  ! *** DEBUG ***
  ! print some debugging info if 'ctrl_misc_debug1' option is selected
  IF (ctrl_misc_debug1) THEN
     i = par_misc_debug_i
     j = par_misc_debug_j
     print*,''
     print*,'--- DEBUG ---'
     print*,'> SEDGEM LOCATION (i,j): ',i,j
     print*,'> TIME, TIME-STEP: ',loc_dts,loc_dtyr
     print*, &
          & phys_sed(ips_D,i,j),               &
          & dum_sfcsumocn(io_T,i,j),           &
          & dum_sfcsumocn(io_S,i,j)
     print*, &
          & dum_sfcsumocn(io_DIC,i,j),         &
          & dum_sfcsumocn(io_ALK,i,j),         &
          & 1.0E+06*sed_carb(ic_dCO3_cal,i,j)
     print*, &
          & 100.0*sed_top(is_CaCO3,i,j),       &
          & 100.0*sed_top(is_opal,i,j)
     print*, &
          & 1.0E+06*sed_fsed(is_CaCO3,i,j)/loc_dtyr, &
          & 1.0E+06*sed_fsed(is_POC,i,j)/loc_dtyr,   &
          & 1.0E+06*sed_fdis(is_CaCO3,i,j)/loc_dtyr, &
          & 1.0E+06*sed_fdis(is_POC,i,j)/loc_dtyr
     print*, &
          & sum(sed_fsed(is_CaCO3,:,:)*conv_m2_cm2*phys_sed(ips_A,:,:))/loc_dtyr, &
          & sum(sed_fsed(is_POC,:,:)*conv_m2_cm2*phys_sed(ips_A,:,:))/loc_dtyr,   &
          & sum(sed_fdis(is_CaCO3,:,:)*conv_m2_cm2*phys_sed(ips_A,:,:))/loc_dtyr, &
          & sum(sed_fdis(is_POC,:,:)*conv_m2_cm2*phys_sed(ips_A,:,:))/loc_dtyr
     print*,'---'
     print*,''
  end if
  ! catch any carbonate chemistry errors arising in sub_update_sed
  if (error_stop) then
     call end_sedgem(     &
          & dum_dts,      &
          & dum_sfcsumocn &
          & )
  end if

  ! *** RUN-TIME OUTPUT ***
  ! GHC 20/05/09 - Save time-series output
  IF (ctrl_timeseries_output) THEN
     ! increment timestep counter  
     tstep_count = tstep_count + 1  
     ! if output due then change year  
     CALL sub_output_year()  
     IF (tstep_count.eq.output_tsteps_0d(output_counter_0d)) THEN
        call sub_data_save_seddiag_GLOBAL(loc_dtyr,dum_sfcsumocn)  
     ENDIF
     IF (tstep_count.eq.output_tsteps_2d(output_counter_2d)) THEN
        ! save requested sediment cores as ASCII     
        ! call sub_sedgem_save_sedcore()
        ! save oecan-sediment interface properties
        !if (ctrl_data_save_ascii) call sub_data_save_seddiag_2D(loc_dtyr,dum_sfcsumocn)
        call sub_save_netcdf(year)
        call sub_save_netcdf_sed2d(loc_dtyr,dum_sfcsumocn)
        call sub_closefile(ntrec_siou)
        ntrec_sout = ntrec_sout + 1  
     ENDIF
     ! if output then increment output counter  
     CALL sub_output_counters()
  ENDIF

  ! *** UPDATE SEDGEM TIME ***
  IF (ctrl_misc_debug4) print*,'*** UPDATE SEDGEM TIME ***'
  ! update sediment age (== current time in years)
  sed_age = sed_age - loc_dtyr

end SUBROUTINE sedgem
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! AGE SEDIMENTS
SUBROUTINE sedgem_dsedage(  &
     & dum_dts,             &
     & dum_sfxsumsed        &
     & )
  USE sedgem_lib
  IMPLICIT NONE
  ! dummy arguments
  REAL,INTENT(in)::dum_dts                                              ! time-step
  real,DIMENSION(n_sed,n_i,n_j),intent(inout)::dum_sfxsumsed            ! sediment rain flux interface array
  ! local variables
  integer::i,j                                                 ! COUNTING AND ARRAY INDEX VARIABLES
  real::loc_dtyr                                               ! 
  ! set age decrement (years)
  loc_dtyr = dum_dts/conv_yr_s
  ! decrement (CaCO3) sediment age tracer
  DO i=1,n_i
     DO j=1,n_j
        IF (sed_mask(i,j)) THEN
           if (sed_select(is_CaCO3_age)) then
              dum_sfxsumsed(is_CaCO3_age,i,j) = dum_sfxsumsed(is_CaCO3_age,i,j) - loc_dtyr*dum_sfxsumsed(is_CaCO3,i,j)
           end if
           if (sed_select(is_det_age)) then
              dum_sfxsumsed(is_det_age,i,j) = dum_sfxsumsed(is_det_age,i,j) - loc_dtyr*dum_sfxsumsed(is_det,i,j)
           end if
        end IF
     end DO
  end DO
end SUBROUTINE sedgem_dsedage
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! *** RESTART SEDGEM (save data) ************************************************************************************************* !
! ******************************************************************************************************************************** !
SUBROUTINE sedgem_save_rst(dum_genie_clock,dum_sfxocn)
  USE sedgem_lib
  use sedgem_data_netCDF
  IMPLICIT NONE
  ! ---------------------------------------------------------- !
  ! DEFINE DUMMY ARGUMENTS
  ! ---------------------------------------------------------- !
  integer(kind=8),INTENT(IN)::dum_genie_clock                  ! genie clock (milliseconds since start) NOTE: 8-byte integer
  real,DIMENSION(n_ocn,n_i,n_j),intent(in)::dum_sfxocn         ! sediment dissolution flux interface array
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
     call sub_data_netCDF_ncrstsave(trim(string_ncrst),loc_yr,loc_iou,dum_sfxocn)
  else
     ! ------------------------------------------------------- !
     ! SAVE RESTART DATA: BINARY DUMP FORMAT
     ! ------------------------------------------------------- !
     ! NOTE: data is saved unformatted for minimal file size
     !       also means that arrays can be written directly to file without needing to loop thought data
     loc_filename = TRIM(par_outdir_name)//trim(par_outfile_name)
     OPEN(unit=out,status='replace',file=loc_filename,form='unformatted',action='write')
     WRITE(unit=out)                                         &
          & n_l_sed,                                         &
          & (conv_iselected_is(l),l=1,n_l_sed),              &
          & (sed(conv_iselected_is(l),:,:,:),l=1,n_l_sed),   &
          & (sed_top(conv_iselected_is(l),:,:),l=1,n_l_sed), &
          & sed_top_h(:,:)
     close(unit=out)
  end IF
  ! ---------------------------------------------------------- !
  ! END
  ! ---------------------------------------------------------- !
END SUBROUTINE sedgem_save_rst
! ******************************************************************************************************************************** !
