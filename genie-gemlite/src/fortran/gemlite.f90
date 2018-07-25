
! ******************************************************************************************************************************** !
! GEMlite LOOP SUBROUTINE
! NOTE: RESTORING AND FLUX FORCINGS WILL NOT AFFECT THE UPDATING (THEY ARE SPECIFIC TO BIOGEM)
! NOTE: SURFACE FCO2 IS RAPIDLY EQUILIBRIATED WITH THE ATM, HENCE A PULSE OF CO2 EMITTED TO THE ATMOSERE
! WILL NOT EFFECTIVELY BE DEALT WITH BY GEMLIE AS IT REQUIRES DISEQUILIBRIUM BETWEEN OCEAN *SURFACE* AND ATM TO WORK
! NOTE: <ocn> here has GEMlite module scope only (i.e. there is an entirely seperate <ocn> for BIOGEM)
subroutine gemlite(    &
     & dum_sfcocn1,    &
     & dum_sfxsumsed1, &
     & dum_sfxocn1,    &
     & dum_sfxsumrok1_gem, &
     & dum_sfxsumatm1_gem  &
     & )
  use gem_cmn
  use gem_util
  use gem_carbchem
  USE gemlite_lib
  IMPLICIT NONE
  ! DUMMY ARGUMENTS
  real,dimension(n_ocn,n_i,n_j),intent(out)::dum_sfcocn1                ! sediment-interface ocean tracer composition; ocn grid
  real,DIMENSION(n_sed,n_i,n_j),intent(in)::dum_sfxsumsed1              ! sediment rain flux (mol m-2 per time-step)
  real,DIMENSION(n_ocn,n_i,n_j),intent(in)::dum_sfxocn1                 ! sediment dissolution flux (mol m-2 s-1)
  real,dimension(n_ocn,n_i,n_j),intent(inout)::dum_sfxsumrok1_gem       ! coastal (weathering) flux (mol per time-step)
  real,dimension(n_atm,n_i,n_j),intent(inout)::dum_sfxsumatm1_gem       ! atmospheric fluxes: outgassing and weathering
  ! LOCAL VARIABLES
  INTEGER::i,j,k                                                        ! counting indices
  INTEGER::l,ia,io,is                                                      ! counting indices
  integer::loc_i,loc_tot_i                                              ! 
  integer::loc_k1                                                       ! topography
  real::loc_T,loc_S,loc_DIC,loc_ALK                                     ! 
  real::loc_Ca,loc_SO4                                                  ! 
  real::loc_PO4,loc_SiO2,loc_B,loc_F,loc_H2S,loc_NH4                    ! 
  real::loc_atm_tot_V,loc_ocn_tot_V,loc_ocn_tot_M                       ! atmosphere volume, ocean volume
  real,DIMENSION(n_ocn)::loc_fsed                                       ! tracer sink (sedimentation flux) (ocn tracer array)
  real,DIMENSION(n_l_ocn)::loc_tot_fdis                                 ! total sediment dissolution flux
  real,DIMENSION(n_l_ocn)::loc_tot_fsed                                 ! total sedimentation flux
  real,DIMENSION(n_l_ocn)::loc_tot_fwea                                 ! total weathering flux
  real,DIMENSION(n_l_atm)::loc_tot_fatm                                 ! 
  real::loc_datm_flux,loc_docn_flux                                     ! atmosphere & ocean tracer change from net external fluxes
  real::loc_tot_A                                                       ! 
  real::loc_tot_pCO2,loc_tot_fCO2                                       !
  real::loc_mean_fCO2,loc_mean_pCO2                                     !
  real::loc_mean_fCO2_NEW,loc_mean_pCO2_NEW                             !
  real::loc_mean_fCO2_OLD,loc_mean_pCO2_OLD                             !
  real::loc_mean_DpCO2_NEW,loc_mean_DpCO2_OLD                           !
  real::loc_docn_DIC,loc_docn_DIC_13C                                   !
  real::loc_tot_DpO2                                                    !
  real,DIMENSION(n_i,n_j)::loc_ocn_mask                                 ! wet (ocean) mask
  real,DIMENSION(n_i,n_j)::loc_DCO2                                     !
  real,DIMENSION(n_i,n_j)::loc_DCO2_13C                                 !
  real,DIMENSION(n_i,n_j)::loc_r13C_as                                  ! air-sea 13C fractionation factor
  REAL,DIMENSION(n_i,n_j)::loc_datm_pCO2,loc_datm_pCO2_13C              !
  REAL,DIMENSION(n_i,n_j)::loc_datm_pO2                                 !
  REAL,DIMENSION(n_i,n_j)::loc_conv_atm_mol                             !
  integer::loc_count                                                    !

  ! *** INITIALIZE LOCAL VARIABLES ***
  ! physics
  loc_ocn_mask(:,:) = 0.0
  ! initialize local arrays
  loc_tot_fdis(:)        = 0.0
  loc_tot_fsed (:)       = 0.0
  loc_tot_fwea(:)        = 0.0
  loc_tot_fatm(:)        = 0.0
  loc_datm_pCO2(:,:)     = 0.0
  loc_datm_pCO2_13C(:,:) = 0.0
  loc_datm_pO2(:,:)      = 0.0
  loc_r13C_as(:,:) = 0.0
  ! initialize GEMlite anomoly arrays
  datm(:,:,:)   = 0.0
  docn(:,:,:,:) = 0.0

  ! *** CALCULATE LOCAL CONSTANTS & VARIABLES ***
  ! total atmosphere volume
  loc_atm_tot_V = sum(phys_atm_V(:,:))
  ! total ocean volume
  loc_ocn_tot_V = sum(phys_ocn_V(:,:,:))
  ! total ocean mass
  loc_ocn_tot_M = conv_m3_kg*loc_ocn_tot_V
  ! conversion between partial pressure and molar quantity
  ! NOTE: atm(ia_T,:,:) in C
  loc_conv_atm_mol(:,:) = phys_atm_V(:,:)/(conv_Pa_atm*const_R_SI*(atm(ia_T,:,:)+const_zeroC))

  ! *** UPDATE OCEAN-ATMOSPHERE ***

  DO i=1,n_i
     DO j=1,n_j
        loc_k1 = goldstein_k1(i,j)
        IF (n_k >= loc_k1) THEN
           ! set ocean mask
           loc_ocn_mask(i,j) = phys_ocn_mask(i,j,n_k)
           ! re-calculate carbonate dissociation constants
           CALL sub_calc_carbconst(       &
                & phys_ocn_Dmid(i,j,n_k), &
                & ocn(conv_io_lselected(io_T),i,j,n_k), &
                & ocn(conv_io_lselected(io_S),i,j,n_k), &
                & carbconst(:,i,j)        &
                & )
           ! adjust carbonate constants
           if (ocn_select(io_Ca) .AND. ocn_select(io_Mg)) then
              call sub_adj_carbconst(     &
                   & ocn(conv_io_lselected(io_Ca),i,j,n_k),  &
                   & ocn(conv_io_lselected(io_Mg),i,j,n_k),  &
                   & carbconst(:,i,j) &
                   & )
           end if
        end if
     end do
  end do

  ! *** REPARTITION CO2 BETWEEN OCEAN AND ATMOSPEHRE ***

  ! NOTE: only if 'ctrl_update_pCO2' is selected (DEFAULT)
  ! NOTE: no attempt is made to explicitly homogonise the atmosphere
  if (ctrl_update_pCO2) then
     ! initialize local loop count
     loc_count = 0
     Do
        ! if 2nd (or later) pass through loop => store CO2 values from pervious iteration
        if (loc_count > 0) then
           loc_mean_fCO2_OLD = loc_mean_fCO2_NEW
           loc_mean_pCO2_OLD = loc_mean_pCO2_NEW
           loc_mean_DpCO2_OLD = loc_mean_fCO2_OLD - loc_mean_pCO2_OLD
        end if
        ! zero loop variables
        loc_tot_fCO2 = 0.0
        loc_tot_pCO2 = 0.0
        loc_tot_A = 0.0

        ! *** RE-CALCULATE CO2 VARIABLES ***
        ! calculate weighted mean equilibrium pCO2
        ! NOTE: only update pCO2 over wet grid points, but account for the full grid
        !       (i.e. substitute 'missing' dry grid point values)
        ! NOTE: fCO2 from <carbchem> is used directly (rather than re-calculating solubility constants seperately)
        DO i=1,n_i
           DO j=1,n_j
              loc_k1 = goldstein_k1(i,j)
              IF (n_k >= loc_k1) THEN
                 ! set local tracer values
                 ! NOTE: although climate and hence salinity is implicitly assumed to have not changed,
                 !       not all the tracers may have been selected (and hence included in the dimensionable array),
                 !       hence re-estimate Ca and borate concentrations from salinity 
                 loc_T   = ocn(conv_io_lselected(io_T),i,j,n_k)
                 loc_S   = ocn(conv_io_lselected(io_S),i,j,n_k)
                 loc_DIC = ocn(conv_io_lselected(io_DIC),i,j,n_k)
                 loc_ALK = ocn(conv_io_lselected(io_ALK),i,j,n_k)
                 IF (.NOT. ocn_select(io_Ca)) then
                    loc_Ca = fun_calc_Ca(loc_S)
                 else
                    loc_Ca = ocn(conv_io_lselected(io_Ca),i,j,n_k)
                 endif
                 IF (.NOT. ocn_select(io_SO4)) then
                    loc_SO4 = fun_calc_SO4tot(loc_S)
                 else
                    loc_SO4 = ocn(conv_io_lselected(io_SO4),i,j,n_k)
                 endif
                 IF (.NOT. ocn_select(io_B)) then
                    loc_B = fun_calc_Btot(loc_S)
                 else
                    loc_B = ocn(conv_io_lselected(io_B),i,j,n_k)
                 endif
                 IF (.NOT. ocn_select(io_F)) then
                    loc_F = fun_calc_Ftot(loc_S)
                 else
                    loc_F = ocn(conv_io_lselected(io_F),i,j,n_k)
                 endif
                 IF (.NOT. ocn_select(io_PO4)) then
                    loc_PO4 = 0.0
                 else
                    loc_PO4 = ocn(conv_io_lselected(io_PO4),i,j,n_k)
                 endif
                 IF (.NOT. ocn_select(io_SiO2)) then
                    loc_SiO2 = 0.0
                 else
                    loc_SiO2 = ocn(conv_io_lselected(io_SiO2),i,j,n_k)
                 endif
                 IF (.NOT. ocn_select(io_H2S)) then
                    loc_H2S = 0.0
                 else
                    loc_H2S = ocn(conv_io_lselected(io_H2S),i,j,n_k)
                 endif
                 IF (.NOT. ocn_select(io_NH4)) then
                    loc_NH4 = 0.0
                 else
                    loc_NH4 = ocn(conv_io_lselected(io_NH4),i,j,n_k)
                 endif
                 ! seed default initial ocean pH
                 if (carb(ic_H,i,j) <= const_real_nullsmall) carb(ic_H,i,j) = 10**(-7.8)
                 ! re-calculate surface ocean carbonate chemistry
                 CALL sub_calc_carb(                            &
                      & ocn(conv_io_lselected(io_DIC),i,j,n_k), &
                      & ocn(conv_io_lselected(io_ALK),i,j,n_k), &
                      & loc_Ca,                                 &
                      & loc_PO4,                                &
                      & loc_SiO2,                               &
                      & loc_B,                                  &
                      & loc_SO4,                                &
                      & loc_F ,                                 &
                      & loc_H2S,                                &
                      & loc_NH4,                                &
                      & carbconst(:,i,j),                       & 
                      & carb(:,i,j),                            &  
                      & carbalk(:,i,j)                          & 
                      & )
                 ! re-calculate carbonate system isotopic properties
                 if (ocn_select(io_DIC_13C)) then
                    call sub_calc_carb_r13C(                           &
                         & ocn(conv_io_lselected(io_T),i,j,n_k),       &
                         & ocn(conv_io_lselected(io_DIC),i,j,n_k),     &
                         & ocn(conv_io_lselected(io_DIC_13C),i,j,n_k), &
                         & carb(:,i,j),                                &
                         & carbisor(:,i,j)                             &
                         & )
                    ! set local air->sea 13CO2 fractionation factor
                    ! NOTE: ocn(io_T) is in units of K (needs to be C for empirical fractionation equation)
                    loc_r13C_as(i,j) = 0.99869 + 4.9E-6*(ocn(conv_io_lselected(io_T),i,j,n_k)-const_zeroC)
                 end IF
                 ! update cumulative CO2 and area variables
                 ! NOTE: weight by actual grid area (so as to accommodate for non equal area grids)
                 loc_tot_fCO2 = loc_tot_fCO2 + &
                      & (1.0 - phys_ocnatm_seaice(i,j))*phys_atm_V(i,j)*carb(ic_fug_CO2,i,j)
                 loc_tot_pCO2 = loc_tot_pCO2 + &
                      & (1.0 - phys_ocnatm_seaice(i,j))*phys_atm_V(i,j)*atm(ia_pCO2,i,j)
                 loc_tot_A = loc_tot_A + &
                      & (1.0 - phys_ocnatm_seaice(i,j))*phys_atm_V(i,j)
              end if
           end do
        end do
        ! *** UPDTE CO2 VARIABLES ***
        ! calculate (ice-free) area-weighted mean surface (fCO2) and atmosphere (pCO2) CO2
        if (loc_tot_A > const_real_nullsmall) then
           loc_mean_fCO2 = loc_tot_fCO2/loc_tot_A
           loc_mean_pCO2 = loc_tot_pCO2/loc_tot_A
        else
           ! this should not be possible ... but just in case:
           ! NOTE: ideally would be done with a generic error handling call (future TO-DO!)
           print*,'!!! IMPOSSIBLE ERROR !!!'
           print*,'gemlite.f90 / gemlie'
           stop
        endif
        ! set cycle and local loop start CO2 values
        ! NOTE: the assumption is being made that ocn-atm start the cycle in equilibrium, i.e,
        !       averaged atmospheric pCO2 == averaged ocean surface fCO2
        ! NOTE: positive values of cycle_tot_DpCO2_0 <-> fCO2 average is too high compared to pCO2 average
        !       i.e., ocean will tend to spuriously lose CO2 to the atmosphere, which must be corrected for
        if (loc_count == 0) then
           if (gemcycle_count == 0) then
              gemcycle_mean_fCO2 = loc_mean_fCO2
              gemcycle_mean_pCO2 = loc_mean_pCO2
              gemcycle_mean_DpCO2_0 = gemcycle_mean_fCO2 - gemcycle_mean_pCO2
              loc_mean_fCO2_OLD = loc_mean_fCO2
              loc_mean_pCO2_OLD = loc_mean_pCO2
           else
              loc_mean_fCO2_OLD = gemcycle_mean_fCO2
              loc_mean_pCO2_OLD = gemcycle_mean_pCO2
           endif
           loc_mean_DpCO2_OLD = loc_mean_fCO2_OLD - loc_mean_pCO2_OLD
        endif
        ! set NEW mean CO2 variable values to updated values; set new DpCO2
        loc_mean_fCO2_NEW = loc_mean_fCO2
        loc_mean_pCO2_NEW = loc_mean_pCO2
        loc_mean_DpCO2_NEW = loc_mean_fCO2_NEW - loc_mean_pCO2_NEW
        ! update count variables
        ! NOTE: loc_count is used to determine the first pass through the loop
        !       gemcycle_count is used to determine the start of the GEM cycle phase itself
        loc_count = loc_count + 1
        gemcycle_count = gemcycle_count + 1
        ! *** CALCUILATE MASS TRANSFER ***
        ! CO2 to be moved between ocn and atm is proportional to the pCO2 difference, calculated in terms of atm mass equivalent
        ! NOTE: currently, only 50% of the total pCO2 difference between ocean and atmosphere is used for calculating CO2 transfer
        !       => this gives a relayively smooth convergence
        !          100% is more likely to give alternating +/- oscillations in transfer (but still converging)
        ! NOTE: mask out non wet grid points; weight by ice-free fraction
        ! AR.2012.09.05: changed transfer fraction to 0.25 for imporved solution stability under extreme CO2 forcing
        loc_DCO2(:,:) = 0.25*loc_ocn_mask(:,:)*(1.0 - phys_ocnatm_seaice(:,:))* &
             & (loc_mean_fCO2_NEW - loc_mean_pCO2_NEW - gemcycle_mean_DpCO2_0)* &
             & loc_conv_atm_mol(:,:)
        ! calculate change in atmospheric pCO2 (atm) => update atmosphere
        loc_datm_pCO2(:,:)    = loc_DCO2(:,:)/loc_conv_atm_mol(:,:)
        datm(ia_pCO2,:,:)     = datm(ia_pCO2,:,:)     + loc_datm_pCO2(:,:)
        datm_sum(ia_pCO2,:,:) = datm_sum(ia_pCO2,:,:) + loc_datm_pCO2(:,:)
        atm(ia_pCO2,:,:)      = atm(ia_pCO2,:,:)      + loc_datm_pCO2(:,:)
        ! calculate change in ocean DIC (mol kg-1) => update ocean
        loc_docn_DIC           = -sum(loc_DCO2(:,:))/loc_ocn_tot_M
        docn(io_DIC,:,:,:)     = docn(io_DIC,:,:,:)     + loc_docn_DIC
        docn_sum(io_DIC,:,:,:) = docn_sum(io_DIC,:,:,:) + loc_docn_DIC
        ocn(io_DIC,:,:,:)      = ocn(io_DIC,:,:,:)      + loc_docn_DIC
        ! update isotopes
        ! NOTE: no formal attempt is made to re-equillibriate ocean and atmosphere isotopically
        !       => the isotopic properties of CO2 (ocn)  and pCO2 (atm) are simply transfered
        !          (depending sign of disequilibrium)
        ! NOTE: the air-sea CO2 disequilibrium fractionation is accounted for atm->ocn CO2 transfer
        ! NOTE: 2-way kinetic effects are not account for currently ... but could be
        !       (GEMlite methodology is such an approximation of nominally whole-system change -- does it really matter?)
        if (atm_select(ia_pCO2_13C)) then
           ! test for ocn-atm flux direction
           if (loc_docn_DIC < 0.0) then
              ! net CO2 addition to atmosphere => use [CO2] isotopic composition
              loc_DCO2_13C(:,:) = carbisor(ici_CO2_r13C,:,:)*loc_DCO2(:,:)
           else
              ! net CO2 addition to the ocean => use pCO2 isotopic composition
              loc_DCO2_13C(:,:) = loc_r13C_as(:,:)*atm(ia_pCO2_13C,:,:)/atm(ia_pCO2,:,:)*loc_DCO2(:,:)
           endif
           ! calculate change in atmospheric 13pCO2 (atm) => update atmosphere
           loc_datm_pCO2_13C(:,:)    = loc_DCO2_13C(:,:)/loc_conv_atm_mol(:,:)
           datm(ia_pCO2_13C,:,:)      = datm(ia_pCO2_13C,:,:)     + loc_datm_pCO2_13C(:,:)
           datm_sum(ia_pCO2_13C,:,:)  = datm_sum(ia_pCO2_13C,:,:) + loc_datm_pCO2_13C(:,:)
           atm(ia_pCO2_13C,:,:)       = atm(ia_pCO2_13C,:,:)      + loc_datm_pCO2_13C(:,:)
           ! calculate change in ocean 13DIC (mol kg-1) => update ocean
           loc_docn_DIC_13C           = -sum(loc_DCO2_13C(:,:))/loc_ocn_tot_M
           docn(io_DIC_13C,:,:,:)     = docn(io_DIC_13C,:,:,:)     + loc_docn_DIC_13C
           docn_sum(io_DIC_13C,:,:,:) = docn_sum(io_DIC_13C,:,:,:) + loc_docn_DIC_13C
           ocn(io_DIC_13C,:,:,:)      = ocn(io_DIC_13C,:,:,:)      + loc_docn_DIC_13C
        end IF
        ! *** TEST FOR CONVERGENCE ***
        ! test for convergence (of CO2 atmospheric inventory)
        ! => update global CO2 variable values
        ! => EXIT loop
        IF (ABS(loc_mean_DpCO2_NEW - loc_mean_DpCO2_OLD) < par_DpCO2_thresh) then
           gemcycle_mean_fCO2 = loc_mean_fCO2_NEW
           gemcycle_mean_pCO2 = loc_mean_pCO2_NEW
           EXIT
        end if
        ! test for an unreasonable struggle to converge
        if (loc_count > 100) then
           ! NOTE: ideally would be done with a generic error handling call (future TO-DO!)
           ! NOTE: also: the threshold iteration count could be a namelist parameter (not hard-wired in)
           ! AR.2012.09.05: STOP model entirely (not just exit loop)
           print*,'!!! HELP !!!'
           print*,'(failure to converge CO2 re-partitioning within 100 iterations)'
           print*,'gemlite.f90 / gemlie'
           print*,loc_mean_fCO2_OLD,loc_mean_fCO2_NEW
           print*,loc_mean_pCO2_OLD,loc_mean_pCO2_NEW
           print*,loc_mean_DpCO2_OLD,loc_mean_DpCO2_NEW
           print*,loc_count,ABS(loc_mean_DpCO2_NEW - loc_mean_DpCO2_OLD),par_DpCO2_thresh
           !!!print*,'--- ocean fCO2 ---'
           !!!print*,carb(ic_fug_CO2,:,:)
           !!!print*,'--- atmosphere pCO2 ---'
           !!!print*,atm(ia_pCO2,:,:)
           print*,'--- END ---'
           stop !!!exit
        endif
     end Do
  else
    ! NOTHING
  endif

  ! *** CALCULATE GEOLOGICAL FLUXES ***
  DO i=1,n_i
     DO j=1,n_j
        ! (1) FULL GRID
        ! calculate atm exchange fluxes -- CO2 outgassing and weathering consumption (assuming no weathering short-circuiting)
           DO l=3,n_l_atm
              ia = conv_iselected_ia(l)
              loc_tot_fatm(l) = loc_tot_fatm(l) + phys_atm_A(i,j)*dum_sfxsumatm1_gem(ia,i,j)
           end do
        ! (2) OCEAN GRID
        loc_k1 = goldstein_k1(i,j)
        IF (n_k >= loc_k1) THEN
           ! calculate total sediment dissolution flux
           ! NOTE: convert from units of (mol m-2 s-1) -> (mol m-2 yr-1)
           DO l=3,n_l_ocn
              io = conv_iselected_io(l)
              loc_tot_fdis(l) = loc_tot_fdis(l) + phys_ocn_A(i,j,loc_k1)*dum_sfxocn1(io,i,j)/conv_s_yr
           end do
           ! calculate total sedimentation flux (ocn tracer equivalents)
           ! NOTE: units of: (mol m-2 per time-step) with the assumption made of an annual time-step for SEDGEM
           !       (hence (mol m-2 yr-1))
           ! NOTE: negative sign (== loss from ocean)
           loc_fsed(:) = 0.0
           DO l=1,n_l_sed
              is = conv_iselected_is(l)
              loc_tot_i = conv_sed_ocn_i(0,is)
              do loc_i=1,loc_tot_i
                 io = conv_sed_ocn_i(loc_i,is)
                 loc_fsed(io) = loc_fsed(io) - phys_ocn_A(i,j,loc_k1)*conv_sed_ocn(io,is)*dum_sfxsumsed1(is,i,j)
              end do
           end DO
           ! calculate sedimentation flux
           DO l=3,n_l_ocn
              io = conv_iselected_io(l)
              loc_tot_fsed(l) = loc_tot_fsed(l) + loc_fsed(io)
           end do
           ! calculate total weathering flux
           ! NOTE: units of: (mol per integrating time-step) with the assumption made of an *annual* time-step (hence (mol yr-1))
           DO l=3,n_l_ocn
              io = conv_iselected_io(l)
              loc_tot_fwea(l) = loc_tot_fwea(l) + dum_sfxsumrok1_gem(io,i,j)
           end do
        end if
     END DO
  END DO
  ! corrections for missing boundary conditions
  ! NOTE: after conversion to dissolved tracers: prevent removal or additio of Fe
  ! (becasue dust input is not enabled in GEMlite)
  ! NOTE: no accounting (yet) at all for 56Fe ...
  if (ocn_select(io_Fe)) loc_tot_fdis(conv_io_lselected(io_Fe)) = 0.0
  if (ocn_select(io_Fe)) loc_tot_fsed(conv_io_lselected(io_Fe)) = 0.0

  ! *** PARTITION O2 FLUXES BETWEEN OCEAN AND ATMOSPEHRE ***
  ! partition O2 fluxes between ocean and atmosphere in direct proportion to the ratio of their inventories
  ! NOTE: for now: simply add (all) O2 associated with buried POM to the atmosphere
  ! NOTE: loc_tot_fsed is negative for a flux leaving ocean (sedimentation)
  if (atm_select(ia_pO2)) then
     loc_tot_DpO2 = -(loc_tot_fdis(conv_io_lselected(io_O2)) + loc_tot_fsed(conv_io_lselected(io_O2)))
     loc_tot_fdis(conv_io_lselected(io_O2)) = 0.0
     loc_tot_fsed(conv_io_lselected(io_O2)) = 0.0
     ! update atmosphere
     loc_datm_pO2(:,:)    = loc_tot_DpO2/real(n_i*n_j)/loc_conv_atm_mol(:,:)
     datm(ia2l(ia_pO2),:,:)     = datm(ia2l(ia_pO2),:,:)     + loc_datm_pO2(:,:)
     datm_sum(conv_ia_lselected(ia_pO2),:,:) = datm_sum(conv_ia_lselected(ia_pO2),:,:) + loc_datm_pO2(:,:)
     atm(conv_ia_lselected(ia_pO2),:,:)      = atm(conv_ia_lselected(ia_pO2),:,:)      + loc_datm_pO2(:,:)
  endif

  ! *** !!! DECAY TRACERS !!! ***

  ! *** UPDATE OCEAN and ATMOSPHERE COMPOSITION ***
  ! NOTE: fluxes due to air-sea gas exchange already accounted for
  DO i=1,n_i
     DO j=1,n_j
        ! (1) atmosphere
        !     NOTE: /real(n_i*n_j) is used once in calculating a mean conversion between atm and mol, 
        !           and a second time in dividing up the total flux between atm grid points 
           DO l=3,n_l_atm
              ia = conv_iselected_ia(l)
              loc_datm_flux   = loc_tot_fatm(l)/(sum(loc_conv_atm_mol(:,:))/real(n_i*n_j))/real(n_i*n_j)
              atm(l,i,j)      = atm(l,i,j)      + loc_datm_flux
              datm(l,i,j)     = datm(l,i,j)     + loc_datm_flux
              datm_sum(l,i,j) = datm_sum(l,i,j) + loc_datm_flux
           end do
        ! (2) ocean
        ! set local depth loop limit
        loc_k1 = goldstein_k1(i,j)
        IF (n_k >= loc_k1) THEN
           ! 
           DO k=loc_k1,n_k
              DO l=3,n_l_ocn
                 io = conv_iselected_io(l)
                 loc_docn_flux = (loc_tot_fdis(l) + loc_tot_fsed(l) + loc_tot_fwea(l))/(conv_m3_kg*loc_ocn_tot_V)
                 ocn(l,i,j,k)      = ocn(l,i,j,k)  + loc_docn_flux
                 docn(l,i,j,k)     = docn(l,i,j,k) + loc_docn_flux
                 docn_sum(l,i,j,k) = docn_sum(l,i,j,k) + loc_docn_flux
              end do
           end do
        end if
     END DO
  END DO

  ! *** SET INTERFACE ARRAYS ***
  DO i=1,n_i
     DO j=1,n_j
        ! set local depth loop limit
        loc_k1 = goldstein_k1(i,j)
        IF (n_k >= loc_k1) THEN
           ! set bottom-water tracers
           ! NOTE: no need to re-estimate e.g. Ca and borate concentrations from salinity becasue
           !       climate is implicitly assumed to have not changed'
           DO l=3,n_l_ocn
              io = conv_iselected_io(l)
              dum_sfcocn1(io,i,j) = ocn(l,i,j,loc_k1)
           end do
        end if
     END DO
  END DO

end subroutine gemlite
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! GEMLITE LOOP SUBROUTINE - CLIMATE STATE UPDATE
     subroutine gemlite_climate( &
     & dum_frac_sic             &
     & )
  USE gemlite_lib
  IMPLICIT NONE
  ! DUMMY ARGUMENTS
  real,dimension(n_i,n_j),INTENT(in)::dum_frac_sic                      ! sea-ice fractional cover (2-D)

  ! *** UPDATE CLIMATE PROPERTIES ***
  phys_ocnatm_seaice(:,:) = dum_frac_sic(:,:)

end subroutine gemlite_climate
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! GEMLITE LOOP SUBROUTINE - INITIALIZATION
     subroutine gemlite_cycleinit( &
     & )
  USE gemlite_lib
  IMPLICIT NONE

  ! *** INITIALIZE VARIABLES ***
  ! (re-)set GEMlite cycle counter
  gemcycle_count = 0

end subroutine gemlite_cycleinit
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! GEMLITE LOOP SUBROUTINE - INITIALIZATION
     subroutine gemlite_cycleclean( &
     & dum_sfxsumrok1_gem      &
     & )
  USE gemlite_lib
  IMPLICIT NONE
  ! dummy arguments
  REAL,dimension(n_ocn,n_i,n_j),intent(inout)::dum_sfxsumrok1_gem       ! 

  ! *** RE-INITIALIZE VARIABLES ***
  ! reset cumulative (annual average) weathering array (<dum_sfxsumrok1_gem>)
  dum_sfxsumrok1_gem(:,:,:) = 0.0

end subroutine gemlite_cycleclean
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! GEMlite<->ts COUPLING SUBROUTINE
subroutine gemlite_ts( &
     & dum_ts,         &
     & dum_ts1         &
     & )
  USE gemlite_lib
  IMPLICIT NONE
  ! DUMMY ARGUMENTS
  real,intent(inout),dimension(intrac_ocn,n_i,n_j,n_k)::dum_ts          ! NOTE: number of tracers in GOLDSTEIN used in dimension #1
  real,intent(inout),dimension(intrac_ocn,n_i,n_j,n_k)::dum_ts1         ! NOTE: number of tracers in GOLDSTEIN used in dimension #1
  ! LOCAL VARIABLES
  integer::i,j,k                                                        !
  integer::l,io                                                         !
  integer::loc_k1                                                       !
  real::loc_ocn_tot_V                                                   ! ocean volume
  real::loc_ocn_mean_S_NEW                                              ! new mean ocean salinity

  ! *** CALCULATE LOCAL CONSTANTS ***
  ! total ocean volume
  loc_ocn_tot_V = sum(phys_ocn_V(:,:,:))

  ! *** OCEAN TRACER UPDATE ***
  ! calculate mean ocean salinity
  loc_ocn_mean_S_NEW = SUM(ocn(2,:,:,:)*phys_ocn_V(:,:,:))/loc_ocn_tot_V
  ! update GOLDSTEIn tracer arrays and salinity-normalize
  block_iloop: DO i=1,n_i
     block_jloop: DO j=1,n_j
        ! set local depth loop limit
        loc_k1 = goldstein_k1(i,j)
        IF (n_k >= loc_k1) THEN
           DO k=loc_k1,n_k
              DO l=3,n_l_ocn
                 io = conv_iselected_io(l)
                 dum_ts(l,i,j,k)  = dum_ts(l,i,j,k) + (loc_ocn_mean_S_NEW/ocn(2,i,j,k))*docn_sum(l,i,j,k)
                 dum_ts1(l,i,j,k) = dum_ts(l,i,j,k)
              end do
           end do
        end if
     END DO block_jloop
  END DO block_iloop

end subroutine gemlite_ts
! ******************************************************************************************************************************** !
