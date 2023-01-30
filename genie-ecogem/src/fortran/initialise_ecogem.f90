
! ******************************************************************************************************************************** !
! SETUP ECOGEM
!ckc isotope version
SUBROUTINE initialise_ecogem(    &
     & dum_egbg_sfcpart,         &
     & dum_egbg_sfcremin,        &
     & dum_egbg_sfcocn,          &
     & dum_dsc,                  &
     & dum_k1,                   &
     & dum_dz,                   &
     & dum_dza,                  &
     & dum_sv                    &
     & )

  USE ecogem_lib
  USE ecogem_box
  USE ecogem_data
  use ecogem_data_netCDF
  USE genie_util, ONLY: check_iostat
  ! ---------------------------------------------------------- !
  ! DUMMY ARGUMENTS
  ! ---------------------------------------------------------- !
  real,intent(inout),dimension(n_sed ,n_i,n_j,n_k)::dum_egbg_sfcpart  ! ocean -> ecology flux; ocn grid
  real,intent(inout),dimension(n_ocn ,n_i,n_j,n_k)::dum_egbg_sfcremin ! ecology -> ocean flux; ocn grid
  real,intent(inout),dimension(n_ocn ,n_i,n_j,n_k)::dum_egbg_sfcocn   ! ecology-interface ocean tracer composition; ocn grid
  real,intent(in)                                 ::dum_dsc           !
  integer,DIMENSION(n_i,n_j),INTENT(in)::dum_k1                  !
  REAL,DIMENSION(n_k),INTENT(in)::dum_dz,dum_dza                 !
  REAL,DIMENSION(0:n_j),INTENT(in)::dum_sv    !
  CHARACTER(len=64)::site_string
  ! ---------------------------------------------------------- !
  ! local variables
  integer :: jp,k
  INTEGER :: loc_iou

  print*,'======================================================='
  print*,' >>> Initialising ECOGEM ocean ecology module ...'

  ! *** load goin information ***
  call sub_load_goin_ecogem()
  ! ---------------------------------------------------------- ! set time
  ! NOTE: modify 'par_misc_t_start' according to the run-time accumulated in any requested restart,
  !       so that the time that EcoGeM starts with is the same as the requested start time
  !       (EcoGeM calculates its internal time as equal to par_misc_t_start + elapsed GOLDSTEIn time,
  !        BUT, elapsed GOLDSTEIn time will include any accumulated restart time,
  !        hence accumulated restart time is subtracted from par_misc_t_start)
  ! NOTE: I'm sure that this doesn't make any sense at all ... :(
  if (ctrl_misc_t_BP) then
     par_misc_t_end = par_misc_t_start - par_misc_t_runtime
  else
     par_misc_t_end = par_misc_t_start + par_misc_t_runtime
  end if

  ! ---------------------------------------------------------- !
  ! *** copy GOLDSTEIn parameters ***
  ! ---------------------------------------------------------- !
  ! ---------------------------------------------------------- ! copy dimensional scale factors for ocean
  goldstein_dsc     = dum_dsc
  ! ---------------------------------------------------------- ! copy grid information
  goldstein_k1(:,:) = dum_k1(:,:)
  goldstein_dz(:)   = dum_dz(:)
  goldstein_dza(:)  = dum_dza(:)
  goldstein_sv(:)   = dum_sv(:)
  ! ---------------------------------------------------------- !
  CALL sub_init_phys_ocn()

  ! check required nutrients are carried by BIOGEM!
  call check_egbg_compatible()

  ! get specifications of plankton populations from input file
  CALL sub_init_populations()
  ! get names and locations of time-series sites for output
  CALL sub_init_timeseries()

  if (ctrl_debug_eco_init) then
     write(*,*),' ---------------------------------------------------'
     write(*,*),'- Plankton population specifications from input file'
     print '(a32,i5)','- number of plankton:          ',npmax
     print '(a37)','   PFT                diameter      n'
     do jp=1,npmax
        write(*,'(a3,a16,a4,F7.1,a2,i5)'),'     ',pft(jp),"  ",diameter(jp),"  ",random_n(jp)
     enddo
     write(*,*),' ---------------------------------------------------'
  endif
  ! calculate other indices dependent on goin information
  !order and indices for nutrient elements (unused elements to zero)
  iDIC  = 1 ! Mandatory                                                  ! index for dissolved inorganic carbon
  iNO3  = (    iDIC                           +1) * MERGE(1,0,useNO3)    ! index for nitrate
  iNO2  = (MAX(iDIC,iNO3,iNO2)                +1) * MERGE(1,0,useNO2)    ! index for nitrite
  iNH4  = (MAX(iDIC,iNO3,iNO2)                +1) * MERGE(1,0,useNH4)    ! index for ammonium
  iPO4  = (MAX(iDIC,iNO3,iNO2,iNH4)           +1) * MERGE(1,0,usePO4)    ! index for phospate
  iFe   = (MAX(iDIC,iNO3,iNO2,iNH4,iPO4)      +1) * MERGE(1,0,useFe)     ! index for dissolved iron
  iSiO2 = (MAX(iDIC,iNO3,iNO2,iNH4,iPO4,iFe)  +1) * MERGE(1,0,useSiO2)   ! index for silicate
  iimax =  MAX(iDIC,iNO3,iNO2,iNH4,iPO4,iFe,iSiO2) ! number of dissolved inorganic 'nutrients'
  !ckc order and indices for nutrient isotopes, put in if statement?
  iDIC_13C = 1
  !iimaxiso = MAX(iDIC_13C) !ckc expandable for future isotope tracing
  iimaxiso = 1
  !order and indices for cellular element reservoirs & organic matter
  iCarb = 1 ! Mandatory                                                  ! index for C  quota
  iNitr = (    iCarb                         +1) * MERGE(1,0,nquota)     ! index for N  quota
  iPhos = (MAX(iCarb,iNitr)                  +1) * MERGE(1,0,pquota)     ! index for P  quota
  iIron = (MAX(iCarb,iNitr,iPhos)            +1) * MERGE(1,0,fquota)     ! index for Fe quota
  iSili = (MAX(iCarb,iNitr,iPhos,iIron)      +1) * MERGE(1,0,squota)     ! index for Si quota
  iChlo = (MAX(iCarb,iNitr,iPhos,iIron,iSili)+1) * MERGE(1,0,chlquota)   ! index for Chlorophyll quota
  iomax =  MAX(iCarb,iNitr,iPhos,iIron,iSili) ! number of planktonic elemental pools (not including chlorophyll)
  komax = 2
  !usage flags
  iChl = MERGE(1,0,chlquota) ! integer flag for dynamic chlorophyll
  iSil = MERGE(1,0,useSiO2)  ! integer flag for silicate quotas (diatoms only)
  ! remember to load data fields the same order !

  !ckc create isotope tracing array for each plankton group, put in if statement?
  iCarb13C = 1
  !iomaxiso = MAX(iCarb13C) !ckc expandable for future isotope tracing
  iomaxiso = 1

  ! ---------------------------------------- !
  ! *** set-up ecogem-biogem interfacing ***
  ! ---------------------------------------- !
  ! ---------------------------------------- ! initialize interface arrays
  dum_egbg_sfcpart(:,:,:,:)  = 0.0
  dum_egbg_sfcremin(:,:,:,:) = 0.0
  dum_egbg_sfcocn(:,:,:,:)   = 0.0


  ! *** dimension tracer arrays ***
  ! NOTE: check for problems allocating array space
  ! ecogem state variables
  ALLOCATE(nutrient(iimax,n_i,n_j,n_k),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(plankton(iomax+iChl,npmax,n_i,n_j,n_k),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(uptake_flux(iomax,npmax,n_i,n_j,n_k),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(phys_limit(iomax+2,npmax,n_i,n_j,n_k),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(zoo_limit(npmax,n_i,n_j,n_k),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  !ckc ISOTOPES
  ALLOCATE(nutiso(iimaxiso,n_i,n_j,n_k),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(plankiso(iomaxiso,npmax,n_i,n_j,n_k),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(up_flux_iso(iomaxiso,npmax,n_i,n_j,n_k),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(export_flux(iomax,npmax,n_i,n_j,n_k),STAT=alloc_error)    !export flux per plankton type    Fanny/Maria - Aug19
  call check_iostat(alloc_error,__LINE__,__FILE__)

  ! ecogem time-slice arrays
  ALLOCATE(int_plankton_timeslice(iomax+iChl,npmax,n_i,n_j,n_k),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(int_uptake_timeslice(iomax,npmax,n_i,n_j,n_k),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(int_gamma_timeslice(iomax+2,npmax,n_i,n_j,n_k),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(int_nutrient_timeslice(iimax,n_i,n_j,n_k),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(int_zoogamma_timeslice(npmax,n_i,n_j,n_k),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(int_export_timeslice(iomax,npmax,n_i,n_j,n_k),STAT=alloc_error)    !export flux per plankton type    Fanny/Maria - Aug19
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ! Time-series storage arrays
  if (n_tser.gt.0) then
     ALLOCATE(nutrient_tser(iimax,n_tser,48),STAT=alloc_error)
     call check_iostat(alloc_error,__LINE__,__FILE__)
     ALLOCATE(plankton_tser(iomax+iChl,npmax,n_tser,48),STAT=alloc_error)
     call check_iostat(alloc_error,__LINE__,__FILE__)
     ALLOCATE(uptake_tser(iomax,npmax,n_tser,48),STAT=alloc_error)
     call check_iostat(alloc_error,__LINE__,__FILE__)
     ALLOCATE(gamma_tser(iomax+2,npmax,n_tser,48),STAT=alloc_error)
     call check_iostat(alloc_error,__LINE__,__FILE__)
     ALLOCATE(time_tser(48),STAT=alloc_error)
     call check_iostat(alloc_error,__LINE__,__FILE__)
  endif
  ! Initialise time slices
  CALL sub_init_int_timeslice()

  ! Carbonate chemistry parameters
  ALLOCATE(eco_carb(n_carb,n_i,n_j,n_k),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(eco_carbconst(n_carbconst,n_i,n_j,n_k),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(eco_carbalk(n_carbalk,n_i,n_j,n_k),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(eco_carbisor(n_carbisor,n_i,n_j,n_k),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  eco_carb(:,:,:,:)      = 0.0   !
  eco_carb(ic_H,:,:,:)   = 1.0e-7   !
  eco_carbconst(:,:,:,:) = 0.0   !
  ! *** dimension parameter vectors (for npmax plankton) ***
  ! Size
  ALLOCATE(volume(npmax),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(logvol(npmax),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(logesd(npmax),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ! PFT associated traits
  ALLOCATE(autotrophy(npmax),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(heterotrophy(npmax),STAT=alloc_error)
  ALLOCATE(herbivory(npmax),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(carnivory(npmax),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(palatability(npmax),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(NO3up(npmax),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(Nfix(npmax),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(calcify(npmax),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(silicify(npmax),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(prey_refuge(npmax),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  !ALLOCATE(grazing_protect(npmax),STAT=alloc_error)
  !call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(pp_opt_a_array(npmax),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(pp_sig_a_array(npmax),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(ns_array(npmax),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(mort_protect(npmax),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(growthcost_factor(npmax),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ! Nutrient and nutrient quota parameters
  ALLOCATE(qmin(iomax,npmax),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(qmax(iomax,npmax),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(vmax(iimax,npmax),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(affinity(iimax,npmax),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(kexc(iomax,npmax),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ! Carbon quota and Photosynthesis parameters
  ALLOCATE(qcarbon(npmax),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(alphachl(npmax),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ! Grazing parameters
  ALLOCATE(graz(npmax),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(kg(npmax),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(pp_opt(npmax),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(pp_sig(npmax),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(gkernel(npmax,npmax),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(gkernelT(npmax,npmax),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ! Other loss parameters
  ALLOCATE(respir(npmax),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(biosink(npmax),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(mort(npmax),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(beta_graz(npmax),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(beta_mort(npmax),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)

  ! Quota string labels
  ALLOCATE(quotastrng(iomax+iChl),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  quotastrng(iCarb) = 'C'
  if (nquota )  quotastrng(iNitr) = 'N'
  if (pquota )  quotastrng(iPhos) = 'P'
  if (squota )  quotastrng(iSili) = 'Si'
  if (fquota )  quotastrng(iIron) = 'Fe'
  if (chlquota) quotastrng(iChlo) = 'Chl'

  ALLOCATE(quotaunits(iomax+iChl),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  quotaunits(iCarb) = 'mmol C m^-3'
  if (nquota )  quotaunits(iNitr) = 'mmol N m^-3'
  if (pquota )  quotaunits(iPhos) = 'mmol P m^-3'
  if (squota )  quotaunits(iSili) = 'mmol Si m^-3'
  if (fquota )  quotaunits(iIron) = 'mmol Fe m^-3'
  if (chlquota) quotaunits(iChlo) = 'mg Chl m^-3'

  ! Match nutrients to quotas
  ALLOCATE(nut2quota(iimax),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  nut2quota(iDIC)  = iCarb
  if (useNO3 )  nut2quota(iNO3)  = iNitr
  if (useNO2 )  nut2quota(iNO2)  = iNitr
  if (useNH4 )  nut2quota(iNH4)  = iNitr
  if (usePO4 )  nut2quota(iPO4)  = iPhos
  if (useFe )   nut2quota(iFe)   = iIron
  if (useSiO2 ) nut2quota(iSiO2) = iSili

  ! Resource string labels
  ALLOCATE(rsrcstrng(iimax),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  rsrcstrng(iDIC)  = 'DIC'
  if (useNO3 )  rsrcstrng(iNO3)  = 'NO3'
  if (useNO2 )  rsrcstrng(iNO2)  = 'NO2'
  if (useNH4 )  rsrcstrng(iNH4)  = 'NH4'
  if (usePO4 )  rsrcstrng(iPO4)  = 'PO4'
  if (useFe )   rsrcstrng(iFe)   = 'Fe'
  if (useSiO2 ) rsrcstrng(iSiO2) = 'SiO2'

  ! ---------------------------------------------------------- !
  ! OPEN ASCII FILES !---------------------------------------- !
  ! ---------------------------------------------------------- !
  open(301,File=TRIM(par_outdir_name)//"/Plankton_params.txt"       ,Status="Replace",Action="Write")
  open(302,File=TRIM(par_outdir_name)//"/Plankton_params_nohead.dat",Status="Replace",Action="Write")
  open(303,File=TRIM(par_outdir_name)//"/Plankton_grazing.dat"      ,Status="Replace",Action="Write")

  ! make wet mask for ocean cells
  wet_mask_ij(:,:) = MERGE(1,0,goldstein_k1.le.n_k)
  do k=1,n_k
     wet_mask_ijk(:,:,k) = MERGE(1,0,goldstein_k1.le.k)
  enddo

  ! get explicit grazing parameters from input file
  if(ctrl_grazing_explicit)then
    CALL sub_init_explicit_grazing_params()
  endif

  ! *** initialise plankton biomass array
  call sub_init_plankton()

  ! JDW: allocate and load temperature forcing dataset
  if(ctrl_force_T)then
        allocate(T_input(n_i,n_j),STAT=alloc_error)
        T_input(:,:)=0.0
        call sub_init_load_forceT()
  end if

  ! ---------------------------------------------------------- ! INITIALIZE netCDF OUTPUT
  IF (ctrl_debug_init > 0) print*,'INITIALIZE netCDF OUTPUT'
  string_ncout2d = TRIM(par_outdir_name)//'fields_ecogem_2d.nc'
  string_ncout3d = TRIM(par_outdir_name)//'fields_ecogem_3d.nc'
  string_nctsint = TRIM(par_outdir_name)//'timeseries_ecogem.nc'
  string_nctsi   = TRIM(par_outdir_name)//'ts_ecogem_int.nc'


  ALLOCATE(ncout1d_iou(n_tser),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  ALLOCATE(ncout1d_ntrec(n_tser),STAT=alloc_error)
  call check_iostat(alloc_error,__LINE__,__FILE__)

  if (n_tser.gt.0) then
     do k=1,n_tser
        site_string = TRIM(par_outdir_name)//'timeseries_'//trim(tser_name(k))//'.nc'
        call sub_init_netcdf_tser(site_string,loc_iou)
        ncout1d_iou(k) = loc_iou
        ncout1d_ntrec(k) = 0
     enddo
  endif

  call sub_init_netcdf(trim(string_ncout2d),loc_iou,2)
  ncout2d_iou = loc_iou
  ncout2d_ntrec = 0
  ! NOTE: 3D data is not currently saved, so disable nc file creation
  !call sub_init_netcdf(trim(string_ncout3d),loc_iou,3)
  !ncout3d_iou = loc_iou
  !ncout3d_ntrec = 0
  ! ---------------------------------------------------------- ! LOAD RE-START
  IF (ctrl_continuing) then
     IF (ctrl_debug_init > 0) print*,'LOAD RE-START'
     call sub_data_load_rst()
  end if
  ! ---------------------------------------------------------- !


  print*,' <<< Initialisation complete'
  print*,'======================================================='

  return

END SUBROUTINE initialise_ecogem
! ******************************************************************************************************************************** !
