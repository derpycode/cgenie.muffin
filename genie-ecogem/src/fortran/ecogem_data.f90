! ******************************************************************************************************************************** !
! ecogem_data.f90
!
! DATA LOADING/SAVING/INITIALIZATION ROUTINES
! ******************************************************************************************************************************** !


MODULE ecogem_data

  USE ecogem_lib
  USE genie_util, ONLY: check_iostat
  USE ecogem_box, ONLY: lower_case
  IMPLICIT NONE
  SAVE

CONTAINS

  ! ****************************************************************************************************************************** !
  ! LOAD ECOGEM 'goin' FILE OPTIONS
  SUBROUTINE sub_load_goin_ecogem()
    USE genie_util, ONLY: check_unit,check_iostat
    ! local variables
    integer::ios
    ! read data_ECOGEM file
    call check_unit(in,__LINE__,__FILE__)
    open(unit=in,file='data_ECOGEM',status='old',action='read',iostat=ios)
    if (ios /= 0) then
       print*,'ERROR: could not open ECOGEM initialisation namelist file'
       stop
    end if
    ! read in namelist and close data_ECOGEM file
    read(UNIT=in,NML=ini_ecogem_nml,IOSTAT=ios)
    if (ios /= 0) then
       print*,'ERROR: could not read ECOGEM namelist'
       stop
    else
       close(unit=in)
    end if
    ! set and report namelist data
    par_indir_name = trim(par_indir_name)//'/'
    par_outdir_name = trim(par_outdir_name)//'/'
    par_rstdir_name = trim(par_rstdir_name)//'/'
    if ((ctrl_debug_init > 0) .OR. ctrl_debug_eco_init) then
       ! #### INSERT CODE TO LOAD ADDITIONAL PARAMETERS ############################################################################# !
       ! --- ECOLOGICALCONFIGURATION --------------------------------------------------------------------------------------------- !
       write(*,*), ' >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>'
       write(*,*), '--- ECOLOGICAL MODEL CONFIGURATION-------------------'
       write(*,67),'   - number of layers with living plankton        :   ',n_keco
       write(*,*), '---- Plankton ---------------------------------------'
       write(*,66),'   - planktonic N biomass                         :   ',nquota
       write(*,66),'   - planktonic P biomass                         :   ',pquota
       write(*,66),'   - planktonic Fe biomass                        :   ',fquota
       write(*,66),'   - planktonic Si biomass                        :   ',squota
       write(*,66),'   - planktonic chlorophyll biomass               :   ',chlquota
       write(*,*), '---- Organic Matter ---------------------------------'
       write(*,67),'   - number of organic matter classes             :   ',komax
       write(*,*), '---- Inorganic nutrients ----------------------------'
       write(*,66),'   - using DIC                                    :   ',useDIC
       write(*,66),'   - using NO3                                    :   ',useNO3
       write(*,66),'   - using NO2                                    :   ',useNO2
       write(*,66),'   - using NH4                                    :   ',useNH4
       write(*,66),'   - using PO4                                    :   ',usePO4
       write(*,66),'   - using Fe (dissolved iron)                    :   ',useFe
       write(*,66),'   - using SiO2                                   :   ',useSiO2
       write(*,*), '-----------------------------------------------------'
       write(*,*), '--- ECOLOGICAL PARAMETERS ---------------------------'
       if (nquota) then
          write(*,*), '- Nitrogen parameters  -------------------------------'
          write(*,69),'  - N minimum quota                       (qminN) : a=',    qminN_a,', b=',    qminN_b
          write(*,69),'  - N maximum quota                       (qmaxN) : a=',    qmaxN_a,', b=',    qmaxN_b
          if (useNO3) then
             write(*,69),'  - Maximum nitrate uptake rate         (vmaxNO3) : a=',  vmaxNO3_a,', b=',  vmaxNO3_b
             write(*,69),'  - Nitrate uptake affinity            (affinNO3) : a=', affinNO3_a,', b=', affinNO3_b
             write(*,69),'  - Half-sat. for nitrate uptake           (kNO3) : a=', vmaxNO3_a/affinNO3_a,', b=', vmaxNO3_b-affinNO3_b
          endif
          if (useNO2) then
             write(*,69),'  - Maximum nitrite uptake rate         (vmaxNO2) : a=',  vmaxNO2_a,', b=',  vmaxNO2_b
             write(*,69),'  - Nitrite uptake affinity            (affinNO2) : a=', affinNO2_a,', b=', affinNO2_b
             write(*,69),'  - Half-sat. for nitrite uptake           (kNO2) : a=', vmaxNO2_a/affinNO2_a,', b=', vmaxNO2_b-affinNO2_b
          endif
          if (useNH4) then
             write(*,69),'  - Maximum ammonium uptake rate        (vmaxNH4) : a=',  vmaxNH4_a,', b=',  vmaxNH4_b
             write(*,69),'  - Ammonium uptake affinity           (affinNH4) : a=', affinNH4_a,', b=', affinNH4_b
             write(*,69),'  - Half-sat. for ammonium uptake          (kNH4) : a=', vmaxNH4_a/affinNH4_a,', b=', vmaxNH4_b-affinNH4_b
          endif
          write(*,69),'  - N excretion rate                      (kexcN) : a=',    kexcN_a,', b=',    kexcN_b
       endif
       if (pquota) then
          write(*,*), '- Phosphorus parameters  -----------------------------'
          write(*,69),'  - P minimum quota                       (qminP) : a=',    qminP_a,', b=',    qminP_b
          write(*,69),'  - P maximum quota                       (qmaxP) : a=',    qmaxP_a,', b=',    qmaxP_b
          write(*,69),'  - Maximum phosphate uptake rate       (vmaxPO4) : a=',  vmaxPO4_a,', b=',  vmaxPO4_b
          write(*,69),'  - Phosphate uptake affinity          (affinPO4) : a=', affinPO4_a,', b=', affinPO4_b
          write(*,69),'  - Half-sat. for phosphate uptake         (kPO4) : a=', vmaxPO4_a/affinPO4_a,', b=', vmaxPO4_b-affinPO4_b
          write(*,69),'  - P excretion rate                      (kexcP) : a=',    kexcP_a,', b=',    kexcP_b
       endif
       if (fquota) then
          write(*,*), '-- Dynamic Iron quotas  ------------------------------'
          write(*,69),'  - Fe minimum quota                     (qminFe) : a=',   qminFe_a,', b=',   qminFe_b
          write(*,69),'  - Fe maximum quota                     (qmaxFe) : a=',   qmaxFe_a,', b=',   qmaxFe_b
          write(*,69),'  - Maximum iron uptake rate             (vmaxFe) : a=',   vmaxFe_a,', b=',   vmaxFe_b
          write(*,69),'  - Iron uptake affinity                (affinFe) : a=',  affinFe_a,', b=',  affinFe_b
          write(*,69),'  - Half-sat. for iron uptake               (kFe) : a=', vmaxFe_a/affinFe_a,', b=', vmaxFe_b-affinFe_b
          write(*,69),'  - Fe excretion rate                    (kexcFe) : a=',   kexcFe_a,', b=',   kexcFe_b
       endif
       if (squota) then
          write(*,*), '-- Dynamic Silicon quotas  ---------------------------'
          write(*,69),'  - Si minimum quota                     (qminSi) : a=' ,   qminSi_a,', b=',   qminSi_b
          write(*,69),'  - Si maximum quota                     (qmaxSi) : a=' ,   qmaxSi_a,', b=',   qmaxSi_b
          write(*,69),'  - Maximum silica uptake rate         (vmaxSiO2) : a=' , vmaxSiO2_a,', b=', vmaxSiO2_b
          write(*,69),'  - Silica uptake affinity            (affinSiO2) : a=' ,affinSiO2_a,', b=',affinSiO2_b
          write(*,69),'  - Half-sat. for silica uptake           (kSiO2) : a=', vmaxSiO2_a/affinSiO2_a,', b=', vmaxSiO2_b-affinSiO2_b
          write(*,69),'  - Si excretion rate                    (kexcSi) : a=' ,   kexcSi_a,', b=',   kexcSi_b
       endif
       write(*,*), '- Carbon quota and Photosynthesis parameters --------'
       write(*,70),'  - Maximum photosynthetic rate         (vmaxDIC) : a=',  vmaxDIC_a,', b=',  vmaxDIC_b,', c=',  vmaxDIC_c
       write(*,69),'  - Carbon per cell                     (qcarbon) : a=',  qcarbon_a,', b=',  qcarbon_b
       write(*,69),'  - initial slope of PI curve          (alphachl) : a=', alphachl_a,', b=', alphachl_b
       write(*,68),'  - maximum chlorophyll to N ratio     (chl2nmax) :   ',   chl2nmax
       write(*,68),'  - cost of biosynthesis               (biosynth) :   ',   biosynth
       write(*,68),'  - light attenuation by water              (k_w) :   ',        k_w
       write(*,68),'  - light attenuation by chlorophyll a    (k_chl) :   ',      k_chl
       write(*,66),'  - restrict MLD for mean light calculation?      :   ',      ctrl_restrict_mld
       write(*,*), '- Grazing parameters --------------------------------'
       write(*,68),'  - maximum assimilation efficiency     (ass_eff) :   ',    ass_eff
       !write(*,67),'  - prey switching exponent (integer)        (ns) :   ',    using switch_feeding plankton specific parameter (rather than ns)!,         ns   - commented out for Grigoratou foram code, Sep18
       write(*,68),'  - hill number for grazing assimilation   (hill) :   ',       hill
       write(*,68),'  - grazing refuge parameter             (Lambda) :   ',     Lambda
       write(*,69),'  - maximum grazing rate                   (graz) : a=',     graz_a,', b=',     graz_b
       write(*,69),'  - half-sat. concentration for grazing      (kg) : a=',       kg_a,', b=',       kg_b
       write(*,69),'  - optimal predator:prey length ratio   (pp_opt) : a=',   pp_opt_a,', b=',   pp_opt_b
       write(*,69),'  - width of grazing kernel              (pp_sig) : a=',   pp_sig_a,', b=',   pp_sig_b
       write(*,*), '- Other loss parameters ------------------------------'
       write(*,69),'  - carbon respiration rate              (respir) : a=',   respir_a,', b=',   respir_b
       write(*,69),'  - biomass sinking rate                (biosink) : a=',  biosink_a,', b=',  biosink_b
       write(*,69),'  - basal mortality                        (mort) : a=',     mort_a,', b=',     mort_b
       write(*,70),'  - fraction messy feed. to dissolved (beta_graz) : a=',beta_graz_a,', b=',beta_graz_b,', c=',beta_graz_c
       write(*,70),'  - fraction mortality to dissolved   (beta_mort) : a=',beta_mort_a,', b=',beta_mort_b,', c=',beta_mort_c
       write(*,*), '- Other stuff -----------------------------'
       write(*,71),'  - ecogem tsteps per biogem tstep     (nsubtime) :   ',nsubtime
       write(*,68),'  - maximum temperature                (temp_max) :   ',temp_max
       ! ------------------- ISOTOPIC FRACTIONATION ------------------------------------------------------------------------------ !
       print*,'Corg 13C fractionation scheme ID string             : ',trim(opt_d13C_DIC_Corg)
       print*,'b value for Popp et al. fractionation               : ',par_d13C_DIC_Corg_b
       print*,'fractionation for intercellular C fixation          : ',par_d13C_DIC_Corg_ef
       ! --- RUN CONTROL --------------------------------------------------------------------------------------------------------- !
       print*,'--- RUN CONTROL ------------------------------------'
       print*,'Continuing run?                                     : ',ctrl_continuing
       ! --- I/O DIRECTORY DEFINITIONS ------------------------------------------------------------------------------------------- !
       print*,'--- I/O DIRECTORY DEFINITIONS ----------------------'
       print*,'Input dir. name                                     : ',trim(par_indir_name)
       print*,'Output dir. name                                    : ',trim(par_outdir_name)
       print*,'Restart (input) dir. name                           : ',trim(par_rstdir_name)
       print*,'Filename for restart input                          : ',trim(par_infile_name)
       print*,'Filename for restart output                         : ',trim(par_outfile_name)
       ! --- DATA SAVING: MISC --------------------------------------------------------------------------------------------------- !
       print*,'--- DATA SAVING: MISC ------------------------------'
       print*,'Restart in netCDF format?                           : ',ctrl_ncrst
       print*,'netCDF restart file name                            : ',trim(par_ncrst_name)
    end if ! end ctrl_debug_eco_init
66  format(a56,l2)
67  format(a56,i2)
71  format(a56,i4)
68  format(a56,d10.3)
69  format(a56,d10.3,a4,f5.2)
70  format(a56,d10.3,a4,d10.3,a4,d10.3)
  END SUBROUTINE sub_load_goin_ecogem
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! *** LOAD EcoGEM RESTART DATA ************************************************************************************************* !
  ! ****************************************************************************************************************************** !
  SUBROUTINE sub_data_load_rst()
    USE ecogem_lib
    use gem_netcdf
    USE genie_util, ONLY:check_unit,check_iostat
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    integer::ios,io,jp,l,iv                                    !
    integer::loc_l
    integer::loc_ncid                                          !
    CHARACTER(len=255)::loc_filename                           ! filename string
    CHARACTER(len=255)::shrtstrng                              ! variable string
    real,dimension(n_i,n_j,n_k)::loc_ijk                       !
    integer::loc_ndims,loc_nvars
    integer::loc_n_l_plankton                                  ! number of plankton in binary re-start file
    integer,ALLOCATABLE,dimension(:)::loc_dimlen
    integer,ALLOCATABLE,dimension(:,:)::loc_varlen
    integer,ALLOCATABLE,dimension(:)::loc_vdims
    character(20),ALLOCATABLE,dimension(:)::loc_varname
    ! -------------------------------------------------------- !
    ! INITIALIZE
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! set filename
    IF (ctrl_ncrst) THEN
       loc_filename = TRIM(par_rstdir_name)//par_ncrst_name
    else
       loc_filename = TRIM(par_rstdir_name)//trim(par_infile_name)
    endif
    ! -------------------------------------------------------- ! check file status
    call check_unit(in,__LINE__,__FILE__)
    OPEN(unit=in,status='old',file=loc_filename,form='unformatted',action='read',IOSTAT=ios)
    close(unit=in)
    If (ios /= 0) then
       CALL sub_report_error( &
            & 'ecogem_data','sub_data_load_restart', &
            & 'You have requested a CONTINUING run, but restart file <'//trim(loc_filename)//'> does not exist', &
            & 'SKIPPING - using default initial small values [ecogem_data.f90]', &
            & (/const_real_null/),.false. &
            & )
    else
       ! ----------------------------------------------------- !
       ! LOAD RESTART
       ! ----------------------------------------------------- !
       IF (ctrl_ncrst) THEN
          call sub_openfile(loc_filename,loc_ncid)
          ! -------------------------------------------------- ! determine number of variables
          call sub_inqdims (loc_filename,loc_ncid,loc_ndims,loc_nvars)
          ! -------------------------------------------------- ! allocate arrays
          ALLOCATE(loc_dimlen(loc_ndims),STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          ALLOCATE(loc_varlen(2,loc_nvars),STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          ALLOCATE(loc_vdims(loc_nvars),STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          ALLOCATE(loc_varname(loc_nvars),STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          ! -------------------------------------------------- ! get variable names
          call sub_inqvars(loc_ncid,loc_ndims,loc_nvars,loc_dimlen,loc_varname,loc_vdims,loc_varlen)
          ! -------------------------------------------------- ! load plankton restart fields
          IF (ctrl_debug_eco_init) print*,' * Loading plankton restart fields: '
          DO iv=1,loc_nvars
             DO io=1,iomax+iChl
                DO jp=1,npmax
                   if ((io.le.iomax).or.(autotrophy(jp).gt.0.0)) then ! do not output non-existent chlorophyll for non-autotroph
                      ! ------------------------------------------------------------------------------------
                      write (shrtstrng, "(A1,A,A1,I3.3)") "_",trim(adjustl(quotastrng(io))),'_',jp
                      ! ------------------------------------------------------------------------------------
                      ! read 2D variables
                      !                  if ('eco2D'//trim(shrtstrng) == trim(loc_varname(iv))) then
                      !                    IF (ctrl_debug_eco_init) print*,"Loading "//trim(loc_varname(iv))
                      !                    loc_ij(:,:) = 0.0 ! NEED OCEAN MASK
                      !                    call sub_getvarij(loc_ncid,'eco2D'//shrtstrng,n_i,n_j,loc_ij(:,:)) ! load 2D fields
                      !                    ! ???(io,jp,:,:) = loc_ij(:,:)
                      !                  endif
                      ! ------------------------------------------------------------------------------------
                      ! read 3D variables
                      if ('eco3D'//trim(shrtstrng) == trim(loc_varname(iv))) then
                         IF (ctrl_debug_eco_init) print*,"Loading "//trim(loc_varname(iv))
                         loc_ijk(:,:,:) = 0.0 ! NEED OCEAN MASK
                         ! *************************************************************************************************************
                         ! possible 'malloc' error associated with this call ...
                         call sub_getvarijk(loc_ncid,'eco3D'//shrtstrng,n_i,n_j,n_k,loc_ijk(:,:,:)) ! load 3D fields
                         ! *************************************************************************************************************
                         plankton(io,jp,:,:,:) = loc_ijk(:,:,:)
                      endif
                      ! ------------------------------------------------------------------------------------
                   endif ! end if not zooplankton chlorophyll
                end DO ! end do jp
             end DO ! end do io
          end DO ! end do iv
          ! -------------------------------------------------- ! deallocate arrays
          deALLOCATE(loc_dimlen,STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          deALLOCATE(loc_varlen,STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          deALLOCATE(loc_vdims,STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          deALLOCATE(loc_varname,STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          ! -------------------------------------------------- ! close file
          call sub_closefile(loc_ncid)
       else
          OPEN(unit=in,status='old',file=loc_filename,form='unformatted',action='read',IOSTAT=ios)
          loc_n_l_plankton=iomax+iChl
          read(unit=in,iostat=ios)                                          &
               & loc_n_l_plankton,                                               &
               & (loc_l,l=1,loc_n_l_plankton),                &
               & (plankton(l,:,:,:,:),l=1,loc_n_l_plankton)
          call check_iostat(ios,__LINE__,__FILE__)
          close(unit=in,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
       endif
    endif
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  end SUBROUTINE sub_data_load_rst
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALISE PLANKTON
  SUBROUTINE sub_init_plankton()
    ! local variables and parameter vectors
    integer                    :: io,jp,n
    integer                    :: jpred,jprey
    real,dimension(npmax,1)    ::pred_diam
    real,dimension(1,npmax)    ::prey_diam
    real,dimension(npmax,npmax)::prdpry,ppopt_mat,ppsig_mat

    ! to convert per day rates into per second
    real,parameter :: pday = 86400.0

    ! size parameters
    volume(:)   = 1.0/6.0 * const_pi * diameter(:) ** 3
    logvol(:)   = log10(volume(:))
    logesd(:)   = log10(diameter(:))

    ! Define Traits of Plankton Functional Types
    do jp=1,npmax
       call lower_case(pft(jp))
       if (pft(jp).eq.'prochlorococcus') then
          NO3up(jp)           = 0.0
          Nfix(jp)            = 0.0
          calcify(jp)         = 0.0
          silicify(jp)        = 0.0
          autotrophy(jp)      = 1.0
          heterotrophy(jp)    = 0.0
          palatability(jp)    = 1.0
          prey_refuge(jp)     = 1.0
          pp_optC(jp)         = 10.0
          pp_sig_C(jp)        = 2.0
          !grazing_protect(jp) = 1.0
          mort_protect(jp)    = trophic_tradeoff
          switch_feeding(jp)  = 0.0
       elseif (pft(jp).eq.'synechococcus') then
          NO3up(jp)           = 0.0
          Nfix(jp)            = 0.0
          calcify(jp)         = 0.0
          silicify(jp)        = 0.0
          autotrophy(jp)      = 1.0
          heterotrophy(jp)    = 0.0
          palatability(jp)    = 1.0
          prey_refuge(jp)     = 1.0
          pp_optC(jp)         = 10.0
          pp_sig_C(jp)        = 2.0
          !grazing_protect(jp) = 1.0
          mort_protect(jp)    = trophic_tradeoff
          switch_feeding(jp)  = 0.0
       elseif (pft(jp).eq.'picoeukaryote') then
          NO3up(jp)           = 0.0
          Nfix(jp)            = 0.0
          calcify(jp)         = 0.0
          silicify(jp)        = 0.0
          autotrophy(jp)      = 1.0
          heterotrophy(jp)    = 0.0
          palatability(jp)    = 1.0
          prey_refuge(jp)     = 1.0
          pp_optC(jp)         = 10.0
          pp_sig_C(jp)        = 2.0
          !grazing_protect(jp) = 1.0
          mort_protect(jp)    = trophic_tradeoff
          switch_feeding(jp)  = 0.0
       elseif (pft(jp).eq.'diatom') then
          NO3up(jp)           = 0.0
          Nfix(jp)            = 0.0
          calcify(jp)         = 0.0
          silicify(jp)        = 0.0
          autotrophy(jp)      = 1.0
          heterotrophy(jp)    = 0.0
          palatability(jp)    = 1.0
          prey_refuge(jp)     = 1.0
          pp_optC(jp)         = 10.0
          pp_sig_C(jp)        = 2.0
          !grazing_protect(jp) = 1.0
          mort_protect(jp)    = trophic_tradeoff
          switch_feeding(jp)  = 0.0
       elseif (pft(jp).eq.'coccolithophore') then
          NO3up(jp)           = 0.0
          Nfix(jp)            = 0.0
          calcify(jp)         = 0.0
          silicify(jp)        = 0.0
          autotrophy(jp)      = 1.0
          heterotrophy(jp)    = 0.0
          palatability(jp)    = 1.0
          prey_refuge(jp)     = 1.0
          pp_optC(jp)         = 10.0
          pp_sig_C(jp)        = 2.0
          !grazing_protect(jp) = 1.0
          mort_protect(jp)    = trophic_tradeoff
          switch_feeding(jp)  = 0.0
       elseif (pft(jp).eq.'diazotroph') then
          NO3up(jp)           = 0.0
          Nfix(jp)            = 0.0
          calcify(jp)         = 0.0
          silicify(jp)        = 0.0
          autotrophy(jp)      = 1.0
          heterotrophy(jp)    = 0.0
          palatability(jp)    = 1.0
          prey_refuge(jp)     = 1.0
          pp_optC(jp)         = 10.0
          pp_sig_C(jp)        = 2.0
          !grazing_protect(jp) = 1.0
          mort_protect(jp)    = trophic_tradeoff
          switch_feeding(jp)  = 0.0
       elseif (pft(jp).eq.'phytoplankton') then
          NO3up(jp)           = 0.0
          Nfix(jp)            = 0.0
          calcify(jp)         = 0.0
          silicify(jp)        = 0.0
          autotrophy(jp)      = 1.0
          heterotrophy(jp)    = 0.0
          palatability(jp)    = 1.0
          prey_refuge(jp)     = 1.0
          pp_optC(jp)         = 10.0
          pp_sig_C(jp)        = 2.0
          !grazing_protect(jp) = 1.0
          mort_protect(jp)    = trophic_tradeoff
          switch_feeding(jp)  = 0.0
       elseif (pft(jp).eq.'zooplankton') then
          NO3up(jp)           = 0.0
          Nfix(jp)            = 0.0
          calcify(jp)         = 0.0
          silicify(jp)        = 0.0
          autotrophy(jp)      = 0.0
	        heterotrophy(jp)    = 1.0               !omnivory
          herbivory (jp)      = 0.0
	        carnivory (jp)      = 0.0
          palatability(jp)    = 1.0
          prey_refuge(jp)     = 1.0
          pp_optC(jp)         = 10.0              !optimum PP ratio
          pp_sig_C(jp)        = 2.0               !standard deviation for PP ratio
          !grazing_protect(jp) = 1.0
          mort_protect(jp)    = trophic_tradeoff
          switch_feeding(jp)  = 2.0               !active vs passive
       elseif (pft(jp).eq.'mixotroph') then
          NO3up(jp)           = 0.0
          Nfix(jp)            = 0.0
          calcify(jp)         = 0.0
          silicify(jp)        = 0.0
          autotrophy(jp)      = 0.0
	  heterotrophy(jp)    = 1.0               !omnivory
          herbivory (jp)      = 0.0
	  carnivory (jp)      = 0.0
          palatability(jp)    = 1.0
          prey_refuge(jp)     = 1.0
          pp_optC(jp)         = 10.0              !optimum PP ratio
          pp_sig_C(jp)        = 2.0               !standard deviation for PP ratio
          !grazing_protect(jp) = 1.0
          mort_protect(jp)    = trophic_tradeoff
          switch_feeding(jp)  = 2.0               !active vs passive
       elseif (pft(jp).eq.'foram') then
          NO3up(jp)           = 0.0
          Nfix(jp)            = 0.0
          calcify(jp)         = 0.0
          silicify(jp)        = 0.0
          autotrophy(jp)      = 0.0
	  heterotrophy(jp)    = trophic_tradeoff  *0.9   !omnivory
          herbivory (jp)      = 1.0
	  carnivory (jp)      = 0.0
          palatability(jp)    = 0.8
          prey_refuge(jp)     = 1.0
          pp_optC(jp)         = 10.0              !optimum PP ratio
          pp_sig_C(jp)        = 6.0               !standard deviation for PP ratio
          !grazing_protect(jp) = 1.0
          mort_protect(jp)    = trophic_tradeoff * 0.70
          switch_feeding(jp)  = 1.0               !active vs passive
       else
          print*," "
          print*,"! ERROR !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
          print*,"! Unknown plankton functional type '"//trim(pft(jp))//"'"
          print*,"! Specified in input file "//TRIM(par_indir_name)//TRIM(par_ecogem_plankton_file)
          print*,"Choose from Prochlorococcus, Synechococcus, Picoeukaryote, Diatom, Coccolithophore, Diazotroph, Phytoplankton, Zooplankton or Mixotroph"
          print*,"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
          stop
       endif
    enddo

    ! initialise plankton parameters
    qmin(:,:)     = 0.0
    qmax(:,:)     = 0.0
    vmax(:,:)     = 0.0
    affinity(:,:) = 0.0
    kexc(:,:)     = 0.0
    qcarbon(:)    = 0.0
    alphachl(:)   = 0.0
    graz(:)       = 0.0
    kg(:)         = 0.0
    pp_opt(:)     = 0.0
    pp_sig(:)     = 0.0
    respir(:)     = 0.0
    biosink(:)    = 0.0
    mort(:)       = 0.0

    !-----------------------------------------------------------------------------------------
    ! maximum photosynthetic rate
    !    vmax(iDIC,:)    = vmaxDIC_a * volume(:) ** vmaxDIC_b * autotrophy(:)
    vmax(iDIC,:)    = (vmaxDIC_a  + log10(volume(:))) / (vmaxDIC_b + vmaxDIC_c * log10(volume(:)) + log10(volume(:))**2) * autotrophy(:)
    !-----------------------------------------------------------------------------------------
    if (nquota) then ! nitrogen parameters
       qmin(iNitr,:)      =    qminN_a * volume(:) **    qminN_b
       qmax(iNitr,:)      =    qmaxN_a * volume(:) **    qmaxN_b
       if (maxval((qmin(iNitr,:)/qmax(iNitr,:))).gt.1.0) print*,"WARNING: Nitrogen Qmin > Qmax. Population inviable!"
       if (useNO3) then ! nitrate parameters
          vmax(iNO3,:)     =  vmaxNO3_a * volume(:) **  vmaxNO3_b * autotrophy(:) * NO3up(:)
          affinity(iNO3,:) = affinNO3_a * volume(:) ** affinNO3_b * autotrophy(:) * NO3up(:)
       endif
       if (useNO2) then ! nitrite parameters
          vmax(iNO2,:)     =  vmaxNO2_a * volume(:) **  vmaxNO2_b * autotrophy(:)
          affinity(iNO2,:) = affinNO2_a * volume(:) ** affinNO2_b * autotrophy(:)
       endif
       if (useNH4) then ! ammonium parameters
          vmax(iNH4,:)     =  vmaxNH4_a * volume(:) **  vmaxNH4_b * autotrophy(:)
          affinity(iNH4,:) = affinNH4_a * volume(:) ** affinNH4_b * autotrophy(:)
       endif
       kexc(iNitr,:)      =    kexcN_a * volume(:) **    kexcN_b

       !      mumax(iNitr,:) = vmax(iDIC,:)*vmax(iNO3,:) &
       !                     & /(vmax(iDIC,:)*Qmin(iNitr,:) + vmax(iNO3,:)*qmax(iNitr,:)/(qmax(iNitr,:)-qmin(iNitr,:)))
       !      alpha(iNitr,:) = affinity(iNitr,:)/Qmin(iNitr,:)
    endif
    !-----------------------------------------------------------------------------------------
    if (pquota) then ! phosphorus parameters
       qmin(iPhos,:)    =   qminP_a  * volume(:) **    qminP_b
       qmax(iPhos,:)    =   qmaxP_a  * volume(:) **    qmaxP_b
       if (maxval((qmin(iPhos,:)/qmax(iPhos,:))).gt.1.0) print*,"WARNING: Phosphate Qmin > Qmax. Population inviable!"
       vmax(iPO4,:)     = vmaxPO4_a  * volume(:) **  vmaxPO4_b * autotrophy(:)
       affinity(iPO4,:) = affinPO4_a * volume(:) ** affinPO4_b * autotrophy(:)
       kexc(iPhos,:)    =   kexcP_a  * volume(:) **    kexcP_b
    endif
    !-----------------------------------------------------------------------------------------
    if (fquota) then ! iron parameters
       qmin(iIron,:)   =  qminFe_a * volume(:) **  qminFe_b
       qmax(iIron,:)   =  qmaxFe_a * volume(:) **  qmaxFe_b
       if (maxval((qmin(iIron,:)/qmax(iIron,:))).gt.1.0) print*,"WARNING: Iron Qmin > Qmax. Population inviable!"
       vmax(iFe,:)     =  vmaxFe_a * volume(:) **  vmaxFe_b * autotrophy(:)
       affinity(iFe,:) = affinFe_a * volume(:) ** affinFe_b * autotrophy(:)
       kexc(iIron,:)   =  kexcFe_a * volume(:) **  kexcFe_b
    endif
    !-----------------------------------------------------------------------------------------
    if (squota) then ! silicon parameters
       qmin(iSili,:)     =   qminSi_a * volume(:) **    qminSi_b                 * silicify(:)
       qmax(iSili,:)     =   qmaxSi_a * volume(:) **    qmaxSi_b                 * silicify(:)
       if (maxval((qmin(iSili,:)/qmax(iSili,:))).gt.1.0) print*,"WARNING: Silicon Qmin > Qmax. Population inviable!"
       vmax(iSiO2,:)     = vmaxSiO2_a * volume(:) **  vmaxSiO2_b * autotrophy(:) * silicify(:)
       affinity(iSiO2,:) =affinSiO2_a * volume(:) ** affinSiO2_b * autotrophy(:)
       kexc(iSili,:)     =  kexcSi_a  * volume(:) **    kexcSi_b                 * silicify(:)
    endif
    !-----------------------------------------------------------------------------------------

    ! other parameters
    qcarbon(:)  =     qcarbon_a * volume(:) ** qcarbon_b
    alphachl(:) =    alphachl_a * volume(:) ** alphachl_b
    graz(:)     =        graz_a * volume(:) ** graz_b     * heterotrophy(:)
    kg(:)       =          kg_a * volume(:) ** kg_b
    !pp_opt(:)   =      pp_opta * volume(:) ** pp_opt_b      !commented out for foram project by Grigoratou, Dec2018, see ppot_mat defined below
    !pp_sig(:)   =      pp_sig_C * volume(:) ** pp_sig_b     !commented out for foram project by Grigoratou, Dec2018, see ppsig_mat defined below
    respir(:)   =      respir_a * volume(:) ** respir_b
    biosink(:)  =     biosink_a * volume(:) ** biosink_b
    mort(:)     =        (mort_a * volume(:) ** mort_b) * mort_protect(:) ! mort_protect added by Grigoratou, Dec2018 as a benefit for foram's calcification
    do jp=1,npmax ! grazing kernel (npred,nprey)
       ! pad predator dependent pp_opt and pp_sig so that they vary along matrix columns
       ! (they should be constant within each row)
       ppopt_mat(:,jp)=pp_optC(jp) * volume(:) ** pp_opt_b   !added an optimal predator-prey length ratio for each plankton group, Grigoratou, Dec18
       ppsig_mat(:,jp)=pp_sig_C(jp) * volume(:) ** pp_sig_b  !added an optimal standar deviation for predator-prey length ratio for each plankton group, Grigoratou, Dec18
    enddo
    pred_diam(:,1)=diameter(:) ! standard  prey diameter vector
    prey_diam(1,:)=diameter(:) ! transpose pred diameter vector
    prdpry(:,:)   =matmul(pred_diam,1.0/prey_diam)
    gkernel(:,:)  =exp(-log(prdpry(:,:)/ppopt_mat(:,:))**2 / (2*ppsig_mat(:,:)**2)) ! [jpred,jprey] populate whole array at once, then find exceptions to set to 0.0 based on type
    do jpred=1,npmax
    do jprey=1,npmax
    select case(pft(jpred))
      case('phytoplankton')
        gkernel(jpred,jprey)=0.0
      case('zooplankton','foram')
        select case(pft(jprey))
          case('phytoplankton')
            if(carnivory(jpred).gt.0.0) gkernel(jpred,jprey)=0.0 ! if predator is carnivorous and prey is phytoplankton, - no grazing
          case('zooplankton', 'foram')
            if(herbivory(jpred).gt.0.0) gkernel(jpred,jprey)=0.0 ! if predator is herbivorous and prey is zooplanktonn - no grazing
        end select
    end select
    end do
    end do
    gkernel(:,:)  =merge(gkernel(:,:),0.0,gkernel(:,:).gt.1e-2) ! set kernel<1e-2 to 0.0
    gkernelT(:,:) =transpose(gkernel(:,:))

    ! detrital partitioning
    beta_graz(:) =beta_graz_a - (beta_graz_a-beta_graz_b) / (1.0+beta_mort_c/diameter(:))
    beta_mort(:) =beta_mort_a - (beta_mort_a-beta_mort_b) / (1.0+beta_mort_c/diameter(:))

    ! ****************************************************************************************
    ! ****************************************************************************************
    ! Write plankton parameters to output file (opened in initialise_ecogem)
    ! parameter headers
    WRITE(301,301,ADVANCE = "NO" ),"PFT             "
    WRITE(301,202,ADVANCE = "NO" ),"    diameter","      volume"
    WRITE(301,201,ADVANCE = "NO" ),"      vmax_C"
    if (nquota) then
       WRITE(301,202,ADVANCE = "NO" ),"      qmin_N","      qmax_N"
       if (useNO3) WRITE(301,202,ADVANCE = "NO" ),"    vmax_NO3","   affin_NO3"
       if (useNO2) WRITE(301,202,ADVANCE = "NO" ),"    vmax_NO2","   affin_NO2"
       if (useNH4) WRITE(301,202,ADVANCE = "NO" ),"    vmax_NH4","   affin_NH4"
       WRITE(301,201,ADVANCE = "NO" ),"      kexc_N"
    endif
    if (pquota) then
       WRITE(301,205,ADVANCE = "NO" ),"      qmin_P","      qmax_P","    vmax_PO4","   affin_PO4","      kexc_P"
    endif
    if (fquota) then
       WRITE(301,205,ADVANCE = "NO" ),"     qmin_Fe","     qmax_Fe","     vmax_Fe","    affin_Fe","     kexc_Fe"
    endif
    if (squota) then
       WRITE(301,205,ADVANCE = "NO" ),"     qmin_Si","     qmax_Si","   vmax_SiO4","  affin_SiO4","     kexc_Si"
    endif
    WRITE(301,202,ADVANCE = "NO" ),"         q_C","    alphachl"
    WRITE(301,204,ADVANCE = "NO" ),"  max_graz_C","        kg_C","      pp_opt","      pp_sig"
    WRITE(301,203,ADVANCE = "NO" )," respiration","     biosink","   mortality"
    WRITE(301,202,ADVANCE = "YES"),"   beta_graz","   beta_mort"
    ! ****************************************************************************************
    ! parameter values
    do n=1,2
       do jp=1,npmax
          if (n.eq.1)     WRITE(300+n,301,ADVANCE = "NO" ),pft(jp)
          WRITE(300+n,102,ADVANCE = "NO" ),diameter(jp),volume(jp)
          WRITE(300+n,101,ADVANCE = "NO" ),vmax(iDIC,jp)
          if (nquota) then
             WRITE(300+n,102,ADVANCE = "NO" ),qmin(iNitr,jp),qmax(iNitr,jp)
             if (useNO3) WRITE(300+n,102,ADVANCE = "NO" ),vmax(iNO3,jp),affinity(iNO3,jp)
             if (useNO2) WRITE(300+n,102,ADVANCE = "NO" ),vmax(iNO2,jp),affinity(iNO2,jp)
             if (useNH4) WRITE(300+n,102,ADVANCE = "NO" ),vmax(iNO3,jp),affinity(iNO3,jp)
             WRITE(300+n,101,ADVANCE = "NO" ),kexc(iNitr,jp)
          endif
          if (pquota) then
             WRITE(300+n,105,ADVANCE = "NO" ),qmin(iPhos,jp),qmax(iPhos,jp),vmax(iPO4,jp),affinity(iPO4,jp),kexc(iPhos,jp)
          endif
          if (fquota) then
             WRITE(300+n,105,ADVANCE = "NO" ),qmin(iIron,jp),qmax(iIron,jp),vmax(iFe,jp),affinity(iFe,jp),kexc(iIron,jp)
          endif
          if (squota) then
             WRITE(300+n,105,ADVANCE = "NO" ),qmin(iSili,jp),qmax(iSili,jp),vmax(iSiO2,jp),affinity(iSiO2,jp),kexc(iSili,jp)
          endif
          WRITE(300+n,102,ADVANCE = "NO" ),qcarbon(jp),alphachl(jp)
          WRITE(300+n,104,ADVANCE = "NO" ),graz(jp),kg(jp),pp_opt(jp),pp_sig(jp)
          WRITE(300+n,103,ADVANCE = "NO" ),respir(jp),biosink(jp),mort(jp)
          WRITE(300+n,102,ADVANCE = "YES"),beta_graz(jp),beta_mort(jp)
       enddo
    enddo
    ! close plankton parameter files
    close(301)
    close(302)

    ! write out grazing matrix
    do jpred=1,npmax
	do jprey=1,npmax-1
		WRITE(303,101,ADVANCE = "NO" ),gkernel(jpred,jprey)
       enddo
       WRITE(303,101,ADVANCE = "YES" ),gkernel(jpred,npmax)
    end do
    close(303)



     ! grazing matrix WARD17 copy
    ! do jpred=1,npmax
    ! print*,jpred,jprey
    !    if (heterotrophy(jpred).le.0.0) then
    !       gkernel(jpred,:) = 0.0
     !       endif
    !    endif
    !    do jprey=1,npmax-1
    !       WRITE(303,101,ADVANCE = "NO" ),gkernel(jpred,jprey)
    !    enddo
    !    WRITE(303,101,ADVANCE = "YES" ),gkernel(jpred,npmax)
    ! enddo
    ! close(303)
    ! ****************************************************************************************
    ! ****************************************************************************************

    !-------------------------------------------------
    ! convert all rates form per day to per second
    vmax(:,:)     = vmax(:,:)     / pday
    affinity(:,:) = affinity(:,:) / pday
    kexc(:,:)     = kexc(:,:)     / pday
    graz(:)       = graz(:)       / pday
    respir(:)     = respir(:)     / pday
    biosink(:)    = biosink(:)    / pday
    mort(:)       = mort(:)       / pday
    !-------------------------------------------------

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! initialise plankton state variables with maximum quotas
    plankton(:,:,:,:,:) = 0.0
    plankton(iCarb, :,:,:,n_k-n_keco+1:n_k) = 1.0e-1
    do jp=1,npmax
       if (chlquota) plankton(iChlo,jp,:,:,n_k-n_keco+1:n_k) = chl2nmax / 6.625 * plankton(iCarb,jp,:,:,n_k-n_keco+1:n_k)
       if (nquota)   plankton(iNitr,jp,:,:,n_k-n_keco+1:n_k) = qmin(iNitr,jp)   * plankton(iCarb,jp,:,:,n_k-n_keco+1:n_k)
       if (pquota)   plankton(iPhos,jp,:,:,n_k-n_keco+1:n_k) = qmin(iPhos,jp)   * plankton(iCarb,jp,:,:,n_k-n_keco+1:n_k)
       if (squota)   plankton(iSili,jp,:,:,n_k-n_keco+1:n_k) = qmin(iSili,jp)   * plankton(iCarb,jp,:,:,n_k-n_keco+1:n_k)
       if (fquota)   plankton(iIron,jp,:,:,n_k-n_keco+1:n_k) = qmin(iIron,jp)   * plankton(iCarb,jp,:,:,n_k-n_keco+1:n_k)
    enddo

    !print*, 'plankiso(iCarb13C)     ', plankiso(iCarb13C, :,:,:,:)

    ! set non ocean cells to zero
    do io=1,iomax+iChl
       do jp=1,npmax
          plankton(io,jp,:,:,:) = plankton(io,jp,:,:,:)*real(wet_mask_ijk(:,:,:))
       enddo
    enddo


    if (c13trace) then !ckc initialise carbon 13 for full food web tracing
       plankiso(:,:,:,:,:) = 0.0
       plankiso(iCarb13C, :,:,:,n_k-n_keco+1:n_k) = plankton(iCarb, :,:,:,n_k-n_keco+1:n_k) * 0.0109 !about -24permil
       !plankiso initialisation works here, but when it gets to ecogem.f90 its got weird...
       !print*, 'plankiso(iCarb13C)     ', plankiso(iCarb13C, :,:,:,:)
    endif


301 format( 1a16)

201 format( 1a12)
202 format( 2a12)
203 format( 3a12)
204 format( 4a12)
205 format( 5a12)

101 format( 1e12.3)
102 format( 2e12.3)
103 format( 3e12.3)
104 format( 4e12.3)
105 format( 5e12.3)

  END SUBROUTINE sub_init_plankton

  ! ****************************************************************************************************************************** !
  ! DEFINE AND INITIALIZE PLANKTON POPULATIONS FROM INPUT FILE
  SUBROUTINE sub_init_populations()
    ! local variables
    INTEGER::n
    INTEGER           :: loc_n_elements,loc_n_start
    CHARACTER(len=16) :: loc_plnktn_pft
    REAL              :: loc_plnktn_size
    INTEGER           :: loc_plnktn_n
    CHARACTER(len=255)::loc_filename
    ! check file format and determine number of lines of data
    loc_filename = TRIM(par_indir_name)//"/"//TRIM(par_ecogem_plankton_file)
    CALL sub_check_fileformat(loc_filename,loc_n_elements,loc_n_start)

    if (loc_n_elements.eq.0) then
       print*," "
       print*,"! ERROR !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
       print*,"! No plankton types specified in input file ",TRIM(par_indir_name)//"/"//TRIM(par_ecogem_plankton_file)
       print*,"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
       stop
    endif

    ! open file pipe
    OPEN(unit=in,file=loc_filename,action='read')
    ! goto start-of-file tag
    DO n = 1,loc_n_start
       READ(unit=in,fmt='(1X)')
    END DO

    ALLOCATE(     pft(loc_n_elements),STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(diameter(loc_n_elements),STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)
    ALLOCATE(random_n(loc_n_elements),STAT=alloc_error)
    call check_iostat(alloc_error,__LINE__,__FILE__)

    ! re-set filepipe
    REWIND(unit=in)
    ! goto start-of-file tag
    DO n = 1,loc_n_start
       READ(unit=in,fmt='(1X)')
    END DO
    ! read in population specifications
    DO n = 1,loc_n_elements
       READ(unit=in,FMT=*)         &
            & loc_plnktn_pft,      & ! COLUMN #02: plankton PFT
            & loc_plnktn_size,     & ! COLUMN #01: plankton size
            & loc_plnktn_n           ! COLUMN #03: number of randomised replicates
       pft(n)      = loc_plnktn_pft
       diameter(n) = loc_plnktn_size
       random_n(n) = loc_plnktn_n
    END DO
    npmax=loc_n_elements
    ! close file pipe
    CLOSE(unit=in)
  END SUBROUTINE sub_init_populations


  ! ****************************************************************************************************************************** !
  ! LOAD TIME-SERIES LOCATIONS FROM INPUT FILE SUBROUTINE sub_init_timeseries()
  SUBROUTINE sub_init_timeseries()
    ! local variables
    INTEGER::n
    INTEGER           :: loc_n_elements,loc_n_start
    CHARACTER(len=16) :: loc_tser_name
    REAL              :: loc_tser_lat,loc_tser_lon,tmp
    CHARACTER(len=255)::loc_filename
    real,dimension(1:n_i)::loc_lon
    real,dimension(1:n_j)::loc_lat

    ! get grid coordinates
    loc_lon(1:n_i) = fun_get_grid_lon(n_i)
    loc_lat(1:n_j) = fun_get_grid_lat(n_j)

    ! check file format and determine number of lines of data
    loc_filename = TRIM(par_indir_name)//"/timeseries_sites.eco"!//TRIM(par_ecogem_timeseries_file)
    CALL sub_check_fileformat(loc_filename,loc_n_elements,loc_n_start)

    ! open file pipe
    OPEN(unit=in,file=loc_filename,action='read')
    ! goto start-of-file tag
    DO n = 1,loc_n_start
       READ(unit=in,fmt='(1X)')
    END DO

    n_tser=loc_n_elements

    if (n_tser.gt.0) then
       ALLOCATE(tser_name(n_tser),STAT=alloc_error)
       call check_iostat(alloc_error,__LINE__,__FILE__)
       ALLOCATE(tser_i(n_tser),STAT=alloc_error)
       call check_iostat(alloc_error,__LINE__,__FILE__)
       ALLOCATE(tser_j(n_tser),STAT=alloc_error)
       call check_iostat(alloc_error,__LINE__,__FILE__)

       ! re-set filepipe
       REWIND(unit=in)
       ! goto start-of-file tag
       DO n = 1,loc_n_start
          READ(unit=in,fmt='(1X)')
       END DO

       ! read in population specifications
       if ((ctrl_debug_init > 0) .OR. ctrl_debug_eco_init) then
          write(*,*), ' >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>'
          print*,"Time-series output locations (shifted to sit of GEnIE grid)"
       endif
       DO n = 1,n_tser
          READ(unit=in,FMT=*)      &
               & loc_tser_name,    & ! COLUMN #02: time series name
               & loc_tser_lat,     & ! COLUMN #01: time series lat
               & loc_tser_lon        ! COLUMN #03: time series lon
          tser_name(n) = TRIM(loc_tser_name)
          if (loc_tser_lon.gt.maxval(loc_lon)) loc_tser_lon = loc_tser_lon - 360.00
          tser_i(n) = minloc(abs(loc_tser_lon-loc_lon), DIM=1)
          tser_j(n) = minloc(abs(loc_tser_lat-loc_lat), DIM=1)
          if ((ctrl_debug_init > 0) .OR. ctrl_debug_eco_init) then
             print*,tser_name(n),loc_lat(tser_j(n)),loc_lon(tser_i(n))
          endif
       END DO
       if ((ctrl_debug_init > 0) .OR. ctrl_debug_eco_init) then
          write(*,*), ' >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>'
       endif
    endif
    ! close file pipe
    CLOSE(unit=in)
  END SUBROUTINE sub_init_timeseries



  ! ****************************************************************************************************************************** !
  ! INITIALIZE INTEGRATED TIME-SLICE VALUE ARRAYS
  SUBROUTINE sub_init_int_timeslice()

    ! initialize integrated time
    int_t_timeslice = 0.0
    int_t_timeslice_count = 0
    ! initialize time-slice data - ocn
    int_plankton_timeslice(:,:,:,:,:) = 0.0
    int_uptake_timeslice(:,:,:,:,:)   = 0.0
    int_gamma_timeslice(:,:,:,:,:)    = 0.0
    int_nutrient_timeslice(:,:,:,:)   = 0.0
    int_zoogamma_timeslice(:,:,:,:)   = 0.0
    int_export_timeslice(:,:,:,:,:)   = 0.0  !Fanny/Maria - Aug19

    ! ### ADD ADDITIONAL TIME-SLICE ARRAY INITIALIZATIONS HERE ################################################################### !
    !
    ! ############################################################################################################################ !
  END SUBROUTINE sub_init_int_timeslice
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALIZE 'PHYSICS' - OCEAN
  SUBROUTINE sub_init_phys_ocn()
    ! local variables
    INTEGER::i,j
    REAL::loc_grid_area
    ! initialize local variables
    loc_grid_dz(1:n_k)  = 0.0
    loc_grid_dz(1:n_k)  = goldstein_dz(:)
    ! zero array
    ocn_grid_vol(:,:,:) = 0.0
    ! initialize array values
    ! NOTE: initialize basic grid structure values for the (i,j,k) grid, not just ocean-only points
    ! NOTE: depth in in unit of m BELOW sealevel (i.e., a +ve scale)
    ! NOTE: set default rho
    DO i=1,n_i
       DO j=1,n_j
          loc_grid_area = 2.0*const_pi*(const_rEarth**2)*(1.0/n_i)*(goldstein_sv(j) - goldstein_sv(j-1))
          ocn_grid_vol(i,j,:) = loc_grid_area * goldstein_dz(:) * goldstein_dsc
          !print*,ocn_grid_vol(i,j,n_k)
       END DO
    END DO

  END SUBROUTINE sub_init_phys_ocn
  ! ****************************************************************************************************************************** !

  ! ****************************************************************************************************************************** !
  ! READ IN TEMPERATURE FORCING FILE - JDW
  subroutine sub_init_load_forceT()
  ! local variables
  integer::i,j,loc_n_elements,loc_n_start,ios
  character(LEN=127)::loc_filename

  loc_filename = TRIM(par_indir_name)//TRIM(par_ecogem_force_T_file)
  !CALL sub_check_fileformat(loc_filename,loc_n_elements,loc_n_start)

  ! open file pipe
  OPEN(unit=in,file=loc_filename,action='read',iostat=ios)
  call check_iostat(alloc_error,__LINE__,__FILE__)
  READ(unit=in,fmt=*,IOSTAT=ios) T_input

  ! close file pipe
  CLOSE(unit=in,iostat=ios)
  call check_iostat(ios,__LINE__,__FILE__)

  end subroutine sub_init_load_forceT
  ! ****************************************************************************************************************************** !

END MODULE ecogem_data
