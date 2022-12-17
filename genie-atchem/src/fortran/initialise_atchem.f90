
! ******************************************************************************************************************************** !
! SETUP AtChem
SUBROUTINE initialise_atchem( &
     & dum_c,dum_cv,dum_s,dum_sv,                   &
     & dum_sfxsumatm,         &
     & dum_sfcatm             &
     & )
  USE atchem_lib
  USE atchem_data
  use atchem_data_netCDF
  ! ---- YK added 02.08.2021 -----
  use genie_util, only: check_unit
  ! ----- End addition -----
  ! dummy arguments
  REAL,DIMENSION(0:n_j),INTENT(in)::dum_c,dum_cv,dum_s,dum_sv    ! 
  real,dimension(n_atm,n_i,n_j),intent(inout)::dum_sfxsumatm ! atmosphere-surface fluxes; integrated, atm grid
  real,dimension(n_atm,n_i,n_j),intent(inout)::dum_sfcatm    ! atmosphere-surface tracer composition; atm grid
  
  ! ---- YK added 02.10.2021 -----
  real,dimension(6)::dum_slab                                ! slab data read from restart 
  integer::io                                                ! checking the file 
  ! ----- End addition -----

  print*,'======================================================='
  print*,' >>> Initialising ATCHEM atmospheric chem. module ...'
  
  ! *** load goin information ***
  call sub_load_goin_atchem()
  
  ! ---------------------------------------------------------- !
  ! *** copy GOLDSTEIn parameters ***
  ! ---------------------------------------------------------- !
  ! ---------------------------------------------------------- ! copy grid information
  goldstein_c(:)   = dum_c(:)
  goldstein_cv(:)  = dum_cv(:)
  goldstein_s(:)   = dum_s(:)
  goldstein_sv(:)  = dum_sv(:)
  ! ---------------------------------------------------------- !

  ! *** initialize AtChem ***
  CALL sub_init_phys_atm()
  CALL sub_init_tracer_atm_comp()

  ! *** load restart information ***
  IF (ctrl_continuing) then
     call sub_data_load_rst()
  end if

  ! *** initialize external interface arrays ***
  dum_sfxsumatm(:,:,:) = 0.0
  dum_sfcatm(:,:,:)    = atm(:,:,:)
              
  ! *** initialize MISC ***
  call sub_init_slabbiosphere()
  
  ! ---- YK added 02.08.2021 -----
  if (par_atm_slabON) then 
     
     call sub_init_slabbiosphere_box()
     
     if (par_atm_slab_hetero) call sub_init_slabbiosphere_hetero()
     
     if (par_atm_slabsave) then 
        call system ('mkdir -p '//trim(adjustl(par_outdir_name))//'/tem/')
        utest = 100
        call check_unit(utest)
        open(unit=utest,file=trim(adjustl(par_outdir_name))//'/tem/terFLX.res',action='write',status='replace')
        write(utest,*) 'time / T (oC)/ pCO2 (atm) / NPP (mol yr-1) / resp (mol yr-1) / Net CO2 flux (mol yr-1)'
        close(utest)
        open(unit=utest,file=trim(adjustl(par_outdir_name))//'/tem/terFLXg.res',action='write',status='replace')
        write(utest,*) 'time / T (oC)/ pCO2 (atm) / NPP (PgC yr-1) / resp (PgC yr-1) / Net CO2 flux (PgC yr-1)'
        close(utest)
        open(unit=utest,file=trim(adjustl(par_outdir_name))//'/tem/terPOOl.res',action='write',status='replace')
        write(utest,*) 'time / T (oC)/ pCO2 (atm) / Vegie (mol) / Litter (mol) / Total terrestrial OM (mol)'
        close(utest)
        open(unit=utest,file=trim(adjustl(par_outdir_name))//'/tem/terPOOlg.res',action='write',status='replace')
        write(utest,*) 'time / T (oC)/ pCO2 (atm) / Vegie (PgC) / Litter (PgC) / Total terrestrial OM (PgC)'
        close(utest)
     endif 
     
     if (par_atm_slab_restart) call sub_load_terrbio()
     
  endif 
  ! ----- End addition -----

  print*,' <<< Initialisation complete'
  print*,'======================================================='

end SUBROUTINE initialise_atchem
! ******************************************************************************************************************************** !
