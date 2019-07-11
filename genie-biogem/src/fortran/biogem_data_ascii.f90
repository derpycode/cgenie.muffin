! ******************************************************************************************************************************** !
! biogem_data_ascii.f90
! BioGeM
! DATA LOADING/SAVING ROUTINES -- ASCII OUTPUT
! ******************************************************************************************************************************** !


MODULE biogem_data_ascii


  USE biogem_lib
  USE biogem_box
  USE biogem_data_netCDF
  IMPLICIT NONE
  SAVE


CONTAINS


  ! ****************************************************************************************************************************** !
  ! INITIALIZE DATA SAVING
  SUBROUTINE sub_init_data_save_runtime()
    USE genie_util, ONLY:check_unit,check_iostat
    ! local variables
    INTEGER::l,io,ia,is,ic,ios,idm2D
    integer::ib,id
    CHARACTER(len=255)::loc_filename
    CHARACTER(len=255)::loc_string
    real::loc_t = 0.0
    logical::loc_save

    ! tracer
    IF (ctrl_data_save_sig_ocn) THEN
       DO l=1,n_l_ocn
          io = conv_iselected_io(l)
          loc_filename=fun_data_timeseries_filename( &
               & loc_t,par_outdir_name,trim(par_outfile_name)//'_series','ocn_'//TRIM(string_ocn(io)),string_results_ext &
               & )
          IF (ctrl_data_save_sig_ocn_sur) THEN
             SELECT CASE (ocn_type(io))
             CASE (0)
                If (io == io_T) loc_string = &
                     & '% time (yr) / temperature (C) / _surT (ice-free) (C) / _benT (C) / _surT (C)'
                If (io == io_S) loc_string = &
                     & '% time (yr) / salinity (o/oo) / _surS (ice-free) (o/oo) / _benS (o/oo) / _surS (o/oo)'
             CASE (1)
                loc_string = '% time (yr) / ' //&
                     & 'global total ' //TRIM(string_ocn(io))//' (mol) / ' //&
                     & 'global mean ' //TRIM(string_ocn(io))//' (mol kg-1) / ' //&
                     & 'surface (ice-free) ' //TRIM(string_ocn(io))//' (mol kg-1) / ' //&
                     & 'benthic ' //TRIM(string_ocn(io))//' (mol kg-1) / ' //&
                     & 'surface ' //TRIM(string_ocn(io))//' (mol kg-1)'
             CASE (n_itype_min:n_itype_max)
                loc_string = '% time (yr) / ' //&
                     & 'global total '//TRIM(string_ocn(io))//' (mol) / ' //&
                     & 'global ' // TRIM(string_ocn(io))//' (o/oo) / ' //&
                     & 'surface (ice-free) ' //TRIM(string_ocn(io))//' (o/oo) / ' //&
                     & 'benthic ' //TRIM(string_ocn(io))//' (o/oo) / ' //&
                     & 'surface ' //TRIM(string_ocn(io))//' (o/oo)'
             end SELECT
          else
             SELECT CASE (ocn_type(io))
             CASE (0)
                If (io == io_T) loc_string = '% time (yr) / temperature (degrees C)'
                If (io == io_S) loc_string = '% time (yr) / salinity (o/oo)'
             CASE (1)
                loc_string = '% time (yr) / global total '//TRIM(string_ocn(io))//' (mol) / global mean ' // &
                     & TRIM(string_ocn(io))//' (mol kg-1)'
             CASE (n_itype_min:n_itype_max)
                loc_string = '% time (yr) / global total '//TRIM(string_ocn(io))//' (mol) / global ' // &
                     & TRIM(string_ocn(io))//' (o/oo)'
             end SELECT
          end IF
          SELECT CASE (ocn_type(io))
          CASE (0,1,n_itype_min:n_itype_max)
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             write(unit=out,fmt=*,iostat=ios) trim(loc_string)
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
          end SELECT
       END DO
    END IF
    ! atmospheric tracer
    IF (ctrl_data_save_sig_ocnatm) THEN
       DO l=1,n_l_atm
          ia = conv_iselected_ia(l)
          loc_filename=fun_data_timeseries_filename( &
               & loc_t,par_outdir_name,trim(par_outfile_name)//'_series','atm_'//TRIM(string_atm(ia)),string_results_ext &
               & )
          SELECT CASE (atm_type(ia))
          CASE (0)
             If (ia == ia_T) loc_string = '% time (yr) / surface air temperature (degrees C)'
             If (ia == ia_q) loc_string = '% time (yr) / surface humidity (???)'
             If (ia == ia_pcolr) &
                  & loc_string = '% time (yr) / global '//TRIM(string_atm(ia))//' (mol) / global ' //TRIM(string_atm(ia))//' (atm)'
          CASE (1)
             loc_string = '% time (yr) / global '//TRIM(string_atm(ia))//' (mol) / global ' //TRIM(string_atm(ia))//' (atm)'
          CASE (n_itype_min:n_itype_max)
             loc_string = '% time (yr) / global '//TRIM(string_atm(ia))//' (mol) / global ' //TRIM(string_atm(ia))//' (o/oo)'
          end SELECT
          SELECT CASE (atm_type(ia))
          CASE (0,1,n_itype_min:n_itype_max)
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             write(unit=out,fmt=*,iostat=ios) trim(loc_string)
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
          end SELECT
       END DO
    END IF
    ! export flux
    IF (ctrl_data_save_sig_fexport) THEN
       DO l=1,n_l_sed
          is = conv_iselected_is(l)
          loc_filename=fun_data_timeseries_filename( &
               & loc_t,par_outdir_name,trim(par_outfile_name)//'_series','fexport_'//TRIM(string_sed(is)),string_results_ext &
               & )
          SELECT CASE (sed_type(is))
          CASE (par_sed_type_bio,par_sed_type_abio, &
               & par_sed_type_POM,par_sed_type_CaCO3,par_sed_type_opal,par_sed_type_det, &
               & par_sed_type_scavenged)
             loc_string = '% time (yr) / global '//TRIM(string_sed(is))//' flux (mol yr-1) / global '// &
                  & TRIM(string_sed(is))//' density (mol m-2 yr-1) / global ' // TRIM(string_sed(is)) // ' DOM fraction'
          CASE (par_sed_type_age)
             loc_string = '% time (yr) / CaCO3 age (yr)'
          CASE (n_itype_min:n_itype_max)
             loc_string = '% time (yr) / global '//TRIM(string_sed(is))//' flux (mol yr-1) / global '// &
                  & TRIM(string_sed(is))//' delta (o/oo)'
          end SELECT
          SELECT CASE (sed_type(is))
          CASE (par_sed_type_bio,par_sed_type_abio, &
               & par_sed_type_POM,par_sed_type_CaCO3,par_sed_type_opal,par_sed_type_det, &
               & par_sed_type_scavenged,par_sed_type_age,n_itype_min:n_itype_max)
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             write(unit=out,fmt=*,iostat=ios) trim(loc_string)
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
          end SELECT
       END DO
       ! export rain ratios
       if (sed_select(is_POC) .AND. sed_select(is_CaCO3)) then
          loc_filename=fun_data_timeseries_filename( &
               & loc_t,par_outdir_name,trim(par_outfile_name)//'_series','misc_CaCO3toPOC',string_results_ext)
          loc_string = '% time (yr) / CaCO3/POC ratio / POC/CaCO3 ratio'
          call check_unit(out,__LINE__,__FILE__)
          OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          write(unit=out,fmt=*,iostat=ios) trim(loc_string)
          call check_iostat(ios,__LINE__,__FILE__)
          CLOSE(unit=out,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
       end if
       if (sed_select(is_POC) .AND. sed_select(is_opal)) then
          loc_filename=fun_data_timeseries_filename( &
               & loc_t,par_outdir_name,trim(par_outfile_name)//'_series','misc_opaltoPOC',string_results_ext)
          loc_string = '% time (yr) / opal/POC ratio / POC/opal ratio'
          call check_unit(out,__LINE__,__FILE__)
          OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          write(unit=out,fmt=*,iostat=ios) trim(loc_string)
          call check_iostat(ios,__LINE__,__FILE__)
          CLOSE(unit=out,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
       end if
       if (sed_select(is_POC) .AND. sed_select(is_POP)) then
          loc_filename=fun_data_timeseries_filename( &
               & loc_t,par_outdir_name,trim(par_outfile_name)//'_series','misc_POPtoPOC',string_results_ext)
          loc_string = '% time (yr) / POP/POC ratio (o/oo) / POC/POP ratio'
          call check_unit(out,__LINE__,__FILE__)
          OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          write(unit=out,fmt=*,iostat=ios) trim(loc_string)
          call check_iostat(ios,__LINE__,__FILE__)
          CLOSE(unit=out,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
       end if
       if (sed_select(is_POC) .AND. sed_select(is_POFe)) then
          loc_filename=fun_data_timeseries_filename( &
               & loc_t,par_outdir_name,trim(par_outfile_name)//'_series','misc_POFetoPOC',string_results_ext)
          loc_string = '% time (yr) / POFe/POC ratio (1.0E3 o/oo) / POC/POFe ratio'
          call check_unit(out,__LINE__,__FILE__)
          OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          write(unit=out,fmt=*,iostat=ios) trim(loc_string)
          call check_iostat(ios,__LINE__,__FILE__)
          CLOSE(unit=out,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
       end if
    END IF
    ! air-sea gas exchange
    IF (ctrl_data_save_sig_fairsea) THEN
       DO l=3,n_l_atm
          ia = conv_iselected_ia(l)
          loc_filename=fun_data_timeseries_filename( &
               & loc_t,par_outdir_name,trim(par_outfile_name)//'_series','fseaair_'//TRIM(string_atm(ia)),string_results_ext &
               & )
          SELECT CASE (atm_type(ia))
          CASE (0,1)
             loc_string = '% time (yr) / global '//TRIM(string_atm(ia))// ' sea->air transfer flux (mol yr-1) / '//&
                  & 'global '//TRIM(string_atm(ia))// ' density (mol m-2 yr-1)'
          CASE (n_itype_min:n_itype_max)
             loc_string = '% time (yr) / global '//TRIM(string_atm(ia))//' sea->air transfer flux (mol yr-1) / ' //&
                  & 'global '//TRIM(string_atm(ia))//' (o/oo)'
          end SELECT
          SELECT CASE (atm_type(ia))
          CASE (0,1,n_itype_min:n_itype_max)
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             write(unit=out,fmt=*,iostat=ios) trim(loc_string)
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
          end SELECT
       END DO
    end IF
    ! ocean-atmosphere interface flux
    IF (ctrl_data_save_sig_focnatm) THEN
       DO l=3,n_l_atm
          ia = conv_iselected_ia(l)
          loc_filename=fun_data_timeseries_filename( &
               & loc_t,par_outdir_name,trim(par_outfile_name)//'_series','focnatm_'//TRIM(string_atm(ia)),string_results_ext &
               & )
          SELECT CASE (atm_type(ia))
          CASE (0,1)
             loc_string = '% time (yr) / global '//TRIM(string_atm(ia))//' flux (mol yr-1) / global '// &
                  & TRIM(string_atm(ia))//' density (mol m-2 yr-1) '//&
                  & ' NOTE: is the atmospheric forcing flux *net* of the sea-air gas exchange flux.'
          CASE (n_itype_min:n_itype_max)
             loc_string = '% time (yr) / global '//TRIM(string_atm(ia))//' flux (mol yr-1) / global '// &
                  & TRIM(string_atm(ia))//' (o/oo)'//&
                  & ' NOTE: is the atmospheric forcing flux *net* of the sea-air gas exchange flux.'
          end SELECT
          SELECT CASE (atm_type(ia))
          CASE (0,1,n_itype_min:n_itype_max)
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             write(unit=out,fmt=*,iostat=ios) trim(loc_string)
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
          end SELECT
       END DO
    END IF
    ! ocean->sediment flux
    IF (ctrl_data_save_sig_focnsed) THEN
       DO l=1,n_l_sed
          is = conv_iselected_is(l)
          loc_filename=fun_data_timeseries_filename( &
               & loc_t,par_outdir_name,trim(par_outfile_name)//'_series','focnsed_'//TRIM(string_sed(is)),string_results_ext &
               & )
          SELECT CASE (sed_type(is))
          CASE (par_sed_type_bio,par_sed_type_abio, &
               & par_sed_type_POM,par_sed_type_CaCO3,par_sed_type_opal,par_sed_type_det, &
               & par_sed_type_scavenged)
             loc_string = '% time (yr) / global '//TRIM(string_sed(is))//' flux (mol yr-1) / global '// &
                  & TRIM(string_sed(is))//' density (mol m-2 yr-1)'
          CASE (par_sed_type_age)
             loc_string = '% time (yr) / CaCO3 age (yr)'
          CASE (n_itype_min:n_itype_max)
             loc_string = '% time (yr) / global '//TRIM(string_sed(is))//' flux (mol yr-1) / global '// &
                  & TRIM(string_sed(is))//' delta (o/oo)'
          end SELECT
          SELECT CASE (sed_type(is))
          CASE (par_sed_type_bio,par_sed_type_abio, &
               & par_sed_type_POM,par_sed_type_CaCO3,par_sed_type_opal,par_sed_type_det, &
               & par_sed_type_scavenged,par_sed_type_age,n_itype_min:n_itype_max)
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             write(unit=out,fmt=*,iostat=ios) trim(loc_string)
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
          end SELECT
       END DO
    END IF
    ! sediment->ocean flux
    IF (ctrl_data_save_sig_fsedocn) THEN
       DO l=1,n_l_ocn
          io = conv_iselected_io(l)
          loc_filename=fun_data_timeseries_filename( &
               & loc_t,par_outdir_name,trim(par_outfile_name)//'_series','fsedocn_'//TRIM(string_ocn(io)),string_results_ext &
               & )
          SELECT CASE (ocn_type(io))
          CASE (1)
             loc_string = '% time (yr) / global '//TRIM(string_ocn(io))//' flux (mol yr-1) / global '// &
                  & TRIM(string_ocn(io))//' density (mol m-2 yr-1)'
          CASE (n_itype_min:n_itype_max)
             loc_string = '% time (yr) / global '//TRIM(string_ocn(io))//' flux (mol yr-1) / global '// &
                  & TRIM(string_ocn(io))//' delta (o/oo)'
          end SELECT
          SELECT CASE (ocn_type(io))
          CASE (1,n_itype_min:n_itype_max)
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             write(unit=out,fmt=*,iostat=ios) trim(loc_string)
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
          end SELECT
       END DO
    END IF
    ! ocean surface carbonate chemistry
    IF (ctrl_data_save_sig_carb_sur) THEN
       DO ic=1,n_carb
          loc_filename=fun_data_timeseries_filename( &
               & loc_t,par_outdir_name,trim(par_outfile_name)//'_series','carb_sur_'//TRIM(string_carb(ic)),string_results_ext &
               & )
          SELECT CASE (ic)
          CASE (ic_ohm_cal,ic_ohm_arg)
             loc_string = '% time (yr) / mean saturation state'
          CASE (ic_conc_CO2,ic_conc_HCO3,ic_conc_CO3)
             if (ocn_select(io_DIC_14C)) then
                loc_string = '% time (yr) / ' //&
                     & 'surface mean (ice-free) '//TRIM(string_carb(ic))//' (mol kg-1) / ' //&
                     & 'surface '//TRIM(string_carb(ic))// ' d13C (o/oo) / ' //&
                     & 'surface '//TRIM(string_carb(ic))//' d14C (o/oo) / ' //&
                     & 'surface mean '//TRIM(string_carb(ic))//' (mol kg-1)' 
             elseif (ocn_select(io_DIC_13C)) then
                loc_string = '% time (yr) / ' //&
                     & 'surface mean (ice-free) '//TRIM(string_carb(ic))//' (mol kg-1) / ' //&
                     & 'surface '//TRIM(string_carb(ic))// ' d13C (o/oo) / ' //&
                     & 'surface mean '//TRIM(string_carb(ic))//' (mol kg-1)'
             else
                loc_string = '% time (yr) / surface '//TRIM(string_carb(ic))//' (mol kg-1)'
             end if
          CASE (ic_fug_CO2)
             loc_string = '% time (yr) / ' //&
                  & 'surface (ice-free) '//TRIM(string_carb(ic))//' (atm) / ' //&
                  & 'surface '//TRIM(string_carb(ic))//' (atm)'
          case default
             loc_string = '% time (yr) / ' //&
                  & 'surface (ice-free) '//TRIM(string_carb(ic))//' (mol kg-1) / ' //&
                  & 'surface '//TRIM(string_carb(ic))//' (mol kg-1)'
          end SELECT
          call check_unit(out,__LINE__,__FILE__)
          OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          write(unit=out,fmt=*,iostat=ios) trim(loc_string)
          call check_iostat(ios,__LINE__,__FILE__)
          CLOSE(unit=out,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
       END DO
    end if
    ! core-top sediment composition
    IF (ctrl_data_save_sig_ocnsed) THEN
       DO l=1,n_l_sed
          is = conv_iselected_is(l)
          loc_filename=fun_data_timeseries_filename( &
               & loc_t,par_outdir_name,trim(par_outfile_name)//'_series','sed_'//TRIM(string_sed(is)),string_results_ext &
               & )
          SELECT CASE (sed_type(is))
          CASE (par_sed_type_bio,par_sed_type_abio, &
               & par_sed_type_POM,par_sed_type_CaCO3,par_sed_type_opal,par_sed_type_det, &
               & par_sed_type_scavenged)
             loc_string = '% time (yr) / mean '//TRIM(string_sed(is))//' composition (wt%)'
          CASE (par_sed_type_age)
             loc_string = '% time (yr) / mean core-top age (yr)'
          CASE (n_itype_min:n_itype_max)
             loc_string = '% time (yr) / mean core-top '//TRIM(string_sed(is))//' delta (o/oo)'
          end SELECT
          SELECT CASE (sed_type(is))
          CASE (par_sed_type_bio,par_sed_type_abio, &
               & par_sed_type_POM,par_sed_type_CaCO3,par_sed_type_opal,par_sed_type_det, &
               & par_sed_type_scavenged,par_sed_type_age,n_itype_min:n_itype_max)
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             write(unit=out,fmt=*,iostat=ios) trim(loc_string)
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
          end SELECT
       END DO
    END IF
    ! miscellaneous
    IF (ctrl_data_save_sig_misc) THEN
       if (flag_gemlite) then
          !
          loc_filename=fun_data_timeseries_filename( &
               & loc_t,par_outdir_name,trim(par_outfile_name)//'_series','misc_gemlite',string_results_ext)
          loc_string = '% time (yr) / GEMlite weighting)'
          call check_unit(out,__LINE__,__FILE__)
          OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          write(unit=out,fmt=*,iostat=ios) trim(loc_string)
          call check_iostat(ios,__LINE__,__FILE__)
          CLOSE(unit=out,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
       end if
       !
       loc_filename=fun_data_timeseries_filename( &
            & loc_t,par_outdir_name,trim(par_outfile_name)//'_series','misc_seaice',string_results_ext)
       loc_string = '% time (yr) / global sea-ice area (m2) / mean sea-ice cover (%) / '// &
            & 'global sea-ice volume (m3) / mean sea-ice thickness (m)'
       call check_unit(out,__LINE__,__FILE__)
       OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
       write(unit=out,fmt=*,iostat=ios) trim(loc_string)
       call check_iostat(ios,__LINE__,__FILE__)
       CLOSE(unit=out,iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
       !
       loc_filename=fun_data_timeseries_filename( &
            & loc_t,par_outdir_name,trim(par_outfile_name)//'_series','misc_opsi',string_results_ext)
       select case (fname_topo)
       case ('worbe2', 'worjh2', 'worjh4', 'worlg2', 'worlg4', 'wv2jh2', 'wv3jh2', 'worri4')
          loc_string = '% time (yr) / global min overturning (Sv) / global max overturning (Sv) / '// &
               & 'Atlantic min overturning (Sv) / Atlantic max overturning (Sv)'
       case default
          loc_string = '% time (yr) / global min overturning (Sv) / global max overturning (Sv)'
       end select
       call check_unit(out,__LINE__,__FILE__)
       OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
       write(unit=out,fmt=*,iostat=ios) trim(loc_string)
       call check_iostat(ios,__LINE__,__FILE__)
       CLOSE(unit=out,iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
       !
       IF (atm_select(ia_pCO2_14C)) THEN
          loc_filename=fun_data_timeseries_filename( &
               & loc_t,par_outdir_name,trim(par_outfile_name)//'_series','misc_atm_D14C',string_results_ext)
          loc_string = '% time (yr) / mean isotopic composition (o/oo)'
          call check_unit(out,__LINE__,__FILE__)
          OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          write(unit=out,fmt=*,iostat=ios) trim(loc_string)
          call check_iostat(ios,__LINE__,__FILE__)
          CLOSE(unit=out,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
       end IF
       ! 
       IF (ctrl_data_save_sig_carb_sur) THEN
          loc_filename=fun_data_timeseries_filename( &
               & loc_t,par_outdir_name,trim(par_outfile_name)//'_series','misc_surpH',string_results_ext)
          loc_string = '% time (yr) / mean ocean surface pH'
          call check_unit(out,__LINE__,__FILE__)
          OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          write(unit=out,fmt=*,iostat=ios) trim(loc_string)
          call check_iostat(ios,__LINE__,__FILE__)
          CLOSE(unit=out,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
       end IF
       ! surface land (air) temperature SLT
       loc_filename=fun_data_timeseries_filename( &
            & loc_t,par_outdir_name,trim(par_outfile_name)//'_series','misc_SLT',string_results_ext)
       loc_string = '% time (yr) / mean (land) surface air temperature (degrees C)'
       call check_unit(out,__LINE__,__FILE__)
       OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
       write(unit=out,fmt=*,iostat=ios) trim(loc_string)
       call check_iostat(ios,__LINE__,__FILE__)
       CLOSE(unit=out,iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
       ! aeolian Fe diagnostics
       IF (ocn_select(io_Fe)) THEN
          loc_filename=fun_data_timeseries_filename( &
               & loc_t,par_outdir_name,trim(par_outfile_name)//'_series','misc_det_Fe_tot',string_results_ext)
          loc_string = '% time (yr) / total aeolian Fe input (mol yr-1)'
          call check_unit(out,__LINE__,__FILE__)
          OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          write(unit=out,fmt=*,iostat=ios) trim(loc_string)
          call check_iostat(ios,__LINE__,__FILE__)
          CLOSE(unit=out,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          loc_filename=fun_data_timeseries_filename( &
               & loc_t,par_outdir_name,trim(par_outfile_name)//'_series','misc_det_Fe_dis',string_results_ext)
          loc_string = '% time (yr) / dissolved aeolian Fe input (mol yr-1)'
          call check_unit(out,__LINE__,__FILE__)
          OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          write(unit=out,fmt=*,iostat=ios) trim(loc_string)
          call check_iostat(ios,__LINE__,__FILE__)
          CLOSE(unit=out,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          loc_filename=fun_data_timeseries_filename( &
               & loc_t,par_outdir_name,trim(par_outfile_name)//'_series','misc_det_Fe_sol',string_results_ext)
          loc_string = '% time (yr) / mean aeolian Fe solubility (%)'
          call check_unit(out,__LINE__,__FILE__)
          OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          write(unit=out,fmt=*,iostat=ios) trim(loc_string)
          call check_iostat(ios,__LINE__,__FILE__)
          CLOSE(unit=out,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
       end IF
       IF (ocn_select(io_Fe)) THEN
          ! insolation (wet grid only)
          loc_filename=fun_data_timeseries_filename( &
               & loc_t,par_outdir_name,trim(par_outfile_name)//'_series','misc_ocn_insol',string_results_ext)
          loc_string = '% time (yr) / mean (wet grid) insolation (W m-2) / ' // &
               & 'N mean zonal insolation (W m-2) @ j=' // fun_conv_num_char_n(2,par_sig_j_N) // &
               & ' and BIOGEM time-step: ' // fun_conv_num_char_n(2,par_t_sig_count_N) // &
               & ' / ' // &
               & 'S mean zonal insolation (W m-2) @ j=' // fun_conv_num_char_n(2,par_sig_j_S) // &
               & ' and BIOGEM time-step: ' // fun_conv_num_char_n(2,par_t_sig_count_S)
          call check_unit(out,__LINE__,__FILE__)
          OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          write(unit=out,fmt=*,iostat=ios) trim(loc_string)
          call check_iostat(ios,__LINE__,__FILE__)
          CLOSE(unit=out,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          loc_filename=fun_data_timeseries_filename( &
               & loc_t,par_outdir_name,trim(par_outfile_name)//'_series','misc_ocn_swflux',string_results_ext)
          loc_string = '% time (yr) / mean annual Sw flux at ocean surface (W m-2)'
          call check_unit(out,__LINE__,__FILE__)
          OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          write(unit=out,fmt=*,iostat=ios) trim(loc_string)
          call check_iostat(ios,__LINE__,__FILE__)
          CLOSE(unit=out,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
       end if
       ! Sr diagnostics
       IF (ocn_select(io_Sr_87Sr) .AND. ocn_select(io_Sr_88Sr)) THEN
          loc_filename=fun_data_timeseries_filename(loc_t, &
               & par_outdir_name,trim(par_outfile_name)//'_series','misc_Sr_ocn_Sr',string_results_ext)
          loc_string = '% time (yr) / Sr / 86Sr / 87Sr / 88Sr'
          call check_unit(out,__LINE__,__FILE__)
          OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          write(unit=out,fmt=*,iostat=ios) trim(loc_string)
          call check_iostat(ios,__LINE__,__FILE__)
          CLOSE(unit=out,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          loc_filename=fun_data_timeseries_filename(loc_t, &
               & par_outdir_name,trim(par_outfile_name)//'_series','misc_Sr_ocn_r87Sr',string_results_ext)
          loc_string = '% time (yr) / mean ocean 87Sr / mean ocean 87/86 ratio'
          call check_unit(out,__LINE__,__FILE__)
          OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          write(unit=out,fmt=*,iostat=ios) trim(loc_string)
          call check_iostat(ios,__LINE__,__FILE__)
          CLOSE(unit=out,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          loc_filename=fun_data_timeseries_filename(loc_t, &
               & par_outdir_name,trim(par_outfile_name)//'_series','misc_Sr_ocn_d88Sr',string_results_ext)
          loc_string = '% time (yr) / mean ocean 88Sr / mean ocean d88Sr (o/oo)'
          call check_unit(out,__LINE__,__FILE__)
          OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          write(unit=out,fmt=*,iostat=ios) trim(loc_string)
          call check_iostat(ios,__LINE__,__FILE__)
          CLOSE(unit=out,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)  
          loc_filename=fun_data_timeseries_filename(loc_t, &
               & par_outdir_name,trim(par_outfile_name)//'_series','misc_Sr_fexport_Sr',string_results_ext)
          loc_string = '% time (yr) / Sr / 86Sr / 87Sr / 88Sr'
          call check_unit(out,__LINE__,__FILE__)
          OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          write(unit=out,fmt=*,iostat=ios) trim(loc_string)
          call check_iostat(ios,__LINE__,__FILE__)
          CLOSE(unit=out,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          IF (flag_sedgem) THEN
             ! (1) OCN -> SED FLUXES       
             loc_filename=fun_data_timeseries_filename(loc_t, &
                  & par_outdir_name,trim(par_outfile_name)//'_series','misc_Sr_focnsed_Sr',string_results_ext)
             loc_string = '% time (yr) / Sr (mol yr-1) / 86Sr (mol yr-1) / 87Sr (mol yr-1) / 88Sr (mol yr-1)'
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             write(unit=out,fmt=*,iostat=ios) trim(loc_string)
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             loc_filename=fun_data_timeseries_filename(loc_t, &
                  & par_outdir_name,trim(par_outfile_name)//'_series','misc_Sr_focnsed_r87Sr',string_results_ext)
             loc_string = '% time (yr) / 87Sr flux (mol yr-1) / 87/86 ratio'
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             write(unit=out,fmt=*,iostat=ios) trim(loc_string)
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             loc_filename=fun_data_timeseries_filename(loc_t, &
                  & par_outdir_name,trim(par_outfile_name)//'_series','misc_Sr_focnsed_d88Sr',string_results_ext)
             loc_string = '% time (yr) / 88Sr flux (mol yr-1) / flux d88Sr (o/oo)'
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             write(unit=out,fmt=*,iostat=ios) trim(loc_string)
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__) 
             ! (2) SED -> OCN FLUXES        
             loc_filename=fun_data_timeseries_filename(loc_t, &
                  & par_outdir_name,trim(par_outfile_name)//'_series','misc_Sr_fsedocn_Sr',string_results_ext)
             loc_string = '% time (yr) / Sr (mol yr-1) / 86Sr (mol yr-1) / 87Sr (mol yr-1) / 88Sr (mol yr-1)'
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             write(unit=out,fmt=*,iostat=ios) trim(loc_string)
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             loc_filename=fun_data_timeseries_filename(loc_t, &
                  & par_outdir_name,trim(par_outfile_name)//'_series','misc_Sr_fsedocn_r87Sr',string_results_ext)
             loc_string = '% time (yr) / 87Sr flux (mol yr-1) / 87/86 ratio'
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             write(unit=out,fmt=*,iostat=ios) trim(loc_string)
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             loc_filename=fun_data_timeseries_filename(loc_t, &
                  & par_outdir_name,trim(par_outfile_name)//'_series','misc_Sr_fsedocn_d88Sr',string_results_ext)
             loc_string = '% time (yr) / 88Sr flux (mol yr-1) / flux d88Sr (o/oo)'
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             write(unit=out,fmt=*,iostat=ios) trim(loc_string)
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)     
!!$          loc_filename=fun_data_timeseries_filename(loc_t, &
!!$               & par_outdir_name,trim(par_outfile_name)//'_series','misc_sed_r87Sr',string_results_ext)
!!$          loc_string = '% time (yr) / mean sediment core-top CaCO3 87/86 ratio'
!!$          call check_unit(out,__LINE__,__FILE__)
!!$          OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
!!$          call check_iostat(ios,__LINE__,__FILE__)
!!$          write(unit=out,fmt=*,iostat=ios) trim(loc_string)
!!$          call check_iostat(ios,__LINE__,__FILE__)
!!$          CLOSE(unit=out,iostat=ios)
!!$          call check_iostat(ios,__LINE__,__FILE__)
!!$          loc_filename=fun_data_timeseries_filename(loc_t, &
!!$               & par_outdir_name,trim(par_outfile_name)//'_series','misc_sed_d88Sr',string_results_ext)
!!$          loc_string = '% time (yr) / mean sediment core-top CaCO3 d88Sr'
!!$          call check_unit(out,__LINE__,__FILE__)
!!$          OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
!!$          call check_iostat(ios,__LINE__,__FILE__)
!!$          write(unit=out,fmt=*,iostat=ios) trim(loc_string)
!!$          call check_iostat(ios,__LINE__,__FILE__)
!!$          CLOSE(unit=out,iostat=ios)
!!$          call check_iostat(ios,__LINE__,__FILE__)           
          end IF
          if (flag_rokgem) then
             loc_filename=fun_data_timeseries_filename(loc_t, &
                  & par_outdir_name,trim(par_outfile_name)//'_series','misc_Sr_weather_Sr',string_results_ext)
             loc_string = '% time (yr) / Sr (mol yr-1) / 86Sr (mol yr-1) / 87Sr (mol yr-1) / 88Sr (mol yr-1)'
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             write(unit=out,fmt=*,iostat=ios) trim(loc_string)
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             loc_filename=fun_data_timeseries_filename(loc_t, &
                  & par_outdir_name,trim(par_outfile_name)//'_series','misc_Sr_weather_r87Sr',string_results_ext)
             loc_string = '% time (yr) / weathering 87Sr (mol yr-1) / weathering 87/86 ratio'
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             write(unit=out,fmt=*,iostat=ios) trim(loc_string)
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             loc_filename=fun_data_timeseries_filename(loc_t, &
                  & par_outdir_name,trim(par_outfile_name)//'_series','misc_Sr_weather_d88Sr',string_results_ext)
             loc_string = '% time (yr) / weathering 88Sr (mol yr-1) / weathering d88Sr (o/oo)'
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             write(unit=out,fmt=*,iostat=ios) trim(loc_string)
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)  
          end if
       end if
       ! insolation (wet grid only)
       loc_filename=fun_data_timeseries_filename( &
            & loc_t,par_outdir_name,trim(par_outfile_name)//'_series','misc_ocn_insol',string_results_ext)
       loc_string = '% time (yr) / mean (wet grid) insolation (W m-2) / ' // &
            & 'N mean zonal insolation (W m-2) @ j=' // fun_conv_num_char_n(2,par_sig_j_N) // &
            & ' and BIOGEM time-step: ' // fun_conv_num_char_n(2,par_t_sig_count_N) // &
            & ' / ' // &
            & 'S mean zonal insolation (W m-2) @ j=' // fun_conv_num_char_n(2,par_sig_j_S) // &
            & ' and BIOGEM time-step: ' // fun_conv_num_char_n(2,par_t_sig_count_S)
       call check_unit(out,__LINE__,__FILE__)
       OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
       write(unit=out,fmt=*,iostat=ios) trim(loc_string)
       call check_iostat(ios,__LINE__,__FILE__)
       CLOSE(unit=out,iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
       loc_filename=fun_data_timeseries_filename( &
            & loc_t,par_outdir_name,trim(par_outfile_name)//'_series','misc_ocn_swflux',string_results_ext)
       loc_string = '% time (yr) / mean annual Sw flux at ocean surface (W m-2)'
       call check_unit(out,__LINE__,__FILE__)
       OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
       write(unit=out,fmt=*,iostat=ios) trim(loc_string)
       call check_iostat(ios,__LINE__,__FILE__)
       CLOSE(unit=out,iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
    end if
    ! diagnostics
    IF (ctrl_data_save_sig_diag_bio .AND. ctrl_data_save_sig_fexport) THEN
       DO ib=1,n_diag_bio
          loc_filename=fun_data_timeseries_filename(loc_t, &
               & par_outdir_name,trim(par_outfile_name)//'_series_diag_bio',trim(string_diag_bio(ib)),string_results_ext)
          SELECT CASE (ib)
          CASE (idiag_bio_dPO4,idiag_bio_dPO4_1,idiag_bio_dPO4_2,idiag_bio_N2fixation,idiag_bio_NH4assim)
             loc_string = '% time (yr) / integrated global rate (mol yr-1)'
          case default
             loc_string = '% time (yr) / global mean'
          end select
          call check_unit(out,__LINE__,__FILE__)
          OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          write(unit=out,fmt=*,iostat=ios) trim(loc_string)
          call check_iostat(ios,__LINE__,__FILE__)
          CLOSE(unit=out,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
       end DO
    end if
    IF (ctrl_data_save_sig_diag_geochem .AND. ctrl_data_save_sig_diag_redox_old) THEN
       DO id=1,n_diag_geochem
          loc_filename=fun_data_timeseries_filename(loc_t, &
               & par_outdir_name,trim(par_outfile_name)//'_series_diag_geochem',trim(string_diag_geochem(id)),string_results_ext)
          loc_string = '% time (yr) / global rate (mol yr-1) / mean rate (mol kg-1 yr-1)'
          call check_unit(out,__LINE__,__FILE__)
          OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          write(unit=out,fmt=*,iostat=ios) trim(loc_string)
          call check_iostat(ios,__LINE__,__FILE__)
          CLOSE(unit=out,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
       end DO
    end if
    IF (ctrl_data_save_sig_diag_geochem) THEN
       DO id=1,n_diag_redox
          loc_filename=fun_data_timeseries_filename(loc_t, &
               & par_outdir_name,trim(par_outfile_name)//'_series_diag_redox',trim(string_diag_redox(id)),string_results_ext)
          loc_string = '% time (yr) / global rate (mol yr-1) / mean rate (mol kg-1 yr-1)'
          call check_unit(out,__LINE__,__FILE__)
          OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          write(unit=out,fmt=*,iostat=ios) trim(loc_string)
          call check_iostat(ios,__LINE__,__FILE__)
          CLOSE(unit=out,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
       end DO
    end if
    IF (ctrl_data_save_sig_diag .AND. flag_rokgem) THEN
       DO l=1,n_l_ocn
          io = conv_iselected_io(l)
          loc_filename=fun_data_timeseries_filename(loc_t, &
               & par_outdir_name,trim(par_outfile_name)//'_series_diag_weather',TRIM(string_ocn(io)),string_results_ext &
               & )
          SELECT CASE (ocn_type(io))
          CASE (1)
             loc_string = '% time (yr) / global '//TRIM(string_ocn(io))//' (mol yr-1)'
          CASE (n_itype_min:n_itype_max)
             loc_string = '% time (yr) / global '//TRIM(string_ocn(io))//' flux (mol yr-1) / global '// &
                  & TRIM(string_ocn(io))//' (o/oo)'
          end SELECT
          SELECT CASE (ocn_type(io))
          CASE (1,n_itype_min:n_itype_max)
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             write(unit=out,fmt=*,iostat=ios) trim(loc_string)
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
          end SELECT
       END DO
       IF (ocn_select(io_DIC) .AND. ocn_select(io_Ca)) THEN
          loc_filename=fun_data_timeseries_filename(loc_t, &
               & par_outdir_name,trim(par_outfile_name)//'_series_misc_exweather','Ca',string_results_ext &
               & )
          loc_string = '% time (yr) / excess cation compared to DIC weathering (mol 2+ yr-1) / % excess (not useful!)'
          OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
          write(unit=out,fmt=*,iostat=ios) trim(loc_string)
          CLOSE(unit=out,iostat=ios)
       end IF
    end if
    ! misc diagnostics
    ! NOTE: also save diagnozed fluxes from 'normal' pCO2 flux and restoring forcing
    IF (ctrl_data_save_sig_diag .AND. ctrl_data_save_inversion) THEN
       DO idm2D=1,n_diag_misc_2D
          loc_filename = fun_data_timeseries_filename(loc_t, &
               & par_outdir_name,trim(par_outfile_name)//'_series_diag_misc','inversion_forcing_' &
               & //trim(string_diag_misc_2D(idm2D)),string_results_ext)
          select case (idm2D)
          case (idiag_misc_2D_FpCO2_13C,idiag_misc_2D_FDIC_13C,idiag_misc_2D_FCa_44Ca) 
             loc_string = '% time (yr) / integrated flux (mol) / isotopic composition (o/oo)'//&
                  & ' NOTE: is the integrated (per save interval) diagnosed inversion flux.'
          case default
             loc_string = '% time (yr) / integrated flux (mol)'//&
                  & ' NOTE: is the integrated (per save interval) diagnosed inversion flux.'
          end select
          call check_unit(out,__LINE__,__FILE__)
          OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          write(unit=out,fmt=*,iostat=ios) trim(loc_string)
          call check_iostat(ios,__LINE__,__FILE__)
          CLOSE(unit=out,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
       end DO
    END IF
    ! forcing flux
    IF (ctrl_data_save_sig_diag) THEN
       DO l=3,n_l_atm
          ia = conv_iselected_ia(l)
          if (force_flux_atm_select(ia) .OR. force_restore_atm_select(ia)) then
             loc_filename=fun_data_timeseries_filename( &
                  & loc_t,par_outdir_name, &
                  & trim(par_outfile_name)//'_series_diag_misc','specified_forcing_'//TRIM(string_atm(ia)),string_results_ext &
                  & )
             SELECT CASE (atm_type(ia))
             CASE (0,1)
                loc_string = '% time (yr) / global '//TRIM(string_atm(ia))//' flux (mol yr-1) '//&
                     & ' NOTE: is the instantaneous (per unit time) atmospheric forcing flux.'
             CASE (n_itype_min:n_itype_max)
                loc_string = '% time (yr) / global '//TRIM(string_atm(ia))//' flux (mol yr-1) / global '// &
                     & TRIM(string_atm(ia))//' (o/oo)'//&
                     & ' NOTE: is the instantaneous (per unit time) atmospheric forcing flux.'
             end SELECT
             SELECT CASE (atm_type(ia))
             CASE (0,1,n_itype_min:n_itype_max)
                call check_unit(out,__LINE__,__FILE__)
                OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
                call check_iostat(ios,__LINE__,__FILE__)
                write(unit=out,fmt=*,iostat=ios) trim(loc_string)
                call check_iostat(ios,__LINE__,__FILE__)
                CLOSE(unit=out,iostat=ios)
                call check_iostat(ios,__LINE__,__FILE__)
             end SELECT
          end if
       END DO
    END IF
    ! age tracers
    IF (ctrl_data_save_sig_ocn .AND. ctrl_force_ocn_age) THEN
       loc_filename=fun_data_timeseries_filename(loc_t, &
            & par_outdir_name,trim(par_outfile_name)//'_series','misc_col_age',string_results_ext)
       IF (ctrl_data_save_sig_ocn_sur) THEN
          loc_string = '% time (yr) / mean global ventilation age (yr) / surface ventilation age (yr) / benthic ventilation age (yr)'
       else
          loc_string = '% time (yr) / mean global ventilation age (yr)'
       end if
       call check_unit(out,__LINE__,__FILE__)
       OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
       write(unit=out,fmt=*,iostat=ios) trim(loc_string)
       call check_iostat(ios,__LINE__,__FILE__)
       CLOSE(unit=out,iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
    end if
    ! preformed tracers
    IF (ctrl_data_save_sig_diag .AND. ctrl_bio_preformed) THEN
          do io=io_col0,io_col9
             if (ocn_select(io)) then
                select case (io)
                CASE (io_col0)
                   if (ocn_select(io_DIC)) then
                      loc_save = .true.
                      loc_filename=fun_data_timeseries_filename( &
                           & loc_t,par_outdir_name,trim(par_outfile_name)//'_series_diag','preformed_DIC',string_results_ext &
                           & )
                      loc_string = '% time (yr) / global total preformed DIC (mol) / global mean (mol kg-1)'
                   end if
                CASE (io_col1)
                   if (ocn_select(io_ALK)) then
                      loc_save = .true.
                      loc_filename=fun_data_timeseries_filename( &
                           & loc_t,par_outdir_name,trim(par_outfile_name)//'_series_diag','preformed_ALK',string_results_ext &
                           & )
                      loc_string = '% time (yr) / global total preformed ALK (mol) / global mean (mol kg-1)'
                   end if
                CASE (io_col2)
                   if (ocn_select(io_O2)) then
                      loc_save = .true.
                      loc_filename=fun_data_timeseries_filename( &
                           & loc_t,par_outdir_name,trim(par_outfile_name)//'_series_diag','preformed_O2',string_results_ext &
                           & )
                      loc_string = '% time (yr) / global total preformed O2 (mol) / global mean (mol kg-1)'
                   end if
                CASE (io_col3)
                   if (ocn_select(io_PO4)) then 
                      loc_save = .true.
                      loc_filename=fun_data_timeseries_filename( &
                           & loc_t,par_outdir_name,trim(par_outfile_name)//'_series_diag','preformed_PO4',string_results_ext &
                           & )
                      loc_string = '% time (yr) / global total preformed PO4 (mol) / global mean (mol kg-1)'
                   end if
                CASE (io_col4)
                   if (ocn_select(io_NO3)) then
                      loc_save = .true.
                      loc_filename=fun_data_timeseries_filename( &
                           & loc_t,par_outdir_name,trim(par_outfile_name)//'_series_diag','preformed_NO3',string_results_ext &
                           & )
                      loc_string = '% time (yr) / global total preformed NO3 (mol) / global mean (mol kg-1)'
                   end if
                CASE (io_col5)
                   if (ocn_select(io_Ca)) then
                      loc_save = .true.
                      loc_filename=fun_data_timeseries_filename( &
                           & loc_t,par_outdir_name,trim(par_outfile_name)//'_series_diag','preformed_Ca',string_results_ext &
                           & )
                      loc_string = '% time (yr) / global total preformed Ca (mol) / global mean (mol kg-1)'
                   end if
                CASE (io_col6)
                   if (ocn_select(io_SiO2)) then
                      loc_save = .true.
                      loc_filename=fun_data_timeseries_filename( &
                           & loc_t,par_outdir_name,trim(par_outfile_name)//'_series_diag','preformed_SiO2',string_results_ext &
                           & )
                      loc_string = '% time (yr) / global total preformed SiO2 (mol) / global mean (mol kg-1)'
                   end if
                CASE (io_col7)
                   if (ocn_select(io_DIC_13C)) then
                      loc_save = .true.
                      loc_filename=fun_data_timeseries_filename( &
                           & loc_t,par_outdir_name,trim(par_outfile_name)//'_series_diag','preformed_d13C',string_results_ext &
                           & )
                      loc_string = '% time (yr) / global total preformed 13C (mol) / global mean (o/oo)'
                   end if
                end select
                if (loc_save) then
                   call check_unit(out,__LINE__,__FILE__)
                   OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
                   call check_iostat(ios,__LINE__,__FILE__)
                   write(unit=out,fmt=*,iostat=ios) trim(loc_string)
                   call check_iostat(ios,__LINE__,__FILE__)
                   CLOSE(unit=out,iostat=ios)
                   call check_iostat(ios,__LINE__,__FILE__)
                end if
             end if
          END DO
    end if
    ! Save 3D water column DIC d13C data for specific ij location
    IF (ctrl_data_save_ocn_3D_ij .AND. (ocn_select(io_DIC_13C))) THEN
       loc_filename=fun_data_timeseries_filename( &
            & loc_t,par_outdir_name,trim(par_outfile_name)//'_series','ocn_DIC_13C_ij',string_results_ext)
       loc_string = '% time (yr) / DIC_13C_k1 / DIC_13C_k2 / DIC_13C_k3 / DIC_13C_k4 / DIC_13C_k5 /' // &
            & 'DIC_13C_k6 / DIC_13C_k7 / DIC_13C_k8 / DIC_13C_k9 / DIC_13C_k10 / DIC_13C_k11 /' // &
            & 'DIC_13C_k12 / DIC_13C_k13 / DIC_13C_k14 / DIC_13C_k15 / DIC_13C_k16'
       call check_unit(out,__LINE__,__FILE__)
       OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
       write(unit=out,fmt=*,iostat=ios) trim(loc_string)
       call check_iostat(ios,__LINE__,__FILE__)
       CLOSE(unit=out,iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
    END IF
    ! And temperature
    IF (ctrl_data_save_ocn_3D_ij .AND. (ocn_select(io_T))) THEN
       loc_filename=fun_data_timeseries_filename( &
            & loc_t,par_outdir_name,trim(par_outfile_name)//'_series','ocn_temp_ij',string_results_ext)
       loc_string = '%time (yr) / temp_k1 / temp_k2 / temp_k3 / temp_k4 / temp_k5 /' // &
            & 'temp_k6 / temp_k7 / temp_k8 / temp_k9 / temp_k10 / temp_k11/' // &
            & 'temp_k12 / temp_k13 / temp_k14 / temp_k15 / temp_k16'
       call check_unit(out,__LINE__,__FILE__)
       OPEN(unit=out,file=loc_filename,action='write',status='replace',iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
       write(unit=out,fmt=*,iostat=ios) trim(loc_string)
       call check_iostat(ios,__LINE__,__FILE__)
       CLOSE(unit=out,iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
    END IF
  END SUBROUTINE sub_init_data_save_runtime
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! SAVE RUN-TIME DATA
  SUBROUTINE sub_data_save_runtime(dum_yr_save,dum_t)
    USE genie_util, ONLY:check_unit,check_iostat
    ! dummy arguments
    REAL,INTENT(in)::dum_yr_save,dum_t
    ! local variables
    INTEGER::l,io,ia,is,ic,ios,idm2D,k
    integer::ib,id
    REAL::loc_t
    real::loc_opsi_scale
    real::loc_ocn_tot_M,loc_ocn_tot_M_sur,loc_ocn_tot_A
    real::loc_sig,loc_sig_sur,loc_sig_opn,loc_sig_ben,loc_rsig
    real::loc_tot,loc_tot_sur,loc_tot_opn,loc_tot_ben
    real::loc_frac,loc_frac_sur,loc_frac_opn,loc_frac_ben,loc_standard
    real::loc_d13C,loc_d14C
    REAL,DIMENSION(n_k)::loc_sig_3D,loc_tot_3D,loc_frac_3D,loc_standard_3D 
    CHARACTER(len=255)::loc_filename
    REAL,DIMENSION(n_carbisor)::loc_carbisor
    logical::loc_save

    ! *** set-up local constants ***
    ! calculate local opsi conversion constant
    loc_opsi_scale = goldstein_dsc*goldstein_usc*const_rEarth*1.0E-6
    ! total ocean mass
    loc_ocn_tot_M = int_ocn_tot_M_sig/int_t_sig
    ! ocean surface mass
    loc_ocn_tot_M_sur = int_ocn_tot_M_sur_sig/int_t_sig
    ! ocean surface area
    loc_ocn_tot_A = sum(phys_ocn(ipo_A,:,:,n_k))
    ! local time
    loc_t = dum_yr_save

    ! *** initialize local arrays
    loc_carbisor(:) = 0.0

    ! *** <sig_ocn_*> ***
    ! write ocean tracer data
    ! NOTE: write data both as the total inventory, and as the equivalent mean concentration
    IF (ctrl_data_save_sig_ocn) THEN
       DO l=1,n_l_ocn
          io = conv_iselected_io(l)
          loc_filename=fun_data_timeseries_filename( &
               & dum_t,par_outdir_name,trim(par_outfile_name)//'_series','ocn_'//TRIM(string_ocn(io)),string_results_ext &
               & )
          SELECT CASE (ocn_type(io))
          CASE (0)
             If (io == io_T) then
                loc_sig = int_ocn_sig(io)/int_t_sig - const_zeroC
             else
                loc_sig = int_ocn_sig(io)/int_t_sig
             end If
             IF (ctrl_data_save_sig_ocn_sur .OR. (par_data_save_level > 3)) THEN
                If (io == io_T) then
                   loc_sig_opn = int_ocn_opn_sig(io)/int_t_sig - const_zeroC
                   loc_sig_ben = int_ocn_ben_sig(io)/int_t_sig - const_zeroC
                   loc_sig_sur = int_ocn_sur_sig(io)/int_t_sig - const_zeroC
                else
                   loc_sig_opn = int_ocn_opn_sig(io)/int_t_sig
                   loc_sig_ben = int_ocn_ben_sig(io)/int_t_sig
                   loc_sig_sur = int_ocn_sur_sig(io)/int_t_sig
                end If
                call check_unit(out,__LINE__,__FILE__)
                OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
                call check_iostat(ios,__LINE__,__FILE__)
                WRITE(unit=out,fmt='(f12.3,4f12.6)',iostat=ios) &
                     & loc_t,                                   &
                     & loc_sig,                                 &
                     & loc_sig_opn,                             &
                     & loc_sig_ben,                             &
                     & loc_sig_sur
                call check_iostat(ios,__LINE__,__FILE__)
                CLOSE(unit=out,iostat=ios)
                call check_iostat(ios,__LINE__,__FILE__)
             else
                call check_unit(out,__LINE__,__FILE__)
                OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
                call check_iostat(ios,__LINE__,__FILE__)
                WRITE(unit=out,fmt='(f12.3,f12.6)',iostat=ios) &
                     & loc_t,                                  &
                     & loc_sig
                call check_iostat(ios,__LINE__,__FILE__)
                CLOSE(unit=out,iostat=ios)
                call check_iostat(ios,__LINE__,__FILE__)
             end IF
          CASE (1)
             loc_sig = int_ocn_sig(io)/int_t_sig
             IF (ctrl_data_save_sig_ocn_sur) THEN
                loc_sig_opn = int_ocn_opn_sig(io)/int_t_sig
                loc_sig_ben = int_ocn_ben_sig(io)/int_t_sig
                loc_sig_sur = int_ocn_sur_sig(io)/int_t_sig
                call check_unit(out,__LINE__,__FILE__)
                OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
                call check_iostat(ios,__LINE__,__FILE__)
                WRITE(unit=out,fmt='(f12.3,e20.12,4e14.6)',iostat=ios) &
                     & loc_t,                                          &
                     & loc_ocn_tot_M*loc_sig,                          &
                     & loc_sig,                                        &
                     & loc_sig_opn,                                    &
                     & loc_sig_ben,                                    &
                     & loc_sig_sur
                call check_iostat(ios,__LINE__,__FILE__)
                CLOSE(unit=out,iostat=ios)
                call check_iostat(ios,__LINE__,__FILE__)                
             else
                call check_unit(out,__LINE__,__FILE__)
                OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
                call check_iostat(ios,__LINE__,__FILE__)
                WRITE(unit=out,fmt='(f12.3,e20.12,e14.6)',iostat=ios) &
                     & loc_t,                                   &
                     & loc_ocn_tot_M*loc_sig,                   &
                     & loc_sig
                call check_iostat(ios,__LINE__,__FILE__)
                CLOSE(unit=out,iostat=ios)
                call check_iostat(ios,__LINE__,__FILE__)
             end IF
          case (n_itype_min:n_itype_max)
             loc_tot      = int_ocn_sig(ocn_dep(io))/int_t_sig
             loc_frac     = int_ocn_sig(io)/int_t_sig
             loc_standard = const_standards(ocn_type(io))
             loc_sig      = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_nulliso)
             IF (ctrl_data_save_sig_ocn_sur) THEN
                loc_standard = const_standards(ocn_type(io))
                loc_tot_opn    = int_ocn_opn_sig(ocn_dep(io))/int_t_sig
                loc_frac_opn   = int_ocn_opn_sig(io)/int_t_sig
                loc_sig_opn    = fun_calc_isotope_delta(loc_tot_opn,loc_frac_opn,loc_standard,.FALSE.,const_nulliso)
                loc_tot_ben    = int_ocn_ben_sig(ocn_dep(io))/int_t_sig
                loc_frac_ben   = int_ocn_ben_sig(io)/int_t_sig
                loc_sig_ben    = fun_calc_isotope_delta(loc_tot_ben,loc_frac_ben,loc_standard,.FALSE.,const_nulliso)
                loc_tot_sur    = int_ocn_sur_sig(ocn_dep(io))/int_t_sig
                loc_frac_sur   = int_ocn_sur_sig(io)/int_t_sig
                loc_sig_sur    = fun_calc_isotope_delta(loc_tot_sur,loc_frac_sur,loc_standard,.FALSE.,const_nulliso)
                call check_unit(out,__LINE__,__FILE__)
                OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
                call check_iostat(ios,__LINE__,__FILE__)
                WRITE(unit=out,fmt='(f12.3,e20.12,4f12.3)',iostat=ios) &
                     & loc_t,                                          &
                     & loc_ocn_tot_M*loc_frac,                         &
                     & loc_sig,                                        &
                     & loc_sig_opn,                                    &
                     & loc_sig_ben,                                    &
                     & loc_sig_sur
                call check_iostat(ios,__LINE__,__FILE__)
                CLOSE(unit=out,iostat=ios)
                call check_iostat(ios,__LINE__,__FILE__)
             else
                call check_unit(out,__LINE__,__FILE__)
                OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
                call check_iostat(ios,__LINE__,__FILE__)
                WRITE(unit=out,fmt='(f12.3,e15.7,f12.3)',iostat=ios) &
                     & loc_t,                                        &
                     & loc_ocn_tot_M*loc_frac,                       &
                     & loc_sig
                call check_iostat(ios,__LINE__,__FILE__)
                CLOSE(unit=out,iostat=ios)
                call check_iostat(ios,__LINE__,__FILE__)
             end IF
          END SELECT
       END DO
    END IF

    ! *** <sig_carb_sur_*> ***
    IF (ctrl_data_save_sig_carb_sur) THEN
       ! calculate isotopic properties of CO2(aq), HCO3-, and CO32-
       IF (ctrl_data_save_sig_ocn_sur) THEN
          if (ocn_select(io_DIC_13C)) then
             call sub_calc_carb_r13C( &
                  & int_ocn_opn_sig(io_T)/int_t_sig, &
                  & int_ocn_opn_sig(io_DIC)/int_t_sig, &
                  & int_ocn_opn_sig(io_DIC_13C)/int_t_sig, &
                  & int_carb_sur_sig(:)/int_t_sig, &
                  & loc_carbisor(:) &
                  & )
          end IF
          if (ocn_select(io_DIC_14C)) then
             call sub_calc_carb_r14C( &
                  & int_ocn_opn_sig(io_T)/int_t_sig, &
                  & int_ocn_opn_sig(io_DIC)/int_t_sig, &
                  & int_ocn_opn_sig(io_DIC_14C)/int_t_sig, &
                  & int_carb_sur_sig(:)/int_t_sig, &
                  & loc_carbisor(:) &
                  & )
          end IF
       end IF
       ! write ocean surface carbonate chemistry data
       ! NOTE: also write d13C and d14C isotopic properties of the carbonate species (CO2(aq), HCO3-, and CO32-)
       !       depending on whether either or both of these isotopic tracers have been selected
       DO ic=1,n_carb
          loc_filename=fun_data_timeseries_filename( &
               & dum_t,par_outdir_name,trim(par_outfile_name)//'_series','carb_sur_'//TRIM(string_carb(ic)),string_results_ext &
               & )
          SELECT CASE (ic)
          CASE (ic_conc_CO2,ic_conc_HCO3,ic_conc_CO3)
             if (ocn_select(io_DIC_14C)) then
                loc_sig_opn = int_carb_opn_sig(ic)/int_t_sig
                loc_sig_sur = int_carb_sur_sig(ic)/int_t_sig
                call check_unit(out,__LINE__,__FILE__)
                OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
                call check_iostat(ios,__LINE__,__FILE__)
                WRITE(unit=out,fmt='(f12.3,e15.7,2f12.3,e15.7)',iostat=ios) &
                     & loc_t, &
                     & loc_sig_opn, &
                     & fun_calc_isotope_delta&
                     & (loc_sig_opn,loc_carbisor(ic - 1)*loc_sig_opn,const_standards(11),.FALSE.,const_nulliso), &
                     & fun_calc_isotope_delta &
                     & (loc_sig_opn,loc_carbisor(ic + 3)*loc_sig_opn,const_standards(12),.FALSE.,const_nulliso), &
                     & loc_sig_sur
                call check_iostat(ios,__LINE__,__FILE__)
                CLOSE(unit=out,iostat=ios)
                call check_iostat(ios,__LINE__,__FILE__)
             elseif (ocn_select(io_DIC_13C)) then
                loc_sig_sur = int_carb_sur_sig(ic)/int_t_sig
                loc_sig_opn = int_carb_opn_sig(ic)/int_t_sig
                call check_unit(out,__LINE__,__FILE__)
                OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
                WRITE(unit=out,fmt='(f12.3,e15.7,f12.3)',iostat=ios) &
                     & loc_t, &
                     & loc_sig_opn, &
                     & fun_calc_isotope_delta &
                     & (loc_sig_opn,loc_carbisor(ic - 1)*loc_sig_opn,const_standards(11),.FALSE.,const_nulliso), &
                     & loc_sig_sur
                call check_iostat(ios,__LINE__,__FILE__)
                CLOSE(unit=out,iostat=ios)
                call check_iostat(ios,__LINE__,__FILE__)
             else
                loc_sig_opn = int_carb_opn_sig(ic)/int_t_sig
                loc_sig_sur = int_carb_sur_sig(ic)/int_t_sig
                call check_unit(out,__LINE__,__FILE__)
                OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
                call check_iostat(ios,__LINE__,__FILE__)
                WRITE(unit=out,fmt='(f12.3,2e15.7)',iostat=ios) &
                     & loc_t, &
                     & loc_sig_opn, &
                     & loc_sig_sur
                call check_iostat(ios,__LINE__,__FILE__)
                CLOSE(unit=out,iostat=ios)
                call check_iostat(ios,__LINE__,__FILE__)
             end if
          case default
                loc_sig_opn = int_carb_opn_sig(ic)/int_t_sig
                loc_sig_sur = int_carb_sur_sig(ic)/int_t_sig
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             WRITE(unit=out,fmt='(f12.3,2e15.7)',iostat=ios) &
                  & loc_t, &
                  & loc_sig_opn, &
                  & loc_sig_sur
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
          end SELECT
       END DO
    end if

    ! *** <sig_ocnatm_*> ***
    ! write atmosphere tracer data
    ! NOTE: write data both as the total inventory, and as the equivalent mean partial pressure
    ! NOTE: simple conversion factor from atm to mol is used
    IF (ctrl_data_save_sig_ocnatm) THEN
       DO l=1,n_l_atm
          ia = conv_iselected_ia(l)
          loc_filename=fun_data_timeseries_filename( &
               & dum_t,par_outdir_name,trim(par_outfile_name)//'_series','atm_'//TRIM(string_atm(ia)),string_results_ext &
               & )
          SELECT CASE (atm_type(ia))
          CASE (0)
             loc_sig = int_ocnatm_sig(ia)/int_t_sig
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             If (ia == ia_T .OR. ia == ia_q)  then
                WRITE(unit=out,fmt='(f12.3,f12.6)',iostat=ios) &
                     & loc_t, &
                     & loc_sig
             elseif (ia == ia_pcolr) then
                WRITE(unit=out,fmt='(f12.3,e20.12,e14.6)',iostat=ios) &
                     & loc_t, &
                     & conv_atm_mol*loc_sig, &
                     & loc_sig
             end if
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
          CASE (1)
             loc_sig = int_ocnatm_sig(ia)/int_t_sig
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             WRITE(unit=out,fmt='(f12.3,e20.12,e14.6)',iostat=ios) &
                  & loc_t, &
                  & conv_atm_mol*loc_sig, &
                  & loc_sig
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
          case (n_itype_min:n_itype_max)
             loc_tot  = int_ocnatm_sig(atm_dep(ia))/int_t_sig
             loc_frac = int_ocnatm_sig(ia)/int_t_sig
             loc_standard = const_standards(atm_type(ia))
             loc_sig = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_nulliso)
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             WRITE(unit=out,fmt='(f12.3,e20.12,f14.3)',iostat=ios) &
                  & loc_t, &
                  & conv_atm_mol*loc_frac, &
                  & loc_sig
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
          end SELECT
       END DO
    END IF

    ! *** <sig_fexport_*> ***
    ! write export flux data
    ! NOTE: write data both as mole and mass flux
    IF (ctrl_data_save_sig_fexport) THEN
       DO l=1,n_l_sed
          is = conv_iselected_is(l)
          loc_filename=fun_data_timeseries_filename( &
               & dum_t,par_outdir_name,trim(par_outfile_name)//'_series','fexport_'//TRIM(string_sed(is)),string_results_ext &
               & )
          SELECT CASE (sed_type(is))
          CASE (par_sed_type_bio,par_sed_type_abio,par_sed_type_POM,par_sed_type_CaCO3,par_sed_type_opal,par_sed_type_det, &
               & par_sed_type_scavenged)
             loc_sig = int_fexport_sig(is)/int_t_sig
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             WRITE(unit=out,fmt='(f12.3,2e15.7,f9.3)',iostat=ios) &
                  & loc_t, &
                  & loc_sig, &
                  & loc_sig/loc_ocn_tot_A, &
                  & int_fracdom_sig(is)/int_t_sig
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
          CASE (par_sed_type_age)
             if (int_fexport_sig(sed_dep(is)) > const_real_nullsmall) then
                loc_sig = int_fexport_sig(is)/int_t_sig
             else
                loc_sig = 0.0
             end if
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             WRITE(unit=out,fmt='(f12.3,e15.7)',iostat=ios) &
                  & loc_t, &
                  & loc_sig
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
          case (n_itype_min:n_itype_max)
             loc_tot  = int_fexport_sig(sed_dep(is))/int_t_sig
             loc_frac = int_fexport_sig(is)/int_t_sig
             loc_standard = const_standards(sed_type(is))
             loc_sig = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_nulliso)
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             WRITE(unit=out,fmt='(f12.3,e15.7,f14.3)',iostat=ios) &
                  & loc_t, &
                  & loc_frac, &
                  & loc_sig
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
          end SELECT
       END DO
       ! export rain ratios
       if (sed_select(is_POC) .AND. sed_select(is_CaCO3)) then
          loc_filename=fun_data_timeseries_filename( &
               & dum_t,par_outdir_name,trim(par_outfile_name)//'_series','misc_CaCO3toPOC',string_results_ext)
          call check_unit(out,__LINE__,__FILE__)
          OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          if ((int_fexport_sig(is_POC) > const_real_nullsmall) .AND. (int_fexport_sig(is_CaCO3) > const_real_nullsmall)) then
             loc_sig  = int_fexport_sig(is_CaCO3)/int_fexport_sig(is_POC)
             loc_rsig = int_fexport_sig(is_POC)/int_fexport_sig(is_CaCO3)
          else
             loc_sig  = 0.0
             loc_rsig = 0.0
          end if
          WRITE(unit=out,fmt='(f12.3,2f12.6)',iostat=ios) loc_t,loc_sig,loc_rsig
          call check_iostat(ios,__LINE__,__FILE__)
          CLOSE(unit=out,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
       end if
       if (sed_select(is_POC) .AND. sed_select(is_opal)) then
          loc_filename=fun_data_timeseries_filename( &
               & dum_t,par_outdir_name,trim(par_outfile_name)//'_series','misc_opaltoPOC',string_results_ext)
          call check_unit(out,__LINE__,__FILE__)
          OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          if ((int_fexport_sig(is_POC) > const_real_nullsmall) .AND. (int_fexport_sig(is_opal) > const_real_nullsmall)) then
             loc_sig  = int_fexport_sig(is_opal)/int_fexport_sig(is_POC)
             loc_rsig = int_fexport_sig(is_POC)/int_fexport_sig(is_opal)
          else
             loc_sig  = 0.0
             loc_rsig = 0.0
          end if
          WRITE(unit=out,fmt='(f12.3,2f12.6)',iostat=ios) loc_t,loc_sig,loc_rsig
          call check_iostat(ios,__LINE__,__FILE__)
          CLOSE(unit=out,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
       end if
       if (sed_select(is_POC) .AND. sed_select(is_POP)) then
          loc_filename=fun_data_timeseries_filename( &
               & dum_t,par_outdir_name,trim(par_outfile_name)//'_series','misc_POPtoPOC',string_results_ext)
          call check_unit(out,__LINE__,__FILE__)
          OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          if ((int_fexport_sig(is_POC) > const_real_nullsmall) .AND. (int_fexport_sig(is_POP) > const_real_nullsmall)) then
             loc_sig  = 1.0E3*int_fexport_sig(is_POP)/int_fexport_sig(is_POC)
             loc_rsig = int_fexport_sig(is_POC)/int_fexport_sig(is_POP)
          else
             loc_sig  = 0.0
             loc_rsig = 0.0
          end if
          WRITE(unit=out,fmt='(f12.3,2f12.6)',iostat=ios) loc_t,loc_sig,loc_rsig
          call check_iostat(ios,__LINE__,__FILE__)
          CLOSE(unit=out,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
       end if
       if (sed_select(is_POC) .AND. sed_select(is_POFe)) then
          loc_filename=fun_data_timeseries_filename( &
               & dum_t,par_outdir_name,trim(par_outfile_name)//'_series','misc_POFetoPOC',string_results_ext)
          call check_unit(out,__LINE__,__FILE__)
          OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          if ((int_fexport_sig(is_POC) > const_real_nullsmall) .AND. (int_fexport_sig(is_POFe) > const_real_nullsmall)) then
             loc_sig  = 1.0E6*int_fexport_sig(is_POFe)/int_fexport_sig(is_POC)
             loc_rsig = int_fexport_sig(is_POC)/int_fexport_sig(is_POFe)
          else
             loc_sig  = 0.0
             loc_rsig = 0.0
          end if
          WRITE(unit=out,fmt='(f12.3,2f12.6)',iostat=ios) loc_t,loc_sig,loc_rsig
          call check_iostat(ios,__LINE__,__FILE__)
          CLOSE(unit=out,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
       end if
    END IF

    ! *** <int_diag_fseaair_sig_*> ***
    ! write air-sea has exchange flux data
    ! NOTE: write data both as the total flux, and as the equivalent mean flux density
    ! NOTE: a positive value of the array represents net ocean to atmosphere transfer
    IF (ctrl_data_save_sig_fairsea) THEN
       DO l=3,n_l_atm
          ia = conv_iselected_ia(l)
          loc_filename=fun_data_timeseries_filename( &
               & dum_t,par_outdir_name,trim(par_outfile_name)//'_series','fseaair_'//TRIM(string_atm(ia)),string_results_ext &
               & )
          SELECT CASE (atm_type(ia))
          CASE (0,1)
             loc_sig = int_diag_airsea_sig(ia)/int_t_sig
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             WRITE(unit=out,fmt='(f12.3,e15.7,f12.3)',iostat=ios) &
                  & loc_t, &
                  & loc_sig, &
                  & loc_sig/loc_ocn_tot_A
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
          case (n_itype_min:n_itype_max)
             loc_tot  = int_diag_airsea_sig(atm_dep(ia))/int_t_sig
             loc_frac = int_diag_airsea_sig(ia)/int_t_sig
             loc_standard = const_standards(atm_type(ia))
             loc_sig = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.TRUE.,const_nulliso)
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             WRITE(unit=out,fmt='(f12.3,e15.7,f14.3)',iostat=ios) &
                  & loc_t, &
                  & loc_frac, &
                  & loc_sig
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
          end SELECT
       END DO
    END IF

    ! *** <sig_focnatm_*> ***
    ! write ocean-atmopshere interface flux data
    ! NOTE: write data both as the total flux, and as the equivalent mean flux density
    IF (ctrl_data_save_sig_focnatm) THEN
       DO l=3,n_l_atm
          ia = conv_iselected_ia(l)
          loc_filename=fun_data_timeseries_filename( &
               & dum_t,par_outdir_name,trim(par_outfile_name)//'_series','focnatm_'//TRIM(string_atm(ia)),string_results_ext &
               & )
          SELECT CASE (atm_type(ia))
          CASE (0,1)
             loc_sig = int_focnatm_sig(ia)/int_t_sig
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             WRITE(unit=out,fmt='(f12.3,e15.7,f12.3)',iostat=ios) &
                  & loc_t, &
                  & loc_sig, &
                  & loc_sig/loc_ocn_tot_A
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
          case (n_itype_min:n_itype_max)
             loc_tot  = int_focnatm_sig(atm_dep(ia))/int_t_sig
             loc_frac = int_focnatm_sig(ia)/int_t_sig
             loc_standard = const_standards(atm_type(ia))
             loc_sig = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.TRUE.,const_nulliso)
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             WRITE(unit=out,fmt='(f12.3,e15.7,f14.3)',iostat=ios) &
                  & loc_t, &
                  & loc_frac, &
                  & loc_sig
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
          end SELECT
       END DO
    END IF

    ! *** <sig_focnsed_*> ***
    ! write ocean-sediment flux data
    ! NOTE: write data both as the total flux, and as the equivalent mean flux density
    ! NOTE: the surface ocean area is used as a proxy for the ocean bottom area
    IF (ctrl_data_save_sig_focnsed) THEN
       DO l=1,n_l_sed
          is = conv_iselected_is(l)
          loc_filename=fun_data_timeseries_filename( &
               & dum_t,par_outdir_name,trim(par_outfile_name)//'_series','focnsed_'//TRIM(string_sed(is)),string_results_ext &
               & )
          SELECT CASE (sed_type(is))
          CASE (par_sed_type_bio,par_sed_type_abio,par_sed_type_POM,par_sed_type_CaCO3,par_sed_type_opal,par_sed_type_det, &
               & par_sed_type_scavenged)
             loc_sig = int_focnsed_sig(is)/int_t_sig
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             WRITE(unit=out,fmt='(f12.3,2e15.7)',iostat=ios) &
                  & loc_t, &
                  & loc_sig, &
                  & loc_sig/loc_ocn_tot_A
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
          CASE (par_sed_type_age)
             if (int_focnsed_sig(sed_dep(is)) > const_real_nullsmall) then
                loc_sig = int_focnsed_sig(is)/int_t_sig
             else
                loc_sig = 0.0
             end if
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             WRITE(unit=out,fmt='(f12.3,e15.7)',iostat=ios) &
                  & loc_t, &
                  & loc_sig
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
          case (n_itype_min:n_itype_max)
             loc_tot  = int_focnsed_sig(sed_dep(is))/int_t_sig
             loc_frac = int_focnsed_sig(is)/int_t_sig
             loc_standard = const_standards(sed_type(is))
             loc_sig = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.TRUE.,const_nulliso)
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             WRITE(unit=out,fmt='(f12.3,e15.7,f14.3)',iostat=ios) &
                  & loc_t, &
                  & loc_frac, &
                  & loc_sig
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
          end SELECT
       END DO
    END IF

    ! *** <sig_fsedocn_*> ***
    ! write sediment->ocean flux data
    ! NOTE: write data both as the total flux, and as the equivalent mean flux density
    ! NOTE: the surface ocean area is used as a proxy for the ocean bottom area
    IF (ctrl_data_save_sig_fsedocn) THEN
       DO l=1,n_l_ocn
          io = conv_iselected_io(l)
          loc_filename=fun_data_timeseries_filename( &
               & dum_t,par_outdir_name,trim(par_outfile_name)//'_series','fsedocn_'//TRIM(string_ocn(io)),string_results_ext &
               & )
          SELECT CASE (ocn_type(io))
          CASE (1)
             loc_sig = int_fsedocn_sig(io)/int_t_sig
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             WRITE(unit=out,fmt='(f12.3,2e15.7)',iostat=ios) &
                  & loc_t, &
                  & loc_sig, &
                  & loc_sig/loc_ocn_tot_A
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
          case (n_itype_min:n_itype_max)
             loc_tot  = int_fsedocn_sig(ocn_dep(io))/int_t_sig
             loc_frac = int_fsedocn_sig(io)/int_t_sig
             loc_standard = const_standards(ocn_type(io))
             loc_sig = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.TRUE.,const_nulliso)
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             WRITE(unit=out,fmt='(f12.3,e15.7,f12.3)',iostat=ios) &
                  & loc_t, &
                  & loc_frac, &
                  & loc_sig
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
          end SELECT
       END DO
    END IF

    ! *** <sig_ocnsed_*> ***
    ! write sediment (core-top) composition data
    ! NOTE: the call to fun_sed_coretop made in populating <loc_sed_coretop> has already made the necessary type conversions
    !       for solid tracers as wt%, isotopes in per mill, and recovery of the carbonate 'age' value
    IF (ctrl_data_save_sig_ocnsed) THEN
       DO l=1,n_l_sed
          is = conv_iselected_is(l)
          loc_filename=fun_data_timeseries_filename( &
               & dum_t,par_outdir_name,trim(par_outfile_name)//'_series','sed_'//TRIM(string_sed(is)),string_results_ext &
               & )
          SELECT CASE (sed_type(is))
          CASE (par_sed_type_bio,par_sed_type_abio,par_sed_type_POM,par_sed_type_CaCO3,par_sed_type_opal,par_sed_type_det, &
               & par_sed_type_scavenged)
             loc_sig = int_ocnsed_sig(is)/int_t_sig
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             WRITE(unit=out,fmt='(f12.3,f12.6)',iostat=ios) &
                  & loc_t, &
                  & loc_sig
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
          CASE (n_itype_min:n_itype_max)
             loc_sig = int_ocnsed_sig(is)/int_t_sig
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             WRITE(unit=out,fmt='(f12.3,f14.3)',iostat=ios) &
                  & loc_t, &
                  & loc_sig
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
          CASE (par_sed_type_age)
             loc_sig = int_ocnsed_sig(is)/int_t_sig
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             WRITE(unit=out,fmt='(f12.3,f14.3)',iostat=ios) &
                  & loc_t, &
                  & loc_sig
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
          end SELECT
       END DO
    END IF

    ! *** <sig_misc_*> ***
    ! write miscellaneous data (if requested)
    IF (ctrl_data_save_sig_misc) THEN
       if (flag_gemlite) then
          loc_filename=fun_data_timeseries_filename( &
               & dum_t,par_outdir_name,trim(par_outfile_name)//'_series','misc_gemlite',string_results_ext)
          call check_unit(out,__LINE__,__FILE__)
          OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          WRITE(unit=out,fmt='(f12.3,e12.4)',iostat=ios) &
               & loc_t, &
               & (int_misc_gemlite_sig/int_t_sig)
          call check_iostat(ios,__LINE__,__FILE__)
          CLOSE(unit=out,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
       end if
       !
       loc_filename=fun_data_timeseries_filename( &
            & dum_t,par_outdir_name,trim(par_outfile_name)//'_series','misc_seaice',string_results_ext)
       call check_unit(out,__LINE__,__FILE__)
       OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
       WRITE(unit=out,fmt='(f12.3,e12.4,f9.3,e12.4,f9.3)',iostat=ios) &
            & loc_t, &
            & (int_misc_seaice_sig/int_t_sig), &
            & 100.0*(1.0/SUM(phys_ocn(ipo_A,:,:,n_k)))*int_misc_seaice_sig/int_t_sig, &
            & (int_misc_seaice_sig_vol/int_t_sig), &
            & (int_misc_seaice_sig_th/int_t_sig)
       call check_iostat(ios,__LINE__,__FILE__)
       CLOSE(unit=out,iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
       !
       loc_filename=fun_data_timeseries_filename( &
            & dum_t,par_outdir_name,trim(par_outfile_name)//'_series','misc_opsi',string_results_ext)
       call check_unit(out,__LINE__,__FILE__)
       OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
       select case (fname_topo)
       case ('worbe2', 'worjh2', 'worjh4', 'worlg2', 'worlg4', 'wv2jh2', 'wv3jh2', 'worri4')
          WRITE(unit=out,fmt='(f12.3,4f9.3)',iostat=ios)          &
               & loc_t,                                           &
               & loc_opsi_scale*int_misc_opsi_min_sig/int_t_sig,  &
               & loc_opsi_scale*int_misc_opsi_max_sig/int_t_sig,  &
               & loc_opsi_scale*int_misc_opsia_min_sig/int_t_sig, &
               & loc_opsi_scale*int_misc_opsia_max_sig/int_t_sig
       case default
          WRITE(unit=out,fmt='(f12.3,2f9.3)',iostat=ios)          &
               & loc_t,                                           &
               & loc_opsi_scale*int_misc_opsi_min_sig/int_t_sig,  &
               & loc_opsi_scale*int_misc_opsi_max_sig/int_t_sig
       end select
       call check_iostat(ios,__LINE__,__FILE__)
       CLOSE(unit=out,iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
       ! atmospheric CO2 D14C
       IF (atm_select(ia_pCO2_14C)) THEN
          loc_tot  = int_ocnatm_sig(atm_dep(ia_pCO2))/int_t_sig
          loc_frac = int_ocnatm_sig(ia_pCO2_13C)/int_t_sig
          loc_standard = const_standards(atm_type(ia_pCO2_13C))
          loc_d13C = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_nulliso)
          loc_frac = int_ocnatm_sig(ia_pCO2_14C)/int_t_sig
          loc_standard = const_standards(atm_type(ia_pCO2_14C))
          loc_d14C = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_nulliso)
          loc_sig = fun_convert_delta14CtoD14C(loc_d13C,loc_d14C)
          loc_filename=fun_data_timeseries_filename( &
               & dum_t,par_outdir_name,trim(par_outfile_name)//'_series','misc_atm_D14C',string_results_ext)
          call check_unit(out,__LINE__,__FILE__)
          OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          WRITE(unit=out,fmt='(f12.3,f12.3)',iostat=ios) loc_t,loc_sig
          call check_iostat(ios,__LINE__,__FILE__)
          CLOSE(unit=out,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
       end IF
       ! pH
       ! NOTE: catch case of zero average [H+]!!!
       IF (ctrl_data_save_sig_carb_sur) THEN
          loc_filename=fun_data_timeseries_filename( &
               & dum_t,par_outdir_name,trim(par_outfile_name)//'_series','misc_surpH',string_results_ext)
          call check_unit(out,__LINE__,__FILE__)
          OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          loc_sig = (int_carb_sur_sig(ic_H)/int_t_sig)
          if (loc_sig > const_real_nullsmall) then
             loc_sig = -log10(loc_sig)
          else
             loc_sig = 0.0
          end if
          WRITE(unit=out,fmt='(f12.3,f9.6)',iostat=ios) loc_t,loc_sig
          call check_iostat(ios,__LINE__,__FILE__)
          CLOSE(unit=out,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
       end IF
       ! SLT
       loc_filename = fun_data_timeseries_filename( &
            & dum_t,par_outdir_name,trim(par_outfile_name)//'_series','misc_SLT',string_results_ext)
       call check_unit(out,__LINE__,__FILE__)
       OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
       WRITE(unit=out,fmt='(f12.3,f12.6)',iostat=ios) loc_t,int_misc_SLT_sig/int_t_sig
       call check_iostat(ios,__LINE__,__FILE__)
       CLOSE(unit=out,iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
       ! aeolian Fe diagnostics
       IF (ocn_select(io_Fe)) THEN
          loc_filename=fun_data_timeseries_filename( &
               & loc_t,par_outdir_name,trim(par_outfile_name)//'_series','misc_det_Fe_tot',string_results_ext)
          call check_unit(out,__LINE__,__FILE__)
          OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          WRITE(unit=out,fmt='(f12.3,e12.4)',iostat=ios) &
               & loc_t, &
               & (int_misc_det_Fe_tot_sig/int_t_sig)
          call check_iostat(ios,__LINE__,__FILE__)
          CLOSE(unit=out,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          loc_filename=fun_data_timeseries_filename( &
               & loc_t,par_outdir_name,trim(par_outfile_name)//'_series','misc_det_Fe_dis',string_results_ext)
          call check_unit(out,__LINE__,__FILE__)
          OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          WRITE(unit=out,fmt='(f12.3,e12.4)',iostat=ios) &
               & loc_t, &
               & (int_misc_det_Fe_dis_sig/int_t_sig)
          call check_iostat(ios,__LINE__,__FILE__)
          CLOSE(unit=out,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          if (int_misc_det_Fe_tot_sig > const_real_nullsmall) then
             loc_sig = 100.0*int_misc_det_Fe_dis_sig/int_misc_det_Fe_tot_sig
          else
             loc_sig = 0.0
          end if
          loc_filename=fun_data_timeseries_filename( &
               & loc_t,par_outdir_name,trim(par_outfile_name)//'_series','misc_det_Fe_sol',string_results_ext)
          call check_unit(out,__LINE__,__FILE__)
          OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          WRITE(unit=out,fmt='(f12.3,f9.3)',iostat=ios) &
               & loc_t, &
               & loc_sig
          call check_iostat(ios,__LINE__,__FILE__)
          CLOSE(unit=out,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
       end IF
       ! Sr diagnostics
       IF (ocn_select(io_Sr_87Sr) .AND. ocn_select(io_Sr_88Sr)) THEN
          ! all Sr species
          loc_filename=fun_data_timeseries_filename(loc_t, &
               & par_outdir_name,trim(par_outfile_name)//'_series','misc_Sr_ocn_Sr',string_results_ext)
          call check_unit(out,__LINE__,__FILE__)
          OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          WRITE(unit=out,fmt='(f12.3,4e15.7)',iostat=ios) &
               & loc_t, &
               & int_ocn_sig(io_Sr)/int_t_sig, &
               & (int_ocn_sig(io_Sr)-int_ocn_sig(io_Sr_87Sr)-int_ocn_sig(io_Sr_88Sr))/int_t_sig, &
               & int_ocn_sig(io_Sr_87Sr)/int_t_sig, &
               & int_ocn_sig(io_Sr_88Sr)/int_t_sig
          call check_iostat(ios,__LINE__,__FILE__)
          CLOSE(unit=out,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          ! 87Sr
          ! NOTE: 87Sr/86Sr == 87Sr / (Sr(tot) - 87Sr - 88Sr)
          !       in calculations, variable loc_tot == 86Sr
          loc_filename=fun_data_timeseries_filename(loc_t, &
               & par_outdir_name,trim(par_outfile_name)//'_series','misc_Sr_ocn_r87Sr',string_results_ext)
          call check_unit(out,__LINE__,__FILE__)
          OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          loc_tot = int_ocn_sig(io_Sr)-int_ocn_sig(io_Sr_87Sr)-int_ocn_sig(io_Sr_88Sr)
          loc_frac = int_ocn_sig(io_Sr_87Sr)/int_t_sig
          if (loc_tot > const_real_nullsmall) then
             loc_sig = int_ocn_sig(io_Sr_87Sr)/loc_tot
          else
             loc_sig = -999.9
          end if
          WRITE(unit=out,fmt='(f12.3,e15.7,f12.7)',iostat=ios) &
               & loc_t, &
               & loc_frac, &
               & loc_sig
          call check_iostat(ios,__LINE__,__FILE__)
          CLOSE(unit=out,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          ! 88Sr
          loc_filename=fun_data_timeseries_filename(loc_t, &
               & par_outdir_name,trim(par_outfile_name)//'_series','misc_Sr_ocn_d88Sr',string_results_ext)
          call check_unit(out,__LINE__,__FILE__)
          OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          loc_tot  = (int_ocn_sig(io_Sr)-int_ocn_sig(io_Sr_87Sr)-int_ocn_sig(io_Sr_88Sr))/int_t_sig
          loc_frac = int_ocn_sig(io_Sr_88Sr)/int_t_sig
          loc_standard = const_standardsR(ocn_type(io_Sr_88Sr))
          loc_sig = fun_calc_isotope_deltaR(loc_tot,loc_frac,loc_standard,const_real_null)
          WRITE(unit=out,fmt='(f12.3,2e15.7,f12.7)',iostat=ios) &
               & loc_t, &
               & loc_frac, &
               & loc_sig
          call check_iostat(ios,__LINE__,__FILE__)
          CLOSE(unit=out,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__) 
          ! all Sr species
          loc_filename=fun_data_timeseries_filename(loc_t, &
               & par_outdir_name,trim(par_outfile_name)//'_series','misc_Sr_fexport_Sr',string_results_ext)
          call check_unit(out,__LINE__,__FILE__)
          OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          WRITE(unit=out,fmt='(f12.3,4e15.7)',iostat=ios) &
               & loc_t, &
               & int_fexport_sig(is_SrCO3)/int_t_sig, &
               & (int_fexport_sig(is_SrCO3)-int_fexport_sig(is_SrCO3_87Sr)-int_fexport_sig(is_SrCO3_88Sr))/int_t_sig, &
               & int_fexport_sig(is_SrCO3_87Sr)/int_t_sig, &
               & int_fexport_sig(is_SrCO3_88Sr)/int_t_sig
          call check_iostat(ios,__LINE__,__FILE__)
          CLOSE(unit=out,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          IF (flag_sedgem) THEN
             ! (1) OCN -> SED FLUXES
             ! all Sr species
             loc_filename=fun_data_timeseries_filename(loc_t, &
                  & par_outdir_name,trim(par_outfile_name)//'_series','misc_Sr_focnsed_Sr',string_results_ext)
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             WRITE(unit=out,fmt='(f12.3,4e15.7)',iostat=ios) &
                  & loc_t, &
                  & int_focnsed_sig(is_SrCO3)/int_t_sig, &
                  & (int_focnsed_sig(is_SrCO3)-int_focnsed_sig(is_SrCO3_87Sr)-int_focnsed_sig(is_SrCO3_88Sr))/int_t_sig, &
                  & int_focnsed_sig(is_SrCO3_87Sr)/int_t_sig, &
                  & int_focnsed_sig(is_SrCO3_88Sr)/int_t_sig
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             ! 87Sr
             ! NOTE: 87Sr/86Sr == 87Sr / (Sr(tot) - 87Sr - 88Sr)
             loc_filename=fun_data_timeseries_filename(loc_t, &
                  & par_outdir_name,trim(par_outfile_name)//'_series','misc_Sr_focnsed_r87Sr',string_results_ext)
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             loc_tot = int_focnsed_sig(is_SrCO3)-int_focnsed_sig(is_SrCO3_87Sr)-int_focnsed_sig(is_SrCO3_88Sr)
             if (loc_tot > const_real_nullsmall) then
                loc_sig = int_focnsed_sig(is_SrCO3_87Sr)/loc_tot
             else
                loc_sig = -999.9
             end if
             WRITE(unit=out,fmt='(f12.3,e15.7,f12.7)',iostat=ios) &
                  & loc_t, &
                  & int_focnsed_sig(is_SrCO3_87Sr), &
                  & loc_sig
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             ! 88Sr
             loc_filename=fun_data_timeseries_filename(loc_t, &
                  & par_outdir_name,trim(par_outfile_name)//'_series','misc_Sr_focnsed_d88Sr',string_results_ext)
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             loc_tot  = (int_focnsed_sig(is_SrCO3)-int_focnsed_sig(is_SrCO3_87Sr)-int_focnsed_sig(is_SrCO3_88Sr))/int_t_sig
             loc_frac = int_focnsed_sig(is_SrCO3_88Sr)/int_t_sig
             loc_standard = const_standardsR(ocn_type(io_Sr_88Sr))
             loc_sig = fun_calc_isotope_deltaR(loc_tot,loc_frac,loc_standard,const_real_null)
             WRITE(unit=out,fmt='(f12.3,e15.7,f12.7)',iostat=ios) &
                  & loc_t, &
                  & loc_frac, &
                  & loc_sig
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             ! (2) SED -> OCN FLUXES
             ! all Sr species
             loc_filename=fun_data_timeseries_filename(loc_t, &
                  & par_outdir_name,trim(par_outfile_name)//'_series','misc_Sr_fsedocn_Sr',string_results_ext)
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             WRITE(unit=out,fmt='(f12.3,4e15.7)',iostat=ios) &
                  & loc_t, &
                  & int_fsedocn_sig(io_Sr)/int_t_sig, &
                  & (int_fsedocn_sig(io_Sr)-int_fsedocn_sig(io_Sr_87Sr)-int_fsedocn_sig(io_Sr_88Sr))/int_t_sig, &
                  & int_fsedocn_sig(io_Sr_87Sr)/int_t_sig, &
                  & int_fsedocn_sig(io_Sr_88Sr)/int_t_sig
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             ! 87Sr
             ! NOTE: 87Sr/86Sr == 87Sr / (Sr(tot) - 87Sr - 88Sr)
             loc_filename=fun_data_timeseries_filename(loc_t, &
                  & par_outdir_name,trim(par_outfile_name)//'_series','misc_Sr_fsedocn_r87Sr',string_results_ext)
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             loc_tot = int_fsedocn_sig(io_Sr)-int_fsedocn_sig(io_Sr_87Sr)-int_fsedocn_sig(io_Sr_88Sr)
             if (abs(loc_tot) > const_real_nullsmall) then
                loc_sig = int_fsedocn_sig(io_Sr_87Sr)/loc_tot
             else
                loc_sig = -999.9
             end if
             WRITE(unit=out,fmt='(f12.3,e15.7,f12.7)',iostat=ios) &
                  & loc_t, &
                  & int_fsedocn_sig(io_Sr_87Sr), &
                  & loc_sig
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             ! 88Sr
             loc_filename=fun_data_timeseries_filename(loc_t, &
                  & par_outdir_name,trim(par_outfile_name)//'_series','misc_Sr_fsedocn_d88Sr',string_results_ext)
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             loc_tot  = (int_fsedocn_sig(io_Sr)-int_fsedocn_sig(io_Sr_87Sr)-int_fsedocn_sig(io_Sr_88Sr))/int_t_sig
             loc_frac = int_fsedocn_sig(io_Sr_88Sr)/int_t_sig
             loc_standard = const_standardsR(ocn_type(io_Sr_88Sr))
             loc_sig = fun_calc_isotope_deltaR(loc_tot,loc_frac,loc_standard,const_real_null)
             WRITE(unit=out,fmt='(f12.3,e15.7,f12.7)',iostat=ios) &
                  & loc_t, &
                  & loc_frac, &
                  & loc_sig
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
!!$          loc_filename=fun_data_timeseries_filename(loc_t, &
!!$               & par_outdir_name,trim(par_outfile_name)//'_series','misc_sed_r87Sr',string_results_ext)
!!$          call check_unit(out,__LINE__,__FILE__)
!!$          OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
!!$          call check_iostat(ios,__LINE__,__FILE__)
!!$          loc_tot = int_ocnsed_sig(is_SrCO3)-int_ocnsed_sig(is_SrCO3_87Sr)-int_ocnsed_sig(is_SrCO3_88Sr)
!!$          if (loc_tot > const_real_nullsmall) then
!!$             loc_sig = int_ocnsed_sig(is_SrCO3_87Sr)/loc_tot
!!$          else
!!$             loc_sig = -999.9
!!$          end if
!!$          WRITE(unit=out,fmt='(f12.3,f12.6)',iostat=ios) &
!!$               & loc_t, &
!!$               & loc_sig
!!$          call check_iostat(ios,__LINE__,__FILE__)
!!$          CLOSE(unit=out,iostat=ios)
!!$          call check_iostat(ios,__LINE__,__FILE__)
!!$          loc_filename=fun_data_timeseries_filename(loc_t, &
!!$               & par_outdir_name,trim(par_outfile_name)//'_series','misc_sed_d88Sr',string_results_ext)
!!$          call check_unit(out,__LINE__,__FILE__)
!!$          OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
!!$          call check_iostat(ios,__LINE__,__FILE__)
!!$          loc_tot  = (int_ocnsed_sig(is_SrCO3)-int_ocnsed_sig(is_SrCO3_87Sr))/int_t_sig
!!$          loc_frac = int_ocnsed_sig(is_SrCO3_88Sr)/int_t_sig
!!$          loc_standard = const_standard_88Sr
!!$          loc_sig = fun_calc_isotope_deltaR(loc_tot,loc_frac,loc_standard,.TRUE.,const_nulliso)
!!$          WRITE(unit=out,fmt='(f12.3,f12.3)',iostat=ios) &
!!$               & loc_t, &
!!$               & loc_sig
!!$          call check_iostat(ios,__LINE__,__FILE__)
!!$          CLOSE(unit=out,iostat=ios)
!!$          call check_iostat(ios,__LINE__,__FILE__)
          end if
          if (flag_gemlite) then
             ! all Sr species
             loc_filename=fun_data_timeseries_filename(loc_t, &
                  & par_outdir_name,trim(par_outfile_name)//'_series','misc_Sr_weather_Sr',string_results_ext)
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             WRITE(unit=out,fmt='(f12.3,4e15.7)',iostat=ios) &
                  & loc_t, &
                  & int_diag_weather_sig(io_Sr)/int_t_sig, &
                  & (int_diag_weather_sig(io_Sr)-int_diag_weather_sig(io_Sr_87Sr)-int_diag_weather_sig(io_Sr_88Sr))/int_t_sig, &
                  & int_diag_weather_sig(io_Sr_87Sr)/int_t_sig, &
                  & int_diag_weather_sig(io_Sr_88Sr)/int_t_sig
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             ! 87Sr
             ! NOTE: 87Sr/86Sr == 87Sr / (Sr(tot) - 87Sr - 88Sr)
             !       in calculations, variable loc_tot == 86Sr
             loc_filename=fun_data_timeseries_filename(loc_t, &
                  & par_outdir_name,trim(par_outfile_name)//'_series','misc_Sr_weather_r87Sr',string_results_ext)
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             loc_tot = int_diag_weather_sig(io_Sr)-int_diag_weather_sig(io_Sr_87Sr)-int_diag_weather_sig(io_Sr_88Sr)
             if (loc_tot > const_real_nullsmall) then
                loc_sig = int_diag_weather_sig(io_Sr_87Sr)/loc_tot
             else
                loc_sig = -999.9
             end if
             WRITE(unit=out,fmt='(f12.3,e15.7,f12.7)',iostat=ios) &
                  & loc_t, &
                  & int_diag_weather_sig(io_Sr_87Sr)/int_t_sig, &
                  & loc_sig
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             ! 88Sr
             loc_filename=fun_data_timeseries_filename(loc_t, &
                  & par_outdir_name,trim(par_outfile_name)//'_series','misc_Sr_weather_d88Sr',string_results_ext)
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             loc_tot  = (int_diag_weather_sig(io_Sr)-int_diag_weather_sig(io_Sr_87Sr)-int_diag_weather_sig(io_Sr_88Sr))/int_t_sig
             loc_frac = int_diag_weather_sig(io_Sr_88Sr)/int_t_sig
             loc_standard = const_standardsR(ocn_type(io_Sr_88Sr))
             loc_sig = fun_calc_isotope_deltaR(loc_tot,loc_frac,loc_standard,const_real_null)
             WRITE(unit=out,fmt='(f12.3,2e15.7,f12.7)',iostat=ios) &
                  & loc_t, &
                  & loc_tot, &
                  & loc_frac, &
                  & loc_sig
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)  
          end if
       end if
       ! insolation (wet grid only)
       loc_filename=fun_data_timeseries_filename( &
            & dum_t,par_outdir_name,trim(par_outfile_name)//'_series','misc_ocn_insol',string_results_ext)
       call check_unit(out,__LINE__,__FILE__)
       OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
       WRITE(unit=out,fmt='(f12.3,3e12.4)',iostat=ios) &
            & loc_t, &
            & (int_misc_ocn_solfor_sig/int_t_sig), &
            & snap_misc_ocn_solfor_N_sig, &
            & snap_misc_ocn_solfor_S_sig
       call check_iostat(ios,__LINE__,__FILE__)
       CLOSE(unit=out,iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
       ! SW flux at surface (wet grid only)
       loc_filename=fun_data_timeseries_filename( &
            & dum_t,par_outdir_name,trim(par_outfile_name)//'_series','misc_ocn_swflux',string_results_ext)
       call check_unit(out,__LINE__,__FILE__)
       OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
       WRITE(unit=out,fmt='(f12.3,e12.4)',iostat=ios) &
            & loc_t, &
            & (int_misc_ocn_fxsw_sig/int_t_sig)
       call check_iostat(ios,__LINE__,__FILE__)
       CLOSE(unit=out,iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
    end if
    ! diagnostic diagnostics
    IF (ctrl_data_save_sig_diag_bio .AND. ctrl_data_save_sig_fexport) THEN
       DO ib=1,n_diag_bio
          loc_sig = int_diag_bio_sig(ib)/int_t_sig
          loc_filename=fun_data_timeseries_filename(loc_t, &
               & par_outdir_name,trim(par_outfile_name)//'_series_diag_bio',trim(string_diag_bio(ib)),string_results_ext)
          call check_unit(out,__LINE__,__FILE__)
          OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          SELECT CASE (ib)
          CASE (idiag_bio_dPO4,idiag_bio_dPO4_1,idiag_bio_dPO4_2,idiag_bio_N2fixation,idiag_bio_NH4assim)
             WRITE(unit=out,fmt='(f16.3,e15.6)',iostat=ios) &
                  & loc_t,                        &
                  & loc_ocn_tot_M_sur*loc_sig
          CASE (idiag_bio_CaCO3toPOC_nsp,idiag_bio_opaltoPOC_sp,idiag_bio_fspPOC)
             ! correct for the number of sub-slices to create an average
             WRITE(unit=out,fmt='(f16.3,e15.6)',iostat=ios) &
                  & loc_t,                        &
                  & loc_sig/real(int_t_sig_count)
          case default
             WRITE(unit=out,fmt='(f16.3,e15.6)',iostat=ios) &
                  & loc_t,                        &
                  & loc_sig
          end select
          call check_iostat(ios,__LINE__,__FILE__)
          CLOSE(unit=out,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
       end DO
    end if
    IF (ctrl_data_save_sig_diag_geochem .AND. ctrl_data_save_sig_diag_redox_old) THEN
       DO id=1,n_diag_geochem
          loc_sig = int_diag_geochem_sig(id)/int_t_sig
          loc_filename=fun_data_timeseries_filename(loc_t, &
               & par_outdir_name,trim(par_outfile_name)//'_series_diag_geochem',trim(string_diag_geochem(id)),string_results_ext)
          call check_unit(out,__LINE__,__FILE__)
          OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          WRITE(unit=out,fmt='(f16.3,2e15.6)',iostat=ios) &
               & loc_t,                        &
               & loc_ocn_tot_M*loc_sig,        &
               & loc_sig
          call check_iostat(ios,__LINE__,__FILE__)
          CLOSE(unit=out,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
       end DO
    end if
    IF (ctrl_data_save_sig_diag_geochem) THEN
       DO id=1,n_diag_redox
          loc_sig = int_diag_redox_sig(id)/int_t_sig
          loc_filename=fun_data_timeseries_filename(loc_t, &
               & par_outdir_name,trim(par_outfile_name)//'_series_diag_redox',trim(string_diag_redox(id)),string_results_ext)
          call check_unit(out,__LINE__,__FILE__)
          OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          WRITE(unit=out,fmt='(f16.3,2e15.6)',iostat=ios) &
               & loc_t,                        &
               & loc_ocn_tot_M*loc_sig,        &
               & loc_sig
          call check_iostat(ios,__LINE__,__FILE__)
          CLOSE(unit=out,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
       end DO
    end if
    IF (ctrl_data_save_sig_diag .AND. flag_rokgem) THEN
       DO l=1,n_l_ocn
          io = conv_iselected_io(l)
          loc_filename=fun_data_timeseries_filename(dum_t, &
               & par_outdir_name,trim(par_outfile_name)//'_series_diag_weather',TRIM(string_ocn(io)),string_results_ext)
          SELECT CASE (ocn_type(io))
          CASE (1)
             loc_sig = int_diag_weather_sig(io)/int_t_sig
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             WRITE(unit=out,fmt='(f12.3,e15.7)',iostat=ios) &
                  & loc_t,                                  &
                  & loc_sig
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
          case (n_itype_min:n_itype_max)
             loc_tot      = int_diag_weather_sig(ocn_dep(io))/int_t_sig
             loc_frac     = int_diag_weather_sig(io)/int_t_sig
             loc_standard = const_standards(ocn_type(io))
             loc_sig      = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_nulliso)
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             WRITE(unit=out,fmt='(f12.3,e15.7,f12.3)',iostat=ios) &
                  & loc_t,                                        &
                  & loc_frac,                       &
                  & loc_sig
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
          END SELECT
       END DO
       IF (ocn_select(io_DIC) .AND. ocn_select(io_Ca)) THEN
          ! NOTE: first calculate excess of cations compared to DIC
          loc_filename=fun_data_timeseries_filename(loc_t, &
               & par_outdir_name,trim(par_outfile_name)//'_series_misc_exweather','Ca',string_results_ext &
               & )
          loc_tot = ((int_diag_weather_sig(io_Ca)+int_diag_weather_sig(io_Mg)) - int_diag_weather_sig(io_DIC))/int_t_sig
          if (int_diag_weather_sig(io_DIC) > const_real_nullsmall) then
             loc_frac = 100.0*(loc_tot/(int_diag_weather_sig(io_DIC)/int_t_sig))
          else
             loc_frac = -999.9
          end if
          OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
          WRITE(unit=out,fmt='(f12.3,e15.7,f12.3)',iostat=ios) &
               & loc_t,                                  &
               & loc_tot,                                &
               & loc_frac
          CLOSE(unit=out,iostat=ios)
       end IF
    end if
    ! misc diagnostics
    ! NOTE: also save diagnozed fluxes from 'normal' pCO2 flux and restoring forcing
    IF (ctrl_data_save_sig_diag .AND. ctrl_data_save_inversion) THEN
       DO idm2D=1,n_diag_misc_2D
          loc_filename = fun_data_timeseries_filename(loc_t, &
               & par_outdir_name,trim(par_outfile_name)//'_series_diag_misc','inversion_forcing_' &
               & //trim(string_diag_misc_2D(idm2D)),string_results_ext)
          if (idm2D == idiag_misc_2D_FpCO2_13C) then
             loc_tot  = int_diag_misc_2D_sig(idiag_misc_2D_FpCO2)
             loc_frac = int_diag_misc_2D_sig(idiag_misc_2D_FpCO2_13C)
             loc_standard = const_standards(atm_type(ia_pCO2_13C))
             loc_sig = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.TRUE.,const_nulliso)
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             WRITE(unit=out,fmt='(f12.3,e15.7,f12.3)',iostat=ios) &
                  & loc_t,                                  &
                  & loc_frac, &
                  & loc_sig
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
          elseif (idm2D == idiag_misc_2D_FDIC_13C) then
             loc_tot  = int_diag_misc_2D_sig(idiag_misc_2D_FDIC)
             loc_frac = int_diag_misc_2D_sig(idiag_misc_2D_FDIC_13C)
             loc_standard = const_standards(ocn_type(io_DIC_13C))
             loc_sig = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.TRUE.,const_nulliso)
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             WRITE(unit=out,fmt='(f12.3,e15.7,f12.3)',iostat=ios) &
                  & loc_t,                                  &
                  & loc_frac, &
                  & loc_sig
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
          elseif (idm2D == idiag_misc_2D_FCa_44Ca) then
             loc_tot  = int_diag_misc_2D_sig(idiag_misc_2D_FCa)
             loc_frac = int_diag_misc_2D_sig(idiag_misc_2D_FCa_44Ca)
             loc_standard = const_standards(ocn_type(io_Ca_44Ca))
             loc_sig = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.TRUE.,const_nulliso)
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             WRITE(unit=out,fmt='(f12.3,e15.7,f12.3)',iostat=ios) &
                  & loc_t,                                  &
                  & loc_frac, &
                  & loc_sig
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
          else
             loc_tot  = int_diag_misc_2D_sig(idm2D)
             call check_unit(out,__LINE__,__FILE__)
             OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
             WRITE(unit=out,fmt='(f12.3,e15.7)',iostat=ios) &
                  & loc_t,                                  &
                  & loc_tot
             call check_iostat(ios,__LINE__,__FILE__)
             CLOSE(unit=out,iostat=ios)
             call check_iostat(ios,__LINE__,__FILE__)
          endif
       end DO
    END IF
    ! forcing flux
    ! NOTE: the oringal calculation was of the the flux imposed from the ocean (but not subtracted from the ocean) but less any 
    !       transfer from the ocean to the atm via air-sea gas exchange, i.e.
    !       loc_sig = int_focnatm_sig(ia)/int_t_sig - int_diag_airsea_sig(ia)/int_t_sig
    !       loc_tot  = int_focnatm_sig(atm_dep(ia))/int_t_sig - int_diag_airsea_sig(atm_dep(ia))/int_t_sig
    !       loc_frac = int_focnatm_sig(ia)/int_t_sig - int_diag_airsea_sig(ia)/int_t_sig
    IF (ctrl_data_save_sig_diag) THEN
       DO l=3,n_l_atm
          ia = conv_iselected_ia(l)
          if (force_flux_atm_select(ia) .OR. force_restore_atm_select(ia)) then
             loc_filename=fun_data_timeseries_filename( &
                  & dum_t,par_outdir_name, &
                  & trim(par_outfile_name)//'_series_diag_misc','specified_forcing_'//TRIM(string_atm(ia)),string_results_ext &
                  & )
             SELECT CASE (atm_type(ia))
             CASE (0,1)
                loc_sig = int_diag_forcing_sig(ia)/int_t_sig
                call check_unit(out,__LINE__,__FILE__)
                OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
                call check_iostat(ios,__LINE__,__FILE__)
                WRITE(unit=out,fmt='(f12.3,e15.7,f12.3)',iostat=ios) &
                     & loc_t, &
                     & loc_sig
                call check_iostat(ios,__LINE__,__FILE__)
                CLOSE(unit=out,iostat=ios)
                call check_iostat(ios,__LINE__,__FILE__)
             case (n_itype_min:n_itype_max)
                loc_tot = int_diag_forcing_sig(atm_dep(ia))/int_t_sig
                loc_frac = int_diag_forcing_sig(ia)/int_t_sig
                loc_standard = const_standards(atm_type(ia))
                loc_sig = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.TRUE.,const_nulliso)
                call check_unit(out,__LINE__,__FILE__)
                OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
                call check_iostat(ios,__LINE__,__FILE__)
                WRITE(unit=out,fmt='(f12.3,e15.7,f12.3)',iostat=ios) &
                     & loc_t, &
                     & loc_frac, &
                     & loc_sig
                call check_iostat(ios,__LINE__,__FILE__)
                CLOSE(unit=out,iostat=ios)
                call check_iostat(ios,__LINE__,__FILE__)
             end SELECT
          end if
       END DO
    end IF
    ! age tracers
    IF (ctrl_data_save_sig_ocn .AND. ctrl_force_ocn_age) THEN
       loc_filename=fun_data_timeseries_filename( &
            & dum_t,par_outdir_name,trim(par_outfile_name)//'_series','misc_col_age',string_results_ext)
       loc_sig = int_misc_age_sig - dum_t
       IF (ctrl_data_save_sig_ocn_sur .OR. (par_data_save_level > 3)) THEN
          loc_sig_sur = int_misc_age_sur_sig - dum_t
          loc_sig_ben = int_misc_age_ben_sig - dum_t
          call check_unit(out,__LINE__,__FILE__)
          OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          WRITE(unit=out,fmt='(f12.3,3f12.3)',iostat=ios) &
               & loc_t,                                  &
               & loc_sig,                                &
               & loc_sig_sur,                            &
               & loc_sig_ben
          call check_iostat(ios,__LINE__,__FILE__)
          CLOSE(unit=out,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
       else
          call check_unit(out,__LINE__,__FILE__)
          OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
          WRITE(unit=out,fmt='(f12.3,f12.3)',iostat=ios) &
               & loc_t,                                 &
               & loc_sig
          call check_iostat(ios,__LINE__,__FILE__)
          CLOSE(unit=out,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
       end IF
    end if
    ! preformed tracers
    IF (ctrl_data_save_sig_diag .AND. ctrl_bio_preformed) THEN
          do io=io_col0,io_col9
             if (ocn_select(io)) then
                loc_save = .false.
                select case (io)
                CASE (io_col0)
                   if (ocn_select(io_DIC)) then
                      loc_save = .true.
                      loc_filename=fun_data_timeseries_filename( &
                           & loc_t,par_outdir_name,trim(par_outfile_name)//'_series_diag','preformed_DIC',string_results_ext &
                           & )
                   end if
                CASE (io_col1)
                   if (ocn_select(io_ALK)) then
                      loc_save = .true.
                      loc_filename=fun_data_timeseries_filename( &
                           & loc_t,par_outdir_name,trim(par_outfile_name)//'_series_diag','preformed_ALK',string_results_ext &
                           & )
                   end if
                CASE (io_col2)
                   if (ocn_select(io_O2)) then
                      loc_save = .true.
                      loc_filename=fun_data_timeseries_filename( &
                           & loc_t,par_outdir_name,trim(par_outfile_name)//'_series_diag','preformed_O2',string_results_ext &
                           & )
                   end if
                CASE (io_col3)
                   if (ocn_select(io_PO4)) then 
                      loc_save = .true.
                      loc_filename=fun_data_timeseries_filename( &
                           & loc_t,par_outdir_name,trim(par_outfile_name)//'_series_diag','preformed_PO4',string_results_ext &
                           & )
                   end if
                CASE (io_col4)
                   if (ocn_select(io_NO3)) then
                      loc_save = .true.
                      loc_filename=fun_data_timeseries_filename( &
                           & loc_t,par_outdir_name,trim(par_outfile_name)//'_series_diag','preformed_NO3',string_results_ext &
                           & )
                   end if
                CASE (io_col5)
                   if (ocn_select(io_Ca)) then
                      loc_save = .true.
                      loc_filename=fun_data_timeseries_filename( &
                           & loc_t,par_outdir_name,trim(par_outfile_name)//'_series_diag','preformed_Ca',string_results_ext &
                           & )
                   end if
                CASE (io_col6)
                   if (ocn_select(io_SiO2)) then
                      loc_save = .true.
                      loc_filename=fun_data_timeseries_filename( &
                           & loc_t,par_outdir_name,trim(par_outfile_name)//'_series_diag','preformed_SiO2',string_results_ext &
                           & )
                   end if
                CASE (io_col7)
                   if (ocn_select(io_DIC_13C)) then
                      loc_save = .true.
                      loc_filename=fun_data_timeseries_filename( &
                           & loc_t,par_outdir_name,trim(par_outfile_name)//'_series_diag','preformed_d13C',string_results_ext &
                           & )
                   end if
                end select
                !
                if (loc_save) then
                   select case (io)
                   CASE (io_col0,io_col1,io_col2,io_col3,io_col4,io_col5,io_col6)
                      loc_sig = int_ocn_sig(io)/int_t_sig
                      call check_unit(out,__LINE__,__FILE__)
                      OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
                      call check_iostat(ios,__LINE__,__FILE__)
                      WRITE(unit=out,fmt='(f12.3,2e15.7)',iostat=ios) &
                           & loc_t,                                   &
                           & loc_ocn_tot_M*loc_sig,                   &
                           & loc_sig
                      call check_iostat(ios,__LINE__,__FILE__)
                   CASE (io_col7)
                      loc_tot  = int_ocn_sig(io_col0)/int_t_sig
                      loc_frac = int_ocn_sig(io_col7)/int_t_sig
                      loc_standard = const_standards(ocn_type(io_DIC_13C))
                      loc_sig = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_nulliso)
                      call check_unit(out,__LINE__,__FILE__)
                      OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
                      call check_iostat(ios,__LINE__,__FILE__)
                      WRITE(unit=out,fmt='(f12.3,e15.7,f12.3)',iostat=ios) &
                           & loc_t, &
                           & loc_frac, &
                           & loc_sig
                      call check_iostat(ios,__LINE__,__FILE__)
                   case default
                      ! NOTHING DOING
                   end select
                   CLOSE(unit=out,iostat=ios)
                   call check_iostat(ios,__LINE__,__FILE__)
                end if
             end if
          END DO
    end if
    ! Save 3D data from a particular ij location
    IF (ctrl_data_save_ocn_3D_ij .AND. (ocn_select(io_DIC_13C))) THEN
       loc_filename=fun_data_timeseries_filename( &
            & loc_t,par_outdir_name,trim(par_outfile_name)//'_series','ocn_DIC_13C_ij',string_results_ext)
       DO k=1,n_k
          loc_tot_3D(k)=ocn(io_DIC,par_misc_save_i,par_misc_save_j,k)/int_t_sig
          loc_frac_3D(k)=ocn(io_DIC_13C,par_misc_save_i,par_misc_save_j,k)/int_t_sig
          loc_standard_3D(k)=const_standards(ocn_type(io_DIC_13C))
          loc_sig_3D(k)=fun_calc_isotope_delta(loc_tot_3D(k),loc_frac_3D(k),loc_standard_3D(k),.FALSE.,const_nulliso)
       END DO
       call check_unit(out,__LINE__,__FILE__)
       OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
       WRITE(unit=out,fmt='(f12.3,16f12.3)',iostat=ios) loc_t,(loc_sig_3D(k), k=1,n_k)
       call check_iostat(ios,__LINE__,__FILE__)
       CLOSE(unit=out,iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
    END IF
    ! And temperature
    IF (ctrl_data_save_ocn_3D_ij .AND. (ocn_select(io_T))) THEN
       loc_filename=fun_data_timeseries_filename( & 
            & loc_t,par_outdir_name,trim(par_outfile_name)//'_series','ocn_temp_ij',string_results_ext)
       DO k=1,n_k
          loc_sig_3D(k)=ocn(io_T,par_misc_save_i,par_misc_save_j,k)/int_t_sig - const_zeroC
       END DO
       call check_unit(out,__LINE__,__FILE__)
       OPEN(unit=out,file=loc_filename,action='write',status='old',position='append',iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
       WRITE(unit=out,fmt='(f12.3,16f12.3)',iostat=ios) loc_t,(loc_sig_3D(k), k=1,n_k)
       call check_iostat(ios,__LINE__,__FILE__)
       CLOSE(unit=out,iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
    END IF
  END SUBROUTINE sub_data_save_runtime
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! SAVE GLOBAL DATA
  SUBROUTINE sub_data_save_global_snap(dum_t,dum_sfcatm1)
    USE genie_util, ONLY:check_unit,check_iostat
    ! dummy arguments
    REAL,INTENT(IN)::dum_t
    REAL,DIMENSION(n_atm,n_i,n_j),INTENT(in)::dum_sfcatm1      ! atmosphere composition interface array
    ! local variables
    INTEGER::l,ia,io,is,ios
    real::loc_tot,loc_frac,loc_standard
    real::loc_atm_ave,loc_ocn_ave,loc_sed_ave
    real::loc_ocn_tot_M,loc_ocn_tot_A
    CHARACTER(len=255)::loc_filename

    ! *** set local parameters ***
    loc_filename= &
         & fun_data_timesnap_filename( &
         & dum_t,par_outdir_name,trim(par_outfile_name)//'_year','diag_GLOBAL_SNAP',string_results_ext)
    ! total ocean mass
    loc_ocn_tot_M = sum(phys_ocn(ipo_M,:,:,:))
    ! ocean surface area
    loc_ocn_tot_A = sum(phys_ocn(ipo_A,:,:,n_k))

    ! *** save data - OPEN FILE ***
    call check_unit(out,__LINE__,__FILE__)
    OPEN(unit=out,file=TRIM(loc_filename),action='write',iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)

    ! *** save data - ALL ***
    ! write atmospheric data
    DO l=3,n_l_atm
       ia = conv_iselected_ia(l)
       SELECT CASE (atm_type(ia))
       CASE (1)
          loc_atm_ave = &
               & SUM(phys_ocnatm(ipoa_A,:,:)*dum_sfcatm1(ia,:,:))/SUM(phys_ocnatm(ipoa_A,:,:))
          write(unit=out,fmt='(A13,A16,A3,f10.3,A15,A5,E15.7,A4)',iostat=ios) &
               & ' Atmospheric ',string_atm(ia),' : ', &
               & conv_mol_umol*loc_atm_ave, &
               & ' uatm          ', &
               & ' <-> ', &
               & conv_atm_mol*loc_atm_ave, &
               & ' mol'
          call check_iostat(ios,__LINE__,__FILE__)
       case (n_itype_min:n_itype_max)
          loc_tot = &
               & SUM(phys_ocnatm(ipoa_A,:,:)*dum_sfcatm1(atm_dep(ia),:,:))/SUM(phys_ocnatm(ipoa_A,:,:))
          loc_frac =  &
               & SUM(phys_ocnatm(ipoa_A,:,:)*dum_sfcatm1(ia,:,:))/SUM(phys_ocnatm(ipoa_A,:,:))
          loc_standard = const_standards(atm_type(ia))
          loc_atm_ave = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_nulliso)
          write(unit=out,fmt='(A13,A16,A3,f10.3,A5)',iostat=ios) &
               & ' Atmospheric ',string_atm(ia),' : ', &
               & loc_atm_ave, &
               & ' o/oo'
          call check_iostat(ios,__LINE__,__FILE__)
       end SELECT
    END DO
    ! write ocean data
    DO l=1,n_l_ocn
       io = conv_iselected_io(l)
       SELECT CASE (ocn_type(io))
       CASE (0)
          loc_ocn_ave = &
               & SUM(phys_ocn(ipo_M,:,:,:)*ocn(io,:,:,:))/loc_ocn_tot_M
          if (io == io_T) then
             write(unit=out,fmt='(A7,A16,A9,f10.3,A10)',iostat=ios) &
                  & ' Ocean ',string_ocn(io),'       : ',           &
                  & loc_ocn_ave - const_zeroC,                      &
                  & ' degrees C'
             call check_iostat(ios,__LINE__,__FILE__)
          else
             write(unit=out,fmt='(A7,A16,A9,f10.3,A4)',iostat=ios) &
                  & ' Ocean ',string_ocn(io),'       : ',          &
                  & loc_ocn_ave,                                   &
                  & ' PSU'
          end if
       CASE (1)
          loc_ocn_ave = &
               & SUM(phys_ocn(ipo_M,:,:,:)*ocn(io,:,:,:))/loc_ocn_tot_M
          write(unit=out,fmt='(A7,A16,A9,f10.3,A15,A5,E15.7,A4)',iostat=ios) &
               & ' Ocean ',string_ocn(io),'       : ', &
               & conv_mol_umol*loc_ocn_ave, &
               & ' umol kg-1     ', &
               & ' <-> ', &
               & loc_ocn_tot_M*loc_ocn_ave, &
               & ' mol'
          call check_iostat(ios,__LINE__,__FILE__)
       case (n_itype_min:n_itype_max)
          loc_tot = &
               & SUM(phys_ocn(ipo_M,:,:,:)*ocn(ocn_dep(io),:,:,:))/loc_ocn_tot_M
          loc_frac =  &
               & SUM(phys_ocn(ipo_M,:,:,:)*ocn(io,:,:,:))/loc_ocn_tot_M
          loc_standard = const_standards(ocn_type(io))
          loc_ocn_ave = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_nulliso)
          write(unit=out,fmt='(A7,A16,A9,f10.3,A10)',iostat=ios) &
               & ' Ocean ',string_ocn(io),'       : ', &
               & loc_ocn_ave, &
               & ' o/oo     '
          call check_iostat(ios,__LINE__,__FILE__)
       end SELECT
    END DO
    ! write export data
    DO l=1,n_l_sed
       is = conv_iselected_is(l)
       SELECT CASE (sed_type(is))
       CASE (1,2,4)
          loc_sed_ave = SUM(int_bio_settle_timeslice(is,:,:,n_k))/int_t_timeslice/loc_ocn_tot_A
          write(unit=out,fmt='(A13,A16,A3,f10.3,A15,A5,E15.7,A9)',iostat=ios) &
               & ' Export flux ',string_sed(is),' : ', &
               & conv_mol_umol*loc_sed_ave/conv_m2_cm2, &
               & ' umol cm-2 yr-1', &
               & ' <-> ', &
               & loc_ocn_tot_A*loc_sed_ave, &
               & ' mol yr-1'
          call check_iostat(ios,__LINE__,__FILE__)
       case (n_itype_min:n_itype_max)
          loc_tot  = SUM(int_bio_settle_timeslice(sed_dep(is),:,:,n_k))/int_t_timeslice/loc_ocn_tot_A
          loc_frac = SUM(int_bio_settle_timeslice(is,:,:,n_k))/int_t_timeslice/loc_ocn_tot_A
          loc_standard = const_standards(sed_type(is))
          loc_sed_ave = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_nulliso)
          write(unit=out,fmt='(A13,A16,A3,f10.3,A10)',iostat=ios) &
               & ' Export flux ',string_sed(is),' : ', &
               & loc_sed_ave, &
               & ' o/oo     '
          call check_iostat(ios,__LINE__,__FILE__)
       end SELECT
    END DO

    ! *** save data - CLOSE FILE ***
    call check_iostat(ios,__LINE__,__FILE__)
    CLOSE(unit=out)

  END SUBROUTINE sub_data_save_global_snap
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! SAVE GLOBAL DATA
  SUBROUTINE sub_data_save_global_av()
    USE genie_util, ONLY:check_unit,check_iostat
    ! local variables
    INTEGER::i,j,k,l,ia,io,is,ios,ic
    integer::loc_k1
    real::loc_t,loc_dt,loc_K
    real::loc_tot,loc_frac,loc_standard
    real::loc_atm_ave,loc_ocn_ave,loc_sed_ave
    real::loc_ocn_tot_M,loc_ocn_tot_A,loc_ocnatm_tot_A
    CHARACTER(len=255)::loc_filename
    REAL,DIMENSION(n_phys_ocn,n_i,n_j,n_k)::loc_phys_ocn       !
    REAL,DIMENSION(n_ocn,n_i,n_j,n_k)::loc_ocn                 !
    REAL,DIMENSION(n_carbconst,n_i,n_j,n_k)::loc_carbconst     !
    REAL,DIMENSION(n_carb,n_i,n_j,n_k)::loc_carb               !
    REAL,DIMENSION(n_carbalk,n_i,n_j,n_k)::loc_carbalk         ! 

    ! *** initialize local variables ***
    loc_phys_ocn(:,:,:,:)  = 0.0
    loc_ocn(:,:,:,:)       = 0.0
    loc_carbconst(:,:,:,:) = 0.0
    loc_carb(:,:,:,:)      = 0.0
    loc_carbalk(:,:,:,:)   = 0.0

    ! *** set local parameters ***
    loc_dt = int_t_timeslice
    loc_filename= &
         & fun_data_timeslice_filename( &
         & par_outdir_name,trim(par_outfile_name)//'_year','diag_GLOBAL_AVERAGE',string_results_ext)
    IF (ctrl_misc_t_BP) THEN
       loc_t = par_data_save_timeslice(par_data_save_timeslice_i) + par_misc_t_end
    ELSE
       loc_t = par_misc_t_end - par_data_save_timeslice(par_data_save_timeslice_i)
    END IF
    ! ocean physics
    If (ctrl_data_save_slice_phys_ocn) then
       loc_phys_ocn(:,:,:,:) = int_phys_ocn_timeslice(:,:,:,:)/int_t_timeslice
    else
       loc_phys_ocn(:,:,:,:) = phys_ocn(:,:,:,:)
    end If
    ! ocean tracers
    If (ctrl_data_save_slice_ocn) then
       loc_ocn(:,:,:,:) = int_ocn_timeslice(:,:,:,:)/int_t_timeslice
    else
       loc_ocn(:,:,:,:) = ocn(:,:,:,:)
    end if
    ! total ocean mass
    loc_ocn_tot_M = sum(phys_ocn(ipo_M,:,:,:))
    ! ocean surface area
    loc_ocn_tot_A = sum(phys_ocn(ipo_A,:,:,n_k))
    ! ocean surface area
    loc_ocnatm_tot_A = sum(phys_ocnatm(ipoa_A,:,:))

    ! *** solve carbonate system ***
    IF (opt_select(iopt_select_carbchem)) THEN
       DO i=1,n_i
          DO j=1,n_j
             loc_k1 = goldstein_k1(i,j)
             DO k=goldstein_k1(i,j),n_k
                ! calculate carbonate dissociation constants
                CALL sub_calc_carbconst(           &
                     & loc_phys_ocn(ipo_Dmid,i,j,k), &
                     & loc_ocn(io_T,i,j,k),          &
                     & loc_ocn(io_S,i,j,k),          &
                     & loc_carbconst(:,i,j,k)        &
                     & )
                ! adjust carbonate constants
                if (ocn_select(io_Ca) .AND. ocn_select(io_Mg)) then
                   call sub_adj_carbconst(   &
                        & loc_ocn(io_Ca,i,j,k),  &
                        & loc_ocn(io_Mg,i,j,k),  &
                        & loc_carbconst(:,i,j,k) &
                        & )
                end if
                ! re-estimate Ca and borate concentrations from salinity (if not selected and therefore explicitly treated)
                IF (.NOT. ocn_select(io_Ca))  loc_ocn(io_Ca,i,j,n_k)  = fun_calc_Ca(loc_ocn(io_S,i,j,n_k))
                IF (.NOT. ocn_select(io_B))   loc_ocn(io_B,i,j,n_k)   = fun_calc_Btot(loc_ocn(io_S,i,j,n_k))
                IF (.NOT. ocn_select(io_SO4)) loc_ocn(io_SO4,i,j,n_k) = fun_calc_SO4tot(loc_ocn(io_S,i,j,n_k))
                IF (.NOT. ocn_select(io_F))   loc_ocn(io_F,i,j,n_k)   = fun_calc_Ftot(loc_ocn(io_S,i,j,n_k))
                ! seed default initial ocean pH
                loc_carb(ic_H,i,j,k) = 10**(-7.8)
                ! calculate carbonate chemistry
                CALL sub_calc_carb(        &
                     & loc_ocn(io_DIC,i,j,k),  &
                     & loc_ocn(io_ALK,i,j,k),  &
                     & loc_ocn(io_Ca,i,j,k),   &
                     & loc_ocn(io_PO4,i,j,k),  &
                     & loc_ocn(io_SiO2,i,j,k), &
                     & loc_ocn(io_B,i,j,k),    &
                     & loc_ocn(io_SO4,i,j,k),  &
                     & loc_ocn(io_F,i,j,k),    &
                     & loc_ocn(io_H2S,i,j,k),  &
                     & loc_ocn(io_NH4,i,j,k),  &
                     & loc_carbconst(:,i,j,k), & 
                     & loc_carb(:,i,j,k),      & 
                     & loc_carbalk(:,i,j,k)    & 
                     & )
             end do
          end DO
       end DO
    end IF

    ! *** save data - OPEN FILE ***
    call check_unit(out,__LINE__,__FILE__)
    OPEN(unit=out,file=TRIM(loc_filename),action='write',iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)

    ! *** save data - OPEN FILE HEADER ***
    ! write header
    Write(unit=out,fmt=*) '=========================='
    Write(unit=out,fmt=*) 'GLOBAL DIAGNOSTICS'
    Write(unit=out,fmt=*) '=========================='
    Write(unit=out,fmt=*) ' '
    write(unit=out,fmt='(A23,f12.3)',iostat=ios) &
         & ' Year ............... : ',              &
         & loc_t
    call check_iostat(ios,__LINE__,__FILE__)
    write(unit=out,fmt='(A23,f12.3,A6)',iostat=ios) &
         & ' Integration interval : ',              &
         & int_t_timeslice,                        &
         & ' yr'
    call check_iostat(ios,__LINE__,__FILE__)

    ! *** save data - MISC / GLOBAL PHYSICAL PROPERTIES ***
    ! write misc data
    Write(unit=out,fmt=*) ' '
    Write(unit=out,fmt=*) '--------------------------'
    Write(unit=out,fmt=*) 'MISCELLANEOUS PROPERTIES'
    Write(unit=out,fmt=*) ' '
    Write(unit=out,fmt='(A49,E15.7,A3)',iostat=ios) &
         & ' Global surface area ............. : ', &
         & loc_ocnatm_tot_A, &
         & ' m2'
    call check_iostat(ios,__LINE__,__FILE__)
    Write(unit=out,fmt='(A49,E15.7,A3)',iostat=ios) &
         & ' Global ocean k = n_k (surface) area ............. : ', &
         & SUM(loc_phys_ocn(ipo_A,:,:,n_k)), &
         & ' m2'
    call check_iostat(ios,__LINE__,__FILE__)
    Write(unit=out,fmt='(A49,E15.7,A3)',iostat=ios) &
         & ' Global ocean k = (n_k - 1) (sub-surface layer) area : ', &
         & SUM(loc_phys_ocn(ipo_A,:,:,n_k - 1)), &
         & ' m2'
    call check_iostat(ios,__LINE__,__FILE__)
    Write(unit=out,fmt='(A49,E15.7,A3)',iostat=ios) &
         & ' Global ocean volume ......................... : ', &
         & SUM(loc_phys_ocn(ipo_V,:,:,:)), &
         & ' m3'
    call check_iostat(ios,__LINE__,__FILE__)
    loc_K = sum( &
         & phys_ocnatm(ipoa_A,:,:)*int_phys_ocnatm_timeslice(ipoa_KCO2,:,:)* &
         & (1.0 - int_phys_ocnatm_timeslice(ipoa_seaice,:,:)) &
         & ) &
         & / &
         & sum( &
         & phys_ocnatm(ipoa_A,:,:)*int_phys_ocn_timeslice(ipo_mask_ocn,:,:,n_k)* &
         & (1.0 - int_phys_ocnatm_timeslice(ipoa_seaice,:,:)) &
         & )
    Write(unit=out,fmt='(A49,f8.6,A24)',iostat=ios) &
         & ' Global mean air-sea coefficient, K(CO2) ..... : ', &
         & loc_K, &
         & '     mol m-2 yr-1 uatm-1'
    call check_iostat(ios,__LINE__,__FILE__)

    ! *** save data - ATMOSPHERIC TRACER PROPERTIES ***
    ! write atmospheric data
    Write(unit=out,fmt=*) ' '
    Write(unit=out,fmt=*) '--------------------------'
    Write(unit=out,fmt=*) 'ATMOSPHERIC PROPERTIES'
    Write(unit=out,fmt=*) ' '
    DO l=3,n_l_atm
       ia = conv_iselected_ia(l)
       SELECT CASE (atm_type(ia))
       CASE (1)
          loc_atm_ave = &
               & SUM(phys_ocnatm(ipoa_A,:,:)*int_sfcatm1_timeslice(ia,:,:)/int_t_timeslice)/SUM(phys_ocnatm(ipoa_A,:,:))
          write(unit=out,fmt='(A13,A16,A3,f10.3,A5)',iostat=ios) &
               & ' Atmospheric ',string_atm(ia),' : ', &
               & conv_mol_umol*loc_atm_ave, &
               & ' uatm'
          call check_iostat(ios,__LINE__,__FILE__)
       case (n_itype_min:n_itype_max)
          loc_tot = &
               & SUM(phys_ocnatm(ipoa_A,:,:)*int_sfcatm1_timeslice(atm_dep(ia),:,:)/int_t_timeslice)/SUM(phys_ocnatm(ipoa_A,:,:))
          loc_frac =  &
               & SUM(phys_ocnatm(ipoa_A,:,:)*int_sfcatm1_timeslice(ia,:,:)/int_t_timeslice)/SUM(phys_ocnatm(ipoa_A,:,:))
          loc_standard = const_standards(atm_type(ia))
          loc_atm_ave = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_nulliso)
          write(unit=out,fmt='(A13,A16,A3,f10.3,A5)',iostat=ios) &
               & ' Atmospheric ',string_atm(ia),' : ', &
               & loc_atm_ave, &
               & ' o/oo'
          call check_iostat(ios,__LINE__,__FILE__)
       end SELECT
    END DO

    ! *** save data - OCEAN TRACER PROPERTIES ***
    ! write ocean data
    Write(unit=out,fmt=*) ' '
    Write(unit=out,fmt=*) '--------------------------'
    Write(unit=out,fmt=*) 'BULK OCEAN PROPERTIES'
    Write(unit=out,fmt=*) ' '
    DO l=1,n_l_ocn
       io = conv_iselected_io(l)
       SELECT CASE (ocn_type(io))
       CASE (0)
          loc_ocn_ave = &
               & SUM(loc_phys_ocn(ipo_M,:,:,:)*loc_ocn(io,:,:,:))/loc_ocn_tot_M
          if (io == io_T) then
             write(unit=out,fmt='(A7,A16,A9,f10.3,A10)',iostat=ios) &
                  & ' Ocean ',string_ocn(io),' ..... : ',           &
                  & loc_ocn_ave - const_zeroC,                      &
                  & ' degrees C'
             call check_iostat(ios,__LINE__,__FILE__)
          else
             write(unit=out,fmt='(A7,A16,A9,f10.3,A4)',iostat=ios) &
                  & ' Ocean ',string_ocn(io),' ..... : ',          &
                  & loc_ocn_ave,                                   &
                  & ' PSU'
          end if
       CASE (1)
          loc_ocn_ave = &
               & SUM(loc_phys_ocn(ipo_M,:,:,:)*loc_ocn(io,:,:,:))/loc_ocn_tot_M
          write(unit=out,fmt='(A7,A16,A9,f10.3,A10,A5,E15.7,A4)',iostat=ios) &
               & ' Ocean ',string_ocn(io),' ..... : ', &
               & conv_mol_umol*loc_ocn_ave, &
               & ' umol kg-1', &
               & ' <-> ', &
               & loc_ocn_tot_M*loc_ocn_ave, &
               & ' mol'
          call check_iostat(ios,__LINE__,__FILE__)
       case (n_itype_min:n_itype_max)
          loc_tot = &
               & SUM(loc_phys_ocn(ipo_M,:,:,:)*loc_ocn(ocn_dep(io),:,:,:))/loc_ocn_tot_M
          loc_frac =  &
               & SUM(loc_phys_ocn(ipo_M,:,:,:)*loc_ocn(io,:,:,:))/loc_ocn_tot_M
          loc_standard = const_standards(ocn_type(io))
          loc_ocn_ave = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_nulliso)
          write(unit=out,fmt='(A7,A16,A9,f10.3,A10)',iostat=ios) &
               & ' Ocean ',string_ocn(io),' ..... : ', &
               & loc_ocn_ave, &
               & ' o/oo     '
          call check_iostat(ios,__LINE__,__FILE__)
       end SELECT
    END DO

    ! *** save data - CARBONATE CHEMSITRY ***
    ! write carbonate chemsitry data
    Write(unit=out,fmt=*) ' '
    Write(unit=out,fmt=*) '--------------------------'
    Write(unit=out,fmt=*) 'BULK OCEAN CARBONATE CHEMSITRY'
    Write(unit=out,fmt=*) ' '
    DO ic=1,n_carb
       SELECT CASE (ic)
       CASE (ic_conc_CO2,ic_conc_HCO3,ic_conc_CO3)
          loc_ocn_ave = &
               & SUM(loc_phys_ocn(ipo_M,:,:,:)*loc_carb(ic,:,:,:))/loc_ocn_tot_M
          write(unit=out,fmt='(A11,A16,A5,f10.3,A10,A5,E15.7,A4)',iostat=ios) &
               & ' Carb chem ',string_carb(ic),' . : ', &
               & conv_mol_umol*loc_ocn_ave, &
               & ' umol kg-1', &
               & ' <-> ', &
               & loc_ocn_tot_M*loc_ocn_ave, &
               & ' mol'
          call check_iostat(ios,__LINE__,__FILE__)
       end SELECT
    END DO

    ! *** save data - BIOLOGICAL EXPLORT ***
    ! write export data
    Write(unit=out,fmt=*) ' '
    Write(unit=out,fmt=*) '------------------------------'
    Write(unit=out,fmt=*) 'SURFACE EXPORT PRODUCTION'
    Write(unit=out,fmt=*) ' '
    DO l=1,n_l_sed
       is = conv_iselected_is(l)
       SELECT CASE (sed_type(is))
       CASE (1,2,4)
          loc_sed_ave = SUM(int_bio_settle_timeslice(is,:,:,n_k))/int_t_timeslice/loc_ocn_tot_A
          write(unit=out,fmt='(A13,A16,A3,f10.3,A15,A5,E15.7,A9)',iostat=ios) &
               & ' Export flux ',string_sed(is),' : ', &
               & conv_mol_umol*loc_sed_ave/conv_m2_cm2, &
               & ' umol cm-2 yr-1', &
               & ' <-> ', &
               & loc_ocn_tot_A*loc_sed_ave, &
               & ' mol yr-1'
          call check_iostat(ios,__LINE__,__FILE__)
       case (n_itype_min:n_itype_max)
          loc_tot  = SUM(int_bio_settle_timeslice(sed_dep(is),:,:,n_k))/int_t_timeslice/loc_ocn_tot_A
          loc_frac = SUM(int_bio_settle_timeslice(is,:,:,n_k))/int_t_timeslice/loc_ocn_tot_A
          loc_standard = const_standards(sed_type(is))
          loc_sed_ave = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_nulliso)
          write(unit=out,fmt='(A13,A16,A3,f10.3,A10)',iostat=ios) &
               & ' Export flux ',string_sed(is),' : ', &
               & loc_sed_ave, &
               & ' o/oo     '
          call check_iostat(ios,__LINE__,__FILE__)
       end SELECT
    END DO

    ! *** save data - SEDIMENTATION FLUX ***
    ! write sedimentation flux
    Write(unit=out,fmt=*) ' '
    Write(unit=out,fmt=*) '------------------------------'
    Write(unit=out,fmt=*) 'SEDIMENTATION'
    Write(unit=out,fmt=*) ' '
    DO l=1,n_l_sed
       is = conv_iselected_is(l)
       SELECT CASE (sed_type(is))
       CASE (1,2,4)
          loc_sed_ave = SUM(int_focnsed_timeslice(is,:,:))/int_t_timeslice/loc_ocn_tot_A
          write(unit=out,fmt='(A13,A16,A3,f10.3,A15,A5,E15.7,A9)',iostat=ios) &
               & ' Export flux ',string_sed(is),' : ', &
               & conv_mol_umol*loc_sed_ave/conv_m2_cm2, &
               & ' umol cm-2 yr-1', &
               & ' <-> ', &
               & loc_ocn_tot_A*loc_sed_ave, &
               & ' mol yr-1'
          call check_iostat(ios,__LINE__,__FILE__)
       case (n_itype_min:n_itype_max)
          loc_tot  = SUM(int_focnsed_timeslice(sed_dep(is),:,:))/int_t_timeslice/loc_ocn_tot_A
          loc_frac = SUM(int_focnsed_timeslice(is,:,:))/int_t_timeslice/loc_ocn_tot_A
          loc_standard = const_standards(sed_type(is))
          loc_sed_ave = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_nulliso)
          write(unit=out,fmt='(A13,A16,A3,f10.3,A10)',iostat=ios) &
               & ' Export flux ',string_sed(is),' : ', &
               & loc_sed_ave, &
               & ' o/oo     '
          call check_iostat(ios,__LINE__,__FILE__)
       end SELECT
    END DO
    Write(unit=out,fmt=*) ' '
    Write(unit=out,fmt=*) '------------------------------'
    Write(unit=out,fmt=*) 'SURFACE EXPORT & SEDIMENT DEPOSITION (RAIN) FLUX SUMMARY'
    Write(unit=out,fmt=*) ' '
    write(unit=out,fmt='(A22,e15.7,A12,f7.3,A9)',iostat=ios) &
         & ' Total POC export   : ', &
         & SUM(int_bio_settle_timeslice(is_POC,:,:,n_k))/int_t_timeslice, &
         & ' mol yr-1 = ', &
         & 1.0E-12*conv_C_mol_kg*SUM(int_bio_settle_timeslice(is_POC,:,:,n_k))/int_t_timeslice, &
         & ' PgC yr-1'
    call check_iostat(ios,__LINE__,__FILE__)
    write(unit=out,fmt='(A22,e15.7,A12,f7.3,A9)',iostat=ios) &
         & ' Total CaCO3 export : ', &
         & SUM(int_bio_settle_timeslice(is_CaCO3,:,:,n_k))/int_t_timeslice, &
         & ' mol yr-1 = ', &
         & 1.0E-12*conv_CaCO3_mol_kgC*SUM(int_bio_settle_timeslice(is_CaCO3,:,:,n_k))/int_t_timeslice, &
         & ' PgC yr-1'
    call check_iostat(ios,__LINE__,__FILE__)
    Write(unit=out,fmt=*) ' '
    write(unit=out,fmt='(A22,e15.7,A12,f7.3,A9)',iostat=ios) &
         & ' Total POC rain     : ', &
         & SUM(int_focnsed_timeslice(is_POC,:,:))/int_t_timeslice, &
         & ' mol yr-1 = ', &
         & 1.0E-12*conv_C_mol_kg*SUM(int_focnsed_timeslice(is_POC,:,:))/int_t_timeslice, &
         & ' PgC yr-1'
    call check_iostat(ios,__LINE__,__FILE__)
    write(unit=out,fmt='(A22,e15.7,A12,f7.3,A9)',iostat=ios) &
         & ' Total CaCO3 rain   : ', &
         & SUM(int_focnsed_timeslice(is_CaCO3,:,:))/int_t_timeslice, &
         & ' mol yr-1 = ', &
         & 1.0E-12*conv_CaCO3_mol_kgC*SUM(int_focnsed_timeslice(is_CaCO3,:,:))/int_t_timeslice, &
         & ' PgC yr-1'
    if (int_diag_bio_sig(idiag_bio_dPO4) < const_real_nullsmall) then
       int_diag_bio_sig(idiag_bio_dPO4) = const_real_nullsmall
    end if
    select case (par_bio_prodopt)
    CASE (                        &
         & 'bio_PFeSi',           &
         & 'bio_PFeSi_Ridgwell02' &
         & )
       Write(unit=out,fmt=*) ' '
       write(unit=out,fmt='(A22,e15.7,A12,f7.3,A13)',iostat=ios) &
            & ' Total opal export  : ', &
            & SUM(int_bio_settle_timeslice(is_opal,:,:,n_k))/int_t_timeslice, &
            & ' mol yr-1 = ', &
            & 1.0E-12*SUM(int_bio_settle_timeslice(is_opal,:,:,n_k))/int_t_timeslice, &
            & ' Tmol Si yr-1'
       write(unit=out,fmt='(A22,f7.3,A13)',iostat=ios) &
            & ' -> sp POC export   : ', &
            & (int_diag_bio_sig(idiag_bio_dPO4_1)/int_diag_bio_sig(idiag_bio_dPO4))* &
            & 1.0E-12*conv_C_mol_kg*SUM(int_bio_settle_timeslice(is_POC,:,:,n_k))/int_t_timeslice, &
            & ' PgC yr-1'
       write(unit=out,fmt='(A22,f7.3,A13)',iostat=ios) &
            & ' -> nsp POC export  : ', &
            & (int_diag_bio_sig(idiag_bio_dPO4_2)/int_diag_bio_sig(idiag_bio_dPO4))* &
            & 1.0E-12*conv_C_mol_kg*SUM(int_bio_settle_timeslice(is_POC,:,:,n_k))/int_t_timeslice, &
            & ' PgC yr-1'
    end select

    !
    Write(unit=out,fmt=*) ' '
    Write(unit=out,fmt=*) '=========================='
    ! *** save data - CLOSE FILE ***
    call check_iostat(ios,__LINE__,__FILE__)
    CLOSE(unit=out)

  END SUBROUTINE sub_data_save_global_av
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! RUN-TIME REPORTING
  SUBROUTINE sub_echo_runtime(dum_yr,dum_opsi_scale,dum_opsia_minmax,dum_sfcatm1,dum_gemlite)
    ! dummy arguments
    REAL,INTENT(in)::dum_yr
    REAL,INTENT(in)::dum_opsi_scale
    REAL,DIMENSION(2),INTENT(in)::dum_opsia_minmax
    REAL,DIMENSION(n_atm,n_i,n_j),INTENT(in)::dum_sfcatm1
    logical,intent(in)::dum_gemlite                                     ! in GEMlite phase of cycle?
    ! local variables
    real::loc_tot,loc_frac,loc_standard
    real::loc_ocn_tot_M,loc_ocn_tot_A,loc_ocnatm_tot_A
    real::loc_pCO2
    ! calculate local constants
    ! total ocean mass
    loc_ocn_tot_M = sum(phys_ocn(ipo_M,:,:,:))
    ! total ocean surface area (ice-free)
    loc_ocn_tot_A = sum((1.0 - phys_ocnatm(ipoa_seaice,:,:))*phys_ocn(ipo_A,:,:,n_k))
    ! total ocean-atmosphere interface area
    loc_ocnatm_tot_A = sum(phys_ocnatm(ipoa_A,:,:))
    ! calculate local isotopic variables
    loc_tot  = SUM(phys_ocnatm(ipoa_A,:,:)*dum_sfcatm1(ia_pCO2,:,:))/loc_ocnatm_tot_A
    loc_frac = SUM(phys_ocnatm(ipoa_A,:,:)*dum_sfcatm1(ia_pCO2_13C,:,:))/loc_ocnatm_tot_A
    loc_standard = const_standards(atm_type(ia_pCO2_13C))
    if (loc_frac < const_real_nullsmall) then
       loc_frac = fun_calc_isotope_fraction(0.0,loc_standard)*loc_tot
    end if

    ! *** echo run-time ***
    IF (opt_select(iopt_select_carbchem)) THEN
       select case (fname_topo)
       case ('worbe2', 'worjh2', 'worjh4', 'worlg2', 'worlg4', 'wv2jh2', 'wv3jh2', 'worri4')
          ! print header (if necessary)
          if (par_misc_t_echo_header) then
             print*,' '
             ! ### MAKE MODIFICATIONS TO SCREEN PRINTING INFORMATION HERE ####################################################### !
             PRINT'(A5,A11,A3,A11,A9,A3,A9,A8,A8,A8,A3,A11,A11)', &
                  & '    ',      &
                  & ' model year', &
                  & '  *',         &
                  & ' pCO2(uatm)', &
                  & '   d13CO2',   &
                  & '  *',         &
                  & '  AMO(Sv)',   &
                  & '  ice(%)',    &
                  & '   <SST>',    &
                  & '   <SSS>',    &
                  & '  *',         &
                  & '  <DIC>(uM)', &
                  & '  <ALK>(uM)'
             print*,' '
             par_misc_t_echo_header = .FALSE.
          end if
          ! calculate local variables
          loc_pCO2 = conv_mol_umol*SUM(phys_ocnatm(ipoa_A,:,:)*dum_sfcatm1(ia_pCO2,:,:))/loc_ocnatm_tot_A
          ! print values
          if (dum_gemlite) then
             PRINT'(A5,F11.2,3X,F11.3,F9.3,3X,F9.3,F8.3,F8.3,F8.3,3X,F11.3,F11.3)', &
                  & ' #G# ', &
                  & dum_yr, &
                  & loc_pCO2, &
                  & fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_nulliso), &
                  & dum_opsi_scale*dum_opsia_minmax(2), &
                  & 100.0*(1.0/SUM(phys_ocn(ipo_A,:,:,n_k)))*SUM(phys_ocn(ipo_A,:,:,n_k)*phys_ocnatm(ipoa_seaice,:,:)), &
                  & SUM((1.0 - phys_ocnatm(ipoa_seaice,:,:))*phys_ocn(ipo_A,:,:,n_k)*ocn(io_T,:,:,n_k))/loc_ocn_tot_A - &
                  & const_zeroC, &
                  & SUM((1.0 - phys_ocnatm(ipoa_seaice,:,:))*phys_ocn(ipo_A,:,:,n_k)*ocn(io_S,:,:,n_k))/loc_ocn_tot_A, &
                  & conv_mol_umol*SUM(phys_ocn(ipo_M,:,:,:)*ocn(io_DIC,:,:,:))/loc_ocn_tot_M, &
                  & conv_mol_umol*SUM(phys_ocn(ipo_M,:,:,:)*ocn(io_ALK,:,:,:))/loc_ocn_tot_M
          else
             PRINT'(A5,F11.2,3X,F11.3,F9.3,3X,F9.3,F8.3,F8.3,F8.3,3X,F11.3,F11.3)', &
                  & ' $N$ ', &
                  & dum_yr, &
                  & loc_pCO2, &
                  & fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_nulliso), &
                  & dum_opsi_scale*dum_opsia_minmax(2), &
                  & 100.0*(1.0/SUM(phys_ocn(ipo_A,:,:,n_k)))*SUM(phys_ocn(ipo_A,:,:,n_k)*phys_ocnatm(ipoa_seaice,:,:)), &
                  & SUM((1.0 - phys_ocnatm(ipoa_seaice,:,:))*phys_ocn(ipo_A,:,:,n_k)*ocn(io_T,:,:,n_k))/loc_ocn_tot_A - &
                  & const_zeroC, &
                  & SUM((1.0 - phys_ocnatm(ipoa_seaice,:,:))*phys_ocn(ipo_A,:,:,n_k)*ocn(io_S,:,:,n_k))/loc_ocn_tot_A, &
                  & conv_mol_umol*SUM(phys_ocn(ipo_M,:,:,:)*ocn(io_DIC,:,:,:))/loc_ocn_tot_M, &
                  & conv_mol_umol*SUM(phys_ocn(ipo_M,:,:,:)*ocn(io_ALK,:,:,:))/loc_ocn_tot_M
          endif
          ! ###################################################################################################################### !
       case default
          ! print header (if necessary)
          if (par_misc_t_echo_header) then
             print*,' '
             ! ### MAKE MODIFICATIONS TO SCREEN PRINTING INFORMATION HERE ####################################################### !
             PRINT'(A5,A11,A3,A11,A9,A3,A8,A8,A8,A3,A11,A11)', &
                  & '    ',      &
                  & ' model year', &
                  & '  *',         &
                  & ' pCO2(uatm)', &
                  & '   d13CO2',   &
                  & '  *',         &
                  & '  ice(%)',    &
                  & '   <SST>',    &
                  & '   <SSS>',    &
                  & '  *',         &
                  & '  <DIC>(uM)', &
                  & '  <ALK>(uM)'
             print*,' '
             par_misc_t_echo_header = .FALSE.
          end if
          ! calculate local variables
          loc_pCO2 = conv_mol_umol*SUM(phys_ocnatm(ipoa_A,:,:)*dum_sfcatm1(ia_pCO2,:,:))/loc_ocnatm_tot_A
          ! print values
          if (dum_gemlite) then
             PRINT'(A5,F11.2,3X,F11.3,F9.3,3X,F8.3,F8.3,F8.3,3X,F11.3,F11.3)', &
                  & ' #G# ', &
                  & dum_yr, &
                  & loc_pCO2, &
                  & fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_nulliso), &
                  & 100.0*(1.0/SUM(phys_ocn(ipo_A,:,:,n_k)))*SUM(phys_ocn(ipo_A,:,:,n_k)*phys_ocnatm(ipoa_seaice,:,:)), &
                  & SUM((1.0 - phys_ocnatm(ipoa_seaice,:,:))*phys_ocn(ipo_A,:,:,n_k)*ocn(io_T,:,:,n_k))/loc_ocn_tot_A - &
                  & const_zeroC, &
                  & SUM((1.0 - phys_ocnatm(ipoa_seaice,:,:))*phys_ocn(ipo_A,:,:,n_k)*ocn(io_S,:,:,n_k))/loc_ocn_tot_A, &
                  & conv_mol_umol*SUM(phys_ocn(ipo_M,:,:,:)*ocn(io_DIC,:,:,:))/loc_ocn_tot_M, &
                  & conv_mol_umol*SUM(phys_ocn(ipo_M,:,:,:)*ocn(io_ALK,:,:,:))/loc_ocn_tot_M
          else
             PRINT'(A5,F11.2,3X,F11.3,F9.3,3X,F8.3,F8.3,F8.3,3X,F11.3,F11.3)', &
                  & ' $N$ ', &
                  & dum_yr, &
                  & loc_pCO2, &
                  & fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_nulliso), &
                  & 100.0*(1.0/SUM(phys_ocn(ipo_A,:,:,n_k)))*SUM(phys_ocn(ipo_A,:,:,n_k)*phys_ocnatm(ipoa_seaice,:,:)), &
                  & SUM((1.0 - phys_ocnatm(ipoa_seaice,:,:))*phys_ocn(ipo_A,:,:,n_k)*ocn(io_T,:,:,n_k))/loc_ocn_tot_A - &
                  & const_zeroC, &
                  & SUM((1.0 - phys_ocnatm(ipoa_seaice,:,:))*phys_ocn(ipo_A,:,:,n_k)*ocn(io_S,:,:,n_k))/loc_ocn_tot_A, &
                  & conv_mol_umol*SUM(phys_ocn(ipo_M,:,:,:)*ocn(io_DIC,:,:,:))/loc_ocn_tot_M, &
                  & conv_mol_umol*SUM(phys_ocn(ipo_M,:,:,:)*ocn(io_ALK,:,:,:))/loc_ocn_tot_M
          endif
          ! ###################################################################################################################### !          
       end select
    else
       select case (fname_topo)
       case ('worbe2', 'worjh2', 'worjh4', 'worlg2', 'worlg4', 'wv2jh2', 'wv3jh2', 'worri4')
          if (par_misc_t_echo_header) then
             print*,' '
             ! ### MAKE MODIFICATIONS TO SCREEN PRINTING INFORMATION HERE ######################################################## !
             PRINT'(A5,A11,A3,A9,A8,A8,A8)', &
                  & '    ',      &
                  & ' model year', &
                  & '  *',         &
                  & '  AMO(Sv)',   &
                  & '  ice(%)',    &
                  & '   <SST>',    &
                  & '   <SSS>'
             print*,' '
             par_misc_t_echo_header = .FALSE.
          end if
          PRINT'(A5,F11.2,3X,F9.3,F8.3,F8.3,F8.3)', &
               & ' *** ', &
               & dum_yr, &
               & dum_opsi_scale*dum_opsia_minmax(2), &
               & 100.0*(1.0/SUM(phys_ocn(ipo_A,:,:,n_k)))*SUM(phys_ocn(ipo_A,:,:,n_k)*phys_ocnatm(ipoa_seaice,:,:)), &
               & SUM((1.0 - phys_ocnatm(ipoa_seaice,:,:))*phys_ocn(ipo_A,:,:,n_k)*ocn(io_T,:,:,n_k))/loc_ocn_tot_A - const_zeroC, &
               & SUM((1.0 - phys_ocnatm(ipoa_seaice,:,:))*phys_ocn(ipo_A,:,:,n_k)*ocn(io_S,:,:,n_k))/loc_ocn_tot_A
          ! ###################################################################################################################### !
       case default
          if (par_misc_t_echo_header) then
             print*,' '
             ! ### MAKE MODIFICATIONS TO SCREEN PRINTING INFORMATION HERE ######################################################## !
             PRINT'(A5,A11,A3,A8,A8,A8)', &
                  & '    ',      &
                  & ' model year', &
                  & '  *',         &
                  & '  ice(%)',    &
                  & '   <SST>',    &
                  & '   <SSS>'
             print*,' '
             par_misc_t_echo_header = .FALSE.
          end if
          PRINT'(A5,F11.2,3X,F8.3,F8.3,F8.3)', &
               & ' *** ', &
               & dum_yr, &
               & 100.0*(1.0/SUM(phys_ocn(ipo_A,:,:,n_k)))*SUM(phys_ocn(ipo_A,:,:,n_k)*phys_ocnatm(ipoa_seaice,:,:)), &
               & SUM((1.0 - phys_ocnatm(ipoa_seaice,:,:))*phys_ocn(ipo_A,:,:,n_k)*ocn(io_T,:,:,n_k))/loc_ocn_tot_A - const_zeroC, &
               & SUM((1.0 - phys_ocnatm(ipoa_seaice,:,:))*phys_ocn(ipo_A,:,:,n_k)*ocn(io_S,:,:,n_k))/loc_ocn_tot_A
          ! ###################################################################################################################### !
       end select
    end if

  END SUBROUTINE sub_echo_runtime
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! RUN-TIME max,min VALUE REPORTING
  SUBROUTINE sub_echo_maxmin()
    ! local variables
    integer::i,j,k
    integer::l,io
    integer::loc_i_min,loc_j_min,loc_k_min
    integer::loc_i_max,loc_j_max,loc_k_max
    real::loc_value_min
    real::loc_value_max
    real::loc_value,loc_tot,loc_frac,loc_standard

    ! *** determine max and min ocean tracer values + location ***
    IF (ctrl_audit) THEN
       DO l=1,n_l_ocn
          io = conv_iselected_io(l)
          loc_value_min = const_real_nullhigh
          loc_value_max = const_real_null
          DO i=1,n_i
             DO j=1,n_j
                do k=goldstein_k1(i,j),n_k
                   SELECT CASE (ocn_type(io))
                   CASE (0,1)
                      loc_value = ocn(io,i,j,k)
                   case (n_itype_min:n_itype_max)
                      loc_tot = ocn(ocn_dep(io),i,j,k)
                      loc_frac = ocn(io,i,j,k)
                      loc_standard = const_standards(ocn_type(io))
                      loc_value = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.,const_nulliso)
                   case default
                      loc_value = 0.0
                   END SELECT
                   if (loc_value < loc_value_min) then
                      loc_value_min = loc_value
                      loc_i_min = i
                      loc_j_min = j
                      loc_k_min = k
                   end if
                   if (loc_value > loc_value_max) then
                      loc_value_max = loc_value
                      loc_i_max = i
                      loc_j_max = j
                      loc_k_max = k
                   end if
                end do
             end do
          end DO
          PRINT'(A5,A16,A3,A6,E12.4,A2,I2,A1,I2,A1,I2,A4,A6,E12.4,A2,I2,A1,I2,A1,I2,A1)', &
               & '     ', &
               & string_ocn(io), &
               & ' / ', &
               & 'min = ', &
               & loc_value_min, &
               & ' (', &
               & loc_i_min, &
               & ',', &
               & loc_j_min, &
               & ',', &
               & loc_k_min, &
               & ') / ', &
               & 'max = ', &
               & loc_value_max, &
               & ' (', &
               & loc_i_max, &
               & ',', &
               & loc_j_max, &
               & ',', &
               & loc_k_max, &
               & ')'
       end do
    end if

  END SUBROUTINE sub_echo_maxmin
  ! ****************************************************************************************************************************** !


END MODULE biogem_data_ascii
