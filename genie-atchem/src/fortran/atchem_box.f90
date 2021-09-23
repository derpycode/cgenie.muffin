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

  ! ****************************************************************************************************************************** !
  ! EXCHANGE CARBON WITH A DYNAMIC (SLAB) TERRESTRIAL BIOSPHERE | (CTR|01-2021) 
  ! should be called once per one call of atmchem(?) YK 02.08.2021
  SUBROUTINE sub_calc_terrbio(dum_dtyr,dum_T,dum_fatm)
    IMPLICIT NONE
    ! dummy arguments
    real,intent(in)::dum_dtyr
    REAL,DIMENSION(n_atm,n_i,n_j),intent(inout)::dum_fatm              ! net flux to atmosphere (mol)
    REAL,DIMENSION(n_i,n_j),intent(in)::dum_T                          ! 
    ! local variables
    real::loc_avSLT
    real::loc_SLT(n_i,n_j)
    real::loc_SLT0
    real::loc_maxSLT
    real::loc_minSLT

    real::loc_CO2
    real::loc_CO22(n_i,n_j)
    real::loc_CO20
    real::loc_maxCO2
    real::loc_minCO2

    real::loc_Fnpp,loc_Fresp
    real::loc_Rnpp,loc_Rresp
    
    real::loc_Fnpp_int,loc_Fresp_int
    real::loc_litter_int,loc_vegi_int
    real::loc_SLT_int,loc_CO2_int
    
    real::loc_Fnpp0
    real::loc_Fnpp0_m2
    real::loc_B
    real::loc_CO2ref
    
    real::loc_decay
    real::loc_tau
    real::loc_resp_G
    real::resp_kT
    real::loc_Tref
    
    real::loc_litter2(n_i,n_j)
    real::loc_vegi2(n_i,n_j)
    real::loc_litter
    real::loc_vegi
    real::loc_litter_new
    real::loc_vegi_new
    real::loc_Ratm,loc_Rterr                                   ! local isotopic variables
    
    real::slab_save_dtyr
    real::slab_ss_dtyr
    
    real::landmask(n_i,n_j)                                    ! 
    
    integer::i,j
    
    ! logical::checkstuff = .true.
    logical::checkstuff = .false.                        ! 
    
    logical::landonly 
    
    landonly = .false.
    if (par_atm_slab_hetero) landonly = .true.
    
    if (checkstuff) print *, ' <<<< now in sub_calc_terrbio >>>> '
    
    
    if ( .not. atm_select(ia_pCO2) ) then 
       if (checkstuff) then 
          print*, 'pCO2 is traced: nothing to do in slab-biosphere'
          print*, 'Return to atchem '
       endif 
       return
    endif 
    
    ! in this initial attempt, landmask is not used; 
    ! one probably have to referred to *.k1 data as done in rokgem 
    ! (and additionally creating subroutines to read data as when initializing rokgem) 
    landmask = slab_landmask
    
    ! open(unit=utest,file=trim(adjustl(par_outdir_name))//'/tem/chk.res',action='write',status='unknown')
    ! do i = 1,n_i
       ! write(utest,*) landmask(i,:)
    ! enddo 
    ! close(utest) 
    ! stop
    
    ! resp and production paramter values 
    ! production
    loc_CO2ref = par_atm_slab_pCO2ref !365. ! ppm (Arora & Matthews 2008)
    loc_Fnpp0 = par_atm_slab_Fnpp0* 1e15 / 12. ! 72.0 * 1e15 / 12. ! Pg C yr-1 converted to mol yr-1
    loc_Fnpp0_m2 = loc_Fnpp0/ sum(phys_atm(ipa_A,:,:)*landmask(:,:)) ! divided by totla land surface area mol /m2 /yr 
    loc_B = par_atm_slab_B ! 0.71
    ! resp
    loc_decay = 1.0/par_atm_slab_tau ! 1./8.3 ! yr-1 Turnover of vegitation to litter (SOM)
    loc_resp_G = par_atm_slab_gamma !0.049 ! yr-1 Decay of SOM
    resp_kT = par_atm_slab_Q10 !2.1 ! Q10
    loc_Tref = 15.  ! degree C
    ! time step for saving 
    slab_save_dtyr = max(dum_dtyr,par_atm_slab_savedtyr)
    ! time when assuming steady state
    slab_ss_dtyr = par_atm_slab_ss_dtyr
    
    ! *** PUT TEMP INTO LOCAL ARRAY
    ! NOTE: extract temps to local array this way to please intel compilers
    loc_SLT(:,:) = dum_T(:,:)
    loc_CO22(:,:) = 1.0E+06*atm(ia_PCO2,:,:) ! *** CONVERT pCO2 FROM ATM TO PPM 
    loc_litter2(:,:) = atm_slabbiosphere(ia_pCO2,:,:)*(1.- slab_frac_vegi(:,:)) ! mol
    loc_vegi2(:,:) = atm_slabbiosphere(ia_pCO2,:,:)*slab_frac_vegi(:,:) ! mol
    

    if (.not. landonly) then  ! averaging over the world (but removing seaice area)
       loc_avSLT = sum(dum_T*landmask)/sum(landmask )
       loc_CO2 = sum(loc_CO22*landmask)/sum(landmask )   
       loc_litter = sum(loc_litter2)!/real(n_i*n_j)   
       loc_vegi = sum(loc_vegi2)!/real(n_i*n_j)   
    
       if ( atm_select(ia_pCO2_13C) ) then 
          loc_Ratm = sum(atm(ia_pCO2_13C,:,:))/sum(atm(ia_pCO2,:,:))
          loc_Rterr = sum(atm_slabbiosphere(ia_pCO2_13C,:,:))/sum(atm_slabbiosphere(ia_pCO2,:,:))
       endif 
    
    
       ! initially reaching steady state
       if ((.not.par_atm_slab_restart) .and. (slab_time_cnt <= slab_ss_dtyr) .and. (slab_time_cnt2 <=slab_ss_dtyr) ) then 
       ! *** CALCULATE TERRESTRIAL NPP BASED ON CO2 ***
          loc_Fnpp  = dum_dtyr*(loc_Fnpp0*(1. + loc_B*LOG(loc_CO2/loc_CO2ref)))
          loc_vegi_new = loc_Fnpp/(loc_decay*dum_dtyr)
       
          ! when using V vs lambda relationship
          if ( (par_atm_slab_dtaudvegi /= 0.0) .and.  (par_atm_slab_tau0 /= 0.0)) then
             loc_vegi_new = par_atm_slab_tau0/(1./(loc_Fnpp/dum_dtyr) - 12./1e15*par_atm_slab_dtaudvegi)
             loc_decay = (loc_vegi_new*12./1e15)*par_atm_slab_dtaudvegi + par_atm_slab_tau0 ! yr
             loc_decay = 1./loc_decay                     ! yr-1
          endif 
       
          loc_litter_new = loc_vegi_new *loc_decay*dum_dtyr &
               & /(dum_dtyr*loc_resp_G*resp_kT**((loc_avSLT - loc_Tref)*0.1))
          loc_Fresp = dum_dtyr*loc_litter_new*loc_resp_G*resp_kT**((loc_avSLT - loc_Tref)*0.1)
       else 
          ! *** CALCULATE TERRESTRIAL NPP BASED ON CO2 ***
          loc_Fnpp  = dum_dtyr*(loc_Fnpp0*(1 + loc_B*LOG(loc_CO2/loc_CO2ref)))
        
          ! *** CALCULATE SOIL RESPIRATION BASED ON GLOBAL AVERAGE LAND TEMPERATURE ***
          loc_Fresp = dum_dtyr*loc_litter*loc_resp_G*resp_kT**((loc_avSLT - loc_Tref)*0.1)
       
          ! when using V vs lambda relationship
          if ( (par_atm_slab_dtaudvegi /= 0.0) .and.  (par_atm_slab_tau0 /= 0.0)) then 
             loc_decay = (loc_vegi*12./1e15)*par_atm_slab_dtaudvegi + par_atm_slab_tau0 ! yr
             loc_decay = 1./loc_decay                     ! yr-1
          endif 

          loc_vegi_new = loc_vegi + (loc_Fnpp - loc_vegi*loc_decay*dum_dtyr)
          loc_litter_new = loc_litter + (loc_vegi*loc_decay*dum_dtyr - loc_Fresp)
       endif 
    
       slab_frac_vegi(:,:) = loc_vegi_new/(loc_vegi_new + loc_litter_new)
    
       if (any(slab_frac_vegi < 0.) .or. any(slab_frac_vegi > 1.)) then 
          print *, 'error in sub_calc_terrbio'
          stop
       endif 

       ! *** EXCHANGE CO2 ***
       ! NOTE: atm_slabbiosphere in units of mol
       ! bulk CO2
       dum_fatm(ia_pCO2,:,:) = dum_fatm(ia_pCO2,:,:) + loc_Fresp/real(n_i*n_j) - loc_Fnpp/real(n_i*n_j)
       ! distribute evenly
       atm_slabbiosphere(ia_pCO2,:,:) = &
          & atm_slabbiosphere(ia_pCO2,:,:) - loc_Fresp/real(n_i*n_j)  + loc_Fnpp/real(n_i*n_j) 
       ! d13C
       if ( atm_select(ia_pCO2_13C) ) then 
          dum_fatm(ia_pCO2_13C,:,:) = &
             & dum_fatm(ia_pCO2_13C,:,:) + loc_Rterr*loc_Fresp/real(n_i*n_j) - loc_Ratm*loc_Fnpp/real(n_i*n_j)
       ! distribute evenly
          atm_slabbiosphere(ia_pCO2_13C,:,:) = &
             & atm_slabbiosphere(ia_pCO2_13C,:,:) - loc_Rterr*loc_Fresp/real(n_i*n_j) + loc_Ratm*loc_Fnpp/real(n_i*n_j) 
       endif 
       
       
    else ! explicitly on land    
       slab_frac_vegi = 0.0
       loc_Fnpp_int   = 0.0
       loc_Fresp_int  = 0.0
       loc_litter_int = 0.0
       loc_vegi_int   = 0.0
       loc_CO2_int    = 0.0
       loc_SLT_int    = 0.0
       ! print *,runoff_land
       ! print *,size(ilandmask1_atm)
       ! print *,n_i,n_j
       do i=1,n_i
          do j=1,n_j 
             if (landmask(i,j) .eq. 0) cycle
                loc_avSLT = loc_SLT(i,j)
                loc_CO2 = loc_CO22(i,j)   
                loc_litter = loc_litter2(i,j)   
                loc_vegi = loc_vegi2(i,j)   
                loc_Fnpp0 = loc_Fnpp0_m2 * phys_atm(ipa_A,i,j) ! mol yr-1

                if ( atm_select(ia_pCO2_13C) ) then 
                   loc_Ratm = atm(ia_pCO2_13C,i,j)/atm(ia_pCO2,i,j)
                   loc_Rterr = atm_slabbiosphere(ia_pCO2_13C,i,j)/atm_slabbiosphere(ia_pCO2,i,j)
                endif     
               
                ! *** CALCULATE TERRESTRIAL NPP BASED ON CO2 ***
                loc_Fnpp  = dum_dtyr*(loc_Fnpp0*(1 + loc_B*LOG(loc_CO2/loc_CO2ref)))
 
                ! *** CALCULATE SOIL RESPIRATION BASED ON GLOBAL AVERAGE LAND TEMPERATURE ***
                loc_Fresp = dum_dtyr*loc_litter*loc_resp_G*resp_kT**((loc_avSLT - loc_Tref)*0.1)
 
                ! when using V vs lambda relationship
                if ( (par_atm_slab_dtaudvegi /= 0.0) .and.  (par_atm_slab_tau0 /= 0.0)) then 
                   loc_tau = (sum(loc_vegi2)*12./1e15)*par_atm_slab_dtaudvegi + par_atm_slab_tau0 ! yr
                   loc_decay = 1./loc_tau                     ! yr-1
                endif 
 
                loc_vegi_new = loc_vegi + (loc_Fnpp - loc_vegi*loc_decay*dum_dtyr)
                loc_litter_new = loc_litter + (loc_vegi*loc_decay*dum_dtyr - loc_Fresp)
             
                slab_frac_vegi(i,j) = loc_vegi_new/(loc_vegi_new + loc_litter_new)
             
                if (slab_frac_vegi(i,j) < 0. .or. slab_frac_vegi(i,j) > 1.) then 
                   print *, 'error in sub_calc_terrbio',i,j
                   stop
                endif 
                 
                loc_Fnpp_int   = loc_Fnpp_int   + loc_Fnpp
                loc_Fresp_int  = loc_Fresp_int  + loc_Fresp
                loc_litter_int = loc_litter_int + loc_litter_new
                loc_vegi_int   = loc_vegi_int   + loc_vegi_new
                
                loc_CO2_int    = loc_CO2_int    + loc_CO2
                loc_SLT_int    = loc_SLT_int    + loc_avSLT
 
                ! *** EXCHANGE CO2 ***
                ! NOTE: atm_slabbiosphere in units of mol
                ! bulk CO2
                dum_fatm(ia_pCO2,i,j) = dum_fatm(ia_pCO2,i,j) + loc_Fresp - loc_Fnpp
                ! distribute unevenly
                atm_slabbiosphere(ia_pCO2,i,j) = &
                   & atm_slabbiosphere(ia_pCO2,i,j) - loc_Fresp  + loc_Fnpp 
                ! d13C
                if ( atm_select(ia_pCO2_13C) ) then 
                   dum_fatm(ia_pCO2_13C,i,j) = &
                      & dum_fatm(ia_pCO2_13C,i,j) + loc_Rterr*loc_Fresp - loc_Ratm*loc_Fnpp
                ! distribute evenly
                   atm_slabbiosphere(ia_pCO2_13C,i,j) = &
                      & atm_slabbiosphere(ia_pCO2_13C,i,j) - loc_Rterr*loc_Fresp + loc_Ratm*loc_Fnpp 
                endif 
           
          enddo 
       enddo
       ! because the output uses left-hand side variables ...
       loc_Fnpp       =   loc_Fnpp_int   
       loc_Fresp      =   loc_Fresp_int  
       loc_litter_new =   loc_litter_int 
       loc_vegi_new   =   loc_vegi_int   
       
       loc_CO2        =   loc_CO2_int/sum(landmask)
       loc_avSLT      =   loc_SLT_int/sum(landmask)
       
    endif 
    
    ! integrating avSLT 
    slab_int_avSLT = slab_int_avSLT + loc_avSLT*dum_dtyr
    slab_int_t = slab_int_t + dum_dtyr
    slab_int_soilC = slab_int_soilC + loc_litter_new*dum_dtyr
    slab_int_vegiC = slab_int_vegiC + loc_vegi_new*dum_dtyr
    slab_int_resp = slab_int_resp + loc_Fresp
    slab_int_prod = slab_int_prod + loc_Fnpp

    if (par_atm_slabsave) then 
       slab_time_cnt = slab_time_cnt + dum_dtyr
       if (slab_time_cnt > slab_save_dtyr) then 
           if (checkstuff) print *, ' --- slab saving --- '
           slab_time_cnt2 = slab_time_cnt2 + slab_time_cnt
           ! print *,'!!! saving vertual box biosphere in atmchem !!!',slab_time_cnt2
           open(unit=utest,file=trim(adjustl(par_outdir_name))//'/tem/terFLX.res',action='write',status='old',position='append')
           write(utest,*) slab_time_cnt2, slab_int_avSLT/slab_int_t,loc_CO2, & 
              ! & loc_Fnpp/dum_dtyr, loc_Fresp/dum_dtyr, (loc_Fresp - loc_Fnpp)/dum_dtyr
              & slab_int_prod/slab_int_t, slab_int_resp/slab_int_t, (slab_int_resp - slab_int_prod)/slab_int_t
           close(utest)
           open(unit=utest,file=trim(adjustl(par_outdir_name))//'/tem/terFLXg.res',action='write',status='old',position='append')
           write(utest,*) slab_time_cnt2, slab_int_avSLT/slab_int_t, loc_CO2, &
              ! & loc_Fnpp/dum_dtyr*12./1e15, loc_Fresp/dum_dtyr*12./1e15, (loc_Fresp - loc_Fnpp)/dum_dtyr*12./1e15
              & slab_int_prod/slab_int_t*12./1e15, slab_int_resp/slab_int_t*12./1e15, (slab_int_resp - slab_int_prod)/slab_int_t*12./1e15
           close(utest)
           open(unit=utest,file=trim(adjustl(par_outdir_name))//'/tem/terPOOl.res',action='write',status='old',position='append')
           write(utest,*) slab_time_cnt2, slab_int_avSLT/slab_int_t, loc_CO2, &
              & slab_int_vegiC/slab_int_t, slab_int_soilC/slab_int_t, (slab_int_soilC + slab_int_vegiC)/slab_int_t
           close(utest)
           open(unit=utest,file=trim(adjustl(par_outdir_name))//'/tem/terPOOlg.res',action='write',status='old',position='append')
           write(utest,*) slab_time_cnt2, slab_int_avSLT/slab_int_t, loc_CO2, &
              & slab_int_vegiC/slab_int_t*12./1e15, slab_int_soilC/slab_int_t*12./1e15,(slab_int_soilC + slab_int_vegiC)*12./1e15/slab_int_t
           close(utest)
           ! slab_time_cnt = slab_time_cnt - slab_save_dtyr
           slab_time_cnt = 0. 
           slab_int_avSLT = 0.
           slab_int_t = 0.
           slab_int_soilC = 0.
           slab_int_vegiC = 0.
           slab_int_resp = 0.
           slab_int_prod = 0.
           if (checkstuff) print *, ' --- slab saved --- '
       endif 
    endif 

    ! *** CALCULATE CURRENT MEAN SURFACE LAND (AIR) TEMPERATURE (degrees C)
    ! NOTE: assumes equal-area grid
       ! loc_avSLT = 0.0
       ! loc_maxSLT = -100.0
       ! loc_minSLT =  100.0
       ! DO i=1,n_i
          ! DO j=1,n_j
             ! m = landmask(i,j) * loc_SLT(i,j)
             ! loc_avSLT = loc_avSLT + m
             ! IF ((m.GT.loc_maxSLT).AND.(landmask(i,j).EQ.1)) THEN
                ! loc_maxSLT = m
             ! ENDIF
             ! IF ((m.LT.loc_minSLT).AND.(landmas(i,j).EQ.1)) THEN
                ! loc_minSLT = m
             ! ENDIF
          ! END DO
       ! END DO
       ! loc_avSLT = loc_avSLT/nlandcells

    ! *** CALCULATE MEAN CO2 (ppm)
    ! NOTE: assumes equal-area grid
    ! loc_CO2 = 0.0
    ! loc_maxCO2 = 0.0
    ! loc_minCO2 = 0.0
    ! DO i=1,n_i
       ! DO j=1,n_j
          ! m = landmask(i,j) * loc_CO22(i,j)
          ! loc_CO2 = loc_CO2 + m
          ! IF ((m.GT.loc_maxCO2).AND.(landmask(i,j).EQ.1)) THEN
             ! loc_maxCO2 = m
          ! ENDIF
          ! IF ((m.LT.loc_minCO2).AND.(landmask(i,j).EQ.1)) THEN
             ! loc_minCO2 = m
          ! ENDIF
       ! END DO
    ! END DO
    ! loc_CO2 = loc_CO2/nlandcells        
    

! #    ! *** INITIALIZE LOCAL VARIABLES ***
! #    loc_Fatm  = dum_dtyr*par_atm_FterrCO2exchange/real(n_i*n_j)
! #    loc_Fterr = dum_dtyr*par_atm_FterrCO2exchange/real(n_i*n_j)
! #    loc_Ratm = atm(ia_pCO2_13C,dum_i,dum_j)/atm(ia_pCO2,dum_i,dum_j)
! #    loc_Rterr = atm_slabbiosphere(ia_pCO2_13C,dum_i,dum_j)/atm_slabbiosphere(ia_pCO2,dum_i,dum_j)
! #
! #    ! *** EXCHANGE CO2 ***
! #    ! NOTE: atm_slabbiosphere in units of mol
! #    ! bulk CO2
! #    dum_fatm(ia_pCO2) = dum_fatm(ia_pCO2) + loc_Fatm - loc_Fterr
! #    atm_slabbiosphere(ia_pCO2,dum_i,dum_j) = &
! #         & atm_slabbiosphere(ia_pCO2,dum_i,dum_j) - loc_Fatm + loc_Fterr
! #    ! d13C
! #    dum_fatm(ia_pCO2_13C) = dum_fatm(ia_pCO2_13C) + loc_Rterr*loc_Fatm - loc_Ratm*loc_Fterr
! #    atm_slabbiosphere(ia_pCO2_13C,dum_i,dum_j) = &
! #         & atm_slabbiosphere(ia_pCO2_13C,dum_i,dum_j) - loc_Rterr*loc_Fatm + loc_Ratm*loc_Fterr

  END SUBROUTINE sub_calc_terrbio
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
