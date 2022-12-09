
module sedgem_box_hydrate

use genie_control
use sedgem_lib
use gem_cmn
use sedgem_box_benthic

implicit none

real,save::mhinv_map(n_i,n_j)
real,save::mbinv_map(n_i,n_j)
real,save::so4flx_map(n_i,n_j)
real,save::ch4flx_map(n_i,n_j)
real,save::dicflx_map(n_i,n_j)
real,save::alkflx_map(n_i,n_j)
real,save::zs_map(n_i,n_j)
real,save::hsz_map(n_i,n_j)
real,save::ceqh_map(n_i,n_j)
real,save::ceqb_map(n_i,n_j)
real,save::heatflow_map(n_i,n_j)
real,save::depth_map(n_i,n_j)
real,save::sedthick_map(n_i,n_j)
real,save::om_map(n_i,n_j)
real,save::omprs_map(n_i,n_j)
real,save::ombur_map(n_i,n_j)
real,save::omfrc_map(n_i,n_j)
real,save::sedv_map(n_i,n_j)
real,save::btso4_map(n_i,n_j)
real,save::temp_map(n_i,n_j)
real,save::sal_map(n_i,n_j)
real,save::margin_map(n_i,n_j)
real,save::mhinv_map_OLD(n_i,n_j)
real,save::mbinv_map_OLD(n_i,n_j)
real,save::so4flx_map_INIT(n_i,n_j)
real,save::ch4flx_map_INIT(n_i,n_j)
real,save::h_diss_map(n_i,n_j)
real,save::b_diss_map(n_i,n_j)
real,save::ch4gen_map(n_i,n_j)
real,save::ch4adv_map(n_i,n_j)
real,save::so4red_map(n_i,n_j)
real,save::aom_map(n_i,n_j)
real,save::degas_map(n_i,n_j)
! real,save::cflx_source(n_i,n_j)

integer,save::irec_hydrate

contains

!**************************************************************************************************************************************
subroutine hydrate_update(          &
    dum_i,dum_j,dtyr_in,            &
    depth_in,btmocn_in,             &
    new_sed_in,dis_sed_in           &
    )
    
integer,intent(in)::dum_i,dum_j
real,intent(in)::depth_in,btmocn_in(n_ocn),dtyr_in,new_sed_in(n_sed),dis_sed_in(n_sed)

real loc_D_hydrate,loc_T_hydrate,loc_DO_hydrate,loc_v_sed,dum_dtyr,loc_org_0,loc_geotherm,loc_margin,loc_v_sedv
real loc_sed_pres_fracC,loc_sed_pres_fracP,loc_exe_ocn(n_ocn),loc_sed_mean_OM_top,loc_sed_mean_OM_bot,sedthick
real loc_thermcond

loc_D_hydrate = depth_in
loc_T_hydrate = btmocn_in(io_T)
loc_DO_hydrate = btmocn_in(io_O2)*1e6 ! converting from mol/kg to umol/kg
dum_dtyr = dtyr_in
loc_v_sed = (fun_calc_sed_mass(new_sed_in(:)) - fun_calc_sed_mass(dis_sed_in(:)))/dum_dtyr  ! g/cm2/yr
loc_v_sedv = (fun_calc_sed_vol(new_sed_in(:)) - fun_calc_sed_vol(dis_sed_in(:)))/dum_dtyr  ! cm3/cm2/yr

selectcase(trim(par_sed_hydrate_opt_sedthermcond))
    case('larowe17') ! depth dependent conductivity (but ignoring different parameterization for Antarctica for now)
        if ( loc_D_hydrate < 200. ) then 
            loc_thermcond = 1.5 ! W/m/oC
        elseif ( (200. <= loc_D_hydrate) .and. (loc_D_hydrate < 3500.) ) then 
            loc_thermcond = 0.87 ! W/m/oC
        else
            loc_thermcond = 0.82 ! W/m/oC
        endif 
    case('cnst')
        loc_thermcond = par_sed_hydrate_thermcond ! W/m/oC
endselect 

if (par_sed_hydrate_hunter2013) then ! use depth, temperature, DO from global maps in Hunter et al. (2013)
    call get_bwt_Hunter13(dum_i,dum_j,loc_T_hydrate)
    loc_T_hydrate = loc_T_hydrate + const_zeroC ! converting temperature from oC to K 
    call get_topo_Hunter13(dum_i,dum_j,loc_D_hydrate)
    call get_bDO_Hunter13(dum_i,dum_j,loc_DO_hydrate)
endif 

select case(trim(par_sed_hydrate_opt_org))
    case('muds') ! use muds to get om conc. (wt%) and sediment flux (g/cm2/yr) from Muds results
        call interp_muds(loc_DO_hydrate,loc_D_hydrate,loc_v_sed,loc_org_0)
    case('omen') ! use omen to get om conc. (wt%) 
        if (trim(par_sed_diagen_Corgopt)=='huelse2016') then 
            loc_org_0 = om_map(dum_i,dum_j) ! om_map records the results of OMEN in sedgem_box
            ! calculating OM conc. from OM flux and burial rate 
            loc_v_sed = sedv_map(dum_i,dum_j)  ! g cm-2 yr-1
            loc_org_0 = omfrc_map(dum_i,dum_j)*ombur_map(dum_i,dum_j) &! om_map records the results of OMEN in sedgem_box
                /( &
                (2650.d0/30d-3)*1d-2*(1d0-0.69d0)*  &! mol m-2 yr-1
                loc_v_sed/(2.65d0*(1d0-0.67d0))*1d3*1d-2/1d3 &! m/yr 
                )  
            om_map(dum_i,dum_j) = loc_org_0
            ! print*,'omen',loc_org_0,loc_v_sed,fun_calc_sed_mass(new_sed_in(:)),fun_calc_sed_mass(dis_sed_in(:)),dum_dtyr  
            ! print*,'omen',loc_org_0,loc_v_sed,(fun_calc_sed_mass(new_sed_in(:))-fun_calc_sed_mass(dis_sed_in(:)))/dum_dtyr  
            if (irec_hydrate==0 .and. loc_org_0 == 0.) then 
                return
            endif 
            ! if (dum_i==2 .and. dum_j==22)then 
                ! print *, 'preservation  :', (1.-0.25*margin_map(dum_i,dum_j))*omfrc_map(dum_i,dum_j)*ombur_map(dum_i,dum_j)
                ! print *, 'decomposition :', 0.25*margin_map(dum_i,dum_j)*omfrc_map(dum_i,dum_j)*ombur_map(dum_i,dum_j)
                ! print *, 'total rain    :', ombur_map(dum_i,dum_j)
                ! print *, 'margin factor :',margin_map(dum_i,dum_j)
                ! print *, 'pause at hydrate_update'
                ! pause
            ! endif
        else 
            ! case where OMEN is used just to calculate OM conc. 
            ! this option may not be good 
            ! loc_v_sed = 0.18  ! g/cm2/yr assumed constant sediment flux
            call sub_huelseetal2016_main(                                                                    &
                & dum_i,dum_j,dum_dtyr,loc_D_hydrate,loc_v_sedv*dum_dtyr,                                        &
                & new_sed_in(:),sed_fsed(is_POC_frac2,dum_i,dum_j),btmocn_in,                                   &
                & loc_sed_pres_fracC,loc_sed_pres_fracP,loc_exe_ocn(:),loc_sed_mean_OM_top,loc_sed_mean_OM_bot  &
                & )
            loc_org_0 = loc_sed_mean_OM_bot
            loc_org_0 = loc_sed_mean_OM_top
            omfrc_map(dum_i,dum_j) = loc_sed_pres_fracC
            ombur_map(dum_i,dum_j) = sed_fsed(is_POC,dum_i,dum_j)/dum_dtyr/conv_cm2_m2  ! converting mol/cm2 to mol/m2/yr
        endif 
endselect 

! one can also use sediment flux from genie
! sed_diag(idiag_OMEN_bur,dum_i,dum_j)/dum_dtyr 

select case(trim(par_sed_hydrate_opt_geotherm))
    case('cnst')
        loc_geotherm = par_sed_hydrate_geotherm
    case('hamza08')
        call get_heatflow_data_Hamza08(dum_i,dum_j,loc_geotherm)  ! get heatflux from Hamza et al. (2008)
        loc_geotherm = loc_geotherm/loc_thermcond/1000.
endselect

select case(trim(par_sed_hydrate_opt_margin)) ! calculation is conducted only on margins 
    case('depth')  ! margin is assumed to be areas whose depths are shallower than some threshold value 
        if (loc_D_hydrate > par_sed_hydrate_threshold) then 
            loc_margin = 0.0
        else 
            loc_margin = 1.0
        endif
        ! if (dum_i==1 .and. dum_j==11) then    
        ! if (dum_i==2 .and. dum_j==22) then    
            ! loc_margin = 1.0
            ! print*,loc_D_hydrate,loc_T_hydrate,loc_DO_hydrate,loc_v_sed,loc_org_0
        ! else 
            ! loc_margin = 0.0
        ! endif 
    case('archer09')  ! margin is obtained from Archer et al. (2009)
        call get_margin_mask_Archer09(dum_i,dum_j,loc_margin)
endselect 

call get_sed_thick_Laske97(dum_i,dum_j,sedthick)

if (loc_v_sed < const_real_nullsmall .or. loc_org_0 < const_real_nullsmall) then 
    loc_margin = 0.0
endif 

call hydrate_main(                                                          &
    dum_i,dum_j,dum_dtyr,                                                   &
    loc_geotherm,loc_D_hydrate,loc_T_hydrate,btmocn_in(io_S),loc_v_sed,     &
    btmocn_in(io_SO4)*1e3,btmocn_in(io_CH4),loc_org_0,                      &
    sedthick,loc_margin                                                     &    
    )

endsubroutine hydrate_update
!**************************************************************************************************************************************

!**************************************************************************************************************************************
subroutine hydrate_main(                            &
    dum_i,dum_j,dum_dt,                             &
    geotherm_in,wdpth_in,wtemp_in,sal_in,v_sed_in,  &
    so4s_in,cm_0_in,org_0_in,                       &
    sedthick_in,margin_in                           &
    )
implicit none

!  assume both bubbles and hydrates are separatable from fluid (porewater) 
!  accordingly, porewater velocity consider effects of both phases
!  This treatment may be different from those by Buffet model and Hunter's as well
!  Klauda (e.g., Klauda and Sandler, 2005) does not take into account bubbles at all. 
!  from hydrate_v0_5_5.f90 to conduct a large number of simulations (results are stored in directory 'output-"runid"') 
!  improve the solution of HSZ
!  options to choose methods to calculate HSZ 
!  try to produce bubbles
!  adding sulfate reduction zone calculation to hydrate_v0_5_5_gb_v5.f90 
!  further adding the depth dependence of the degradation rate to v6
!  adding constraint from sediment thickness
!  allowing bubble only calculation (allowing a negative 3 phase equilibrium point but (yet) only with thermo. data by Tishchenko and Duan and Mao and others)
!  new version (9/30/2019) trying to clean things up and facilitate calculation ?
!  changing the main program to subroutine to be incorporated in genie (for this macros are removed)

integer,intent(in)::dum_i,dum_j
real,intent(in)::geotherm_in,wdpth_in,wtemp_in,sal_in,v_sed_in,so4s_in,cm_0_in,org_0_in,dum_dt,margin_in,sedthick_in

integer iz, nz, iz3,it, izso4
parameter (nz = 30)
real ceq(nz), z(nz), zmax, z3, sal 
real geotherm, wdpth, wtemp
real rho_f, rho_h, rho_s, rho_b, grav
real xis, p0, p3, temp3, f2
real df2dtemp, err3, tor3, dp3, dtemp3
parameter (tor3 = 1d-4) 

real poro(nz), poro_0, poro_scale, poro_ext
real v(nz), u(nz), v_sed, u_ext, u_0, v_ext

real org(nz),dec_s(nz),dec_m(nz),org_0,zs,dec_m0,dec_s0,sedtemp(nz),orgx(nz)
real dif_m_0, dif_s_0, dif_c_0,dif_m(nz), dif_s(nz), dif_c(nz)
real phi_m(nz), phi_s(nz), cm(nz), h(nz), b(nz), ch, cb, cm_ext, cm_0,cm_ext_in
real cs(nz), cc(nz), cmx(nz), hbx(nz), ux(nz), chb, rho_hb, hb(nz)
real cc_0, ccx(nz), cc_ext, cs_0, csx(nz), cs_ext, sdif_bot, sdif_top
real phi_hb(nz), hx(nz), bx(nz), phi_h(nz), phi_b(nz), prodh, prodb
real ci(nz), cbr(nz)
! real ci_0, ci_ext, cbr_0, cbr_ex

real,allocatable :: amx(:,:), ymx(:), emx(:)  ! matrix is going to be solved by DGESV in LAPACK lib
integer info, nmx, imx, ifmx, ihmx, ihfmx
real imbr
integer,allocatable :: ipiv(:)

integer row, col, itr

real rxn(nz), dt, ucnv, rxn_scale, dt_om, dt_aq, dt_sld
real bioprod, hydloss, gasloss, difloss, advloss, resid, inext
real bioprodc, hydlossc, gaslossc, diflossc, advlossc, residc, inextc
real sred
real rxn_b(nz),rxn_h(nz), frac, seddep

real start_time, finish_time  
integer iteq
! real clrxn, clgrad, clss
integer clrxn, clgrad, clss
integer itrmin, itr2, itrmax, itrcns
parameter (itrmin = 10)
! parameter (itrmax = 100)
parameter (itrmax = 40)
integer itrmaxso4
parameter (itrmaxso4 = 5)
integer itrnew, itrnewmax 
parameter (itrnewmax = 100) 

character(100) :: arg, outfile, outfilename, runid, dbase, home, outdir, indir
integer :: narg, ia

real mhinv, mbinv  ! inventory
real mhinvc, mbinvc  ! inventory in previous calculation
real mhinve, mbinve  ! errors in inventories
real mhinv_OLD, mbinv_OLD
real maxmh, maxmb, avemh, avemb  ! max and average values 
real ceqav, ceqh, ceqb, ceqmx,ceqdh,ceqdb
integer :: cnth, cntb
character(100) :: latch, lonch
integer :: latint, lonint

character(100) procn 

integer :: hszflg

real cmmb, hmb, bmb  ! errors in mass balances of aqueous methane, hydrate and bubble

real maxrate   ! maximum rate of hydrate and bubble formation 
parameter (maxrate = 1d-8)

real rxn_hb(nz), rxn_hb_max, rxn_bh(nz), rxn_bh_max, expbb, rxn_hb_ex, rxn_bh_ex  ! reaction rate from h to b 
real rxn_aom(nz), rxn_aom_0 ! reaction of AOM (1-30 cm3 yr-1 mmol-1; Wallmann et al. GCA 2006
parameter (rxn_hb_max = 1d-8)
parameter (rxn_bh_max = 1d-8)

real avx,avy,crr,sumx2,sumy2,sumxy,itcpt,slp, so4s,so4new,zsnew  ! parameters to iteratively calculate SRZ
real avx_up,avy_up,crr_up,sumx2_up,sumy2_up,sumxy_up,itcpt_up,slp_up, xa, ya, q1, q2, q3, delx
! parameter (so4s = 28d0)  ! 28 mM
integer itrso4, nsmpl
real zeros(100)
parameter (zeros = 0d0)

real :: zdeep = 10d3

character(100) method
! declaration from TDcomplete (obtained from Stephen Hunter)
REAL,DIMENSION(1001,2)::eq_allo
REAL,ALLOCATABLE::ceq_allo(:),T_allo(:),zeq(:)
INTEGER:: size_eq, sizeT
REAL::alpha,c3,dz2

real::ztot=1000d0
real clsp,beta,dz(nz)
character(100) clst_type

real::mch4=16d-3 ! ch4 kg/mol
real::mch2o=30d-3 ! ch2o kg/mol
real::mso4=96d-3  ! so4 kg/mol

real::tol = 1d-6
real::yr2sec = 365d0*24d0*60d0*60d0  ! sec/yr
real,dimension(nz) :: up,dwn,cnr,adf,cmxx,csxx,hxx,bxx
real::time,error
integer::nflx
parameter(nflx=10)
integer::itflx,iadv,idif,irxn_ah,irxn_ab,irxn_hb,irxn_om,irxn_aom,irxn_dec,ires
data itflx,iadv,idif,irxn_ah,irxn_ab,irxn_hb,irxn_om,irxn_aom,irxn_dec,ires/1,2,3,4,5,6,7,8,9,10/
real,dimension(nflx,nz)::omflx,so4flx,ch4flx,hflx,bflx
character(100)::flxnames(nflx)
data flxnames/'itflx','iadv','idif','irxn_ah','irxn_ab','irxn_hb','irxn_om','irxn_aom','irxn_dec','ires'/
integer iflx
logical fix_cm_ext,flg_esc,fix_nonfix
real::hsz(nz),bsz(nz),srz(nz)  ! 1 if inside hsz, bsz or srz; 0 otherwise 
character(100) kin_type,run_type 
real dt_ref
logical spin_aq_done, zs_can_change 

integer file_out,file_errhsz,file_errsedep,file_therm,file_om,file_ch4,file_erritr,file_busat
data file_out,file_errhsz,file_errsedep,file_therm,file_om,file_ch4,file_erritr,file_busat/101,102,103,104,105,106,107,108/
integer file_SRZFastEnd,file_anion,file_errsrz,file_errnew,file_ch4flx,file_so4flx,file_bnd,file_rec
data file_SRZFastEnd,file_anion,file_errsrz,file_errnew,file_ch4flx,file_so4flx,file_bnd,file_rec/109,110,111,112,113,114,115,116/
integer file_so4flxss,file_ch4flxss,file_hflxss,file_bflxss,file_omflxss
data file_so4flxss,file_ch4flxss,file_hflxss,file_bflxss,file_omflxss/117,118,119,120,121/
logical flg_err,define_z
integer,allocatable::file_ch4ser(:),file_omser(:),file_so4ser(:)
integer,allocatable::file_ch4flxser(:),file_so4flxser(:),file_hflxser(:),file_bflxser(:),file_omflxser(:)

real,allocatable::rectime(:)
integer nrec,irec
character(1000) readfile
logical::loc_display =.true.
! logical::loc_display =.false.
logical::limsed =.false.
logical::bubble_only =.true.
! logical::bubble_only =.false.
logical::first_call =.false.
logical::first_call_save =.false.
real time_fin
real z_prev(nz),z_err,dz_prev(nz)
real,dimension(nflx,nz)::dumflx
real loc_margin
real thermcond

integer,parameter::n_interp=1
integer::iz_min,iz_max,nz_interp
real,allocatable::interp_data(:,:),t_data(:),p_data(:,:)
! real::d_threshold = 3000.
! real::d_threshold = 2000.
real::d_threshold = 20000.
logical::err_interp
real::dum_interp(5,nz)

! logical::hydrate_restart = .true.
logical::hydrate_restart = .false.
logical::hydrate_restart_OK = .false.
! logical::bubble_cut = .true.
! logical::bubble_cut = .false.
character(1000) resdir 
real:: min_thr = 1d-10

logical:: grid_evol = .false.
logical dir_e

real::loc_zs,loc_hsz

call cpu_time(start_time)

mhinv_OLD = mhinv_map(dum_i,dum_j)
mbinv_OLD = mbinv_map(dum_i,dum_j)

mhinv_map_OLD(dum_i,dum_j) = -1d0
mbinv_map_OLD(dum_i,dum_j) = -1d0

! parameters to make grid 
beta = 1.5d0 ! for up concentrating grid 
! beta = 50d0 ! for mid concentrating grid 
clsp=100
clst_type='up'
! clst_type='mid'
! clst_type='upmid'
! clst_type='default'
clst_type='two'
clst_type='two_x5'
! clst_type='two_x8'
! clst_type='regular'

! write(home,*) './'
! write(outdir,*) '../hydrate_output/'
indir = trim(par_indir_name)
outdir = trim(par_outdir_name)//'/hydrate/'
! resdir = 'EXAMPLE.worbe2.RH1997_36x36_Hydrate_G.SPINEMIT'
resdir = trim(par_sed_hydrate_resdir)
resdir = '../'//trim(resdir)//'/sedgem/hydrate/'
hydrate_restart = par_sed_hydrate_restart
! print*,outdir
! stop

! write(method,*) 'maekawa'
! write(method,*) 'davie'
! write(method,*) 'duan'
! method = trim(method)
method = trim(par_sed_hydrate_opt_therm)

if (.not. trim(method) == 'duan') then 
    bubble_only = .false.  ! when thermodynamics is not by duan, bubble only case cannot be simulated
endif 

call get_par_defs(                      &! all output
    geotherm,wdpth,wtemp,xis,sal,       &
    rho_f,rho_h,rho_b,rho_s,grav,p0,    &
    poro_0,poro_scale,                  &
    v_sed,u_ext,cm_ext,                 &
    frac,org_0,                         &
    dec_s0,dec_m0,zs,                   &
    dif_m_0,dif_s_0,dif_c_0,            &
    ch,cb,rxn,rxn_h,rxn_b,              &
    ucnv,cs_0,seddep,                   &
    rxn_aom_0,cm_0,                     &
    nz                                  &! input
    )

geotherm = geotherm_in
wdpth = wdpth_in
wtemp = wtemp_in - const_zeroC
sal = sal_in/58.44d0  ! g/kg to mol/kg
xis = sal/(1000d0/18d0) ! mole fraction assuming salt is all NaCl
v_sed = v_sed_in/(2.65d0*(1d0-0.67d0))*1d3 ! converting g/cm2/yr to cm/kyr where porosity 0.67 and solid density 2.65 is assumed 
so4s = so4s_in
cs_0 = so4s*1d-3  ! converting mM to M
! cm_0 = cm_0_in
cm_0 = max(0.0,cm_0_in) 
org_0 = org_0_in
loc_margin = margin_in

thermcond = par_sed_hydrate_thermcond

print '(11(3x,A9))', 'wdpth', 'wtemp', 'org_0', 'frac', 'v_sed', 'u_ext', 'geotherm', 'dec_m0', 'so4s', 'sal', 'seddep'
print '(11(3x,e9.3e2))', wdpth, wtemp, org_0, frac, v_sed, u_ext, geotherm, dec_m0, so4s, sal, seddep

print '(8(3x,A9))', 'zs', 'rho_f', 'rho_h', 'rho_s', 'rho_b', 'pore_scale', 'dec_s0','cm_0'
print '(8(3x,e9.3e2))', zs, rho_f, rho_h, rho_s, rho_b, poro_scale,dec_s0,cm_0

lonint = dum_j
latint = dum_i
write(lonch,'(I3.3)') lonint
write(latch,'(I3.3)') latint
outfilename = trim(adjustl(latch))//'-'//trim(adjustl(lonch))

ceqav = 0d0
ceqmx = 0d0
ceqdh = 0d0
ceqdb = 0d0
ceqh = 0d0
ceqh = 0d0

define_z = .true.

call calctherm(                                     &
    wtemp,geotherm,grav,rho_f,xis,wdpth,sal,        &! input
    clst_type,nz,beta,seddep,                       &! input 
    method,latint,lonint,indir,define_z,            &! input
    limsed,bubble_only,loc_display,                 &! input  
    dz,z,p3,temp3,z3,zmax,ceq,hszflg                &! output   
    )

ceqav = sum(ceq(1:nz)*dz(1:nz))/sum(dz(1:nz))*rho_f/16d0/1d-3 ! conversion from g CH4/g l to mM 
ceqmx = ceq(int(0.5d0*nz))*rho_f/16d0/1d-3
ceqdh = ceq(1)*rho_f/16d0/1d-3-ceqmx
ceqdb = ceq(nz)*rho_f/16d0/1d-3-ceqmx
ceqh  = sum(ceq(1:int(0.5d0*nz))*dz(1:int(0.5d0*nz)))/sum(dz(1:int(0.5d0*nz)))*rho_f/16d0/1d-3
ceqb  = sum(ceq(int(0.5d0*nz+1):nz)*dz(int(0.5d0*nz+1):nz))/sum(dz(int(0.5d0*nz+1):nz))*rho_f/16d0/1d-3
loc_zs = const_real_null
loc_hsz = z3

if(.not.bubble_only)then
    if (hszflg == 2) then 
        ceqmx = const_real_null
        ceqdh = const_real_null
        ceqdb = const_real_null
        ceqh  = const_real_null
        ceqb  = const_real_null
        loc_zs = const_real_null
        loc_hsz = const_real_null
    endif
endif

! if(wdpth<=175d0)then
    ! mhinv_map(dum_i,dum_j) = -1.
    ! mbinv_map(dum_i,dum_j) = -1.
    ! ch4flx_map(dum_i,dum_j) = -1.
    ! so4flx_map(dum_i,dum_j) = -1.
    ! heatflow_map(dum_i,dum_j) = -1.
    ! return
! endif 

! if(wdpth > d_threshold) then
if(loc_margin <= 0d0 .or. (.not.bubble_only.and.hszflg == 2)) then
! if(loc_margin <= 0d0) then
! if(.not.(dum_i==50 .and. dum_j==3))then
! if(.not.(dum_i==34 .and. dum_j==5))then
! if(.not.(dum_i==2 .and. dum_j==49))then
    print*,'exit the hydrate calculation as depth is deeper than threshold depth:',d_threshold,' at',dum_i,dum_j
    mhinv_map(dum_i,dum_j) = 0.
    mbinv_map(dum_i,dum_j) = 0.
    ch4flx_map(dum_i,dum_j) = 0.
    so4flx_map(dum_i,dum_j) = 0.
    dicflx_map(dum_i,dum_j) = 0.
    alkflx_map(dum_i,dum_j) = 0.
    ch4gen_map(dum_i,dum_j) = 0.
    ch4adv_map(dum_i,dum_j) = 0.
    so4red_map(dum_i,dum_j) = 0.
    degas_map(dum_i,dum_j) = 0.
    aom_map(dum_i,dum_j) = 0.
    zs_map(dum_i,dum_j) = loc_zs
    hsz_map(dum_i,dum_j) = loc_hsz
    ceqh_map(dum_i,dum_j) = ceqh
    ceqb_map(dum_i,dum_j) = ceqb
    heatflow_map(dum_i,dum_j) = geotherm_in*par_sed_hydrate_thermcond*1000.
    depth_map(dum_i,dum_j) = wdpth
    om_map(dum_i,dum_j) = org_0 * frac
    btso4_map(dum_i,dum_j) = so4s
    temp_map(dum_i,dum_j) = wtemp
    sedv_map(dum_i,dum_j) = v_sed   ! cm/kyr
    sal_map(dum_i,dum_j) = sal*58.44d0
    margin_map(dum_i,dum_j) = loc_margin
    sedthick_map(dum_i,dum_j) = sedthick_in
    return
endif

! inquire(file=trim(adjustl(outdir))//'res/.', exist=dir_e)
! if (.not.dir_e) then 
    ! irec_hydrate = 0
! endif 
    
call system('mkdir -p '//trim(adjustl(outdir))//'profiles/'//trim(adjustl(outfilename)))
call system('mkdir -p '//trim(adjustl(outdir))//'res')

call checkfile(trim(adjustl(outdir))//'/profiles/'//trim(adjustl(outfilename))&
    //'/org.txt',first_call)


call checkfile(trim(adjustl(outdir))//'/res/Global_C-0.res',dir_e)

if (hydrate_restart) then 
    call checkfile(trim(adjustl(resdir))//'/profiles/'//trim(adjustl(outfilename))&
        //'/org.txt',hydrate_restart_OK)
    hydrate_restart_OK = .not. hydrate_restart_OK
endif 

! if(first_call)irec_hydrate = 0
if(first_call .and. (.not. irec_hydrate> 0)) irec_hydrate = 0
! if(dir_e .and. (.not. irec_hydrate>=0)) irec_hydrate = 0

if (first_call) then 
    first_call_save = .true.
    so4flx_map_INIT(dum_i,dum_j) = 0.
    ch4flx_map_INIT(dum_i,dum_j) = 0.
endif 

if(loc_display)print*,'first call? A: ',first_call
if(loc_display)print*,'can restart? A: ',hydrate_restart_OK
! pause
if (.not.first_call) hydrate_restart = .false.
! if (hydrate_restart) first_call = .false.
if (hydrate_restart .and. hydrate_restart_OK) first_call = .false.

if(.not.first_call)then
    ! reading cm, cs, h, b, z, dz, v, u, org etc. from previous run 
    if (.not.hydrate_restart) then 
        call readprofiles(                                                                                          &
            file_om,z,dz,org,phi_s,phi_m,file_ch4,cm,ceq,h,b,u,v,rxn_h,rxn_b,cc,rho_f,cs,cbr,ci,nz,file_anion,         &
            outdir,outfilename                                                                                      &
            )
    elseif (hydrate_restart) then  
        call readprofiles(                                                                                          &
            file_om,z,dz,org,phi_s,phi_m,file_ch4,cm,ceq,h,b,u,v,rxn_h,rxn_b,cc,rho_f,cs,cbr,ci,nz,file_anion,         &
            resdir,outfilename                                                                                      &
            )
    endif 
    cm = cm/(1e3*mch4/rho_f)
    cs = cs/1e3
    ! make newton iteration easy to converge?
    ! do iz = 1,nz 
        ! if (cm(iz) < 1d-100) cm(iz) = 1d-310
    ! enddo
        
    dumflx = 0d0
    call display_prof(                              &
        nz,nflx,                                    &
        ch,cb,rho_h,rho_b,mch4,100000.,100000.,     &
        z,cm,cs,h,b,dz,                             &
        dumflx,dumflx,dumflx,dumflx,dumflx,         &
        flxnames                                    &
        )
    ! pause

    if (.not.hydrate_restart) then 
        mhinv_map_OLD(dum_i,dum_j) = mhinv_map(dum_i,dum_j)
        mbinv_map_OLD(dum_i,dum_j) = mbinv_map(dum_i,dum_j)
    elseif (hydrate_restart) then  
        mhinv_map_OLD(dum_i,dum_j) = -1d0
        mbinv_map_OLD(dum_i,dum_j) = -1d0
    endif 
endif

call openfiles(                                                         &
    file_om,file_ch4,file_anion,file_ch4flx,file_so4flx,file_bnd,       &
    file_so4flxss,file_ch4flxss,file_hflxss,file_bflxss,file_omflxss,   &
    outdir,outfilename                                                  &
    )
if(loc_display)then
    print*,'going to calculate thermodynmaics for the first time'
endif 

if((.not.first_call) .and. grid_evol)then 
    ! better not to switch on 'grid_evol' as this may cause problems in mass balance 
    ! when re-scaling etc. attemp to re-scale from the previous run is made below 
    ! but not certain this will work ... (observed some crushes)
    z_prev = z 
    dz_prev = dz
    ! added 4/30/2020
    define_z = .true.
    call calctherm(                                     &
        wtemp,geotherm,grav,rho_f,xis,wdpth,sal,        &! input
        clst_type,nz,beta,seddep,                       &! input 
        method,latint,lonint,indir,define_z,            &! input
        limsed,bubble_only,loc_display,                 &! input  
        dz,z,p3,temp3,z3,zmax,ceq,hszflg                &! output   
        )
    ! added-end 4/30/2020
    z_err = maxval(abs((z - z_prev)/z_prev))
    if (z_err>tol) then 
        iz_min = 1
        iz_max = nz
        do iz=1,nz
            if (z(iz)>=z_prev(1)) then 
                iz_min = iz
                exit 
            endif 
        enddo 
        do iz=nz,1,-1
            if (z(iz)<=z_prev(nz)) then 
                iz_max = iz
                exit 
            endif 
        enddo 
        nz_interp = iz_max - iz_min + 1
        do iz=iz_min,iz_max
            call interp1d(nz,org,z_prev,z(iz),dum_interp(1,iz),err_interp)
            call interp1d(nz,cm,z_prev,z(iz),dum_interp(2,iz),err_interp)
            call interp1d(nz,cs,z_prev,z(iz),dum_interp(3,iz),err_interp)
            call interp1d(nz,h,z_prev,z(iz),dum_interp(4,iz),err_interp)
            call interp1d(nz,b,z_prev,z(iz),dum_interp(5,iz),err_interp)
        enddo 
        org(iz_min:iz_max) = dum_interp(1,iz_min:iz_max)
        cm(iz_min:iz_max) = dum_interp(2,iz_min:iz_max)
        cs(iz_min:iz_max) = dum_interp(3,iz_min:iz_max)
        h(iz_min:iz_max) = dum_interp(4,iz_min:iz_max)
        b(iz_min:iz_max) = dum_interp(5,iz_min:iz_max)
        
        if(iz_min/=1) then
            do iz=iz_min-1,1,-1
                org(iz) = org(iz+1) - (org(iz+2)-org(iz+1))/(0.5d0*(dz(iz+2)+dz(iz+1)))*(0.5d0*(dz(iz+1)+dz(iz)))
                cm(iz) = cm(iz+1) - (cm(iz+2)-cm(iz+1))/(0.5d0*(dz(iz+2)+dz(iz+1)))*(0.5d0*(dz(iz+1)+dz(iz)))
                cs(iz) = cs(iz+1) - (cs(iz+2)-cs(iz+1))/(0.5d0*(dz(iz+2)+dz(iz+1)))*(0.5d0*(dz(iz+1)+dz(iz)))
                h(iz) = h(iz+1) - (h(iz+2)-h(iz+1))/(0.5d0*(dz(iz+2)+dz(iz+1)))*(0.5d0*(dz(iz+1)+dz(iz)))
                b(iz) = b(iz+1) - (b(iz+2)-b(iz+1))/(0.5d0*(dz(iz+2)+dz(iz+1)))*(0.5d0*(dz(iz+1)+dz(iz)))
            enddo
        endif 
        
        if(iz_max/=nz) then
            do iz=iz_max-1,nz
                org(iz) = org(iz-1) + (org(iz-1) - org(iz-2))/(0.5d0*(dz(iz-2)+dz(iz-1)))*(0.5d0*(dz(iz-1)+dz(iz)))
                cm(iz) = cm(iz-1) + (cm(iz-1) - cm(iz-2))/(0.5d0*(dz(iz-2)+dz(iz-1)))*(0.5d0*(dz(iz-1)+dz(iz)))
                cs(iz) = cs(iz-1) + (cs(iz-1) - cs(iz-2))/(0.5d0*(dz(iz-2)+dz(iz-1)))*(0.5d0*(dz(iz-1)+dz(iz)))
                h(iz) = h(iz-1) + (h(iz-1) - h(iz-2))/(0.5d0*(dz(iz-2)+dz(iz-1)))*(0.5d0*(dz(iz-1)+dz(iz)))
                b(iz) = b(iz-1) + (b(iz-1) - b(iz-2))/(0.5d0*(dz(iz-2)+dz(iz-1)))*(0.5d0*(dz(iz-1)+dz(iz)))
            enddo
        endif 
        
        dumflx = 0d0
        call display_prof(                              &
            nz,nflx,                                    &
            ch,cb,rho_h,rho_b,mch4,100000.,100000.,     &
            z,cm,cs,h,b,dz,                             &
            dumflx,dumflx,dumflx,dumflx,dumflx,         &
            flxnames                                    &
            )
        ! pause
    endif 
endif 

if(limsed)then
    if (seddep <= zs) then 
    ! #ifdef SO4reg
        ! exit
    ! #endif
        print *, '... too thin sediment'
        call closefiles(                                                        &
            file_om,file_ch4,                                                   &
            file_anion,file_ch4flx,file_so4flx,file_bnd,                        &
            file_so4flxss,file_ch4flxss,file_hflxss,file_bflxss,file_omflxss    &
            )
        stop
    endif
endif

poro_ext = poro_0*exp(-1d0)
org_0 = org_0*frac

clss = 1  !  when assuminng steady state and omit terms relevant to methane
clrxn = 0  ! 1 when using phi_hb 
clgrad = 1-clrxn ! 1 when using gradient of v*poro*hb instead of phi_hb
 
! dec_s0 = 1d-13
! dif_s_0 = (157.68d0 + 7.88d0*wtemp)*1d-2
 
poro(:) = poro_0*exp(-z(:)/poro_scale)
dec_s0 = dec_s0*yr2sec  ! converting /sec to /yr
dec_m0 = dec_m0*yr2sec  ! converting /sec to /yr
dif_s_0 = dif_s_0*yr2sec  ! converting /sec to /yr
dif_m_0 = dif_m_0*yr2sec  ! converting /sec to /yr
v_sed = v_sed*1d-2/1d3 ! converting cm/kyr to m/yr
v(:) = (1d0-poro_0)*v_sed/(1d0-poro(:))  ! m/yr
v_ext = (1d0-poro_0)*v_sed/(1d0-poro_ext)
! v_ext = v(nz)
sedtemp(:) = wtemp + geotherm*z(:)
! v = v*1d-2/1d3/365d0/24d0/60d0/60d0  ! m/s

if(limsed)then
    if (zmax < seddep) then
        poro_ext = poro_0*exp(-1d0)
    else 
        poro_ext = poro_0*exp(-(zmax)/poro_scale)
    endif
endif

! coefficients
dif_m = dif_m_0
dif_s = dif_s_0!*poro*poro
dif_c = dif_c_0
rxn_aom = rxn_aom_0
! rxn_aom = 0d0
! rxn_aom(1:imx) = rxn_aom_0; rxn_aom(imx+1:nz) = 0d0

rxn_h = 1d-8 ! Davie and Buffett 2001 in s-1
rxn_b = 1d-8 ! Davie and Buffett 2001 in s-1

call calchbsz(  &
    nz,z3,z,dz,hszflg,tol  &! input 
    ,hsz,bsz  &! output
    ) 
rxn_h = rxn_h*hsz*yr2sec
rxn_b = rxn_b*bsz*yr2sec
rxn_hb = rxn_b
rxn_bh = rxn_h
! rxn_hb = 0d0
! rxn_bh = 0d0

call calcsrz(  &
    nz,zs,z,dz  &! input 
    ,srz  &! output
    ) 
! rxn_aom = rxn_aom*0d0
! initial conditions
if(first_call)then
    org = 1d-20  ! unit is wt%
    h = 1d-20  ! unitless [vol. faction]
    b = 1d-20
    cm = cm_0  !  unit is mol L-1  
    cm = ceq/(1d3*mch4/rho_f)*2d0   ! when assuming initially saturated state x2 
    cs = cs_0 
    ! cs = max(cs_0 -cs_0*z/zs,1d-300)   ! mol L-1
    ! print *, cs
endif 

cm_ext_in = ceq(nz)/(1d3*mch4/rho_f)  ! assume bottom cm conc. as bottom equilibrium conc. unit coverted from g/g to mol/L
! cm_ext_in = 0.5d0*cm_ext_in  ! half saturation 

u_ext = u_ext*1d-2/1d3  ! converting cm/kyr to m/yr
u = (1d0-poro_0)*(poro_ext/(1d0-poro_ext)- poro*(h+b)/(1d0-poro)) *v_sed + poro_ext*u_ext  ! in m/yr
u_0 = (1d0-poro_0)*(poro_ext/(1d0-poro_ext)) *v_sed + poro_ext*u_ext
! u_0 = u(1)

call calcupwindscheme(      &
    up,dwn,cnr,adf,         &! output 
    u,nz                    &! input 
    )

! passing temporal variables 
orgx=org
hx = h 
bx = b 
cmx = cm
csx = cs
! cmx = 1d-100
! csx = 1d-100

fix_cm_ext = .false.
! fix_cm_ext = .true.

kin_type = 'default'
! kin_type = 'wallmann'
! kin_type = 'davie'

! time loop start 
it = 1
time = 0d0
if (first_call) then 
    time_fin = 1d9
    dt_ref = 1d5
    run_type = 'steadystate'
else 
    time_fin = dum_dt
    dt_ref = dum_dt
    run_type = 'transient'
endif 
flg_esc = .false. 
spin_aq_done = .false.
if (.not.first_call) spin_aq_done = .true.
zs_can_change = .true.
! zs_can_change = .false. ! when assuming fixed srz (i.e., fixed depth profiles of methanogenesis)
fix_nonfix = .true.  ! first assumes fixed cm conc. and then assume no-flux boundary at the bottom 
! fix_nonfix = .false.
timeloop:do
    if (time + dt_ref >= time_fin) dt_ref = time_fin - time
    if (.not.flg_esc) dt = dt_ref  ! in yr
    dt_om = 1d100
    if (spin_aq_done) dt_om = dt
    dt_aq = dt
    dt_sld = dt
    
    if (spin_aq_done) fix_cm_ext = .false.
    ! if (first_call .and. time > 1d8) bubble_cut = .true.
    
    ! test case of temperature change 
    ! if (time>=1d8) wtemp = min((time-1d8)*5d0/(10d3) + 3d0,8d0)
    
    ! thermodynamic caclulation 
    define_z = .false.
    call calctherm(                                     &
        wtemp,geotherm,grav,rho_f,xis,wdpth,sal,        &! input
        clst_type,nz,beta,seddep,                       &! input 
        method,latint,lonint,indir,define_z,            &! input
        limsed,bubble_only,loc_display,                 &! input  
        dz,z,p3,temp3,z3,zmax,ceq,hszflg                &! output   
        )
    ! stability zone update 
    call calchbsz(              &
        nz,z3,z,dz,hszflg,tol,  &! input 
        hsz,bsz                 &! output
        ) 
    ! reaction rate update 
    rxn_h = 1d-8 ! Davie and Buffett 2001 in s-1
    rxn_b = 1d-8 ! Davie and Buffett 2001 in s-1
    rxn_h = rxn_h*hsz*yr2sec
    rxn_b = rxn_b*bsz*yr2sec
    rxn_hb = rxn_b
    rxn_bh = rxn_h
    ! rxn_hb = 0d0
    ! rxn_bh = 0d0
    
    ! calculation of om 
    call omcalc(                                                                                            &
        zs,nz,org_0,v,org,dec_m0,dec_s0,z,dz,mch4,mch2o,mso4,poro,dt_om,tol,rho_s,rho_f,poro_0,v_sed,nflx,  &! input 
        phi_s,phi_m,orgx,omflx                                                                              &! output
        )

    ! if (dum_i==2 .and. dum_j==22 .and. time==0d0)then 
        ! print*,dum_i,dum_j 
        ! print*,loc_margin,abs(sum(omflx(iadv,:)*dz(:)))*loc_margin
        ! print*,'pause in hydrate_main'
        ! pause
    ! endif 
    ! exit timeloop
    ! phi_s = 0d0
    ! phi_m = 0d0
    error = 1d4
    itr=1
    iteration: do while (error>tol)

        u = (1d0-poro_0)*(poro_ext/(1d0-poro_ext)- poro*(hx+bx)/(1d0-poro)) *v_sed+poro_ext*u_ext   ! in m/yr; Davie and Buffett, 2001, 2003
        ! u = poro_ext*v_ext+poro_ext*u_ext                                                           ! in m/yr; Wallmann et al., 2006

        call calcupwindscheme(  &
            up,dwn,cnr,adf      &! output 
            ,u,nz               &! input 
            )
            
        cmxx=cmx
        csxx=csx
        hxx=hx
        bxx=bx
        flg_esc = .false. 
        if (trim(run_type)=='transient') then 
            call ch4so4calc(                                                                                        &
                nz,v,u,phi_m,phi_s,cs_0,cm_0,rho_h,rho_f,rho_b,ceq,cm,tol,cs,h,b,rxn_b,rxn_h,rxn_aom,v_sed,poro_0,  &! input 
                up,dwn,cnr,adf,u_0,ch,cb,u_ext,poro_ext,v_ext,z,dt_aq,mch4,poro,dz,dif_m,dif_s,zs,z3,hx,bx,nflx,    &! input 
                cm_ext_in,fix_cm_ext,kin_type,loc_display,                                                          &! input
                cmx,csx,ch4flx,so4flx,flg_esc                                                                       &! output 
                )
        elseif (trim(run_type)=='steadystate') then 
        ! following is an attempt to reach steady state at once 
            zs=10d0
            ! dt_aq = 1d100
            call ch4so4calc(                                                                                        &
                nz,v,u,phi_m,phi_s,cs_0,cm_0,rho_h,rho_f,rho_b,ceq,cm,tol,cs,h,b,rxn_b,rxn_h,rxn_aom,v_sed,poro_0,  &! input 
                up,dwn,cnr,adf,u_0,ch,cb,u_ext,poro_ext,v_ext,z,dt_aq,mch4,poro,dz,dif_m,dif_s,zs,z3,hx,bx,nflx,    &! input 
                cm_ext_in,fix_cm_ext,kin_type,loc_display,                                                          &! input
                cmx,csx,ch4flx,so4flx,flg_esc                                                                       &! output 
                ) 
            ! stop
        endif 
        if (flg_esc) then 
            dt = dt/1d1
            dt_aq = dt
            dt_sld = dt
            cmx=cmxx
            csx=csxx
            hx=hxx
            bx=bxx
            if (dt <= 1d-300) then
                print*
                print*,' *** WARNING: Convergence is difficult?: dt is now <= 1d-8 after CH4-SO4 calc'
                print '(11(3x,A9))', 'wdpth', 'wtemp', 'org_0', 'frac', 'v_sed', 'u_ext', 'geotherm', 'dec_m0', 'so4s', 'sal', 'seddep'
                print '(11(3x,e9.3e2))', wdpth, wtemp, org_0, frac, v_sed, u_ext, geotherm, dec_m0, so4s, sal, seddep
                print '(8(3x,A9))', 'zs', 'rho_f', 'rho_h', 'rho_s', 'rho_b', 'pore_scale', 'dec_s0','cm_0'
                print '(8(3x,e9.3e2))', zs, rho_f, rho_h, rho_s, rho_b, poro_scale,dec_s0,cm_0
                print*
                pause
            endif 
            cycle iteration 
        endif 
        if (trim(run_type)=='transient')then 
            call hydbubcalc(                                                                &
                nz,v,rho_f,ceq,h,b,rxn_b,rxn_h,rxn_hb,rxn_bh,v_sed,poro_0,hszflg,           &! input 
                z,dt_sld,mch4,poro,dz,cmx,nflx,ch,cb,rho_h,rho_b,kin_type,zs,loc_display,   &! input 
                bx,hx,hflx,bflx,flg_esc                                                     &! output 
                )
        elseif(trim(run_type)=='steadystate')then 
            ! dt_sld=1d100  ! attempt to reach steady state at once 
            call hydbubcalc(                                                                &
                nz,v,rho_f,ceq,h,b,rxn_b,rxn_h,rxn_hb,rxn_bh,v_sed,poro_0,hszflg,           &! input 
                z,dt_sld,mch4,poro,dz,cmx,nflx,ch,cb,rho_h,rho_b,kin_type,zs,loc_display,   &! input 
                bx,hx,hflx,bflx,flg_esc                                                     &! output 
                )
        endif 
        if (flg_esc) then 
            dt = dt/1d1
            dt_aq = dt
            dt_sld = dt
            cmx=cmxx
            csx=csxx
            hx=hxx
            bx=bxx
            if (dt <= 1d-300) then
                print*
                print*,' *** WARNING: Convergence is difficult?: dt is now <= 1d-8 after H-B calc'
                print '(11(3x,A9))', 'wdpth', 'wtemp', 'org_0', 'frac', 'v_sed', 'u_ext', 'geotherm', 'dec_m0', 'so4s', 'sal', 'seddep'
                print '(11(3x,e9.3e2))', wdpth, wtemp, org_0, frac, v_sed, u_ext, geotherm, dec_m0, so4s, sal, seddep
                print '(8(3x,A9))', 'zs', 'rho_f', 'rho_h', 'rho_s', 'rho_b', 'pore_scale', 'dec_s0','cm_0'
                print '(8(3x,e9.3e2))', zs, rho_f, rho_h, rho_s, rho_b, poro_scale,dec_s0,cm_0
                print*
                pause
            endif 
            cycle iteration 
        endif 
        
        error=0d0
        if (trim(run_type)=='transient')then
            do iz=1,nz
                if (cmxx(iz)/=0d0) error=max(abs((cmxx(iz)-cmx(iz))/cmxx(iz)),error)
                if (csxx(iz)/=0d0) error=max(abs((csxx(iz)-csx(iz))/csxx(iz)),error)
                if (hxx(iz)/=0d0) error=max(abs((hxx(iz)-hx(iz))/hxx(iz)),error)
                if (bxx(iz)/=0d0) error=max(abs((bxx(iz)-bx(iz))/bxx(iz)),error)
            enddo 
        endif 
        ! 4/30/2020 added 
        ! if ((sum(cmx) < min_thr) .and. (sum(hx) < min_thr) .and. (sum(bx) < min_thr)) then 
            ! error = 0d0
        ! endif 
        ! Addition end
        
        if(loc_display)then     
            print'(a,i0,a,e10.3e3)','itr=',itr,'  error=',error
        endif 
        itr=itr+1
        if (itr > 10) then 
            flg_esc = .true. 
            exit iteration
        endif 
    enddo iteration 
    ! exit timeloop
    if (flg_esc) then 
        dt = dt/1d1
        cmx=cmxx
        csx=csxx
        hx=hxx
        bx=bxx
        cycle timeloop
    endif 
    if(loc_display)then        
        call display_prof(                      &
            nz,nflx,                            &
            ch,cb,rho_h,rho_b,mch4,dt,time,     &
            z,cmx,csx,hx,bx,dz,                 &
            omflx,so4flx,ch4flx,hflx,bflx,      &
            flxnames                            &
            )
    endif     
    if (                                                                                        &
        abs(sum(omflx(ires,:)*dz(:)))/(abs(sum(omflx(iadv,:)*dz(:))))>tol .or.                  &
        abs(sum(so4flx(ires,:)*dz(:)))/(abs(sum(omflx(iadv,:)*dz(:))))>tol.or.                  &
        abs(sum(ch4flx(ires,:)*dz(:)))/(abs(sum(omflx(iadv,:)*dz(:))))>tol.or.                  &
        abs(sum(hflx(ires,:)*dz(:)))*(ch*rho_h/mch4)/(abs(sum(omflx(iadv,:)*dz(:))))>tol.or.    &
        abs(sum(bflx(ires,:)*dz(:)))*(cb*rho_b/mch4)/(abs(sum(omflx(iadv,:)*dz(:))))>tol        &
        ) then 
        if(loc_display)then
            print *,'too large error in calculation?'
        endif 
        ! pause
    endif 
    
    time = time + dt
    it = it + 1
    
    cm = cmx
    cs = csx
    h = hx
    b = bx
    org = orgx
    
    if (.not.spin_aq_done  &
        .and.abs(sum(so4flx(itflx,:)*dz(:)))/(abs(sum(so4flx(idif,:)*dz(:))))<tol) then 
        spin_aq_done = .true.
    endif 
    
    if (spin_aq_done) then 
        if (zs_can_change) then 
            call calc_zs(                   &
                nz,cm,cs,cm_0,cs_0,z,ztot,  &! input
                zs                          &! output
                )
        endif 
    endif 
    
    if (.not.first_call) then 
        write(file_ch4flx,*) time,(sum(ch4flx(iflx,:)*dz(:)),iflx=1,nflx)
        write(file_so4flx,*) time,(sum(so4flx(iflx,:)*dz(:)),iflx=1,nflx)
        write(file_bnd,*) time,wtemp,so4s
    endif 
            
    if (time>=time_fin) exit timeloop
    
    if (                                                                                            &
        first_call .and.                                                                            &
        abs(sum(omflx(itflx,:)*dz(:)))/(abs(sum(omflx(iadv,:)*dz(:))))<tol .and.                    &
        abs(sum(so4flx(itflx,:)*dz(:)))/(abs(sum(omflx(iadv,:)*dz(:))))<tol .and.                   &
        abs(sum(ch4flx(itflx,:)*dz(:)))/(abs(sum(omflx(iadv,:)*dz(:))))<tol .and.                   &
        abs(sum(hflx(itflx,:)*dz(:)))*(ch*rho_h/mch4)/(abs(sum(omflx(iadv,:)*dz(:))))<tol .and.     &
        abs(sum(bflx(itflx,:)*dz(:)))*(cb*rho_b/mch4)/(abs(sum(omflx(iadv,:)*dz(:))))<tol           &
        ) then 
        exit timeloop
    endif 

enddo timeloop

call recordprofiles(                                                                                                            &
    file_om,z,dz,org,phi_s,phi_m,file_ch4,cm*(1e3*mch4/rho_f),ceq,h,b,u,v,rxn_h,rxn_b,ccx,rho_f,hx,csx*1e3,cbr,ci,nz,file_anion    &
    )
call recordfluxes(                                                                      &
    nflx,nz,flxnames,omflx,so4flx,ch4flx,hflx*(ch*rho_h/mch4),bflx*(cb*rho_b/mch4),z,   &
    file_so4flxss,file_ch4flxss,file_hflxss,file_bflxss,file_omflxss                    &
    )
        
mhinv = 0d0
mbinv = 0d0
maxmh = 0d0
maxmb = 0d0
avemh = 0d0
avemb = 0d0
cnth = 0
cntb = 0

do iz = 1,nz
    mhinv=mhinv+poro(iz)*h(iz)*rho_h*ch*dz(iz) ! kg CH4/m2
    mbinv=mbinv+poro(iz)*b(iz)*rho_b*cb*dz(iz)
    if (h(iz) > 0d0) then 
        maxmh = max(maxmh,h(iz))
        avemh = avemh + h(iz)
        cnth = cnth + 1
    endif 
    if (b(iz) > 0d0) then 
        maxmb = max(maxmb,b(iz))
        avemb = avemb + b(iz)
        cntb = cntb + 1
    endif
enddo
if (cnth>0) avemh = avemh/cnth
if (cntb>0) avemb = avemb/cntb

mhinv_map(dum_i,dum_j) = mhinv 
mbinv_map(dum_i,dum_j) = mbinv
! ch4flx_map(dum_i,dum_j) = sum(ch4flx(iadv,:)*dz(:)) + sum(ch4flx(idif,:)*dz(:)) + sum(ch4flx(itflx,:)*dz(:))
! so4flx_map(dum_i,dum_j) = sum(so4flx(iadv,:)*dz(:)) + sum(so4flx(idif,:)*dz(:)) + sum(so4flx(itflx,:)*dz(:))
! ch4flx_map(dum_i,dum_j) = sum(ch4flx(iadv,:)*dz(:)) + sum(ch4flx(idif,:)*dz(:)) 
! so4flx_map(dum_i,dum_j) = sum(so4flx(iadv,:)*dz(:)) + sum(so4flx(idif,:)*dz(:)) 
! ch4flx_map(dum_i,dum_j) = sum(ch4flx(irxn_aom,:)*dz(:)) ! mol m-2 yr-1; negative when lost from sediment 
! so4flx_map(dum_i,dum_j) = sum(so4flx(irxn_aom,:)*dz(:))  

omprs_map(dum_i,dum_j) = (rho_s/mch2o)*1d-2*((1d0-poro(nz))*org(nz)*v(nz))
if (frac*ombur_map(dum_i,dum_j)*omfrc_map(dum_i,dum_j) > 0d0 &
    .and. abs(frac*ombur_map(dum_i,dum_j)*omfrc_map(dum_i,dum_j)  &
            -omprs_map(dum_i,dum_j)- abs(sum(omflx(iadv,:)*dz(:)))) &
        /(frac*ombur_map(dum_i,dum_j)*omfrc_map(dum_i,dum_j))>tol  )then 
    print*,'om not in balance'
    print*,dum_i,dum_j 
    print*,abs(sum(omflx(iadv,:)*dz(:))),frac*ombur_map(dum_i,dum_j)*omfrc_map(dum_i,dum_j)  &
        ,(rho_s/mch2o)*1d-2*((1d0-poro(nz))*org(nz)*v(nz)) &
        ,abs(frac*ombur_map(dum_i,dum_j)*omfrc_map(dum_i,dum_j)  &
            -omprs_map(dum_i,dum_j)- abs(sum(omflx(iadv,:)*dz(:)))) &
            /(frac*ombur_map(dum_i,dum_j)*omfrc_map(dum_i,dum_j))
    print*,org_0,v_sed
    ! pause
endif 

ch4adv_map(dum_i,dum_j) = sum(ch4flx(iadv,:)*dz(:))  
ch4gen_map(dum_i,dum_j) = sum(ch4flx(irxn_om,:)*dz(:))  
so4red_map(dum_i,dum_j) = sum(so4flx(irxn_om,:)*dz(:))  
aom_map(dum_i,dum_j) = sum(so4flx(irxn_aom,:)*dz(:))  
degas_map(dum_i,dum_j) = (cb*rho_b/mch4)*sum(bflx(irxn_dec,:)*dz(:))
! degas_map(dum_i,dum_j) = 0d0

select case(trim(par_sed_hydrate_opt_org))
    case('muds') 
        if (ocn_select(io_CH4))then
            ch4flx_map(dum_i,dum_j) = - degas_map(dum_i,dum_j) ! mol m-2 yr-1; negative when lost from sediment  
            dicflx_map(dum_i,dum_j) = 0d0
            alkflx_map(dum_i,dum_j) = 0d0
            so4flx_map(dum_i,dum_j) = 0d0
        else 
            ! dic and alk fluxes are meaningful only when not tracing methane and complete oxidation of methane by sulfate 
            ch4flx_map(dum_i,dum_j) =    0d0 ! mol m-2 yr-1; negative when lost from sediment  
            dicflx_map(dum_i,dum_j) = - degas_map(dum_i,dum_j)
            alkflx_map(dum_i,dum_j) = - degas_map(dum_i,dum_j)
            so4flx_map(dum_i,dum_j) =   degas_map(dum_i,dum_j)
        endif 
    case('omen') ! use omen to get om conc. (wt%) 
        if (trim(par_sed_diagen_Corgopt)=='huelse2016') then
            if (ocn_select(io_CH4))then
                so4flx_map(dum_i,dum_j) = (&
                    so4red_map(dum_i,dum_j) &
                    + aom_map(dum_i,dum_j) &
                    )
                ! so4flx_map(dum_i,dum_j) = sum(so4flx(iadv,:)*dz(:)) + sum(so4flx(idif,:)*dz(:))
                dicflx_map(dum_i,dum_j) = (&
                    ch4gen_map(dum_i,dum_j) &
                    - 2d0*so4red_map(dum_i,dum_j) &
                    - aom_map(dum_i,dum_j)  &
                    )
                alkflx_map(dum_i,dum_j) = (&
                    - 1d0*so4red_map(dum_i,dum_j) &
                    - aom_map(dum_i,dum_j)  &
                    )
                ch4flx_map(dum_i,dum_j) = (&
                    - degas_map(dum_i,dum_j) &
                    + ch4gen_map(dum_i,dum_j) &
                    + aom_map(dum_i,dum_j) &
                    + ch4adv_map(dum_i,dum_j) &!  added 7/7/2020
                    )
            else 
                so4flx_map(dum_i,dum_j) = ( &
                    so4red_map(dum_i,dum_j) &
                    + aom_map(dum_i,dum_j) &
                    - (&
                        - degas_map(dum_i,dum_j) &
                        + ch4gen_map(dum_i,dum_j) &
                        + aom_map(dum_i,dum_j) &
                        + ch4adv_map(dum_i,dum_j) &!  added 7/7/2020
                    )  &
                    )
                ! so4flx_map(dum_i,dum_j) = sum(so4flx(iadv,:)*dz(:)) + sum(so4flx(idif,:)*dz(:))
                dicflx_map(dum_i,dum_j) = ( &
                    ch4gen_map(dum_i,dum_j) &
                    - 2d0*so4red_map(dum_i,dum_j) &
                    - aom_map(dum_i,dum_j)  &
                    + ( &
                        - degas_map(dum_i,dum_j) &
                        + ch4gen_map(dum_i,dum_j) &
                        + aom_map(dum_i,dum_j) &
                        + ch4adv_map(dum_i,dum_j) &!  added 7/7/2020
                        ) &
                    )
                alkflx_map(dum_i,dum_j) = (&
                    - 1d0*so4red_map(dum_i,dum_j) &
                    - aom_map(dum_i,dum_j)  &
                    + ( &
                        - degas_map(dum_i,dum_j) &
                        + ch4gen_map(dum_i,dum_j) &
                        + aom_map(dum_i,dum_j)  &
                        + ch4adv_map(dum_i,dum_j) &!  added 7/7/2020
                        )&
                    )
                ! ch4flx_map(dum_i,dum_j) = - (cb*rho_b/mch4)*sum(bflx(irxn_dec,:)*dz(:)) &
                    ! + sum(ch4flx(irxn_om,:)*dz(:)) + sum(ch4flx(irxn_aom,:)*dz(:)) 
                    ! + sum(ch4flx(iadv,:)*dz(:)) + sum(ch4flx(idif,:)*dz(:)) 
                ch4flx_map(dum_i,dum_j) = 0d0
            endif 
        else 
            ! ch4flx_map(dum_i,dum_j) = - (cb*rho_b/mch4)*sum(bflx(irxn_dec,:)*dz(:)) ! mol m-2 yr-1; negative when lost from sediment 
            ! so4flx_map(dum_i,dum_j) = ch4flx_map(dum_i,dum_j)
            if (ocn_select(io_CH4))then
                ch4flx_map(dum_i,dum_j) = - degas_map(dum_i,dum_j) ! mol m-2 yr-1; negative when lost from sediment  
                dicflx_map(dum_i,dum_j) = 0d0
                alkflx_map(dum_i,dum_j) = 0d0
                so4flx_map(dum_i,dum_j) = 0d0
            else 
                ! dic and alk fluxes are meaningful only when not tracing methane and complete oxidation of methane by sulfate 
                ch4flx_map(dum_i,dum_j) =    0d0 ! mol m-2 yr-1; negative when lost from sediment  
                dicflx_map(dum_i,dum_j) = - degas_map(dum_i,dum_j)
                alkflx_map(dum_i,dum_j) = - degas_map(dum_i,dum_j)
                so4flx_map(dum_i,dum_j) =   degas_map(dum_i,dum_j)
            endif 
        endif 
endselect 
! ch4flx_map(dum_i,dum_j) = (mhinv_OLD - mhinv + mbinv_OLD - mbinv)*1e3/16./dum_dt ! mol m-2 yr-1; negative when lost from sediment 
! so4flx_map(dum_i,dum_j) = ch4flx_map(dum_i,dum_j) 
zs_map(dum_i,dum_j) = zs
hsz_map(dum_i,dum_j) = z3
ceqh_map(dum_i,dum_j) = ceqh
ceqb_map(dum_i,dum_j) = ceqb
heatflow_map(dum_i,dum_j) = geotherm_in*par_sed_hydrate_thermcond*1000.
depth_map(dum_i,dum_j) = wdpth
om_map(dum_i,dum_j) = org_0 ! /frac
btso4_map(dum_i,dum_j) = so4s
temp_map(dum_i,dum_j) = wtemp
sedv_map(dum_i,dum_j) = v_sed/(1d-2/1d3) ! converting m/yr to cm/kyr 
sal_map(dum_i,dum_j) = sal*58.44d0
margin_map(dum_i,dum_j) = loc_margin
sedthick_map(dum_i,dum_j) = sedthick_in

if (first_call) then 
    mhinv_map_OLD(dum_i,dum_j) = mhinv_map(dum_i,dum_j)
    mbinv_map_OLD(dum_i,dum_j) = mbinv_map(dum_i,dum_j)
endif 

if (first_call_save) then 
    so4flx_map_INIT(dum_i,dum_j) = so4flx_map(dum_i,dum_j)
    ch4flx_map_INIT(dum_i,dum_j) = ch4flx_map(dum_i,dum_j)
endif 

call closefiles(                                                        &
    file_om,file_ch4,                                                   &
    file_anion,file_ch4flx,file_so4flx,file_bnd,                        &
    file_so4flxss,file_ch4flxss,file_hflxss,file_bflxss,file_omflxss    &
    )

call cpu_time(finish_time)
write(*,'(f10.3,A)')finish_time-start_time,'[CPU sec]'

end subroutine hydrate_main
!**************************************************************************************************************************************

!**************************************************************************************************************************************
subroutine hydrate_main_ss(                         &
    dum_i,dum_j,dum_dt,                             &
    geotherm_in,wdpth_in,wtemp_in,sal_in,v_sed_in,  &
    so4s_in,cm_0_in,org_0_in,                       &
    margin_in                                       &
    )
implicit none

!  assume both bubbles and hydrates are separatable from fluid (porewater) 
!  accordingly, porewater velocity consider effects of both phases
!  This treatment may be different from those by Buffet model and Hunter's as well
!  Klauda (e.g., Klauda and Sandler, 2005) does not take into account bubbles at all. 
!  from hydrate_v0_5_5.f90 to conduct a large number of simulations (results are stored in directory 'output-"runid"') 
!  improve the solution of HSZ
!  options to choose methods to calculate HSZ 
!  try to produce bubbles
!  adding sulfate reduction zone calculation to hydrate_v0_5_5_gb_v5.f90 
!  further adding the depth dependence of the degradation rate to v6
!  adding constraint from sediment thickness
!  allowing bubble only calculation (allowing a negative 3 phase equilibrium point but (yet) only with thermo. data by Tishchenko and Duan and Mao and others)
!  new version (9/30/2019) trying to clean things up and facilitate calculation ?
!  v11 allowing irregular grid

! UNITS need be carefully treated:
! aq CH4 (cm, ceq) are generally given in g CH4/g water (i.e., weight fraction)
! hydrate and bubble CH4 (h, b) are generally given in cm3/cm3 total pore (volume fraction)

integer,intent(in)::dum_i,dum_j
real,intent(in)::geotherm_in,wdpth_in,wtemp_in,sal_in,v_sed_in,so4s_in,cm_0_in,org_0_in,dum_dt,margin_in

integer iz, nz, iz3
parameter (nz = 50)
real ceq(nz), z(nz), zmax, z3, sal 
real geotherm, wdpth, wtemp
real rho_f, rho_h, rho_s, rho_b, grav
real xis, p0, p3, temp3, f2
real df2dtemp, err3, tor3, dp3, dtemp3
parameter (tor3 = 1d-4) 

real poro(nz), poro_0, poro_scale, poro_ext
real v(nz), u(nz), v_sed, v_ext

real org(nz), dec_s(nz), dec_m(nz), org_0, zs, dec_m0, dec_s0, sedtemp(nz)
real dif_m_0, dif_s_0, dif_c_0,dif_m(nz),dif_s(nz),dif_c(nz)
real phi_m(nz), phi_s(nz), cm(nz), h(nz), b(nz), ch, cb, cm_ext
real cs(nz), cc(nz), cmx(nz), hbx(nz), ux(nz), chb, rho_hb, hb(nz)
real cc_0, ccx(nz), cc_ext, cs_0, csx(nz), cs_ext, sdif_bot, sdif_top
real phi_hb(nz), hx(nz), bx(nz), phi_h(nz), phi_b(nz), prodh, prodb
real ci(nz), cbr(nz)
! real ci_0, ci_ext, cbr_0, cbr_ex

real,allocatable :: amx(:,:), ymx(:), emx(:)  ! matrix is going to be solved by DGESV in LAPACK lib
integer info, nmx, imx, ifmx, ihmx, ihfmx
real imbr
integer,allocatable :: ipiv(:)

integer row, col, itr

real rxn(nz), dt, ucnv, rxn_scale
real bioprod, hydloss, gasloss, difloss, advloss, resid, inext
real bioprodc, hydlossc, gaslossc, diflossc, advlossc, residc, inextc
real sred
real rxn_b(nz),rxn_h(nz), frac, seddep

real start_time, finish_time  
integer iteq
! real clrxn, clgrad, clss
integer clrxn, clgrad, clss
integer itrmin, itr2, itrmax, itrcns
parameter (itrmin = 10)
! parameter (itrmax = 100)
parameter (itrmax = 40)
integer itrmaxso4
! parameter (itrmaxso4 = 5)
integer itrnew, itrnewmax 
parameter (itrnewmax = 100) 

character(100) :: arg, outfile, outfilename, runid, dbase, indir, outdir
integer :: narg, ia

real mhinv, mbinv  ! inventory
real mhinvc, mbinvc  ! inventory in previous calculation
real mhinve, mbinve  ! errors in inventories
real maxmh, maxmb, avemh, avemb  ! max and average values 
real ceqav, ceqh, ceqb, ceqmx,ceqdh,ceqdb
integer :: cnth, cntb
character(3) :: latch, lonch
integer :: latint, lonint

character(100) procn 

integer :: hszflg

real cmmb, hmb, bmb  ! errors in mass balances of aqueous methane, hydrate and bubble

real maxrate   ! maximum rate of hydrate and bubble formation 
parameter (maxrate = 1d-8)

real rxn_hb, rxn_hb_max, rxn_bh, rxn_bh_max, expbb, rxn_hb_ex, rxn_bh_ex  ! reaction rate from h to b 
parameter (rxn_hb_max = 1d-8)
parameter (rxn_bh_max = 1d-8)
real func  ! a function return 1 if true 0 if false

real avx,avy,crr,sumx2,sumy2,sumxy,itcpt,slp, so4s,so4new,zsnew  ! parameters to iteratively calculate SRZ
real avx_up,avy_up,crr_up,sumx2_up,sumy2_up,sumxy_up,itcpt_up,slp_up, xa, ya, q1, q2, q3, delx
! parameter (so4s = 28d0)  ! 28 mM
integer itrso4, nsmpl
real zeros(100)
parameter (zeros = 0d0)

real :: zdeep = 10d3

character(100) :: method
! declaration from TDcomplete
REAL,DIMENSION(1001,2)::eq_allo
REAL,ALLOCATABLE::ceq_allo(:),T_allo(:),zeq(:)
INTEGER:: size_eq, sizeT
REAL::alpha,c3,dz2

real::aomflx_ch4=0d0 !  mol cm-2 yr-1

real::ztot=1000d0
real clsp,beta,dz(nz)
character(100) clst_type
logical::define_z = .true.

integer file_out,file_errhsz,file_errsedep,file_therm,file_om,file_ch4,file_erritr,file_busat
data file_out,file_errhsz,file_errsedep,file_therm,file_om,file_ch4,file_erritr,file_busat/101,102,103,104,105,106,107,108/

integer file_SRZFastEnd,file_anion,file_errsrz,file_errnew
data file_SRZFastEnd,file_anion,file_errsrz,file_errnew/109,110,111,112/

logical flg_err

integer::nflx
parameter(nflx = 10)

real cm_0,rxm_aom(nz),rxn_aom_0
real loc_margin
real,dimension(nflx,nz)::dumflx

real z_prev(nz),z_err,u_ext

integer,parameter::n_interp=1
integer::iz_min,iz_max,nz_interp
real,allocatable::interp_data(:,:),t_data(:),p_data(:,:)
! real::d_threshold = 3000.
! real::d_threshold = 2000.
real::d_threshold = 20000.
logical::err_interp
real::dum_interp(5,nz)

real::mch4=16d-3 ! ch4 kg/mol
real::mch2o=30d-3 ! ch2o kg/mol
real::mso4=96d-3  ! so4 kg/mol

real::tol = 1d-6

real,dimension(nflx,nz)::omflx,so4flx,ch4flx,hflx,bflx

integer file_so4flxss,file_ch4flxss,file_hflxss,file_bflxss,file_omflxss
data file_so4flxss,file_ch4flxss,file_hflxss,file_bflxss,file_omflxss/117,118,119,120,121/
integer file_bnd,file_so4flx,file_ch4flx
data file_bnd,file_so4flx,file_ch4flx/122,123,124/

character(100)::flxnames(nflx)
data flxnames/'itflx','iadv','idif','irxn_ah','irxn_ab','irxn_hb','irxn_om','irxn_aom','irxn_dec','ires'/

logical::loc_display =.true.
! logical::loc_display =.false.
logical::first_call = .false.
logical::so4reg = .false.
logical::so4_err = .false.
logical::fastend = .false.
logical::dickens = .false.
logical::limsed =.false.
logical::chknohb =.false.
logical::bubble_only =.true.


call cpu_time(start_time)


! parameters to make grid 
beta = 1.5d0 ! for up concentrating grid 
! beta = 50d0 ! for mid concentrating grid 
clsp=100
clst_type='up'
! clst_type='mid'
! clst_type='upmid'
clst_type='two_x5'
! clst_type='two_x8'
! clst_type='default'
! clst_type='regular'

! write(indir,*) './'
! write(outdir,*) '../hydrate_output/'
indir = trim(par_indir_name)
outdir = trim(par_outdir_name)//'/hydrate/'

write(method,*) "maekawa"
write(method,*) "davie"
write(method,*) "duan"
method = trim(method)

call get_par_defs(                      &! all output
    geotherm,wdpth,wtemp,xis,sal,       &
    rho_f,rho_h,rho_b,rho_s,grav,p0,    &
    poro_0,poro_scale,                  &
    v_sed,u_ext,cm_ext,                 &
    frac,org_0,                         &
    dec_s0,dec_m0,zs,                   &
    dif_m_0,dif_s_0,dif_c_0,            &
    ch,cb,rxn,rxn_h,rxn_b,              &
    ucnv,cs_0,seddep,                   &
    rxn_aom_0,cm_0,                     &
    nz                                  &! input
    )

geotherm = geotherm_in
wdpth = wdpth_in
wtemp = wtemp_in - const_zeroC
sal = sal_in/58.44d0  ! g/kg to mol/kg
xis = sal/(1000d0/18d0) ! mole fraction assuming salt is all NaCl
v_sed = v_sed_in/(2.65d0*(1d0-0.67d0))*1d3 ! converting g/cm2/yr to cm/kyr where porosity 0.67 and solid density 2.65 is assumed 
so4s = so4s_in
cs_0 = so4s*1d-3  ! converting mM to M
cm_0 = cm_0_in
org_0 = org_0_in
loc_margin = margin_in

if (loc_display) then
    print '(11(3x,A9))', 'wdpth', 'wtemp', 'org_0', 'frac', 'v_sed', 'u_ext', 'geotherm', 'dec_m0', 'so4s', 'sal', 'seddep'
    print '(11(3x,e9.3e2))', wdpth, wtemp, org_0, frac, v_sed, u_ext, geotherm, dec_m0, so4s, sal, seddep

    print '(7(3x,A9))', 'zs', 'rho_f', 'rho_h', 'rho_s', 'rho_b', 'pore_scale', 'dec_s0'
    print '(7(3x,e9.3e2))', zs, rho_f, rho_h, rho_s, rho_b, poro_scale,dec_s0
endif 

print*,dum_i,dum_j

lonint = dum_j
latint = dum_i
write(lonch,'(I3.3)') lonint
write(latch,'(I3.3)') latint
outfilename = trim(adjustl(latch))//'-'//trim(adjustl(lonch))

poro_ext = poro_0*exp(-1d0)
org_0 = org_0*frac

clss = 1  !  when assuminng steady state and omit terms relevant to methane
clrxn = 0  ! 1 when using phi_hb 
clgrad = 1-clrxn ! 1 when using gradient of v*poro*hb instead of phi_hb

ceqav = 0d0
ceqmx = 0d0
ceqdh = 0d0
ceqdb = 0d0
ceqh = 0d0
ceqh = 0d0

! if(wdpth<=175d0)then
    ! mhinv_map(dum_i,dum_j) = -1.
    ! mbinv_map(dum_i,dum_j) = -1.
    ! ch4flx_map(dum_i,dum_j) = -1.
    ! so4flx_map(dum_i,dum_j) = -1.
    ! heatflow_map(dum_i,dum_j) = -1.
    ! return
! endif 

! if(wdpth > d_threshold) then
if(loc_margin <= 0d0) then
! if(.not.(dum_i==50 .and. dum_j==3))then
! if(.not.(dum_i==34 .and. dum_j==5))then
    print*,'exit the hydrate calculation as depth is deeper than threshold depth:',d_threshold,' at',dum_i,dum_j
    mhinv_map(dum_i,dum_j) = 0.
    mbinv_map(dum_i,dum_j) = 0.
    ch4flx_map(dum_i,dum_j) = 0.
    so4flx_map(dum_i,dum_j) = 0.
    heatflow_map(dum_i,dum_j) = geotherm_in*1.5*1000.
    depth_map(dum_i,dum_j) = wdpth
    om_map(dum_i,dum_j) = org_0/frac
    btso4_map(dum_i,dum_j) = so4s
    temp_map(dum_i,dum_j) = wtemp
    sedv_map(dum_i,dum_j) = v_sed
    sal_map(dum_i,dum_j) = sal*58.44d0
    margin_map(dum_i,dum_j) = loc_margin
    return
endif
    
call system('mkdir -p '//trim(adjustl(outdir))//'profiles/'//trim(adjustl(outfilename)))
call system('mkdir -p '//trim(adjustl(outdir))//'res')

call checkfile(trim(adjustl(outdir))//'/profiles/'//trim(adjustl(outfilename))&
    //'/org.txt',first_call)

if(first_call)irec_hydrate = 0

if(loc_display)print*,'first call? A: ',first_call
! pause
if(.not.first_call)then
    call readprofiles(                                                                                          &
        file_om,z,dz,org,phi_s,phi_m,file_ch4,cm,ceq,h,b,u,v,rxn_h,rxn_b,cc,rho_f,cs,cbr,ci,nz,file_anion,         &
        outdir,outfilename                                                                                      &
        )
    cm = cm/(1e3*mch4/rho_f)
    cs = cs/1e3
        
    dumflx = 0d0
    call display_prof(                              &
        nz,nflx,                                    &
        ch,cb,rho_h,rho_b,mch4,100000.,100000.,     &
        z,cm,cs,h,b,dz,                             &
        dumflx,dumflx,dumflx,dumflx,dumflx,         &
        flxnames                                    &
        )
    ! pause
endif

call openfiles(                                                         &
    file_om,file_ch4,file_anion,file_ch4flx,file_so4flx,file_bnd,       &
    file_so4flxss,file_ch4flxss,file_hflxss,file_bflxss,file_omflxss,   &
    outdir,outfilename                                                  &
    )
    
if(loc_display)then
    print*,'going to calculate thermodynmaics for the first time'
endif 

if(.not.first_call) z_prev = z

call calctherm(                                     &
    wtemp,geotherm,grav,rho_f,xis,wdpth,sal,        &! input
    clst_type,nz,beta,seddep,                       &! input 
    method,latint,lonint,indir,define_z,            &! input
    limsed,bubble_only,loc_display,                 &! input  
    dz,z,p3,temp3,z3,zmax,ceq,hszflg                &! output   
    )

if(.not.first_call)then
    z_err = maxval(abs((z - z_prev)/z_prev))
    if (z_err>tol) then 
        do iz=1,nz
            if (z(iz)>=z_prev(1)) then 
                iz_min = iz
                exit 
            endif 
        enddo 
        do iz=nz,1,-1
            if (z(iz)<=z_prev(nz)) then 
                iz_max = iz
                exit 
            endif 
        enddo 
        nz_interp = iz_max - iz_min + 1
        do iz=iz_min,iz_max
            call interp1d(nz,org,z_prev,z(iz),dum_interp(1,iz),err_interp)
            call interp1d(nz,cm,z_prev,z(iz),dum_interp(2,iz),err_interp)
            call interp1d(nz,cs,z_prev,z(iz),dum_interp(3,iz),err_interp)
            call interp1d(nz,h,z_prev,z(iz),dum_interp(4,iz),err_interp)
            call interp1d(nz,b,z_prev,z(iz),dum_interp(5,iz),err_interp)
        enddo 
        org(iz_min:iz_max) = dum_interp(1,iz_min:iz_max)
        cm(iz_min:iz_max) = dum_interp(2,iz_min:iz_max)
        cs(iz_min:iz_max) = dum_interp(3,iz_min:iz_max)
        h(iz_min:iz_max) = dum_interp(4,iz_min:iz_max)
        b(iz_min:iz_max) = dum_interp(5,iz_min:iz_max)
        
        if(iz_min/=1) then
            do iz=iz_min-1,1,-1
                org(iz) = org(iz+1) - (org(iz+2)-org(iz+1))/(0.5d0*(dz(iz+2)+dz(iz+1)))*(0.5d0*(dz(iz+1)+dz(iz)))
                cm(iz) = cm(iz+1) - (cm(iz+2)-cm(iz+1))/(0.5d0*(dz(iz+2)+dz(iz+1)))*(0.5d0*(dz(iz+1)+dz(iz)))
                cs(iz) = cs(iz+1) - (cs(iz+2)-cs(iz+1))/(0.5d0*(dz(iz+2)+dz(iz+1)))*(0.5d0*(dz(iz+1)+dz(iz)))
                h(iz) = h(iz+1) - (h(iz+2)-h(iz+1))/(0.5d0*(dz(iz+2)+dz(iz+1)))*(0.5d0*(dz(iz+1)+dz(iz)))
                b(iz) = b(iz+1) - (b(iz+2)-b(iz+1))/(0.5d0*(dz(iz+2)+dz(iz+1)))*(0.5d0*(dz(iz+1)+dz(iz)))
            enddo
        endif 
        
        if(iz_max/=nz) then
            do iz=iz_max-1,nz
                org(iz) = org(iz-1) + (org(iz-1) - org(iz-2))/(0.5d0*(dz(iz-2)+dz(iz-1)))*(0.5d0*(dz(iz-1)+dz(iz)))
                cm(iz) = cm(iz-1) + (cm(iz-1) - cm(iz-2))/(0.5d0*(dz(iz-2)+dz(iz-1)))*(0.5d0*(dz(iz-1)+dz(iz)))
                cs(iz) = cs(iz-1) + (cs(iz-1) - cs(iz-2))/(0.5d0*(dz(iz-2)+dz(iz-1)))*(0.5d0*(dz(iz-1)+dz(iz)))
                h(iz) = h(iz-1) + (h(iz-1) - h(iz-2))/(0.5d0*(dz(iz-2)+dz(iz-1)))*(0.5d0*(dz(iz-1)+dz(iz)))
                b(iz) = b(iz-1) + (b(iz-1) - b(iz-2))/(0.5d0*(dz(iz-2)+dz(iz-1)))*(0.5d0*(dz(iz-1)+dz(iz)))
            enddo
        endif 
        
        dumflx = 0d0
        call display_prof(                              &
            nz,nflx,                                    &
            ch,cb,rho_h,rho_b,mch4,100000.,100000.,     &
            z,cm,cs,h,b,dz,                             &
            dumflx,dumflx,dumflx,dumflx,dumflx,         &
            flxnames                                    &
            )
        ! pause
    endif 
endif 

if(limsed)then
    if (seddep <= zs) then 
    ! #ifdef SO4reg
        ! exit
    ! #endif
        print *, '... too thin sediment'
        call closefiles(                                                        &
            file_om,file_ch4,                                                   &
            file_anion,file_ch4flx,file_so4flx,file_bnd,                        &
            file_so4flxss,file_ch4flxss,file_hflxss,file_bflxss,file_omflxss    &
            )
        stop
    endif
endif

ceqav = sum(ceq(1:nz))/nz*rho_f/16d0/1d-3 ! conversion from g CH4/g l to mM 
ceqmx = ceq(int(0.5d0*nz))*rho_f/16d0/1d-3
ceqdh = ceq(1)*rho_f/16d0/1d-3-ceqmx
ceqdb = ceq(nz)*rho_f/16d0/1d-3-ceqmx
ceqh = sum(ceq(1:int(0.5d0*nz)))/int(0.5d0*nz)*rho_f/16d0/1d-3
ceqb = sum(ceq(int(0.5d0*nz+1):nz))/int(0.5d0*nz)*rho_f/16d0/1d-3

if(bubble_only)then
    if (hszflg == 2) then 
        ceqmx = 0d0
        ceqdh = 0d0
        ceqdb = 0d0
        ceqh = 0d0
        ceqb = ceqav
    endif
endif
 
! dec_s0 = 1d-13
! dif_s_0 = (157.68d0 + 7.88d0*wtemp)*1d-2

dif_m = dif_m_0
dif_s = dif_s_0
dif_c = dif_c_0
 
poro(:) = poro_0*exp(-z(:)/poro_scale)
v(:) = (1d0-poro_0)*v_sed/(1d0-poro(:))
sedtemp(:) = wtemp + geotherm*z(:)
v = v*1d-2/1d3/365d0/24d0/60d0/60d0  ! m/s

if(limsed)then
    if (zmax < seddep) then
        poro_ext = poro_0*exp(-1d0)
    else 
        poro_ext = poro_0*exp(-(zmax)/poro_scale)
    endif
endif

! #ifdef SO4reg
itrso4 = 1
nsmpl = 5
itrmaxso4 = 2
so4_err = .false.
if(so4reg)itrmaxso4 = 5
sulfate: do while (itrso4 < itrmaxso4)
    if(loc_display) print'(3x,a9,3x,i5,3x,a9,3x,e9.3e2)','itrso4=', itrso4, 'zs=', zs
! #endif

    phi_m = 0d0
    phi_s = 0d0
    bioprod = 0d0
    sred = 0d0
    dec_s = dec_s0
    dec_m = dec_m0
    
    if (dickens) dec_m(:) = 1d-14*exp(-110d0/8.314d-3*(1d0/(sedtemp(:)+273.15d0)-1d0/(3d0+273.15d0)))
    
    do iz = 1, nz  ! calculation of organic matter conc. and associated methane production and sulfate reduction 
        if (z(iz) < zs) then 
            if (iz == 1) then 
                org(iz) = v(iz)*org_0*1d-2/dz(iz)/(v(iz)/dz(iz) +dec_s(iz))
            else 
                org(iz) = v(iz)*org(iz-1)/dz(iz)/(v(iz)/dz(iz) +dec_s(iz))
            end if 
            phi_s(iz) = -org(iz)*dec_s(iz)*96d0*rho_s*(1d0-poro(iz))/30d0/2d0/rho_f
        else 
            if (iz == 1) then 
                org(iz) = v(iz)*org_0*1d-2/dz(iz)/(v(iz)/dz(iz) +dec_m(iz))
            else 
                org(iz) = v(iz)*org(iz-1)/dz(iz)/(v(iz)/dz(iz) +dec_m(iz))
            end if 
            phi_m(iz) = org(iz)*dec_m(iz)*16d0*rho_s*(1d0-poro(iz))/30d0/rho_f
        end if 
        bioprod = bioprod + phi_m(iz)*dz(iz)
        sred = sred + phi_s(iz)*dz(iz)
    end do 

    nmx = 0
    ifmx = nz
    ! do iz = 1, nz  ! counting the grids where methane conc. >0 
        ! if (z(iz) < zs) cycle
        ! nmx = nmx + 1
        ! if (nmx == 1) imx = iz  ! imx; upper most grid
        ! if (iz/=1) then 
            ! if ((z(iz-1)-z3)*(z(iz)-z3)<=0d0) ifmx = iz - 1 ! ifmx; interface grid
        ! endif
    ! end do 
    ! 2/11/2020 replaced 
    do iz = 1, nz  ! counting the grids where methane conc. >0 
        if (z(iz) < zs) cycle
        nmx = nmx + 1
        if (nmx == 1) imx = iz  ! imx; upper most grid
    enddo
    do iz=1,nz 
        ! if (z(iz)<zs) imx = iz
        if (iz/=1) then 
            if ((z(iz-1)-z3)*(z(iz)-z3)<=0d0) ifmx = iz - 1 ! ifmx; interface grid
        endif 
    enddo 
    if (bubble_only) then 
        if (hszflg == 2) ifmx = 1
    endif
    if (loc_display) then
        print '(3(3x,a5))', 'imx','ifmx', 'nmx'
        print '(3(3x,i5))', imx,ifmx, nmx
        print '(3(3x,a9))', 'z(imx)', 'z(ifmx)', 'z(nz)'
        print '(3(3x,e9.3e2))', z(imx), z(ifmx), z(nz)
        print '(2(3x,a9))','zs', 'z3'
        print '(2(3x,e9.3e2))',zs, z3
    endif
    
    dt = 1d0*365d0*24d0*60d0*60d0 ! 1yr
    dt = 10d20*365d0*24d0*60d0*60d0 ! 10^20 yr

    if (chknohb) then 
    ! following is done only when checking the case without forming any hydrates and bubbles
        call calcCH4sys_nohb(   &
            ceq,nz,imx,ifmx,itrnewmax,tor3,outdir,runid,latint,lonint,zs,rho_h,rho_b,ch,cb,dif_m,poro  &
            ,poro_ext,v_sed,u_ext,dz,outfilename,poro_0,procn,rho_f,phi_m,z,v,dt,loc_display  &
            ,flg_err  &
            )
        if (flg_err) then 
            write(*,*) 'ERROR DETACTED after calcCH4sys_nohb',latint, lonint, zs
            stop
        endif 
    endif

    if (zmax >= z3) then  ! the case where 3 phases are involved (whether or not sediment thickness is taken into account, i.e., #ifdef or #ifndef limsed)
    
        call calcCH4sys_3phs(   &
            ceq,nz,imx,ifmx,itrnewmax,tor3,outdir,runid,latint,lonint,zs,rho_h,rho_b,ch,cb,dif_m,poro  &
            ,poro_ext,v_sed,u_ext,dz,outfilename,poro_0,procn,rho_f,phi_m,z,v  &
            ,itrmax,maxrate,rxn_bh_max,rxn_hb_max,ucnv,dif_s,dt,loc_display  &
            ,cm,h,b,u,itr,hb,rxn_h,rxn_b,err3,sdif_bot,phi_hb,hx,flg_err,aomflx_ch4            & ! output
            )
        ! call calcCH4sys_3phs_v2(   &
            ! ceq,nz,imx,ifmx,itrnewmax,tor3,outdir,runid,latint,lonint,zs,rho_h,rho_b,ch,cb,dif_m,poro  &
            ! ,poro_ext,v_sed,v_ext,dz,outfilename,poro_0,procn,rho_f,phi_m,z,v  &
            ! ,itrmax,maxrate,rxn_bh_max,rxn_hb_max,ucnv,dif_s,dt  &
            ! ,cm,h,b,u,itr,hb,rxn_h,rxn_b,err3,sdif_bot,phi_hb,hx,flg_err,aomflx_ch4            & ! output
            ! )
        if (flg_err) then 
            write(*,*) 'ERROR DETACTED after calcCH4sys_3phs',latint, lonint, zs
            stop
        endif 
    
    elseif (zmax < z3) then ! the case where only 2 phases are involved (i.e., no bubbles); 
                         ! possible only when sediment thickness is taken into account, i.e., #ifdef limsed; otherwise, zmax = 2*z3 > z3 > 0
                         ! ifmx (interface grid) = nz (bottom grid) in this case 
         if (.not.limsed) then         
            call calcCH4sys_2phs(   &
                ceq,nz,imx,ifmx,itrnewmax,tor3,outdir,runid,latint,lonint,zs,rho_h,rho_b,ch,cb,dif_m,poro  &
                ,poro_ext,v_sed,u_ext,dz,outfilename,poro_0,procn,rho_f,phi_m,z,v  &
                ,itrmax,maxrate,rxn_bh_max,rxn_hb_max,ucnv,dif_s,dt,loc_display  &
                ,cm,h,b,u,itr,hb,rxn_h,rxn_b,err3,sdif_bot,phi_hb,hx,flg_err,aomflx_ch4            & ! output
                )
            if (flg_err) then 
                write(*,*) 'ERROR DETACTED: after calcCH4sys_2phs',latint, lonint, zs
                stop
            endif 
        endif 
    end if  !!! end of CH4 system calculation 

    ! if (err3 < 1d0) then 
    do iz = 1, nz
        if (hb(iz) < 0d0) hb(iz) = 0d0
        if (h(iz) < 0d0) h(iz) = 0d0
        if (b(iz) < 0d0) b(iz) = 0d0
    end do 
    ! end if 

    if (itr>=itrmax) then 
        write(file_erritr,*) latint, lonint, err3, zs
    end if 

    if (b(nz)<=0d0) then 
    print *, "... bubble unsaturated at the bottom..."
    write(file_busat,*) latint, lonint, zs
    endif

    ! do iz = ifmx+1, nz ! calculation of fluid velocity 
        ! u(iz) = (1d0-poro_0)*(poro_ext/(1d0-poro_ext) - hb(iz)*poro(iz)/(1d0-poro(iz))) *v_sed+poro_ext*v_ext  ! in cm/kyr
        ! u(iz) = u(iz)*1d-2/1d3/365d0/24d0/60d0/60d0
    ! end do 

    do itrcns = 3, 1, -1

        select case(itrcns)
            case(1)  ! Cl calculation 
                cc_0 = 555d0  ! mM
                cc_ext = 510d0
                ! print *, "case",itrcns, cc_0, cc_ext
                cc_0 = cc_0/rho_f*35.453d0*1d-3  ! conversion from mM to g Cl/g l
                cc_ext = cc_ext/rho_f*35.453d0*1d-3  ! conversion from mM to g Cl/g l

                cc = cc_0

            case(2)  !  Br calculation 

                cc_0 = 1d0  ! mM
                cc_ext = 3.2d0
                ! print *, "case",itrcns, cc_0, cc_ext
                cc_0 = cc_0/rho_f*79.904d0*1d-3  ! conversion from mM to g Cl/g l
                cc_ext = cc_ext/rho_f*79.904d0*1d-3  ! conversion from mM to g Cl/g l

                cc = cc_0

            case(3)  ! I calculation 

                cc_0 = 0d0  ! mM
                cc_ext = 1.8d0
                ! print *, "case",itrcns, cc_0, cc_ext
                cc_0 = cc_0/rho_f*126.90d0*1d-3  ! conversion from mM to g Cl/g l
                cc_ext = cc_ext/rho_f*126.90d0*1d-3  ! conversion from mM to g Cl/g l

                cc = cc_0

        end select 

        call calcCnsvs(   &
            itrcns,clss,clrxn,clgrad,nz,ifmx,itrnewmax,tor3,outdir,runid,latint,lonint,zs,rho_h,rho_b,ch,cb,dif_c,poro  &
            ,dz,procn,rho_f,v,u,hb,cc_0,cc_ext,dt,phi_hb,z,loc_display   &
            ,err3,cc,ccx,flg_err            & ! output
            ) 
        if (flg_err) write(*,*) latint, lonint, zs

        select case(itrcns)
            case(2)  ! Br calculation
                cbr = cc
            case(3)
                ci = cc
        end select

    end do 

 
    sdif_bot = -sdif_bot *dif_s(imx)*poro(imx)
    do iz = imx-1,2,-1
        ! cs(iz) = 0d0
        ! u(iz)*sdif_top = (sdif_bot - sdif_top)/dz + phi_s(iz) 
        ! u(iz)*sdif_top*dz = sdif_bot - sdif_top + phi_s(iz)*dz
        ! (1d0+u(iz)*dz)*sdif_top = sdif_bot - phi_s(iz)*dz
        sdif_top = (sdif_bot - phi_s(iz)*dz(iz))/(1d0+u(iz)*dz(iz))
        ! sdif_top = dif_s*poro(iz-1)*(csx(iz)-csx(iz-1))/dz
        ! sdif_top*dz/dif_s/poro(iz-1) = csx(iz)-csx(iz-1)
        csx(iz-1) = csx(iz) - sdif_top*dz(iz)/dif_s(Iz)/poro(iz-1)
        sdif_bot = sdif_top
    end do 

    sdif_top = (sdif_bot - phi_s(1)*dz(1))/(1d0+u(1)*dz(1))
    cs_0 = csx(1) - sdif_top*dz(1)/dif_s(1)/poro_0

! #ifdef SO4reg

    if (fastend) then 
        if (itrso4==1 .and. max(0d0,maxval(h)) == 0d0 .and. max(0d0,maxval(b)) == 0d0) then 
            exit
        endif
    endif
    if (imx/=1) then 
  
        nsmpl = min(imx,nsmpl)

        avx = sum(csx(max(1,imx-nsmpl+1):imx))/nsmpl
        avy = sum(z(max(1,imx-nsmpl+1):imx))/nsmpl
        sumx2 = sum((csx(max(1,imx-nsmpl+1):imx)-avx)**2.0d0)
        sumy2 = sum((z(max(1,imx-nsmpl+1):imx)-avy)**2.0d0)
        sumxy = sum((csx(max(1,imx-nsmpl+1):imx)-avx)*(z(max(1,imx-nsmpl+1):imx)-avy))

        slp = sumxy/sumx2
        itcpt = avy - slp*avx
        crr = sumxy**2.0d0/sumx2/sumy2

        avx_up = sum(csx(1:nsmpl))/nsmpl
        avy_up = sum(z(1:nsmpl))/nsmpl
        sumx2_up = sum((csx(1:nsmpl)-avx)**2.0d0)
        sumy2_up = sum((z(1:nsmpl)-avy)**2.0d0)
        sumxy_up = sum((csx(1:nsmpl)-avx)*(z(1:nsmpl)-avy))

        slp_up = sumxy/sumx2
        itcpt_up = avy - slp*avx
        crr_up = sumxy**2.0d0/sumx2/sumy2

        xa = -itcpt_up/slp_up
        ya = itcpt 

        q2 = slp
        q3 = ya
        q1 = -(q2*xa+q3)/(xa**2d0)

        delx = xa - so4s/(rho_f/96d0/1d-3)

        zsnew = q3 + q1*delx**2d0 + q2*delx
        so4new = xa*rho_f/96d0/1d-3

        so4new = -itcpt/slp*rho_f/96d0/1d-3  ! just using a linear regression
        zsnew = -slp*so4s/(rho_f/96d0/1d-3)  ! just using a linear regression

        so4new = cs_0*rho_f/96d0/1d-3   ! calc only with cs_0 and zs 
        zsnew = so4s/so4new*zs
        if (loc_display) then 
            print '(2(3x,a9))','so4new', 'zsnew'
            print '(2(3x,e9.3e2))',so4new, zsnew
        endif
        if (so4new >= so4s*0.90d0 .and. so4new <= so4s*1.10d0) then 
            exit
        else if (so4new < so4s*0.90d0) then 
            ! zs = (zs+zsnew)*0.5d0
            zs = zsnew
            if (zs <dz(1)) then 
                print *, 'no sulfate ', imx
                ! stop
                so4_err = .true.
                exit
            end if 
            ! zs = itcpt
            ! v_ext = v_ext*max(1.5d0,(so4s/so4new*0.95d0)**5.0d0)
            ! dec_m = dec_m*max(1.1d0,(so4s/so4new*0.95d0)**5.0d0)
        else if (so4new > so4s*1.10d0) then 
            ! zs = (zs+zsnew)*0.5d0
            zs = zsnew
            if (zs <dz(1)) then 
                print *, 'no sulfate ',imx
                so4_err = .true.
                ! stop
                exit 
            end if 
            ! zs = itcpt
            ! v_ext = v_ext*min(0.5d0,(so4s/so4new*1.05d0)**5.0d0)
            ! dec_m = dec_m*min(0.90d0,(so4s/so4new*1.05d0)**5.0d0)
        end if 

    else if (imx==1) then 
        if (itrso4 == 1) then 
            sdif_top = (sdif_bot - phi_s(1)*dz(1))/(1d0+u(1)*dz(1))
            so4new = csx(1) - sdif_top*dz(1)/dif_s(1)/poro_0
            zsnew = -so4s/(rho_f/96d0/1d-3)/(sdif_top/dif_s(1)/poro_0)
            so4new = so4new*rho_f/96d0/1d-3
            if (loc_display) then 
                ! print *,so4new, zsnew
                print '(2(3x,a9))','so4new', 'zsnew'
                print '(2(3x,e9.3e2))',so4new, zsnew
            endif
            if (so4new >= so4s*0.90d0 .and. so4new <= so4s*1.10d0) then 
                exit
            else if (so4new < so4s*0.90d0) then 
                ! zs = (zs+zsnew)*0.5d0
                zs = zsnew
                if (zs <dz(1)) then 
                    print *, 'no sulfate ', imx
                    so4_err = .true.
                    exit 
                    ! stop
                end if 
                ! zs = itcpt
                ! v_ext = v_ext*max(1.5d0,(so4s/so4new*0.95d0)**5.0d0)
                ! dec_m = dec_m*max(1.1d0,(so4s/so4new*0.95d0)**5.0d0)
            else if (so4new > so4s*1.10d0) then 
                ! zs = (zs+zsnew)*0.5d0
                zs = zsnew
                if (zs <dz(1)) then 
                    print *, 'no sulfate ',imx
                    so4_err = .true.
                    exit 
                    ! stop
                end if 
                ! zs = itcpt
                ! v_ext = v_ext*min(0.5d0,(so4s/so4new*1.05d0)**5.0d0)
                ! dec_m = dec_m*min(0.90d0,(so4s/so4new*1.05d0)**5.0d0)
            end if 
        elseif (itrso4 /= 1) then
            exit
        endif
    end if

    if (sdif_bot==0d0) exit

    itrso4 = itrso4+1

enddo sulfate

if (itrso4 >= itrmaxso4) then 
    write(*,*) 'SO4 iteration > max',latint, lonint, zs, zsnew, so4s, so4new
end if 

! #endif

call recordprofiles(                                                                                                            &
    file_om,z,dz,org,phi_s,phi_m,file_ch4,cm,ceq,h,b,u,v,rxn_h,rxn_b,ccx,rho_f,hx,csx*1e3,cbr,ci,nz,file_anion    &
    )
call recordfluxes(                                                                      &
    nflx,nz,flxnames,omflx,so4flx,ch4flx,hflx*(ch*rho_h/mch4),bflx*(cb*rho_b/mch4),z,   &
    file_so4flxss,file_ch4flxss,file_hflxss,file_bflxss,file_omflxss                    &
    )
        
mhinv = 0d0
mbinv = 0d0
maxmh = 0d0
maxmb = 0d0
avemh = 0d0
avemb = 0d0
cnth = 0
cntb = 0

do iz = 1,nz
    mhinv=mhinv+poro(iz)*h(iz)*rho_h*ch*dz(iz)
    mbinv=mbinv+poro(iz)*b(iz)*rho_b*cb*dz(iz)
    if (h(iz) > 0d0) then 
        maxmh = max(maxmh,h(iz))
        avemh = avemh + h(iz)
        cnth = cnth + 1
    endif 
    if (b(iz) > 0d0) then 
        maxmb = max(maxmb,b(iz))
        avemb = avemb + b(iz)
        cntb = cntb + 1
    endif
enddo
if (cnth>0) avemh = avemh/cnth
if (cntb>0) avemb = avemb/cntb

mhinv_map(dum_i,dum_j) = mhinv 
mbinv_map(dum_i,dum_j) = mbinv
ch4flx_map(dum_i,dum_j) = aomflx_ch4
so4flx_map(dum_i,dum_j) = sdif_bot
heatflow_map(dum_i,dum_j) = geotherm_in*1.5*1000.
depth_map(dum_i,dum_j) = wdpth
om_map(dum_i,dum_j) = org_0/frac
btso4_map(dum_i,dum_j) = so4s
temp_map(dum_i,dum_j) = wtemp
sedv_map(dum_i,dum_j) = v_sed
sal_map(dum_i,dum_j) = sal*58.44d0
margin_map(dum_i,dum_j) = loc_margin

call closefiles(                                                        &
    file_om,file_ch4,                                                   &
    file_anion,file_ch4flx,file_so4flx,file_bnd,                        &
    file_so4flxss,file_ch4flxss,file_hflxss,file_bflxss,file_omflxss    &
    )

call cpu_time(finish_time)
write(*,'(f10.3,A)')finish_time-start_time,"[CPU sec]"

! pause

endsubroutine hydrate_main_ss
!**************************************************************************************************************************************

!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$  GLOBAL USE  $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

!**************************************************************************************************************************************
subroutine interp_muds(o2in,zin,sedout,omout)
implicit none
real,intent(in)::o2in,zin
real,intent(out)::sedout,omout

integer::nz,no2
parameter(nz=32,no2=40)
real::fz(nz),fo2(no2),fsed(nz,no2),fom(nz,no2)
character(100)t_data

t_data='sed'
call get_muds_data(t_data,fz,fo2,fsed)
t_data='org'
call get_muds_data(t_data,fz,fo2,fom)
! write(*,*)'Enter z[m],o2[uM]'
! read(*,*)zin,o2in
! write(*,*)zin,o2in
call interp2d(no2,nz,o2in,zin,fo2,fz,fsed,sedout)
call interp2d(no2,nz,o2in,zin,fo2,fz,fom,omout)
! write(*,*)'Output:sed,om'
! write(*,*)sedout,omout

endsubroutine interp_muds
!**************************************************************************************************************************************

!**************************************************************************************************************************************
subroutine interp2d(ny,nx,yin0,xin0,fy,fx,fz,zout)
implicit none 
integer,intent(in)::ny,nx
real,intent(in)::yin0,xin0,fy(ny),fx(nx),fz(nx,ny)

real,intent(out)::zout

integer ix,iy,ix0,ix1,iy0,iy1
real::fx0,fx1,fy0,fy1,fz00,fz01,fz10,fz11,fzx0,fzx1,xin,yin 

xin=xin0
yin=yin0


if (xin<fx(1).or.xin>fx(nx).or.yin<fy(1).or.yin>fy(ny)) then 
    print*,'****ERROR: need to extrapolate >>>> return'
    print*,'xin= ',xin,' too small? ',xin<fx(1),' too large? ',xin>fx(nx)
    print*,'yin= ',yin,' too small? ',yin<fy(1),' too large? ',yin>fy(ny)
    if(xin<fx(1))xin=fx(1)
    if(xin>fx(nx))xin=fx(nx)
    if(yin<fy(1))yin=fy(1)
    if(yin>fy(ny))yin=fy(ny)
    ! stop
    ! return
endif 

do ix=1,nx-1
    if (xin>=fx(ix).and.xin<fx(ix+1)) then 
        ix0=ix
        ix1=ix+1
        exit 
    endif 
enddo 

do iy=1,ny-1    
    if (yin>=fy(iy).and.yin<fy(iy+1)) then
        iy0=iy
        iy1=iy+1
        exit 
    endif 
enddo 

fy0=fy(iy0)
fy1=fy(iy1)
fx0=fx(ix0)
fx1=fx(ix1)
fz00=fz(ix0,iy0)
fz01=fz(ix0,iy1)
fz10=fz(ix1,iy0)
fz11=fz(ix1,iy1)
fzx0=(xin-fx0)/(fx1-fx0)*(fz10-fz00)+fz00
fzx1=(xin-fx0)/(fx1-fx0)*(fz11-fz01)+fz01
zout=(yin-fy0)/(fy1-fy0)*(fzx1-fzx0)+fzx0        

endsubroutine interp2d
!**************************************************************************************************************************************

!**************************************************************************************************************************************
subroutine interp1d(n_data,fy,fx,xin,yout,flg_err)
implicit none 
integer,intent(in)::n_data
real,intent(in)::fy(n_data),fx(n_data),xin

real,intent(out)::yout
logical,intent(inout)::flg_err

integer ix,iy,ix0,ix1,iy0,iy1
real::fx0,fx1,fy0,fy1,fz00,fz01,fz10,fz11,fzx0,fzx1 

flg_err = .false.

if (xin<fx(1).or.xin>fx(n_data)) then 
    print*,'****ERROR: need to extrapolate >>>> return'
    flg_err = .true.
    return
endif 

do ix=1,n_data-1
    if (xin>=fx(ix).and.xin<fx(ix+1)) then 
        ix0=ix
        ix1=ix+1
        exit 
    endif 
enddo 

fy0=fy(ix0)
fy1=fy(ix1)
fx0=fx(ix0)
fx1=fx(ix1)
yout=(xin-fx0)/(fx1-fx0)*(fy1-fy0)+fy0        

endsubroutine interp1d
!**************************************************************************************************************************************

!**************************************************************************************************************************************
subroutine get_muds_data(t_data,fz,fo2,fsed)
implicit none
real,intent(out)::fsed(32,40),fz(32),fo2(40)
character(100),intent(in)::t_data

integer iz,nz,no2
real dum
nz=32;no2=40
fsed=0d0;fz=0d0;fo2=0d0
if (trim(t_data)=='sed') open(unit=100,file=trim(par_indir_name)//'/EpslOmega_v2.dat',status='old',action='read')
if (trim(t_data)=='org') open(unit=100,file=trim(par_indir_name)//'/EpslToc_v2.dat',status='old',action='read')
do iz=1,nz+1
    if (iz==1) then 
        read(100,*)dum,fo2(1:no2)
    else 
        read(100,*)fz(iz-1),fsed(iz-1,1:no2)
    endif 
enddo
close(100)
endsubroutine get_muds_data
!**************************************************************************************************************************************

!**************************************************************************************************************************************
subroutine get_heatflow_data_Hamza08(loc_i,loc_j,heatflow)
implicit none
real,intent(out)::heatflow
integer,intent(in)::loc_i,loc_j

integer loc_k
real dum(n_j)

if(n_j==72) open(unit=100,file=trim(par_indir_name)//'/heatflow_Hamza08_origin_72x72.dat',status='old',action='read')
if(n_j==36) open(unit=100,file=trim(par_indir_name)//'/heatflow_Hamza08_origin_36x36.dat',status='old',action='read')
do loc_k=1,loc_j
    read(100,*)dum(:)
enddo

heatflow = dum(loc_i)

close(100)

endsubroutine get_heatflow_data_Hamza08
!**************************************************************************************************************************************

!**************************************************************************************************************************************
subroutine get_margin_mask_Archer09(loc_i,loc_j,margin_mask)
implicit none
real,intent(out)::margin_mask
integer,intent(in)::loc_i,loc_j

integer loc_k
real dum(n_j)

if(n_j==72) open(unit=100,file=trim(par_indir_name)//'/margin_Archer2009_72x72.dat',status='old',action='read')
if(n_j==36) open(unit=100,file=trim(par_indir_name)//'/margin_Archer2009_36x36.dat',status='old',action='read')
do loc_k=1,loc_j
    read(100,*)dum(:)
enddo

margin_mask = dum(loc_i)

close(100)

endsubroutine get_margin_mask_Archer09
!**************************************************************************************************************************************

!**************************************************************************************************************************************
subroutine get_sed_thick_Laske97(loc_i,loc_j,sed_thick)
implicit none
real,intent(out)::sed_thick
integer,intent(in)::loc_i,loc_j

integer loc_k
real dum(n_j)

if(n_j==72) open(unit=100,file=trim(par_indir_name)//'/sedthickness_laske97_72x72.dat',status='old',action='read')
if(n_j==36) open(unit=100,file=trim(par_indir_name)//'/sedthickness_laske97_36x36.dat',status='old',action='read')
do loc_k=1,loc_j
    read(100,*)dum(:)
enddo

sed_thick = dum(loc_i)

close(100)

endsubroutine get_sed_thick_Laske97
!**************************************************************************************************************************************

!**************************************************************************************************************************************
subroutine get_bDO_Hunter13(loc_i,loc_j,bDO)
implicit none
real,intent(out)::bDO
integer,intent(in)::loc_i,loc_j

integer loc_k
real dum(n_j)

if(n_j==72) open(unit=100,file=trim(par_indir_name)//'/bDO_Hunter13_72x72.dat',status='old',action='read')
if(n_j==36) open(unit=100,file=trim(par_indir_name)//'/bDO_Hunter13_36x36.dat',status='old',action='read')
do loc_k=1,loc_j
    read(100,*)dum(:)
enddo

bDO = dum(loc_i)

close(100)

endsubroutine get_bDO_Hunter13
!**************************************************************************************************************************************

!**************************************************************************************************************************************
subroutine get_topo_Hunter13(loc_i,loc_j,loc_D)
implicit none
real,intent(out)::loc_D
integer,intent(in)::loc_i,loc_j

integer loc_k
real dum(n_j)

if(n_j==72) open(unit=100,file=trim(par_indir_name)//'/topo_Hunter13_72x72.dat',status='old',action='read')
if(n_j==36) open(unit=100,file=trim(par_indir_name)//'/topo_Hunter13_36x36.dat',status='old',action='read')
do loc_k=1,loc_j
    read(100,*)dum(:)
enddo

loc_D = dum(loc_i)

close(100)

endsubroutine get_topo_Hunter13
!**************************************************************************************************************************************

!**************************************************************************************************************************************
subroutine get_bwt_Hunter13(loc_i,loc_j,loc_T)
implicit none
real,intent(out)::loc_T
integer,intent(in)::loc_i,loc_j

integer loc_k
real dum(n_j)

if(n_j==72) open(unit=100,file=trim(par_indir_name)//'/bwt_Hunter13_72x72.dat',status='old',action='read')
if(n_j==36) open(unit=100,file=trim(par_indir_name)//'/bwt_Hunter13_36x36.dat',status='old',action='read')
do loc_k=1,loc_j
    read(100,*)dum(:)
enddo

loc_T = dum(loc_i)

close(100)

endsubroutine get_bwt_Hunter13
!**************************************************************************************************************************************

!**************************************************************************************************************************************
subroutine sub_save_hydrate()
implicit none

integer i,j
character*256 workdir,intchr
real h_globe,b_globe,ch4flx_globe,ombur_globe,omdec_globe,omprs_globe,so4flx_globe,dicflx_globe,alkflx_globe
real aom_globe,ch4gen_globe,so4red_globe,degas_globe,ch4adv_globe
logical dir_e

! print*, ' printing data by signal tracking model '

workdir = trim(par_outdir_name)
workdir = trim(adjustl(workdir))//'/hydrate/res/'

! inquire(file=trim(adjustl(workdir))//'/hydrate/res/.', exist=dir_e)
! if (.not.dir_e) then 
    ! irec_hydrate = 0
    ! call system('mkdir -p '//trim(adjustl(workdir)))
! endif 

call system('mkdir -p '//trim(adjustl(workdir)))
write(intchr,*)irec_hydrate

h_globe = 0d0
b_globe = 0d0
ch4flx_globe = 0d0
so4flx_globe = 0d0
dicflx_globe = 0d0
alkflx_globe = 0d0
ombur_globe = 0d0
omdec_globe = 0d0
omprs_globe = 0d0
ch4gen_globe = 0d0
ch4adv_globe = 0d0
so4red_globe = 0d0
aom_globe = 0d0
degas_globe = 0d0

do j=1,n_j
    do i=1,n_i
        ! if ((.not.sed_mask(i,j))) then 
        if (phys_sed(ips_D,i,j)<=0d0) then 
            mhinv_map(i,j)    = const_real_null
            mbinv_map(i,j)    = const_real_null
            so4flx_map(i,j)   = const_real_null
            ch4flx_map(i,j)   = const_real_null
            dicflx_map(i,j)   = const_real_null
            alkflx_map(i,j)   = const_real_null
            so4red_map(i,j)   = const_real_null
            ch4adv_map(i,j)   = const_real_null
            ch4gen_map(i,j)   = const_real_null
            degas_map(i,j)    = const_real_null
            aom_map(i,j)      = const_real_null
            zs_map(i,j)       = const_real_null
            hsz_map(i,j)      = const_real_null
            ceqh_map(i,j)     = const_real_null
            ceqb_map(i,j)     = const_real_null
            heatflow_map(i,j) = const_real_null
            sedthick_map(i,j) = const_real_null
            depth_map(i,j)    = const_real_null
            om_map(i,j)       = const_real_null
            omfrc_map(i,j)    = const_real_null
            ombur_map(i,j)    = const_real_null
            omprs_map(i,j)    = const_real_null
            btso4_map(i,j)    = const_real_null
            temp_map(i,j)     = const_real_null
            sedv_map(i,j)     = const_real_null
            sal_map(i,j)      = const_real_null
            margin_map(i,j)   = const_real_null
            h_diss_map(i,j)   = const_real_null
            b_diss_map(i,j)   = const_real_null
        endif 
        if (mhinv_map(i,j)>0d0) h_globe = h_globe + mhinv_map(i,j)*phys_sed(ips_A,i,j)*12d0/16d0*max(0d0,margin_map(i,j))
        if (mbinv_map(i,j)>0d0) b_globe = b_globe + mbinv_map(i,j)*phys_sed(ips_A,i,j)*12d0/16d0*max(0d0,margin_map(i,j))
        if (abs(ch4flx_map(i,j))<abs(const_real_null)) ch4flx_globe = ch4flx_globe + ch4flx_map(i,j)*phys_sed(ips_A,i,j)*max(0d0,margin_map(i,j))
        if (abs(so4flx_map(i,j))<abs(const_real_null)) so4flx_globe = so4flx_globe + so4flx_map(i,j)*phys_sed(ips_A,i,j)*max(0d0,margin_map(i,j))
        if (abs(dicflx_map(i,j))<abs(const_real_null)) dicflx_globe = dicflx_globe + dicflx_map(i,j)*phys_sed(ips_A,i,j)*max(0d0,margin_map(i,j))
        if (abs(alkflx_map(i,j))<abs(const_real_null)) alkflx_globe = alkflx_globe + alkflx_map(i,j)*phys_sed(ips_A,i,j)*max(0d0,margin_map(i,j))
        if (abs(ch4gen_map(i,j))<abs(const_real_null)) ch4gen_globe = ch4gen_globe + ch4gen_map(i,j)*phys_sed(ips_A,i,j)*max(0d0,margin_map(i,j))
        if (abs(ch4adv_map(i,j))<abs(const_real_null)) ch4adv_globe = ch4adv_globe + ch4adv_map(i,j)*phys_sed(ips_A,i,j)*max(0d0,margin_map(i,j))
        if (abs(so4red_map(i,j))<abs(const_real_null)) so4red_globe = so4red_globe + so4red_map(i,j)*phys_sed(ips_A,i,j)*max(0d0,margin_map(i,j))
        if (abs(aom_map(i,j))   <abs(const_real_null)) aom_globe    = aom_globe    + aom_map(i,j)   *phys_sed(ips_A,i,j)*max(0d0,margin_map(i,j))
        if (abs(degas_map(i,j)) <abs(const_real_null)) degas_globe  = degas_globe  + degas_map(i,j) *phys_sed(ips_A,i,j)*max(0d0,margin_map(i,j))
        if (omprs_map(i,j)>0d0) omprs_globe = omprs_globe + omprs_map(i,j)*phys_sed(ips_A,i,j)*max(0d0,margin_map(i,j))
        if (ombur_map(i,j)>0d0) ombur_globe = ombur_globe &
            + par_sed_hydrate_orgCfrac*ombur_map(i,j)*omfrc_map(i,j)*phys_sed(ips_A,i,j)*max(0d0,margin_map(i,j))
    enddo
enddo 
omdec_globe = ombur_globe - omprs_globe

! print*, ' data prepared ... now going to record '
open(unit=100,file=trim(adjustl(workdir))//'mhinv-'//trim(adjustl(intchr))//'.res',action='write',status='unknown')
do j=1,n_j
    write(100,*) (mhinv_map(i,j)*margin_map(i,j)*merge(1d0,-1d0,margin_map(i,j)>=0d0),i=1,n_i)
enddo 
close(100)

open(unit=100,file=trim(adjustl(workdir))//'mbinv-'//trim(adjustl(intchr))//'.res',action='write',status='unknown')
do j=1,n_j
    write(100,*) (mbinv_map(i,j)*margin_map(i,j)*merge(1d0,-1d0,margin_map(i,j)>=0d0),i=1,n_i)
enddo 
close(100)

open(unit=100,file=trim(adjustl(workdir))//'ch4flx-'//trim(adjustl(intchr))//'.res',action='write',status='unknown')
do j=1,n_j
    write(100,*) (ch4flx_map(i,j)*margin_map(i,j)*merge(1d0,-1d0,margin_map(i,j)>=0d0),i=1,n_i)
enddo 
close(100)

open(unit=100,file=trim(adjustl(workdir))//'so4flx-'//trim(adjustl(intchr))//'.res',action='write',status='unknown')
do j=1,n_j
    write(100,*) (so4flx_map(i,j)*margin_map(i,j)*merge(1d0,-1d0,margin_map(i,j)>=0d0),i=1,n_i)
enddo 
close(100)

open(unit=100,file=trim(adjustl(workdir))//'so4red-'//trim(adjustl(intchr))//'.res',action='write',status='unknown')
do j=1,n_j
    write(100,*) (so4red_map(i,j)*margin_map(i,j)*merge(1d0,-1d0,margin_map(i,j)>=0d0),i=1,n_i)
enddo 
close(100)

open(unit=100,file=trim(adjustl(workdir))//'ch4gen-'//trim(adjustl(intchr))//'.res',action='write',status='unknown')
do j=1,n_j
    write(100,*) (ch4gen_map(i,j)*margin_map(i,j)*merge(1d0,-1d0,margin_map(i,j)>=0d0),i=1,n_i)
enddo 
close(100)

open(unit=100,file=trim(adjustl(workdir))//'ch4adv-'//trim(adjustl(intchr))//'.res',action='write',status='unknown')
do j=1,n_j
    write(100,*) (ch4adv_map(i,j)*margin_map(i,j)*merge(1d0,-1d0,margin_map(i,j)>=0d0),i=1,n_i)
enddo 
close(100)

open(unit=100,file=trim(adjustl(workdir))//'degas-'//trim(adjustl(intchr))//'.res',action='write',status='unknown')
do j=1,n_j
    write(100,*) (degas_map(i,j)*margin_map(i,j)*merge(1d0,-1d0,margin_map(i,j)>=0d0),i=1,n_i)
enddo 
close(100)

open(unit=100,file=trim(adjustl(workdir))//'aom-'//trim(adjustl(intchr))//'.res',action='write',status='unknown')
do j=1,n_j
    write(100,*) (aom_map(i,j)*margin_map(i,j)*merge(1d0,-1d0,margin_map(i,j)>=0d0),i=1,n_i)
enddo 
close(100)

open(unit=100,file=trim(adjustl(workdir))//'zs-'//trim(adjustl(intchr))//'.res',action='write',status='unknown')
do j=1,n_j
    write(100,*) (zs_map(i,j),i=1,n_i)
enddo 
close(100)

open(unit=100,file=trim(adjustl(workdir))//'hsz-'//trim(adjustl(intchr))//'.res',action='write',status='unknown')
do j=1,n_j
    write(100,*) (hsz_map(i,j),i=1,n_i)
enddo 
close(100)

open(unit=100,file=trim(adjustl(workdir))//'ceqh-'//trim(adjustl(intchr))//'.res',action='write',status='unknown')
do j=1,n_j
    write(100,*) (ceqh_map(i,j),i=1,n_i)
enddo 
close(100)

open(unit=100,file=trim(adjustl(workdir))//'ceqb-'//trim(adjustl(intchr))//'.res',action='write',status='unknown')
do j=1,n_j
    write(100,*) (ceqb_map(i,j),i=1,n_i)
enddo 
close(100)

open(unit=100,file=trim(adjustl(workdir))//'h_diss-'//trim(adjustl(intchr))//'.res',action='write',status='unknown')
do j=1,n_j
    write(100,*) (h_diss_map(i,j),i=1,n_i)
enddo 
close(100)

open(unit=100,file=trim(adjustl(workdir))//'b_diss-'//trim(adjustl(intchr))//'.res',action='write',status='unknown')
do j=1,n_j
    write(100,*) (b_diss_map(i,j),i=1,n_i)
enddo 
close(100)

open(unit=100,file=trim(adjustl(workdir))//'tot_diss-'//trim(adjustl(intchr))//'.res',action='write',status='unknown')
do j=1,n_j
    write(100,*) (h_diss_map(i,j) + b_diss_map(i,j),i=1,n_i)
enddo 
close(100)

if (irec_hydrate == par_sed_hydrate_savefreq) then 
! if (irec_hydrate == 0) then 
    open(unit=100,file=trim(adjustl(workdir))//'heatflow-'//trim(adjustl(intchr))//'.res',action='write',status='unknown')
    do j=1,n_j
        write(100,*) (heatflow_map(i,j),i=1,n_i)
    enddo 
    close(100)
    open(unit=100,file=trim(adjustl(workdir))//'sedthick-'//trim(adjustl(intchr))//'.res',action='write',status='unknown')
    do j=1,n_j
        write(100,*) (sedthick_map(i,j),i=1,n_i)
    enddo 
    close(100)
    open(unit=100,file=trim(adjustl(workdir))//'margin-'//trim(adjustl(intchr))//'.res',action='write',status='unknown')
    do j=1,n_j
        write(100,*) (margin_map(i,j),i=1,n_i)
    enddo 
    close(100)
    open(unit=100,file=trim(adjustl(workdir))//'depth-'//trim(adjustl(intchr))//'.res',action='write',status='unknown')
    do j=1,n_j
        write(100,*) (depth_map(i,j),i=1,n_i)
    enddo 
    close(100)
    open(unit=100,file=trim(adjustl(workdir))//'om-'//trim(adjustl(intchr))//'.res',action='write',status='unknown')
    do j=1,n_j
        write(100,*) (om_map(i,j),i=1,n_i)
    enddo 
    close(100)
    open(unit=100,file=trim(adjustl(workdir))//'so4-'//trim(adjustl(intchr))//'.res',action='write',status='unknown')
    do j=1,n_j
        write(100,*) (btso4_map(i,j),i=1,n_i)
    enddo 
    close(100)
    open(unit=100,file=trim(adjustl(workdir))//'temp-'//trim(adjustl(intchr))//'.res',action='write',status='unknown')
    do j=1,n_j
        write(100,*) (temp_map(i,j),i=1,n_i)
    enddo 
    close(100)
    open(unit=100,file=trim(adjustl(workdir))//'sedv-'//trim(adjustl(intchr))//'.res',action='write',status='unknown')
    do j=1,n_j
        write(100,*) (sedv_map(i,j),i=1,n_i)
    enddo 
    close(100)
    open(unit=100,file=trim(adjustl(workdir))//'sal-'//trim(adjustl(intchr))//'.res',action='write',status='unknown')
    do j=1,n_j
        write(100,*) (sal_map(i,j),i=1,n_i)
    enddo 
    close(100)
endif 

open(unit=100,file=trim(adjustl(workdir))//'Global_C-'//trim(adjustl(intchr))//'.res',action='write',status='unknown')
write(100,*) 'Global CH4 hydrate   (Pg C): ',h_globe*1d3/1d15
write(100,*) 'Global CH4 bubble    (Pg C): ',b_globe*1d3/1d15
write(100,*) 'Global CH4 total     (Pg C): ',(h_globe+b_globe)*1d3/1d15
write(100,*) '---------------------------'
write(100,*) 'Global CH4 flux            : ', ch4flx_globe*12d0/1d15, ' (Pg C yr-1) = ',ch4flx_globe, ' (mol yr-1)'
write(100,*) 'Global SO4 flux            : ', so4flx_globe*12d0/1d15, ' (Pg C yr-1) = ',so4flx_globe, ' (mol yr-1)'
write(100,*) 'Global DIC flux            : ', dicflx_globe*12d0/1d15, ' (Pg C yr-1) = ',dicflx_globe, ' (mol yr-1)'
write(100,*) 'Global ALK flux            : ', alkflx_globe*12d0/1d15, ' (Pg C yr-1) = ',alkflx_globe, ' (mol yr-1)'
write(100,*) '---------------------------'
write(100,*) 'Global CH4 genesis         : ', ch4gen_globe*12d0/1d15, ' (Pg C yr-1) = ',ch4gen_globe, ' (mol yr-1)'
write(100,*) 'Global SO4 reduction       : ', so4red_globe*12d0/1d15, ' (Pg C yr-1) = ',so4red_globe, ' (mol yr-1)'
write(100,*) 'Global AOM                 : ', aom_globe*12d0/1d15,    ' (Pg C yr-1) = ',aom_globe,    ' (mol yr-1)'
write(100,*) 'Global CH4 degas           : ', degas_globe*12d0/1d15,  ' (Pg C yr-1) = ',degas_globe,  ' (mol yr-1)'
write(100,*) 'Global CH4 adv             : ', ch4adv_globe*12d0/1d15, ' (Pg C yr-1) = ',ch4adv_globe, ' (mol yr-1)'
write(100,*) '---------------------------'
write(100,*) 'Global OM rain             : ', ombur_globe*12d0/1d15,' (Pg C yr-1) = ',ombur_globe, ' (mol yr-1)'
write(100,*) 'Global OM degradation      : ', omdec_globe*12d0/1d15,' (Pg C yr-1) = ',omdec_globe, ' (mol yr-1)'
write(100,*) 'Global OM preservation     : ', omprs_globe*12d0/1d15,' (Pg C yr-1) = ',omprs_globe, ' (mol yr-1)'
write(100,*) 'Preservation ratio     (%) : ', omprs_globe/ombur_globe*1d2
close(100)

! irec_hydrate = irec_hydrate + 1

! pause 

endsubroutine sub_save_hydrate
!**************************************************************************************************************************************

!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$  MISC $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

!**************************************************************************************************************************************
subroutine make_grid(  &
    clst_type,nz,clsp,beta,ztot  &! input 
    ,z,dz         &! output
    )

implicit none 

integer,intent(in)::nz
real,intent(in)::clsp,beta,ztot
real,intent(out)::z(nz),dz(nz)
character(100),intent(in)::clst_type

! local variables 
integer::iz
real::eta(nz),alpha


do iz=1,nz
    eta(iz) = 1d0*iz/(1d0*nz)
enddo
select case (trim(clst_type))
    case('mid')
        alpha = 0.5d0/beta*log((1d0+(exp(beta)-1d0)*clsp/ztot)/(1d0+(exp(-beta)-1d0)*clsp/ztot))
        z = clsp*(1d0+sinh(beta*(eta-alpha))/sinh(beta*alpha))
        dz = z/sum(z)*ztot
    case('up')
        z = ztot*((beta+1d0)-(beta-1d0)*(((beta+1d0)/(beta-1d0))**(1d0-eta)))  &
            /((((beta+1d0)/(beta-1d0))**(1d0-eta))+1d0)
        dz = z/sum(z)*ztot
    case('upmid')
        alpha =0.5d0
        z(1:nz/2) = (ztot*0.5d0*(2d0*alpha+beta)*(((beta+1d0)/(beta-1d0))**((eta(1:nz/2)-alpha)/1d0-alpha))+2d0*alpha-beta)  &
            /(2d0*alpha+1d0)/(1d0+((beta+1d0)/(beta-1d0))**((eta(1:nz/2)-alpha)/(1d0-alpha)))
        z(nz/2+1:) = 0.5d0*ztot*((beta+1d0)-(beta-1d0)*(((beta+1d0)/(beta-1d0))**(1d0-eta(nz/2+1:))))  &
            /((((beta+1d0)/(beta-1d0))**(1d0-eta(nz/2+1:)))+1d0)
        dz = z/sum(z)*ztot
    case('default')
        dz=ztot/nz
    case('two')
        do iz=1,nz
            if (iz<=nz/5)then 
                dz(iz)=1d0-0.99d0*(real(nz/5-iz,kind=8)/real(nz/5,kind=8))**0.5d0
            elseif (iz>nz/5 .and. iz<=nz/5*2)then 
                dz(iz)=1d0
            elseif (iz>nz/5*2 .and. iz<=nz/5*3)then 
                dz(iz)=1d0-0.99d0*(real(iz-nz/5*2,kind=8)/real(nz/5,kind=8))**0.5d0
            elseif (iz>nz/5*3 .and. iz<=nz/5*4)then 
                dz(iz)=1d0-0.99d0*(real(nz/5*4-iz,kind=8)/real(nz/5,kind=8))**0.5d0
            elseif (iz>nz/5*4) then 
                dz(iz)=1d0
            endif 
        enddo 
        dz(:) = dz(:)/sum(dz)*ztot
    case('two_x5')
        do iz=1,nz
            if (iz<=nz/5)then 
                dz(iz)=1d0-0.99d0*(real(nz/5-iz,kind=8)/real(nz/5,kind=8))**0.5d0
            elseif (iz>nz/5 .and. iz<=nz/5*2)then 
                dz(iz)=1d0
            elseif (iz>nz/5*2 .and. iz<=nz/5*3)then 
                dz(iz)=1d0-0.99d0*(real(iz-nz/5*2,kind=8)/real(nz/5,kind=8))**0.5d0
            elseif (iz>nz/5*3 .and. iz<=nz/5*4)then 
                dz(iz)=1d0-0.99d0*(real(nz/5*4-iz,kind=8)/real(nz/5,kind=8))**0.5d0
            elseif (iz>nz/5*4) then 
                dz(iz)=1d0
            endif 
        enddo 
        alpha=sum(dz(1:nz/5*3))
        dz(1:nz/5*3)=dz(1:nz/5*3)*sum(dz(nz/5*3+1:nz))
        dz(nz/5*3+1:nz)=dz(nz/5*3+1:nz)*alpha
        dz(:) = dz(:)/sum(dz)*ztot
    case('two_x8')
        do iz=1,nz
            if (iz<=nz/8*2)then 
                dz(iz)=1d0-0.99d0*(real(nz/8*2-iz,kind=8)/real(nz/4,kind=8))**0.5d0
            elseif (iz>nz/8*2 .and. iz<=nz/8*4)then 
                dz(iz)=1d0
            elseif (iz>nz/8*4 .and. iz<=nz/8*6)then 
                dz(iz)=1d0-0.99d0*(real(iz-nz/8*4,kind=8)/real(nz/4,kind=8))**0.5d0
            elseif (iz>nz/8*6 .and. iz<=nz/8*7)then 
                dz(iz)=1d0-0.99d0*(real(nz/8*7-iz,kind=8)/real(nz/8,kind=8))**0.5d0
            elseif (iz>nz/8*7) then 
                dz(iz)=1d0
            endif 
        enddo 
        alpha=sum(dz(1:nz/8*6))
        dz(1:nz/8*6)=dz(1:nz/8*6)*sum(dz(nz/8*6+1:nz))
        dz(nz/8*6+1:nz)=dz(nz/8*6+1:nz)*alpha
        dz(:) = dz(:)/sum(dz)*ztot
    case('regular')
        do iz = 1, nz  ! assignment of real value to depth 
            z(iz) = iz*ztot/nz
        end do 
        dz(:) = z(2) - z(1)  !  m
        return
endselect 

do iz=1,nz  ! depth is defined at the middle of individual layers 
    if (iz==1) z(iz)=dz(iz)*0.5d0  
    if (iz/=1) z(iz) = z(iz-1)+dz(iz-1)*0.5d0 + 0.5d0*dz(iz)
enddo

! open(unit=250,file='test.txt',action = 'write',status='replace')
! do iz=1,nz
    ! write(250,*) eta(iz),dz(iz),z(iz)
! enddo 
! close(250)
! stop
        
endsubroutine make_grid
!**************************************************************************************************************************************

!**************************************************************************************************************************************
subroutine get_par_defs(                  & ! all output
    geotherm,wdpth,wtemp,xis,sal        &
    ,rho_f,rho_h,rho_b,rho_s,grav,p0     &
    ,poro_0,poro_scale                  &
    ,v_sed,u_ext,cm_ext                 &
    ,frac,org_0                         &
    ,dec_s0,dec_m0,zs                   &
    ,dif_m,dif_s,dif_c                  &
    ,ch,cb,rxn,rxn_h,rxn_b              &
    ,ucnv,cs_0,seddep                   &
    ,rxn_aom_0,cm_0                     &
    ,nz                                 &   ! input 
    )
implicit none 
! #include <defines.h>
integer,intent(in)::nz
real,intent(out)::geotherm,wdpth,wtemp,xis,sal    &
    ,rho_f,rho_h,rho_b,rho_s,grav,p0            &
    ,poro_0,poro_scale                          &
    ,v_sed,u_ext,cm_ext                         &
    ,frac,org_0                                 &
    ,dec_s0,dec_m0,zs                           &
    ,dif_m,dif_s,dif_c                          &
    ,ch,cb,rxn(nz),rxn_h(nz),rxn_b(nz)          &
    ,ucnv,cs_0,seddep,rxn_aom_0,cm_0 

! geotherm = 0.043d0 ! K/m
geotherm = 0.042d0 ! K/m
! geotherm = 65.24d0*1d-3/1.5d0 ! K/m
wdpth = 2781.d0 ! m
! wdpth = 1836.5d0 ! m
! wdpth = 500.d0 ! m
wtemp = 3.d0 ! deg C
! wtemp = 276.06d0 - 273d0 ! deg C
! wtemp = 5.d0 ! deg C
xis = 0.d0  ! mol/mol (Salt(NaCl) concentration; Peszynska et al. (2016); Maekawa et al., 1995)
sal = 0.6d0 ! mol/L   Davie et al. 2003 MG
xis = sal/(1000d0/18d0) ! assuming salt is all NaCl
! rho_f = 1035.d0 ! kg/m3  ! Davie et al. (2003)
rho_f = 1000.d0 ! kg/m3   ! a typical value in Davie and Buffett (2004) 
rho_h = 930.d0 ! kg/m3   ! a typical value in Davie and Buffett (2004) 
rho_s = 2650.d0 ! kg/m3   ! Davie and Buffett (2001) 
rho_b = 200.d0 ! kg/m3   ! Davie and Buffett (2001) 
grav = 9.8d0 ! m/s2
p0 = 0.101d0 ! MPa

poro_0 = 0.69d0
poro_scale = 2000d0 ! m

v_sed = 22d0 ! cm/kyr 
! v_sed = 7.72d-2 * 1d2 ! cm/kyr 
! u_ext = -50d0  ! cm/kyr
u_ext = -30d0  ! cm/kyr  ! Hunter et al. (2013)
! u_ext = -10d0  ! cm/kyr  ! Hunter et al. (2013)
u_ext = par_sed_hydrate_exflow
! #ifdef noextflow
! u_ext = -0d0  ! cm/kyr
! #endif
cm_ext = 0d0 ! mass fraction

frac = 0.25d0
frac = par_sed_hydrate_orgCfrac

org_0 = 1.1d0 ! wt%, dry fraction 
org_0 = 1.5d0 ! wt%, dry fraction 
! org_0 = org_0*frac ! wt%, dry fraction 
! org_0 = 5.79d-1*0.25d0 ! wt%, dry fraction 
! org_0 = 3.1d0 ! wt%, dry fraction 
dec_s0 = 5d-13 ! /s 
dec_s0 = 2d-13 ! /s 
dec_s0 = 0d0 ! /s !  Davie and Buffett 2001
! dec_m = 3d-13 ! /s
! dec_m = 20d-14 ! /s  !  Davie and Buffett 2001
! dec_m = 1.5d-14 ! /s  !  Klauda and Sandler 2005
! dec_m = 3.d-15 ! /s  !  Hunter et al. 2013
dec_m0 = 0.5d-13 ! /s  !  Hunter et al. 2013
zs = 22d0 ! m
zs = 10d0 ! m

dif_m = 0.87d-9 !m2/s
dif_s = 0.56d-9 !m2/s
dif_c = 1.01d-9 !m2/s

ch = 0.134d0 ! methane conc. in hydrate 
cb = 1d0  ! methan conc. in bubble

rxn = 1d-15  ! /s
rxn_b = 1d-15  ! /s
rxn_h = 1d-15  ! /s
ucnv = 1d-2/1d3/365d0/24d0/60d0/60d0

cs_0 = 28d-3 ! conc. SO4 water column (M)

seddep = 12d3  ! m sediment thickness

rxn_aom_0 = 1d0 ! reaction of AOM (1-30 cm3 yr-1 mmol-1; Wallmann et al. GCA 2006
rxn_aom_0 = 1d2 ! reaction of AOM (1-30 cm3 yr-1 mmol-1; Wallmann et al. GCA 2006
cm_0 = 0.5d-9 ! ocean CH4 conc. [mol L-1] (from ~0.5 - 10 nM according to Reeburgh 2007)

endsubroutine get_par_defs
!**************************************************************************************************************************************

!**************************************************************************************************************************************
subroutine count_lines(filename,counter)
implicit none
character(1000),intent(in)::filename
integer,intent(out)::counter

! print *, 'Enter file name to count lines: '
! read '(A)', filename
open(11,file=filename,status='old')
counter = 0
do
    read(11,'()',end=100)
    counter = counter + 1
enddo
100 close(11)
! print *,'Number of lines is',counter
endsubroutine count_lines
!**************************************************************************************************************************************

!**************************************************************************************************************************************
subroutine checkfile(fname,ox) 
! returning .true.  if a file does not exist
! returning .false. if a file does     exist
implicit none
character(*),intent(in)::fname
logical,intent(out)::ox
open(100,file=trim(fname),status='old',err=999)
close(100)
! write(6,'(3A)')"file '",fname,"' exist"
ox=.false.
return
999 continue
close(100)
! write(6,'(3A)')"file '",fname,"' don't exist"
ox=.true.
return
end subroutine checkfile
!**************************************************************************************************************************************

!**************************************************************************************************************************************
subroutine readprofiles(                                                                                    &
    file_om,z,dz,org,phi_s,phi_m,file_ch4,cm,ceq,h,b,u,v,rxn_h,rxn_b,cc,rho_f,cs,cbr,ci,nz,file_anion,      &
    outdir,outfilename                                                                                      &
    )
implicit none 
integer,intent(in)::file_om,file_ch4,nz,file_anion
real,dimension(nz),intent(inout)::z,dz,org,phi_s,phi_m,cm,ceq,h,b,u,v,rxn_h,rxn_b,cc,cs,cbr,ci
real,intent(in)::rho_f
character(100),intent(in)::outdir,outfilename

integer iz
real dum(3,nz)

open(unit=file_om,file=trim(adjustl(outdir))//'/profiles/'//trim(adjustl(outfilename))&
    //'/org.txt',status='old')
open(unit=file_ch4,file=trim(adjustl(outdir))//'/profiles/'//trim(adjustl(outfilename))&
    //'/ch4.txt',status='old')
open(unit=file_anion,file=trim(adjustl(outdir))//'/profiles/'//trim(adjustl(outfilename))&
    //'/anion.txt',status='old')
    
do iz = 1, nz
    read(file_om,*) z(iz), dz(iz), org(iz), phi_s(iz), phi_m(iz)
    read(file_ch4,*) z(iz), cm(iz),ceq(iz),h(iz),b(iz), u(iz), v(iz), rxn_h(iz), rxn_b(iz)
    read(file_anion,*) z(iz), cc(iz), dum(1,iz),cs(iz), cbr(iz),dum(2,iz), ci(iz),dum(3,iz),cbr(iz)
    
    cc(iz) = cc(iz)/(rho_f/35.453d0/1d-3)
    cbr(iz) = cbr(iz)/(rho_f/79.904d0/1d-3)   
    ci(iz) = ci(iz)/(rho_f/126.90d0/1d-3)  
end do

close(file_om)
close(file_ch4)
close(file_anion)

endsubroutine readprofiles
!**************************************************************************************************************************************

!**************************************************************************************************************************************
subroutine openfiles(                                                   &
    file_om,file_ch4,file_anion,file_ch4flx,file_so4flx,file_bnd,       &
    file_so4flxss,file_ch4flxss,file_hflxss,file_bflxss,file_omflxss,   &
    outdir,outfilename                                                  &
    )
implicit none 
integer,intent(in)::file_om,file_ch4,file_anion,file_ch4flx,file_so4flx,file_bnd,  &
    file_so4flxss,file_ch4flxss,file_hflxss,file_bflxss,file_omflxss  
character(100),intent(in)::outdir,outfilename

open(unit=file_om,file=trim(adjustl(outdir))//'/profiles/'//trim(adjustl(outfilename))&
    //'/org.txt',status='replace')
open(unit=file_ch4,file=trim(adjustl(outdir))//'/profiles/'//trim(adjustl(outfilename))&
    //'/ch4.txt',status='replace')
open(unit=file_anion,file=trim(adjustl(outdir))//'/profiles/'//trim(adjustl(outfilename))&
    //'/anion.txt',status='replace')
open(unit=file_ch4flxss,file=trim(adjustl(outdir))//'/profiles/'//trim(adjustl(outfilename))&
    //'/ch4flxss.txt',status='replace')
open(unit=file_so4flxss,file=trim(adjustl(outdir))//'/profiles/'//trim(adjustl(outfilename))&
    //'/so4flxss.txt',status='replace')
open(unit=file_hflxss,file=trim(adjustl(outdir))//'/profiles/'//trim(adjustl(outfilename))&
    //'/hydrateflxss.txt',status='replace')
open(unit=file_bflxss,file=trim(adjustl(outdir))//'/profiles/'//trim(adjustl(outfilename))&
    //'/bubbleflxss.txt',status='replace')
open(unit=file_omflxss,file=trim(adjustl(outdir))//'/profiles/'//trim(adjustl(outfilename))&
    //'/omflxss.txt',status='replace')
open(unit=file_ch4flx,file=trim(adjustl(outdir))//'/profiles/'//trim(adjustl(outfilename))&
    //'/ch4flx.txt',status='replace',position='append')
open(unit=file_so4flx,file=trim(adjustl(outdir))//'/profiles/'//trim(adjustl(outfilename))&
    //'/so4flx.txt',status='replace',position='append')
open(unit=file_bnd,file=trim(adjustl(outdir))//'/profiles/'//trim(adjustl(outfilename))&
    //'/bnd.txt',status='replace',position='append')
    
endsubroutine openfiles
!**************************************************************************************************************************************

!**************************************************************************************************************************************
subroutine closefiles(                                                  &
    file_om,file_ch4,                                                   &
    file_anion,file_ch4flx,file_so4flx,file_bnd,                        &
    file_so4flxss,file_ch4flxss,file_hflxss,file_bflxss,file_omflxss    &
    )
implicit none 
integer,intent(in)::file_om,file_ch4,file_anion,file_ch4flx,file_so4flx,file_bnd, &
    file_so4flxss,file_ch4flxss,file_hflxss,file_bflxss,file_omflxss  

close(file_om)
close(file_ch4)
close(file_anion)
close(file_ch4flx)
close(file_so4flx)
close(file_bnd)
close(file_so4flxss)
close(file_ch4flxss)
close(file_hflxss)
close(file_bflxss)
close(file_omflxss)

endsubroutine closefiles 
!**************************************************************************************************************************************

!**************************************************************************************************************************************
subroutine recordprofiles(  &
    file_om,z,dz,org,phi_s,phi_m,file_ch4,cm,ceq,h,b,u,v,rxn_h,rxn_b,ccx,rho_f,hx,csx,cbr,ci,nz,file_anion  &
    )
implicit none 
integer,intent(in)::file_om,file_ch4,nz,file_anion
real,dimension(nz),intent(in)::z,dz,org,phi_s,phi_m,cm,ceq,h,b,u,v,rxn_h,rxn_b,ccx,hx,csx,cbr,ci
real,intent(in)::rho_f

integer iz

do iz = 1, nz
    write(file_om,*) z(iz), dz(iz), org(iz), phi_s(iz), phi_m(iz)
    write(file_ch4,*) z(iz), cm(iz),ceq(iz),h(iz),b(iz), u(iz), v(iz), rxn_h(iz), rxn_b(iz)
    write(file_anion,*) z(iz), ccx(iz)*rho_f/35.453d0/1d-3, (1d0-hx(iz))*ccx(iz)*rho_f/35.453d0/1d-3,  &
        csx(iz), cbr(iz)*rho_f/79.904d0/1d-3, &  
        (1d0-hx(iz))*cbr(iz)*rho_f/79.904d0/1d-3, ci(iz)*rho_f/126.90d0/1d-3,  &
        (1d0-hx(iz))*ci(iz)*rho_f/126.90d0/1d-3,cbr(iz)/79.904d0/(ci(iz)/126.90d0)
end do


endsubroutine recordprofiles
!**************************************************************************************************************************************

!**************************************************************************************************************************************
subroutine recordfluxes(                                                &
    nflx,nz,flxnames,omflx,so4flx,ch4flx,hflx,bflx,z,                   &
    file_so4flxss,file_ch4flxss,file_hflxss,file_bflxss,file_omflxss    &
    )
implicit none 
integer,intent(in)::nflx,nz,file_so4flxss,file_ch4flxss,file_hflxss,file_bflxss,file_omflxss  
character(100),intent(in)::flxnames(nflx)
real,dimension(nflx,nz),intent(in)::omflx,so4flx,ch4flx,hflx,bflx
real,dimension(nz),intent(in)::z

integer iz,iflx

do iz = 1, nz
    if (iz==1) then 
        write(file_omflxss, '(a5,10(3x,a10))')'z(m)',adjustl(flxnames(:))
        write(file_so4flxss,'(a5,10(3x,a10))')'z(m)',adjustl(flxnames(:))
        write(file_ch4flxss,'(a5,10(3x,a10))')'z(m)',adjustl(flxnames(:))
        write(file_hflxss,  '(a5,10(3x,a10))')'z(m)',adjustl(flxnames(:))
        write(file_bflxss,  '(a5,10(3x,a10))')'z(m)',adjustl(flxnames(:))
    endif 
    write(file_omflxss, *) z(iz),(omflx(iflx,iz) ,iflx=1,nflx)
    write(file_so4flxss,*) z(iz),(so4flx(iflx,iz),iflx=1,nflx)
    write(file_ch4flxss,*) z(iz),(ch4flx(iflx,iz),iflx=1,nflx)
    write(file_hflxss,  *) z(iz),(hflx(iflx,iz),  iflx=1,nflx)
    write(file_bflxss,  *) z(iz),(bflx(iflx,iz),  iflx=1,nflx)
enddo 


endsubroutine recordfluxes
!**************************************************************************************************************************************

!**************************************************************************************************************************************
subroutine display_prof(                &
    nz,nflx,                            &
    ch,cb,rho_h,rho_b,mch4,dt,time,     &
    z,cmx,csx,hx,bx,dz,                 &
    omflx,so4flx,ch4flx,hflx,bflx,      &
    flxnames                            &
    )
implicit none
integer,intent(in)::nz,nflx
real,intent(in)::ch,cb,rho_h,rho_b,mch4,dt,time
real,dimension(nz),intent(in)::z,cmx,csx,hx,bx,dz
real,dimension(nflx,nz),intent(in)::omflx,so4flx,ch4flx,hflx,bflx
character(100),intent(in)::flxnames(nflx)

integer iz,iflx

print *, '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
print'(a,e10.3e3)','TIME=',time
print'(a,e10.3e3)','DT=',dt
print *
print *, '>>>>>> CONCS.'
print '(5(3x,a10))', 'z [m]', 'csx [M]','cmx [M]','hx [v.%]','bx [v.%]'
do iz=1,nz!,nz/10
    print '(5(3x,e10.3e3))', z(iz),csx(iz),cmx(iz),hx(iz)*1d2,bx(iz)*1d2
enddo 
print *
print *,'------ FLUXES [mol m-2 yr-1]'
print '(a5,10(3x,a10))', '     ',adjustl(flxnames(:))
print '(a5,10(3x,e10.3e3))', 'om  :',(sum(omflx(iflx,:)*dz(:)),iflx=1,nflx)
print '(a5,10(3x,e10.3e3))', 'so4 :',(sum(so4flx(iflx,:)*dz(:)),iflx=1,nflx)
print '(a5,10(3x,e10.3e3))', 'ch4 :',(sum(ch4flx(iflx,:)*dz(:)),iflx=1,nflx)
print '(a5,10(3x,e10.3e3))', 'h   :',(sum(hflx(iflx,:)*dz(:))*(ch*rho_h/mch4),iflx=1,nflx)
print '(a5,10(3x,e10.3e3))', 'b   :',(sum(bflx(iflx,:)*dz(:))*(cb*rho_b/mch4),iflx=1,nflx)
print *
print *, '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'

endsubroutine display_prof
!**************************************************************************************************************************************

!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$  REACTIVE TRANSPORT (TRANSIENT) $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

!**************************************************************************************************************************************
subroutine omcalc(                                                                                  &
    zs,nz,org_0,v,org,dec_m0,dec_s0,z,dz,mch4,mch2o,mso4,poro,dt,tol,rho_s,rho_f,poro_0,v_sed,nflx  &! input 
    ,phi_s,phi_m,orgx,omflx                                                                         &! output
    )
! solving om conc. 
! first assing different rate const for sulfate reduction and methanogenesis zones 
! then sovlign a govering equation 
!           (1-poro)*(omx-om)/dt = -[(1-poro)*v*omx(iz)-(1-poro)*v*omx(iz-1)]/dz - dec*omx 
! 
implicit none 
integer,intent(in)::nz,nflx
real,intent(in)::zs,org_0,v(nz),org(nz),dec_m0,dec_s0,z(nz),dz(nz),mch4,mch2o,mso4,poro(nz)  &
    ,dt,tol,rho_s,rho_f,poro_0,v_sed  
real,intent(out)::phi_s(nz),phi_m(nz),orgx(nz),omflx(nflx,nz) 

! local variables
integer::nmx,iz,info,row
real,allocatable::amx(:,:),ymx(:)
integer,allocatable::ipiv(:)
real dec_m(nz),dec_s(nz)
integer::itflx,iadv,idif,irxn_ah,irxn_ab,irxn_hb,irxn_om,irxn_aom,irxn_dec,ires
data itflx,iadv,idif,irxn_ah,irxn_ab,irxn_hb,irxn_om,irxn_aom,irxn_dec,ires/1,2,3,4,5,6,7,8,9,10/

dec_s = 0d0
dec_m = 0d0
if (zs <= dz(1)) then 
    dec_s(1) = zs/dz(1)*dec_s0
    dec_m(1) = (dz(1)-zs)/dz(1)*dec_m0
    dec_m(2:) = dec_m0
else 
    do iz=2,nz  
        ! print*,iz,z(iz-1)+0.5d0*dz(iz-1),zs,z(iz)+0.5d0*dz(iz)
        if (z(iz-1)+0.5d0*dz(iz-1)<zs .and. zs<=z(iz)+0.5d0*dz(iz)) then 
            dec_s(iz)=(z(iz)+0.5d0*dz(iz)-zs)/dz(iz)*dec_s0
            dec_m(iz)=(zs-(z(iz-1)+0.5d0*dz(iz-1)))/dz(iz)*dec_m0
            if (iz/=nz) then 
                dec_m(iz+1:)=dec_m0
            endif 
            dec_s(:iz-1) = dec_s0
            exit 
        endif 
    enddo 
endif 

! open(unit=250,file='test_om_dec.txt',action = 'write',status='replace')
! do iz=1,nz
    ! write(250,*) z(iz),dz(iz),dec_s(iz),dec_m(iz)
! enddo 
! close(250)

nmx = nz 

if (allocated(amx)) deallocate(amx)
if (allocated(ymx)) deallocate(ymx)
if (allocated(ipiv)) deallocate(ipiv)
allocate (amx(nmx,nmx),ymx(nmx),ipiv(nmx))

amx = 0d0
ymx = 0d0

do iz=1,nz
    row = iz
    if (iz==1) then 
        ymx(row) = ymx(row) + (  &
            -(rho_s/mch2o)*1d-2*(1d0-poro(iz))*(0d0-org_0)/dt  &! time chage rate in CH2O mol / sediment m3
            -(rho_s/mch2o)*1d-2*((1d0-poro(iz))*0d0*v(iz)-(1d0-poro_0)*org_0*v_sed)/dz(iz) &! advecction
            -(rho_s/mch2o)*1d-2*(1d0-poro(iz))*(dec_m(iz)+dec_s(iz))*0d0  &! decomposition 
            )
        amx(row,row) = amx(row,row) + (&
            -(rho_s/mch2o)*1d-2*(1d0-poro(iz))*(1d0-0d0)/dt  &! time chage rate in CH2O mol / sediment m3
            -(rho_s/mch2o)*1d-2*((1d0-poro(iz))*1d0*v(iz)-(1d0-poro_0)*0d0*v_sed)/dz(iz) &! advecction
            -(rho_s/mch2o)*1d-2*(1d0-poro(iz))*(dec_m(iz)+dec_s(iz))*1d0  &! decomposition 
            )
    else 
        ymx(row) = ymx(row) + (  &
            -(rho_s/mch2o)*1d-2*(1d0-poro(iz))*(0d0-org(iz))/dt  &! time chage rate in CH2O mol / sediment m3
            -(rho_s/mch2o)*1d-2*((1d0-poro(iz))*0d0*v(iz)-(1d0-poro(iz-1))*0d0*v(iz-1))/dz(iz) &! advecction
            -(rho_s/mch2o)*1d-2*(1d0-poro(iz))*(dec_m(iz)+dec_s(iz))*0d0  &! decomposition
            )
        amx(row,row) = amx(row,row) + (&
            -(rho_s/mch2o)*1d-2*(1d0-poro(iz))*(1d0-0d0)/dt  &! time chage rate in CH2O mol / sediment m3
            -(rho_s/mch2o)*1d-2*((1d0-poro(iz))*1d0*v(iz)-(1d0-poro(iz-1))*0d0*v(iz-1))/dz(iz) &! advecction
            -(rho_s/mch2o)*1d-2*(1d0-poro(iz))*(dec_m(iz)+dec_s(iz))*1d0  &! decomposition 
            )
        amx(row,row-1) = amx(row,row-1) + (&
            -(rho_s/mch2o)*1d-2*(1d0-poro(iz))*(0d0-0d0)/dt  &! time chage rate in CH2O mol / sediment m3
            -(rho_s/mch2o)*1d-2*((1d0-poro(iz))*0d0*v(iz)-(1d0-poro(iz-1))*1d0*v(iz-1))/dz(iz) &! advecction
            -(rho_s/mch2o)*1d-2*(1d0-poro(iz))*(dec_m(iz)+dec_s(iz))*0d0  &! decomposition 
            )
    endif 
enddo 
ymx = -ymx
call DGESV(nmx,int(1),amx,nmx,ipiv,ymx,nmx,info) 
orgx = ymx
               
if (info/=0) then 
    print *, '*** FATAL ERROR in using matrix solver ****'
    print *, info
    stop
endif 


select case(trim(par_sed_hydrate_opt_org))
    case('muds') 
        phi_m = (rho_s/mch2o)*1d-2*(1d0-poro)*dec_m*orgx  ! mol m-3 yr-1 reflecting stoichiometry 2CH2O + CO2 + 4H2 = 2CH4 + 2H2O       
    case('omen') ! use omen to get om conc. (wt%) 
        if (trim(par_sed_diagen_Corgopt)=='huelse2016') then 
            phi_m = 0.5d0*(rho_s/mch2o)*1d-2*(1d0-poro)*dec_m*orgx  ! mol m-3 yr-1 reflecting stoichiometry CH2O  = 0.5 CH4 + 0.5 CO2 
        else 
            phi_m = (rho_s/mch2o)*1d-2*(1d0-poro)*dec_m*orgx  ! mol m-3 yr-1 reflecting stoichiometry 2CH2O + CO2 + 4H2 = 2CH4 + 2H2O     
        endif 
endselect 
! phi_m = (rho_s/mch2o)*1d-2*(1d0-poro)*dec_m*orgx  ! mol m-3 yr-1 reflecting stoichiometry 2CH2O + CO2 + 4H2 = 2CH4 + 2H2O 
! phi_m = 0.5d0*(rho_s/mch2o)*1d-2*(1d0-poro)*dec_m*orgx  ! mol m-3 yr-1 reflecting stoichiometry CH2O  = 0.5 CH4 + 0.5 CO2 
phi_s = 0.5d0*(rho_s/mch2o)*1d-2*(1d0-poro)*dec_s*orgx  ! mol m-3 yr-1 reflecting sotichiometry CH2O + 0.5 SO42- = 0.5 H2S + HCO3-

! flux calculation 
omflx = 0d0

do iz=1,nz
    if (iz==1) then 
        omflx(itflx,iz) = omflx(itflx,iz) + (  &
            -(rho_s/mch2o)*1d-2*(1d0-poro(iz))*(orgx(iz)-org_0)/dt  &! time chage rate in CH2O mol / sediment m3
            )
        omflx(iadv,iz) = omflx(iadv,iz) + (  &
            -(rho_s/mch2o)*1d-2*((1d0-poro(iz))*orgx(iz)*v(iz)-(1d0-poro_0)*org_0*v_sed)/dz(iz) &! advecction
            )
        omflx(irxn_om,iz) = omflx(irxn_om,iz) + (  &
            -(rho_s/mch2o)*1d-2*(orgx(iz)-poro(iz))*(dec_m(iz)+dec_s(iz))*0d0  &! decomposition 
            )
    else 
        omflx(itflx,iz) = omflx(itflx,iz) + (  &
            -(rho_s/mch2o)*1d-2*(1d0-poro(iz))*(orgx(iz)-org(iz))/dt  &! time chage rate in CH2O mol / sediment m3
            )
        omflx(iadv,iz) = omflx(iadv,iz) + (  &
            -(rho_s/mch2o)*1d-2*((1d0-poro(iz))*orgx(iz)*v(iz)-(1d0-poro(iz-1))*orgx(iz-1)*v(iz-1))/dz(iz) &! advecction
            )
        omflx(irxn_om,iz) = omflx(irxn_om,iz) + (  &
            -(rho_s/mch2o)*1d-2*(1d0-poro(iz))*(dec_m(iz)+dec_s(iz))*orgx(iz)  &! decomposition
            )
    endif 
    omflx(ires,iz)=sum(omflx(:,iz))
enddo 

print*,'net om flx  :',(rho_s/mch2o)*1d-2*((1d0-poro_0)*org_0*v_sed) - (rho_s/mch2o)*1d-2*((1d0-poro(nz))*orgx(nz)*v(nz))
print*,'om rain flx :',(rho_s/mch2o)*1d-2*((1d0-poro_0)*org_0*v_sed) 
print*,'om bur flx  :',(rho_s/mch2o)*1d-2*((1d0-poro(nz))*orgx(nz)*v(nz))

endsubroutine omcalc
!**************************************************************************************************************************************

!**************************************************************************************************************************************
subroutine calcrxncoefs(  &
    nz,z3,z,dz,hszflg,yr2sec  &! input 
    ,rxn_h,rxn_b,rxn_hb,rxn_bh  &! output
    ) 
implicit none

integer,intent(in)::nz
real,intent(in)::z3,z(nz),dz(nz),yr2sec
integer,intent(in)::hszflg
real,dimension(nz),intent(out)::rxn_h,rxn_b,rxn_hb,rxn_bh

integer iz

if (hszflg == 2) then 
    ! there is no hsz 
    rxn_h = 0d0
    rxn_b = 1d-8*yr2sec
else
    rxn_b = 0d0
    rxn_h = 0d0
    if (z3 <= dz(1)) then 
        rxn_b(1) = z3/dz(1)*1d-8*yr2sec
        rxn_h(1) = (dz(1)-z3)/dz(1)*1d-8*yr2sec
        rxn_b(2:) = 1d0*1d-8*yr2sec
    else 
        do iz=2,nz  
            if (z(iz-1)+0.5d0*dz(iz-1)<z3 .and. z3<=z(iz)+0.5d0*dz(iz)) then 
                rxn_b(iz)=(z(iz)+0.5d0*dz(iz)-z3)/dz(iz)*1d-8*yr2sec
                rxn_h(iz)=(z3-(z(iz-1)+0.5d0*dz(iz-1)))/dz(iz)*1d-8*yr2sec
                if (iz/=nz) then 
                    rxn_b(iz+1:)=1d0*1d-8*yr2sec
                endif 
                rxn_h(:iz-1) = 1d0*1d-8*yr2sec
                exit 
            endif 
        enddo 
    endif 
endif 

rxn_hb = rxn_b
rxn_bh = rxn_h

endsubroutine calcrxncoefs
!**************************************************************************************************************************************

!**************************************************************************************************************************************
subroutine calchbsz(        &
    nz,z3,z,dz,hszflg,tol,  &! input 
    hsz,bsz                 &! output
    ) 
implicit none

integer,intent(in)::nz
real,intent(in)::z3,z(nz),dz(nz),tol
integer,intent(in)::hszflg
real,dimension(nz),intent(out)::hsz,bsz

integer iz

if (hszflg == 2) then 
    ! there is no hsz 
    hsz = 0.
    bsz = 1.
else
    bsz = 0.
    hsz = 0.
    ! used by 4/29/2020
    ! if (z3 <= dz(1)) then 
        ! bsz(1) = z3/dz(1)
        ! hsz(1) = (dz(1)-z3)/dz(1)
        ! bsz(2:) = 1d0
    ! else 
        ! do iz=2,nz  
            ! if (z(iz-1)+0.5d0*dz(iz-1)<z3 .and. z3<=z(iz)+0.5d0*dz(iz)) then 
                ! bsz(iz)=(z(iz)+0.5d0*dz(iz)-z3)/dz(iz)
                ! hsz(iz)=(z3-(z(iz-1)+0.5d0*dz(iz-1)))/dz(iz)
                ! if (iz/=nz) then 
                    ! bsz(iz+1:)=1d0
                ! endif 
                ! hsz(:iz-1) = 1d0
                ! exit 
            ! endif 
        ! enddo 
    ! endif 
    ! tested new version 4/29/2020
    do iz=1,nz  
        ! if (z(iz)-0.5*dz(iz) < z3 .and. z3 <= z(iz)+0.5*dz(iz)) then 
        if ((z(iz)-0.5*dz(iz) - z3)*(z(iz)+0.5*dz(iz)-z3)<=0.) then 
            bsz(iz)=(z(iz)+0.5*dz(iz)-z3)/dz(iz)
            hsz(iz)=(z3-(z(iz)-0.5*dz(iz)))/dz(iz)
        elseif ((z(iz)-0.5*dz(iz) - z3)*(z(iz)+0.5*dz(iz)-z3)>0.) then 
            if ((z(iz)-0.5*dz(iz) - z3)>0. .and. (z(iz)+0.5*dz(iz)-z3)>0.) then 
                hsz(iz) = 0.
                bsz(iz) = 1.
            elseif ((z(iz)-0.5*dz(iz) - z3)<0. .and. (z(iz)+0.5*dz(iz)-z3)<0.) then 
                hsz(iz) = 1.
                bsz(iz) = 0.
            else 
                print*, '***ERROR in calchbsz: if-if clause'
                ! pause
            endif 
        else 
            print*, '***ERROR in calchbsz: if clause' 
            ! pause
        endif 
    enddo
endif 

if (any(abs(hsz+bsz-1.)>tol)) then 
    print '(a)', '*** ERROR in calchbsz'
    print '(2(3x,a10))', 'hszflg', 'z3[m]'
    print '((i13),(3x,e10.3e3))', hszflg,z3
    print '(6(3x,a10))', 'z [m]', 'hsz [-]','bsz [-]','righ?','left?','find z3'
    do iz=1,nz
        print '(3(3x,e10.3e3),3(3x,L10))', z(iz),hsz(iz),bsz(iz),z(iz)-0.5*dz(iz) < z3,z3 <= z(iz)+0.5*dz(iz), &
            (z(iz)-0.5*dz(iz) - z3)*(z(iz)+0.5*dz(iz)-z3)<=0.
    enddo 
    print *
    pause
endif 

endsubroutine calchbsz
!**************************************************************************************************************************************

!**************************************************************************************************************************************
subroutine calcsrz(  &
    nz,zs,z,dz  &! input 
    ,srz  &! output
    ) 
implicit none

integer,intent(in)::nz
real,intent(in)::zs,z(nz),dz(nz)
real,dimension(nz),intent(out)::srz

integer iz

srz = 0d0

if (zs <= dz(1)) then 
    srz(1) = zs/dz(1)
else 
    do iz=2,nz  
        if (z(iz-1)+0.5d0*dz(iz-1)<zs .and. zs<=z(iz)+0.5d0*dz(iz)) then 
            srz(iz)=(z(iz)+0.5d0*dz(iz)-zs)/dz(iz)
            srz(:iz-1) = 1d0
            exit 
        endif 
    enddo 
endif 

endsubroutine calcsrz
!**************************************************************************************************************************************

!**************************************************************************************************************************************
subroutine calcupwindscheme(        &
    up,dwn,cnr,adf,                 &! output 
    w,nz                            &! input 
    )
! from caco3 model 
implicit none
integer,intent(in)::nz
real,intent(in)::w(nz)
real,dimension(nz),intent(out)::up,dwn,cnr,adf
real corrf
integer iz


! ------------ determine variables to realize advection 
!  upwind scheme 
!  up  ---- burial advection at grid i = sporo(i)*w(i)*(some conc. at i) - sporo(i-1)*w(i-1)*(some conc. at i - 1) 
!  dwn ---- burial advection at grid i = sporo(i+1)*w(i+1)*(some conc. at i+1) - sporo(i)*w(i)*(some conc. at i) 
!  cnr ---- burial advection at grid i = sporo(i+1)*w(i+1)*(some conc. at i+1) - sporo(i-1)*w(i-1)*(some conc. at i - 1) 
!  when burial rate is positive, scheme need to choose up, i.e., up = 1.  
!  when burial rate is negative, scheme need to choose dwn, i.e., dwn = 1.  
!  where burial change from positive to negative or vice versa, scheme chooses cnr, i.e., cnr = 1. for the mass balance sake 

up = 0
dwn=0
cnr =0
adf=1d0
do iz=1,nz 
    if (iz==1) then 
        if (w(iz)>=0d0 .and. w(iz+1)>=0d0) then  ! positive burial 
            up(iz) = 1
        elseif (w(iz)<=0d0 .and. w(iz+1)<=0d0) then  ! negative burial 
            dwn(iz) = 1
        else   !  where burial sign changes  
            if (.not.(w(iz)*w(iz+1) <=0d0)) then 
                print*,'error'
                stop
            endif
            cnr(iz) = 1
        endif
    elseif (iz==nz) then 
        if (w(iz)>=0d0 .and. w(iz-1)>=0d0) then
            up(iz) = 1
        elseif (w(iz)<=0d0 .and. w(iz-1)<=0d0) then
            dwn(iz) = 1
        else 
            if (.not.(w(iz)*w(iz-1) <=0d0)) then 
                print*,'error'
                stop
            endif
            cnr(iz) = 1
        endif
    else 
        if (w(iz) >=0d0) then 
            if (w(iz+1)>=0d0 .and. w(iz-1)>=0d0) then
                up(iz) = 1
            else
                cnr(iz) = 1
            endif
        else  
            if (w(iz+1)<=0d0 .and. w(iz-1)<=0d0) then
                dwn(iz) = 1
            else
                cnr(iz) = 1
            endif
        endif
    endif
enddo        

if (sum(up(:)+dwn(:)+cnr(:))/=nz) then
    print*,'error',sum(up),sum(dwn),sum(cnr)
    stop
endif

do iz=1,nz-1
    if (cnr(iz)==1 .and. cnr(iz+1)==1) then 
        if (w(iz)>=0d0 .and. w(iz+1) < 0d0) then
            corrf = 5d0  !  This assignment of central advection term helps conversion especially when assuming turbo2 mixing 
            cnr(iz+1)=abs(w(iz)**corrf)/(abs(w(iz+1)**corrf)+abs(w(iz)**corrf))
            cnr(iz)=abs(w(iz+1)**corrf)/(abs(w(iz+1)**corrf)+abs(w(iz)**corrf))
            dwn(iz+1)=1d0-cnr(iz+1)
            up(iz)=1d0-cnr(iz)
        endif 
    endif 
    if (cnr(iz)==1 .and. cnr(iz+1)==1) then 
        if (w(iz)< 0d0 .and. w(iz+1) >= 0d0) then
            cnr(iz+1)=0
            cnr(iz)=0
            up(iz+1)=1
            dwn(iz)=1
            adf(iz)=abs(w(iz+1))/(abs(w(iz+1))+abs(w(iz)))
            adf(iz+1)=abs(w(iz))/(abs(w(iz+1))+abs(w(iz)))
        endif 
    endif 
enddo       

endsubroutine calcupwindscheme
!**************************************************************************************************************************************

!**************************************************************************************************************************************
subroutine ch4so4calc(                                                                                  &
    nz,v,u,phi_m,phi_s,cs_0,cm_0,rho_h,rho_f,rho_b,ceq,cm,tol,cs,h,b,rxn_b,rxn_h,rxn_aom,v_sed,poro_0,  &! input 
    up,dwn,cnr,adf,u_0,ch,cb,u_ext,poro_ext,v_ext,z,dt,mch4,poro,dz,dif_m,dif_s,zs,z3,hx,bx,nflx,       &! input 
    cm_ext_in,fix_cm_ext,kin_type,loc_display,                                                          &! input
    cmx,csx,ch4flx,so4flx,flg_esc                                                                       &! output 
    )
implicit none 
! #include <defines.h>
integer,intent(in)::nz,nflx
real,dimension(nz),intent(in)::v,u,phi_m,phi_s,ceq,cm,cs,h,b,rxn_b,rxn_h,rxn_aom,up,dwn,cnr,adf,z  &
    ,poro,dz,dif_m,dif_s,hx,bx
real,intent(in)::cs_0,cm_0,rho_h,rho_f,rho_b,tol,v_sed,poro_0,u_0,ch,cb,u_ext,poro_ext,v_ext,dt  &
    ,mch4,zs,z3,cm_ext_in  
real,dimension(nz),intent(out)::cmx,csx
real,dimension(nflx,nz),intent(out)::ch4flx,so4flx
logical,intent(in)::fix_cm_ext
logical,intent(in)::loc_display
logical,intent(inout)::flg_esc
character(100),intent(in)::kin_type

! local variables
integer::nmx,iz,info,row,nsp
real,allocatable::amx(:,:),ymx(:)
integer,allocatable::ipiv(:)
real u_ext_loc,error,fact,cm_ext
integer itr,iiz
integer::itflx,iadv,idif,irxn_ah,irxn_ab,irxn_hb,irxn_om,irxn_aom,irxn_dec,ires
data itflx,iadv,idif,irxn_ah,irxn_ab,irxn_hb,irxn_om,irxn_aom,irxn_dec,ires/1,2,3,4,5,6,7,8,9,10/
real phih(nz),phib(nz),dphih_dcmx(nz),dphib_dcmx(nz)
real::conc_low = 1d-300
! logical::bubble_cut = .true.
! real::bubble_thr = 0.01
! real::bubble_dec = 0.01

! solving ch4 system including aqch4, so4 and solid ch4 hydrate and gas bubble ch4. 
! governing equations are: 
!
!     1. SO4
!     poro*(csx - cs)/dt = -(csx(iz)*u(iz)-csx(iz-1)*u(iz-1))/dz(iz) 
!         +  [0.5d0*(poro(iz+1)*dif(iz+1)*(1d0-hx(iz+1)-bx(iz+1))+poro(iz)*dif(iz)*(1d0-hx(iz)-bx(iz)))*(csx(iz+1)-csx(iz))
!              /(0.5d0*(dz(iz+1)+dz(iz)))
!         -  0.5d0*(poro(iz)*dif(iz)*(1d0-hx(iz)-bx(iz))+poro(iz-1)*dif(iz-1)*(1d0-hx(iz-1)-bx(iz-1)))*(csx(iz)-csx(iz-1))
!              /(0.5d0*(dz(iz)+dz(iz-1)))]/dz(iz)
!         - phi_s(iz) 
!         - poro(iz)*rxn_aom(iz)*1d3*csx(iz)*cmx(iz)
!
!     2. CH4(aq)
!     poro*(cmx - cm)/dt = -(cmx(iz)*u(iz)-cmx(iz-1)*u(iz-1))/dz(iz) 
!         +  [0.5d0*(poro(iz+1)*dif(iz+1)*(1d0-hx(iz+1)-bx(iz+1))+poro(iz)*dif(iz)*(1d0-hx(iz)-bx(iz)))*(cmx(iz+1)-cmx(iz))
!              /(0.5d0*(dz(iz+1)+dz(iz)))
!         -  0.5d0*(poro(iz)*dif(iz)*(1d0-hx(iz)-bx(iz))+poro(iz-1)*dif(iz-1)*(1d0-hx(iz-1)-bx(iz-1)))*(cmx(iz)-cmx(iz-1))
!              /(0.5d0*(dz(iz)+dz(iz-1)))]/dz(iz)
!         + phi_m(iz) 
!         - poro(iz)*rxn_aom(iz)*1d3*csx(iz)*cmx(iz)
!
!     3. CH4 hydrate(sld)
!     poro*(hx - h)/dt = - (poro(iz)*v(iz)*hx(iz) - poro(iz-1)*v(iz-1)*hx(iz-1))/dz(iz) 
!         + rxn_h(iz)*(cm(iz)*mch4/rho_f - ceq(iz))
!
!     4. CH4 bubble(gas but treated as if sld)
!     poro*(hx - h)/dt = - (poro(iz)*v(iz)*hx(iz) - poro(iz-1)*v(iz-1)*hx(iz-1))/dz(iz) 
!         + rxn_h(iz)*(cm(iz)*mch4/rho_f - ceq(iz))
!
!     Solve by Newton's method 
! 

u_ext_loc = (1d0-poro_0)*(poro_ext/(1d0-poro_ext)- poro_ext*(1d0)/(1d0-poro_ext)) *v_sed+poro_ext*u_ext
u_ext_loc = u(nz)
fact = 1d-3
! fix_cm_ext = .true.
! fix_cm_ext = .false.
if (.not.fix_cm_ext) then 
    cm_ext = cmx(nz)
else 
    cm_ext = cm_ext_in
endif 

nsp = 2
nmx = nz*nsp 

if (allocated(amx)) deallocate(amx)
if (allocated(ymx)) deallocate(ymx)
if (allocated(ipiv)) deallocate(ipiv)
allocate (amx(nmx,nmx),ymx(nmx),ipiv(nmx))


error = 1d4
itr = 0
newton: do while (error > tol)

selectcase (trim(kin_type))
    case('default')
        phih(:) = (ch*rho_h/mch4)*rxn_h(:)*(1d3*cmx(:)*mch4/rho_f - ceq(:))*hx(:)
        phib(:) = (cb*rho_b/mch4)*rxn_b(:)*(1d3*cmx(:)*mch4/rho_f - ceq(:))*bx(:)   
        dphih_dcmx(:) = (ch*rho_h/mch4)*rxn_h(:)*(1d3*1d0*mch4/rho_f - ceq(:))*hx(:)
        dphib_dcmx(:) = (cb*rho_b/mch4)*rxn_b(:)*(1d3*1d0*mch4/rho_f - ceq(:))*bx(:)   
    case('wallmann')
        phih(:) = (ch*rho_h/mch4)*rxn_h(:)*(1d3*cmx(:)*mch4/rho_f - ceq(:)) 
        phib(:) = (cb*rho_b/mch4)*rxn_b(:)*(1d3*cmx(:)*mch4/rho_f - ceq(:))  
        dphih_dcmx(:) = (ch*rho_h/mch4)*rxn_h(:)*(1d3*1d0*mch4/rho_f - ceq(:))  
        dphib_dcmx(:) = (cb*rho_b/mch4)*rxn_b(:)*(1d3*1d0*mch4/rho_f - ceq(:))  
        where ((1d3*cmx(:)*mch4/rho_f - ceq(:))<0d0)
            phih(:) = phih(:)*hx(:)
            dphih_dcmx(:) = dphih_dcmx(:)*hx(:)
            phib(:) = phib(:)*bx(:)
            dphib_dcmx(:) = dphib_dcmx(:)*bx(:)
        endwhere
    case('davie')
        phih(:) = (ch*rho_h/mch4)*rxn_h(:)*(1d3*cmx(:)*mch4/rho_f - ceq(:))
        phib(:) = (cb*rho_b/mch4)*rxn_b(:)*(1d3*cmx(:)*mch4/rho_f - ceq(:))   
        dphih_dcmx(:) = 0d0
        dphib_dcmx(:) = 0d0   
endselect

! if (bubble_cut) then 
    ! do iz=1,nz
        ! if (b(iz)-bubble_thr > 0d0) phib(iz) = phib(iz) - (cb*rho_b/mch4)*bubble_dec*(b(iz)-bubble_thr)
    ! enddo
! endif 

amx = 0d0
ymx = 0d0

do iz=1,nz
    row =  1 + (iz-1)*nsp 
    if (iz==1) then 
        ymx(row) = ymx(row) + (  &! sulfate 
            - poro(iz)*1d3*(csx(iz)*(1d0-hx(iz)-bx(iz)) - cs(iz)*(1d0-h(iz)-b(iz)))/dt   &! time change rate 
            - adf(iz)*up(iz)*1d3*(csx(iz)*u(iz)-cs_0*u_0)/dz(iz)  &! adv(up) 
            - adf(iz)*dwn(iz)*1d3*(csx(iz+1)*u(iz+1)-csx(iz)*u(iz))/dz(iz)  &! adv(down) 
            - adf(iz)*cnr(iz)*1d3*(csx(iz+1)*u(iz+1)-cs_0*u_0)/dz(iz)  &! adv(center) 
            + (0.5d0*(poro(iz+1)*dif_s(iz+1)*(1d0-hx(iz+1)-bx(iz+1))+poro(iz)*dif_s(iz)*(1d0-hx(iz)-bx(iz))) &
                *1d3*(csx(iz+1)-csx(iz))/(0.5d0*(dz(iz+1)+dz(iz)))  &
            - 0.5d0*(poro(iz)*dif_s(iz)*(1d0-hx(iz)-bx(iz))+poro_0*dif_s(iz)*(1d0))*1d3*(csx(iz)-cs_0)  &
                /(0.5d0*(dz(iz)+dz(iz))))/dz(iz)   &
            - phi_s(iz)   &
            - poro(iz)*rxn_aom(iz)*1d3*csx(iz)*cmx(iz)*(1d0-hx(iz)-bx(iz))  &
            ! - poro(iz)*rxn_aom(iz)*1d3*csx(iz)*cmx(iz)  &
            )
        amx(row,row) = amx(row,row) + (&
            - poro(iz)*1d3*(1d0*(1d0-hx(iz)-bx(iz)))/dt   &! time change rate 
            - adf(iz)*up(iz)*1d3*(1d0*u(iz))/dz(iz)  &! adv(up) 
            - adf(iz)*dwn(iz)*1d3*(-1d0*u(iz))/dz(iz)  &! adv(down) 
            + (0.5d0*(poro(iz+1)*dif_s(iz+1)*(1d0-hx(iz+1)-bx(iz+1))+poro(iz)*dif_s(iz)*(1d0-hx(iz)-bx(iz)))*1d3*(-1d0)  &
                /(0.5d0*(dz(iz+1)+dz(iz)))  &
            - 0.5d0*(poro(iz)*dif_s(iz)*(1d0-hx(iz)-bx(iz))+poro_0*dif_s(iz)*(1d0))*1d3*(1d0)  &
                /(0.5d0*(dz(iz)+dz(iz))))/dz(iz)   &
            - poro(iz)*rxn_aom(iz)*1d3*1d0*cmx(iz)*(1d0-hx(iz)-bx(iz))  &
            ! - poro(iz)*rxn_aom(iz)*1d3*1d0*cmx(iz)  &
            )*csx(iz)
        amx(row,row+1) = amx(row,row+1) + (&
            - poro(iz)*rxn_aom(iz)*1d3*csx(iz)*1d0*(1d0-hx(iz)-bx(iz))  &
            ! - poro(iz)*rxn_aom(iz)*1d3*csx(iz)*1d0  &
            )*cmx(iz)
        amx(row,row+nsp) = amx(row,row+nsp) + (&
            - adf(iz)*dwn(iz)*1d3*(1d0*u(iz+1))/dz(iz)  &! adv(down) 
            - adf(iz)*cnr(iz)*1d3*(1d0*u(iz+1))/dz(iz)  &! adv(center) 
            + (0.5d0*(poro(iz+1)*dif_s(iz+1)*(1d0-hx(iz+1)-bx(iz+1))+poro(iz)*dif_s(iz)*(1d0-hx(iz)-bx(iz)))*1d3*(1d0)  &
                /(0.5d0*(dz(iz+1)+dz(iz)))  &
            - 0d0)/dz(iz)   &
            )*csx(iz+1)
            
            
        ymx(row+1) = ymx(row+1) + (  &! aq-methane 
            - poro(iz)*1d3*(cmx(iz)*(1d0-hx(iz)-bx(iz)) - cm(iz)*(1d0-h(iz)-b(iz)))/dt   &! time change rate 
            - adf(iz)*up(iz)*1d3*(cmx(iz)*u(iz)-cm_0*u_0)/dz(iz)  &! adv(up) 
            - adf(iz)*dwn(iz)*1d3*(cmx(iz+1)*u(iz+1)-cmx(iz)*u(iz))/dz(iz)  &! adv(down) 
            - adf(iz)*cnr(iz)*1d3*(cmx(iz+1)*u(iz+1)-cm_0*u_0)/dz(iz)  &! adv(center) 
            + (0.5d0*(poro(iz+1)*dif_m(iz+1)*(1d0-hx(iz+1)-bx(iz+1))+poro(iz)*dif_m(iz)*(1d0-hx(iz)-bx(iz)))  &
                *1d3*(cmx(iz+1)-cmx(iz))/(0.5d0*(dz(iz+1)+dz(iz)))  &
            - 0.5d0*(poro(iz)*dif_m(iz)*(1d0-hx(iz)-bx(iz))+poro_0*dif_m(iz)*(1d0))*1d3*(cmx(iz)-cm_0)  &
                /(0.5d0*(dz(iz)+dz(iz))))/dz(iz)   &
            + phi_m(iz)   &
            - poro(iz)*rxn_aom(iz)*1d3*csx(iz)*cmx(iz)*(1d0-hx(iz)-bx(iz))  &
            ! - poro(iz)*rxn_aom(iz)*1d3*csx(iz)*cmx(iz)  &
            - phih(iz)   &
            - phib(iz)   &
            )
        amx(row+1,row) = amx(row+1,row) + (&
            - poro(iz)*rxn_aom(iz)*1d3*cmx(iz)*1d0*(1d0-hx(iz)-bx(iz))  &
            ! - poro(iz)*rxn_aom(iz)*1d3*cmx(iz)*1d0  &
            )*csx(iz)
        amx(row+1,row+1) = amx(row+1,row+1) + (&
            - poro(iz)*1d3*(1d0*(1d0-hx(iz)-bx(iz)))/dt   &! time change rate 
            - adf(iz)*up(iz)*1d3*(1d0*u(iz))/dz(iz)  &! adv(up) 
            - adf(iz)*dwn(iz)*1d3*(-1d0*u(iz))/dz(iz)  &! adv(down) 
            + (0.5d0*(poro(iz+1)*dif_m(iz+1)*(1d0-hx(iz+1)-bx(iz+1))+poro(iz)*dif_m(iz)*(1d0-hx(iz)-bx(iz)))*1d3*(-1d0)  &
                /(0.5d0*(dz(iz+1)+dz(iz)))  &
            - 0.5d0*(poro(iz)*dif_m(iz)*(1d0-hx(iz)-bx(iz))+poro_0*dif_m(iz)*(1d0))*1d3*(1d0)  &
                /(0.5d0*(dz(iz)+dz(iz))))/dz(iz)   &
            - poro(iz)*rxn_aom(iz)*1d3*1d0*csx(iz)*(1d0-hx(iz)-bx(iz))  &
            ! - poro(iz)*rxn_aom(iz)*1d3*1d0*csx(iz)  &
            - dphih_dcmx(iz)   &
            - dphib_dcmx(iz)   &
            )*cmx(iz)
        amx(row+1,row+1+nsp) = amx(row+1,row+1+nsp) + (&
            - adf(iz)*dwn(iz)*1d3*(1d0*u(iz+1))/dz(iz)  &! adv(down) 
            - adf(iz)*cnr(iz)*1d3*(1d0*u(iz+1))/dz(iz)  &! adv(center) 
            + (0.5d0*(poro(iz+1)*dif_m(iz+1)*(1d0-hx(iz+1)-bx(iz+1))+poro(iz)*dif_m(iz)*(1d0-hx(iz)-bx(iz)))*1d3*(1d0)  &
                /(0.5d0*(dz(iz+1)+dz(iz)))  &
            - 0d0)/dz(iz)   &
            )*cmx(iz+1)
            
    elseif (iz==nz) then 
        ymx(row) = ymx(row) + (  &! sulfate 
            - poro(iz)*1d3*(csx(iz)*(1d0-hx(iz)-bx(iz)) - cs(iz)*(1d0-h(iz)-b(iz)))/dt   &! time change rate 
            - adf(iz)*up(iz)*1d3*(csx(iz)*u(iz)-csx(iz-1)*u(iz-1))/dz(iz)  &! adv(up) 
            - adf(iz)*dwn(iz)*1d3*(csx(iz)*u_ext_loc-csx(iz)*u(iz))/dz(iz)  &! adv(down) 
            - adf(iz)*cnr(iz)*1d3*(csx(iz)*u_ext_loc-csx(iz-1)*u(iz-1))/dz(iz)  &! adv(center) 
            + (0d0  &
            - 0.5d0*(poro(iz)*dif_s(iz)*(1d0-hx(iz)-bx(iz))+poro(iz-1)*dif_s(iz-1)*(1d0-hx(iz-1)-bx(iz-1)))  &
                *1d3*(csx(iz)-csx(iz-1))/(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   &
            - phi_s(iz)   &
            - poro(iz)*rxn_aom(iz)*1d3*csx(iz)*cmx(iz)*(1d0-hx(iz)-bx(iz))  &
            ! - poro(iz)*rxn_aom(iz)*1d3*csx(iz)*cmx(iz)  &
            )
        amx(row,row) = amx(row,row) + (&
            - poro(iz)*1d3*(1d0*(1d0-hx(iz)-bx(iz)))/dt   &! time change rate 
            - adf(iz)*up(iz)*1d3*(1d0*u(iz))/dz(iz)  &! adv(up) 
            - adf(iz)*dwn(iz)*1d3*(1d0*u_ext_loc-1d0*u(iz))/dz(iz)  &! adv(down) 
            - adf(iz)*cnr(iz)*1d3*(1d0*u_ext_loc)/dz(iz)  &! adv(down) 
            + (0d0  &
            - 0.5d0*(poro(iz)*dif_s(iz)*(1d0-hx(iz)-bx(iz))+poro(iz-1)*dif_s(iz-1)*(1d0-hx(iz-1)-bx(iz-1)))*1d3*(1d0)  &
                /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   &
            - poro(iz)*rxn_aom(iz)*1d3*1d0*cmx(iz)*(1d0-hx(iz)-bx(iz))  &
            ! - poro(iz)*rxn_aom(iz)*1d3*1d0*cmx(iz) &
            )*csx(iz)
        amx(row,row+1) = amx(row,row+1) + (&
            - poro(iz)*rxn_aom(iz)*1d3*csx(iz)*1d0*(1d0-hx(iz)-bx(iz))  &
            ! - poro(iz)*rxn_aom(iz)*1d3*csx(iz)*1d0  &
            )*cmx(iz)
        amx(row,row-nsp) = amx(row,row-nsp) + (&
            - adf(iz)*up(iz)*1d3*(-1d0*u(iz-1))/dz(iz)  &! adv(down) 
            - adf(iz)*cnr(iz)*1d3*(-1d0*u(iz-1))/dz(iz)  &! adv(center) 
            + (-0.5d0*(poro(iz)*dif_s(iz)*(1d0-hx(iz)-bx(iz))+poro(iz-1)*dif_s(iz-1)*(1d0-hx(iz-1)-bx(iz-1)))*1d3*(-1d0)  &
                /(0.5d0*(dz(iz-1)+dz(iz)))  &
                )/dz(iz)   &
            )*csx(iz-1)
            
            
        ymx(row+1) = ymx(row+1) + (  &! aq-methane 
            - poro(iz)*1d3*(cmx(iz)*(1d0-hx(iz)-bx(iz)) - cm(iz)*(1d0-h(iz)-b(iz)))/dt   &! time change rate 
            - adf(iz)*up(iz)*1d3*(cmx(iz)*u(iz)-cmx(iz-1)*u(iz-1))/dz(iz)  &! adv(up) 
            - adf(iz)*dwn(iz)*1d3*(cm_ext*u_ext_loc-cmx(iz)*u(iz))/dz(iz)  &! adv(down) 
            - adf(iz)*cnr(iz)*1d3*(cm_ext*u_ext_loc-cmx(iz-1)*u(iz-1))/dz(iz)  &! adv(center) 
            + (0d0  &
            - 0.5d0*(poro(iz)*dif_m(iz)*(1d0-hx(iz)-bx(iz))+poro(iz-1)*dif_m(iz-1)*(1d0-hx(iz-1)-bx(iz-1)))  &
                *1d3*(cmx(iz)-cmx(iz-1))/(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   &
            + phi_m(iz)   &
            - poro(iz)*rxn_aom(iz)*1d3*csx(iz)*cmx(iz)*(1d0-hx(iz)-bx(iz))  &
            ! - poro(iz)*rxn_aom(iz)*1d3*csx(iz)*cmx(iz)  &
            - phih(iz)   &
            - phib(iz)   &
            )
        amx(row+1,row) = amx(row+1,row) + (&
            - poro(iz)*rxn_aom(iz)*1d3*cmx(iz)*1d0*(1d0-hx(iz)-bx(iz))  &
            ! - poro(iz)*rxn_aom(iz)*1d3*cmx(iz)*1d0  &
            )*csx(iz)
        amx(row+1,row+1) = amx(row+1,row+1) + (&
            - poro(iz)*1d3*(1d0*(1d0-hx(iz)-bx(iz)))/dt   &! time change rate 
            - adf(iz)*up(iz)*1d3*(1d0*u(iz))/dz(iz)  &! adv(up) 
            - adf(iz)*dwn(iz)*1d3*(merge(1d0,0d0,.not.fix_cm_ext)*u_ext_loc-1d0*u(iz))/dz(iz)  &! adv(down) 
            - adf(iz)*cnr(iz)*1d3*(merge(1d0,0d0,.not.fix_cm_ext)*u_ext_loc)/dz(iz)  &! adv(down) 
            + (0d0  &
            - 0.5d0*(poro(iz)*dif_m(iz)*(1d0-hx(iz)-bx(iz))+poro(iz-1)*dif_m(iz-1)*(1d0-hx(iz-1)-bx(iz-1)))*1d3*(1d0)  &
                /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   &
            - poro(iz)*rxn_aom(iz)*1d3*1d0*csx(iz)*(1d0-hx(iz)-bx(iz))  &
            ! - poro(iz)*rxn_aom(iz)*1d3*1d0*csx(iz)  &
            - dphih_dcmx(iz)   &
            - dphib_dcmx(iz)   &
            )*cmx(iz)
        amx(row+1,row+1-nsp) = amx(row+1,row+1-nsp) + (&
            - adf(iz)*up(iz)*1d3*(-1d0*u(iz-1))/dz(iz)  &! adv(down) 
            - adf(iz)*cnr(iz)*1d3*(-1d0*u(iz-1))/dz(iz)  &! adv(center) 
            + (-0.5d0*(poro(iz)*dif_m(iz)*(1d0-hx(iz)-bx(iz))+poro(iz-1)*dif_m(iz-1)*(1d0-hx(iz-1)-bx(iz-1)))*1d3*(-1d0)  &
                /(0.5d0*(dz(iz-1)+dz(iz)))   &
                )/dz(iz)   &
            )*cmx(iz-1)
    
    else 
        ymx(row) = ymx(row) + (  &! sulfate 
            - poro(iz)*1d3*(csx(iz)*(1d0-hx(iz)-bx(iz)) - cs(iz)*(1d0-h(iz)-b(iz)))/dt   &! time change rate 
            - adf(iz)*up(iz)*1d3*(csx(iz)*u(iz)-csx(iz-1)*u(iz-1))/dz(iz)  &! adv(up) 
            - adf(iz)*dwn(iz)*1d3*(csx(iz+1)*u(iz+1)-csx(iz)*u(iz))/dz(iz)  &! adv(down) 
            - adf(iz)*cnr(iz)*1d3*(csx(iz+1)*u(iz+1)-csx(iz-1)*u(iz-1))/dz(iz)  &! adv(center) 
            + (0.5d0*(poro(iz+1)*dif_s(iz+1)*(1d0-hx(iz+1)-bx(iz+1))+poro(iz)*dif_s(iz)*(1d0-hx(iz)-bx(iz)))  &
                *1d3*(csx(iz+1)-csx(iz))/(0.5d0*(dz(iz+1)+dz(iz)))  &
                - 0.5d0*(poro(iz)*dif_s(iz)*(1d0-hx(iz)-bx(iz))+poro(iz-1)*dif_s(iz-1)*(1d0-hx(iz-1)-bx(iz-1)))  &
                *1d3*(csx(iz)-csx(iz-1))/(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   &
            - phi_s(iz)   &
            - poro(iz)*rxn_aom(iz)*1d3*csx(iz)*cmx(iz)*(1d0-hx(iz)-bx(iz))  &
            ! - poro(iz)*rxn_aom(iz)*1d3*csx(iz)*cmx(iz)  &
            )
        amx(row,row) = amx(row,row) + (&
            - poro(iz)*1d3*(1d0*(1d0-hx(iz)-bx(iz)))/dt   &! time change rate 
            - adf(iz)*up(iz)*1d3*(1d0*u(iz))/dz(iz)  &! adv(up) 
            - adf(iz)*dwn(iz)*1d3*(-1d0*u(iz))/dz(iz)  &! adv(down) 
            + (0.5d0*(poro(iz+1)*dif_s(iz+1)*(1d0-hx(iz+1)-bx(iz+1))+poro(iz)*dif_s(iz)*(1d0-hx(iz)-bx(iz)))*1d3*(-1d0)  &
                /(0.5d0*(dz(iz+1)+dz(iz)))  &
            - 0.5d0*(poro(iz)*dif_s(iz)*(1d0-hx(iz)-bx(iz))+poro(iz-1)*dif_s(iz-1)*(1d0-hx(iz-1)-bx(iz-1)))*1d3*(1d0)  &
                /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   &
            - poro(iz)*rxn_aom(iz)*1d3*1d0*cmx(iz)*(1d0-hx(iz)-bx(iz))  &
            ! - poro(iz)*rxn_aom(iz)*1d3*1d0*cmx(iz)  &
            )*csx(iz)
        amx(row,row+1) = amx(row,row+1) + (&
            - poro(iz)*rxn_aom(iz)*1d3*csx(iz)*1d0*(1d0-hx(iz)-bx(iz))  &
            ! - poro(iz)*rxn_aom(iz)*1d3*csx(iz)*1d0  &
            )*cmx(iz)
        amx(row,row+nsp) = amx(row,row+nsp) + (&
            - adf(iz)*dwn(iz)*1d3*(1d0*u(iz+1))/dz(iz)  &! adv(down) 
            - adf(iz)*cnr(iz)*1d3*(1d0*u(iz+1))/dz(iz)  &! adv(center) 
            + (0.5d0*(poro(iz+1)*dif_s(iz+1)*(1d0-hx(iz+1)-bx(iz+1))+poro(iz)*dif_s(iz)*(1d0-hx(iz)-bx(iz)))*1d3*(1d0)  &
                /(0.5d0*(dz(iz+1)+dz(iz)))  &
            - 0d0)/dz(iz)   &
            )*csx(iz+1)
        amx(row,row-nsp) = amx(row,row-nsp) + (&
            - adf(iz)*up(iz)*1d3*(-1d0*u(iz-1))/dz(iz)  &! adv(down) 
            - adf(iz)*cnr(iz)*1d3*(-1d0*u(iz-1))/dz(iz)  &! adv(center) 
            + (-0.5d0*(poro(iz)*dif_s(iz)*(1d0-hx(iz)-bx(iz))+poro(iz-1)*dif_s(iz-1)*(1d0-hx(iz-1)-bx(iz-1)))*1d3*(-1d0)  &
                /(0.5d0*(dz(iz-1)+dz(iz)))  &
                )/dz(iz)   &
            )*csx(iz-1)
            
        ymx(row+1) = ymx(row+1) + (  &! aq-methane 
            - poro(iz)*1d3*(cmx(iz)*(1d0-hx(iz)-bx(iz)) - cm(iz)*(1d0-h(iz)-b(iz)))/dt   &! time change rate 
            - adf(iz)*up(iz)*1d3*(cmx(iz)*u(iz)-cmx(iz-1)*u(iz-1))/dz(iz)  &! adv(up) 
            - adf(iz)*dwn(iz)*1d3*(cmx(iz+1)*u(iz+1)-cmx(iz)*u(iz))/dz(iz)  &! adv(down) 
            - adf(iz)*cnr(iz)*1d3*(cmx(iz+1)*u(iz+1)-cmx(iz-1)*u(iz-1))/dz(iz)  &! adv(center) 
            + (0.5d0*(poro(iz+1)*dif_m(iz+1)*(1d0-hx(iz+1)-bx(iz+1))+poro(iz)*dif_m(iz)*(1d0-hx(iz)-bx(iz)))  &
                *1d3*(cmx(iz+1)-cmx(iz))/(0.5d0*(dz(iz+1)+dz(iz)))  &
                - 0.5d0*(poro(iz)*dif_m(iz)*(1d0-hx(iz)-bx(iz))+poro(iz-1)*dif_m(iz-1)*(1d0-hx(iz-1)-bx(iz-1)))  &
                *1d3*(cmx(iz)-cmx(iz-1))/(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   &
            + phi_m(iz)   &
            - poro(iz)*rxn_aom(iz)*1d3*csx(iz)*cmx(iz)*(1d0-hx(iz)-bx(iz))  &
            ! - poro(iz)*rxn_aom(iz)*1d3*csx(iz)*cmx(iz)  &
            - phih(iz)   &
            - phib(iz)   &
            )
        amx(row+1,row) = amx(row+1,row) + (&
            - poro(iz)*rxn_aom(iz)*1d3*cmx(iz)*1d0*(1d0-hx(iz)-bx(iz))  &
            ! - poro(iz)*rxn_aom(iz)*1d3*cmx(iz)*1d0  &
            )*csx(iz)
        amx(row+1,row+1) = amx(row+1,row+1) + (&
            - poro(iz)*1d3*(1d0*(1d0-hx(iz)-bx(iz)))/dt   &! time change rate 
            - adf(iz)*up(iz)*1d3*(1d0*u(iz))/dz(iz)  &! adv(up) 
            - adf(iz)*dwn(iz)*1d3*(-1d0*u(iz))/dz(iz)  &! adv(down) 
            + (0.5d0*(poro(iz+1)*dif_m(iz+1)*(1d0-hx(iz+1)-bx(iz+1))+poro(iz)*dif_m(iz)*(1d0-hx(iz)-bx(iz)))*1d3*(-1d0)  &
                /(0.5d0*(dz(iz+1)+dz(iz)))  &
            - 0.5d0*(poro(iz)*dif_m(iz)*(1d0-hx(iz)-bx(iz))+poro(iz-1)*dif_m(iz-1)*(1d0-hx(iz-1)-bx(iz-1)))*1d3*(1d0)  &
                /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   &
            - poro(iz)*rxn_aom(iz)*1d3*1d0*csx(iz)*(1d0-hx(iz)-bx(iz))  &
            ! - poro(iz)*rxn_aom(iz)*1d3*1d0*csx(iz)  &
            - dphih_dcmx(iz)   &
            - dphib_dcmx(iz)   &
            )*cmx(iz)
        amx(row+1,row+1+nsp) = amx(row+1,row+1+nsp) + (&
            - adf(iz)*dwn(iz)*1d3*(1d0*u(iz+1))/dz(iz)  &! adv(down) 
            - adf(iz)*cnr(iz)*1d3*(1d0*u(iz+1))/dz(iz)  &! adv(center) 
            + (0.5d0*(poro(iz+1)*dif_m(iz+1)*(1d0-hx(iz+1)-bx(iz+1))+poro(iz)*dif_m(iz)*(1d0-hx(iz)-bx(iz)))*1d3*(1d0)  &
                /(0.5d0*(dz(iz+1)+dz(iz)))  &
            - 0d0)/dz(iz)   &
            )*cmx(iz+1)
        amx(row+1,row+1-nsp) = amx(row+1,row+1-nsp) + (&
            - adf(iz)*up(iz)*1d3*(-1d0*u(iz-1))/dz(iz)  &! adv(down) 
            - adf(iz)*cnr(iz)*1d3*(-1d0*u(iz-1))/dz(iz)  &! adv(center) 
            + (-0.5d0*(poro(iz)*dif_m(iz)*(1d0-hx(iz)-bx(iz))+poro(iz-1)*dif_m(iz-1)*(1d0-hx(iz-1)-bx(iz-1)))*1d3*(-1d0)  &
                /(0.5d0*(dz(iz-1)+dz(iz)))   &
                )/dz(iz)   &
            )*cmx(iz-1)
    endif 
enddo 
ymx = -ymx

if (any(isnan(ymx))) then 
    print*,'NAN in ymx - pre (ch4so4calc)'
    open(unit=250,file='chk_ymx_pre.txt',status = 'unknown')
    do iz = 1, nmx
        write (250,*) ymx(iz)
    enddo
    close(250)
    if (any(isnan(dif_s))) print*,'cs nan'
    stop 
endif

if (any(isnan(amx))) then 
    print*,'NAN in amx (ch4so4calc)'
    open(unit=250,file='chk_amx_pre.txt',status = 'unknown')
    do iz = 1, nmx
        write (250,*) (amx(iz,iiz),iiz=1,nmx)
    enddo
    close(250)
    stop 
endif

call DGESV(nmx,int(1),amx,nmx,ipiv,ymx,nmx,info) 

if (any(isnan(ymx))) then 
    print*,'NAN in ymx - aft (ch4so4calc)'
    open(unit=250,file='chk_ymx_aft.txt',status = 'unknown')
    do iz = 1, nmx
        write (250,*) ymx(iz)
    enddo
    close(250)
    print*,csx
    print*
    print*,cmx
    print*
    print*,hx
    stop 
endif

if (info/=0) then 
    print*,'*** FATAL ERROR: STOP (ch4so4calc) ***'
    stop
endif 

do iz = 1, nz 
    row = 1+(iz-1)*nsp
    
    if (ymx(row)>10d0) then 
        csx(iz)=csx(iz)*1.5d0
    elseif (ymx(row)<-10d0) then 
        csx(iz)=csx(iz)*0.5d0
    else 
        csx(iz) = csx(iz)*exp(ymx(row))
    endif
    
    if (ymx(row+1)>10d0) then 
        cmx(iz)=cmx(iz)*1.5d0
    elseif (ymx(row+1)<-10d0) then 
        cmx(iz)=cmx(iz)*0.5d0
    else 
        cmx(iz) = cmx(iz)*exp(ymx(row+1))
    endif
    
    
    if (csx(iz)<conc_low) ymx(row) = 0d0
    if (cmx(iz)<conc_low) ymx(row+1) = 0d0
    
    if (csx(iz)<conc_low) csx(iz) = conc_low
    if (cmx(iz)<conc_low) cmx(iz)=conc_low
enddo

error = maxval(exp(abs(ymx))) - 1d0
itr = itr + 1

if (itr > 300) then 
    ! YK added 5/18/2020 
    error = 0d0
    do iz=1,nz
        row = 1+(iz-1)*nsp
        if (csx(iz)>1d-20) error = max(error,exp(abs(ymx(row)))-1d0)
        if (cmx(iz)>1d-20) error = max(error,exp(abs(ymx(row+1)))-1d0)
    enddo 
    if (error> tol) flg_esc = .true.
    print *, '------------------------------------------------'
    print *, '*** WARNING: Newton iteration reaches 300'
    print *, ' if errors are large only where concs. are small'
    print *, ' calculation proceeds without raising flag'
    print *, '------------------------------------------------'
    if (flg_esc) then 
        print*,'error',error,'cm_0',cm_0
        print*
        print*,'z                    ','csx                      ','err in csx                       '  &
            ,'cmx                     ','err in cmx                  '
        do iz =1,nz
            row = 1+(iz-1)*nsp
            print*,z(iz),csx(iz),exp(abs(ymx(row)))-1d0,cmx(iz),exp(abs(ymx(row+1)))-1d0
        enddo 
    endif 
    ! \EndAddition YK
    ! flg_esc = .true.
    exit newton 
endif 

if(loc_display)then
    print *, "============================================"
    print '(a12,i5,a12,e10.3e3)', 'SO4&CH4 itr:',itr,'/error:', error
    print *, '>>>>>>'
    print '(5(3x,a10))', 'z', 'csx','cmx','hx','bx'
    do iz=1,nz,nz/10
        print '(5(3x,e10.3e3))', z(iz),csx(iz),cmx(iz),hx(iz),bx(iz)
    enddo 
endif


enddo newton

selectcase (trim(kin_type))
    case('default')
        phih(:) = (ch*rho_h/mch4)*rxn_h(:)*(1d3*cmx(:)*mch4/rho_f - ceq(:))*hx(:)
        phib(:) = (cb*rho_b/mch4)*rxn_b(:)*(1d3*cmx(:)*mch4/rho_f - ceq(:))*bx(:)   
        dphih_dcmx(:) = (ch*rho_h/mch4)*rxn_h(:)*(1d3*1d0*mch4/rho_f - ceq(:))*hx(:)
        dphib_dcmx(:) = (cb*rho_b/mch4)*rxn_b(:)*(1d3*1d0*mch4/rho_f - ceq(:))*bx(:)   
    case('wallmann')
        phih(:) = (ch*rho_h/mch4)*rxn_h(:)*(1d3*cmx(:)*mch4/rho_f - ceq(:))  
        phib(:) = (cb*rho_b/mch4)*rxn_b(:)*(1d3*cmx(:)*mch4/rho_f - ceq(:))     
        dphih_dcmx(:) = (ch*rho_h/mch4)*rxn_h(:)*(1d3*1d0*mch4/rho_f - ceq(:))  
        dphib_dcmx(:) = (cb*rho_b/mch4)*rxn_b(:)*(1d3*1d0*mch4/rho_f - ceq(:))  
        where((1d3*cmx(:)*mch4/rho_f - ceq(:))<0d0)
            phih(:) = phih(:)*hx(:)
            phib(:) = phib(:)*bx(:)
            dphih_dcmx(:) = dphih_dcmx(:)*hx(:)
            dphib_dcmx(:) = dphib_dcmx(:)*bx(:)
        endwhere
    case('davie')
        phih(:) = (ch*rho_h/mch4)*rxn_h(:)*(1d3*cmx(:)*mch4/rho_f - ceq(:))
        phib(:) = (cb*rho_b/mch4)*rxn_b(:)*(1d3*cmx(:)*mch4/rho_f - ceq(:))   
        dphih_dcmx(:) = 0d0
        dphib_dcmx(:) = 0d0   
endselect

! if (bubble_cut) then 
    ! where (b(:)-bubble_thr > 0d0) 
        ! phib(:) = phib(:) - (cb*rho_b/mch4)*bubble_dec*(b(:)-bubble_thr)
    ! endwhere
! endif 

ch4flx = 0d0
so4flx = 0d0

do iz=1,nz
    if (iz==1) then 
        so4flx(itflx,iz) = so4flx(itflx,iz) + (  &! sulfate 
            - poro(iz)*1d3*(csx(iz)*(1d0-hx(iz)-bx(iz)) - cs(iz)*(1d0-h(iz)-b(iz)))/dt   &! time change rate 
            )
        so4flx(iadv,iz) = so4flx(iadv,iz) + (  &! sulfate 
            - adf(iz)*up(iz)*1d3*(csx(iz)*u(iz)-cs_0*u_0)/dz(iz)  &! adv(up) 
            - adf(iz)*dwn(iz)*1d3*(csx(iz+1)*u(iz+1)-csx(iz)*u(iz))/dz(iz)  &! adv(down) 
            - adf(iz)*cnr(iz)*1d3*(csx(iz+1)*u(iz+1)-cs_0*u_0)/dz(iz)  &! adv(center) 
            )
        so4flx(idif,iz) = so4flx(idif,iz) + (  &! sulfate 
            + (0.5d0*(poro(iz+1)*dif_s(iz+1)*(1d0-hx(iz+1)-bx(iz+1))+poro(iz)*dif_s(iz)*(1d0-hx(iz)-bx(iz))) &
                *1d3*(csx(iz+1)-csx(iz))/(0.5d0*(dz(iz+1)+dz(iz)))  &
            - 0.5d0*(poro(iz)*dif_s(iz)*(1d0-hx(iz)-bx(iz))+poro_0*dif_s(iz)*(1d0))*1d3*(csx(iz)-cs_0)  &
                /(0.5d0*(dz(iz)+dz(iz))))/dz(iz)   &
            )
        so4flx(irxn_om,iz) = so4flx(irxn_om,iz) + (  &! sulfate 
            - phi_s(iz)   &
            )
        so4flx(irxn_aom,iz) = so4flx(irxn_aom,iz) + (  &! sulfate 
            - poro(iz)*rxn_aom(iz)*1d3*csx(iz)*cmx(iz)*(1d0-hx(iz)-bx(iz))  &
            ! - poro(iz)*rxn_aom(iz)*1d3*csx(iz)*cmx(iz)  &
            )            
            
        ch4flx(itflx,iz) = ch4flx(itflx,iz) +  (  &! aq-methane 
            - poro(iz)*1d3*(cmx(iz)*(1d0-hx(iz)-bx(iz)) - cm(iz)*(1d0-h(iz)-b(iz)))/dt   &! time change rate 
            )
        ch4flx(iadv,iz) = ch4flx(iadv,iz) +  (  &! aq-methane 
            - adf(iz)*up(iz)*1d3*(cmx(iz)*u(iz)-cm_0*u_0)/dz(iz)  &! adv(up) 
            - adf(iz)*dwn(iz)*1d3*(cmx(iz+1)*u(iz+1)-cmx(iz)*u(iz))/dz(iz)  &! adv(down) 
            - adf(iz)*cnr(iz)*1d3*(cmx(iz+1)*u(iz+1)-cm_0*u_0)/dz(iz)  &! adv(center) 
            )
        ch4flx(idif,iz) = ch4flx(idif,iz) +  (  &! aq-methane 
            + (0.5d0*(poro(iz+1)*dif_m(iz+1)*(1d0-hx(iz+1)-bx(iz+1))+poro(iz)*dif_m(iz)*(1d0-hx(iz)-bx(iz)))  &
                *1d3*(cmx(iz+1)-cmx(iz))/(0.5d0*(dz(iz+1)+dz(iz)))  &
            - 0.5d0*(poro(iz)*dif_m(iz)*(1d0-hx(iz)-bx(iz))+poro_0*dif_m(iz)*(1d0))*1d3*(cmx(iz)-cm_0)  &
                /(0.5d0*(dz(iz)+dz(iz))))/dz(iz)   &
            )
        ch4flx(irxn_om,iz) = ch4flx(irxn_om,iz) +  (  &! aq-methane 
            + phi_m(iz)   &
            )
        ch4flx(irxn_aom,iz) = ch4flx(irxn_aom,iz) +  (  &! aq-methane 
            - poro(iz)*rxn_aom(iz)*1d3*csx(iz)*cmx(iz) *(1d0-hx(iz)-bx(iz)) &
            ! - poro(iz)*rxn_aom(iz)*1d3*csx(iz)*cmx(iz)  &
            )
        ch4flx(irxn_ah,iz) = ch4flx(irxn_ah,iz) +  (  &! aq-methane 
            - phih(iz)   &
            )
        ch4flx(irxn_ab,iz) = ch4flx(irxn_ab,iz) +  (  &! aq-methane 
            - phib(iz)   &
            )
            
    elseif (iz==nz) then 
        so4flx(itflx,iz) = so4flx(itflx,iz) + (  &! sulfate 
            - poro(iz)*1d3*(csx(iz)*(1d0-hx(iz)-bx(iz)) - cs(iz)*(1d0-h(iz)-b(iz)))/dt   &! time change rate 
            )
        so4flx(iadv,iz) = so4flx(iadv,iz) + (  &! sulfate 
            - adf(iz)*up(iz)*1d3*(csx(iz)*u(iz)-csx(iz-1)*u(iz-1))/dz(iz)  &! adv(up) 
            - adf(iz)*dwn(iz)*1d3*(csx(iz)*u_ext_loc-csx(iz)*u(iz))/dz(iz)  &! adv(down) 
            - adf(iz)*cnr(iz)*1d3*(csx(iz)*u_ext_loc-csx(iz-1)*u(iz-1))/dz(iz)  &! adv(center) 
            )
        so4flx(idif,iz) = so4flx(idif,iz) + (  &! sulfate 
            + (0d0  &
            - 0.5d0*(poro(iz)*dif_s(iz)*(1d0-hx(iz)-bx(iz))+poro(iz-1)*dif_s(iz-1)*(1d0-hx(iz-1)-bx(iz-1)))  &
                *1d3*(csx(iz)-csx(iz-1))/(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   &
            )
        so4flx(irxn_om,iz) = so4flx(irxn_om,iz) + (  &! sulfate 
            - phi_s(iz)   &
            )
        so4flx(irxn_aom,iz) = so4flx(irxn_aom,iz) + (  &! sulfate 
            - poro(iz)*rxn_aom(iz)*1d3*csx(iz)*cmx(iz) *(1d0-hx(iz)-bx(iz)) &
            ! - poro(iz)*rxn_aom(iz)*1d3*csx(iz)*cmx(iz)  &
            )
            
            
        ch4flx(itflx,iz) = ch4flx(itflx,iz) +  (  &! aq-methane 
            - poro(iz)*1d3*(cmx(iz)*(1d0-hx(iz)-bx(iz)) - cm(iz)*(1d0-h(iz)-b(iz)))/dt   &! time change rate 
            )
        ch4flx(iadv,iz) = ch4flx(iadv,iz) +  (  &! aq-methane 
            - adf(iz)*up(iz)*1d3*(cmx(iz)*u(iz)-cmx(iz-1)*u(iz-1))/dz(iz)  &! adv(up) 
            - adf(iz)*dwn(iz)*1d3*(cm_ext*u_ext_loc-cmx(iz)*u(iz))/dz(iz)  &! adv(down) 
            - adf(iz)*cnr(iz)*1d3*(cm_ext*u_ext_loc-cmx(iz-1)*u(iz-1))/dz(iz)  &! adv(center) 
            )
        ch4flx(idif,iz) = ch4flx(idif,iz) +  (  &! aq-methane 
            + (0d0  &
            - 0.5d0*(poro(iz)*dif_m(iz)*(1d0-hx(iz)-bx(iz))+poro(iz-1)*dif_m(iz-1)*(1d0-hx(iz-1)-bx(iz-1)))  &
                *1d3*(cmx(iz)-cmx(iz-1))/(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   &
            )
        ch4flx(irxn_om,iz) = ch4flx(irxn_om,iz) +  (  &! aq-methane 
            + phi_m(iz)   &
            )
        ch4flx(irxn_aom,iz) = ch4flx(irxn_aom,iz) +  (  &! aq-methane 
            - poro(iz)*rxn_aom(iz)*1d3*csx(iz)*cmx(iz) *(1d0-hx(iz)-bx(iz)) &
            ! - poro(iz)*rxn_aom(iz)*1d3*csx(iz)*cmx(iz)  &
            )
        ch4flx(irxn_ah,iz) = ch4flx(irxn_ah,iz) +  (  &! aq-methane 
            - phih(iz)   &
            )
        ch4flx(irxn_ab,iz) = ch4flx(irxn_ab,iz) +  (  &! aq-methane 
            - phib(iz)   &
            )
    
    else 
        so4flx(itflx,iz) = so4flx(itflx,iz) + (  &! sulfate 
            - poro(iz)*1d3*(csx(iz)*(1d0-hx(iz)-bx(iz)) - cs(iz)*(1d0-h(iz)-b(iz)))/dt   &! time change rate
            )
        so4flx(iadv,iz) = so4flx(iadv,iz) + (  &! sulfate 
            - adf(iz)*up(iz)*1d3*(csx(iz)*u(iz)-csx(iz-1)*u(iz-1))/dz(iz)  &! adv(up) 
            - adf(iz)*dwn(iz)*1d3*(csx(iz+1)*u(iz+1)-csx(iz)*u(iz))/dz(iz)  &! adv(down) 
            - adf(iz)*cnr(iz)*1d3*(csx(iz+1)*u(iz+1)-csx(iz-1)*u(iz-1))/dz(iz)  &! adv(center) 
            )
        so4flx(idif,iz) = so4flx(idif,iz) + (  &! sulfate 
            + (0.5d0*(poro(iz+1)*dif_s(iz+1)*(1d0-hx(iz+1)-bx(iz+1))+poro(iz)*dif_s(iz)*(1d0-hx(iz)-bx(iz)))  &
                *1d3*(csx(iz+1)-csx(iz))/(0.5d0*(dz(iz+1)+dz(iz)))  &
                - 0.5d0*(poro(iz)*dif_s(iz)*(1d0-hx(iz)-bx(iz))+poro(iz-1)*dif_s(iz-1)*(1d0-hx(iz-1)-bx(iz-1)))  &
                *1d3*(csx(iz)-csx(iz-1))/(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   &
            )
        so4flx(irxn_om,iz) = so4flx(irxn_om,iz) + (  &! sulfate 
            - phi_s(iz)   &
            )
        so4flx(irxn_aom,iz) = so4flx(irxn_aom,iz) + (  &! sulfate 
            - poro(iz)*rxn_aom(iz)*1d3*csx(iz)*cmx(iz)*(1d0-hx(iz)-bx(iz))  &
            ! - poro(iz)*rxn_aom(iz)*1d3*csx(iz)*cmx(iz)  &
            )
            
        ch4flx(itflx,iz) = ch4flx(itflx,iz) +  (  &! aq-methane 
            - poro(iz)*1d3*(cmx(iz)*(1d0-hx(iz)-bx(iz)) - cm(iz)*(1d0-h(iz)-b(iz)))/dt   &! time change rate 
            )
        ch4flx(iadv,iz) = ch4flx(iadv,iz) +  (  &! aq-methane 
            - adf(iz)*up(iz)*1d3*(cmx(iz)*u(iz)-cmx(iz-1)*u(iz-1))/dz(iz)  &! adv(up) 
            - adf(iz)*dwn(iz)*1d3*(cmx(iz+1)*u(iz+1)-cmx(iz)*u(iz))/dz(iz)  &! adv(down) 
            - adf(iz)*cnr(iz)*1d3*(cmx(iz+1)*u(iz+1)-cmx(iz-1)*u(iz-1))/dz(iz)  &! adv(center) 
            )
        ch4flx(idif,iz) = ch4flx(idif,iz) +  (  &! aq-methane 
            + (0.5d0*(poro(iz+1)*dif_m(iz+1)*(1d0-hx(iz+1)-bx(iz+1))+poro(iz)*dif_m(iz)*(1d0-hx(iz)-bx(iz)))  &
                *1d3*(cmx(iz+1)-cmx(iz))/(0.5d0*(dz(iz+1)+dz(iz)))  &
                - 0.5d0*(poro(iz)*dif_m(iz)*(1d0-hx(iz)-bx(iz))+poro(iz-1)*dif_m(iz-1)*(1d0-hx(iz-1)-bx(iz-1)))  &
                *1d3*(cmx(iz)-cmx(iz-1))/(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   &
            )
        ch4flx(irxn_om,iz) = ch4flx(irxn_om,iz) +  (  &! aq-methane 
            + phi_m(iz)   &
            )
        ch4flx(irxn_aom,iz) = ch4flx(irxn_aom,iz) +  (  &! aq-methane 
            - poro(iz)*rxn_aom(iz)*1d3*csx(iz)*cmx(iz)*(1d0-hx(iz)-bx(iz))  &
            ! - poro(iz)*rxn_aom(iz)*1d3*csx(iz)*cmx(iz)  &
            )
        ch4flx(irxn_ah,iz) = ch4flx(irxn_ah,iz) +  (  &! aq-methane 
            - phih(iz)   &
            )
        ch4flx(irxn_ab,iz) = ch4flx(irxn_ab,iz) +  (  &! aq-methane 
            - phib(iz)   &
            )
    endif 
    ch4flx(ires,iz) = sum(ch4flx(:,iz))
    so4flx(ires,iz) = sum(so4flx(:,iz))
enddo 

endsubroutine ch4so4calc
!**************************************************************************************************************************************

!**************************************************************************************************************************************
subroutine hydbubcalc(                                                      &
    nz,v,rho_f,ceq,h,b,rxn_b,rxn_h,rxn_hb,rxn_bh,v_sed,poro_0,hszflg,       &! input 
    z,dt,mch4,poro,dz,cmx,nflx,ch,cb,rho_h,rho_b,kin_type,zs,loc_display,   &! input 
    bx,hx,hflx,bflx,flg_esc                                                 &! output 
    )

implicit none 
! #include <defines.h>
integer,intent(in)::nz,nflx,hszflg
real,dimension(nz),intent(in)::v,ceq,b,z,h,poro,dz,cmx
real,dimension(nz),intent(inout)::rxn_h,rxn_b,rxn_hb,rxn_bh
real,intent(in)::rho_f,v_sed,poro_0,dt,ch,cb,rho_h,rho_b  &
    ,mch4,zs  
real,dimension(nz),intent(out)::bx,hx
real,dimension(nflx,nz),intent(out)::hflx,bflx
character(100),intent(in)::kin_type
logical,intent(in)::loc_display
logical,intent(inout)::flg_esc

! local variables
integer::nmx,iz,info,row,nsp,izs
real,allocatable::amx(:,:),ymx(:)
integer,allocatable::ipiv(:)
real u_ext_loc,error
integer itr,iiz
integer::itflx,iadv,idif,irxn_ah,irxn_ab,irxn_hb,irxn_om,irxn_aom,irxn_dec,ires
data itflx,iadv,idif,irxn_ah,irxn_ab,irxn_hb,irxn_om,irxn_aom,irxn_dec,ires/1,2,3,4,5,6,7,8,9,10/
real phi_h(nz),phi_b(nz),dphih_dh(nz),dphib_db(nz)
! logical::bubble_cut = .true.
logical::bubble_cut = .false.
! logical::non_local_transport = .true.
logical::non_local_transport = .false.
! real::bubdec = 1e-6
real::bubdec = 0.31536 ! /yr
real::bubthr = 1e-2
real bubredis(nz),bubrespon(nz),bub_coeff1(nz),bub_coeff2(nz,nz)
integer jz,col

! solving ch4 system including aqch4, so4 and solid ch4 hydrate and gas bubble ch4. 
! governing equations are: 
!
!     1. SO4
!     poro*(csx - cs)/dt = -(csx(iz)*u(iz)-csx(iz-1)*u(iz-1))/dz(iz) 
!         +  [0.5d0*(poro(iz+1)*dif(iz+1)*(1d0-hx(iz+1)-bx(iz+1))+poro(iz)*dif(iz)*(1d0-hx(iz)-bx(iz)))*(csx(iz+1)-csx(iz))
!              /(0.5d0*(dz(iz+1)+dz(iz)))
!         -  0.5d0*(poro(iz)*dif(iz)*(1d0-hx(iz)-bx(iz))+poro(iz-1)*dif(iz-1)*(1d0-hx(iz-1)-bx(iz-1)))*(csx(iz)-csx(iz-1))
!              /(0.5d0*(dz(iz)+dz(iz-1)))]/dz(iz)
!         - phi_s(iz) 
!         - poro(iz)*rxn_aom(iz)*1d3*csx(iz)*cmx(iz)
!
!     2. CH4(aq)
!     poro*(cmx - cm)/dt = -(cmx(iz)*u(iz)-cmx(iz-1)*u(iz-1))/dz(iz) 
!         +  [0.5d0*(poro(iz+1)*dif(iz+1)*(1d0-hx(iz+1)-bx(iz+1))+poro(iz)*dif(iz)*(1d0-hx(iz)-bx(iz)))*(cmx(iz+1)-cmx(iz))
!              /(0.5d0*(dz(iz+1)+dz(iz)))
!         -  0.5d0*(poro(iz)*dif(iz)*(1d0-hx(iz)-bx(iz))+poro(iz-1)*dif(iz-1)*(1d0-hx(iz-1)-bx(iz-1)))*(cmx(iz)-cmx(iz-1))
!              /(0.5d0*(dz(iz)+dz(iz-1)))]/dz(iz)
!         + phi_m(iz) 
!         - poro(iz)*rxn_aom(iz)*1d3*csx(iz)*cmx(iz)
!
!     3. CH4 hydrate(sld)
!     poro*(hx - h)/dt = - (poro(iz)*v(iz)*hx(iz) - poro(iz-1)*v(iz-1)*hx(iz-1))/dz(iz) 
!         + rxn_h(iz)*(cm(iz)*mch4/rho_f - ceq(iz))
!
!     4. CH4 bubble(gas but treated as if sld)
!     poro*(hx - h)/dt = - (poro(iz)*v(iz)*hx(iz) - poro(iz-1)*v(iz-1)*hx(iz-1))/dz(iz) 
!         + rxn_h(iz)*(cm(iz)*mch4/rho_f - ceq(iz))
!
!     Solve by Newton's method 
! 

selectcase (trim(kin_type))
    case('default') 
        phi_h(:) = rxn_h(:)*(1d3*cmx(:)*mch4/rho_f - ceq(:))*hx(:)
        phi_b(:) = rxn_b(:)*(1d3*cmx(:)*mch4/rho_f - ceq(:))*bx(:)
        dphih_dh(:) = rxn_h(:)*(1d3*cmx(:)*mch4/rho_f - ceq(:))
        dphib_db(:) = rxn_b(:)*(1d3*cmx(:)*mch4/rho_f - ceq(:))   
    case('wallmann')
        phi_h(:) = rxn_h(:)*(1d3*cmx(:)*mch4/rho_f - ceq(:))  
        phi_b(:) = rxn_b(:)*(1d3*cmx(:)*mch4/rho_f - ceq(:))  
        where((1d3*cmx(:)*mch4/rho_f - ceq(:))<0d0)
            phi_h(:) = phi_h(:)*hx(:)
            phi_b(:) = phi_b(:)*bx(:)
        endwhere
        dphih_dh(:) = rxn_h(:)*(1d3*cmx(:)*mch4/rho_f - ceq(:))  &
            *merge(1d0,0d0,(1d3*cmx(:)*mch4/rho_f - ceq(:))<0d0)
        dphib_db(:) = rxn_b(:)*(1d3*cmx(:)*mch4/rho_f - ceq(:))  &
            *merge(1d0,0d0,(1d3*cmx(:)*mch4/rho_f - ceq(:))<0d0)   
    case('davie')
        phi_h(:) = rxn_h(:)*(1d3*cmx(:)*mch4/rho_f - ceq(:))
        phi_b(:) = rxn_b(:)*(1d3*cmx(:)*mch4/rho_f - ceq(:))   
        dphih_dh(:) = 0d0
        dphib_db(:) = 0d0   
endselect

bubble_cut = par_sed_bubble_limit
bubthr = par_sed_bubble_threshold

if (hszflg==2) then 
    ! in the case with no hsz, there is no bubble formed from hydrate dissociation
    bubble_cut = .false.
endif 

if (bubble_cut) then
    bubredis = 0d0
    bubrespon = 0d0
    bub_coeff1 = 0d0
    bub_coeff2 = 0d0
    do iz=1,nz/2
        ! implementing disappearance of bubble as decay (no re-appearance of bubbles
        ! assumed to be immune to oxidation and transported out of system 
        if (.not.non_local_transport) then
            if (b(iz) > bubthr.and.iz/=1) then  
                bubredis(iz) =  bubredis(iz) + bubdec*(bx(iz)-bubthr)
                bub_coeff1(iz) = bub_coeff1(iz) - bubdec*(-bubthr)
                bub_coeff2(iz,iz) = bub_coeff2(iz,iz) - bubdec*(1d0)
            endif 
        
        ! implementing non-local transport !!!!
        elseif (non_local_transport) then 
            if (b(iz) > bubthr.and.iz/=1) then 
                bubredis(iz) =  bubredis(iz) + bubdec*(bx(iz)-bubthr)
                bubrespon(1:iz-1) = bubrespon(1:iz-1) &
                    + bubdec*(bx(iz)-bubthr)*exp(z(1:iz-1)**2/sum(z(1:iz-1)**2))/sum(exp(z(1:iz-1)**2/sum(z(1:iz-1)**2)))
                bub_coeff1(iz) = bub_coeff1(iz) - bubdec*(-bubthr)
                bub_coeff2(iz,iz) = bub_coeff2(iz,iz) - bubdec*(1d0)
                do jz=1,iz-1            
                    bub_coeff1(jz) = bub_coeff1(jz) &
                        + bubdec*(-bubthr)*exp(z(jz)**2/sum(z(1:iz-1)**2))/sum(exp(z(1:iz-1)**2/sum(z(1:iz-1)**2)))
                    bub_coeff2(jz,iz) = bub_coeff2(jz,iz) &
                        + bubdec*(1d0)*exp(z(jz)**2/sum(z(1:iz-1)**2))/sum(exp(z(1:iz-1)**2/sum(z(1:iz-1)**2)))
                enddo
            endif 
        endif 
    enddo
endif 

! izs = 0
! do iz=1,nz
    ! if (z(iz)<zs) izs = iz
! enddo 

! if (izs==0) izs = 1

nsp = 2
nmx = nz*nsp 

if (allocated(amx)) deallocate(amx)
if (allocated(ymx)) deallocate(ymx)
if (allocated(ipiv)) deallocate(ipiv)
allocate (amx(nmx,nmx),ymx(nmx),ipiv(nmx))


amx = 0d0
ymx = 0d0

do iz=1,nz
    row =  1 + (iz-1)*nsp 
    if (iz==1) then 
        ymx(row) = ymx(row) + (&!  hydrate
            - poro(iz)*(0d0 - h(iz))/dt   & 
            - (poro(iz)*v(iz)*0d0 - poro_0*v_sed*0d0)/dz(iz)   &
            + phi_h(iz)*merge(1d0,0d0,trim(kin_type)=='davie')  &
            + phi_h(iz)*merge(1d0,0d0,trim(kin_type)=='wallmann')  &
                *merge(1d0,0d0,.not.(1d3*cmx(iz)*mch4/rho_f - ceq(iz))<0d0)  &
            )
        amx(row,row) = amx(row,row) + (&!  hydrate
            - poro(iz)*(1d0)/dt   & 
            - (poro(iz)*v(iz)*1d0)/dz(iz)   &
            + dphih_dh(iz)   &
            - rxn_hb(iz)  &
            )
        amx(row,row+1) = amx(row,row+1) + (&!  hydrate
            + rxn_bh(iz)*(cb*rho_b/mch4)/(ch*rho_h/mch4)   &
            )
            
        ymx(row+1) = ymx(row+1) + (&!  hydrate
            - poro(iz)*(0d0 - b(iz))/dt   & 
            - (poro(iz)*v(iz)*0d0 - poro_0*v_sed*0d0)/dz(iz)   &
            + phi_b(iz)*merge(1d0,0d0,trim(kin_type)=='davie')  &
            + phi_b(iz)*merge(1d0,0d0,trim(kin_type)=='wallmann')  &
                *merge(1d0,0d0,.not.(1d3*cmx(iz)*mch4/rho_f - ceq(iz))<0d0)  &
            )
        amx(row+1,row) = amx(row+1,row) + (&!  hydrate
            + rxn_hb(iz)*(ch*rho_h/mch4)/(cb*rho_b/mch4)   &
            )
        amx(row+1,row+1) = amx(row+1,row+1) + (&!  hydrate
            - poro(iz)*(1d0)/dt   & 
            - (poro(iz)*v(iz)*1d0)/dz(iz)   &
            + dphib_db(iz)   &
            - rxn_bh(iz)   &
            )
    
    else 
        ymx(row) = ymx(row) + (&!  hydrate
            - poro(iz)*(0d0 - h(iz))/dt   & 
            - (poro(iz)*v(iz)*0d0 - poro(iz-1)*v(iz-1)*0d0)/dz(iz)   &
            + phi_h(iz)*merge(1d0,0d0,trim(kin_type)=='davie')  &
            + phi_h(iz)*merge(1d0,0d0,trim(kin_type)=='wallmann')  &
                *merge(1d0,0d0,.not.(1d3*cmx(iz)*mch4/rho_f - ceq(iz))<0d0)  &
            )
        amx(row,row) = amx(row,row) + (&!  hydrate
            - poro(iz)*(1d0)/dt   & 
            - (poro(iz)*v(iz)*1d0)/dz(iz)   &
            + dphih_dh(iz)   &
            - rxn_hb(iz)  &
            )!*hx(iz)
        amx(row,row+1) = amx(row,row+1) + (&!  hydrate
            + rxn_bh(iz)*(cb*rho_b/mch4)/(ch*rho_h/mch4)   &
            )
        amx(row,row-nsp) = amx(row,row-nsp) + (&!  hydrate
            - (-poro(iz-1)*v(iz-1)*1d0)/dz(iz)   &
            )!*hx(iz-1)
            
        ymx(row+1) = ymx(row+1) + (&!  hydrate
            - poro(iz)*(0d0 - b(iz))/dt   & 
            - (poro(iz)*v(iz)*0d0 - poro(iz-1)*v(iz-1)*0d0)/dz(iz)   &
            + phi_b(iz)*merge(1d0,0d0,trim(kin_type)=='davie')  &
            + phi_b(iz)*merge(1d0,0d0,trim(kin_type)=='wallmann')  &
                *merge(1d0,0d0,.not.(1d3*cmx(iz)*mch4/rho_f - ceq(iz))<0d0)  &
            )
        amx(row+1,row) = amx(row+1,row) + (&!  hydrate
            + rxn_hb(iz)*(ch*rho_h/mch4)/(cb*rho_b/mch4)   &
            )
        amx(row+1,row+1) = amx(row+1,row+1) + (&!  hydrate
            - poro(iz)*(1d0)/dt   & 
            - (poro(iz)*v(iz)*1d0)/dz(iz)   &
            + dphib_db(iz)   &
            - rxn_bh(iz)   &
            )!*bx(iz)
        amx(row+1,row+1-nsp) = amx(row+1,row+1-nsp) + (&!  hydrate
            - (-poro(iz-1)*v(iz-1)*1d0)/dz(iz)   &
            )!*bx(iz-1)
    endif 
    
    if (bubble_cut) then 
        ymx(row+1) = ymx(row+1) + bub_coeff1(iz)
        do jz = 1,iz
            col = 1 + (jz-1)*nsp 
            amx(col+1,row+1) = amx(col+1,row+1) + bub_coeff2(jz,iz)
        enddo
    endif 
enddo 
ymx = -ymx

if (any(isnan(ymx))) then 
    print*,'NAN in ymx - pre (hydbubcalc)'
    open(unit=250,file='chk_ymx_pre.txt',status = 'unknown')
    do iz = 1, nmx
        write (250,*) ymx(iz)
    enddo
    close(250)
    stop 
endif

if (any(isnan(amx))) then 
    print*,'NAN in amx (hydbubcalc)'
    open(unit=250,file='chk_amx_pre.txt',status = 'unknown')
    do iz = 1, nmx
        write (250,*) (amx(iz,iiz),iiz=1,nmx)
    enddo
    close(250)
    stop 
endif

call DGESV(nmx,int(1),amx,nmx,ipiv,ymx,nmx,info) 

if (any(isnan(ymx))) then 
    print*,'NAN in ymx - aft (hydbubcalc)'
    open(unit=250,file='chk_ymx_aft.txt',status = 'unknown')
    do iz = 1, nmx
        write (250,*) ymx(iz)
    enddo
    close(250)
    print*,cmx
    print*
    print*,hx
    print*
    print*,bx
    stop 
endif

if (info/=0) then 
    print*,'*** FATAL ERROR: STOP (hydbubcalc) ***'
    stop
endif 

do iz=1,nz 
    row =  1 + (iz-1)*nsp 
    hx(iz) = ymx(row)
    bx(iz) = ymx(row+1)

    ! if (hx(iz)<1d-300) hx(iz)=1d-300
    ! if (bx(iz)<1d-300) bx(iz)=1d-300

enddo 

selectcase (trim(kin_type))
    case('default') 
        phi_h(:) = rxn_h(:)*(1d3*cmx(:)*mch4/rho_f - ceq(:))*hx(:)
        phi_b(:) = rxn_b(:)*(1d3*cmx(:)*mch4/rho_f - ceq(:))*bx(:)
        dphih_dh(:) = rxn_h(:)*(1d3*cmx(:)*mch4/rho_f - ceq(:))
        dphib_db(:) = rxn_b(:)*(1d3*cmx(:)*mch4/rho_f - ceq(:))   
    case('wallmann')
        phi_h(:) = rxn_h(:)*(1d3*cmx(:)*mch4/rho_f - ceq(:))  
        phi_b(:) = rxn_b(:)*(1d3*cmx(:)*mch4/rho_f - ceq(:))  
        where((1d3*cmx(:)*mch4/rho_f - ceq(:))<0d0)
            phi_h(:) = phi_h(:)*hx(:)
            phi_b(:) = phi_b(:)*bx(:)
        endwhere
        dphih_dh(:) = rxn_h(:)*(1d3*cmx(:)*mch4/rho_f - ceq(:))  &
            *merge(1d0,0d0,(1d3*cmx(:)*mch4/rho_f - ceq(:))<0d0)
        dphib_db(:) = rxn_b(:)*(1d3*cmx(:)*mch4/rho_f - ceq(:))  &
            *merge(1d0,0d0,(1d3*cmx(:)*mch4/rho_f - ceq(:))<0d0)   
    case('davie')
        phi_h(:) = rxn_h(:)*(1d3*cmx(:)*mch4/rho_f - ceq(:))
        phi_b(:) = rxn_b(:)*(1d3*cmx(:)*mch4/rho_f - ceq(:))   
        dphih_dh(:) = 0d0
        dphib_db(:) = 0d0   
endselect

! if (bubble_cut) then 
    ! where (b(:)-bubthr > 0d0) 
        ! phi_b(:) = phi_b(:) - bubdec*(b(:) - bubthr)
    ! endwhere
! endif 

if (bubble_cut) then 

    bubredis = 0d0
    bubrespon = 0d0
    do iz=1,nz/2
        if (b(iz) > bubthr.and.iz/=1) then 
            bubredis(iz) =  bubredis(iz) + bubdec*(bx(iz)-bubthr)
            
            ! when non-local transport is implemented 
            if (non_local_transport) then 
                bubrespon(1:iz-1) = bubrespon(1:iz-1) &
                    + bubdec*(bx(iz)-bubthr)*exp(z(1:iz-1)**2/sum(z(1:iz-1)**2))/sum(exp(z(1:iz-1)**2/sum(z(1:iz-1)**2)))
            endif 
        endif 
    enddo
endif 

if(loc_display)then
    print *, "============================================"
    print '(a12)', 'hydrate-calculation'
    print *, '>>>>>>'
    print '(4(3x,a10))', 'z', 'cmx','hx','bx'
    do iz=1,nz,nz/10
        print '(4(3x,e10.3e3))', z(iz),cmx(iz),hx(iz),bx(iz)
    enddo 
endif

hflx = 0d0
bflx = 0d0


do iz=1,nz
    if (iz==1) then 
        hflx(itflx,iz) = hflx(itflx,iz) + (&!  hydrate
            - poro(iz)*(hx(iz) - h(iz))/dt   & 
            )
        hflx(iadv,iz) = hflx(iadv,iz) + (&!  hydrate
            - (poro(iz)*v(iz)*hx(iz) - poro_0*v_sed*0d0)/dz(iz)   &
            )
        hflx(irxn_ah,iz) = hflx(irxn_ah,iz) + (&!  hydrate
            + phi_h(iz)   &
            )
        hflx(irxn_hb,iz) = hflx(irxn_hb,iz) + (&!  hydrate
            - rxn_hb(iz)*hx(iz)   &
            + rxn_bh(iz)*bx(iz)*(cb*rho_b/mch4)/(ch*rho_h/mch4)   &
            )
            
        bflx(itflx,iz) = bflx(itflx,iz) + (&!  hydrate
            - poro(iz)*(bx(iz) - b(iz))/dt   & 
            )
        bflx(iadv,iz) = bflx(iadv,iz) + (&!  hydrate
            - (poro(iz)*v(iz)*bx(iz) - poro_0*v_sed*0d0)/dz(iz)   &
            )
        bflx(irxn_ab,iz) = bflx(irxn_ab,iz) + (&!  hydrate
            + phi_b(iz)   &
            )
        bflx(irxn_hb,iz) = bflx(irxn_hb,iz) + (&!  hydrate
            + rxn_hb(iz)*hx(iz)*(ch*rho_h/mch4)/(cb*rho_b/mch4)   &
            - rxn_bh(iz)*bx(iz)   &
            )
    
    else 
        hflx(itflx,iz) = hflx(itflx,iz) + (&!  hydrate
            - poro(iz)*(hx(iz) - h(iz))/dt   & 
            )
        hflx(iadv,iz) = hflx(iadv,iz) + (&!  hydrate
            - (poro(iz)*v(iz)*hx(iz) - poro(iz-1)*v(iz-1)*hx(iz-1))/dz(iz)   &
            )
        hflx(irxn_ah,iz) = hflx(irxn_ah,iz) + (&!  hydrate
            + phi_h(iz)  &
            )
        hflx(irxn_hb,iz) = hflx(irxn_hb,iz) + (&!  hydrate
            - rxn_hb(iz)*hx(iz)   &
            + rxn_bh(iz)*bx(iz)*(cb*rho_b/mch4)/(ch*rho_h/mch4)   &
            )
            
        bflx(itflx,iz) = bflx(itflx,iz) + (&!  hydrate
            - poro(iz)*(bx(iz) - b(iz))/dt   & 
            )
        bflx(iadv,iz) = bflx(iadv,iz) + (&!  hydrate
            - (poro(iz)*v(iz)*bx(iz) - poro(iz-1)*v(iz-1)*bx(iz-1))/dz(iz)   &
            )
        bflx(irxn_ab,iz) = bflx(irxn_ab,iz) + (&!  hydrate
            + phi_b(iz)  &
            )
        bflx(irxn_hb,iz) = bflx(irxn_hb,iz) + (&!  hydrate
            + rxn_hb(iz)*hx(iz)*(ch*rho_h/mch4)/(cb*rho_b/mch4)   &
            - rxn_bh(iz)*bx(iz)   &
            )
    endif 
    if (bubble_cut) then 
        ! non-local transport 
        if (non_local_transport) then 
            bflx(iadv,iz) = bflx(iadv,iz) + (&!  hydrate
                - bubredis(iz) + bubrespon(iz)   &
                )
        ! non-loca transport as decay
        elseif (.not. non_local_transport) then 
            bflx(irxn_dec,iz) = bflx(irxn_dec,iz) + (&!  hydrate
                - bubredis(iz) + bubrespon(iz)   &
                )
        endif 
    endif
    hflx(ires,iz) = sum(hflx(:,iz))
    bflx(ires,iz) = sum(bflx(:,iz))
enddo 

if (any(hx<0d0) .or. any(bx<0d0)) then 
    selectcase(trim(kin_type))
        case('default','wallmann')
            flg_esc = .true. 
            ! where(hx<0d0)hx=0d0
            ! where(bx<0d0)bx=0d0
        case('davie')
            ! in case of davie's kinetic expression,
            ! convergence is hard to achieve 
            ! without focing solution neglecting the problem of calculating negative conc. 
            where(hx<0d0)hx=0d0
            where(bx<0d0)bx=0d0
    endselect 
    ! stop
    return
endif 

if (any(hx>1d0) .or. any(bx>1d0)) then
    print*,'*** Hydrate or bubble vol% exceed 100%'
    flg_esc = .true.
    do iz=1,nz
        if(hx(iz)>1d0)then
            rxn_h(iz)=0d0
            rxn_bh(iz)=0d0
        endif 
        if(bx(iz)>1d0)then
            rxn_b(iz)=0d0
            rxn_hb(iz)=0d0
        endif
    enddo
    ! pause
    return
endif 

endsubroutine hydbubcalc
!**************************************************************************************************************************************

!**************************************************************************************************************************************
subroutine calc_zs( &
    nz,cm,cs,cm_0,cs_0,z,ztot  &! input
    ,zs   &! output
    )
implicit none
integer,intent(in)::nz
real,intent(in)::cm(nz),cs(nz),cm_0,cs_0,z(nz),ztot
real,intent(out)::zs
! local variables 
integer iz

if (cm_0>=cs_0) then 
    zs = 0d0
    return
else 
    do iz=1,nz
        if (cm(iz)>=cs(iz)) then 
            if (iz==1) then 
                zs=-(cm_0-cs_0)*z(iz)/(cm(iz)-cs(iz)-(cm_0-cs_0))
            else 
                zs=z(iz-1)-(cm(iz-1)-cs(iz-1))*(z(iz)-z(iz-1))/(cm(iz)-cs(iz)-(cm(iz-1)-cs(iz-1)))
            endif 
            return
        endif 
    enddo 
endif 
            
zs = ztot
return 

endsubroutine calc_zs
!**************************************************************************************************************************************

!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$  REACTIVE TRANSPORT (STEADY STATE)  $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

!**************************************************************************************************************************************
subroutine calcCH4sys_nohb(   &
    ceq,nz,imx,ifmx,itrnewmax,tor3,home,runid,latint,lonint,zs,rho_h,rho_b,ch,cb,dif_m,poro  &
    ,poro_ext,v_sed,v_ext,dz,outfilename,poro_0,procn,rho_f,phi_m,z,v,dt,loc_display   &
    ,flg_err &
    ) 
implicit none
integer,intent(in)::nz,imx,ifmx,itrnewmax,latint,lonint
real,intent(in)::ceq(nz),tor3,zs,rho_h,rho_b,ch,cb,dif_m(nz),poro(nz),poro_ext,v_sed,v_ext,dz(nz)  &
    ,poro_0,rho_f,phi_m(nz),z(nz),v(nz),dt
character(100),intent(in)::home,runid,outfilename
character(3),intent(in)::procn
logical,intent(in)::loc_display
logical,intent(out)::flg_err

! local variables
integer::nmx,itrnew,iz,info,row
real,allocatable::amx(:,:),ymx(:),emx(:)
integer,allocatable::ipiv(:)
real::cm_ext,cm(nz),b(nz),h(nz),rxn_b(nz),rxn_h(nz),hb(nz),u(nz) &
    ,cmx(Nz),hbx(nz),bx(nz),hx(nz),ux(nz),err3,rho_hb,chb

flg_err = .false.

nmx = nz - imx + 1

if (allocated(amx)) deallocate(amx)
if (allocated(ymx)) deallocate(ymx)
if (allocated(emx)) deallocate(emx)
if (allocated(ipiv)) deallocate(ipiv)
allocate (amx(nmx,nmx),ymx(nmx),emx(nmx),ipiv(nmx))

!  here start of iteration to determine methane profile 

cm_ext = ceq(nz)*1.00d0

cm = 0d0
b =  0d0
h =  0d0
! dt = 1d0
! rxn_scale = nz/100

rxn_b = 0d0
rxn_h = 0d0

hb(1:ifmx) = h(1:ifmx)
hb(ifmx+1:nz) = b(ifmx+1:nz)

u(:) = (1d0-poro_0)*(poro_ext/(1d0-poro_ext)- poro(:)*hb(:)/(1d0-poro(:))) *v_sed+poro_ext*v_ext  ! in cm/kyr
u(:) = u(:)*1d-2/1d3/365d0/24d0/60d0/60d0

cmx = cm

hbx = hb
hx = h
bx = b
ux = u
err3 = 1d4
itrnew = 0
do while (err3 > tor3)
    amx = 0d0
    ymx = 0d0

    itrnew = itrnew + 1

    if (itrnew > itrnewmax) then 

        ! open(250,file=trim(adjustl(home))//'global_out'//trim(adjustl(procn))&
        ! //'_err_itrnew-'//trim(adjustl(runid))//'.txt',status='unknown',position='append')
        ! write(250,*) latint, lonint, zs
        ! close(250)
        ! stop
        flg_err = .true.
        return
    endif

    do iz = imx, nz
        row = iz - imx + 1
        if (iz <= ifmx) then 
            chb = ch
            rho_hb = rho_h
        else
            chb = cb
            rho_hb = rho_b
        end if 

        if (iz == nz) then  
            if (cm_ext == 0d0) then  ! no flux toward bottom (cm(iz+1) = cm(iz))
                ymx(row) = (-(dif_m(iz-1)+dif_m(iz))/2d0*(poro(iz-1)+poro(iz))/2d0*(1d0-(bx(iz-1)+bx(iz))/2d0)*(cmx(iz)-cmx(iz-1)) &
                        /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  &
                    + (cmx(iz)-rho_b/rho_f*cb)*rxn_b(iz)*(cmx(iz)-ceq(iz)) &
                    - ux(iz)*(cmx(iz)-cmx(iz-1))/2d0/dz(iz)  &
                    - poro(iz)*(1d0-bx(iz))*(cmx(iz)-cm(iz))/dt + phi_m(iz)
                amx(row,row) = (-(dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz-1)+poro(iz))/2d0*(1d0-(bx(iz-1)+bx(iz))/2d0)&
                        /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  &   
                    + rxn_b(iz)*(cmx(iz)-ceq(iz))    &
                    + (cmx(iz)-rho_b/rho_f*cb)*rxn_b(iz)   &
                    - ux(iz)/2d0/dz(iz)  &
                    - poro(iz)*(1d0-bx(iz))*1d0/dt                 
                amx(row, row-1) = (-(dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz-1)+poro(iz))/2d0*(1d0-(bx(iz-1)+bx(iz))/2d0)*(-1d0)&
                        /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  &
                    - ux(iz)*(-1d0)/2d0/dz(iz)
            else ! cm(iz+1) = cm_ext
                ! ymx(row) = (dif_m*poro(iz)*(1d0-bx(iz))*(cm_ext-cmx(iz))/dz  &
                    ! - dif_m*(poro(iz)+poro(iz-1))/2d0*(1d0-(bx(iz)+bx(iz-1))/2d0)*(cmx(iz)-cmx(iz-1))/dz)/dz  &
                    ! + (cmx(iz)-rho_b/rho_f*cb)*rxn_b(iz)*(cmx(iz)-ceq(iz)) &
                    ! - ux(iz)*(cm_ext-cmx(iz-1))/2d0/dz  &
                    ! - poro(iz)*(1d0-bx(iz))*(cmx(iz)-cm(iz))/dt + phi_m(iz)
                ! amx(row,row) =  (dif_m*poro(iz)*(1d0-bx(iz))*(1d0)/dz  &
                    ! - dif_m*(poro(iz)+poro(iz-1))/2d0*(1d0-(bx(iz)+bx(iz-1))/2d0)*(1d0)/dz)/dz  &  
                    ! +rxn_b(iz)*(cmx(iz)-ceq(iz))    &
                    ! +(cmx(iz)-rho_b/rho_f*cb)*rxn_b(iz)    &
                    ! -poro(iz)*(1d0-bx(iz))*1d0/dt                      
                ! amx(row, row-1) = (- dif_m*(poro(iz)+poro(iz-1))/2d0*(1d0-(bx(iz)+bx(iz-1))/2d0)*(-1d0)/dz)/dz  &
                    ! - ux(iz)*(-1d0)/2d0/dz
    
                !!! following is used because above equations do not converge 
                ymx(row) = 1.0d0*(cmx(iz)-cm_ext)
                amx(row,row) = 1.0d0
            end if 
        else if (iz == imx) then 
            if (imx/=1) then 
                ymx(row) = ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hx(iz)+hx(iz+1))/2d0)*(cmx(iz+1)-cmx(iz))&
                        /(0.5d0*(dz(iz)+dz(iz+1)))  &
                    - (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hx(iz)+hx(iz-1))/2d0)*(cmx(iz)-cm(iz-1))&
                        /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  &
                    + (cmx(iz)-rho_h/rho_f*ch)*rxn_h(iz)*(cmx(iz)-ceq(iz)) &
                    - ux(iz)*(cmx(iz+1)-cm(iz-1))/2d0/dz(iz)  &
                    - poro(iz)*(1d0-hx(iz))*(cmx(iz)-cm(iz))/dt + phi_m(iz)
                amx(row,row) =  ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hx(iz)+hx(iz+1))/2d0)*(-1d0)&
                        /(0.5d0*(dz(iz)+dz(iz+1)))  &
                    - (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hx(iz)+hx(iz-1))/2d0)*(1d0)&
                        /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  &  
                    +rxn_h(iz)*(cmx(iz)-ceq(iz))    &
                    +(cmx(iz)-rho_h/rho_f*ch)*rxn_h(iz)    &
                    -poro(iz)*(1d0-hx(iz))*1d0/dt             
                amx(row, row+1) =  ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hx(iz)+hx(iz+1))/2d0)*(1d0)&
                        /(0.5d0*(dz(iz)+dz(iz+1))))/dz(iz)  &
                    -ux(iz)/2d0/dz(iz)         
            elseif (imx==1) then 
                ymx(row) = ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hx(iz)+hx(iz+1))/2d0)*(cmx(iz+1)-cmx(iz))&
                        /(0.5d0*(dz(iz)+dz(iz+1)))  &
                    - dif_m(iz)*(poro(iz)+poro_0)/2d0*(1d0-(hx(iz)+0d0)/2d0)*(cmx(iz)-0d0)/dz(iz))/dz(iz)  &
                    + (cmx(iz)-rho_h/rho_f*ch)*rxn_h(iz)*(cmx(iz)-ceq(iz)) &
                    - ux(iz)*(cmx(iz+1)-0d0)/2d0/dz(iz)  &
                    - poro(iz)*(1d0-hx(iz))*(cmx(iz)-cm(iz))/dt + phi_m(iz)
                amx(row,row) =  ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hx(iz)+hx(iz+1))/2d0)*(-1d0)&
                        /(0.5d0*(dz(iz)+dz(iz+1)))  &
                    - dif_m(iz)*(poro(iz)+poro_0)/2d0*(1d0-(hx(iz)+0d0)/2d0)*(1d0)/dz(iz))/dz(iz)  &  
                    +rxn_h(iz)*(cmx(iz)-ceq(iz))    &
                    +(cmx(iz)-rho_h/rho_f*ch)*rxn_h(iz)    &
                    -poro(iz)*(1d0-hx(iz))*1d0/dt             
                amx(row, row+1) =  ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hx(iz)+hx(iz+1))/2d0)*(1d0)&
                        /(0.5d0*(dz(iz)+dz(iz+1))))/dz(iz)  &
                    -ux(iz)/2d0/dz(iz)
            endif 

        else if ((iz < ifmx).and.(iz> imx)) then 
            ymx(row) = ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hx(iz)+hx(iz+1))/2d0)*(cmx(iz+1)-cmx(iz))&
                    /(0.5d0*(dz(iz)+dz(iz+1)))  &
                - (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hx(iz)+hx(iz-1))/2d0)*(cmx(iz)-cmx(iz-1))&
                    /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  &
                + (cmx(iz)-rho_h/rho_f*ch)*rxn_h(iz)*(cmx(iz)-ceq(iz)) &
                - ux(iz)*(cmx(iz+1)-cmx(iz-1))/2d0/dz(iz)  &
                - poro(iz)*(1d0-hx(iz))*(cmx(iz)-cm(iz))/dt + phi_m(iz)
            amx(row,row) =  ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hx(iz)+hx(iz+1))/2d0)*(-1d0)&
                    /(0.5d0*(dz(iz)+dz(iz+1)))  &
                - (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hx(iz)+hx(iz-1))/2d0)*(1d0)&
                    /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  &  
                +rxn_h(iz)*(cmx(iz)-ceq(iz))    &
                +(cmx(iz)-rho_h/rho_f*ch)*rxn_h(iz)    &
                -poro(iz)*(1d0-hx(iz))*1d0/dt             
            amx(row, row+1) =  ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hx(iz)+hx(iz+1))/2d0)*(1d0)&
                    /(0.5d0*(dz(iz)+dz(iz+1))))/dz(iz)  &
                -ux(iz)/2d0/dz(iz)            
            amx(row, row-1) = (- (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hx(iz)+hx(iz-1))/2d0)*(-1d0)&
                    /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  &
                - ux(iz)*(-1d0)/2d0/dz(iz)

        else if ((iz < nz).and.(iz> ifmx)) then 
            ymx(row) = ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(bx(iz)+bx(iz+1))/2d0)*(cmx(iz+1)-cmx(iz))&
                    /(0.5d0*(dz(iz)+dz(iz+1)))  &
                - (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(bx(iz)+bx(iz-1))/2d0)*(cmx(iz)-cmx(iz-1))&
                    /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  &
                + (cmx(iz)-rho_b/rho_f*cb)*rxn_b(iz)*(cmx(iz)-ceq(iz)) &
                - ux(iz)*(cmx(iz+1)-cmx(iz-1))/2d0/dz(iz)  &
                - poro(iz)*(1d0-bx(iz))*(cmx(iz)-cm(iz))/dt + phi_m(iz)
            amx(row,row) =  ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(bx(iz)+bx(iz+1))/2d0)*(-1d0)&
                    /(0.5d0*(dz(iz)+dz(iz+1)))  &
                - (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(bx(iz)+bx(iz-1))/2d0)*(1d0)&
                    /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  &  
                +rxn_b(iz)*(cmx(iz)-ceq(iz))    &
                +(cmx(iz)-rho_b/rho_f*cb)*rxn_b(iz)    &
                -poro(iz)*(1d0-bx(iz))*1d0/dt             
            amx(row, row+1) =  ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(bx(iz)+bx(iz+1))/2d0)*(1d0)&
                    /(0.5d0*(dz(iz)+dz(iz+1))))/dz(iz)  &
                -ux(iz)/2d0/dz(iz)            
            amx(row, row-1) = (- (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(bx(iz)+bx(iz-1))/2d0)*(-1d0)&
                    /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  &
                - ux(iz)*(-1d0)/2d0/dz(iz)

        else if ( iz == ifmx) then 
        ymx(row) = ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hx(iz)+bx(iz+1)+bx(iz))/2d0)*(cmx(iz+1)-cmx(iz))&
                /(0.5d0*(dz(iz)+dz(iz+1)))  &
            - (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hx(iz)+hx(iz-1)+bx(iz))/2d0)*(cmx(iz)-cmx(iz-1))&
                /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  &
            + (cmx(iz)-rho_h/rho_f*ch)*rxn_h(iz)*(cmx(iz)-ceq(iz)) &
            + (cmx(iz)-rho_b/rho_f*cb)*rxn_b(iz)*(cmx(iz)-ceq(iz)) &
            - poro(iz)*(1d0-hx(iz)-bx(iz))*(cmx(iz)-cm(iz))/dt &
            + phi_m(iz)      &
            - ux(iz)*(cmx(iz+1)-cmx(iz-1))/2d0/dz(iz)  
            ! -(dif_m*(poro(iz)+poro(iz+1))/2d0*(1d0-(bx(iz)+bx(iz+1))/2d0)*(cmx(iz+1)-cmx(iz))/dz  &
            ! - dif_m*(poro(iz)+poro(iz-1))/2d0*(1d0-(bx(iz)+bx(iz-1))/2d0)*(cmx(iz)-cmx(iz-1))/dz)/dz  &
            ! - rho_b/rho_f*(cmx(iz)-cb)*rxn_b(iz)*(cmx(iz)-ceq(iz)) &
            ! + poro(iz)*(1d0-bx(iz))*(cmx(iz)-cm(iz))/dt 
        amx(row,row) =  ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hx(iz)+bx(iz+1)+bx(iz))/2d0)*(-1d0)&
                /(0.5d0*(dz(iz)+dz(iz+1)))  &
            - (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hx(iz)+hx(iz-1)+bx(iz))/2d0)*(1d0)&
                /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  &  
            +rxn_h(iz)*(cmx(iz)-ceq(iz))    &
            +(cmx(iz)-rho_h/rho_f*ch)*rxn_h(iz)    &
            +rxn_b(iz)*(cmx(iz)-ceq(iz))    &
            +(cmx(iz)-rho_b/rho_f*cb)*rxn_b(iz)    &
            -poro(iz)*(1d0-hx(iz)-bx(iz))*1d0/dt     
            ! - (dif_m*(poro(iz)+poro(iz+1))/2d0*(1d0-(bx(iz)+bx(iz+1))/2d0)*(-1d0)/dz  &
            ! - dif_m*(poro(iz)+poro(iz-1))/2d0*(1d0-(bx(iz)+bx(iz-1))/2d0)*(1d0)/dz)/dz  &  
            ! -rho_b/rho_f*rxn_b(iz)*(cmx(iz)-ceq(iz))    &
            ! -rho_b/rho_f*(cmx(iz)-cb)*rxn_b(iz)    &
            ! +poro(iz)*(1d0-bx(iz))*1d0/dt                     
        amx(row, row+1) =  ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hx(iz)+bx(iz+1)+bx(iz))/2d0)*(1d0)&
                /(0.5d0*(dz(iz)+dz(iz+1))))/dz(iz)  &
            - ux(iz)*(1d0)/2d0/dz(iz)
            ! - (dif_m*(poro(iz)+poro(iz+1))/2d0*(1d0-(bx(iz)+bx(iz+1))/2d0)*(1d0)/dz)/dz  
        amx(row, row-1) = (- (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hx(iz)+hx(iz-1)+bx(iz))/2d0)*(-1d0)&
                /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  &
            - ux(iz)*(-1d0)/2d0/dz(iz)
            ! - (- dif_m*(poro(iz)+poro(iz-1))/2d0*(1d0-(bx(iz)+bx(iz-1))/2d0)*(-1d0)/dz)/dz 

        end if 
    end do 

    ymx = -ymx

    ! open (unit =200,file='test-mtx1.txt',status='replace')
    ! open (unit =300,file='test-mtx2.txt',status='replace')
    ! do iz = imx, nz
    ! row = iz - imx + 1
    ! write(200,*) (amx(row,col),col = 1,nmx)
    ! write(300,*) ymx(row)
    ! end do 
    ! close(200)
    ! close(300)

    call DGESV(nmx,int(1),amx,nmx,ipiv,ymx,nmx,info) 

    emx = 0d0
    do iz = imx, nz
        row = iz - imx + 1
        cmx(iz) = cmx(iz)+ymx(row)
        if (abs(cmx(iz))>1d-10) emx(row) = abs(ymx(row)/cmx(iz))
    end do 

    err3 = maxval(emx)
    if (loc_display) then 
        print *, "============================================"
        print '(a12,i5,a12,e9.3e2)', "iteration:",itrnew,"/error:", err3
        print *, '>>>>>>'
        print '(5(3x,a9))', 'z', 'cmx','hx','bx','ux'
        do iz=1,nz,nz/10
            print '(5(3x,e9.3e2))', z(iz), cmx(iz),hx(iz),bx(iz),ux(iz)
        enddo 
        ! print *, (z(iz), iz = 1,nz, nz/10 )
        ! print *,(cmx(iz), iz = 1,nz, nz/10)
        ! print *,(hx(iz), iz = 1,nz, nz/10)
        ! print *,(bx(iz), iz = 1,nz, nz/10)
        ! print *,(ux(iz), iz = 1,nz, nz/10)
    endif
    ! if (any(hbx < 0)) then 
    ! print *, "negative hydrate or gas at interation No.", itr

    ! open (unit =200,file='test-calc_stoped.txt',status='replace')
    ! do iz = 1, nz
    ! write(200,*) z(iz), cm(iz),ceq(iz),h(iz),b(iz), u(iz), v(iz)
    ! end do 
    ! close(200)
    ! stop
    ! end if 

end do 

open(unit=200,file=trim(adjustl(home))//trim(adjustl(runid))//'/profiles/'//trim(adjustl(outfilename))&
    //'/ch4_pre.txt',status='replace')
do iz = 1, nz
    write(200,*) z(iz), cmx(iz),ceq(iz),hx(iz),bx(iz), ux(iz), v(iz), rxn_h(iz), rxn_b(iz)
end do 
close(200)

if (.not.any(cmx(:)-ceq(:)>0d0)) then 
    print *, "... hydrate should not be formed ..."
    open(unit=250,file=trim(adjustl(home))//trim(adjustl(runid))//'/res/pre_nohydrate-'//trim(adjustl(procn))&
        //'.txt',status='unknown',position='append')
    write(250,*) latint, lonint
    close(250)
endif

endsubroutine calcCH4sys_nohb
!**************************************************************************************************************************************

!**************************************************************************************************************************************
subroutine calcCH4sys_3phs(   &
    ceq,nz,imx,ifmx,itrnewmax,tor3,home,runid,latint,lonint,zs,rho_h,rho_b,ch,cb,dif_m,poro  &
    ,poro_ext,v_sed,v_ext,dz,outfilename,poro_0,procn,rho_f,phi_m,z,v   &
    ,itrmax,maxrate,rxn_bh_max,rxn_hb_max,ucnv,dif_s,dt,loc_display  &
    ,cm,h,b,u,itr,hb,rxn_h,rxn_b,err3,sdif_bot,phi_hb,hx,flg_err,aomflx_ch4            & ! output
    ) 
implicit none
integer,intent(in)::nz,imx,ifmx,itrmax,latint,lonint,itrnewmax
real,intent(in)::ceq(nz),tor3,zs,rho_h,rho_b,ch,cb,dif_m(nz),poro(nz),poro_ext,v_sed,v_ext,dz(nz)  &
    ,poro_0,rho_f,phi_m(nz),z(nz),v(nz),maxrate,rxn_bh_max,rxn_hb_max,ucnv,dif_s(nz),dt  
character(100),intent(in)::home,runid,outfilename
character(3),intent(in)::procn
real,intent(out)::cm(nz),h(nz),b(nz),u(nz),hb(nz),rxn_b(nz),rxn_h(nz),err3,sdif_bot,phi_hb(nz),hx(nz),aomflx_ch4
logical,intent(in)::loc_display
integer,intent(out)::itr
logical,intent(out)::flg_err

! local variables
integer::nmx,iz,info,row,itrnew,ifmx_tmp,col
real,allocatable::amx(:,:),ymx(:),emx(:)
integer,allocatable::ipiv(:)
real::cm_ext,cmx(Nz),hbx(nz),bx(nz),ux(nz),rho_hb,chb
real::mhinv,mhinve,mbinv,mbinve,rxn(nz),rxn_hb,rxn_bh
real::phi_b(nz),phi_h(nz)
real::prodh,prodb
real::advlossc,diflossc,hydlossc,gaslossc,bioprodc,cmmb,hmb,bmb
real::residc
real::mhinvc,mbinvc
real::rxn_bh_ex,expbb
! real::sdif_bot ! should be output?

flg_err = .false. 

nmx = 4*nz

if (allocated(amx)) deallocate(amx)
if (allocated(ymx)) deallocate(ymx)
if (allocated(emx)) deallocate(emx)
if (allocated(ipiv)) deallocate(ipiv)
allocate (amx(nmx,nmx),ymx(nmx),emx(nmx),ipiv(nmx))

!  here start of iteration to determine methane profile 

cm_ext = 0d0
! #ifdef satupflow
! print*,'assuming saturated upflow'
! cm_ext = ceq(nz)*1.00d0
! #endif
cm = 0d0
! cm = ceq ! 
b =  0d0
h =  0d0
! dt = 1d0
! rxn_scale = nz/100

! do iz = 1, nz
  ! rxn(iz) = 1d-8*exp(-(nz-iz)/rxn_scale)
! end do 

mhinv = 0d0
mbinv = 0d0

mhinve = 1d10
mbinve = 1d10

rxn = 0d0
rxn(ifmx+1:nz) = 1d-8

rxn_b = 0d0
rxn_b(ifmx+1:nz) = 1d-8
rxn_b(ifmx:nz) = 1d-8  ! better ?
rxn_h = 0d0
rxn_b(imx:ifmx) = 1d-8  !  added 

hb(1:ifmx) = h(1:ifmx)
hb(ifmx+1:nz) = b(ifmx+1:nz)

rxn_hb = 1d-8  ! why did I set rates for rxn_bh ?
rxn_bh = 1d-8  !  added 3/11/2018

opt:do itr = 1, itrmax

! if (.not.any(hb<0)) then 
    ! if (itr/=1) then 
        ! rxn(1:nz-1) = rxn(2:nz)
        ! rxn(nz) = 1d-8
    ! end if 
! else 
    ! do iz = 1, nz
        ! if (hb(iz)<0) rxn(iz) = rxn(iz)/1d3
    ! end do 
! end if 

! open (unit =200, file='./output/'//trim(adjustl(outfilename))//'-uf.txt',status='replace')
u(:) = (1d0-poro_0)*(poro_ext/(1d0-poro_ext)- poro(:)*hb(:)/(1d0-poro(:))) *v_sed+poro_ext*v_ext
u(:) = u(:)*1d-2/1d3/365d0/24d0/60d0/60d0
! close(200)

! #ifdef fixmid
! rxn_h(ifmx)=0d0  ! as assumed in Davie and Buffett (2003) 
! rxn_b(ifmx)=0d0
! #endif

! #ifdef fixmid2
! rxn_h(ifmx)=0d0  ! as assumed in Davie and Buffett (2003) 
! rxn_b(ifmx)=0d0
! #endif

cmx = cm

hbx = hb
hx = h
bx = b
ux = u
err3 = 1d4
itrnew = 0
newton:do while (err3 > tor3)
amx = 0d0
ymx = 0d0

phi_b = 0d0
phi_h = 0d0
phi_hb = 0d0

prodh = 0d0
prodb = 0d0

itrnew = itrnew + 1

if (itrnew > itrnewmax) then 
  
    ! open(unit=250,file=trim(adjustl(outdir))//trim(adjustl(runid))//'/res/err_itrnew-'//trim(adjustl(procn))&
        ! //'.txt',status='unknown',position='append')
    ! write(250,*) latint, lonint, zs
    ! close(250)
    ! stop
    flg_err = .true.
    return
endif

! select case (imx < ifmx) 

! case(.true.)
ifmx_tmp = ifmx
if (.not. imx < ifmx) then 
    ifmx_tmp =  -1
endif 

do iz = 1, nz
    row = (iz-1)*4 + 1
    if (iz <= ifmx) then 
        chb = ch
        rho_hb = rho_h
    else
        chb = cb
        rho_hb = rho_b
    end if 

    if (iz < imx) then 
        ymx(row) = 1d0*(cmx(iz)-0d0)
        amx(row,row) = 1d0

        ymx(row+1) = 1d0*(hx(iz)-0d0)
        amx(row+1,row+1) = 1d0
        ymx(row+2) = 1d0*(bx(iz)-0d0)
        amx(row+2,row+2) = 1d0
        ymx(row+3) = 1d0*(ux(iz) - u(iz))
        amx(row+3,row+3) = 1d0

    else if (iz == imx) then 
        if (imx/=1) then 
            ymx(row) = ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hx(iz)+hx(iz+1))/2d0)*(cmx(iz+1)-cmx(iz))  &
                    /(0.5d0*(dz(iz)+dz(iz+1)))  &
                - (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hx(iz)+0d0)/2d0)*(cmx(iz)-0d0) &
                    /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  &
                + (-rho_h/rho_f*ch)*rxn_h(iz)*(cmx(iz)-ceq(iz)) &
                - (cmx(iz+1)*ux(iz+1)-0d0)/2d0/dz(iz)  &
                ! - poro(iz)*((1d0-hx(iz))*cmx(iz)-(1d0-h(iz))*cm(iz))/dt &
                + phi_m(iz)
            amx(row,row) =  ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hx(iz)+hx(iz+1))/2d0)*(-1d0)&
                    /(0.5d0*(dz(iz)+dz(iz+1)))  &
                - (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hx(iz)+hx(iz-1))/2d0)*(1d0)&
                    /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  &
                ! -poro(iz)*((1d0-hx(iz))*1d0)/dt  &   
                +(-rho_h/rho_f*ch)*rxn_h(iz)           
            amx(row, row+4) =  ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hx(iz)+hx(iz+1))/2d0)*(1d0)&
                    /(0.5d0*(dz(iz)+dz(iz+1))))/dz(iz)  &
                -ux(iz+1)/2d0/dz(iz)            
            amx(row, row-4) = 0d0

            amx(row, row+1) =    &
                ! - poro(iz)*((-1d0)*cmx(iz))/dt    &
                ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(-1d0/2d0)*(cmx(iz+1)-cmx(iz))/(0.5d0*(dz(iz)+dz(iz+1)))  &
                - (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(-1d0/2d0)*(cmx(iz)-0d0)/(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   
            amx(row, row-4+1) = 0d0  
            amx(row, row+4+1) = ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(-1d0/2d0)*(cmx(iz+1)-cmx(iz))&
                    /(0.5d0*(dz(iz)+dz(iz+1))))/dz(iz)  
            amx(row, row+4+3) = -(cmx(iz+1))/2d0/dz(iz)

            ymx(row+1) = rxn_h(iz)*(cmx(iz)-ceq(iz)) &
                ! - poro(iz)*(hx(iz)-h(iz))/dt &
                -(v(iz)*poro(iz)*hx(iz)-0d0)/dz(iz)
            amx(row+1, row) = rxn_h(iz) 
            amx(row+1, row+1) = &  
                ! -poro(iz)/dt  &
                -(v(iz)*poro(iz))/dz(iz)
                ! amx(row+1, row-4+1) = -(-v(iz-1)*poro(iz-1))/dz


            ymx(row+2) = 1d0*(bx(iz)-0d0)
            amx(row+2, row+2) = 1d0

            ymx(row+3) = ux(iz) - ucnv*((1d0-poro_0)*(poro_ext/(1d0-poro_ext)- poro(iz)*hx(iz)/(1d0-poro(iz)))   &
                *v_sed + poro_ext*v_ext)
            amx(row+3,row+3) = 1d0
            amx(row+3,row+1) = - (1d0-poro_0)*(- poro(iz)/(1d0-poro(iz))) *v_sed*ucnv
            
            ! print*,ymx(row+3),ux(iz),ucnv,poro_0,poro(iz),v_sed,poro_ext,v_ext,hx(iz)
            
        else if (imx==1) then 
            ymx(row) = ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hx(iz)+hx(iz+1))/2d0)*(cmx(iz+1)-cmx(iz))&
                    /(0.5d0*(dz(iz)+dz(iz+1)))  &
                - dif_m(iz)*(poro(iz)+poro_0)/2d0*(1d0-(hx(iz)+0d0)/2d0)*(cmx(iz)-0d0)/dz(iz))/dz(iz)  &
                + (-rho_h/rho_f*ch)*rxn_h(iz)*(cmx(iz)-ceq(iz)) &
                - (cmx(iz+1)*ux(iz+1)-0d0)/2d0/dz(iz)  &
                ! - poro(iz)*((1d0-hx(iz))*cmx(iz)-(1d0-h(iz))*cm(iz))/dt &
                + phi_m(iz)
            amx(row,row) =  ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hx(iz)+hx(iz+1))/2d0)*(-1d0)&
                    /(0.5d0*(dz(iz)+dz(iz+1)))  &
                - dif_m(iz)*(poro(iz)+poro_0)/2d0*(1d0-(hx(iz)+0d0)/2d0)*(1d0)/dz(iz))/dz(iz)  &
                ! -poro(iz)*((1d0-hx(iz))*1d0)/dt  &   
                +(-rho_h/rho_f*ch)*rxn_h(iz)           
            amx(row, row+4) =  ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hx(iz)+hx(iz+1))/2d0)*(1d0)&
                    /(0.5d0*(dz(iz)+dz(iz+1))))/dz(iz)  &
                -ux(iz+1)/2d0/dz(iz)            
                ! amx(row, row-4) = 0d0

            amx(row, row+1) =    &
                ! - poro(iz)*((-1d0)*cmx(iz))/dt    &
                ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(-1d0/2d0)*(cmx(iz+1)-cmx(iz))/(0.5d0*(dz(iz)+dz(iz+1)))  &
                - dif_m(iz)*(poro(iz)+poro_0)/2d0*(-1d0/2d0)*(cmx(iz)-0d0)/dz(iz))/dz(iz)   
                ! amx(row, row-4+1) = 0d0  
            amx(row, row+4+1) = ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(-1d0/2d0)*(cmx(iz+1)-cmx(iz))&
                    /(0.5d0*(dz(iz)+dz(iz+1))))/dz(iz)  
            amx(row, row+4+3) = -(cmx(iz+1))/2d0/dz(iz)

            ymx(row+1) = rxn_h(iz)*(cmx(iz)-ceq(iz)) &
                ! - poro(iz)*(hx(iz)-h(iz))/dt &
                -(v(iz)*poro(iz)*hx(iz)-0d0)/dz(iz)
            amx(row+1, row) = rxn_h(iz) 
            amx(row+1, row+1) = &  
                ! -poro(iz)/dt  &
                -(v(iz)*poro(iz))/dz(iz)
                ! amx(row+1, row-4+1) = -(-v(iz-1)*poro(iz-1))/dz


            ymx(row+2) = 1d0*(bx(iz)-0d0)
            amx(row+2, row+2) = 1d0

            ymx(row+3) = ux(iz) - ucnv*((1d0-poro_0)*(poro_ext/(1d0-poro_ext)- poro(iz)*hx(iz)/(1d0-poro(iz)))   &
                *v_sed + poro_ext*v_ext)
            amx(row+3,row+3) = 1d0
            amx(row+3,row+1) = - (1d0-poro_0)*(- poro(iz)/(1d0-poro(iz))) *v_sed*ucnv
        endif
    else if (iz == nz) then  
        if (cm_ext == 0d0) then  ! no flux toward bottom (cm(iz+1) = cm(iz))
            ymx(row) = (-(dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz-1)+poro(iz))/2d0*(1d0-(bx(iz-1)+bx(iz))/2d0)*(cmx(iz)-cmx(iz-1))&
                    /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  &
                + (-rho_b/rho_f*cb)*rxn_b(iz)*(cmx(iz)-ceq(iz)) &
                - (ux(iz)*cmx(iz)-ux(iz-1)*cmx(iz-1))/2d0/dz(iz)  &
                ! - poro(iz)*((1d0-bx(iz))*cmx(iz)-(1d0-b(iz))*cm(iz))/dt &
                + phi_m(iz)
            amx(row,row) = (-(dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz-1)+poro(iz))/2d0*(1d0-(bx(iz-1)+bx(iz))/2d0) &
                    /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  &
                + (-rho_b/rho_f*cb)*rxn_b(iz)   &
                ! - poro(iz)*(1d0-bx(iz))*1d0/dt   &
                - ux(iz)/2d0/dz(iz)                
            amx(row, row-4) = (-(dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz-1)+poro(iz))/2d0*(1d0-(bx(iz-1)+bx(iz))/2d0)*(-1d0) &
                    /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  &
                - ux(iz-1)*(-1d0)/2d0/dz(iz)

            amx(row, row+2) =  &
                ! - poro(iz)*(-1d0)*cmx(iz)/dt  &
                + (-(dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz-1)+poro(iz))/2d0*(-1d0/2d0)*(cmx(iz)-cmx(iz-1))&
                    /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   
            amx(row, row-4+2) = (-(dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz-1)+poro(iz))/2d0*(-(1d0)/2d0)*(cmx(iz)-cmx(iz-1))&
                    /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)
            amx(row, row+3) = -(cmx(iz))/2d0/dz(iz)
            amx(row, row-4+3) = -(-cmx(iz-1))/2d0/dz(iz)

            ymx(row+1) = 1d0*(hx(iz) - 0d0)
            amx(row+1, row+1) = 1d0

            ymx(row+2) = rxn_b(iz)*(cmx(iz)-ceq(iz))  &
                ! -poro(iz)*(bx(iz)-b(iz))/dt   &
                -(v(iz)*poro(iz)*bx(iz)-v(iz-1)*poro(iz-1)*bx(iz-1))/dz(iz)
            amx(row+2, row) = rxn_b(iz) 
            amx(row+2, row+2) = &
                ! -poro(iz)/dt  & 
                -(v(iz)*poro(iz))/dz(iz)
            amx(row+2, row-4+2) = -(-v(iz-1)*poro(iz-1))/dz(iz)

            ymx(row+3) = ux(iz) - ucnv*((1d0-poro_0)*(poro_ext/(1d0-poro_ext)- poro(iz)*bx(iz)/(1d0-poro(iz)))   &
                *v_sed + poro_ext*v_ext)
            amx(row+3,row+3) = 1d0
            amx(row+3,row+2) = - (1d0-poro_0)*(- poro(iz)/(1d0-poro(iz))) *v_sed*ucnv

        else ! cm(iz+1) = cm_ext
            !!! following set of equations does not converge so abandoned
            ! ymx(row) = (dif_m*poro(iz)*(1d0-bx(iz))*(cm_ext-cmx(iz))/dz  &
                ! - dif_m*(poro(iz)+poro(iz-1))/2d0*(1d0-(bx(iz)+bx(iz-1))/2d0)*(cmx(iz)-cmx(iz-1))/dz)/dz  &
                ! + (cmx(iz)-rho_b/rho_f*cb)*rxn_b(iz)*(cmx(iz)-ceq(iz)) &
                ! - ux(iz)*(cm_ext-cmx(iz-1))/2d0/dz  &
                ! - poro(iz)*(1d0-bx(iz))*(cmx(iz)-cm(iz))/dt + phi_m(iz)
            ! amx(row,row) =  (dif_m*poro(iz)*(1d0-bx(iz))*(1d0)/dz  &
                ! - dif_m*(poro(iz)+poro(iz-1))/2d0*(1d0-(bx(iz)+bx(iz-1))/2d0)*(1d0)/dz)/dz  &  
                ! +rxn_b(iz)*(cmx(iz)-ceq(iz))    &
                ! +(cmx(iz)-rho_b/rho_f*cb)*rxn_b(iz)    &
                ! -poro(iz)*(1d0-bx(iz))*1d0/dt                      
            ! amx(row, row-4) = (- dif_m*(poro(iz)+poro(iz-1))/2d0*(1d0-(bx(iz)+bx(iz-1))/2d0)*(-1d0)/dz)/dz  &
                ! - ux(iz)*(-1d0)/2d0/dz

            ! amx(row, row+2) = (dif_m*poro(iz)*(1d0)*(cm_ext-cmx(iz))/dz  &
                ! - dif_m*(poro(iz)+poro(iz-1))/2d0*(-1d0/2d0)*(cmx(iz)-cmx(iz-1))/dz)/dz   &
                ! - poro(iz)*(-1d0)*(cmx(iz)-cm(iz))/dt        
            ! amx(row, row-4+2) = (- dif_m*(poro(iz)+poro(iz-1))/2d0*(-1d0/2d0)*(cmx(iz)-cmx(iz-1))/dz)/dz  
            ! amx(row, row+2) = -(cm_ext-cmx(iz-1))/2d0/dz
            !!!

            ymx(row) = 1d0*(cmx(iz) - cm_ext)
            amx(row,row) = 1d0


            ymx(row+1) = 1d0*(hx(iz)-0d0)
            amx(row+1, row+1) = 1d0

            ymx(row+2) = rxn_b(iz)*(cmx(iz)-ceq(iz)) &
                ! - poro(iz)*(bx(iz)-b(iz))/dt &
                -(v(iz)*poro(iz)*bx(iz)-v(iz-1)*poro(iz-1)*bx(iz-1))/dz(iz)
            amx(row+2, row) = rxn_b(iz) 
            amx(row+2, row+2) = &
                ! -poro(iz)/dt  &
                -(v(iz)*poro(iz))/dz(iz)
            amx(row+2, row-4+2) = -(-v(iz-1)*poro(iz-1))/dz(iz)


            ymx(row+3) = ux(iz) - ucnv*((1d0-poro_0)*(poro_ext/(1d0-poro_ext)- poro(iz)*bx(iz)/(1d0-poro(iz)))   &
                *v_sed + poro_ext*v_ext)
            amx(row+3,row+3) = 1d0
            amx(row+3,row+2) = - (1d0-poro_0)*(- poro(iz)/(1d0-poro(iz))) *v_sed*ucnv
        end if 

    else if ((iz < ifmx).and.(iz > imx)) then  ! this cannot be applied when not imx < ifmx
        ymx(row) = ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hx(iz)+hx(iz+1))/2d0)*(cmx(iz+1)-cmx(iz))&
                /(0.5d0*(dz(iz)+dz(iz+1)))  &
            - (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hx(iz)+hx(iz-1))/2d0)*(cmx(iz)-cmx(iz-1))&
                /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  &
            + (-rho_h/rho_f*ch)*rxn_h(iz)*(cmx(iz)-ceq(iz))   &
            - (ux(iz+1)*cmx(iz+1)-ux(iz-1)*cmx(iz-1))/2d0/dz(iz)    &
            ! - poro(iz)*((1d0-hx(iz))*cmx(iz)-(1d0-h(iz))*cm(iz))/dt   &
            + phi_m(iz)  
        amx(row,row) =  ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hx(iz)+hx(iz+1))/2d0)*(-1d0)&
                /(0.5d0*(dz(iz)+dz(iz+1)))  &
            - (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hx(iz)+hx(iz-1))/2d0)*(1d0)&
                /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  &  
            ! -poro(iz)*(1d0-hx(iz))*1d0/dt      &       
            +(-rho_h/rho_f*ch)*rxn_h(iz)    
        amx(row, row+4) =  ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hx(iz)+hx(iz+1))/2d0)*(1d0)&
            /(0.5d0*(dz(iz)+dz(iz+1))))/dz(iz)  &
            -ux(iz+1)/2d0/dz(iz)            
        amx(row, row-4) = (- (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hx(iz)+hx(iz-1))/2d0)*(-1d0)&
            /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  &
            - ux(iz-1)*(-1d0)/2d0/dz(iz)

        amx(row, row+1) = &
            ! - poro(iz)*(-1d0)*cmx(iz)/dt  &
            +((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(-1d0/2d0)*(cmx(iz+1)-cmx(iz))  &
                /(0.5d0*(dz(iz)+dz(iz+1)))  &
            - (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(-1d0/2d0)*(cmx(iz)-cmx(iz-1))&
                /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   
        amx(row, row-4+1) = (- (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(-1d0/2d0)*(cmx(iz)-cmx(iz-1))&
                /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  
        amx(row, row+4+1) = ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(-1d0/2d0)*(cmx(iz+1)-cmx(iz))&
                /(0.5d0*(dz(iz)+dz(iz+1))))/dz(iz)  
        amx(row, row-4+3) = -(-cmx(iz-1))/2d0/dz(iz)
        amx(row, row+4+3) = -(cmx(iz+1))/2d0/dz(iz)

        ymx(row+1) = rxn_h(iz)*(cmx(iz)-ceq(iz)) &
            ! - poro(iz)*(hx(iz)-h(iz))/dt &
            -(v(iz)*poro(iz)*hx(iz)-v(iz-1)*poro(iz-1)*hx(iz-1))/dz(iz)
        amx(row+1, row) = rxn_h(iz) 
        amx(row+1, row+1) = &
            ! -poro(iz)/dt  &
            -(v(iz)*poro(iz))/dz(iz)
        amx(row+1, row-4+1) = -(-v(iz-1)*poro(iz-1))/dz(iz)


        ymx(row+2) = 1d0*(bx(iz)-0d0)
        amx(row+2, row+2) = 1d0

        ymx(row+3) = ux(iz) - ucnv*((1d0-poro_0)*(poro_ext/(1d0-poro_ext)- poro(iz)*hx(iz)/(1d0-poro(iz)))   &
            *v_sed + poro_ext*v_ext)
        amx(row+3,row+3) = 1d0
        amx(row+3,row+1) = - (1d0-poro_0)*(- poro(iz)/(1d0-poro(iz))) *v_sed*ucnv

    else if ((iz < nz).and.(iz> max(ifmx,imx))) then ! max(ifmx,imx) = imx if not imx <ifmx but = ifmx if imx < ifmx
        ymx(row) = ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(bx(iz)+bx(iz+1))/2d0)*(cmx(iz+1)-cmx(iz))&
                /(0.5d0*(dz(iz)+dz(iz+1)))  &
            - (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(bx(iz)+bx(iz-1))/2d0)*(cmx(iz)-cmx(iz-1))&
                /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  &
            + (-rho_b/rho_f*cb)*rxn_b(iz)*(cmx(iz)-ceq(iz)) &
            - (ux(iz+1)*cmx(iz+1)-ux(iz-1)*cmx(iz-1))/2d0/dz(iz)  &
            ! - poro(iz)*((1d0-bx(iz))*cmx(iz)-(1d0-b(iz))*cm(iz))/dt &
            + phi_m(iz)
        amx(row,row) =  ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(bx(iz)+bx(iz+1))/2d0)*(-1d0)&
                /(0.5d0*(dz(iz)+dz(iz+1)))  &
            - (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(bx(iz)+bx(iz-1))/2d0)*(1d0)&
                /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  &
            ! -poro(iz)*(1d0-bx(iz))*1d0/dt          &     
            +(cmx(iz)-rho_b/rho_f*cb)*rxn_b(iz)    
        amx(row, row+4) =  ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(bx(iz)+bx(iz+1))/2d0)*(1d0)&
                /(0.5d0*(dz(iz)+dz(iz+1))))/dz(iz)  &
            -ux(iz+1)/2d0/dz(iz)            
        amx(row, row-4) = (- (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(bx(iz)+bx(iz-1))/2d0)*(-1d0)&
                /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  &
            - ux(iz-1)*(-1d0)/2d0/dz(iz)

        amx(row, row+2) = &
            ! - poro(iz)*(-1d0)*(cmx(iz))/dt  &
            +((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(-1d0/2d0)*(cmx(iz+1)-cmx(iz))&
                /(0.5d0*(dz(iz)+dz(iz+1)))  &
            - (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(-1d0/2d0)*(cmx(iz)-cmx(iz-1))&
                /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   
        amx(row, row-4+2) = (- (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(-1d0/2d0)*(cmx(iz)-cmx(iz-1))&
                /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  
        amx(row, row+4+2) = ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(-1d0/2d0)*(cmx(iz+1)-cmx(iz))&
                /(0.5d0*(dz(iz)+dz(iz+1))))/dz(iz)  
        amx(row, row-4+3) = -(-cmx(iz-1))/2d0/dz(iz)
        amx(row, row+4+3) = -(cmx(iz+1))/2d0/dz(iz)

        ymx(row+1) = 1d0*(hx(iz) - 0d0)
        amx(row+1, row+1) = 1d0

        ymx(row+2) = rxn_b(iz)*(cmx(iz)-ceq(iz)) &
            ! - poro(iz)*(bx(iz)-b(iz))/dt &
            -(v(iz)*poro(iz)*bx(iz)-v(iz-1)*poro(iz-1)*bx(iz-1))/dz(iz)
        amx(row+2, row) = rxn_b(iz) 
        amx(row+2, row+2) = &
            ! -poro(iz)/dt  &
            -(v(iz)*poro(iz))/dz(iz)
        amx(row+2, row-4+2) = -(-v(iz-1)*poro(iz-1))/dz(iz)


        ymx(row+3) = ux(iz) - ucnv*((1d0-poro_0)*(poro_ext/(1d0-poro_ext)- poro(iz)*bx(iz)/(1d0-poro(iz)))   &
            *v_sed + poro_ext*v_ext)
        amx(row+3,row+3) = 1d0
        amx(row+3,row+2) = - (1d0-poro_0)*(- poro(iz)/(1d0-poro(iz))) *v_sed*ucnv

    else if ( iz == ifmx_tmp) then  ! this cannot be applied if not imx < ifmx because then ifmx_tmp = -1
        ymx(row) = ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hx(iz)+bx(iz+1)+bx(iz))/4d0)*(cmx(iz+1)-cmx(iz))&
                /(0.5d0*(dz(iz)+dz(iz+1)))  &
            - (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hx(iz)+hx(iz-1)+bx(iz))/4d0)*(cmx(iz)-cmx(iz-1))&
                /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  &
            + (-rho_h/rho_f*ch)*rxn_h(iz)*(cmx(iz)-ceq(iz)) &
            + (-rho_b/rho_f*cb)*rxn_b(iz)*(cmx(iz)-ceq(iz)) &
            ! - poro(iz)*((1d0-hx(iz)-bx(iz))*cmx(iz)-(1d0-h(iz)-b(iz))*cm(iz))/dt &
            + phi_m(iz)      &
            - (ux(iz+1)*cmx(iz+1)-ux(iz-1)*cmx(iz-1))/2d0/dz(iz)  
        amx(row,row) =  ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hx(iz)+bx(iz+1)+bx(iz))/4d0)*(-1d0)&
                /(0.5d0*(dz(iz)+dz(iz+1)))  &
            - (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hx(iz)+hx(iz-1)+bx(iz))/4d0)*(1d0)&
                /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  &  
            ! -poro(iz)*(1d0-hx(iz)-bx(iz))*1d0/dt             
            +(-rho_h/rho_f*ch)*rxn_h(iz)    &
            +(-rho_b/rho_f*cb)*rxn_b(iz)    
        amx(row, row+4) =  ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hx(iz)+bx(iz+1)+bx(iz))/4d0)*(1d0)&
                /(0.5d0*(dz(iz)+dz(iz+1))))/dz(iz)  &
            - ux(iz+1)*(1d0)/2d0/dz(iz)
        amx(row, row-4) = (- (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hx(iz)+hx(iz-1)+bx(iz))/4d0)*(-1d0)&
                /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  &
            - ux(iz-1)*(-1d0)/2d0/dz(iz) 

        amx(row, row+1) = &
            ! - poro(iz)*(-1d0)*(cmx(iz))/dt  &
            +((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(-1d0/4d0)*(cmx(iz+1)-cmx(iz))  &
                /(0.5d0*(dz(iz)+dz(iz+1)))  &
            - (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(-1d0/4d0)*(cmx(iz)-cmx(iz-1))  &
                /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   
        amx(row, row-4+1) = (- (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(-1d0/4d0)*(cmx(iz)-cmx(iz-1))&
                /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  

        amx(row, row+2) = &
            ! - poro(iz)*(-1d0)*(cmx(iz))/dt   &
            +((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(-(1d0)/4d0)*(cmx(iz+1)-cmx(iz))&
                /(0.5d0*(dz(iz)+dz(iz+1)))  &
            - (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(-(1d0)/4d0)*(cmx(iz)-cmx(iz-1))&
                /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  
        amx(row, row+4+2) = ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(-(1d0)/4d0)*(cmx(iz+1)-cmx(iz))&
                /(0.5d0*(dz(iz)+dz(iz+1))))/dz(iz)  

        amx(row, row-4+3) = - (-cmx(iz-1))/2d0/dz(iz)         
        amx(row, row+4+3) = - (cmx(iz+1))/2d0/dz(iz)         

        ymx(row+1) = & 
            + rxn_h(iz)*(cmx(iz)-ceq(iz)) & 
            ! - poro(iz)*(hx(iz)-h(iz))/dt &
            ! - rxn_hb*hx(iz)*func(hx(iz)> 0d0)    &   
            ! + rxn_bh*bx(iz)*func(bx(iz)> 0d0)    &   
            -(v(iz)*poro(iz)*hx(iz)-v(iz-1)*poro(iz-1)*hx(iz-1))/dz(iz)
        amx(row+1, row) = rxn_h(iz) 
        amx(row+1, row+1) = &
            ! -poro(iz)/dt  &
            ! - rxn_hb*func(hx(iz)> 0d0)   &
            -(v(iz)*poro(iz))/dz(iz)
            ! amx(row+1,row+2) =  +rxn_bh*func(bx(iz)> 0d0)
        amx(row+1, row-4+1) = -(-v(iz-1)*poro(iz-1))/dz(iz)

        ymx(row+2) = rxn_b(iz)*(cmx(iz)-ceq(iz)) &
            ! - poro(iz)*(bx(iz)-b(iz))/dt &
            ! + rxn_hb*hx(iz)*func(hx(iz)> 0d0)   &
            ! - rxn_bh*bx(iz)*func(bx(iz)> 0d0)   &
            -(v(iz)*poro(iz)*bx(iz)-0d0)/dz(iz)
        amx(row+2, row) = rxn_b(iz)
        ! amx(row+2, row+1) = rxn_hb*func(hx(iz)> 0d0)    
        amx(row+2, row+2) = & 
            ! -poro(iz)/dt  &
            ! - rxn_bh*func(bx(iz)> 0d0)   &
            -(v(iz)*poro(iz))/dz(iz)
        amx(row+2, row-4+2) = -(-v(iz-1)*poro(iz-1))/dz(iz)

! #ifdef fixmid
        ! ymx(row+1) = ymx(row+1)   & 
            ! - rxn_hb*hx(iz)*func(hx(iz)> 0d0)    &   
            ! + rxn_bh*bx(iz)*func(bx(iz)> 0d0)   
        ! amx(row+1, row+1) = amx(row+1, row+1)  &
            ! - rxn_hb*func(hx(iz)> 0d0)   
        ! amx(row+1,row+2) =  +rxn_bh*func(bx(iz)> 0d0)

        ! ymx(row+2) = ymx(row+2) &
            ! + rxn_hb*hx(iz)*func(hx(iz)> 0d0)   &
            ! - rxn_bh*bx(iz)*func(bx(iz)> 0d0)   
        ! amx(row+2, row+1) = rxn_hb*func(hx(iz)> 0d0)    
        ! amx(row+2, row+2) = amx(row+2, row+2) & 
            ! - rxn_bh*func(bx(iz)> 0d0)   
! #endif


! #ifdef fixmid2    
        ! ymx(row+1) = & 
            ! + rxn_h(iz)*(cmx(iz)-ceq(iz)) & 
            ! -(v(iz)*poro(iz)*hx(iz)-v(iz-1)*poro(iz-1)*hx(iz-1))/dz(iz)
        ! amx(row+1, row) = rxn_h(iz) 
        ! amx(row+1, row+1) = &
            ! -(v(iz)*poro(iz))/dz(iz)
        ! amx(row+1,row+2) =  0d0
        ! amx(row+1, row-4+1) = -(-v(iz-1)*poro(iz-1))/dz(iz)

        ! ymx(row+2) = 1d0*(bx(iz)-max(0d0,expbb))
        ! amx(row+2,row) = 0d0
        ! amx(row+2,row+1) = 0d0
        ! amx(row+2,row+2) = 1d0
        ! amx(row+2, row-4+2) = 0d0
! #endif    

        ymx(row+3) = ux(iz) - ucnv*((1d0-poro_0)*(poro_ext/(1d0-poro_ext)  & 
            - poro(iz)*(hx(iz)+bx(iz))/2d0/(1d0-poro(iz)))   &
            *v_sed + poro_ext*v_ext)
        amx(row+3,row+3) = 1d0
        amx(row+3,row+1) = - (1d0-poro_0)*(- poro(iz)/2d0/(1d0-poro(iz))) *v_sed*ucnv
        amx(row+3,row+2) = - (1d0-poro_0)*(- poro(iz)/2d0/(1d0-poro(iz))) *v_sed*ucnv

    end if 
    phi_hb(iz) = (rxn_h(iz)+rxn_b(iz))*(cmx(iz)-ceq(iz))
    phi_h(iz) = rxn_h(iz)*(cmx(iz)-ceq(iz))
    phi_b(iz) = rxn_b(iz)*(cmx(iz)-ceq(iz))

    prodb = prodb + rho_b/rho_f*cb*phi_b(iz)*dz(iz)
    prodh = prodh + rho_h/rho_f*ch*phi_h(iz)*dz(iz)
end do 

ymx = -ymx

! open (unit =200,file='test-mtx1.txt',status='replace')
! open (unit =300,file='test-mtx2.txt',status='replace')
! do row = 1,nmx
    ! write(200,*) (amx(row,col),col = 1,nmx)
    ! write(300,*) ymx(row)
! end do 
! close(200)
! close(300)
! stop

call DGESV(nmx,int(1),amx,nmx,ipiv,ymx,nmx,info) 

! open (unit =300,file='test-mtx3.txt',status='replace')
! do row = 1,nmx
    ! write(300,*) ymx(row)
! end do 
! close(300)
! stop

emx = 0d0
do iz = 1, nz
    row = 4*(iz-1)  + 1
    cmx(iz) = cmx(iz)+ymx(row)
    if (abs(cmx(iz))>1d-10) emx(row) = abs(ymx(row)/cmx(iz))
    hx(iz) = hx(iz)+ymx(row+1)
    if (abs(hx(iz))>1d-10) emx(row+1) = abs(ymx(row+1)/hx(iz))
    bx(iz) = bx(iz)+ymx(row+2)
    if (abs(bx(iz))>1d-10) emx(row+2) = abs(ymx(row+2)/bx(iz))
    ux(iz) = ux(iz) + ymx(row+3)
    if (ux(iz)/=0d0) emx(row+3) = abs(ymx(row+3)/ux(iz))
end do 

hbx = hx + bx

err3 = maxval(emx)
if (loc_display) then 
    print *, "============================================"
    print '(a12,i5,a12,e9.3e2)', "iteration:",itrnew,"/error:", err3
    print *, '>>>>>>'
    print '(5(3x,a9))', 'z', 'cmx','hx','bx','ux'
    do iz=1,nz,nz/10
        print '(5(3x,e9.3e2))', z(iz), cmx(iz),hx(iz),bx(iz),ux(iz)
    enddo 
    ! print *, "iteration:",itr,"/error:", err3
    ! print *, (z(iz), iz = 1,nz, nz/10 )
    ! print *,(cmx(iz), iz = 1,nz, nz/10)
    ! print *,(hx(iz), iz = 1,nz, nz/10)
    ! print *,(bx(iz), iz = 1,nz, nz/10)
    ! print *,(ux(iz), iz = 1,nz, nz/10)
endif
! if (any(hbx < 0)) then 
    ! print *, "negative hydrate or gas at interation No.", itr

    ! open (unit =200,file='test-calc_stoped.txt',status='replace')
    ! do iz = 1, nz
        ! write(200,*) z(iz), cm(iz),ceq(iz),h(iz),b(iz), u(iz), v(iz)
    ! end do 
    ! close(200)
    ! stop
! end if 

end do newton

cm = cmx
! h(1:ifmx) = hbx(1:ifmx)
! b(ifmx+1:nz) = hbx(ifmx+1:nz)
h = hx
b = bx
u = ux

! hb(1:ifmx) = h(1:ifmx)
! hb(ifmx+1:nz) = b(ifmx+1:nz)

hb = h+b



! advloss = u(nz)*cm(nz)
do iz = 1, nz
    if (cm(iz) > 0) exit
end do 
! difloss = dif_m*poro(iz)*(1d0-hb(iz))*cm(iz)/dz
if (iz>=1 .and. iz<=nz) then 
    sdif_bot = 96d0/16d0*dif_m(iz)/dif_s(iz)*cm(iz)/dz(iz)
else 
    sdif_bot = 0d0
endif 
! do iz = 1, nz 
    ! if (hb(iz)>0) exit
! end do 

! select case(imx<ifmx)
! case(.true.)
! if (iz<=nz) then 
! hydloss = rho_h/rho_f*ch*(hb(ifmx)*poro(ifmx)*v(ifmx)-hb(iz)*poro(iz)*v(iz))
! gasloss = rho_b/rho_f*cb*(hb(nz)*poro(nz)*v(nz)-hb(ifmx+1)*poro(ifmx+1)*v(ifmx+1))
! else 
! hydloss = 0d0
! gasloss = 0d0
! end if 
! case(.false.)
! hydloss = 0d0
! if (iz<=nz) then 
! gasloss = rho_b/rho_f*cb*(hb(nz)*poro(nz)*v(nz)-hb(iz)*poro(iz)*v(iz))
! else 
! gasloss = 0d0
! end if 
! endselect

! resid = bioprod - advloss - difloss - hydloss - gasloss
! err3 = abs(1d2*resid/bioprod)
! #ifdef display
! print *,"bioprod, advloss, difloss, hydloss, gasloss, resid, error[%]"
! print *, bioprod, advloss, difloss, hydloss, gasloss, resid, err3
! #endif
! if (err3 < 1d0) then 
    ! do iz = 1, nz
        ! if (hb(iz) < 0d0) hb(iz) = 0d0
    ! end do 
! end if 

! if (all(h>=0).and.all(b>=0).and.all(cm>=0).and.err3<2.d0 .and. itr>itrmin) exit 
! #ifdef display
! print *,"all(h>=0)",all(h>=0),"all(b>=0)",all(b>=0),"all(cm>=0)",all(cm>=0),"err3<2.d0",err3<2.d0,"itr>itrmin",itr>itrmin
! #endif

! mass balance check again 
! #ifdef chk2
advlossc = 0d0
diflossc = 0d0
hydlossc = 0d0
gaslossc = 0d0
bioprodc = 0d0

prodh = 0d0
prodb = 0d0

cmmb = 0d0
hmb = 0d0
bmb = 0d0

! select case(imx < ifmx) 
! case(.true.)
do iz = 1, nz
    ! bioprodc = bioprodc + phi_m(iz)*dz
    if (iz < imx) then
        if (phi_m(iz) > 0d0) print *, "phi_m(iz) > 0d0 while iz < imx"
        cycle
    else if (iz == imx) then 
        if (imx/=1)then 
            diflossc = diflossc + dz(iz)*( &
                ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(h(iz)+h(iz+1))/2d0)*(cm(iz+1)-cm(iz))&
                    /(0.5d0*(dz(iz)+dz(iz+1)))  &
                - (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(h(iz)+0d0)/2d0)*(cm(iz)-0d0)&
                    /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   &
                )

            advlossc = advlossc + dz(iz)*(  &
                - (cm(iz+1)*u(iz+1)-0d0)/2d0/dz(iz)  &
                )

            hydlossc = hydlossc + dz(iz)*(  &
                (-rho_h/rho_f*ch)*rxn_h(iz)*(cm(iz)-ceq(iz))  &
                )

            prodh = prodh + dz(iz)*rho_h/rho_f*ch*( &
                -(v(iz)*poro(iz)*h(iz)-0d0)/dz(iz)       &
                ) 

            bioprodc = bioprodc + phi_m(iz)*dz(iz)
        else if (imx==1)then 
            diflossc = diflossc + dz(iz)*( &
                ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(h(iz)+h(iz+1))/2d0)*(cm(iz+1)-cm(iz)) &
                    /(0.5d0*(dz(iz)+dz(iz+1)))  &
                - dif_m(iz)*(poro(iz)+poro_0)/2d0*(1d0-(h(iz)+0d0)/2d0)*(cm(iz)-0d0)/dz(iz))/dz(iz)   &
                )

            advlossc = advlossc + dz(iz)*(  &
                - (cm(iz+1)*u(iz+1)-0d0)/2d0/dz(iz)  &
                )

            hydlossc = hydlossc + dz(iz)*(  &
                (-rho_h/rho_f*ch)*rxn_h(iz)*(cm(iz)-ceq(iz))  &
                )

            prodh = prodh + dz(iz)*rho_h/rho_f*ch*( &
                -(v(iz)*poro(iz)*h(iz)-0d0)/dz(iz)       &
                ) 

            bioprodc = bioprodc + phi_m(iz)*dz(iz)
        endif
    else if ((iz > imx) .and. (iz < ifmx)) then 
        diflossc = diflossc + dz(iz)*( & 
            ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(h(iz)+h(iz+1))/2d0)*(cm(iz+1)-cm(iz))&
                /(0.5d0*(dz(iz)+dz(iz+1)))  &
            - (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(h(iz)+h(iz-1))/2d0)*(cm(iz)-cm(iz-1))&
                /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  &
            )

        advlossc = advlossc + dz(iz)*(  &
            - (u(iz+1)*cm(iz+1)-u(iz-1)*cm(iz-1))/2d0/dz(iz)  &
            )

        hydlossc = hydlossc + dz(iz)*(  &
            (-rho_h/rho_f*ch)*rxn_h(iz)*(cm(iz)-ceq(iz))  &
            )

        prodh = prodh + dz(iz)*rho_h/rho_f*ch*( &
            -(v(iz)*poro(iz)*h(iz)-v(iz-1)*poro(iz-1)*h(iz-1))/dz(iz)    &
            ) 

        bioprodc = bioprodc + phi_m(iz)*dz(iz)
    else if (iz ==  ifmx_tmp) then       
        diflossc = diflossc + dz(iz)*( & 
            ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(h(iz)+b(iz+1)+b(iz))/4d0)*(cm(iz+1)-cm(iz))&
                /(0.5d0*(dz(iz)+dz(iz+1)))  &
            - (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(h(iz)+h(iz-1)+b(iz))/4d0)*(cm(iz)-cm(iz-1))&
                /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  &
            )
        advlossc = advlossc + dz(iz)*(  &
            - (u(iz+1)*cm(iz+1)-u(iz-1)*cm(iz-1))/2d0/dz(iz)  &
            )

        hydlossc = hydlossc + dz(iz)*(  &
            (-rho_h/rho_f*ch)*rxn_h(iz)*(cm(iz)-ceq(iz))  &
            )

        gaslossc = gaslossc + dz(iz)*(  &
            (-rho_b/rho_f*cb)*rxn_b(iz)*(cm(iz)-ceq(iz))  &
            )

        prodh = prodh + dz(iz)*rho_h/rho_f*ch*( &
            -(v(iz)*poro(iz)*h(iz)-v(iz-1)*poro(iz-1)*h(iz-1))/dz(iz)    &
            ) 

        prodb = prodb + dz(iz)*rho_b/rho_f*cb*( &
            -(v(iz)*poro(iz)*b(iz)-0d0)/dz(iz)    &
            ) 

        bioprodc = bioprodc + phi_m(iz)*dz(iz)
    else if ((iz > max(imx,ifmx)) .and. (iz < nz)) then 
        diflossc = diflossc + dz(iz)*(  & 
            ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(b(iz)+b(iz+1))/2d0)*(cm(iz+1)-cm(iz))&
                /(0.5d0*(dz(iz)+dz(iz+1)))  &
            - (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(b(iz)+b(iz-1))/2d0)*(cm(iz)-cm(iz-1))&
                /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  &
            )

        advlossc = advlossc + dz(iz)*(  &
            - (u(iz+1)*cm(iz+1)-u(iz-1)*cm(iz-1))/2d0/dz(iz)  &
            )

        gaslossc = gaslossc + dz(iz)*(  &
            (-rho_b/rho_f*cb)*rxn_b(iz)*(cm(iz)-ceq(iz))  &
            )

        prodb = prodb + dz(iz)*rho_b/rho_f*cb*( &
            -(v(iz)*poro(iz)*b(iz)-v(iz-1)*poro(iz-1)*b(iz-1))/dz(iz)    &
            ) 

        bioprodc = bioprodc + phi_m(iz)*dz(iz)
    else if (iz == nz) then 
        if (cm_ext == 0d0) then !  no flux boundary 
            diflossc = diflossc + dz(iz)*(  & 
                (-(dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz-1)+poro(iz))/2d0*(1d0-(b(iz-1)+b(iz))/2d0)*(cm(iz)-cm(iz-1))&
                    /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  &
                )

            advlossc = advlossc + dz(iz)*(  &
                - (u(iz)*cm(iz)-u(iz-1)*cm(iz-1))/2d0/dz(iz)  &
                )

            gaslossc = gaslossc + dz(iz)*(  &
                (-rho_b/rho_f*cb)*rxn_b(iz)*(cm(iz)-ceq(iz))  &
                )

            prodb = prodb + dz(iz)*rho_b/rho_f*cb*( &
                -(v(iz)*poro(iz)*b(iz)-v(iz-1)*poro(iz-1)*b(iz-1))/dz(iz)    &
                ) 

            bioprodc = bioprodc + phi_m(iz)*dz(iz)
        else 

        end if     

    end if

enddo  

residc = bioprodc + advlossc + diflossc + hydlossc + gaslossc
err3 = abs(1d2*residc/bioprodc)

if (loc_display) then 
    print '(a)', '**** flx check ****'
    print '(7(3x,a9))','bioprod', 'advlossc', 'difloss', 'hydloss', 'gasloss', 'resid', 'error[%]'
    print '(7(3x,e9.3e2))', bioprodc, advlossc, diflossc, hydlossc, gaslossc, residc, err3
    print '(4(3x,a9))', 'prodh', 'hydloss', 'prodb', 'gasloss'
    print '(4(3x,e9.3e2))', prodh, hydlossc, prodb, gaslossc
endif

aomflx_ch4 = diflossc * 1d6/16d0* 60d0*60d0*24d0*365.25d0  ! converting from g/g*m/sec to mol/m2/yr 
aomflx_ch4 = aomflx_ch4*1d-4  ! converting from mol/m2/yr to mol/cm2/yr

! #endif

if (itr/=1) then 
    mhinvc = mhinv
    mbinvc = mbinv
    mhinv = 0d0
    mbinv = 0d0
    ! do iz = 1,nz
        ! mhinv=mhinv+poro(iz)*h(iz)*rho_h*ch*dz
        ! mbinv=mbinv+poro(iz)*b(iz)*rho_b*cb*dz
    ! enddo
    mhinv=mhinv+sum(poro(:)*h(:)*dz(:))*rho_h*ch
    mbinv=mbinv+sum(poro(:)*b(:)*dz(:))*rho_b*cb

    if (mhinv/=0d0) then 
        mhinve = abs((mhinv-mhinvc)/mhinv)
    else  
        mhinve = abs((mhinv-mhinvc))
    end if 

    if (mbinv/=0d0) then 
        mbinve = abs((mbinv-mbinvc)/mbinv)
    else  
        mbinve = abs((mbinv-mbinvc))
    end if 

end if 

if (loc_display) then
    print '(3(3x,a9))', 'mhinv', 'mhinvc', 'mhinve'
    print '(3(3x,e9.3e2))',  mhinv, mhinvc, mhinve
    print '(3(3x,a9))', 'mbinv', 'mbinvc', 'mbinve'
    print '(3(3x,e9.3e2))',  mbinv, mbinvc, mbinve
endif

! #ifdef record
! open(unit=200,file=trim(adjustl(home))//trim(adjustl(runid))//'/profiles/'//trim(adjustl(outfilename))&
    ! //'/ch4.txt',status='replace')
! do iz = 1, nz
    ! write(200,*) z(iz), cm(iz),ceq(iz),h(iz),b(iz), u(iz), v(iz), rxn_h(iz), rxn_b(iz)
! end do 
! close(200)
! #endif


! print *, "hb(171), hb(172), rxn(171), rxn(172)"
! print *,  hb(171), hb(172), rxn(171), rxn(172)
! #if 0
if (ifmx/=1) then 
    do iz = 1,ifmx-1
        if (cm(iz) > ceq(iz)) then 
            if (rxn_h(iz)/=0d0 .and. rxn_h(iz)< maxrate) rxn_h(iz) = rxn_h(iz) * 1d1   ! assuming maximum rate 1d-8
            if (rxn_h(iz)==0d0) rxn_h (iz) = 1d-15
        else  ! not saturated 
            if (cm(iz) > 0) then 
                if (rxn_h(iz)< maxrate) then  ! assuming maximum rate 1d-8
                    if (rxn_h(iz)==0) then 
                        rxn_h (iz) = 1d-15
                    else 
                        rxn_h(iz) = rxn_h(iz)*1d1
                    end if 
                end if 
            end if 
            if (hb(iz)  < 0) then 
                ! print *, iz
                rxn_h(iz) = 0d0
                if (iz/=1) rxn_h(iz-1) = rxn_h(iz-1)/1d3
                if (iz/=ifmx-1) rxn_h(iz+1) = rxn_h(iz+1)/1d3
                if (iz<=ifmx-1-2) rxn_h(iz+2) = rxn_h(iz+2)/1d2
                if (iz>=3) rxn_h(iz-2) = rxn_h(iz-2)/1d2
                if (iz<=ifmx-1-3) rxn_h(iz+3) = rxn_h(iz+3)/1d1
                if (iz>=4) rxn_h(iz-3) = rxn_h(iz-2)/1d1
            end if 
        end if 
        ! if (hb(iz)  < 0) rxn(iz) = rxn(iz)/100d0
        if (abs(b(iz)) < 1d-8)  b(iz) = 0d0
        if (abs(h(iz)) < 1d-8)  h(iz) = 0d0
        if (rxn_h(iz) < 1d-30)  rxn_h(iz) = 0d0 
    end do 
endif

do iz = ifmx+1,nz
    if (cm(iz) > ceq(iz)) then 
        if (rxn_b(iz)/=0d0 .and. rxn_b(iz)< maxrate) rxn_b(iz) = rxn_b(iz) * 1d1  ! assuming maximum rate 1d-8
        if (rxn_b(iz)==0d0) rxn_b (iz) = 1d-15
    else  ! not saturated 
        if (cm(iz) > 0) then 
            if (rxn_b(iz)< maxrate) then   ! assuming maximum rate 1d-8
                if (rxn_b(iz)==0) then 
                    rxn_b (iz) = 1d-15
                else 
                    rxn_b(iz) = rxn_b(iz)*1d1
                end if 
            end if 
        end if 
        if (hb(iz)  < 0) then 
        ! print *, iz
        rxn_b(iz) = 0d0
        if (iz/=ifmx+1) rxn_b(iz-1) = rxn_b(iz-1)/1d3
        if (iz/=nz) rxn_b(iz+1) = rxn_b(iz+1)/1d3
        if (iz<=nz-2) rxn_b(iz+2) = rxn_b(iz+2)/1d2
        if (iz>=ifmx+3) rxn_b(iz-2) = rxn_b(iz-2)/1d2
        if (iz<=nz-3) rxn_b(iz+3) = rxn_b(iz+3)/1d1
        if (iz>=ifmx+4) rxn_b(iz-3) = rxn_b(iz-2)/1d1
        end if 
    end if 
    ! if (hb(iz)  < 0) rxn(iz) = rxn(iz)/100d0
    if (abs(b(iz)) < 1d-8)  b(iz) = 0d0
    if (abs(h(iz)) < 1d-8)  h(iz) = 0d0
    if (rxn_b(iz) < 1d-30)  rxn_b(iz) = 0d0 
end do 

do iz = ifmx,ifmx
    if (cm(iz) > ceq(iz)) then 
        if (rxn_b(iz)/=0d0 .and. rxn_b(iz)< maxrate) rxn_b(iz) = rxn_b(iz) * 1d1  ! assuming maximum rate 1d-8
        if (rxn_b(iz)==0d0) rxn_b (iz) = 1d-15
        if (rxn_h(iz)/=0d0 .and. rxn_h(iz)< maxrate) rxn_h(iz) = rxn_h(iz) * 1d1   ! assuming maximum rate 1d-8
        if (rxn_h(iz)==0d0) rxn_h (iz) = 1d-15
        if (rxn_hb/=0d0 .and. rxn_hb< rxn_hb_max) rxn_hb = rxn_hb * 1d1   ! assuming maximum rate 1d-8
        if (rxn_hb==0d0) rxn_hb = 1d-15
        if (rxn_bh/=0d0 .and. rxn_bh< rxn_bh_max) rxn_bh = rxn_bh * 1d1   ! assuming maximum rate 1d-8
        if (rxn_bh==0d0) rxn_bh = 1d-15
    else  ! not saturated 
        if (cm(iz) > 0) then 
            if (rxn_b(iz)< maxrate) then   ! assuming maximum rate 1d-8
                if (rxn_b(iz)==0) then 
                    rxn_b (iz) = 1d-15
                else 
                    rxn_b(iz) = rxn_b(iz)*1d1
                end if 
            end if 
            if (rxn_h(iz)< maxrate) then  ! assuming maximum rate 1d-8
                if (rxn_h(iz)==0) then 
                    rxn_h (iz) = 1d-15
                else 
                    rxn_h(iz) = rxn_h(iz)*1d1
                end if 
            end if 
            if (rxn_hb< rxn_hb_max) then  ! assuming maximum rate 1d-8
                if (rxn_hb==0) then 
                    rxn_hb = 1d-15
                else 
                    rxn_hb = rxn_hb*1d1
                end if 
            end if 
            if (rxn_bh< rxn_bh_max) then  ! assuming maximum rate 1d-8
                if (rxn_bh==0) then 
                    rxn_bh = 1d-15
                else 
                    rxn_bh = rxn_bh*1d1
                end if 
            end if 
        end if 
        if (h(iz)  < 0) then 
            ! print *, iz
            rxn_h(iz) = 0d0
            rxn_hb = 0d0
            if (iz/=1) rxn_h(iz-1) = rxn_h(iz-1)/1d3
            if (iz/=nz) rxn_h(iz+1) = rxn_h(iz+1)/1d3
            if (iz<=nz-2) rxn_h(iz+2) = rxn_h(iz+2)/1d2
            if (iz>=3) rxn_h(iz-2) = rxn_h(iz-2)/1d2
            if (iz<=nz-3) rxn_h(iz+3) = rxn_h(iz+3)/1d1
            if (iz>=4) rxn_h(iz-3) = rxn_h(iz-2)/1d1
        end if 
        if (b(iz)  < 0) then 
            ! print *, iz
            rxn_b(iz) = 0d0
            rxn_bh = 0d0
            if (iz/=1) rxn_b(iz-1) = rxn_b(iz-1)/1d3
            if (iz/=nz) rxn_b(iz+1) = rxn_b(iz+1)/1d3
            if (iz<=nz-2) rxn_b(iz+2) = rxn_b(iz+2)/1d2
            if (iz>=3) rxn_b(iz-2) = rxn_b(iz-2)/1d2
            if (iz<=nz-3) rxn_b(iz+3) = rxn_b(iz+3)/1d1
            if (iz>=4) rxn_b(iz-3) = rxn_b(iz-2)/1d1
        end if 
    end if 
    ! if (hb(iz)  < 0) rxn(iz) = rxn(iz)/100d0
    if (abs(b(iz)) < 1d-8)  b(iz) = 0d0
    if (abs(h(iz)) < 1d-8)  h(iz) = 0d0
    if (rxn_b(iz) < 1d-30)  rxn_b(iz) = 0d0 
    if (rxn_h(iz) < 1d-30)  rxn_h(iz) = 0d0 
    if (rxn_hb < 1d-30)  rxn_hb = 0d0 
    if (rxn_bh < 1d-30)  rxn_bh = 0d0 
end do 

if(ifmx/=1) then  ! bubble conc. expected from some equation that i forgot 
    expbb = (   &
        rho_h/rho_f*h(ifmx)*v(ifmx)*(ch-cm(ifmx)) &
        -(1d0-h(ifmx))*(dif_m(ifmx)+dif_m(ifmx-1))/2d0*(cm(ifmx)-cm(ifmx-1))/(0.5d0*(dz(ifmx)+dz(ifmx-1)))  &
        +(dif_m(ifmx)+dif_m(ifmx+1))/2d0*(cm(ifmx+1)-cm(ifmx))/(0.5d0*(dz(ifmx)+dz(ifmx+1)))  &
        )  &
        /(  &
        rho_b/rho_f*v(ifmx)*(cb-cm(ifmx))  &
        -(-1d0)*(dif_m(ifmx)+dif_m(ifmx+1))/2d0*(cm(ifmx+1)-cm(ifmx))/(0.5d0*(dz(ifmx)+dz(ifmx+1)))  &
        ) 
else 
    expbb = 0d0
endif
if (loc_display) then 
    print '(a)', 'expected bubble conc. at top if hydrate exists'
    print '(e9.3e2)', expbb
endif 

if (expbb > 0d0) then
    rxn_bh_ex =  (rxn_b(iz)*(cm(iz)-ceq(iz)) &
        -(v(iz)*poro(iz)*expbb)/dz(iz))/(expbb-h(iz))
    if (rxn_bh_ex >= 0d0) then 
        rxn_hb = rxn_bh_ex
        rxn_bh = rxn_bh_ex
    endif
endif
    

! #endif  

! do iz = 1, nz
    ! if (abs(hb(iz)) < 1d-8)  hb(iz) = 0d0
    ! if (rxn(iz) < 1d-30)  rxn(iz) = 0d0 
! end do 

! open (unit =200,file='./output/'//trim(adjustl(outfilename))//'-calc.txt',status='replace')
! do iz = 1, nz
    ! write(200,*) z(iz), cm(iz),ceq(iz),h(iz),b(iz), u(iz), v(iz), rxn_h(iz), rxn_b(iz)
! end do 
! close(200)

end do opt

endsubroutine calcCH4sys_3phs
!**************************************************************************************************************************************

!**************************************************************************************************************************************
subroutine calcCH4sys_2phs(                                                                     &
    ceq,nz,imx,ifmx,itrnewmax,tor3,home,runid,latint,lonint,zs,rho_h,rho_b,ch,cb,dif_m,poro     &
    ,poro_ext,v_sed,v_ext,dz,outfilename,poro_0,procn,rho_f,phi_m,z,v                           &
    ,itrmax,maxrate,rxn_bh_max,rxn_hb_max,ucnv,dif_s,dt,loc_display                             &
    ,cm,h,b,u,itr,hb,rxn_h,rxn_b,err3,sdif_bot,phi_hb,hx,flg_err,aomflx_ch4                     &! output
    ) 
implicit none
integer,intent(in)::nz,imx,ifmx,itrmax,latint,lonint,itrnewmax
real,intent(in)::ceq(nz),tor3,zs,rho_h,rho_b,ch,cb,dif_m(nz),poro(nz),poro_ext,v_sed,v_ext,dz(nz)  &
    ,poro_0,rho_f,phi_m(nz),z(nz),v(nz),maxrate,rxn_bh_max,rxn_hb_max,ucnv,dif_s(nz),dt  
character(100),intent(in)::home,runid,outfilename
character(3),intent(in)::procn
logical,intent(in)::loc_display
real,intent(out)::cm(nz),h(nz),b(nz),u(nz),hb(nz),rxn_b(nz),rxn_h(nz),err3,sdif_bot,phi_hb(nz),hx(nz),aomflx_ch4
integer,intent(out)::itr
logical,intent(out)::flg_err

! local variables
integer::nmx,iz,info,row,itrnew,ifmx_tmp
real,allocatable::amx(:,:),ymx(:),emx(:)
integer,allocatable::ipiv(:)
real::cm_ext,cmx(Nz),hbx(nz),bx(nz),ux(nz),rho_hb,chb
real::mhinv,mhinve,mbinv,mbinve,rxn(nz),rxn_hb,rxn_bh
real::phi_b(nz),phi_h(nz)
real::prodh,prodb
real::advlossc,diflossc,hydlossc,gaslossc,bioprodc,cmmb,hmb,bmb
real::residc
real::mhinvc,mbinvc
real::rxn_bh_ex,expbb
! real::sdif_bot ! should be output?

flg_err = .false.

nmx = 3*nz

if (allocated(amx)) deallocate(amx)
if (allocated(ymx)) deallocate(ymx)
if (allocated(emx)) deallocate(emx)
if (allocated(ipiv)) deallocate(ipiv)
allocate (amx(nmx,nmx),ymx(nmx),emx(nmx),ipiv(nmx))

!  here start of iteration to determine methane profile 

cm_ext = 0d0
! #ifdef satupflow
! cm_ext = ceq(nz)*1.00d0
! #endif
cm = 0d0
! cm = ceq ! 
b =  0d0
h =  0d0
! dt = 1d0
! rxn_scale = nz/100

! do iz = 1, nz
  ! rxn(iz) = 1d-8*exp(-(nz-iz)/rxn_scale)
! end do 

mhinv = 0d0
mbinv = 0d0

mhinve = 1d10
mbinve = 1d10

rxn = 0d0
rxn(ifmx+1:nz) = 1d-8

rxn_b = 0d0
rxn_b(ifmx+1:nz) = 1d-8
rxn_b(ifmx:nz) = 1d-8  ! better ?
rxn_h = 0d0
rxn_b(imx:ifmx) = 1d-8  !  added 

hb(1:ifmx) = h(1:ifmx)
hb(ifmx+1:nz) = b(ifmx+1:nz)

rxn_hb = 1d-8  ! why did I set rates for rxn_bh ?
rxn_bh = 1d-8  !  added 3/11/2018

do itr = 1, itrmax

! if (.not.any(hb<0)) then 
  ! if (itr/=1) then 
    ! rxn(1:nz-1) = rxn(2:nz)
    ! rxn(nz) = 1d-8
  ! end if 
! else 
  ! do iz = 1, nz
    ! if (hb(iz)<0) rxn(iz) = rxn(iz)/1d3
  ! end do 
! end if 

! open (unit =200, file='./output/'//trim(adjustl(outfilename))//'-uf.txt',status='replace')
do iz = 1, nz ! calculation of fluid velocity 
    ! h(25) = -1.52d-7
    u(iz) = (1d0-poro_0)*(poro_ext/(1d0-poro_ext)- poro(iz)*hb(iz)/(1d0-poro(iz))) *v_sed+poro_ext*v_ext  ! in cm/kyr
    u(iz) = u(iz)*1d-2/1d3/365d0/24d0/60d0/60d0
    ! write(200,*) u(iz), rxn(iz)
end do 
! close(200)

! #ifdef fixmid
! rxn_h(ifmx)=0d0  ! as assumed in Davie and Buffett (2003) 
! rxn_b(ifmx)=0d0
! #endif

! #ifdef fixmid2
! rxn_h(ifmx)=0d0  ! as assumed in Davie and Buffett (2003) 
! rxn_b(ifmx)=0d0
! #endif

cmx = cm

hbx = hb
hx = h
bx = b
ux = u
err3 = 1d4
itrnew = 0
do while (err3 > tor3)
amx = 0d0
ymx = 0d0

phi_b = 0d0
phi_h = 0d0
phi_hb = 0d0

prodh = 0d0
prodb = 0d0

itrnew = itrnew + 1

if (itrnew > itrnewmax) then 
  
    ! open(unit=250,file=trim(adjustl(outdir))//trim(adjustl(runid))//'/res/err_itrnew-'//trim(adjustl(procn))&
        ! //'.txt',status='unknown',position='append')
    ! write(250,*) latint, lonint, zs
    ! close(250)
    ! stop
    flg_err=.true.
    return
endif

do iz = 1, nz
    row = (iz-1)*3 + 1
    if (iz <= ifmx) then 
        chb = ch
        rho_hb = rho_h
    else
        chb = cb
        rho_hb = rho_b
    end if 

    if (iz < imx) then 
        ymx(row) = 1d0*(cmx(iz)-0d0)
        amx(row,row) = 1d0

        ymx(row+1) = 1d0*(hx(iz)-0d0)
        amx(row+1,row+1) = 1d0
        ymx(row+2) = 1d0*(ux(iz) - u(iz))
        amx(row+2,row+2) = 1d0

    ! else if (iz == imx) then 
        ! ymx(row) = dif_m*(poro(iz)*(1d0-hbx(iz))-poro(iz-1)*(1d0))/dz*(cmx(iz+1)-0d0)/2d0/dz &
            ! + dif_m*poro(iz)*(1d0-hbx(iz))*(cmx(iz+1) - 2d0*cmx(iz))/dz/dz  &
            ! + rho_hb/rho_f*(cmx(iz)-chb)*rxn(iz)*(cmx(iz)-ceq(iz)) &
            ! - ux(iz)*(cmx(iz+1)-0d0)/2d0/dz  &
            ! - (cmx(iz)-cm(iz))/dt 
        ! amx(row,row) =   &
            ! + dif_m*poro(iz)*(1d0-hbx(iz))*(-2d0)/dz/dz  &   
            ! +rho_hb/rho_f*rxn(iz)*(cmx(iz)-ceq(iz))    &
            ! +rho_hb/rho_f*(cmx(iz)-chb)*rxn(iz)    &
            ! -1d0/dt                 
        ! amx(row, row+3) = dif_m*(poro(iz)*(1d0-hbx(iz))-poro(iz-1)*1d0)/dz*1d0/2d0/dz  &
            ! + dif_m*poro(iz)*(1d0-hbx(iz))*(1d0)/dz/dz   &
            ! + ux(iz)/2d0/dz  

        ! amx(row, row+1) = dif_m*(poro(iz)*(-1d0))/dz*(cmx(iz+1)-0d0)/2d0/dz  &
            ! + dif_m*poro(iz)*(-1d0)*(cmx(iz+1)-2d0*cmx(iz))/dz/dz     
        ! amx(row, row+2) = -(cmx(iz+1)-0d0)/2d0/dz

        ! ymx(row+1) = rxn(iz)*(cmx(iz)-ceq(iz))-poro(iz)*(hbx(iz)-hb(iz))/dt -(v(iz)*poro(iz)*hbx(iz))/dz
        ! amx(row+1, row) = rxn(iz) 
        ! amx(row+1, row+1) = -poro(iz)*(1d0)/dt-(v(iz)*poro(iz))/dz

        ! ymx(row+2) = ux(iz) - (1d0-poro_0)*(poro_ext/(1d0-poro_ext)- poro(iz)*hbx(iz)/(1d0-poro(iz)))  &
            ! *v_sed*ucnv+poro_ext*v_ext*ucnv
        ! amx(row+2,row+2) = 1d0
        ! amx(row+2,row+1) = - (1d0-poro_0)*(- poro(iz)/(1d0-poro(iz))) *v_sed*ucnv

    ! else if (iz == ifmx) then 
        ! ymx(row) = rho_h/rho_f*(-ch)*(v(iz)*h(iz)*poro(iz)-v(iz-1)*h(iz-1)*poro(iz-1))/dz+phi_m(iz)&
            ! -(-cm(iz))/dt
        ! amx(row,row) = dif_m*poro(iz)*(1d0-h(iz))*(-2d0/dz/dz)       &
            ! +rho_h/rho_f*(v(iz)*h(iz)*poro(iz)-v(iz-1)*h(iz-1)*poro(iz-1))/dz   &
            ! -1d0/dt 
        ! amx(row, row+1) = dif_m*poro(iz)*(1d0-h(iz))*(1d0/dz/dz)  &
            ! + dif_m*(poro(iz)*(1d0-h(iz))-poro(iz-1)*(1d0-h(iz-1)))/dz  &
            ! *1d0/dz/2d0 - u(iz)*(1d0)/2d0/dz
        ! amx(row, row-1) = dif_m*poro(iz)*(1d0-h(iz))*(1d0/dz/dz)  &
            ! + dif_m*(poro(iz)*(1d0-h(iz))-poro(iz-1)*(1d0-h(iz-1)))/2d0/dz  &
            ! *(-1d0)/dz/2d0 - u(iz)*(-1d0)/2d0/dz
    else if (iz == imx) then 
        ymx(row) = ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hx(iz)+hx(iz+1))/2d0)*(cmx(iz+1)-cmx(iz))&
                /(0.5d0*(dz(iz)+dz(iz+1)))  &
            - (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hx(iz)+0d0)/2d0)*(cmx(iz)-0d0)&
                /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  &
            + (-rho_h/rho_f*ch)*rxn_h(iz)*(cmx(iz)-ceq(iz)) &
            - (cmx(iz+1)*ux(iz+1)-0d0)/2d0/dz(iz)  &
            ! - poro(iz)*((1d0-hx(iz))*cmx(iz)-(1d0-h(iz))*cm(iz))/dt &
            + phi_m(iz)
        amx(row,row) =  ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hx(iz)+hx(iz+1))/2d0)*(-1d0)&
                /(0.5d0*(dz(iz)+dz(iz+1)))  &
            - (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hx(iz)+hx(iz-1))/2d0)*(1d0)&
                /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  &
            ! -poro(iz)*((1d0-hx(iz))*1d0)/dt  &   
            +(-rho_h/rho_f*ch)*rxn_h(iz)           
        amx(row, row+3) =  ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hx(iz)+hx(iz+1))/2d0)*(1d0)&
                /(0.5d0*(dz(iz)+dz(iz+1))))/dz(iz)  &
            -ux(iz+1)/2d0/dz(iz)            
        amx(row, row-3) = 0d0

        amx(row, row+1) =    &
            ! - poro(iz)*((-1d0)*cmx(iz))/dt    &
            ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(-1d0/2d0)*(cmx(iz+1)-cmx(iz))&
                /(0.5d0*(dz(iz)+dz(iz+1)))  &
            - (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(-1d0/2d0)*(cmx(iz)-0d0)&
                /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   
        amx(row, row-3+1) = 0d0  
        amx(row, row+3+1) = ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(-1d0/2d0)*(cmx(iz+1)-cmx(iz))&
                /(0.5d0*(dz(iz)+dz(iz+1))))/dz(iz)  
        amx(row, row+3+2) = -(cmx(iz+1))/2d0/dz(iz)

        ymx(row+1) = rxn_h(iz)*(cmx(iz)-ceq(iz)) &
            ! - poro(iz)*(hx(iz)-h(iz))/dt &
            -(v(iz)*poro(iz)*hx(iz)-0d0)/dz(iz)
        amx(row+1, row) = rxn_h(iz) 
        amx(row+1, row+1) = &  
            ! -poro(iz)/dt  &
            -(v(iz)*poro(iz))/dz(iz)
            ! amx(row+1, row-4+1) = -(-v(iz-1)*poro(iz-1))/dz

        ymx(row+2) = ux(iz) - ucnv*((1d0-poro_0)*(poro_ext/(1d0-poro_ext)- poro(iz)*hx(iz)/(1d0-poro(iz)))   &
            *v_sed + poro_ext*v_ext)
        amx(row+2,row+2) = 1d0
        amx(row+2,row+1) = - (1d0-poro_0)*(- poro(iz)/(1d0-poro(iz))) *v_sed*ucnv

    else if (iz == nz) then  
        if (cm_ext == 0d0) then  ! no flux toward bottom (cm(iz+1) = cm(iz))
            ymx(row) = (-(dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz-1)+poro(iz))/2d0*(1d0-(hx(iz-1)+hx(iz))/2d0)*(cmx(iz)-cmx(iz-1))&
                    /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  &
                + (-rho_h/rho_f*ch)*rxn_h(iz)*(cmx(iz)-ceq(iz)) &
                - (ux(iz)*cmx(iz)-ux(iz-1)*cmx(iz-1))/2d0/dz(iz)  &
                ! - poro(iz)*((1d0-bx(iz))*cmx(iz)-(1d0-b(iz))*cm(iz))/dt &
                + phi_m(iz)
            amx(row,row) = (-(dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz-1)+poro(iz))/2d0*(1d0-(hx(iz-1)+hx(iz))/2d0)&
                    /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  &
                + (-rho_h/rho_f*ch)*rxn_h(iz)   &
                ! - poro(iz)*(1d0-bx(iz))*1d0/dt   &
                - ux(iz)/2d0/dz(iz)                
            amx(row, row-3) = (-(dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz-1)+poro(iz))/2d0*(1d0-(hx(iz-1)+hx(iz))/2d0)*(-1d0)&
                    /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  &
                - ux(iz-1)*(-1d0)/2d0/dz(iz)

            amx(row, row+1) =  &
                ! - poro(iz)*(-1d0)*cmx(iz)/dt  &
                + (-(dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz-1)+poro(iz))/2d0*(-1d0/2d0)*(cmx(iz)-cmx(iz-1))&
                    /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   
            amx(row, row-3+1) = (-(dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz-1)+poro(iz))/2d0*(-(1d0)/2d0)*(cmx(iz)-cmx(iz-1))&
                    /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)
            amx(row, row+2) = -(cmx(iz))/2d0/dz(iz)
            amx(row, row-3+2) = -(-cmx(iz-1))/2d0/dz(iz)

            ymx(row+1) = rxn_h(iz)*(cmx(iz)-ceq(iz))  &
                ! -poro(iz)*(bx(iz)-b(iz))/dt   &
                -(v(iz)*poro(iz)*hx(iz)-v(iz-1)*poro(iz-1)*hx(iz-1))/dz(iz)
            amx(row+1, row) = rxn_h(iz) 
            amx(row+1, row+1) = &
                ! -poro(iz)/dt  & 
                -(v(iz)*poro(iz))/dz(iz)
                amx(row+1, row-3+1) = -(-v(iz-1)*poro(iz-1))/dz(iz)

            ymx(row+2) = ux(iz) - ucnv*((1d0-poro_0)*(poro_ext/(1d0-poro_ext)- poro(iz)*hx(iz)/(1d0-poro(iz)))   &
                *v_sed + poro_ext*v_ext)
            amx(row+2,row+2) = 1d0
            amx(row+2,row+1) = - (1d0-poro_0)*(- poro(iz)/(1d0-poro(iz))) *v_sed*ucnv

        else ! cm(iz+1) = cm_ext
            !!! following set of equations does not converge so abandoned
            ! ymx(row) = (dif_m*poro(iz)*(1d0-bx(iz))*(cm_ext-cmx(iz))/dz  &
                ! - dif_m*(poro(iz)+poro(iz-1))/2d0*(1d0-(bx(iz)+bx(iz-1))/2d0)*(cmx(iz)-cmx(iz-1))/dz)/dz  &
                ! + (cmx(iz)-rho_b/rho_f*cb)*rxn_b(iz)*(cmx(iz)-ceq(iz)) &
                ! - ux(iz)*(cm_ext-cmx(iz-1))/2d0/dz  &
                ! - poro(iz)*(1d0-bx(iz))*(cmx(iz)-cm(iz))/dt + phi_m(iz)
            ! amx(row,row) =  (dif_m*poro(iz)*(1d0-bx(iz))*(1d0)/dz  &
                ! - dif_m*(poro(iz)+poro(iz-1))/2d0*(1d0-(bx(iz)+bx(iz-1))/2d0)*(1d0)/dz)/dz  &  
                ! +rxn_b(iz)*(cmx(iz)-ceq(iz))    &
                ! +(cmx(iz)-rho_b/rho_f*cb)*rxn_b(iz)    &
                ! -poro(iz)*(1d0-bx(iz))*1d0/dt                      
            ! amx(row, row-4) = (- dif_m*(poro(iz)+poro(iz-1))/2d0*(1d0-(bx(iz)+bx(iz-1))/2d0)*(-1d0)/dz)/dz  &
                ! - ux(iz)*(-1d0)/2d0/dz

            ! amx(row, row+2) = (dif_m*poro(iz)*(1d0)*(cm_ext-cmx(iz))/dz  &
                ! - dif_m*(poro(iz)+poro(iz-1))/2d0*(-1d0/2d0)*(cmx(iz)-cmx(iz-1))/dz)/dz   &
                ! - poro(iz)*(-1d0)*(cmx(iz)-cm(iz))/dt        
            ! amx(row, row-4+2) = (- dif_m*(poro(iz)+poro(iz-1))/2d0*(-1d0/2d0)*(cmx(iz)-cmx(iz-1))/dz)/dz  
            ! amx(row, row+2) = -(cm_ext-cmx(iz-1))/2d0/dz
            !!!

            ymx(row) = 1d0*(cmx(iz) - cm_ext)
            amx(row,row) = 1d0

            ymx(row+1) = rxn_h(iz)*(cmx(iz)-ceq(iz)) &
                ! - poro(iz)*(bx(iz)-b(iz))/dt &
                -(v(iz)*poro(iz)*hx(iz)-v(iz-1)*poro(iz-1)*hx(iz-1))/dz(iz)
            amx(row+1, row) = rxn_h(iz) 
            amx(row+1, row+1) = &
                ! -poro(iz)/dt  &
                -(v(iz)*poro(iz))/dz(iz)
            amx(row+1, row-3+1) = -(-v(iz-1)*poro(iz-1))/dz(iz)


            ymx(row+2) = ux(iz) - ucnv*((1d0-poro_0)*(poro_ext/(1d0-poro_ext)- poro(iz)*hx(iz)/(1d0-poro(iz)))   &
                *v_sed + poro_ext*v_ext)
            amx(row+2,row+2) = 1d0
            amx(row+2,row+1) = - (1d0-poro_0)*(- poro(iz)/(1d0-poro(iz))) *v_sed*ucnv
        end if 

    ! else if (iz > imx) then !  trying to simplifying formulations; not working
        ! ymx(row) = (dif_m*(poro(iz)+poro(iz+1))/2d0*(1d0-(hbx(iz)+hbx(iz+1))/2d0)*(cmx(iz+1)-cmx(iz))/dz  &
            ! - dif_m*(poro(iz)+poro(iz-1))/2d0*(1d0-(hbx(iz)+hbx(iz-1))/2d0)*(cmx(iz)-cmx(iz-1))/dz)/dz  &
            ! + (cmx(iz)-rho_h/rho_f*ch)*rxn_h(iz)*(cmx(iz)-ceq(iz)) &
            ! + (cmx(iz)-rho_b/rho_f*cb)*rxn_b(iz)*(cmx(iz)-ceq(iz)) &
            ! - ux(iz)*(cmx(iz+1)-cmx(iz-1))/2d0/dz  &
            ! - poro(iz)*(1d0-hbx(iz))*(cmx(iz)-cm(iz))/dt + phi_m(iz)
        ! amx(row,row) =  (dif_m*(poro(iz)+poro(iz+1))/2d0*(1d0-(hbx(iz)+hbx(iz+1))/2d0)*(-1d0)/dz  &
            ! - dif_m*(poro(iz)+poro(iz-1))/2d0*(1d0-(hbx(iz)+hbx(iz-1))/2d0)*(1d0)/dz)/dz  &  
            ! +rxn_h(iz)*(cmx(iz)-ceq(iz))    &
            ! +rxn_b(iz)*(cmx(iz)-ceq(iz))    &
            ! +(cmx(iz)-rho_h/rho_f*ch)*rxn_h(iz)    &
            ! +(cmx(iz)-rho_b/rho_f*cb)*rxn_b(iz)    &
            ! -poro(iz)*(1d0-hbx(iz))*1d0/dt             
        ! amx(row, row+4) =  (dif_m*(poro(iz)+poro(iz+1))/2d0*(1d0-(hbx(iz)+hbx(iz+1))/2d0)*(1d0)/dz)/dz  &
            ! -ux(iz)/2d0/dz            
        ! amx(row, row-4) = (- dif_m*(poro(iz)+poro(iz-1))/2d0*(1d0-(hbx(iz)+hbx(iz-1))/2d0)*(-1d0)/dz)/dz  &
            ! - ux(iz)*(-1d0)/2d0/dz

        ! amx(row, row+1) = (dif_m*(poro(iz)+poro(iz+1))/2d0*(-1d0/2d0)*(cmx(iz+1)-cmx(iz))/dz  &
            ! - dif_m*(poro(iz)+poro(iz-1))/2d0*(-1d0/2d0)*(cmx(iz)-cmx(iz-1))/dz)/dz   &
            ! - poro(iz)*(-1d0)*(cmx(iz)-cm(iz))/dt
        ! amx(row, row-4+1) = (- dif_m*(poro(iz)+poro(iz-1))/2d0*(-1d0/2d0)*(cmx(iz)-cmx(iz-1))/dz)/dz  
        ! amx(row, row+4+1) = (dif_m*(poro(iz)+poro(iz+1))/2d0*(-1d0/2d0)*(cmx(iz+1)-cmx(iz))/dz)/dz  
        ! amx(row, row+3) = -(cmx(iz+1)-cmx(iz-1))/2d0/dz

        ! amx(row, row+2) = (dif_m*(poro(iz)+poro(iz+1))/2d0*(-1d0/2d0)*(cmx(iz+1)-cmx(iz))/dz  &
            ! - dif_m*(poro(iz)+poro(iz-1))/2d0*(-1d0/2d0)*(cmx(iz)-cmx(iz-1))/dz)/dz   &
            ! - poro(iz)*(-1d0)*(cmx(iz)-cm(iz))/dt
        ! amx(row, row-4+2) = (- dif_m*(poro(iz)+poro(iz-1))/2d0*(-1d0/2d0)*(cmx(iz)-cmx(iz-1))/dz)/dz  
        ! amx(row, row+4+2) = (dif_m*(poro(iz)+poro(iz+1))/2d0*(-1d0/2d0)*(cmx(iz+1)-cmx(iz))/dz)/dz  
        ! amx(row, row+3) = -(cmx(iz+1)-cmx(iz-1))/2d0/dz

        ! ymx(row+1) = rxn_h(iz)*(cmx(iz)-ceq(iz)) - poro(iz)*(hx(iz)-h(iz))/dt &
            ! -(v(iz)*poro(iz)*hx(iz)-v(iz-1)*poro(iz-1)*hx(iz-1))/dz
        ! amx(row+1, row) = rxn_h(iz) 
        ! amx(row+1, row+1) = -poro(iz)/dt-(v(iz)*poro(iz))/dz
        ! amx(row+1, row-4+1) = -(-v(iz-1)*poro(iz-1))/dz

        ! ymx(row+2) = rxn_b(iz)*(cmx(iz)-ceq(iz)) - poro(iz)*(bx(iz)-b(iz))/dt &
            ! -(v(iz)*poro(iz)*bx(iz)-v(iz-1)*poro(iz-1)*bx(iz-1))/dz
        ! amx(row+2, row) = rxn_b(iz) 
        ! amx(row+2, row+2) = -poro(iz)/dt-(v(iz)*poro(iz))/dz
        ! amx(row+2, row-4+2) = -(-v(iz-1)*poro(iz-1))/dz

        ! ymx(row+3) = ux(iz) - ucnv*((1d0-poro_0)*(poro_ext/(1d0-poro_ext)- poro(iz)*hbx(iz)/(1d0-poro(iz)))   &
            ! *v_sed + poro_ext*v_ext)
        ! amx(row+3,row+3) = 1d0
        ! amx(row+3,row+1) = - (1d0-poro_0)*(- poro(iz)/(1d0-poro(iz))) *v_sed*ucnv
        ! amx(row+3,row+2) = - (1d0-poro_0)*(- poro(iz)/(1d0-poro(iz))) *v_sed*ucnv

    else if ((iz < nz).and.(iz > imx)) then 
        ymx(row) = ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hx(iz)+hx(iz+1))/2d0)*(cmx(iz+1)-cmx(iz))&
                /(0.5d0*(dz(iz)+dz(iz+1)))  &
            - (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hx(iz)+hx(iz-1))/2d0)*(cmx(iz)-cmx(iz-1))&
                /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  &
            + (-rho_h/rho_f*ch)*rxn_h(iz)*(cmx(iz)-ceq(iz))   &
            - (ux(iz+1)*cmx(iz+1)-ux(iz-1)*cmx(iz-1))/2d0/dz(iz)    &
            ! - poro(iz)*((1d0-hx(iz))*cmx(iz)-(1d0-h(iz))*cm(iz))/dt   &
            + phi_m(iz)  
        amx(row,row) =  ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hx(iz)+hx(iz+1))/2d0)*(-1d0)&
                /(0.5d0*(dz(iz)+dz(iz+1)))  &
            - (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hx(iz)+hx(iz-1))/2d0)*(1d0)&
                /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  &  
            ! -poro(iz)*(1d0-hx(iz))*1d0/dt      &       
            +(-rho_h/rho_f*ch)*rxn_h(iz)    
        amx(row, row+3) =  ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hx(iz)+hx(iz+1))/2d0)*(1d0)&
                /(0.5d0*(dz(iz)+dz(iz+1))))/dz(iz)  &
            -ux(iz+1)/2d0/dz(iz)            
        amx(row, row-3) = (- (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hx(iz)+hx(iz-1))/2d0)*(-1d0)&
                /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  &
            - ux(iz-1)*(-1d0)/2d0/dz(iz)

        amx(row, row+1) = &
            ! - poro(iz)*(-1d0)*cmx(iz)/dt  &
            +((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(-1d0/2d0)*(cmx(iz+1)-cmx(iz))/(0.5d0*(dz(iz)+dz(iz+1)))  &
            - (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(-1d0/2d0)*(cmx(iz)-cmx(iz-1))/(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   
        amx(row, row-3+1) = (- (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(-1d0/2d0)*(cmx(iz)-cmx(iz-1))&
                /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  
        amx(row, row+3+1) = ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(-1d0/2d0)*(cmx(iz+1)-cmx(iz))&
                /(0.5d0*(dz(iz)+dz(iz+1))))/dz(iz)  
        amx(row, row-3+2) = -(-cmx(iz-1))/2d0/dz(iz)
        amx(row, row+3+2) = -(cmx(iz+1))/2d0/dz(iz)

        ymx(row+1) = rxn_h(iz)*(cmx(iz)-ceq(iz)) &
            ! - poro(iz)*(hx(iz)-h(iz))/dt &
            -(v(iz)*poro(iz)*hx(iz)-v(iz-1)*poro(iz-1)*hx(iz-1))/dz(iz)
        amx(row+1, row) = rxn_h(iz) 
        amx(row+1, row+1) = &
            ! -poro(iz)/dt  &
            -(v(iz)*poro(iz))/dz(iz)
        amx(row+1, row-3+1) = -(-v(iz-1)*poro(iz-1))/dz(iz)

        ymx(row+2) = ux(iz) - ucnv*((1d0-poro_0)*(poro_ext/(1d0-poro_ext)- poro(iz)*hx(iz)/(1d0-poro(iz)))   &
            *v_sed + poro_ext*v_ext)
        amx(row+2,row+2) = 1d0
        amx(row+2,row+1) = - (1d0-poro_0)*(- poro(iz)/(1d0-poro(iz))) *v_sed*ucnv

    end if 
    phi_hb(iz) = (rxn_h(iz)+rxn_b(iz))*(cmx(iz)-ceq(iz))
    phi_h(iz) = rxn_h(iz)*(cmx(iz)-ceq(iz))
    phi_b(iz) = rxn_b(iz)*(cmx(iz)-ceq(iz))

    prodb = prodb + rho_b/rho_f*cb*phi_b(iz)*dz(iz)
    prodh = prodh + rho_h/rho_f*ch*phi_h(iz)*dz(iz)
end do 

ymx = -ymx

! open (unit =200,file='test-mtx1.txt',status='replace')
! open (unit =300,file='test-mtx2.txt',status='replace')
! do iz = imx, nz
  ! row = iz - imx + 1
  ! write(200,*) (amx(row,col),col = 1,nmx)
  ! write(300,*) ymx(row)
! end do 
! close(200)
! close(300)

call DGESV(nmx,int(1),amx,nmx,ipiv,ymx,nmx,info) 

emx = 0d0
do iz = 1, nz
    row = 3*(iz-1)  + 1
    cmx(iz) = cmx(iz)+ymx(row)
    if (abs(cmx(iz))>1d-10) emx(row) = abs(ymx(row)/cmx(iz))
    hx(iz) = hx(iz)+ymx(row+1)
    if (abs(hx(iz))>1d-10) emx(row+1) = abs(ymx(row+1)/hx(iz))
    ux(iz) = ux(iz) + ymx(row+2)
    if (ux(iz)/=0d0) emx(row+2) = abs(ymx(row+2)/ux(iz))
end do 

hbx = hx + bx

err3 = maxval(emx)
if (loc_display) then 
    print *, "============================================"
    print '(a12,i5,a12,e9.3e2)', "iteration:",itrnew,"/error:", err3
    print *, '>>>>>>'
    print '(5(3x,a9))', 'z', 'cmx','hx','bx','ux'
    do iz=1,nz,nz/10
        print '(5(3x,e9.3e2))', z(iz), cmx(iz),hx(iz),bx(iz),ux(iz)
    enddo 
    ! print *, "============================================"
    ! print *, "iteration:",itr,"/error:", err3
    ! print *, (z(iz), iz = 1,nz, nz/10 )
    ! print *,(cmx(iz), iz = 1,nz, nz/10)
    ! print *,(hx(iz), iz = 1,nz, nz/10)
    ! print *,(bx(iz), iz = 1,nz, nz/10)
    ! print *,(ux(iz), iz = 1,nz, nz/10)
endif
! if (any(hbx < 0)) then 
  ! print *, "negative hydrate or gas at interation No.", itr
  
  ! open (unit =200,file='test-calc_stoped.txt',status='replace')
  ! do iz = 1, nz
    ! write(200,*) z(iz), cm(iz),ceq(iz),h(iz),b(iz), u(iz), v(iz)
  ! end do 
  ! close(200)
  ! stop
! end if 

end do 

cm = cmx
! h(1:ifmx) = hbx(1:ifmx)
! b(ifmx+1:nz) = hbx(ifmx+1:nz)
h = hx
b = bx
u = ux

! hb(1:ifmx) = h(1:ifmx)
! hb(ifmx+1:nz) = b(ifmx+1:nz)

hb = h+b



! advloss = u(nz)*cm(nz)
do iz = 1, nz
    if (cm(iz) > 0) exit
end do 
! difloss = dif_m*poro(iz)*(1d0-hb(iz))*cm(iz)/dz
sdif_bot = 96d0/16d0*dif_m(iz)/dif_s(iz)*cm(iz)/dz(iz)
! do iz = 1, nz 
  ! if (hb(iz)>0) exit
! end do 

! if (iz<=nz) then 
! hydloss = rho_h/rho_f*ch*(hb(ifmx)*poro(ifmx)*v(ifmx)-hb(iz)*poro(iz)*v(iz)) ! note ifmx = nz
! else 
! hydloss = 0d0
! end if 

! gasloss = 0d0

! resid = bioprod - advloss - difloss - hydloss - gasloss
! err3 = abs(1d2*resid/bioprod)
! #ifdef display
! print *,"bioprod, advloss, difloss, hydloss, gasloss, resid, error[%]"
! print *, bioprod, advloss, difloss, hydloss, gasloss, resid, err3
! #endif
! if (err3 < 1d0) then 
  ! do iz = 1, nz
    ! if (hb(iz) < 0d0) hb(iz) = 0d0
  ! end do 
! end if 

! if (all(h>=0).and.all(b>=0).and.all(cm>=0).and.err3<2.d0 .and. itr>itrmin) exit 
! #ifdef display
! print *,"all(h>=0)",all(h>=0),"all(b>=0)",all(b>=0),"all(cm>=0)",all(cm>=0),"err3<2.d0",err3<2.d0,"itr>itrmin",itr>itrmin
! #endif

! mass balance check again 
! #ifdef chk2
advlossc = 0d0
diflossc = 0d0
hydlossc = 0d0
gaslossc = 0d0
bioprodc = 0d0

prodh = 0d0
prodb = 0d0

cmmb = 0d0
hmb = 0d0
bmb = 0d0

do iz = 1, nz
    ! bioprodc = bioprodc + phi_m(iz)*dz
    if (iz < imx) then
        if (phi_m(iz) > 0d0) print *, "phi_m(iz) > 0d0 while iz < imx"
        cycle
    else if (iz == imx) then 
        diflossc = diflossc + dz(iz)*( &
            ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(h(iz)+h(iz+1))/2d0)*(cm(iz+1)-cm(iz))&
                /(0.5d0*(dz(iz)+dz(iz+1)))  &
            - (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(h(iz)+0d0)/2d0)*(cm(iz)-0d0)&
                /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   &
            )

        advlossc = advlossc + dz(iz)*(  &
            - (cm(iz+1)*u(iz+1)-0d0)/2d0/dz(iz)  &
            )

        hydlossc = hydlossc + dz(iz)*(  &
            (-rho_h/rho_f*ch)*rxn_h(iz)*(cm(iz)-ceq(iz))  &
            )

        prodh = prodh + dz(iz)*rho_h/rho_f*ch*( &
            -(v(iz)*poro(iz)*h(iz)-0d0)/dz(iz)       &
            ) 

        bioprodc = bioprodc + phi_m(iz)*dz(iz)
    else if ((iz > imx) .and. (iz < ifmx)) then 
    diflossc = diflossc + dz(iz)*( & 
        ((dif_m(iz)+dif_m(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(h(iz)+h(iz+1))/2d0)*(cm(iz+1)-cm(iz))&
            /(0.5d0*(dz(iz)+dz(iz+1)))  &
        - (dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(h(iz)+h(iz-1))/2d0)*(cm(iz)-cm(iz-1))&
            /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  &
        )

    advlossc = advlossc + dz(iz)*(  &
        - (u(iz+1)*cm(iz+1)-u(iz-1)*cm(iz-1))/2d0/dz(iz)  &
        )

    hydlossc = hydlossc + dz(iz)*(  &
        (-rho_h/rho_f*ch)*rxn_h(iz)*(cm(iz)-ceq(iz))  &
        )

    prodh = prodh + dz(iz)*rho_h/rho_f*ch*( &
        -(v(iz)*poro(iz)*h(iz)-v(iz-1)*poro(iz-1)*h(iz-1))/dz(iz)    &
        ) 

    bioprodc = bioprodc + phi_m(iz)*dz(iz)
    else if (iz == nz) then  
        if (cm_ext == 0d0) then !  no flux boundary 
            diflossc = diflossc + dz(iz)*(  & 
                (-(dif_m(iz)+dif_m(iz-1))/2d0*(poro(iz-1)+poro(iz))/2d0*(1d0-(h(iz-1)+h(iz))/2d0)*(cm(iz)-cm(iz-1))&
                    /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)  &
                )

            advlossc = advlossc + dz(iz)*(  &
                - (u(iz)*cm(iz)-u(iz-1)*cm(iz-1))/2d0/dz(iz)  &
                )

            hydlossc = hydlossc + dz(iz)*(  &
                (-rho_h/rho_f*ch)*rxn_h(iz)*(cm(iz)-ceq(iz))  &
                )

            prodh = prodh + dz(iz)*rho_h/rho_f*ch*( &
                -(v(iz)*poro(iz)*h(iz)-v(iz-1)*poro(iz-1)*h(iz-1))/dz(iz)    &
                ) 

            bioprodc = bioprodc + phi_m(iz)*dz(iz)
        else 

        end if     

    end if

enddo  

residc = bioprodc + advlossc + diflossc + hydlossc + gaslossc
err3 = abs(1d2*residc/bioprodc)

if (loc_display) then 
    print '(a)', '**** flx check ****'
    print '(7(3x,a9))','bioprod', 'advlossc', 'difloss', 'hydloss', 'gasloss', 'resid', 'error[%]'
    print '(7(3x,e9.3e2))', bioprodc, advlossc, diflossc, hydlossc, gaslossc, residc, err3
    print '(4(3x,a9))', 'prodh', 'hydloss', 'prodb', 'gasloss'
    print '(4(3x,e9.3e2))', prodh, hydlossc, prodb, gaslossc
    ! print *, 'second check'
    ! print *,"bioprod, advlossc, difloss, hydloss, gasloss, resid, error[%]"
    ! print *, bioprodc, advlossc, diflossc, hydlossc, gaslossc, residc, err3
    ! print *, "prodh, hydloss, prodb, gasloss"
    ! print *, prodh, hydlossc, prodb, gaslossc
endif


aomflx_ch4 = diflossc * 1d6/16d0* 60d0*60d0*24d0*365.25d0  ! converting from g/g*m/sec to g/m2/yr 
aomflx_ch4 = aomflx_ch4*1d-4

! #endif

if (itr/=1) then 
    mhinvc = mhinv
    mbinvc = mbinv
    mhinv = 0d0
    mbinv = 0d0
    do iz = 1,nz
        mhinv=mhinv+poro(iz)*h(iz)*rho_h*ch*dz(iz)
        mbinv=mbinv+poro(iz)*b(iz)*rho_b*cb*dz(iz)
    enddo

    if (mhinv/=0d0) then 
        mhinve = abs((mhinv-mhinvc)/mhinv)
    else  
        mhinve = abs((mhinv-mhinvc))
    end if 

    if (mbinv/=0d0) then 
        mbinve = abs((mbinv-mbinvc)/mbinv)
    else  
        mbinve = abs((mbinv-mbinvc))
    end if 

end if 

if (loc_display) then 
    print '(3(3x,a9))', 'mhinv', 'mhinvc', 'mhinve'
    print '(3(3x,e9.3e2))',  mhinv, mhinvc, mhinve
    print '(3(3x,a9))', 'mbinv', 'mbinvc', 'mbinve'
    print '(3(3x,e9.3e2))',  mbinv, mbinvc, mbinve
endif

! #ifdef record
! open(unit=200,file=trim(adjustl(home))//trim(adjustl(runid))//'/profiles/'//trim(adjustl(outfilename))&
    ! //'/ch4.txt',status='replace'
! do iz = 1, nz
    ! write(200,*) z(iz), cm(iz),ceq(iz),h(iz),b(iz), u(iz), v(iz), rxn_h(iz), rxn_b(iz)
! end do 
! close(200)
! #endif


! print *, "hb(171), hb(172), rxn(171), rxn(172)"
! print *,  hb(171), hb(172), rxn(171), rxn(172)
! #if 0
do iz = 1,ifmx
    if (cm(iz) > ceq(iz)) then 
        if (rxn_h(iz)/=0d0 .and. rxn_h(iz)< maxrate) rxn_h(iz) = rxn_h(iz) * 1d1   ! assuming maximum rate 1d-8
        if (rxn_h(iz)==0d0) rxn_h (iz) = 1d-15
    else  ! not saturated 
        if (cm(iz) > 0) then 
            if (rxn_h(iz)< maxrate) then  ! assuming maximum rate 1d-8
                if (rxn_h(iz)==0) then 
                    rxn_h (iz) = 1d-15
                else 
                    rxn_h(iz) = rxn_h(iz)*1d1
                end if 
            end if 
        end if 
        if (hb(iz)  < 0) then 
            ! print *, iz
            rxn_h(iz) = 0d0
            if (iz/=1) rxn_h(iz-1) = rxn_h(iz-1)/1d3
            if (iz/=ifmx-1) rxn_h(iz+1) = rxn_h(iz+1)/1d3
            if (iz<=ifmx-1-2) rxn_h(iz+2) = rxn_h(iz+2)/1d2
            if (iz>=3) rxn_h(iz-2) = rxn_h(iz-2)/1d2
            if (iz<=ifmx-1-3) rxn_h(iz+3) = rxn_h(iz+3)/1d1
            if (iz>=4) rxn_h(iz-3) = rxn_h(iz-2)/1d1
        end if 
    end if 
    ! if (hb(iz)  < 0) rxn(iz) = rxn(iz)/100d0
    if (abs(b(iz)) < 1d-8)  b(iz) = 0d0
    if (abs(h(iz)) < 1d-8)  h(iz) = 0d0
    if (rxn_h(iz) < 1d-30)  rxn_h(iz) = 0d0 
end do 


! do iz = 1, nz
    ! if (abs(hb(iz)) < 1d-8)  hb(iz) = 0d0
    ! if (rxn(iz) < 1d-30)  rxn(iz) = 0d0 
! end do 

! open (unit =200,file='./output/'//trim(adjustl(outfilename))//'-calc.txt',status='replace')
! do iz = 1, nz
    ! write(200,*) z(iz), cm(iz),ceq(iz),h(iz),b(iz), u(iz), v(iz), rxn_h(iz), rxn_b(iz)
! end do 
! close(200)

end do 

endsubroutine calcCH4sys_2phs
!**************************************************************************************************************************************

!**************************************************************************************************************************************
subroutine calcCnsvs(   &
    itrcns,clss,clrxn,clgrad,nz,ifmx,itrnewmax,tor3,home,runid,latint,lonint,zs,rho_h,rho_b,ch,cb,dif_c,poro  &
    ,dz,procn,rho_f,v,u,hb,cc_0,cc_ext,dt,phi_hb,z,loc_display   &
    ,err3,cc,ccx,flg_err            & ! output
    ) 
implicit none
integer,intent(in)::nz,ifmx,latint,lonint,itrnewmax,clss,clrxn,clgrad,itrcns
real,intent(in)::tor3,zs,rho_h,rho_b,ch,cb,dif_c(nz),poro(nz),dz(nz)  &
    ,rho_f,v(nz),u(nz),hb(nz),phi_hb(nz),cc_0,cc_ext,dt,z(nz)
character(100),intent(in)::home,runid
character(3),intent(in)::procn
logical,intent(in)::loc_display
real,intent(out)::err3
real,intent(inout)::cc(nz),ccx(nz)
logical,intent(out)::flg_err

! local variables
integer::nmx,iz,info,row,itrnew,ifmx_tmp
real,allocatable::amx(:,:),ymx(:),emx(:)
integer,allocatable::ipiv(:)
real::rho_hb,chb
  
flg_err = .false.
  
nmx = nz

if (allocated(amx)) deallocate(amx)
if (allocated(ymx)) deallocate(ymx)
if (allocated(emx)) deallocate(emx)
if (allocated(ipiv)) deallocate(ipiv)
allocate (amx(nmx,nmx),ymx(nmx),emx(nmx),ipiv(nmx))

! clss = 1  !  when assuminng steady state and omit terms relevant to methane
! clrxn = 0  ! 1 when using phi_hb 
! clgrad = 1-clrxn ! 1 when using gradient of v*poro*hb instead of phi_hb

err3 = 1d9

itrnew = 0
loop1:do while (err3 > tor3) 
ccx = cc

amx = 0d0
ymx = 0d0

itrnew = itrnew+1

if (itrnew > itrnewmax) then 
  
    ! open(unit=250,file=trim(adjustl(outdir))//trim(adjustl(runid))//'/res/err_itrnew-'//trim(adjustl(procn))&
        ! //'.txt',status='unknown',position='append')
    ! write(250,*) latint, lonint, zs
    ! close(250)
    ! stop
    flg_err=.true.
    return
endif

if (clss == 0 .and. clrxn ==1) then ! not default; using phi_hb (reaction terms)

    do iz = 1, nz
        row = iz 
        if (iz <= ifmx) then 
            chb = ch
            rho_hb = rho_h
        else
            chb = cb
            rho_hb = rho_b
        end if 
        if (iz == 1) then 
            ymx(row) =  ((dif_c(iz)+dif_c(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hb(iz)+hb(iz+1))/2d0)*(ccx(iz+1)-ccx(iz))&
                    /(0.5d0*(dz(iz)+dz(iz+1)))  &
                -dif_c(iz)*(poro(iz)+1d0)/2d0*(1d0-(hb(iz)+0d0)/2d0)*(ccx(iz)-cc_0)/dz(iz))/dz(iz)   &
                ! + (clss-1)*clgrad*ccx(iz)*(v(iz)*poro(iz)*hb(iz))/dz &
                ! + rho_hb/rho_f*ccx(iz)*(v(iz)*poro(iz)*hb(iz))/dz &
                ! + rho_hb/rho_f*ccx(iz)*phi_hb(iz) &
                + ccx(iz)*phi_hb(iz) &
                - u(iz)*(ccx(iz+1)-cc_0)/2d0/dz(iz)  &
                ! - clss*(u(iz+1)*ccx(iz+1)-u(iz)*cc_0)/2d0/dz  &
                - poro(iz)*(1d0-hb(iz))*(ccx(iz)-cc(iz))/dt
            amx(row,row) = ((dif_c(iz)+dif_c(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hb(iz)+hb(iz+1))/2d0)*(-1d0)&
                    /(0.5d0*(dz(iz)+dz(iz+1)))  &
                -dif_c(iz)*(poro(iz)+1d0)/2d0*(1d0-(hb(iz)+0d0)/2d0)*(1d0)/dz(iz))/dz(iz)   &
                ! + (clss-1)*clgrad*(v(iz)*poro(iz)*hb(iz))/dz &
                ! + rho_hb/rho_f*(v(iz)*poro(iz)*hb(iz))/dz &
                ! + rho_hb/rho_f*phi_hb(iz) &
                + phi_hb(iz) &
                - poro(iz)*(1d0-hb(iz))*(1d0)/dt
            amx(row, row+1) = ((dif_c(iz)+dif_c(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hb(iz)+hb(iz+1))/2d0)*(1d0)&
                    /(0.5d0*(dz(iz)+dz(iz+1))))/dz(iz)   &
                - u(iz)*(1d0)/2d0/dz(iz)    
                ! - clss*(u(iz+1)*1.d0)/2d0/dz  
        else if (iz == nz) then 
            if (cc_ext == 0d0) then  
                ymx(row) =  (0d0   &
                    -(dif_c(iz)+dif_c(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hb(iz)+hb(iz-1))/2d0)*(ccx(iz)-ccx(iz-1))&
                        /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   &
                    ! + (clss-1)*clgrad*ccx(iz)*(v(iz)*poro(iz)*hb(iz)-v(iz-1)*poro(iz-1)*hb(iz-1))/dz &
                    ! + rho_hb/rho_f*ccx(iz)*(v(iz)*poro(iz)*hb(iz)-v(iz-1)*poro(iz-1)*hb(iz-1))/dz &
                    ! + rho_hb/rho_f*ccx(iz)*phi_hb(iz) &
                    + ccx(iz)*phi_hb(iz) &
                    - u(iz)*(ccx(iz)-ccx(iz-1))/2d0/dz(iz)  &
                    ! - clss*(u(iz)*ccx(iz)-u(iz-1)*ccx(iz-1))/2d0/dz  &
                    - poro(iz)*(1d0-hb(iz))*(ccx(iz)-cc(iz))/dt
                amx(row,row) =  (-(dif_c(iz)+dif_c(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hb(iz)+hb(iz-1))/2d0)*(1d0)&
                        /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   &
                    ! + (clss-1)*clgrad*(v(iz)*poro(iz)*hb(iz)-v(iz-1)*poro(iz-1)*hb(iz-1))/dz &
                    ! + rho_hb/rho_f*(v(iz)*poro(iz)*hb(iz)-v(iz-1)*poro(iz-1)*hb(iz-1))/dz &
                    ! + rho_hb/rho_f*phi_hb(iz) &
                    + phi_hb(iz) &
                    - u(iz)*(1d0)/2d0/dz(iz)  &
                    ! - clss*(u(iz)*1d0)/2d0/dz  &
                    - poro(iz)*(1d0-hb(iz))*(1d0)/dt
                amx(row, row-1) = (-(dif_c(iz)+dif_c(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hb(iz)+hb(iz-1))/2d0)*(-1d0)&
                        /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   &
                    - u(iz)*(-1d0)/2d0/dz(iz)  
                    ! - clss*(-u(iz-1))/2d0/dz  
            else 
                ymx(row) =  (dif_c(iz)*poro(iz)*(1d0-hb(iz))*(cc_ext-ccx(iz))/dz(iz)  &
                    -(dif_c(iz)+dif_c(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hb(iz)+hb(iz-1))/2d0)*(ccx(iz)-ccx(iz-1))&
                        /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   &
                    ! + (clss-1)*clgrad*ccx(iz)*(v(iz)*poro(iz)*hb(iz) - v(iz-1)*poro(iz-1)*hb(iz-1))/dz &
                    ! + rho_hb/rho_f*ccx(iz)*(v(iz)*poro(iz)*hb(iz) - v(iz-1)*poro(iz-1)*hb(iz-1))/dz &
                    ! + rho_hb/rho_f*ccx(iz)*phi_hb(iz) &
                    + ccx(iz)*phi_hb(iz) &
                    - u(iz)*(cc_ext-ccx(iz-1))/2d0/dz(iz)  &
                    ! - clss*(u(iz)*cc_ext-u(iz-1)*ccx(iz-1))/2d0/dz  &
                    - poro(iz)*(1d0-hb(iz))*(ccx(iz)-cc(iz))/dt 
                amx(row, row) =  (dif_c(iz)*poro(iz)*(1d0-hb(iz))*(-1d0)/dz(iz)  &
                    -(dif_c(iz)+dif_c(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hb(iz)+hb(iz-1))/2d0)*(1d0)&
                        /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   &
                    ! + (clss-1)*clgrad*(v(iz)*poro(iz)*hb(iz) - v(iz-1)*poro(iz-1)*hb(iz-1))/dz &
                    ! + rho_hb/rho_f*(v(iz)*poro(iz)*hb(iz) - v(iz-1)*poro(iz-1)*hb(iz-1))/dz &
                    ! + rho_hb/rho_f*phi_hb(iz) &
                    + phi_hb(iz) &
                    - poro(iz)*(1d0-hb(iz))*(1d0)/dt
                amx(row, row-1) =   (-(dif_c(iz)+dif_c(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hb(iz)+hb(iz-1))/2d0)*(-1d0)&
                        /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   &
                    - u(iz)*(-1d0)/2d0/dz(iz)   
                    ! - clss*(-u(iz-1))/2d0/dz  
            end if 
        ! else if (iz == ifmx) then 
            ! ymx(row) = (rho_b/rho_f*b(ifmx)-rho_h/rho_f*h(ifmx))*v(iz)*ccx(iz)  &
                ! -(1d0-h(ifmx))*dif_c*(ccx(iz)-ccx(iz-1))/dz &
                ! +(1d0-b(ifmx))*dif_c*(ccx(iz+1)-ccx(iz))/dz
            ! amx(row, row) = (rho_b/rho_f*b(ifmx)-rho_h/rho_f*h(ifmx))*v(iz)  &
                ! -(1d0-h(ifmx))*dif_c*(1d0)/dz &
                ! +(1d0-b(ifmx))*dif_c*(-1d0)/dz
            ! amx(row, row-1) =   &
                ! -(1d0-h(ifmx))*dif_c*(-1d0)/dz 
            ! amx(row, row+1) = &
                ! +(1d0-b(ifmx))*dif_c*(1d0)/dz
        else 
            ymx(row) =  ((dif_c(iz)+dif_c(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hb(iz)+hb(iz+1))/2d0)*(ccx(iz+1)-ccx(iz))&
                    /(0.5d0*(dz(iz)+dz(iz+1)))  &
                -(dif_c(iz)+dif_c(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hb(iz)+hb(iz-1))/2d0)*(ccx(iz)-ccx(iz-1))&
                    /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   &
                ! + (clss-1)*clgrad*ccx(iz)*(v(iz)*poro(iz)*hb(iz) - v(iz-1)*poro(iz-1)*hb(iz-1))/dz &
                ! + rho_hb/rho_f*ccx(iz)*(v(iz)*poro(iz)*hb(iz) - v(iz-1)*poro(iz-1)*hb(iz-1))/dz &
                ! + rho_hb/rho_f*ccx(iz)*phi_hb(iz) &
                + ccx(iz)*phi_hb(iz) &
                - u(iz)*(ccx(iz+1)-ccx(iz-1))/2d0/dz(iz)  &
                ! - clss*(u(iz+1)*ccx(iz+1)-u(iz-1)*ccx(iz-1))/2d0/dz  &
                - poro(iz)*(1d0-hb(iz))*(ccx(iz)-cc(iz))/dt
            amx(row,row) = ((dif_c(iz)+dif_c(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hb(iz)+hb(iz+1))/2d0)*(-1d0)&
                    /(0.5d0*(dz(iz)+dz(iz+1)))  &
                -(dif_c(iz)+dif_c(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hb(iz)+hb(iz-1))/2d0)*(1d0)&
                    /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   &
                ! + (clss-1)*clgrad*(v(iz)*poro(iz)*hb(iz) - v(iz-1)*poro(iz-1)*hb(iz-1))/dz &
                ! + rho_hb/rho_f*(v(iz)*poro(iz)*hb(iz) - v(iz-1)*poro(iz-1)*hb(iz-1))/dz &
                ! + rho_hb/rho_f*phi_hb(iz) &
                + phi_hb(iz)*clrxn &
                - poro(iz)*(1d0-hb(iz))*(1d0)/dt
            amx(row, row+1) =  ((dif_c(iz)+dif_c(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hb(iz)+hb(iz+1))/2d0)*(1d0)&
                    /(0.5d0*(dz(iz)+dz(iz+1))))/dz(iz)   &
                - u(iz)*(1d0)/2d0/dz(iz)  
                ! - clss*(u(iz+1))/2d0/dz  
            amx(row, row-1) =  (-(dif_c(iz)+dif_c(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hb(iz)+hb(iz-1))/2d0)*(-1d0)&
                    /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   &
                - u(iz)*(-1d0)/2d0/dz(iz)  
                ! - clss*(-u(iz-1))/2d0/dz  
        end if 
    end do     

end if 

if (clss == 0 .and. clrxn ==0) then ! not default; using gradient 

    do iz = 1, nz
        row = iz 
        if (iz <= ifmx) then 
            chb = ch
            rho_hb = rho_h
        else
            chb = cb
            rho_hb = rho_b
        end if 
        if (iz == 1) then 
            ymx(row) =  ((dif_c(iz)+dif_c(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hb(iz)+hb(iz+1))/2d0)*(ccx(iz+1)-ccx(iz))&
                    /(0.5d0*(dz(iz)+dz(iz+1)))  &
                -dif_c(iz)*(poro(iz)+1d0)/2d0*(1d0-(hb(iz)+0d0)/2d0)*(ccx(iz)-cc_0)/dz(iz))/dz(iz)   &
                + ccx(iz)*(v(iz)*poro(iz)*hb(iz))/dz(iz) &
                ! + rho_hb/rho_f*ccx(iz)*(v(iz)*poro(iz)*hb(iz))/dz &
                ! + rho_hb/rho_f*ccx(iz)*phi_hb(iz) &
                ! + ccx(iz)*phi_hb(iz) &
                - u(iz)*(ccx(iz+1)-cc_0)/2d0/dz(iz)  &
                ! - clss*(u(iz+1)*ccx(iz+1)-u(iz)*cc_0)/2d0/dz  &
                - poro(iz)*(1d0-hb(iz))*(ccx(iz)-cc(iz))/dt
            amx(row,row) = ((dif_c(iz)+dif_c(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hb(iz)+hb(iz+1))/2d0)*(-1d0)&
                    /(0.5d0*(dz(iz)+dz(iz+1)))  &
                -dif_c(iz)*(poro(iz)+1d0)/2d0*(1d0-(hb(iz)+0d0)/2d0)*(1d0)/dz(iz))/dz(iz)   &
                + (clss-1)*clgrad*(v(iz)*poro(iz)*hb(iz))/dz(iz) &
                ! + rho_hb/rho_f*(v(iz)*poro(iz)*hb(iz))/dz &
                ! + rho_hb/rho_f*phi_hb(iz) &
                ! + phi_hb(iz) &
                - poro(iz)*(1d0-hb(iz))*(1d0)/dt
            amx(row, row+1) = ((dif_c(iz)+dif_c(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hb(iz)+hb(iz+1))/2d0)*(1d0) &
                    /(0.5d0*(dz(iz)+dz(iz+1))))/dz(iz)   &
                - u(iz)*(1d0)/2d0/dz(iz)    
                ! - clss*(u(iz+1)*1.d0)/2d0/dz  
        else if (iz == nz) then 
            if (cc_ext == 0d0) then  
                ymx(row) =  (0d0   &
                    -(dif_c(iz)+dif_c(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hb(iz)+hb(iz-1))/2d0)*(ccx(iz)-ccx(iz-1)) &
                        /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   &
                    + ccx(iz)*(v(iz)*poro(iz)*hb(iz)-v(iz-1)*poro(iz-1)*hb(iz-1))/dz(iz) &
                    ! + rho_hb/rho_f*ccx(iz)*(v(iz)*poro(iz)*hb(iz)-v(iz-1)*poro(iz-1)*hb(iz-1))/dz &
                    ! + rho_hb/rho_f*ccx(iz)*phi_hb(iz) &
                    ! + ccx(iz)*phi_hb(iz) &
                    - u(iz)*(ccx(iz)-ccx(iz-1))/2d0/dz(iz)  &
                    ! - clss*(u(iz)*ccx(iz)-u(iz-1)*ccx(iz-1))/2d0/dz  &
                    - poro(iz)*(1d0-hb(iz))*(ccx(iz)-cc(iz))/dt
                amx(row,row) =  (-(dif_c(iz)+dif_c(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hb(iz)+hb(iz-1))/2d0)*(1d0) &
                        /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   &
                    + (clss-1)*clgrad*(v(iz)*poro(iz)*hb(iz)-v(iz-1)*poro(iz-1)*hb(iz-1))/dz(iz) &
                    ! + rho_hb/rho_f*(v(iz)*poro(iz)*hb(iz)-v(iz-1)*poro(iz-1)*hb(iz-1))/dz &
                    ! + rho_hb/rho_f*phi_hb(iz) &
                    ! + phi_hb(iz) &
                    - u(iz)*(1d0)/2d0/dz(iz)  &
                    ! - clss*(u(iz)*1d0)/2d0/dz  &
                    - poro(iz)*(1d0-hb(iz))*(1d0)/dt
                amx(row, row-1) = (-(dif_c(iz)+dif_c(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hb(iz)+hb(iz-1))/2d0)*(-1d0) &
                    /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   &
                    - u(iz)*(-1d0)/2d0/dz(iz)  
                    ! - clss*(-u(iz-1))/2d0/dz  
            else 
                ymx(row) =  (dif_c(iz)*poro(iz)*(1d0-hb(iz))*(cc_ext-ccx(iz))/dz(iz)  &
                    -(dif_c(iz)+dif_c(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hb(iz)+hb(iz-1))/2d0)*(ccx(iz)-ccx(iz-1))&
                        /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   &
                    + ccx(iz)*(v(iz)*poro(iz)*hb(iz) - v(iz-1)*poro(iz-1)*hb(iz-1))/dz(iz) &
                    ! + rho_hb/rho_f*ccx(iz)*(v(iz)*poro(iz)*hb(iz) - v(iz-1)*poro(iz-1)*hb(iz-1))/dz &
                    ! + rho_hb/rho_f*ccx(iz)*phi_hb(iz) &
                    ! + ccx(iz)*phi_hb(iz) &
                    - u(iz)*(cc_ext-ccx(iz-1))/2d0/dz(iz)  &
                    ! - clss*(u(iz)*cc_ext-u(iz-1)*ccx(iz-1))/2d0/dz  &
                    - poro(iz)*(1d0-hb(iz))*(ccx(iz)-cc(iz))/dt 
                amx(row, row) =  (dif_c(iz)*poro(iz)*(1d0-hb(iz))*(-1d0)/dz(iz)  &
                    -(dif_c(iz)+dif_c(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hb(iz)+hb(iz-1))/2d0)*(1d0)&
                        /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   &
                    + (clss-1)*clgrad*(v(iz)*poro(iz)*hb(iz) - v(iz-1)*poro(iz-1)*hb(iz-1))/dz(iz) &
                    ! + rho_hb/rho_f*(v(iz)*poro(iz)*hb(iz) - v(iz-1)*poro(iz-1)*hb(iz-1))/dz &
                    ! + rho_hb/rho_f*phi_hb(iz) &
                    ! + phi_hb(iz) &
                    - poro(iz)*(1d0-hb(iz))*(1d0)/dt
                amx(row, row-1) =   (-(dif_c(iz)+dif_c(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hb(iz)+hb(iz-1))/2d0)*(-1d0) &
                        /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   &
                    - u(iz)*(-1d0)/2d0/dz(iz)   
                    ! - clss*(-u(iz-1))/2d0/dz  
            end if 
        ! else if (iz == ifmx) then 
            ! ymx(row) = (rho_b/rho_f*b(ifmx)-rho_h/rho_f*h(ifmx))*v(iz)*ccx(iz)  &
                ! -(1d0-h(ifmx))*dif_c*(ccx(iz)-ccx(iz-1))/dz &
                ! +(1d0-b(ifmx))*dif_c*(ccx(iz+1)-ccx(iz))/dz
            ! amx(row, row) = (rho_b/rho_f*b(ifmx)-rho_h/rho_f*h(ifmx))*v(iz)  &
                ! -(1d0-h(ifmx))*dif_c*(1d0)/dz &
                ! +(1d0-b(ifmx))*dif_c*(-1d0)/dz
            ! amx(row, row-1) =   &
                ! -(1d0-h(ifmx))*dif_c*(-1d0)/dz 
            ! amx(row, row+1) = &
                ! +(1d0-b(ifmx))*dif_c*(1d0)/dz
        else 
            ymx(row) =  ((dif_c(iz)+dif_c(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hb(iz)+hb(iz+1))/2d0)*(ccx(iz+1)-ccx(iz)) &
                    /(0.5d0*(dz(iz)+dz(iz+1)))  &
                -(dif_c(iz)+dif_c(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hb(iz)+hb(iz-1))/2d0)*(ccx(iz)-ccx(iz-1)) &
                    /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   &
                + ccx(iz)*(v(iz)*poro(iz)*hb(iz) - v(iz-1)*poro(iz-1)*hb(iz-1))/dz(iz) &
                ! + rho_hb/rho_f*ccx(iz)*(v(iz)*poro(iz)*hb(iz) - v(iz-1)*poro(iz-1)*hb(iz-1))/dz &
                ! + rho_hb/rho_f*ccx(iz)*phi_hb(iz) &
                ! + ccx(iz)*phi_hb(iz) &
                - u(iz)*(ccx(iz+1)-ccx(iz-1))/2d0/dz(iz)  &
                ! - clss*(u(iz+1)*ccx(iz+1)-u(iz-1)*ccx(iz-1))/2d0/dz  &
                - poro(iz)*(1d0-hb(iz))*(ccx(iz)-cc(iz))/dt
            amx(row,row) = ((dif_c(iz)+dif_c(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hb(iz)+hb(iz+1))/2d0)*(-1d0)  &
                    /(0.5d0*(dz(iz)+dz(iz+1)))  &
                -(dif_c(iz)+dif_c(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hb(iz)+hb(iz-1))/2d0)*(1d0)  &
                    /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   &
                + (clss-1)*clgrad*(v(iz)*poro(iz)*hb(iz) - v(iz-1)*poro(iz-1)*hb(iz-1))/dz(iz) &
                ! + rho_hb/rho_f*(v(iz)*poro(iz)*hb(iz) - v(iz-1)*poro(iz-1)*hb(iz-1))/dz &
                ! + rho_hb/rho_f*phi_hb(iz) &
                ! + phi_hb(iz)*clrxn &
                - poro(iz)*(1d0-hb(iz))*(1d0)/dt
            amx(row, row+1) =  ((dif_c(iz)+dif_c(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hb(iz)+hb(iz+1))/2d0)*(1d0) &
                    /(0.5d0*(dz(iz)+dz(iz+1))))/dz(iz)   &
                - u(iz)*(1d0)/2d0/dz(iz)  
                ! - clss*(u(iz+1))/2d0/dz  
            amx(row, row-1) =  (-(dif_c(iz)+dif_c(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hb(iz)+hb(iz-1))/2d0)*(-1d0) &
                    /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   &
                - u(iz)*(-1d0)/2d0/dz(iz)  
                ! - clss*(-u(iz-1))/2d0/dz(iz)  
        end if 
    end do     

end if 

if (clss == 1) then ! default; assuming steady state

    do iz = 1, nz
        row = iz 
        if (iz <= ifmx) then 
            chb = ch
            rho_hb = rho_h
        else
            chb = cb
            rho_hb = rho_b
        end if 
        if (iz == 1) then 
            ymx(row) =  ((dif_c(iz)+dif_c(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hb(iz)+hb(iz+1))/2d0)*(ccx(iz+1)-ccx(iz))&
                    /(0.5d0*(dz(iz)+dz(iz+1)))  &
                -dif_c(iz)*(poro(iz)+1d0)/2d0*(1d0-(hb(iz)+0d0)/2d0)*(ccx(iz)-cc_0)/dz(iz))/dz(iz)   &
                ! + ccx(iz)*(v(iz)*poro(iz)*hb(iz))/dz &
                ! + rho_hb/rho_f*ccx(iz)*(v(iz)*poro(iz)*hb(iz))/dz &
                ! + rho_hb/rho_f*ccx(iz)*phi_hb(iz) &
                ! + ccx(iz)*phi_hb(iz) &
                ! - u(iz)*(ccx(iz+1)-cc_0)/2d0/dz  &
                - (u(iz+1)*ccx(iz+1)-u(iz)*cc_0)/2d0/dz(iz)  
                ! - poro(iz)*(1d0-hb(iz))*(ccx(iz)-cc(iz))/dt
            amx(row,row) = ((dif_c(iz)+dif_c(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hb(iz)+hb(iz+1))/2d0)*(-1d0) &
                    /(0.5d0*(dz(iz)+dz(iz+1)))  &
                -dif_c(iz)*(poro(iz)+1d0)/2d0*(1d0-(hb(iz)+0d0)/2d0)*(1d0)/dz(iz))/dz(iz)   
                ! + (clss-1)*clgrad*(v(iz)*poro(iz)*hb(iz))/dz &
                ! + rho_hb/rho_f*(v(iz)*poro(iz)*hb(iz))/dz &
                ! + rho_hb/rho_f*phi_hb(iz) &
                ! + phi_hb(iz) &
                ! - poro(iz)*(1d0-hb(iz))*(1d0)/dt
            amx(row, row+1) = ((dif_c(iz)+dif_c(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hb(iz)+hb(iz+1))/2d0)*(1d0)&
                    /(0.5d0*(dz(iz)+dz(iz+1))))/dz(iz)   &
                ! - u(iz)*(1d0)/2d0/dz    
                - (u(iz+1)*1.d0)/2d0/dz(iz)  
        else if (iz == nz) then 
            if (cc_ext == 0d0) then  
                ymx(row) =  (0d0   &
                    -(dif_c(iz)+dif_c(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hb(iz)+hb(iz-1))/2d0)*(ccx(iz)-ccx(iz-1))&
                        /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   &
                    ! + ccx(iz)*(v(iz)*poro(iz)*hb(iz)-v(iz-1)*poro(iz-1)*hb(iz-1))/dz &
                    ! + rho_hb/rho_f*ccx(iz)*(v(iz)*poro(iz)*hb(iz)-v(iz-1)*poro(iz-1)*hb(iz-1))/dz &
                    ! + rho_hb/rho_f*ccx(iz)*phi_hb(iz) &
                    ! + ccx(iz)*phi_hb(iz) &
                    ! - u(iz)*(ccx(iz)-ccx(iz-1))/2d0/dz  &
                    - (u(iz)*ccx(iz)-u(iz-1)*ccx(iz-1))/2d0/dz(iz)  
                    ! - poro(iz)*(1d0-hb(iz))*(ccx(iz)-cc(iz))/dt
                amx(row,row) =  (-(dif_c(iz)+dif_c(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hb(iz)+hb(iz-1))/2d0)*(1d0)&
                        /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   &
                    ! + (clss-1)*clgrad*(v(iz)*poro(iz)*hb(iz)-v(iz-1)*poro(iz-1)*hb(iz-1))/dz &
                    ! + rho_hb/rho_f*(v(iz)*poro(iz)*hb(iz)-v(iz-1)*poro(iz-1)*hb(iz-1))/dz &
                    ! + rho_hb/rho_f*phi_hb(iz) &
                    ! + phi_hb(iz) &
                    ! - u(iz)*(1d0)/2d0/dz  &
                    - (u(iz)*1d0)/2d0/dz(iz)  
                    ! - poro(iz)*(1d0-hb(iz))*(1d0)/dt
                amx(row, row-1) = (-(dif_c(iz)+dif_c(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hb(iz)+hb(iz-1))/2d0)*(-1d0)&
                        /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   &
                    ! - u(iz)*(-1d0)/2d0/dz  
                    - (-u(iz-1))/2d0/dz(iz)  
            else 
                ymx(row) =  (dif_c(iz)*poro(iz)*(1d0-hb(iz))*(cc_ext-ccx(iz))/dz(iz)  &
                    -(dif_c(iz)+dif_c(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hb(iz)+hb(iz-1))/2d0)*(ccx(iz)-ccx(iz-1))&
                        /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   &
                    ! + ccx(iz)*(v(iz)*poro(iz)*hb(iz) - v(iz-1)*poro(iz-1)*hb(iz-1))/dz &
                    ! + rho_hb/rho_f*ccx(iz)*(v(iz)*poro(iz)*hb(iz) - v(iz-1)*poro(iz-1)*hb(iz-1))/dz &
                    ! + rho_hb/rho_f*ccx(iz)*phi_hb(iz) &
                    ! + ccx(iz)*phi_hb(iz) &
                    ! - u(iz)*(cc_ext-ccx(iz-1))/2d0/dz  &
                    - (u(iz)*cc_ext-u(iz-1)*ccx(iz-1))/2d0/dz(iz)  
                    ! - poro(iz)*(1d0-hb(iz))*(ccx(iz)-cc(iz))/dt 
                amx(row, row) =  (dif_c(iz)*poro(iz)*(1d0-hb(iz))*(-1d0)/dz(iz)  &
                    -(dif_c(iz)+dif_c(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hb(iz)+hb(iz-1))/2d0)*(1d0)&
                        /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   
                    ! + (clss-1)*clgrad*(v(iz)*poro(iz)*hb(iz) - v(iz-1)*poro(iz-1)*hb(iz-1))/dz &
                    ! + rho_hb/rho_f*(v(iz)*poro(iz)*hb(iz) - v(iz-1)*poro(iz-1)*hb(iz-1))/dz &
                    ! + rho_hb/rho_f*phi_hb(iz) &
                    ! + phi_hb(iz) &
                    ! - poro(iz)*(1d0-hb(iz))*(1d0)/dt
                amx(row, row-1) =   (-(dif_c(iz)+dif_c(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hb(iz)+hb(iz-1))/2d0)*(-1d0)&
                        /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   &
                    ! - u(iz)*(-1d0)/2d0/dz   
                    - (-u(iz-1))/2d0/dz(iz)  
            end if 
        ! else if (iz == ifmx) then 
            ! ymx(row) = (rho_b/rho_f*b(ifmx)-rho_h/rho_f*h(ifmx))*v(iz)*ccx(iz)  &
                ! -(1d0-h(ifmx))*dif_c*(ccx(iz)-ccx(iz-1))/dz &
                ! +(1d0-b(ifmx))*dif_c*(ccx(iz+1)-ccx(iz))/dz
            ! amx(row, row) = (rho_b/rho_f*b(ifmx)-rho_h/rho_f*h(ifmx))*v(iz)  &
                ! -(1d0-h(ifmx))*dif_c*(1d0)/dz &
                ! +(1d0-b(ifmx))*dif_c*(-1d0)/dz
            ! amx(row, row-1) =   &
                ! -(1d0-h(ifmx))*dif_c*(-1d0)/dz 
            ! amx(row, row+1) = &
                ! +(1d0-b(ifmx))*dif_c*(1d0)/dz
        else 
            ymx(row) =  ((dif_c(iz)+dif_c(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hb(iz)+hb(iz+1))/2d0)*(ccx(iz+1)-ccx(iz))&
                    /(0.5d0*(dz(iz)+dz(iz+1)))  &
                -(dif_c(iz)+dif_c(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hb(iz)+hb(iz-1))/2d0)*(ccx(iz)-ccx(iz-1))&
                    /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   &
                ! + ccx(iz)*(v(iz)*poro(iz)*hb(iz) - v(iz-1)*poro(iz-1)*hb(iz-1))/dz &
                ! + rho_hb/rho_f*ccx(iz)*(v(iz)*poro(iz)*hb(iz) - v(iz-1)*poro(iz-1)*hb(iz-1))/dz &
                ! + rho_hb/rho_f*ccx(iz)*phi_hb(iz) &
                ! + ccx(iz)*phi_hb(iz) &
                ! - u(iz)*(ccx(iz+1)-ccx(iz-1))/2d0/dz  &
                - (u(iz+1)*ccx(iz+1)-u(iz-1)*ccx(iz-1))/2d0/dz(iz)  
                ! - poro(iz)*(1d0-hb(iz))*(ccx(iz)-cc(iz))/dt
            amx(row,row) = ((dif_c(iz)+dif_c(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hb(iz)+hb(iz+1))/2d0)*(-1d0)&
                    /(0.5d0*(dz(iz)+dz(iz+1)))  &
                -(dif_c(iz)+dif_c(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hb(iz)+hb(iz-1))/2d0)*(1d0)&
                    /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   
                ! + (clss-1)*clgrad*(v(iz)*poro(iz)*hb(iz) - v(iz-1)*poro(iz-1)*hb(iz-1))/dz &
                ! + rho_hb/rho_f*(v(iz)*poro(iz)*hb(iz) - v(iz-1)*poro(iz-1)*hb(iz-1))/dz &
                ! + rho_hb/rho_f*phi_hb(iz) &
                ! + phi_hb(iz)*clrxn &
                ! - poro(iz)*(1d0-hb(iz))*(1d0)/dt
            amx(row, row+1) =  ((dif_c(iz)+dif_c(iz+1))/2d0*(poro(iz)+poro(iz+1))/2d0*(1d0-(hb(iz)+hb(iz+1))/2d0)*(1d0)&
                    /(0.5d0*(dz(iz)+dz(iz+1))))/dz(iz)   &
                ! - u(iz)*(1d0)/2d0/dz  
                - (u(iz+1))/2d0/dz(iz)  
            amx(row, row-1) =  (-(dif_c(iz)+dif_c(iz-1))/2d0*(poro(iz)+poro(iz-1))/2d0*(1d0-(hb(iz)+hb(iz-1))/2d0)*(-1d0)&
                    /(0.5d0*(dz(iz)+dz(iz-1))))/dz(iz)   &
                ! - u(iz)*(-1d0)/2d0/dz  
                - (-u(iz-1))/2d0/dz(iz)  
        end if 
    end do     

end if 

ymx = -ymx

call DGESV(nmx,int(1),amx,nmx,ipiv,ymx,nmx,info) 

emx = 0d0
do iz = 1, nz
    row = iz
    ccx(iz) = ccx(iz)+ymx(row)
    if (ccx(iz)/=0d0) emx(row) = abs(ymx(row)/ccx(iz))
end do 

err3 = maxval(emx)
cc = ccx
if (loc_display) then 
    if (itrcns==1) print '(a)', '+++++ Cl +++++'
    if (itrcns==2) print '(a)', '+++++ B  +++++'
    if (itrcns==3) print '(a)', '+++++ I  +++++'
    print '(3x,a9,3x,e9.3e2)', 'error', err3
    print '(2(3x,a9))', 'z', 'conc'
    do iz=1, nz, nz/10
        print '(2(3x,e9.3e2))',z(iz),ccx(iz)*rho_f/35.453d0/1d-3
    enddo 
endif

end do loop1

endsubroutine calcCnsvs
!**************************************************************************************************************************************

!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$  THERMODYNAMICS  $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

!**************************************************************************************************************************************
subroutine calctherm(                               &
    wtemp,geotherm,grav,rho_f,xis,wdpth,sal,        &! input
    clst_type,nz,beta,seddep,                       &! input 
    method,latint,lonint,indir,define_z,            &! input
    limsed,bubble_only,loc_display,                 &! input  
    dz,z,p3,temp3,z3,zmax,ceq,hszflg                &! output   
    )
implicit none
real,intent(in)::wtemp,geotherm,grav,rho_f,xis,wdpth,beta,seddep,sal
integer,intent(in):: nz,latint,lonint   
character(100),intent(in)::clst_type,method,indir
logical,intent(in)::define_z

real,dimension(nz),intent(inout)::ceq,dz,z
real,intent(out)::p3,temp3,z3,zmax
integer,intent(out)::hszflg
logical,intent(in)::loc_display,limsed,bubble_only

!local variables 
real clsp,ztot
integer iz
character(100)dbase
! declaration from TDcomplete (obtained from Stephen Hunter)
REAL,DIMENSION(1001,2)::eq_allo
REAL,ALLOCATABLE::ceq_allo(:),T_allo(:),zeq(:)
INTEGER:: size_eq, sizeT
REAL::alpha,c3,dz2

if(loc_display)then
    print*,'calling a sburoutine to calculate ch4 thermodynamics'
endif 

select case(trim(adjustl(method)))
    case('maekawa')
        if(loc_display)then
            print *,'method using data by Maekawa et al. (1995)'
        endif 
        call threephase_maekawa(  &
            wtemp,geotherm,grav,rho_f,xis,wdpth  &  ! input 
            ,p3,temp3,z3,hszflg   & ! output
            )

        if (hszflg == 2) then 
            print *, '... giving up finding hsz depth'
            ! stop
            return
        end if 
        if(loc_display)then
            print *, p3, temp3, z3
        endif
        zmax = 2d0* z3  ! max. depth equals to twice the depth of the bottom of hydrate stabile zone 
        if(limsed)then
            zmax = min(zmax,seddep)  ! added so that maximum thickness does not exceed sediment thickness
        endif
        ! do iz = 1, nz  ! assignment of real value to depth 
            ! z(iz) = iz*zmax/nz
        ! end do 
        ! dz = z(2) - z(1)  !  m
        ztot = zmax
        clsp = z3
        if (define_z) then 
            call make_grid(  &
                clst_type,nz,clsp,beta,ztot  &! input 
                ,z,dz         &! output
                )
        endif 

        do iz = 1, nz  ! calculation of ceq (mM) along depth
            if (z(iz) < z3) then 
                ceq(iz) = 156.36d0+6.34d0*(temp3-19.d0)+1.11d0*(p3-20.0d0)
                ceq(iz) = ceq(iz)*(1.0d0-0.10d0*sal)
                ceq(iz) = ceq(iz)*exp((wtemp+geotherm*z(iz)-temp3)/14.5)
            else 
                ceq(iz) = 156.36d0+6.34d0*(temp3-19.d0)+1.11d0*(p3-20.0d0)
                ceq(iz) = ceq(iz)*(1.0d0-0.10d0*sal)
            endif
        end do
        ceq = ceq/rho_f*16d0*1d-3 ! conversion from mM to g CH4/g l 

    case('davie')
        if(loc_display)then
            print *,'method using empirical relationship as in Davie et al. (2003)'
        endif 
        if (allocated(ceq_allo)) deallocate(ceq_allo); ALLOCATE (ceq_allo(nz))   
        if (allocated(T_allo)) deallocate(T_allo);ALLOCATE (T_allo(nz))
        if (allocated(zeq)) deallocate(zeq);ALLOCATE (zeq(nz))

        hszflg = 0
        call bsr(wdpth,geotherm,wtemp,z3,temp3,eq_allo,p3,size_eq,hszflg,indir)
               
        if (hszflg == 2) then 
            print *, '... giving up finding hsz depth'
            return
        endif
  
        zmax = 2d0* z3  ! max. depth equals to twice the depth of the bottom of hydrate stable zone 
        if(limsed)then
            zmax = min(zmax,seddep)  ! added so that maximum thickness does not exceed sediment thickness
        endif
        ! do iz = 1, nz  ! assignment of real value to depth 
            ! z(iz) = iz*zmax/nz
        ! end do 
        ! dz = z(2) - z(1)  !  m
        ztot = zmax
        clsp = z3
        if (define_z) then 
            call make_grid(  &
                clst_type,nz,clsp,beta,ztot  &! input 
                ,z,dz         &! output
                )
        endif 
          
        call threephase(wtemp,temp3,p3,geotherm,seddep,nz, alpha,T_allo,ceq_allo,c3,zeq,dz2,sizeT   &
            ,z,z3,limsed  &! added 
            )

        ! print *, (ceq_allo(i),i=1,n,n/5)
        ! print *, (T_allo(i),i=1,n,n/5)
        ! print *, (zeq(i),i=1,n,n/5)

        ceq(1:nz)=(ceq_allo(1:nz)/1.0D03) * (16.0D00 / 18.0D00)  ! this converts milli mole fraction to mass fraction 
  
    case('duan')

        write(dbase,*) 'duan'
        write(dbase,*) 'tish'
        hszflg = 0
        call hydrate_eq(wtemp+273.15, wdpth, sal, geotherm, seddep, nz, dbase, zmax, hszflg, z3, bubble_only, limsed)
        ! z3 = zmax/2d0
        ! call ch4duan(geotherm, wtemp, wdpth, sal, nz, hszflg, ceq, zmax, home)
        if (hszflg == 2) then 
            if(.not. bubble_only)then           
                print *, '... giving up finding hsz depth'
                ! stop
                return
            endif
        endif

        ! do iz = 1, nz  ! assignment of real value to depth 
            ! z(iz) = iz*zmax/nz
        ! end do 
        ! dz = z(2) - z(1)  !  m
        ztot = zmax
        clsp = z3
        if (define_z) then 
            call make_grid(  &
                clst_type,nz,clsp,beta,ztot  &! input 
                ,z,dz         &! output
                )
        endif 
            
        call hydrate_sol(  &
            wtemp+273.15,wdpth,sal,geotherm,nz,dbase,z,z3,grav,rho_f  &! input 
            ,ceq                    &! output  
            )

        ceq(:) = ceq(:)*16d0*1d-3 ! converting mol/kg to mass fraction (g/g): 16 is methane mass per mol 
  
end select 

endsubroutine calctherm
!**************************************************************************************************************************************

!**************************************************************************************************************************************
subroutine threephase_maekawa(  &
    wtemp,geotherm,grav,rho_f,xis,wdpth  &  ! input 
    ,p3,temp3,z3,hszflg   & ! output
    )
implicit none
real,intent(in)::wtemp,geotherm,grav,rho_f,xis,wdpth
real,intent(out)::p3,temp3,z3
integer,intent(out)::hszflg
! local variables 
real::err3,f2,df2dtemp,dtemp3
real::p0=0.101d0 ! MPa
real::tor3 = 1d-4 
integer::iteq,iz


hszflg = 0

p3 = p0*1d6
temp3 = wtemp
err3 = 1d9
iteq=0
do while (err3 > tor3)   ! iteration to determine the bottom of hydrate stable zone
    iteq=iteq+1
    if (iteq > 10000001) then
        print *,"cannot calculate depth of hsz"
        hszflg = 1
        exit
    endif
    f2 = -926.815d0+31979.3d0/(temp3+273.15d0)+144.909d0*log(temp3+273.15d0)   &
        +5847.92d0*xis+3220.26d0*xis*xis+5840.5d0*log(1.0d0-xis)  & 
        -log(rho_f*grav*(wdpth+(temp3-wtemp)/geotherm)/(p0*1d6))
    df2dtemp = (-1.d0)*31979.3d0/((temp3+273.15d0)**2)+144.909d0/(temp3+273.15d0)   &
        -1.d0/(rho_f*grav*(wdpth+(temp3-wtemp)/geotherm)/(p0*1d6))*(rho_f*grav*((1.0d0)/geotherm)/(p0*1d6))

    if (f2 == 0) then 
        print *, 'error'
    else 
        dtemp3 = f2/df2dtemp
    endif
      
    temp3 = temp3 - dtemp3
    p3 = rho_f*grav*(wdpth+(temp3-wtemp)/geotherm)/1d6
    z3 = (temp3-wtemp)/geotherm
    err3 = min(err3,abs(dtemp3/temp3))

    ! print *,'err3:',err3

end do 

if (hszflg == 1) then 
    temp3 = wtemp
    f2 = -926.815d0+31979.3d0/(temp3+273.15d0)+144.909d0*log(temp3+273.15d0)   &
        +5847.92d0*xis+3220.26d0*xis*xis+5840.5d0*log(1.0d0-xis)  & 
        -log(rho_f*grav*(wdpth+(temp3-wtemp)/geotherm)/(p0*1d6))
      
    do iz = 1, 10000000
        temp3 = wtemp + geotherm*iz/1d4  ! 0.1mm step check
        df2dtemp = -926.815d0+31979.3d0/(temp3+273.15d0)+144.909d0*log(temp3+273.15d0)   &
            +5847.92d0*xis+3220.26d0*xis*xis+5840.5d0*log(1.0d0-xis)  & 
            -log(rho_f*grav*(wdpth+(temp3-wtemp)/geotherm)/(p0*1d6))
        if (f2*df2dtemp<=0d0) then 
            print *, '... managed to find hsz depth!'
            z3 = (iz-0.5)/1d4    ! m below seafloor 
            temp3 = wtemp + geotherm*z3
            p3 = rho_f*grav*(wdpth+(temp3-wtemp)/geotherm)/1d6
            exit
        end if 
    enddo
    if (iz>=10000000) hszflg = 2
end if 

endsubroutine threephase_maekawa
!**************************************************************************************************************************************
 
!**************************************************************************************************************************************
subroutine bsr(wdpth,geotherm,wtemp,z3,temp3,eq_allo,P3,size_eq,hszflg, home)
! CODE from Stephen Hunter and modified
! USE global_data, ONLY: rho_f   ,flag_to_escape  ,pt, leng
implicit none

!dummy arguments
REAL,INTENT(IN)::wdpth,geotherm,wtemp
INTEGER,INTENT(OUT):: size_eq,hszflg
REAL,INTENT(OUT)::temp3,p3,z3
REAL,DIMENSION(1001,2),INTENT(OUT)::eq_allo
character(100),intent(in) :: home

!local variables
INTEGER::dint              
INTEGER::flag              
REAL::grav         
REAL::rho_f       
INTEGER::counterc          
INTEGER::counterd          
INTEGER::interfac          
INTEGER::leng
REAL::tempa(1000,2),Tsf_k,T3,x,x0,x1,x2,logp3,y0,y1
INTEGER::i
REAL::tw,A,B,C  
real::pt(67,2)


dint=0          !initialise interface increment to zero [m] 
flag=0          !initialise flag*
grav=9.8D00     !gravitational acceleration [m/s] !originally g
counterc=1      !counter  
counterd=0      !counter
tw=0.0D00       !depth of BSR below water surface
rho_f = 1000.d0


Tsf_k=wtemp+2.7315D02                  !convert seafloor temperature into Kelvin [K]
interfac = 0                         !initialise interface to sea floor

OPEN(UNIT=11,FILE=trim(adjustl(home))//"/pt_1.dat",STATUS="OLD",ACTION="READ")   
! dat file copied from matlab tar file which contain a bar-K relation of three phase equilibrium
leng=67
DO i=1,leng
    READ(11,*)x1,x2
    pt(i,1)=x1 ; pt(i,2)=x2
END DO
CLOSE(11)

! print *, pt

! open (unit = 222, file="testing.txt",status ='unknown')
! do i = 1,leng
! write(222, *) pt(i,1),pt(i,2)
! end do 
! close(222)

! print *, w, Tsf_k, G, rho_f

dobsr:DO
    IF(flag/=0) THEN
        exit dobsr
    END IF

    ! print *, "say sthing"

    interfac=interfac+dint  ! m below seawater-sediment interface  
    z3 = real(interfac,kind=8)
    temp3=Tsf_k+(geotherm*REAL(interfac,KIND=8))  ! Kelvin bsf from geothermal gradient 
    tw=wdpth+REAL(interfac,KIND=8)
    P3=tw*REAL(rho_f,KIND=8)*grav/1.0D06  ! MPa

    ! print *, P3, Tint, interfac, T3

    !interpolate to find T3 L275 to L286 do the same as interpl within bsr.m::L36
    counterd=0 ; x0=0.0D00 ; x1=0.0D00 ; y0=0.0D00 ; y1=0.0D00
    logp3=LOG(P3) 

    findval:DO i=1,leng
        IF(logp3 > LOG(pt(i,1)) ) THEN
            counterd=counterd+1
        END IF
    END DO findval

    if (counterd <=0) then 
        ! flag_to_escape = .true.
        hszflg = 2
        print *,">>> *** CAUTION!!  :  leaving bsr because it is unlikely to be able to find a stability zone for hydrates"
        return
    end if

    x0=LOG(pt(counterd,1)) ; x1=LOG(pt(counterd+1,1)) ; x=logp3 !recycle x0,x1,x
    y0=pt(counterd,2) ; y1=pt(counterd+1,2)
    ! print *, x0, x1,x,y0,y1
    !Do the interpolation **from "Linear_interpolation" from wikipedia
    A=x-x0  ; B=y1-y0   ; C=x1-x0
    T3=y0+( A*B/C )

    IF (ABS(temp3-T3)<0.1) THEN
        flag=1
    ELSEIF (temp3-T3 <0) THEN
        dint=1
        flag=0
    ELSEIF (temp3-T3 >0) THEN
        dint=-1
        flag=0
    END IF

    !Load the temporary vector (as we dont know the final size of 'eq')
    tempa(counterc,1)=temp3
    tempa(counterc,2)=P3

    counterc=counterc+1

    if (counterc > 1000) then 
        ! flag_to_escape = .true.
        hszflg = 2
        print *, ">>> Leaving bst subroutine because stability point is too deep???"
        return
    end if 

END DO dobsr

if (interfac <0) then 
    ! flag_to_escape = .true.
    hszflg = 2
    print *, ">>> *** CAUTION!!  :  leaving bsr because stabillity point is in water column"
    return
end if 

! print *, "hello again"


eq_allo(1:counterc-1,1)=tempa(1:counterc-1,1)  !  depth :temperature records
eq_allo(1:counterc-1,2)=tempa(1:counterc-1,2)  !  depth; pressure records

size_eq=counterc-1  ! depths to reach 3-phase-equilibrium point 

end subroutine bsr
!**************************************************************************************************************************************

!**************************************************************************************************************************************
subroutine threephase(wtemp,temp3,p3,geotherm,seddep,nz,alpha,T_allo,ceq_allo,c3,zeq,dz,sizeT  &
    ,z,z3,limsed  &! added input 
    )
! CODE from Stephen Hunter and modified  
! USE global_data, ONLY:debug

implicit none
! #include <defines.h>
!dummy variables
INTEGER,INTENT(IN)::nz
REAL,INTENT(IN)::wtemp,temp3,p3,geotherm,seddep
REAL,INTENT(IN)::z(nz),z3  ! added 
REAL,INTENT(OUT)::alpha,c3,dz
REAL,DIMENSION(nz),INTENT(OUT)::T_allo,ceq_allo,zeq
INTEGER,INTENT(OUT)::sizeT
LOGICAL,INTENT(IN)::limsed

!local variables
REAL::c3ref,dcdP,dcdT,dT,Pref,P3_bar,Tref,Tsf_k
REAL,DIMENSION(:),ALLOCATABLE::mult
INTEGER::i, iz

zeq(1:nz)=0.0D00

Tsf_k=wtemp+2.7315D02                !Convert to Kelvin[K]
P3_bar=p3*1.0D01                   !convert from Mpa to bar (0.1 MPa = 1bar)
c3ref=2.79D00                      !3-phase solubility of reference  
!  (milli mole fraction corresponding to 156.36 mM (Davie et al., 2003)
Tref=2.92D02                       !3-phase temperature of 200 bar reference [K]
Pref=2.0D02                        !3-phase pressure of reference [bar]
dcdT=0.12D00                       !temperature coeficient  (this value should be 0.11412 if using Davie et al. (2003))
dcdP=0.0016D00                     !pressure coeficient  (this value should be 0.019980000 if using Davie et al. (2003))
alpha=0.0692D00                    !coeficient of solubility curve  (likely 1/alpha in Davie et al. (2003)


!calculate the 3-phase solubility for the base of the HSZ
c3=c3ref +dcdT*(temp3-Tref) +dcdP*(P3_bar-Pref)

!Generate the solubility profile within HSZ
dT= (temp3-Tsf_k) / (REAL(nz,KIND=8)/2.0D00)

sizeT=((nz)/2)  ! half of total grid number

! T_allo(1)=Tsf_k
DO i=1,sizeT
    T_allo(i)=Tsf_k+( REAL(i,KIND=8)*dT )
END DO
if (.not.T_allo(sizeT)==temp3) print *, 'error in sub "3 phase"'  !  half rest of T_allo is not given (remains zero)
DO i=1,sizeT
    ! ceq_allo(i)=c3*exp(-alpha*(T3-T_allo(i)))
    ceq_allo(i)=c3*exp(-alpha*(temp3-T_allo(i)))
    ! if (debug==1) WRITE(*,*)i,ceq_allo(i)
END DO 

!Generate depth profile
dz=dT/geotherm
DO i=1,sizeT
    ! zeq(i)=(T_allo(i)-T_allo(1))/G          !within HSZ   (why do we need zeq? different from z?  YK)
    zeq(i)=(T_allo(i)-Tsf_k)/geotherm          !within HSZ   (why do we need zeq? different from z?  YK)
END DO
if (allocated(mult)) deallocate(mult); ALLOCATE(mult(sizeT)) ; mult=0.0D00
DO i=1,sizeT
    mult(i)=REAL(i,KIND=8)   ! don't know why this is required...? YK
END DO

DO i=sizeT+1,(2*sizeT)
    zeq(i)=zeq(sizeT)+ (mult(i-sizeT) *dz)   ! adding z profile beneath the 3-phase-equilibrium point 
END DO

!Generate solubility profile below HSZ (constant)

DO i=sizeT+1,(2*sizeT)
    ceq_allo(i)=c3
END DO   

if(limsed)then
    if (seddep*geotherm < temp3-Tsf_k) then  ! case where seddep is bottom and bottom of hsz is not within calculaiton domain
        dT= (seddep*geotherm) / (REAL(nz,KIND=8))

        sizeT=(nz)  !  total grid number
        DO i=1,sizeT
            T_allo(i)=Tsf_k+( REAL(i,KIND=8)*dT )
        END DO
        ! if (.not.T_allo(sizeT)==temp3) print *, 'error in sub "3 phase"'  !  half rest of T_allo is not given (remains zero)
        DO i=1,sizeT
            ! ceq_allo(i)=c3*exp(-alpha*(T3-T_allo(i)))
            ceq_allo(i)=c3*exp(-alpha*(temp3-T_allo(i)))
            ! if (debug==1) WRITE(*,*)i,ceq_allo(i)
        END DO 

        !Generate depth profile
        dz=dT/geotherm
        DO i=1,sizeT
            ! zeq(i)=(T_allo(i)-T_allo(1))/G          !within HSZ   (why do we need zeq? different from z?  YK)
            zeq(i)=(T_allo(i)-Tsf_k)/geotherm          !within HSZ   (why do we need zeq? different from z?  YK)
        END DO
        if (allocated(mult)) deallocate(mult); ALLOCATE(mult(sizeT)) ; mult=0.0D00
        DO i=1,sizeT
            mult(i)=REAL(i,KIND=8)   ! don't know why this is required...? YK
        END DO

        ! DO i=sizeT+1,(2*sizeT)
        ! zeq(i)=zeq(sizeT)+ (mult(i-sizeT) *dz)   ! adding z profile beneath the 3-phase-equilibrium point 
        ! END DO

    else if (seddep*geotherm >= temp3 - Tsf_k .and. seddep*geotherm < (temp3 - Tsf_k)*2d0) then ! case where z3 < seddep < 2*z3 

        do iz = 1, nz
            if (seddep*iz/nz*geotherm >= temp3 - Tsf_k) exit 
        enddo
        dT= (temp3 - Tsf_k) / (REAL(iz,KIND=8))

        sizeT=(iz)  !  total grid number
        DO i=1,sizeT
            T_allo(i)=Tsf_k+( REAL(i,KIND=8)*dT )
        END DO
        ! if (.not.T_allo(sizeT)==temp3) print *, 'error in sub "3 phase"'  !  half rest of T_allo is not given (remains zero)
        DO i=1,sizeT
            ! ceq_allo(i)=c3*exp(-alpha*(T3-T_allo(i)))
            ceq_allo(i)=c3*exp(-alpha*(temp3-T_allo(i)))
            ! if (debug==1) WRITE(*,*)i,ceq_allo(i)
        END DO 

        !Generate depth profile
        dz=dT/geotherm
        DO i=1,sizeT
            ! zeq(i)=(T_allo(i)-T_allo(1))/G          !within HSZ   (why do we need zeq? different from z?  YK)
            zeq(i)=(T_allo(i)-Tsf_k)/geotherm          !within HSZ   (why do we need zeq? different from z?  YK)
        END DO
        if (allocated(mult)) deallocate(mult); ALLOCATE(mult(sizeT)) ; mult=0.0D00
        DO i=1,sizeT
            mult(i)=REAL(i,KIND=8)   ! don't know why this is required...? YK
        END DO

        DO i=sizeT+1,nz
            zeq(i)=zeq(sizeT)+ (mult(i-sizeT) *dz)   ! adding z profile beneath the 3-phase-equilibrium point 
        END DO
        !Generate solubility profile below HSZ (constant)

        DO i=sizeT+1,nz
            ceq_allo(i)=c3
        END DO   

    endif
endif

! added 
T_allo = Tsf_k+geotherm*z
do iz=1,nz
    if (z(iz)<=z3) then 
        ceq_allo(iz)=c3*exp(-alpha*(temp3-T_allo(iz)))
    else 
        ceq_allo(iz)=c3
    endif 
enddo 


RETURN
end subroutine threephase
!**************************************************************************************************************************************

!**************************************************************************************************************************************
subroutine ch4duan(geotherm, wtemp, wdpth, mnacl, nz, errflg, mch4eq, zmax, home)
! use compile file to get solubility: NOT USED ANYMORE, KEPT FOR NO REASON  
! this subroutine is working only when main program is running alone (cannot be applied to parallel running)
implicit none 

real,intent(in) :: geotherm, wtemp, wdpth, mnacl
integer, intent(in) :: nz
integer, intent(out) :: errflg
real,intent(out) :: mch4eq(:), zmax
character(100),intent(in) :: home

integer i, j, iz, itr
character(250) dum(10)
real p, temp, mch4, rdum(10)
real rho_f, grav
real x1, x2, x3, maxtemp, maxdpth, torr, errr
real dz
real, allocatable :: z(:)

if (allocated(z)) deallocate(z)
allocate(z(nz))

torr = 1d-6  ! torelance

rho_f = 1000.d0 ! kg/m3
grav = 9.8d0 ! m/s2
maxtemp = 318d0 ! K (Duan & Sun, 2005)

maxdpth = (maxtemp-(wtemp+273.15d0))/geotherm  ! m

! seeking a solution for 3 phase equilibrium by bisection method
! initial guess & check 
x1 = 0d0
x2 = maxdpth
errr = 1d3
itr = 0
do while (errr > torr)
    x3 = x1+x2
    x3 = x3*0.5d0
    ! print*,'x1,x2,x3',x1,x2,x3
    temp = wtemp + 273.15d0 + x3*geotherm
    ! temp = temp + 273.15d0 ! K

    open(unit = 90, file =trim(adjustl(home))//'in.txt',status = 'replace', action = 'write')
    write(90,*) temp
    close(90)

    call system('./Hydrate-ch4 < in.txt 1> out.txt 2> err.txt')
    ! call system('./Hydrate-ch4 < in.txt 1> out.txt')
    ! call system("echo 276.")
    ! stop
    open(unit = 90, file = trim(adjustl(home))//'out.txt',status = 'old', action = 'read')
    do j = 1, 100
        read(90,*) dum(1)
        ! print*,dum(1)
        if (trim(adjustl(dum(1)))=='c1s=') exit
    enddo
    read(90,*) dum(1:6),p,dum(7)
    close(90)
    ! print *,adjustl(dum(1:6)),p,adjustl(dum(7))
    ! print*, p
    if (p<rho_f*grav*(wdpth+x3)/1d5) then 
        x1 = x3
    else
        x2 = x3
    endif 

    errr = abs((x1-x2)/x2)
    ! print*, errr

    itr = itr + 1
    if (itr > 1000) then 
        print*, ' .. unable to find hsz within 1000 iterations ..'
        errflg = 2
        exit
    endif

enddo

zmax = 2d0*x3
do iz = 1,nz
    z(iz) = zmax*iz/nz
enddo

dz = z(2) - z(1)

do iz = 1, nz

    temp = wtemp + 273.15d0 + z(iz)*geotherm
    ! temp = temp + 273.15d0 ! K

    if (z(iz)<=x3) then 

        open(unit = 90, file = trim(adjustl(home))//'in.txt',status = 'replace', action = 'write')
        write(90,*) temp
        close(90)

        call system('./Hydrate-ch4 < in.txt 1> out.txt 2> err.txt')
        ! call system('./Hydrate-ch4 < in.txt 1> out.txt')
        ! call system("echo 276.")
        ! stop
        open(unit = 90, file = trim(adjustl(home))//'out.txt',status = 'old', action = 'read')
        do j = 1, 100
            read(90,*) dum(1)
            ! print*,dum(1)
            if (trim(adjustl(dum(1)))=='c1s=') exit
        enddo
        read(90,*) dum(1:6),p,dum(7)
        close(90)
        ! print *,adjustl(dum(1:6)),p,adjustl(dum(7))
        ! print*, p

    else
        p = rho_f*grav*(wdpth+z(iz))/1d5

    end if

    open(unit = 90, file = trim(adjustl(home))//'in2.dat',status = 'replace', action = 'write')
    write(90,'(i0,/,f6.2,",",f9.3,",",f3.1,/,"exit")') 2, temp, p, mnacl
    close(90)

    call system('./CH4_solubility < in2.dat 1> out2.txt 2> err2.txt')
    ! call system('./CH4_solubility < in2.dat 1> out2.txt')

    open(unit = 90, file =trim(adjustl(home))// 'out2.txt',status = 'old', action = 'read')
    do i = 1, 100
        read(90,*) dum(1)
        ! print*,dum(1)
        if (trim(adjustl(dum(1))) == 'T(K)') exit
    enddo
    read(90,*) rdum(1:8)
    close(90)
    mch4 = rdum(4)
    ! print*, mch4

    mch4eq(iz) = mch4

enddo

! call cpu_time(finish_time)
! write(*,'(f10.3,A)')finish_time-start_time,"[CPU sec]"

end subroutine ch4duan
!**************************************************************************************************************************************

!**************************************************************************************************************************************
subroutine hydrate_eq(wtemp,wdpth,mnacl,geotherm,seddep,nz,dbase,zmax,flg,z3ph,bubble_only,limsed)

implicit none
! #include <defines.h>
real,intent(in) :: wtemp, wdpth, mnacl, geotherm, seddep
integer,intent(in) :: nz
real,intent(out) :: zmax, z3ph
integer,intent(out) :: flg
character(100),intent(in) :: dbase
logical,intent(in)::bubble_only,limsed

real pres, pdis, temp, rho_f, grav
real x1, x2, x3, torr, errr, xlim 
integer iz, cnt
real,allocatable :: z(:) 
real limz, dum1, dum2

if (allocated(z)) deallocate(z)
allocate(z(nz))

torr = 1d-9

rho_f = 1000d0 ! kg/m3
grav = 9.8d0  ! m/s2

limz = (318d0 - wtemp)/geotherm 

xlim = 4d3

x1 = 0d0
! x2 = 1000d0
x2 = xlim
errr = 1d3
cnt = 0
do while (errr> torr) 
    x3 = (x1+x2)/2d0

    cnt = cnt + 1
    ! if (cnt > 10000000 .or. x3 <= 0d0 .or. x3 > 1000d0) then 
    if (cnt > 10000000 .or. x3 <= 0d0 .or. x3 > xlim) then 
        print*, '... unable to find 3 phase equilibrium point ...'
        flg = 2
        if(.not.bubble_only)then
            exit
        endif
        x3 = 0d0
    endif

    temp = wtemp + x3*geotherm   ! K
    pres = rho_f*grav*(wdpth + x3)/1d5  ! bar 

    select case(trim(adjustl(dbase)))

        case('duan')
            call hydrate_sun(temp, pdis)

            if (pdis < pres) then 
                x1 = x3
            else 
                x2 = x3
            endif

        case('tish') 
            call hydrate_teshchenko(temp, pres/10., mnacl, pdis, dum1) 
            call ch4_sol_duan(pres, temp, mnacl, dum2)
            dum2 = dum2/(1d0+mnacl*(22.990d0+35.45d0)*1d-3+dum2*16d0*1d-3)  ! convert from per solvent to per seawater

            if (dum1 < dum2) then 
                x1 = x3
            else 
                x2 = x3
            endif

    end select
    
    errr = abs((x1-x2)/x2)

enddo

z3ph = x3

zmax = 2d0*x3
if(limsed)then
    zmax = min(zmax,seddep)  ! added so that maximum thickness does not exceed sediment thickness
endif

if(bubble_only)then
    if (x3==0d0) zmax = 1d3  ! assume maximum depth 1km in case of no-hydrate conditions
endif

endsubroutine hydrate_eq
!**************************************************************************************************************************************

!**************************************************************************************************************************************
subroutine hydrate_sol(  &
    wtemp,wdpth,mnacl,geotherm,nz,dbase,z,z3ph,grav,rho_f  &! input 
    ,ceq                    &! output  
    )

implicit none

real,intent(in)::wtemp,wdpth,mnacl,geotherm,grav,rho_f,z3ph,z(nz)
integer,intent(in)::nz
real,intent(out)::ceq(nz)
character(100),intent(in)::dbase

real pres,pdis,temp
real x3,torr
integer iz


torr = 1d-9

x3 = z3ph

do iz = 1, nz
    temp = wtemp + z(iz)*geotherm   ! K
    pres = rho_f*grav*(wdpth + z(iz))/1d5  ! bar

    if (z(iz) <= x3) then 
        select case(trim(adjustl(dbase)))
            case('duan')
                call hydrate_sun(temp, pdis) 
                call ch4_sol_duan(pdis, temp, mnacl, ceq(iz))
                ceq(iz) = ceq(iz)/(1d0+mnacl*(22.990d0+35.45d0)*1d-3+ceq(iz)*16d0*1d-3)  ! convert from per solvent to per seawater 
            case('tish')
                call hydrate_teshchenko(temp, pres/10., mnacl, pdis, ceq(iz))
                ! call ch4_sol_duan(pdis*1d1, temp, mnacl, ceq(iz))  
                ! ceq(iz) = ceq(iz)/(1d0+mnacl*(22.990d0+35.45d0)*1d-3+ceq(iz)*16d0*1d-3)  ! convert from per solvent to per seawater 
        endselect 
    else if (z(iz) > x3) then 
        call ch4_sol_duan(pres, temp, mnacl, ceq(iz))
        ceq(iz) = ceq(iz)/(1d0+mnacl*(22.990d0+35.45d0)*1d-3+ceq(iz)*16d0*1d-3)  ! convert from per solvent to per seawater 
    endif
enddo

endsubroutine hydrate_sol
!**************************************************************************************************************************************

!**************************************************************************************************************************************
subroutine hydrate_sun(temp, pdis)

implicit none

real,intent(in) :: temp
real,intent(out) :: pdis

real afit1(7), afit2(7)
integer i

afit1(1) =  0.0072977291687730d0
afit1(2) = -3.2240249849578400d0
afit1(3) =  361.8335866212210000d0
afit1(4:7) = 0d0

afit2(1) = -0.0000000006307769d0
afit2(2) =  0.0000011546215488d0 
afit2(3) = -0.0008790662162124d0 
afit2(4) =  0.3563210990943340d0
afit2(5) = -81.1023625533258000d0 
afit2(6) =  9828.4771746760800000d0 
afit2(7) =  -495455.1196448790000000d0


afit2(1) = 0.0000019543986162d0
afit2(2) =  - 0.0034351881667419d0 
afit2(3) = 2.5147418138308100d0 
afit2(4) =  - 981.3690032399130000d0
afit2(5) =  215315.6017570030000000d0 
afit2(6) = -25181791.9826243000000000d0 
afit2(7) =   1226439052.4788500000000000d0


if (temp <= 272.9d0) then
    pdis = 0d0  
    do i = 1, 7
        pdis = pdis + afit1(7-i+1)*temp**(i-1-4)
    enddo
else if (temp > 272.9d0 .and. temp < 278d0) then
    pdis = 0.1654107142857060d0*temp**2 - 87.9245035714238000d0*temp + 11701.3113857136000000d0
else if (temp > 278d0) then 
    pdis = 0d0  
    do i = 1, 7
        pdis = pdis + afit2(7-i+1)*temp**(i-1)
    enddo
    ! pdis = 10d0**pdis
end if

endsubroutine
!**************************************************************************************************************************************

!**************************************************************************************************************************************
subroutine hydrate_teshchenko(temp, p, mnacl, pdis, cch4) 

implicit none

real,intent(in) :: temp, p, mnacl
real,intent(out) :: pdis, cch4

real afit1(7), afit2(7), afit(7), salts

! temp = temp + 273.15d0 ! K
salts = mnacl*(22.99d0+35.45d0)/(1d0+mnacl*(22.99d0+35.45d0)*1d-3)  ! conventional salinity (ppt; g/1000 g)
! print*,salts
pdis = -1.6444866d3 - 0.1374178d0*temp + 5.4979866d4/temp + 2.64118188d2*log(temp) &
    + salts*(1.1178266d4 + 7.67420344*temp - 4.515213d-3*temp**2 - 2.04872879d5/temp  &
    - 2.17246046d3*log(temp)) + salts**2*(1.70484431d2 + 0.118594073d0*temp  &
    - 7.0581304d-5*temp**2 - 3.09796169d3/temp - 33.2031996d0*log(temp))        !  Eq. (24)
  
pdis = exp(pdis) ! MPa
! pdis = pdis*1d6/1d5 ! bar

cch4 = -2.5640213d5 - 1.6448053d2*temp + 9.1089042d-2*temp**2 + 4.90352929d6/temp  &
    + 4.93009113d4*log(temp) + salts*(- 5.16285134d2 - 0.33622376d0*temp + 1.88199047d-4*temp**2  &
    + 9.76525718d3/temp + 9.9523354d1*log(temp))          ! Eq. (25)

cch4 = cch4 + (5.04597d-2 + 7.64415d-4*salts - (3.90236d-4 + 5.48947d-6*salts)*temp &
    + (7.06154d-7 + 9.87742d-9*salts)*temp**2)*(p - pdis) + (7.57285d-5 - 1.90867d-8*salts &
    - 1.4483d-10*salts**2 - (1.96207d-7 - 6.67456d-11*salts)*temp)*(p - pdis)**2 
  
cch4 = exp(cch4)

endsubroutine hydrate_teshchenko
!**************************************************************************************************************************************

!**************************************************************************************************************************************
subroutine ch4_sol_duan(pres, temp, mnacl, mch4)

implicit none

real,intent(in) :: pres, temp, mnacl
real,intent(out) :: mch4

real pvap, pcrit, xnacl, q5, q6, q7, q8, q9, q10, gt, hx
real agt(6), tempred, tempcrit, ahx(6)
real wvol, wdens, awd(6), wdenscrit
real ych4, yh2o, xh2o, fh2o, afw(6)
real gascnst
real fch4, volred, vol, volcrit, zfm, tempcrit_ch4, pcrit_ch4, afm(15), afm2(7)
real tempred_ch4, pred_ch4, eqa1, deqa1, dvolred, erreqa1, torr
real uch4, ach4, aum(10), avc(10), avc2(10), lambda, chi
real mch4_chk

gascnst = 83.14472d0  ! bat cm3 mol-1 K-1

! calculation of water vapor from Shibue (2003) as function of temperature and NaCl mole ratio 

xnacl = mnacl/(mnacl + 1d3/18d0)  ! mole fraction
! print *, xnacl
if (xnacl > 0.117d0) print*,'***caution: too high salinity'

q5  =  9.00404d2
q6  = -2.92542d4
q7  =  1.39806d6
q8  = -2.80756d7
q9  =  2.41637d8
q10 = -7.18726d8

pcrit = 22.064d0 + q5*xnacl + q6*xnacl**2 + q7*xnacl**3 + q8*xnacl**4 + q9*xnacl**5 + q10*xnacl**6 ! MPa

! print *, pcrit

tempcrit = 647.096d0
tempred = temp/tempcrit

agt(1) = -7.85951783d0
agt(2) =  1.84408259d0
agt(3) = -11.7866497d0
agt(4) =  22.6807411d0
agt(5) = -15.9618719d0
agt(6) =  1.80122502d0

gt = agt(1)*(1d0-tempred)+agt(2)*(1d0-tempred)**1.5d0 + agt(3)*(1d0-tempred)**3 + agt(4)*(1d0-tempred)**3.5d0 &
    + agt(5)*(1d0-tempred)**4 + agt(6)*(1d0-tempred)**7.5d0
! print *, gt
gt = gt/tempred

! print *, gt

ahx(1) =  1.28746d-1  ! a1
ahx(2) = -7.31097d-1  ! a2
ahx(3) = -3.15058d2   ! a3
ahx(4) =  3.92767d2   ! b1
ahx(5) = -2.46440d3   ! b2
ahx(6) =  0.024d0     ! u

if (xnacl <= ahx(6)) then 
    hx = ahx(2)*xnacl/(xnacl+ahx(1)**2)+ahx(3)*xnacl**2
else if (ahx(6) < xnacl .and. xnacl < 0.117d0) then 
    hx = (ahx(1)**2*ahx(2)/((ahx(6)+ahx(1)**2)**2)+2d0*ahx(3)*ahx(6))*(xnacl-ahx(6)) &
        + ahx(4)*(xnacl-ahx(6))**2 + ahx(5)*(xnacl-ahx(6))*(xnacl**2-ahx(6)**2) &
        + ahx(2)*ahx(6)/(ahx(6)+ahx(1)**2) + ahx(3)*ahx(6)**2
else 
    print*,'NaCl conc. is too high or negative'
end if

! print *,hx

pvap = log(pcrit) + gt + hx
! print *, pvap
pvap = exp(pvap) ! MPa
! print *, pvap
pvap = pvap*1d6/1d5 ! bar (1 bar = 1d5 Pa)

! print *, pvap

! calculation of molar volume of water from Wagner and Pruss (1993)

wdenscrit = 322 ! kg/m3
awd(1) =  1.99274064d0
awd(2) =  1.09965342d0
awd(3) = -0.510839303d0
awd(4) = -1.75493479d0
awd(5) = -45.5170352d0
awd(6) = -6.74694450d5

wdens = 1d0 + awd(1)*(1d0-tempred)**(1d0/3d0) + awd(2)*(1d0-tempred)**(2d0/3d0) &
    + awd(3)*(1d0-tempred)**(5d0/3d0) + awd(4)*(1d0-tempred)**(16d0/3d0) &
    + awd(5)*(1d0-tempred)**(43d0/3d0) + awd(6)*(1d0-tempred)**(110d0/3d0)

wdens = wdens*wdenscrit  ! kg/m3
wdens = wdens*1d3*1d-6 ! g/cm3
wvol = 18d0/wdens  ! cm3/mol (18 g/mol divided by g/cm3)

! print*, wvol

! calculation of fugacity coefficient of water (fh2o) (Duan and Mao, 2006)
afw(1) = -1.42006707D-02
afw(2) =  1.08369910D-02
afw(3) = -1.59213160D-06
afw(4) = -1.10804676D-05
afw(5) = -3.14287155D00
afw(6) =  1.06338095D-03

fh2o = afw(1) + afw(2)*pres + afw(3)*pres**2 + afw(4)*pres*temp &
    + afw(5)*pres/temp + afw(6)*pres**2/temp
fh2o = exp(fh2o) 

! print *,fh2o

! calculation of mole fraction of methane in gas (ych4)
xh2o = 1d0 - 2d0*xnacl 
yh2o = xh2o*pvap/fh2o/pres*exp(wvol*(pres-pvap)/temp/gascnst)
ych4 = 1d0 - yh2o

! print *, yh2o

! calculation of fugacity coefficient of methane (fch4) (Duan et al., 1992)
tempcrit_ch4 = -82.55d0 ! deg C
tempcrit_ch4 = tempcrit_ch4 + 273.15d0  ! K
pcrit_ch4 = 46.41d0 ! bar
volcrit = gascnst*tempcrit_ch4/pcrit_ch4  ! cm3

tempred_ch4 = temp/tempcrit_ch4
pred_ch4 = pres/pcrit_ch4

afm(1)  =  8.72553928D-02
afm(2)  = -7.52599476D-01
afm(3)  =  3.75419887D-01
afm(4)  =  1.07291342D-02
afm(5)  =  5.49626360D-03
afm(6)  = -1.84772802D-02
afm(7)  =  3.18993183D-04
afm(8)  =  2.11079375D-04
afm(9)  =  2.01682801D-05
afm(10) = -1.65606189D-05
afm(11) =  1.19614546D-04
afm(12) = -1.08087289D-04
afm(13) =  4.48262295D-02  ! alfa
afm(14) =  7.53970000D-01  ! beta
afm(15) =  7.71670000D-02  ! gamma

afm2(1) = afm(1) + afm(2)/tempred_ch4/tempred_ch4 + afm(3)/tempred_ch4/tempred_ch4/tempred_ch4  ! B
afm2(2) = afm(4) + afm(5)/tempred_ch4/tempred_ch4 + afm(6)/tempred_ch4/tempred_ch4/tempred_ch4  ! C
afm2(3) = afm(7) + afm(8)/tempred_ch4/tempred_ch4 + afm(9)/tempred_ch4/tempred_ch4/tempred_ch4  ! D
afm2(4) = afm(10) + afm(11)/tempred_ch4/tempred_ch4 + afm(12)/tempred_ch4/tempred_ch4/tempred_ch4 ! E
afm2(5) = afm(13)/tempred_ch4/tempred_ch4/tempred_ch4  ! F

! find reduced volume of methane (volred) by Newton method
torr = 1d-9
erreqa1 = 1d3
volred = 1d0 ! initial guess 

do while(erreqa1 > torr) 

    eqa1 = pred_ch4*volred/tempred_ch4 - (1d0 + afm2(1)/volred + afm2(2)/volred/volred &
        + afm2(3)/(volred**4) + afm2(4)/(volred**5)   &  
        + afm2(5)/(volred**2)*(afm(14)+afm(15)/(volred**2))*exp(-afm(15)/(volred**2)))     ! eq. A1 in Duan et al. (1992) 
      
    deqa1 = pred_ch4*(1d0)/tempred_ch4 - (0d0 + afm2(1)*(-1d0)/(volred**2) + afm2(2)*(-2d0)/(volred**3) &
        + afm2(3)*(-4d0)/(volred**5) + afm2(4)*(-5d0)/(volred**6)   &
        + afm2(5)*(-2d0)/(volred**3)*(afm(14)+afm(15)/(volred**2))*exp(-afm(15)/(volred**2))  &  
        + afm2(5)/(volred**2)*(0d0+afm(15)*(-2d0)/(volred**3))*exp(-afm(15)/(volred**2))  &
        + afm2(5)/(volred**2)*(afm(14)+afm(15)/(volred**2))*exp(-afm(15)/(volred**2))*(-afm(15)*(-2d0)/(volred**3)))     ! eq. A1 differentiated by volred

    dvolred = -eqa1/deqa1

    erreqa1 = abs(dvolred/volred)
    volred = volred + dvolred

    ! print *, volred

enddo 

afm2(6) = afm2(5)/2d0/afm(15)*(afm(14) + 1d0 - (afm(14) + 1d0 + afm(15)/(volred**2))*exp(-afm(15)/(volred**2))) ! G
afm2(7) = pred_ch4*volred/tempred_ch4   ! Z

fch4 = afm2(7) - 1d0 - log(afm2(7)) + afm2(1)/volred + afm2(2)/2d0/(volred**2) + afm2(3)/4d0/(volred**4)  &
    + afm2(4)/5d0/(volred**5) + afm2(6)

fch4 = exp(fch4) 

! calculation of reference chemical potential of methane (uch4) and activity coefficient (ach4) (Duan and Mao, 2006)
aum(1)  =  0.83143711D+01
aum(2)  = -0.72772168D-03
aum(3)  =  0.21489858D+04
aum(4)  = -0.14019672D-04
aum(5)  = -0.66743449D+06
aum(6)  =  0.76985890D-02
aum(7)  = -0.50253331D-05
aum(8)  = -0.30092013D+01
aum(9)  =  0.48468502D+03
aum(10) =  0d0

avc(1)  = -0.81222036D+00
avc(2)  =  0.10635172D-02
avc(3)  =  0.18894036D+03
avc(4)  =  0d0
avc(5)  =  0d0
avc(6)  =  0.44105635D-04
avc(7)  =  0d0
avc(8)  =  0d0
avc(9)  =  0d0
avc(10) = -0.46797718D-10

avc2(1)  = -0.29903571D-02
avc2(2)  =  0d0
avc2(3)  =  0d0
avc2(4)  =  0d0
avc2(5)  =  0d0
avc2(6)  =  0d0
avc2(7)  =  0d0
avc2(8)  =  0d0
avc2(9)  =  0d0
avc2(10) =  0d0

uch4 = aum(1) + aum(2)*temp + aum(3)/temp + aum(4)*temp**2 + aum(5)/(temp**2) + aum(6)*pres + aum(7)*pres*temp &
    + aum(8)*pres/temp + aum(9)*pres/(temp**2) + aum(10)*pres**2*temp
  
lambda = avc(1) + avc(2)*temp + avc(3)/temp + avc(4)*temp**2 + avc(5)/(temp**2) + avc(6)*pres + avc(7)*pres*temp &
    + avc(8)*pres/temp + avc(9)*pres/(temp**2) + avc(10)*pres**2*temp
  
chi = avc2(1)

ach4 = 2d0*lambda*mnacl + chi*mnacl*mnacl  ! only considering NaCl

ach4 = exp(ach4)

! print*, uch4, chi, lambda, ach4

! yielding methane solubility in molality (mch4) (Duan and Mao, 2006)

mch4 = log(ych4*pres) - uch4 + log(fch4) - log(ach4)
mch4 = exp(mch4)

! print*,mch4

end subroutine ch4_sol_duan
!**************************************************************************************************************************************

endmodule sedgem_box_hydrate
