! ******************************************************************************************************************************** !
! cpl_comp_sedgem.f90
! SedGeM interface ocean/sediment compositional integrator
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! COUPLE INTERFACE COMPOSITION: OCN->SED
! NOTE: the running sum should 'auto-zero' in creating (e.g. annual) averages
SUBROUTINE cpl_comp_ocnsed(      &
     & dum_ocnstep,              &
     & dum_mbiogem,dum_msedgem,  &
     & dum_n_ocn,                &
     & dum_n_i_ocn,dum_n_j_ocn,  &
     & dum_n_i_sed,dum_n_j_sed,  &
     & dum_sfcocn1,              &
     & dum_sfcsumocn             &
     & )
  USE sedgem_lib
  IMPLICIT NONE
  ! dummy arguments
  integer,intent(in)::dum_ocnstep                               ! 
  integer,intent(in)::dum_mbiogem,dum_msedgem                   ! 
  integer,intent(in)::dum_n_ocn                                 ! 
  integer,intent(in)::dum_n_i_ocn,dum_n_j_ocn                   ! 
  integer,intent(in)::dum_n_i_sed,dum_n_j_sed                   ! 
  real,dimension(dum_n_ocn,dum_n_i_ocn,dum_n_j_ocn),intent(in)::dum_sfcocn1 !
  real,dimension(dum_n_ocn,dum_n_i_sed,dum_n_j_sed),intent(inout)::dum_sfcsumocn !
  ! local variables
  integer::i,j                                                  ! 
  integer::i1,j1                                                ! 
  integer::loc_scalei,loc_scalej                                ! 
  ! ---------------------------------------------------------- !
  ! START
  ! ---------------------------------------------------------- !
  IF (ctrl_debug_lvl1) print*, '*** COUPLE INTERFACE COMPOSITION: OCN->SED ***'
  IF (ctrl_debug_lvl1) print*, '    >>>'
  ! ---------------------------------------------------------- !
  ! initialize local variables
  loc_scalei = dum_n_i_sed/dum_n_i_ocn
  loc_scalej = dum_n_j_sed/dum_n_j_ocn
  ! set ambient bottom-water environmental conditions
  ! NOTE: grid transformation currently assumes;
  !       (i) that the origin of both grids co-incide
  !       (ii) the number of elements counted along either i or j axes of the sedgem grid is
  !            an integer multiple of that of the biogem grid
  !       (iii) within each grid, grid points all have equal area
  !       (iv) the grid masks correspond between biogem and sedgem grids
  !            (i.e., loc_scalei x loc_scalej valid sediment grid points correspond to each valid biogem grid point
  DO i=1,dum_n_i_sed
     i1 = int((real(i) - 0.5)/loc_scalei) + 1
     DO j=1,dum_n_j_sed
        j1 = int((real(j) - 0.5)/loc_scalej) + 1
        dum_sfcsumocn(:,i,j) =  &
             & ( &
             &   real(int(MOD(dum_ocnstep - dum_mbiogem,dum_msedgem)/dum_mbiogem))*dum_sfcsumocn(:,i,j) + &
             &   dum_sfcocn1(:,i1,j1) &
             & ) / &
             & real(int(MOD(dum_ocnstep - dum_mbiogem,dum_msedgem)/dum_mbiogem) + 1)
     end DO
  end DO
  ! ---------------------------------------------------------- !
  ! END
  ! ---------------------------------------------------------- !
  IF (ctrl_debug_lvl1) print*, '    <<<'
  ! ---------------------------------------------------------- !
end SUBROUTINE cpl_comp_ocnsed
! ******************************************************************************************************************************** !


! ******************************************************************************************************************************** !
! COUPLE INRTERFACE COMPOSITION: SED->OCN
SUBROUTINE cpl_comp_sedocn(     &
     & dum_n_sed,               &
     & dum_n_i_ocn,dum_n_j_ocn, &
     & dum_n_i_sed,dum_n_j_sed, &
     & dum_sfcsed1,             &
     & dum_sfcsed               &
     & )
  USE sedgem_lib
  IMPLICIT NONE
  ! dummy arguments
  integer,intent(in)::dum_n_sed                                 !
  integer,intent(in)::dum_n_i_ocn,dum_n_j_ocn                   !
  integer,intent(in)::dum_n_i_sed,dum_n_j_sed                   !
  real,dimension(dum_n_sed,dum_n_i_ocn,dum_n_j_ocn),intent(out)::dum_sfcsed1 ! 
  real,dimension(dum_n_sed,dum_n_i_sed,dum_n_j_sed),intent(in)::dum_sfcsed ! 
  ! local variables
  INTEGER::l,is
  integer::i,j                                                  !
  integer::i1,j1                                                !
  integer::di,dj                                                !
  integer::loc_scalei,loc_scalej                                !
  real::loc_rscale                                               !
  ! ---------------------------------------------------------- !
  ! START
  ! ---------------------------------------------------------- !
  IF (ctrl_debug_lvl1) print*, '*** COUPLE INRTERFACE COMPOSITION: SED->OCN ***'
  IF (ctrl_debug_lvl1) print*, '    >>>'
  ! ---------------------------------------------------------- !
  ! initialize local variables
  loc_scalei = dum_n_i_sed/dum_n_i_ocn
  loc_scalej = dum_n_j_sed/dum_n_j_ocn
  ! integrate sediment composition
  ! NOTE: units of fractional abundance
  ! NOTE: grid transformation currently assumes;
  !       (i) that the origin of both grids co-incide
  !       (ii) the number of elements counted along either i or j axes of the sedgem grid is
  !            an integer multiple of that of the biogem grid
  !       (iii) within each grid, grid points all have equal area
  !       (iv) the grid masks correspond between biogem and sedgem grids
  !            (i.e., loc_scalei x loc_scalej valid sediment grid points correspond to each valid biogem grid point
  ! NOTE: screen out zero wt% bulk locations from associated age and isotope tracer saving
  ! NOTE: weight averaging by number of sub grid points(!) (whcih may not necessarily be e.g. 4!)
  DO i1=1,dum_n_i_ocn
     DO j1=1,dum_n_j_ocn
        DO l=1,n_l_sed
           is = conv_iselected_is(l)
           SELECT CASE (sed_type(is))
           case (par_sed_type_age,11:20)
              loc_rscale = 0.0
              dum_sfcsed1(is,i1,j1) = 0.0
              do di=1,loc_scalei
                 i = loc_scalei*(i1 - 1) + di
                 do dj=1,loc_scalej
                    j = loc_scalei*(j1 - 1) + dj
                    IF (sed_mask(i,j)) THEN
                       IF (dum_sfcsed(sed_dep(is),i,j) > const_real_nullsmall) THEN
                          loc_rscale = loc_rscale + 1.0
                          dum_sfcsed1(is,i1,j1) = dum_sfcsed1(is,i1,j1) + dum_sfcsed(is,i,j)
                       end IF
                    end IF
                 end do
              end do
              if (loc_rscale > const_real_nullsmall) then
                 dum_sfcsed1(is,i1,j1) = (1.0/(loc_rscale))*dum_sfcsed1(is,i1,j1)
              else
                 dum_sfcsed1(is,i1,j1) = const_real_null
              endif
           case default
              loc_rscale = 0.0
              dum_sfcsed1(is,i1,j1) = 0.0
              do di=1,loc_scalei
                 i = loc_scalei*(i1 - 1) + di
                 do dj=1,loc_scalej
                    j = loc_scalei*(j1 - 1) + dj
                    IF (sed_mask(i,j)) THEN
                       loc_rscale = loc_rscale + 1.0
                       dum_sfcsed1(is,i1,j1) = dum_sfcsed1(is,i1,j1) + dum_sfcsed(is,i,j)
                    end IF
                 end do
              end do
              if (loc_rscale > const_real_nullsmall) then
                 dum_sfcsed1(is,i1,j1) = (1.0/(loc_rscale))*dum_sfcsed1(is,i1,j1)
              else
                 dum_sfcsed1(is,i1,j1) = 0.0
              endif
           end SELECT
        end DO
     end DO
  end DO
  ! ---------------------------------------------------------- !
  ! END
  ! ---------------------------------------------------------- !
  IF (ctrl_debug_lvl1) print*, '    <<<'
  ! ---------------------------------------------------------- !
end SUBROUTINE cpl_comp_sedocn
! ******************************************************************************************************************************** !

