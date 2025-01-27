

! ******************************************************************************************************************************** !
! cpl_flux_regocn.f90
! reggem interface flux integrator
! ******************************************************************************************************************************** !


! Edited from original cpl_flux_sedocn.f90 (or cpl_flux_sedgem.f90) 
!  -> changed instances of sed->reg (for rocks on land)
! Want runoff flux from ocean grid (goldstein) to land (reg grid) 
!  and weathering flux from land to ocean
! Maybe don't need this as should be dumping solute fluxes in coastal cells rather than doing
!  a cell to cell flux matching 2 grids!

! ******************************************************************************************************************************** !
! COUPLE FLUXES: reg->OCN
SUBROUTINE cpl_flux_regocn(    &
     & dum_dts,                 &
     & dum_n_maxocn,            &
     & dum_nr_maxi,dum_nr_maxj, &
     & dum_n_maxi,dum_n_maxj,   &
     & dum_sfxreg,              &
     & dum_sfxsumreg1,          &
     & dum_gem                  &
     & )
  IMPLICIT NONE
  ! dummy arguments
  real,intent(in)::dum_dts
  integer,intent(in)::dum_n_maxocn
  integer,intent(in)::dum_nr_maxi,dum_nr_maxj
  integer,intent(in)::dum_n_maxi,dum_n_maxj
  real,dimension(dum_n_maxocn,dum_nr_maxi,dum_nr_maxj),intent(in)::dum_sfxreg
  real,dimension(dum_n_maxocn,dum_n_maxi,dum_n_maxj),intent(out)::dum_sfxsumreg1
  logical,intent(in)::dum_gem
  ! local variables
  integer::i,j
  integer::i1,j1
  integer::di,dj
  integer::loc_scalei,loc_scalej
  real::loc_scale
  ! initialize local variables!
  loc_scalei = dum_nr_maxi/dum_n_maxi
  loc_scalej = dum_nr_maxj/dum_n_maxj
  loc_scale = 1.0/real(loc_scalei*loc_scalej)
  ! set return (dissolution) flux to ocean
  ! NOTE: reg->ocn flux (reg grid) <dum_sfxocn> in units of (mol m-2 s-1)
  ! NOTE: reg->ocn flux (ocn grid) <dum_sfxocn1> in units of (mol m-2 s-1)
  ! NOTE: grid transformation currently assumes;
  !       (i) that the origin of both grids co-incide
  !       (ii) the number of elements counted along either i or j axes of the reggem grid is
  !            an integer multiple of that of the biogem grid
  !       (iii) within each grid, grid points all have equal area
  !       (iv) the grid masks correspond between biogem and reggem grids
  !            (i.e., loc_scalei x loc_scalej valid reggem grid points correspond to each valid biogem grid point
  DO i1=1,dum_n_maxi
     DO j1=1,dum_n_maxj
        do di=1,loc_scalei
           i = loc_scalei*(i1 - 1) + di
           do dj=1,loc_scalej
              j = loc_scalei*(j1 - 1) + dj
              dum_sfxsumreg1(:,i1,j1) = dum_sfxsumreg1(:,i1,j1) + loc_scale*dum_dts*dum_sfxreg(:,i,j)
           end do
        end do
     end DO
  end DO
  if (.NOT. dum_gem) then
     !dum_sfxreg(:,:,:) = 0.0
  end if
end SUBROUTINE cpl_flux_regocn
! ******************************************************************************************************************************** !

