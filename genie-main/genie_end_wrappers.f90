

MODULE genie_end_wrappers

  ! ===========================================================
  ! This module contains wrapper subroutines to hide arg lists
  !             __End Routines Only__
  ! ===========================================================

  use genie_global

contains

  !!
  subroutine end_goldlite_wrapper
    implicit none
    call end_goldlite()
  end subroutine end_goldlite_wrapper

  !!
  subroutine end_ocnlite_wrapper
    implicit none
    call end_ocnlite()
  end subroutine end_ocnlite_wrapper

  !!
  subroutine end_ecogem_wrapper
    implicit none
    call end_ecogem()
  end subroutine end_ecogem_wrapper

  !!
  subroutine end_gem_wrapper
    implicit none
    call end_gem()
  end subroutine end_gem_wrapper

  !!
  subroutine end_biogem_wrapper
    implicit none
    call end_biogem()
  end subroutine end_biogem_wrapper

  !!
  subroutine end_atchem_wrapper
    implicit none
    call end_atchem()
  end subroutine end_atchem_wrapper

  !!
  subroutine end_sedgem_wrapper
    implicit none
    call end_sedgem(                                    &
    & real(conv_kocn_ksedgem*kocn_loop)*genie_timestep, &               ! input
         & genie_sfcsumocn                              &               ! input
         & )
  end subroutine end_sedgem_wrapper

  !!
  subroutine end_rokgem_wrapper
    implicit none
    call end_rokgem()
  end subroutine end_rokgem_wrapper

  !!
  subroutine end_gemlite_wrapper
    implicit none
    call end_gemlite()
  end subroutine end_gemlite_wrapper

END MODULE genie_end_wrappers
