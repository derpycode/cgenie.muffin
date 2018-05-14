! ******************************************************************************************************************************** !
! goldlite_lib.f90
! 
! LIBRARY MODULE
! ******************************************************************************************************************************** !


MODULE goldlite_lib


  use genie_control
  use gem_util
  IMPLICIT NONE
  SAVE


  ! ****************************************************************************************************************************** !
  ! *** NAMELIST DEFINITIONS ***************************************************************************************************** !
  ! ****************************************************************************************************************************** !

  ! ### EDIT ADD AND/OR EXTEND NAME-LIST PARAMETER AND CONTROL OPTIONS ########################################################### !
  integer::opt_colorpattern                                             ! color pattern option
  NAMELIST /ini_goldlite_nml/opt_colorpattern
  ! ############################################################################################################################## !


  ! ****************************************************************************************************************************** !
  ! *** MODEL CONFIGURATION CONSTANTS - ARRAY DIMENSIONS ************************************************************************* !
  ! ****************************************************************************************************************************** !

  ! *** array dimensions ***
  ! grid dimensions
  INTEGER,PARAMETER::n_i = ilon1_ocn                                    ! 
  INTEGER,PARAMETER::n_j = ilat1_ocn                                    ! 
  INTEGER,PARAMETER::n_k = inl1_ocn                                     !
  ! misc arrays dimensions 
  INTEGER,PARAMETER::n_c = 6                                            ! number of circulation tracing colors
  INTEGER,PARAMETER::n_l = 1                                            ! number of surface levels


  ! ****************************************************************************************************************************** !
  ! *** MODEL CONFIGURATION CONSTANTS - PARAMETERS ******************************************************************************* !
  ! ****************************************************************************************************************************** !

  integer,parameter::par_nk_sur = 1                                     ! number of surface levels accounted for
  integer,parameter::par_n_color_ftrans_max = 4                         ! assumed maximum number of connecting cells

  ! ****************************************************************************************************************************** !
  ! *** GLOBAL VARIABLES AND RUN-TIME SET PARAMETERS ***************************************************************************** !
  ! ****************************************************************************************************************************** !

  ! *** Array definitions ***
  ! misc
  REAL,ALLOCATABLE,DIMENSION(:,:,:,:)::color_init                       ! initial color pattern array (values)
  integer,ALLOCATABLE,DIMENSION(:,:,:)::color_init_c                    ! initial color pattern array (color designation)
  REAL,ALLOCATABLE,DIMENSION(:,:,:,:)::color_ftrans                     ! initial color pattern array (values)


END MODULE goldlite_lib

