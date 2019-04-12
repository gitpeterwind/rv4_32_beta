! <DerivedFields_mod.f90 - A component of the EMEP MSC-W Chemical transport Model>
!*****************************************************************************!

module DerivedFields_mod
  use OwnDataTypes_mod, only: Deriv,TXTLEN_DERIV
  implicit none
  private

  integer, public, parameter ::  &
       MAXDEF_DERIV2D = 488 & ! Max. No. 2D derived fields to be defined
      ,MAXDEF_DERIV3D = 230    ! Max. No. 3D derived fields to be defined


  ! We put definitions of **all** possible variables in def_2d, def_3d
  ! and copy the needed ones into f_xx. The data will go into d_2d, d_3d

    type(Deriv),public, dimension(MAXDEF_DERIV2D), save :: def_2d
    type(Deriv),public, dimension(MAXDEF_DERIV3D), save :: def_3d

    type(Deriv),public, allocatable, dimension(:), save :: f_2d
    type(Deriv),public, allocatable, dimension(:), save :: f_3d


  ! Fields for storing derived-style outputs. Will be allocated
  ! in Derived_mod.

  ! e.g. d_2d( num_deriv2d,LIMAX, LJMAX, LENOUT2D)
  ! &    d_3d( num_deriv3d,LIMAX, LJMAX, KMAX_MID, LENOUT3D )
   real, save, public, allocatable, dimension(:,:,:,:) :: d_2d
   real, save, public, allocatable, dimension(:,:,:,:,:) :: d_3d
   integer, save, public :: VGtest_out_ix = -1


end module DerivedFields_mod

