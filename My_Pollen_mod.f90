!-----------------------------------------------------------------------!
! Empty Pollen rountines for "standrd" model compilation
!-----------------------------------------------------------------------!
! Birch pollen emission calculation based on
! M. Sofiev et al. 2006, doi:10.1007/s00484-006-0027-x
!
! Pollen emission based upon meteorology paparameters, and heatsum.
! Pollen particles are assumed of 22 um diameter and 800 kg/m3 density. 
!-----------------------------------------------------------------------!
module Pollen_const_mod
use Config_module,         only: USES
use Debug_module,          only: DEBUG   ! -> DEBUG%POLLEN
use ChemDims_mod,          only: NSPEC_ADV
use CheckStop_mod,         only: CheckStop
implicit none
public

real, parameter  :: &
  N_TOT(3)=1.0  ! avoid div0

contains

subroutine pollen_check(igrp,uconv_adv)
  integer, intent(out), optional :: igrp
  real, dimension(NSPEC_ADV), intent(inout), optional :: uconv_adv
  logical,save :: first_call=.true.
  if(present(igrp))igrp=-1
  if(.not.first_call)return
  first_call=.false.
  call CheckStop(USES%POLLEN.or.DEBUG%POLLEN,&
    "USES%POLLEN/DEBUG%POLLEN on model compiled without pollen modules")
endsubroutine pollen_check
endmodule Pollen_const_mod
!-----------------------------------------------------------------------!
! Empty Pollen rountines for "standrd" model compilation
!-----------------------------------------------------------------------!
! Birch pollen emission calculation based on
! M. Sofiev et al. 2006, doi:10.1007/s00484-006-0027-x
!
! Pollen emission based upon meteorology paparameters, and heatsum.
! Pollen particles are assumed of 22 um diameter and 800 kg/m3 density. 
!-----------------------------------------------------------------------!
module Pollen_mod
use Pollen_const_mod
implicit none
public:: pollen_flux,pollen_dump,pollen_read,pollen_check

real,public,save, allocatable,dimension(:,:,:) :: &
  heatsum,      & ! heatsum, needs to be remembered for forecast
  AreaPOLL,     & ! emission of pollen 
  R               ! pollen released so far

contains

subroutine pollen_flux(i,j,debug_flag) 
  implicit none
  integer, intent(in) :: i,j    ! coordinates of column
  logical, intent(in) :: debug_flag
  call pollen_check()
endsubroutine pollen_flux

subroutine pollen_read()
  call pollen_check()
endsubroutine pollen_read

subroutine pollen_dump()
  call pollen_check()
endsubroutine pollen_dump
endmodule Pollen_mod
