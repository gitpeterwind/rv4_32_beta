module AeroConstants_mod

  ! BoxChem/ESX/EMEP need some specific calculations of aerosol
  ! surface area, and we define 7 aerosol types
  ! We end up with variables here to avoid circularity in
  ! Makefile dependencies

  ! To allow emepctm-like aerosol reactions so we can refer to eg AERO%PM_F:
   
  implicit none

  private 

   integer, parameter, public :: NSAREA_DEF = 6 ! skip SIA_F - not needed!

   type, public :: aero_t ! EMEP only
     character(len=15) :: EQUILIB  = 'MARS ' !aerosol themodynamics
     logical          :: DYNAMICS = .false.
     integer          :: NSIZE    = 7
     integer :: PM_F=1,SS_F=2,DU_F=3,SS_C=4,DU_C=5,PM=6  ! Will be set in GasParticleCoeffs_mod
   end type aero_t
   type(aero_t), public, save :: AERO = aero_t()

end module AeroConstants_mod
